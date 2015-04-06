// Builds with rustc 336349c (Mon Nov 17 20:37:19 2014 +0000)
#![feature(core)]
#![feature(convert)]
#![feature(libc)]
#![feature(as_slice)]
#![feature(collections)]
#![feature(std_misc)]
#![feature(rustc_private)]

#![crate_name = "rbmc"]
extern crate getopts;
extern crate syntax;
extern crate rustc;
extern crate rbml;
extern crate rustc_llvm;
extern crate rustc_back;
extern crate rustc_driver;
extern crate rustc_trans;
extern crate rustc_resolve;
extern crate libc;
extern crate arena;

use std::io::prelude::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::io;
use std::os;
use std::path::{Path, PathBuf};
use rustc::metadata::common::*;
use rustc::plugin;
use rustc::session::config;
use rustc::session;
use rustc::session::config::Input;
use rustc_driver::driver;
use rustc_llvm as llvm;
use rustc_trans::back::link;
use syntax::ast;
use syntax::ast_map;
use arena::TypedArena;

mod trans;

fn main() {
    run_compiler(&env::args().collect::<Vec<_>>()[1..]);
}

fn build_filter_list(matches : &getopts::Matches) -> HashSet<String> {
    match matches.opt_str("crust-filter") {
        None => HashSet::new(),
        Some(ref f_name) => {
            let mut to_ret = HashSet::new();
            let mut f = io::BufReader::new(fs::File::open(&PathBuf::from(f_name)).unwrap());
            for line in f.lines() {
                let l = line.unwrap();
                if l.as_slice().is_empty() {
                    continue;
                } else {
                    to_ret.insert(String::from_str(l.trim()));
                }
            }
            to_ret
        }
    }
}

fn run_compiler(args: &[String]) {
    let mut tool_opts = config::optgroups();
    tool_opts.push(getopts::optopt("", "crust-filter", "Filter function definitions from file", ""));
    let matches = getopts::getopts(args, tool_opts.as_slice()).unwrap();

    let sopts = config::build_session_options(&matches);
    let (input, input_file_path) = match matches.free.len() {
        0 => {
            panic!("no input filename given");
        }
        1 => {
            let ifile = matches.free[0].as_slice();
            if ifile == "-" {
                let mut contents = vec![];
                io::stdin().read_to_end(&mut contents).unwrap();
                let src = String::from_utf8(contents).unwrap();
                (Input::Str(src), None)
            } else {
                (Input::File(PathBuf::from(ifile)), Some(PathBuf::from(ifile)))
            }
        }
        _ => panic!("multiple input filenames provided")
    };
    let filter_fn = build_filter_list(&matches);

    let descriptions = syntax::diagnostics::registry::Registry::new(&[]);
    let sess = session::build_session(sopts, input_file_path, descriptions);
    let cfg = config::build_configuration(&sess);
    let odir = matches.opt_str("out-dir").map(|o| PathBuf::from(o));
    let ofile = matches.opt_str("o").map(|o| PathBuf::from(o));

    compile_input(sess, cfg, &input, &odir, &ofile, filter_fn);
}

pub fn compile_input(sess: session::Session,
                     cfg: ast::CrateConfig,
                     input: &Input,
                     outdir: &Option<PathBuf>,
                     output: &Option<PathBuf>,
                     filter_fn: HashSet<String>) {
    let (outputs, expanded_crate, id) = {
        let krate = driver::phase_1_parse_input(&sess, cfg, input);
        let outputs = driver::build_output_filenames(input,
                                                     outdir,
                                                     output,
                                                     krate.attrs.as_slice(),
                                                     &sess);
        let id = link::find_crate_name(Some(&sess), krate.attrs.as_slice(),
                                       input);
        let expanded_crate
            = match driver::phase_2_configure_and_expand(&sess, krate, id.as_slice(), None) {
                None => return,
                Some(k) => k
            };

        (outputs, expanded_crate, id)
    };

    let mut forest = ast_map::Forest::new(expanded_crate);
    let (ast_map, arenas) =
        (driver::assign_node_ids_and_map(&sess, &mut forest),
         rustc::middle::ty::CtxtArenas::new());
    let analysis = driver::phase_3_run_analysis_passes(sess,
                                                       ast_map,
                                                       &arenas,
                                                       id,
                                                       rustc_resolve::MakeGlobMap::No);

    trans::process(&analysis.ty_cx, filter_fn, analysis.name.clone());
}
