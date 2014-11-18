#![crate_name = "rbmc"]
#![feature(globs)]
extern crate getopts;
extern crate syntax;
extern crate rustc;
extern crate rbml;
extern crate rustc_llvm;
extern crate rustc_back;
extern crate libc;
extern crate arena;

use std::io;
use std::collections::HashMap;
use rustc::metadata::common::*;
use rustc_llvm as llvm;
use rustc::driver::config;
use rustc::driver::session;
use rustc::driver::driver;
use rustc::driver::driver::{Input, FileInput, StrInput};
use rustc::plugin;
use rustc::back::link;
use syntax::ast;
use syntax::ast_map;
use arena::TypedArena;

mod trans;

fn main() {
    run_compiler(std::os::args().as_slice().slice_from(1));
}

fn run_compiler(args: &[String]) {
    let matches = getopts::getopts(std::os::args().as_slice().slice_from(1),
                                   rustc::driver::config::optgroups().as_slice()).unwrap();

    let sopts = config::build_session_options(&matches);
    let (input, input_file_path) = match matches.free.len() {
        0u => {
            panic!("no input filename given");
        }
        1u => {
            let ifile = matches.free[0].as_slice();
            if ifile == "-" {
                let contents = io::stdin().read_to_end().unwrap();
                let src = String::from_utf8(contents).unwrap();
                (StrInput(src), None)
            } else {
                (FileInput(Path::new(ifile)), Some(Path::new(ifile)))
            }
        }
        _ => panic!("multiple input filenames provided")
    };

    let descriptions = syntax::diagnostics::registry::Registry::new(&[]);
    let sess = session::build_session(sopts, input_file_path, descriptions);
    let cfg = config::build_configuration(&sess);
    let odir = matches.opt_str("out-dir").map(|o| Path::new(o));
    let ofile = matches.opt_str("o").map(|o| Path::new(o));

    compile_input(sess, cfg, &input, &odir, &ofile, None);
}

pub fn compile_input(sess: session::Session,
                     cfg: ast::CrateConfig,
                     input: &driver::Input,
                     outdir: &Option<Path>,
                     output: &Option<Path>,
                     addl_plugins: Option<plugin::load::Plugins>) {
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
            = match driver::phase_2_configure_and_expand(&sess, krate, id.as_slice(),
                                                         addl_plugins) {
                None => return,
                Some(k) => k
            };

        (outputs, expanded_crate, id)
    };

    let mut forest = ast_map::Forest::new(expanded_crate);
    let ast_map = driver::assign_node_ids_and_map(&sess, &mut forest);

    let type_arena = TypedArena::new();
    let analysis = driver::phase_3_run_analysis_passes(sess, ast_map, &type_arena, id);

    trans::process(&analysis.ty_cx);
}
