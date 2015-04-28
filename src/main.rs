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
extern crate rustc_privacy;
extern crate rustc_borrowck;
extern crate rustc_typeck;
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
/*
use rustc::metadata::common::*;
use rustc::plugin;
use rustc::session::config;
use rustc::session::config::Input;
use rustc::util::common::time;
use rustc_llvm as llvm;
use rustc_trans::back::link;
*/
use syntax::ast;
use syntax::ast_map;
use arena::TypedArena;

use rustc::session;
use rustc::session::Session;
use rustc::session::config::{self, Input, OutputFilenames};
use rustc::session::search_paths::PathKind;
use rustc::lint;
use rustc::metadata;
use rustc::metadata::creader::CrateReader;
use rustc::middle::{stability, ty, reachable};
use rustc::middle::dependency_format;
use rustc::middle;
use rustc::plugin::registry::Registry;
use rustc::plugin;
use rustc::util::common::time;
use rustc_borrowck as borrowck;
use rustc_driver::driver;
use rustc_resolve as resolve;
use rustc_trans::back::link;
use rustc_trans::back::write;
//use rustc_trans::trans;
use rustc_typeck as typeck;

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
    let (tcx, name) = phase_3_run_analysis_passes(sess,
                                                  ast_map,
                                                  &arenas,
                                                  id,
                                                  rustc_resolve::MakeGlobMap::No);

    trans::process(&tcx, filter_fn, name);
}

/// Run the resolution, typechecking, region checking and other
/// miscellaneous analysis passes on the crate. Return various
/// structures carrying the results of the analysis.
pub fn phase_3_run_analysis_passes<'tcx>(sess: Session,
                                         ast_map: ast_map::Map<'tcx>,
                                         arenas: &'tcx ty::CtxtArenas<'tcx>,
                                         name: String,
                                         make_glob_map: resolve::MakeGlobMap)
                                         -> (ty::ctxt<'tcx>, String) {
    let time_passes = sess.time_passes();
    let krate = ast_map.krate();

    time(time_passes, "external crate/lib resolution", (), |_|
         CrateReader::new(&sess).read_crates(krate));

    let lang_items = time(time_passes, "language item collection", (), |_|
                          middle::lang_items::collect_language_items(krate, &sess));

    let resolve::CrateMap {
        def_map,
        freevars,
        export_map,
        trait_map,
        external_exports,
        glob_map,
    } =
        time(time_passes, "resolution", (),
             |_| resolve::resolve_crate(&sess,
                                        &ast_map,
                                        &lang_items,
                                        krate,
                                        make_glob_map));

    // Discard MTWT tables that aren't required past resolution.
    syntax::ext::mtwt::clear_tables();

    let named_region_map = time(time_passes, "lifetime resolution", (),
                                |_| middle::resolve_lifetime::krate(&sess, krate, &def_map));

    time(time_passes, "looking for entry point", (),
         |_| middle::entry::find_entry_point(&sess, &ast_map));

    sess.plugin_registrar_fn.set(
        time(time_passes, "looking for plugin registrar", (), |_|
            plugin::build::find_plugin_registrar(
                sess.diagnostic(), krate)));

    let region_map = time(time_passes, "region resolution", (), |_|
                          middle::region::resolve_crate(&sess, krate));

    time(time_passes, "loop checking", (), |_|
         middle::check_loop::check_crate(&sess, krate));

    time(time_passes, "static item recursion checking", (), |_|
         middle::check_static_recursion::check_crate(&sess, krate, &def_map, &ast_map));

    let ty_cx = ty::mk_ctxt(sess,
                            arenas,
                            def_map,
                            named_region_map,
                            ast_map,
                            freevars,
                            region_map,
                            lang_items,
                            stability::Index::new(krate));

    // passes are timed inside typeck
    typeck::check_crate(&ty_cx, trait_map);

    time(time_passes, "const checking", (), |_|
         middle::check_const::check_crate(&ty_cx));

    /*
    let (exported_items, public_items) =
            time(time_passes, "privacy checking", (), |_|
                 rustc_privacy::check_crate(&ty_cx, &export_map, external_exports));
                 */

    /*
    // Do not move this check past lint
    time(time_passes, "stability index", (), |_|
         ty_cx.stability.borrow_mut().build(&ty_cx.sess, krate, &public_items));
         */

    time(time_passes, "intrinsic checking", (), |_|
         middle::intrinsicck::check_crate(&ty_cx));

    time(time_passes, "effect checking", (), |_|
         middle::effect::check_crate(&ty_cx));

    time(time_passes, "match checking", (), |_|
         middle::check_match::check_crate(&ty_cx));

    time(time_passes, "liveness checking", (), |_|
         middle::liveness::check_crate(&ty_cx));

    time(time_passes, "borrow checking", (), |_|
         borrowck::check_crate(&ty_cx));

    time(time_passes, "rvalue checking", (), |_|
         middle::check_rvalues::check_crate(&ty_cx, krate));

    // Avoid overwhelming user with errors if type checking failed.
    // I'm not sure how helpful this is, to be honest, but it avoids a
    // lot of annoying errors in the compile-fail tests (basically,
    // lint warnings and so on -- kindck used to do this abort, but
    // kindck is gone now). -nmatsakis
    ty_cx.sess.abort_if_errors();

    /*
    let reachable_map =
        time(time_passes, "reachability checking", (), |_|
             reachable::find_reachable(&ty_cx, &exported_items));
             */

    /*
    time(time_passes, "death checking", (), |_| {
        middle::dead::check_crate(&ty_cx,
                                  &exported_items,
                                  &reachable_map)
    });
    */

    let ref lib_features_used =
        time(time_passes, "stability checking", (), |_|
             stability::check_unstable_api_usage(&ty_cx));

    time(time_passes, "unused lib feature checking", (), |_|
         stability::check_unused_or_stable_features(
             &ty_cx.sess, lib_features_used));

    /*
    time(time_passes, "lint checking", (), |_|
         lint::check_crate(&ty_cx, &exported_items));
         */

    // The above three passes generate errors w/o aborting
    ty_cx.sess.abort_if_errors();

    (ty_cx, name)
}

