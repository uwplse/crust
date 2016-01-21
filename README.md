# Dependencies

 * [rust-lang/rust](https://github.com/rust-lang/rust), revision 1.0.0-alpha.2-1771-gd528aa9

 * GHC 7
 * [parsec](https://hackage.haskell.org/package/parsec)
 * [regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)
 * [syb](https://hackage.haskell.org/package/syb)

 * OCaml compiler
 * ocamlbuild

 * [CBMC](http://www.cprover.org/cbmc/)

Crust runs only on 64-bit Linux.


# Environment

After compiling the version of Rust indicated above:

 * Add `/path/to/rust/x86_64-.../stage2/bin` to `$PATH`
 * Add `/path/to/rust/x86_64-.../stage2/lib` to `$LD_LIBRARY_PATH`

Also, add a symlink to the Rust source code in `stdlib_tests/`:

    ln -s /path/to/rust/src /path/to/crust/stdlib_tests/src

Finally apply `stdlib_tests/patches/eval_panic_fix.patch` to the Rust source
code.  This patch simplifies the `panic!()` macro to avoid using language
features that Crust doesn't support.


# Compiling

Run `make` at the top level.


# Running tests

    mkdir stdlib_tests_build
    cd stdlib_tests_build
    mkdir lib ir driver test
    # Generate test drivers:
    ../stdlib_tests/bin/driver.mk test/eval_vec.c
    # Run tests:
    ../stdlib_tests/bin/run_tests.sh test/eval_vec.c
    # Report failed tests:
    ../stdlib_tests/bin/collect_failing.sh

Replace the name `eval_vec` with the basename of one of the files from
`crust/stdlib_tests/filters` to change which tests are run.


# Adding new tests

To test other code from the Rust standard library, create a new `.filter` file in
`stdlib_tests/filters`, following the format of
`stdlib_tests/filters/eval_vec.filter`.  Edit the `library` and `construction`
lines to reflect the mangled names of the functions you want to test (library
set) and functions for constructing the data types of interest (construction
set).  In general, the mangled name used by Crust is the fully qualified name
of the function (as in `collections::vec::Vec::<T>::new`), with each `::`
replaced by `$`.

After creating the filter, follow the instructions above for running tests,
replacing `eval_vec` with the name of the new filter (minus `.filter`
extension).
