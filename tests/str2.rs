#![crate_type = "lib"]

static Foo : &'static [& 'static str; 1] = &[ "bar" ];
