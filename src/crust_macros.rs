fn crust_abort() -> ! {
    unsafe { core::intrinsics::abort() }
}

macro_rules! panic {
    () => ({
        crust_abort()
    });
    ($msg:expr) => ({
        crust_abort()
    });
    ($fmt:expr, $($arg:tt)+) => ({
        crust_abort()
    })
}

macro_rules! assert {
    ($cond:expr) => (
        if !$cond {
            crust_abort()
        }
        );
    ($cond:expr, $($arg::tt)+) => (
        if !$cond {
            crust_abort()
        }
        )
}
