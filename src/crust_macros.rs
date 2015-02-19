fn crust_abort() -> ! {
    unsafe { core::intrinsics::abort() }
}

macro_rules! panic {
    () => ({
        $crate::crust_abort()
    });
    ($msg:expr) => ({
        $crate::crust_abort()
    });
    ($fmt:expr, $($arg:tt)+) => ({
        $crate::crust_abort()
    })
}

macro_rules! assert {
    ($cond:expr) => (
        if !$cond {
            $crate::crust_abort()
        }
        );
    ($cond:expr, $($arg::tt)+) => (
        if !$cond {
            $crate::crust_abort()
        }
        )
}
