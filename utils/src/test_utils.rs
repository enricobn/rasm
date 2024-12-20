pub fn init_log() {
    let _ = env_logger::builder().is_test(true).try_init();
}

pub fn init_minimal_log() {
    let _ = env_logger::builder()
        .format_timestamp(None)
        .format_module_path(false)
        .format_target(false)
        .format_level(false)
        .is_test(true)
        .try_init();
}
