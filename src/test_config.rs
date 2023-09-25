pub(crate) fn emit_module_graphviz_file() -> bool {
    std::env::var("EMIT_MODULE_GRAPHVIZ_FILE")
        .unwrap_or_default()
        .parse::<bool>()
        .unwrap_or_default()
}
