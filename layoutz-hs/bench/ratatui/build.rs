// ratatui version from Cargo.lock, for `ratatui-dashboard version`.
fn main() {
    println!("cargo:rerun-if-changed=Cargo.lock");
    let lock = std::fs::read_to_string("Cargo.lock").unwrap_or_default();
    let mut lines = lock.lines();
    let mut version = "(unknown version)".to_string();
    while let Some(line) = lines.next() {
        if line == "name = \"ratatui\"" {
            if let Some(v) = lines.next().and_then(|l| l.strip_prefix("version = \"")) {
                version = format!("v{}", v.trim_end_matches('"'));
            }
            break;
        }
    }
    println!("cargo:rustc-env=RATATUI_VERSION={version}");
}
