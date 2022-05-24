use std::process::{Command, Stdio};
use tempdir::TempDir;

#[cfg(test)]

#[test]
fn test_helloworld() {
    test("helloworld", Vec::new(), "Hello world\nHi\n10\n40\n45\none\ntwo\n");
}

#[test]
fn test_fibonacci() {
    test("fibonacci", vec!["10"],"55\n");
}

#[test]
fn test_inline() {
    test("inline", Vec::new(),"5\n");
}

#[test]
fn test_cmdlineargs() {
    test("cmdlineargs", vec!["hello", "world"],"3\nhello\nworld\n");
}

#[test]
fn test_atoi() {
    test("atoi", vec!["1000"],"1000\n");
}

#[test]
fn test_conditionals() {
    test("conditionals", Vec::new(),"False\nequal\nless\nnot greater\nless or equal\n");
}

#[test]
fn test_mc91() {
    test("mc91", Vec::new(),"91\n");
}

#[test]
fn test_enum() {
    test("enum", Vec::new(),"some\nnone\nsome\nnone\n");
}

fn test(source: &str, args: Vec<&str>, expected_output: &str) {
    let dir = TempDir::new("rasm_int_test").unwrap();

    let dest = format!("{}/{}", dir.path().to_str().unwrap(), source);

    let status = test_bin::get_test_bin("rasm")
        .arg(format!("resources/test/{}.rasm", source))
        .arg(&dest)
        .stderr(Stdio::inherit())
        .status()
        .expect("failed to start rasm");

    assert!(status.success());

    let failure_message = format!("failed to execute {}", source);

    let output = Command::new(&dest)
        .args(args)
        .stderr(Stdio::inherit())
        .output()
        .expect(&failure_message);

    assert!(output.status.success());

    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_output
    );
}