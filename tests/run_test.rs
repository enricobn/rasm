use std::process::{Command, Stdio};

#[cfg(test)]

#[test]
fn test_helloworld() {
    test("helloworld", Vec::new(), "Hello world\nHi\n10\n40\n45\none\ntwo\n");
}

#[test]
fn test_fibonacci() {
    test("fibonacci", Vec::new(),"102334155\n");
}

#[test]
fn test_inline() {
    test("inline", Vec::new(),"5\n");
}

fn test(source: &str, args: Vec<&str>, expected_output: &str) {
    let status = test_bin::get_test_bin("rasm")
        .arg(format!("resources/test/{}.rasm", source))
        .arg(format!("target/tmp/{}", source))
        .stderr(Stdio::inherit())
        .status()
        .expect("failed to start rasm");

    assert!(status.success());

    let mut output = Command::new(format!("target/tmp/{}", source))
        .args(args)
        .stderr(Stdio::inherit())
        .output()
        .expect("failed to execute fibonacci");

    assert!(output.status.success());

    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_output
    );
}