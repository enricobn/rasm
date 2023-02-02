use std::process::{Command, Stdio};

use tempdir::TempDir;

#[cfg(test)]
#[test]
fn test_helloworld() {
    test("helloworld", Vec::new(), "Hello world\n");
}

#[test]
fn test_fibonacci() {
    test("fibonacci", vec!["10"], "55\n");
}

#[test]
fn test_inline() {
    test("inline", Vec::new(), "29\n");
}

#[test]
fn test_cmdlineargs() {
    test("cmdlineargs", vec!["hello", "world"], "3\nhello\nworld\n");
}

#[test]
fn test_atoi() {
    test("atoi", vec!["1000"], "1000\n");
}

#[test]
fn test_conditionals() {
    test("conditionals", Vec::new(),"False\nTrue\nequal\nnot equal\nless\nnot less\ngreater\nnot greater\nless or equal\nless or equal\nnot less or equal\n200\n100\n");
}

#[test]
fn test_mc91() {
    test("mc91", Vec::new(), "91\n");
}

#[test]
fn test_enum() {
    test(
        "enum",
        Vec::new(),
        "Some(a value)\nNone\nT2\n10\nSome(a value)\n",
    );
}

#[test]
fn test_lambda_0() {
    test("lambda", vec!["0"], "0\n");
}

#[test]
fn test_lambda_1() {
    test("lambda", vec!["1"], "1\n");
}

#[test]
fn test_lambda_2() {
    test("lambda", vec!["2"], "2\n");
}

#[test]
fn test_lambda_3() {
    test("lambda", vec!["3"], "3\n");
}

#[test]
fn test_lambda_params() {
    test("lambda_params", vec!["20"], "35\n");
}

#[test]
fn test_list() {
    test("list", vec!["5"], "0,1,2,3,4\n4,3,2,1,0\n4\n0-A,1-B\n");
}

#[test]
fn test_list_map() {
    test("list_map", Vec::new(), "25,35\n");
}

#[test]
fn test_recurse() {
    test("recurse", Vec::new(), "5\n4\n3\n2\n1\n0\n");
}

#[test]
fn test_val() {
    test("val", vec!["10"], "10\n10\n10\n");
}

#[test]
fn test_val_in_lambda() {
    test("val_in_lambda", vec!["10"], "10\n");
}

#[test]
fn test_params_order() {
    test("params_order", vec![], "0\n1\n");
}

#[test]
fn test_list_fmap() {
    test("list_fmap", vec![], "0,5,1,5\n");
}

#[test]
fn test_list_fold() {
    test("list_fold", vec!["3"], "4\n10\n");
}

#[test]
fn test_function_as_arg() {
    test("function_as_arg", vec!["3"], "13\n");
}

#[test]
fn test_inner_context() {
    test("inner_context", vec![], "13\n");
}

#[test]
fn test_list_append() {
    test("list_append", vec![], "1,2\n");
}

#[test]
fn test_list_flatten() {
    test("list_flatten", vec![], "1,2,3,4\n");
}

#[test]
fn test_gameoflife_runs() {
    test_("gameoflife", vec!["examples/simple.cells", "5"], None);
}

#[test]
fn test_structs() {
    test("structs", vec![], "10, 20\n");
}

#[test]
fn test_read_file() {
    test(
        "read_file",
        vec!["resources/test/test.txt"],
        "hello\nworld\n",
    );
}

#[test]
#[ignore]
// TODO find a way to re enable it
fn test_malloc() {
    test(
        "malloc",
        vec![],
        "Some(1)\nSome(2)\nSome(3)\nSome(4)\nSome(5)\n24 bytes allocated\n",
    );
}

#[test]
fn test_list_filter() {
    test("list_filter", vec![], "0,1,2,3,4\n");
}

#[test]
fn test_print() {
    test(
        "print",
        Vec::new(),
        "Hello world\nHi\n10\n40\n45\none\ntwo\n",
    );
}

#[test]
fn test_str() {
    test(
        "str",
        Vec::new(),
        "😀\nHello world!\nHello world!\n12\nfirst\nsecond\nthird\n1\n3\n4\ntrue\nfalse\nfalse\nfalse\n",
    );
}

#[test]
fn test_libc() {
    test("libc", Vec::new(), "number:\t\t10\nnumber:\t\t10\n");
}

#[test]
fn test_lines() {
    test("lines", Vec::new(), "first,second,third\n");
}

#[test]
fn test_chars() {
    test("chars", Vec::new(), "a\nè\n😀\ntrue\nfalse\nH,e,l,l,o\n");
}

#[test]
fn test_let() {
    test("let", vec![], "20\n20,10\n10\nè\ntrue\n");
}

#[test]
fn test_dereference() {
    test(
        "dereference",
        vec![],
        "true\ntrue\nfalse\nfalse\ntrue\ntrue\n",
    );
}

#[test]
fn test_vec() {
    test(
        "vec",
        vec![],
        "0,1,\n10,11,\ntrue\nfalse\ntrue\nfalse\n1\n6\n2\n0,1,2,3,4,5,\n5,\n10,15,21,2,2,1,4,5,\n10\n1,2,3,\n1,11,2,12,\n0,1,2,1,2,3,\n0,1,2,\n10\n",
    );
}

#[test]
fn test_function_overloading() {
    test(
        "function_overloading",
        vec![],
        "a number\nHello\nHello world\n11\n",
    );
}

#[test]
fn test_type_check_1() {
    test("type_check_1", vec![], "a string\n");
}

#[test]
fn test_println() {
    test("println", vec![], "a string\n10\n");
}

fn test(source: &str, args: Vec<&str>, expected_output: &str) {
    test_(source, args, Some(expected_output));
}

fn test_(source: &str, args: Vec<&str>, expected_output: Option<&str>) {
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

    if let Some(eo) = expected_output {
        assert_eq!(String::from_utf8_lossy(&output.stdout), eo);
    }
}
