use std::path::Path;
use std::process::{Command, Stdio};

use tempdir::TempDir;

#[cfg(test)]
#[test]
fn test_helloworld() {
    run_test("helloworld", Vec::new(), "Hello world\n");
}

#[test]
fn test_fibonacci() {
    run_test("fibonacci", vec!["10"], "55\n");
}

#[test]
fn test_inline() {
    run_test("inline", Vec::new(), "29\n");
}

#[test]
fn test_cmdlineargs() {
    run_test("cmdlineargs", vec!["hello", "world"], "3\nhello\nworld\n");
}

#[test]
fn test_atoi() {
    run_test("atoi", vec!["1000"], "1000\n");
}

#[test]
fn test_conditionals() {
    run_test("conditionals", Vec::new(), "False\nTrue\nequal\nnot equal\nless\nnot less\ngreater\nnot greater\nless or equal\nless or equal\nnot less or equal\n200\n100\n");
}

#[test]
fn test_mc91() {
    run_test("mc91", Vec::new(), "91\n");
}

#[test]
fn test_enum() {
    run_test(
        "enum",
        Vec::new(),
        "Some(a value)\nNone\nT2\n10\nSome(a value)\n",
    );
}

#[test]
fn test_lambda_0() {
    run_test("lambda", vec!["0"], "0\n");
}

#[test]
fn test_lambda_1() {
    run_test("lambda", vec!["1"], "1\n");
}

#[test]
fn test_lambda_2() {
    run_test("lambda", vec!["2"], "2\n");
}

#[test]
fn test_lambda_3() {
    run_test("lambda", vec!["3"], "3\n");
}

#[test]
fn test_lambda_params() {
    run_test("lambda_params", vec!["20"], "35\n");
}

#[test]
fn test_list() {
    run_test(
        "list",
        vec!["5"],
        "0,1,2,3,4\n4,3,2,1,0\n4\n0-A,1-B\nSome(20)\nNone\n",
    );
}

#[test]
fn test_list_map() {
    run_test("list_map", Vec::new(), "25,35\n");
}

#[test]
fn test_list_combine() {
    run_test("list_combine", Vec::new(), "6,6\n6,5,7,6\n");
}

#[test]
fn test_recurse() {
    run_test("recurse", Vec::new(), "5\n4\n3\n2\n1\n0\n");
}

#[test]
fn test_val() {
    run_test("val", vec!["10"], "10\n10\n10\n");
}

#[test]
fn test_val_in_lambda() {
    run_test("val_in_lambda", vec!["10"], "10\n");
}

#[test]
fn test_params_order() {
    run_test("params_order", vec![], "0\n1\n");
}

#[test]
fn test_list_fmap() {
    run_test("list_fmap", vec![], "0,5,1,5\n");
}

#[test]
fn test_list_fold() {
    run_test("list_fold", vec!["3"], "4\n10\n");
}

#[test]
fn test_function_as_arg() {
    run_test("function_as_arg", vec!["3"], "13\n");
}

#[test]
fn test_inner_context() {
    run_test("inner_context", vec![], "13\n");
}

#[test]
fn test_list_append() {
    run_test("list_append", vec![], "1,2\n");
}

#[test]
fn test_list_flatten() {
    run_test("list_flatten", vec![], "1,2,3,4\n");
}

#[test]
fn test_gameoflife_runs() {
    run(
        "resources/examples/gameoflife.rasm",
        vec!["resources/examples/simple.cells", "5"],
        None,
    );
}

#[test]
fn test_structs() {
    run_test("structs", vec![], "10, 20\n");
}

#[test]
fn test_read_file() {
    run_test(
        "read_file",
        vec!["resources/test/test.txt"],
        "hello\nworld\n",
    );
}

#[test]
#[ignore]
// TODO find a way to re enable it
fn test_malloc() {
    run_test(
        "malloc",
        vec![],
        "Some(1)\nSome(2)\nSome(3)\nSome(4)\nSome(5)\n24 bytes allocated\n",
    );
}

#[test]
fn test_list_filter() {
    run_test("list_filter", vec![], "0,1,2,3,4\n");
}

#[test]
fn test_print() {
    run_test(
        "print",
        Vec::new(),
        "Hello world\nHi\n10\n40\n45\none\ntwo\n",
    );
}

#[test]
fn test_str() {
    run_test(
        "str",
        Vec::new(),
        "😀\nHello world!\nHello world!\n12\nfirst\nsecond\nthird\n1\n3\n4\ntrue\nfalse\nfalse\nfalse\n9999\n",
    );
}

#[test]
fn test_libc() {
    run_test("libc", Vec::new(), "number:\t\t10\nnumber:\t\t10\n");
}

#[test]
fn test_lines() {
    run_test("lines", Vec::new(), "first,second,third\n");
}

#[test]
fn test_chars() {
    run_test("chars", Vec::new(), "a\nè\n😀\ntrue\nfalse\nH,e,l,l,o\n");
}

#[test]
fn test_let() {
    run_test("let", vec![], "20\n20,10\n10\nè\ntrue\na string\n");
}

#[test]
fn test_let1() {
    run_test("let1", vec![], "10,\n10,\n10\n5\n");
}

#[test]
fn test_dereference() {
    run_test(
        "dereference",
        vec![],
        "true\ntrue\nfalse\nfalse\ntrue\ntrue\n",
    );
}

#[test]
fn test_vec() {
    run_test(
        "vec",
        vec![],
        "0,1,\n10,11,\ntrue\nfalse\ntrue\nfalse\n1\n6\n2\n0,1,2,3,4,5,\n5,\n10,15,21,2,2,1,4,5,\n10\n1,2,3,\n1,11,2,12,\n0,1,2,\n10\nNone\nSome(Some(Hello))\n1,2,\n",
    );
}

#[test]
fn test_vec_combine() {
    run_test("vec_combine", vec![], "6,6,\n0,1,2,1,2,3,\n");
}

#[test]
fn test_vec1() {
    run_test(
        "vec1",
        vec![],
        "1,2,3,4,\nfalse\nfalse\ntrue\n1,2,3,\n1,2,3,\n1,2,3,\n1,2,3,\n1,2,3,\n1,2,3,\n1,2,3,\n",
    );
}

#[test]
fn test_function_overloading() {
    run_test(
        "function_overloading",
        vec![],
        "a number\nHello\nHello world\n11\n",
    );
}

#[test]
fn test_type_check_1() {
    run_test("type_check_1", vec![], "a string\n");
}

#[test]
fn test_println() {
    run_test("println", vec![], "a string\n10\n");
}

#[test]
fn test_const() {
    run_test("const", vec![], "10\nè\ntrue\na string\n");
}

#[test]
fn test_write_file() {
    run_test("write_file", vec![], "Hello world\n1000\n");
}

#[test]
fn test_lambda_as_return_value() {
    run_test(
        "lambda_as_return_value",
        vec![],
        "2,3\n0,1\nSome(2),Some(2)\nSome(2),Some(2)\n",
    );
}

#[test]
fn test_lambda_in_struct() {
    run_test("lambda_in_struct", vec![], "1,1\n2,2\n3,3\n4,4\n5,5\n6,6\n");
}

#[test]
fn test_lambda_in_enum() {
    run_test("lambda_in_enum", vec![], "One 11\nTwo 9\n");
}

#[test]
fn test_root() {
    run_test("root", vec![], "Less\nEqual\nGreater\n");
}

// Compile tests

#[test]
fn test_gameoflife_sdl_compile() {
    compile_example("resources/examples/gameoflife_sdl.rasm");
}

#[test]
fn test_gameoflife_vec_compile() {
    compile_example("resources/examples/gameoflife_vec.rasm");
}

#[test]
fn test_gameoflife_vec_sdl_compile() {
    compile_example("resources/examples/gameoflife_vec_sdl.rasm");
}

#[test]
fn test_breakout() {
    compile_example("resources/examples/breakout/breakout.rasm");
}

#[test]
fn test_showimage() {
    compile_example("resources/examples/showimage.rasm");
}

fn run_test(test_name: &str, args: Vec<&str>, expected_output: &str) {
    let dir = TempDir::new("rasm_int_test").unwrap();
    let executable = compile(&dir, &format!("resources/test/{}.rasm", test_name));
    execute(&executable, args, Some(expected_output));
}

fn run(source: &str, args: Vec<&str>, expected_output: Option<&str>) {
    let dir = TempDir::new("rasm_int_test").unwrap();
    let executable = compile(&dir, source);
    execute(&executable, args, expected_output);
}

fn compile_example(source: &str) {
    let dir = TempDir::new("rasm_int_test").unwrap();
    compile(&dir, source);
}

fn compile(dir: &TempDir, source: &str) -> String {
    let source_without_extension = Path::new(source).with_extension("");
    let file_name = source_without_extension
        .file_name()
        .unwrap()
        .to_str()
        .unwrap();
    let dest = format!("{}/{}", dir.path().to_str().unwrap(), file_name);

    let status = test_bin::get_test_bin("rasm")
        .arg(source)
        .arg(&dest)
        .stderr(Stdio::inherit())
        .status()
        .expect("failed to start rasm");

    assert!(status.success());

    dest
}

fn execute(exucutable: &str, args: Vec<&str>, expected_output: Option<&str>) {
    let failure_message = format!("failed to execute {}", exucutable);

    let output = Command::new(exucutable)
        .args(args)
        .stderr(Stdio::inherit())
        .output()
        .expect(&failure_message);

    assert!(output.status.success());

    if let Some(eo) = expected_output {
        assert_eq!(String::from_utf8_lossy(&output.stdout), eo);
    }
}
