use std::path::Path;
use std::process::{Command, Stdio};

use ntest::timeout;
use rasm_core::codegen::c::options::COptions;
use rasm_core::codegen::compile_target::{CompileTarget, C, NASMI386};
use rasm_core::codegen::AsmOptions;
use tempdir::TempDir;

#[cfg(test)]
#[test]
fn test_helloworld() {
    run_test("helloworld", Vec::new(), "Hello world\n");
}

/*
#[test]
fn test_helloworld_c() {
    run_test_with_target("helloworld", Vec::new(), "Hello world\n", CompileTarget::C);
}

 */

#[test]
fn test_fibonacci() {
    run_test("fibonacci", vec!["10"], "55\n");
}

/*
#[test]
fn test_fibonacci_c() {
    run_test_with_target("fibonacci", vec!["10"], "55\n", CompileTarget::C);
}

 */

/*
#[test]
fn test_fibonacci_fixed_c() {
    run_test_with_target("fibonacci_fixed", vec![], "55\n", CompileTarget::C);
}

 */

#[test]
fn test_inline() {
    run_test("inline", Vec::new(), "29\n");
}

#[test]
fn test_cmdlineargs() {
    run_test("cmdlineargs", vec!["hello", "world"], "3\nhello\nworld\n");
}

#[test]
fn test_str_toi32() {
    run_test("str_toi32", vec!["1000"], "Some(1000)\nNone\n");
}

#[test]
fn test_conditionals() {
    run_test("conditionals", Vec::new(), "False\nTrue\nequal\nnot equal\nless\nnot less\ngreater\nnot greater\nless or equal\nless or equal\nnot less or equal\n200\n100\n");
}

#[test]
fn test_conditionals2() {
    run_test("conditionals2", Vec::new(), "Some(first),Some(first),Some(first),\nSome(second),Some(second),Some(second),\nSome(third),Some(third),Some(third),\n");
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
        "Some(a value)\nNone\nT1\n10\n20\nSome(a value 2)\nT2\n10\nSome(a value 3)\n",
    );
}

/*
#[test]
fn test_enum_c() {
    run_test_with_target(
        "enum",
        Vec::new(),
        "Some(a value)\nNone\nT1\n10\n20\nSome(a value 2)\nT2\n10\nSome(a value 3)\n",
        CompileTarget::C,
    );
}

 */

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
fn test_lambda1() {
    run_test("lambda1", vec![], "10\n");
}

/*
#[test]
fn test_lambda1_c() {
    run_test_with_target("lambda1", vec![], "10\n", CompileTarget::C);
}

 */

#[test]
fn test_lambda2() {
    run_test("lambda2", vec![], "Some(10)\n");
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
#[ignore] // it has no sense, we cannot rely on the evaluation order of parameters
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
        "resources/examples/gameoflife",
        vec!["resources/examples/simple.cells", "5"],
        None,
    );
}

#[test]
fn test_structs() {
    run_test("structs", vec![], "10, 20\n");
}

/*
#[test]
fn test_structs_c() {
    run_test_with_target("structs", vec![], "10, 20\n", CompileTarget::C);
}

 */

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
        "😀\nHello world!\nHello world!\n12\nfirst\nsecond\nthird\ntrue\nfalse\nfalse\nfalse\n9999\n     Hello\n-10\na'a\na\"a\na\na\nHello1è☀😀world\n",
    );
}

#[test]
#[ignore] // it needs external printf, but for now there's not a support to supply it to a "single file project"
fn test_libc() {
    run_test("libc", Vec::new(), "number:\t\t10\nnumber:\t\t10\n");
}

#[test]
fn test_lines() {
    run_test("lines", Vec::new(), "first,second,third\n3\nSome(first)\n");
}

#[test]
fn test_chars() {
    run_test("chars", Vec::new(), "a\nè\n😀\ntrue\nfalse\n'\nH,e,l,l,o\n");
}

#[test]
fn test_let() {
    run_test("let", vec![], "20\n20,10\n10\nè\ntrue\na string\n");
}

#[test]
fn test_let_as_return() {
    run_test("let_as_return", vec![], "5\n");
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
        "0,1,\n10,11,\ntrue\nfalse\ntrue\nfalse\n1\n6\n2\n0,1,2,3,4,5,\n5,\n10,15,21,2,2,1,4,5,\n10\n1,2,3,\n1,11,2,12,\n0,1,2,\n10\nNone\nSome(Some(Hello))\n1,2,\nSome(2)\n1,\nfalse\ntrue\n",
    );
}

#[test]
fn test_vec_simple() {
    run_test(
        "vec_simple",
        vec![],
        "2\nSome(10)\n2\nSome(Hello)\n2\nSome(Some(Hello))\nSome(Hello)\n",
    );
}

/*
#[test]
fn test_vec_simple_c() {
    run_test_with_target(
        "vec_simple",
        vec![],
        "2\nSome(10)\n2\nSome(Hello)\n2\nSome(Some(Hello))\nSome(Hello)\n",
        CompileTarget::C,
    );
}

 */

#[test]
fn test_vec2() {
    run_test("vec2", vec![], "\n\n");
}

#[test]
fn test_vec3() {
    run_test("vec3", vec![], "1\n😀\nè\n5\n4\n");
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
        "1,2,3,4,\nfalse\nfalse\ntrue\n1,2,3,\n1,2,3,\n1,2,3,\n1,2,3,\n1,2,3,\n1,2,3,\n1,2,3,\n10,20,\n20\n0\n",
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
fn test_type_check_2() {
    run_test("type_check_2", vec![], "true\n");
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
        "2,3\n0,1\nSome(1),Some(1)\nSome(1),Some(1)\n",
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

#[test]
fn test_f32_nasm() {
    run_test_with_target("f32", vec![], "-7.375000000\n-7.400000092\n-1.375000000\n-1.375432131\n-1.123456712\n-0.375000000\n-0.399999973\n-0.375432071\n0.000000000\n10.000000000\n20.100000379\n20.000999449\n-0.000000000\n-10.000000000\n-20.100000379\n-20.000999449\n10.000000000\n123456.000000000\n-10.000000000\n-123456.000000000\nSome(10)\nSome(-10)\n2.000000000\n5.000000000\n10.199999806\n9.800000188\n17.500000000\n8.199999806\n-8.199999806\ntrue\nfalse\nfalse\ntrue\nfalse\ntrue\ntrue\nfalse\n7.500000000\n7.500000000\n",
    CompileTarget::Nasmi386(AsmOptions::default()));
}

#[test]
fn test_f32_c() {
    run_test_with_target("f32", vec![], "-7.375000000\n-7.400000095\n-1.375000000\n-1.375432134\n-1.123456717\n-0.375000000\n-0.400000006\n-0.375432104\n0.000000000\n10.000000000\n20.100000381\n20.000999451\n0.000000000\n-10.000000000\n-20.100000381\n-20.000999451\n10.000000000\n123456.000000000\n-10.000000000\n-123456.000000000\nSome(10)\nSome(-10)\n2.000000000\n5.000000000\n10.199999809\n9.800000191\n17.500000000\n8.199999809\n-8.199999809\ntrue\nfalse\nfalse\ntrue\nfalse\ntrue\ntrue\nfalse\n7.500000000\n7.500000000\n",
    CompileTarget::C(COptions::default()));
}

#[test]
fn test_uniform() {
    run_test("uniform", vec![], "30\n60\n60\n200\n10\n30\n");
}

#[test]
fn test_structpropertysetter() {
    run_test(
        "structpropertysetter",
        vec![],
        "p (10,20)\np1 (15,20)\np2 (30,20)\np (10,20)\nmoved (110,120)\n",
    );
}

#[test]
fn test_allocation() {
    run_test_with_target(
        "allocation",
        vec![],
        "56 bytes allocated and referenced\n1,2\n56 bytes allocated and referenced\n",
        CompileTarget::Nasmi386(AsmOptions::default()),
    );
    // TODO C
}

#[test]
//#[timeout(1000)]
fn test_oop() {
    run_test(
        "oop",
        vec![],
        "Circle(1,1,2),Rect(10,20,5,5),Point(4,5),\nCircle(101,101,2),Rect(110,120,5,5),Point(104,105),\nPoint(1,1),Point(12,22),Point(4,5),\n",
    );
}

#[test]
//#[timeout(1000)]
fn test_oopv2() {
    run_test(
        "oopv2",
        vec![],
        "Circle(1,1,2),Rect(10,20,5,5),Point(4,5),\nCircle(101,101,2),Rect(110,120,5,5),Point(104,105),\nPoint(1,1),Point(12,22),Point(4,5),\n",
    );
}

#[test]
fn test_namespaces() {
    run_test("namespaces", vec![], "str ok\ni32 ok\nstr ok\ni32 ok\n");
}

#[test]
fn test_option() {
    run_test(
        "option",
        vec![],
        "Some(value = 2)\nSome(10)\nSome(1)\nSome(10)\nSome(1)\n10\n",
    );
}

#[test]
fn test_error_handling() {
    run_test(
        "error_handling",
        vec!["/tmp/error_handling.txt"],
        "c\nHello\n",
    );
}

#[test]
fn test_error_handling1() {
    run_test_with_target(
        "error_handling",
        vec!["unknown_folder/error_handling.txt"],
        "IOError(Error writing to file.)\n",
        CompileTarget::Nasmi386(AsmOptions::default()),
    );
}

#[test]
fn test_error_handling1_c() {
    run_test_with_target(
        "error_handling",
        vec!["unknown_folder/error_handling.txt"],
        "IOError(Bad file descriptor)\n",
        CompileTarget::C(COptions::default()),
    );
}

#[test]
fn test_list_pop_replace() {
    run_test(
        "list_pop_replace",
        Vec::new(),
        "Some(25)\n10,20\nSome(25)\n10,20,30\n",
    );
}

#[test]
fn test_result() {
    run_test(
        "result",
        Vec::new(),
        "ok(0)\nerr: An error\nok(2)\nok(value=0)\n",
    );
}

#[test]
fn test_if() {
    run_test("if", Vec::new(), "");
}

#[test]
fn test_curry() {
    run_test("curry", Vec::new(), "");
}

#[test]
fn test_assertions() {
    run_test("assertions", Vec::new(), "");
}

/*
#[test]
fn test_assertions_c() {
    run_test_with_target("assertions", Vec::new(), "", CompileTarget::C);
}

 */

#[test]
fn test_single_match() {
    run_test("single_match", Vec::new(), "Found\nNotFound\n");
}

#[test]
fn test_call_with_generics() {
    run_test("call_with_generics", Vec::new(), "Ok\nError: an error\n");
}

#[test]
fn test_printref() {
    run_test_no_verify("printref", Vec::new());
}

#[test]
fn test_structpropertysetter1() {
    run_test(
        "structpropertysetter1",
        Vec::new(),
        "p (Some(10),Some(20))\np1 (Some(20),Some(20))\n",
    );
}

#[test]
fn test_escape() {
    run_test("escape", vec![], "true\n'\n\"\n");
}

#[test]
fn test_test() {
    run_test_no_verify("test", vec![]);
}

// Compile tests

#[test]
fn test_gameoflife_compile() {
    compile_example("resources/examples/gameoflife", true);
}

#[test]
fn test_gameoflife_sdl_compile() {
    compile_example("resources/examples/gameoflife_sdl", true);
}

#[test]
fn test_gameoflife_vec_compile() {
    compile_example("resources/examples/gameoflife_vec", true);
}

#[test]
fn test_gameoflife_vec_sdl_compile() {
    compile_example("resources/examples/gameoflife_vec_sdl", true);
}

#[test]
//#[timeout(5000)]
fn test_breakout() {
    compile_example("resources/examples/breakout", true);
}

#[test]
fn test_showimage() {
    compile_example("resources/examples/showimage", true);
}

#[test]
fn test_bouncing_quads() {
    compile_example("resources/examples/bouncing_quads", true);
}

fn run_test(test_name: &str, args: Vec<&str>, expected_output: &str) {
    run_test_with_target(
        test_name,
        args.clone(),
        expected_output,
        CompileTarget::Nasmi386(AsmOptions::default()),
    );
    run_test_with_target(
        test_name,
        args,
        expected_output,
        CompileTarget::C(COptions::default()),
    );
}

fn run_test_with_target(
    test_name: &str,
    args: Vec<&str>,
    expected_output: &str,
    target: CompileTarget,
) {
    let dir = TempDir::new("rasm_int_test").unwrap();

    let mut main = format!("resources/test/{}", test_name);

    if !Path::new(&main).is_dir() {
        main = format!("{main}.rasm");
    }

    let executable = compile_with_target(&dir, &main, false, target);
    execute(&executable.unwrap(), args, Some(expected_output));
}

fn run_test_no_verify(test_name: &str, args: Vec<&str>) {
    let dir = TempDir::new("rasm_int_test").unwrap();

    let mut main = format!("resources/test/{}", test_name);

    if !Path::new(&main).is_dir() {
        main = format!("{main}.rasm");
    }

    let executable = compile(&dir, &main, false);
    execute(&executable.unwrap(), args, None);
}

fn run(source: &str, args: Vec<&str>, expected_output: Option<&str>) {
    let dir = TempDir::new("rasm_int_test").unwrap();
    let executable = compile(&dir, source, false);
    execute(&executable.unwrap(), args.clone(), expected_output);
    let executable =
        compile_with_target(&dir, source, false, CompileTarget::C(COptions::default()));
    execute(&executable.unwrap(), args, expected_output);
}

fn compile_example(source: &str, only_compile: bool) {
    let dir = TempDir::new("rasm_int_test").unwrap();
    compile(&dir, source, only_compile);
    compile_with_target(
        &dir,
        source,
        only_compile,
        CompileTarget::C(COptions::default()),
    );
}

fn compile(dir: &TempDir, source: &str, only_compile: bool) -> Option<String> {
    compile_with_target(
        dir,
        source,
        only_compile,
        CompileTarget::Nasmi386(AsmOptions::default()),
    )
}

fn compile_with_target(
    dir: &TempDir,
    source: &str,
    only_compile: bool,
    target: CompileTarget,
) -> Option<String> {
    let source_without_extension = Path::new(source).with_extension("");
    let file_name = source_without_extension
        .file_name()
        .unwrap()
        .to_str()
        .unwrap();
    let dest = format!("{}/{}", dir.path().to_str().unwrap(), file_name);

    let target_name = match target {
        CompileTarget::Nasmi386(_) => NASMI386,
        CompileTarget::C(_) => C,
    };

    let mut args = vec![
        "build".to_string(),
        source.to_owned(),
        "-o".to_string(),
        dest.clone(),
        "-t".to_string(),
        target_name.to_string(),
    ];

    if only_compile {
        // it's needed for running some tests on github since we are not able to link
        args.push("--compile".to_owned());
    }

    let output = test_bin::get_test_bin("rasm")
        .env(
            "RASM_STDLIB",
            Path::new("../stdlib")
                .canonicalize()
                .unwrap()
                .to_str()
                .unwrap(),
        )
        .args(args)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .expect("failed to start rasm");

    assert!(output.status.success());

    Some(dest)
}

fn execute(executable: &str, args: Vec<&str>, expected_output: Option<&str>) {
    let failure_message = format!("failed to execute {}", executable);

    let output = Command::new(executable)
        .args(args)
        .stderr(Stdio::inherit())
        .output()
        .expect(&failure_message);

    if !output.status.success() {
        println!("{}", String::from_utf8_lossy(&output.stdout));
    }
    assert!(output.status.success());

    if let Some(eo) = expected_output {
        assert_eq!(String::from_utf8_lossy(&output.stdout), eo);
    }
}
