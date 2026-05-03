# Common Errors & Solutions

## Missing semicolons

- Every single statement must end with a semicolon (`;`) - this includes statements inside closures and statements outside functions and closures
- no exceptions: even single statement inside functions or closures need a semicolon
- common error: "Unexpected end of block" or "Found semicolon without an expression"

## "No such file or directory"

- Ensure dependencies are installed. For the examples in this project run:
  `./install_libs.sh`

## SDL test failures in this project

- Install SDL libraries (32 bit for nasmi386 target) or run with
  `SKIP_SDL_TESTS=true`

## Linker errors

- Ensure `gcc-multilib g++-multilib libc++-dev nasm` are installed

## Macro errors

- Check that macro functions return correct type (`MacroExpressionOk`,
  `MacroStatementOk`, etc.)
- Use `simpleASTCall` to generate function calls
- Use `stringASTValue`, `integerASTValue`, etc. for literals