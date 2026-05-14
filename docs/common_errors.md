# Common Errors & Solutions

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
