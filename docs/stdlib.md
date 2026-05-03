# Stdlib

## Common Patterns

### Working with Option/Result

```rasm
// Using Option
let maybeValue: Option<int> = Some(42);
match(maybeValue, fn(v) { v.mul(2); }, { 0; });

// Using Result
let result: Result<int, str> = Ok(42);
match(result, fn(v) { v; }, fn(e) { println(e); 0; });
```

### Functional Operations

```rasm
// Map - closure body ends with semicolon
let doubled = vec.map(fn(x) { x.mul(2); });

// Filter - closure body ends with semicolon
let evens = vec.filter(fn(x) { x.mod(2).eq(0); });

// Fold - closure body ends with semicolon
let sum = vec.foldLeft(0, fn(acc, x) { acc.add(x); });

// Enumerate
let indexed = vec.enumerate();
```

## String Operations

```rasm
let s = "hello";
```

## AST Builder Functions

Helper functions to construct AST, in stdlib:

```rasm
simpleASTCall("name", vecOf(args))         // function call
stringASTValue("hello")                    // string literal
integerASTValue(42)                        // int literal
booleanASTValue(true)                      // bool literal
ASTValueRefExpression("name")              // variable reference
```

## Stdlib Modules

Key modules from stdlib:

- `vec` - Vector operations (map, filter, fold, etc.)
- `list` - Linked list operations
- `str` - String utilities
- `option` - Option type helpers
- `result` - Result type helpers
- `math` - Mathematical functions
- `time` - Time/date operations
- `json` - JSON parsing
- `iter` - Iterator utilities
- `print` - Print macros
- `test` - Testing utilities