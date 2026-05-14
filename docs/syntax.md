# Syntax

## Main

There is no main function in the language; statements outside a function/closure are treated as the "main function". This is a valid RASM program:

```rasm
println("Hello world")
```

## Built-in Types

`int`, `float`, `bool`, `str`, `char`

## Literals

```rasm
42          // int
3.14        // float
"hello"     // str
'a'         // char
true, false // bool
```

## Operators, predefined functions and macros

There are no operators, predefined functions, or macros in the language, except for the automatically defined functions for structs and enums (see below).

## Functions

```rasm
pub fn addTwo(n: int) -> int {
    add(n, 2) // the last statement of a function is the return value.
}

// generic function
pub fn addOne<T>(n: T) -> T {
    add(n, 1)
}

fn anInt() -> int { 0 }

// method: no return type
fn printName(name: str) {
    println("Name: ", name)
}
```

Generic types in generic functions cannot be constrained; they can be any type. The compiler will determine the types and the correctness of the function, depending on how it is called. In the above example, if we call it with an int, it will return an int, if a function "add" is defined that takes an int and returns an int, but if there is no such function, we'll get a compilation error.

## Native functions

Native functions can be defined as:

pub native print(s: str) /{

}/

in the language specification, it is not defined what can be the syntax of a native function, and in which language is written, it depends on the specific compiler.

## Function Calls

```rasm
let two = add(1, 1)

// syntactic sugar...
let three = two.add(1)

// generic function call, useful when the compiler cannot determine the type automatically
let v = evaluate<int>("10") // probably it's not possible to create such function, but as an example...
```

## Structs

```rasm
@toString() // this is an attribute macro (see below)
pub struct Pair<A,B> {
    first: A,
    second: B
}

let p = Pair("number", 1)
println(first(p))
// or with syntactic sugar...
println(p.first)

let p1 = p.second(2) // Pair("number", 2)
let p2 = p.second(fn(v) { v.add(1)}) // Pair("number", 2)
```

For every struct, a constructor function is automatically defined. It has the name of the struct, and the parameters are the attributes of the struct in the order defined; it returns the type of the struct.

```rasm
pub Pair<A,B>(first: A, second: B) -> Pair<A,B>
```

For every attribute in a struct, three functions are automatically defined:

- a getter function with the name of the attribute and a parameter which has the type of the struct.

```rasm
pub fn first<A,B>(s: Pair<A,B>) -> A
pub fn second<A,B>(s: Pair<A,B>) -> B
```

- a "setter" function with the name of the attribute, a parameter which has the type of the struct and another parameter which is the new value, it returns a new instance of the struct with the new attribute value.

```rasm
pub fn first<A,B>(s: Pair<A,B>, v: A) -> Pair<A,B>
pub fn second<A,B>(s: Pair<A,B>, v: B) -> Pair<A,B>
```

- a "setter" function with the name of the attribute, a parameter which has the type of the struct and another parameter which is a closure

```rasm
pub fn first<A,B>(s: Pair<A,B>, f: fn(oldValue: A) -> A) -> Pair<A,B>
pub fn second<A,B>(s: Pair<A,B>, f: fn(oldValue: B) -> B) -> Pair<A,B>
```

## Enums

```rasm
pub enum Option<T> {
    Some(value: T),
    None
}

pub enum Planet {
    Earth,
    Mars,
    Venus,
    Other
}

let v = Some(10)
let s = v.match(fn(v) { v.add(1)}, { 0}) // Some(11)

let planet = Earth() // or, for disambiguation... Planet::Earth()
println(planet.matchEarth({"it's the Earth"}, { "it's not the Earth"}))
```

For every enum, a function named "match" is automatically defined, taking a value of the enum type and one parameter for each variant (a closure with the variant's parameters in the order defined). For each variant, a function named "match" plus the variant name (capitalized) is automatically defined, taking a value of the enum type, a closure with the variant's parameters, and another parameterless closure.

```rasm
pub fn matchEarth<R>(planet: Planet, earth: fn() -> R, notEarth: fn() -> R) -> R
```

## Native types

```rasm
pub type Vec /{

}/
```

in the language specification, it is not defined what can be inside the body of a native type, it depends on the specific compiler.

## Variables

```rasm
let one = 1
let two = one.add(1)
```

## Closures

```rasm
fn(x) { x }
fn(x, y) { x.add(y) }
fn(accum, current) { accum.add(current) }
```

## Macros

Compile-time macros are invoked with `!` suffix. Examples from the stdlib:

```rasm
vec!(1, 2, 3)            // create vec from values
println!("Hello {}", x)  // print with format
print!("Value: {}", v)   // print without newline
```

Attribute macros auto-generate methods, from stdlib:

```rasm
@toString()
@eq()
pub struct Pair<A,B> {
    first: A,
    second: B
}
```

This generates `toString` and `eq` implementations automatically.

## Defining Macros

Macros are ordinary functions, but with specific parameters and return types.

There are two kinds of macros:

### Expression Macros

They receive AST expressions or primitive types, the last parameter can be a Vec of expressions:

```rasm
pub fn vec(exprs: Vec<ASTExpression>) -> MacroExpressionResult {
    exprs.first.match(
        fn(first) {
            let start = simpleASTCall("vecOf", vecOf(first))
            let result = exprs.enumerate.filter(fn(act) { act.index.greater(0) })
                .foldLeft(start, fn(prev, act) {
                    simpleASTCall("push", vecOf(prev, act.value))
                })
            MacroExpressionOk(result, Vec())
        },
        { MacroExpressionResult::MacroError("No values, use Vec()") }
    )
}
```

Returns `MacroExpressionOk(expr, functions)` or `MacroExpressionResult::MacroError(message)`.

### Statement Macros

Used for statements (like `println!`):

```rasm
pub fn println(s: str, exprs: Vec<ASTExpression>) -> MacroStatementResult {
    let parameters = vecOf(stringASTValue(s)).add(exprs)
    let f = format(s, exprs)
    f.match(fn(expr, functions) {
        let statement = ASTExpressionStatement(simpleASTCall("println", vecOf(expr)))
        MacroStatementOk(vecOf(statement), functions)
    }, fn(error) {
        MacroStatementResult::MacroError(error)
    })
}
```

Returns `MacroStatementOk(statements, functions)` or `MacroError(message)`.

### Attribute Macros

Attach to structs/enums with `@`:

```rasm
@toString()
pub struct Pair<A,B> { first: A, second: B }
```

The function receives an `ASTStructDef` or `ASTEnumDef`:

```rasm
pub fn toString(s: ASTStructDef) -> MacroAttributeResult {
    // generate toString function from struct definition
    let function = ASTFunctionDef(...)
    MacroAttributeOk(vecOf(function))
}
```

Returns `MacroAttributeOk(functions)` or `MacroError(message)`.
