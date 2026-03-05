# RASM Compiler Specification

## Profiles and namespaces

test and main profiles are special profiles, for those profiles the namespace does not include the profile name itself, so a private symbol in main can be seen in the same "file" in test, it does not happen for the custom profiles.

## Native source macros

native source macro are macros that can be used inside the native code. They are predefined, and based on the target. To call a macro use the $ as a prefix for the macro name and arguments within round paranthesis.

Arguments are divided in three types:

* plain, it can be anything, but if it must be typed, you can add a : and specify the type, that can be also generic. If the type is not specified, but is needed by the macro, it is an int
* a reference to a function parameter, it is the name of a parameter prefixed with $, in this case the type is inferred by the compiler
* string literal

For example to call a rasm function (this macro is present in both nasmi386 and C target), you can write `$call(aFunctionName, "Hello world", $par1, localVariable: Option<T>)`, the first parameter is a plain, not typed argument, the second is a reference to the outer function's parameter and the third is a local variable for which we specify the rasm type, generic on T which is a generic type of the outer function.

### native C source macros

#### $call

calls a rasm function

takes a plain argument for the name of the function to call and other plain typed arguments, that are passed to the function. In theory the arguments to pass to the function can be expressions, but you cannot use expressions that contain parenthesis or commas, in nasm they could be registers, or register expressions, i.e. eax, [ebx + 5].
An example in C:
`$call(aFunctionName, "Hello world", $par1, localVariable: Option<T>)`

#### $include

adds an #include directive

#### $structDeclaration

allocates a new struct  
takes a plain argument with the prefix of the var name to declare  
it is intended to be used inside a function that has a struct as return type. For example in:

```C
fn aFunction() -> AStruct /{
    $structDeclaration(var_name)
}/
```

the macro will be expanded to:

```C
struct RasmPointer_* var_name = rasmMalloc(sizeof(struct C_struct_name));
```

#### $enumVariantDeclaration

#### $enumVariantAssignment

#### $enumVariant

#### $addRef

adds a reference

takes a typed argument with the expression  

#### $deref

dereference

takes a typed argument with the expression  

#### $typeName

returns the C type of a rasm type  
takes a plain argument that can be generic  
for example for a rasm struct it returns a pointer to the C struct name  

#### $typeNameNoRef

returns the C type of a rasm type, but not as a pointer
takes a plain argument that can be generic  
for example for a rasm struct it returns the C struct name  

#### $realTypeName

returns the "underlying" C type of a rasm type, for example for a rasm struct it returns a RasmPointer_  
takes one plain argument with the type name, that can be generic

#### $castAddress

cast an expression to the C type  
takes a typed argument with the expression  

#### $enumSimple

#### $isRef

creates an if statement based on the fact that a type is a reference type  
takes a plain argument with the type name, that can be generic, a string to return if true and a string to return if false  
it is often used to comment out a block of code if a type is not a reference one, using two macros. Example:  

```C
$isRef(T, "", "/*")
    $realTypeName(T) *array = ($realTypeName(T) *)vector->values->address;
    for (int i=0; i < vector->length; i++) {
        $addRef(array[i]: T)
    }
$isRef(T, "", "*/")
```

#### $inline

inlines the function
