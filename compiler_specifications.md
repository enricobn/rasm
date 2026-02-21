# RASM Compiler Specification

## Profiles and namespaces

test and main profiles are special profiles, for those profiles the namespace does not include the profile name itself, so a private symbol in main can be seen in the same "file" in test, it does not happen for the custom profiles.

## C macros

parameters are divided in three types:

* plain, it can be anything, but if it must be typed you can add a : and specify the type, that can be even generic. If the type is not specified, but is needed by the macro, it is an int
* a reference to a function parameter, it is the name of a parameter prefixed with $, in this case the type is inferred by the compiler
* string literal

### $call

calls a rasm function

### $include

adds an #include directive

### $structDeclaration

creates a new struct  
takes a plain argument with the prefix of the var names to declare  
it is supposed to be used inside a function that has a struct as return type. An example of the result:  

```C
struct RasmPointer_*{var_name} = rasmMalloc(sizeof(struct {safe_name}));
struct {safe_name}*{var_name}_ = (struct {safe_name} *){var_name}->address;
```

### $structType

### $enumVariantDeclaration

### $enumDeclaration

### $enumVariantAssignment

### $enumVariant

### $addRef

adds a reference
there are three versions:  

* takes a typed expression
* takes a typed expression and a plain parameter with the expression  
  the type is determined by the typed expression, but the expression, to which add a reference, is determined by the plain parameter
* takes a typed expression, a plain parameter with the expression and a number for the index of a lambda parameter
  the typed expression determines the type of a lambda, the expression is the second parameter and the third is the
  lambda parameter index from which we determine the type of the expression. For example:  
  
    ```C
    $addRef($f, struct_result_->{name}, 0);
    ```  

    f is a lambda parameter, then we have the expression to which add the reference, 0 means the firs argument of that lambda. So we add areference to `struct_result_->{name}`, given the type of the first parameter of the lambda type

### $deref

dereference
look at addRef

### $typeName

returns the C type of a rasm type  
takes a plain argument that can be generic  
for example for a rasm struct it returns the C struct name  

### $realTypeName

returns the "underlying" C type of a rasm type, for example for a rasm struct it returns a RasmPointer_  
there are three versions:  

* a plain argument that can be generic  
* a reference to a parameter, from which it takes only the type
* a reference to a parameter that is a lambda and a number as the zero based index of lambda argument, from which  
  it takes the type

### $castAddress

cast an expression to the C type  
takes a typed argument of a C expression  

### $enumSimple

### $isRef

creates an if statement based on the fact that a type is a reference type  
takes a plain argument with the rasm type, a string to return if true and a string to return if false  
it is often used to comment out a block of code if a type is not a reference one, using two macros. Example:  

```C
$isRef(T, "", "/*")
    $realTypeName(T) *array = ($realTypeName(T) *)vector->values->address;
    for (int i=0; i < vector->length; i++) {
        $addRef(array[i]: T)
    }
$isRef(T, "", "*/")
```

### $inline

inlines the function
