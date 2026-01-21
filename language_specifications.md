# RASM Language Specification

## Identifiers

An identifier must start with a letter and can contains only letters or numbers.

## Macro

There are two kinds of macros:

### Attribute macro

An attribute macro can be any function that has ASTStructDef or ASTEnumDef as first parameter, and primitive type parameters (int, float, char and str) amd return an AttributeMacroResult. to attach an attribute macro to a struct or enum, it must be prefixed with @ as in:  
`@toString()`  
`pub enum AStruct {}`

### Macro function

A macro function can be any function that has primitive type parameters (int, float, char and str), or ASTExpression parameters, or as a last argument, a vector (Vec) of ASTExpression and returns a MacroResult. While for the definition, it is a standard function, it is treated as a macro when called appending a bang (!) to the function name. If the macro function accepts a Vec of ASTExpression as the last parameter, we can call it passing any number of expressions to the function in substitution of the last parameter.

There can be two macro functions with the same name???

## Character escapes

The supported character escapes in char and string literals are:  
\n  
\r  
\t  
\"  
\'  
\NNN where N is an octal number
