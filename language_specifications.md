# RASM Language Specification

## Macro

A macro function can be any function that has primitive type parameters, or ASTExpression parameters (if there are more as the last parameters, they are treated as a Vec) and returns a MacroResult. There cannot be two macro functions with the same name.
