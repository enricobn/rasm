**Lambda functions**  
When in codegen we call a function (a_function) that has a lambda as parameter, 
we must allocate memory to store the address to the lambda function and then all the values in the context.
For example if we have a lambda called lambda1 (the mnemonic address to the function to call) and values v1 and v2 in the context
we allocate 3 (* word) bytes in memory then we store the address and the values:
[m] = lambda1
[m + word] = v1
[m + 2 * word] = v2

Then we do not pass the mnemonic address (lambda1) to the function (a_function), but m
so the function, in order to call the lambda must get the real address from [m]
and before the function calls the lambda, it must push m so the lambda can access to the context variables

in the lambda1 to access to the variables it should use [m + word] (v1) and [m + 2 * word] (v2) 

**Inline assembly**  
in inline asm, use EAX only to store the final result, because it is used to store the value of the lambda address when 
inlining functions inside lambda




