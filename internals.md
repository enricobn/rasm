**Lambda functions**  
lambda functions has parameters according to the context.  
When in codegen we call a function that has lambda as parameters, add to the stack the values of the context variables,
pay attention that you must pass the context variables in reverse order, then the normal parameters,
so when the function consumes the parameters, the parameter at 4+4 is
the real first parameter. Example:
if(0, { nadd(n1, n2);}, { nadd(n2, n3); }}
we push: n3,n2,n1,lambda2,lambda1,0
// the real parameters
[ebp+4+4]=0
[ebp+4+8]=lambda1
[ebp+4+12]=lambda2
[ebp+4+16]=n1
[ebp+4+20]=n2
[ebp+4+24]=n3

when the code of the lambda is produced, it should search the parameters at the context offset (3 in the example),
plus 2 for the PC counter that has been pushed 2 times in the stack (I think...)

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




