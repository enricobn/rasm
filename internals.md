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
