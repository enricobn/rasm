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

**Registers**  
During the calculation of parameters (function_call_parameters) we use ecx to store the actual lambda space, 
edx for add_lambda_param_from_lambda_space and ebx is the "free" register, don't use eax that is used as the return value of the functions 

**Enum**  
The value of an enum is a pointer to a location in memory where, in the first slot there's the variant order number 
which is 0 for the first variant, 1 for the second and so on, the there is the list of the parameter of that enum 


