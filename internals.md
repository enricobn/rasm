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

**Alloc**  

_allocation_table:
address   allocated (false = 0 true = any other value)         size     count

initial value
"the address of the fist location" 0 MAX_MEMORY 0

allocate(n)  
I search the first slot with almost n free memory
if there's no slot: pack the _allocation_table_ and search for n free memory
if there's no slot: OutOfMemory
otherwise, i return the anAddress, decrease the size of the slot, increment the address of the slot,
set the count to 1

add_ref(address)  
I search the address in _allocation_table_
if there's no slot: Error
increase the count

deref(address)  
I search the address in _allocation_table_
if there's no slot: Error
I decrease the count
if 0 I set allocated to 0 (false)

**New allocator**

- the allocation algorithm
    - first we search in the reusable table
        - we search for a reusable slot that matches the size (use the number of reusable slots to end the search)
        - if we find it
            - we allocate it in the allocation table
            - if there is more than one reusable slot, and it is not the one we used, we copy the last reusable address
              to the one we used
            - we decrement the number of reusable slots
            - we update the allocation table reference to the reusable table for the slot we moved
        - if we don't find it
            - we allocate the next free slot in the allocation table
- the deref algorithm  
  TODO
- the addRef algorithm  
  TODO








