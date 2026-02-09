# Vec details

## push with copy, no addref at the end

* Initial state  
  vec ref 1  
  o1 ref 1

* Then  
  new_vec ref 0  
  o1 ref 1  

* outside of the function

  * full addref(new_vec)  
    new_vec ref 1  
    o1 ref 2  
  
    vec ref 1  
    o1 ref 2  

  * full deref(vec)  
    vec ref 0  
    o1  ref 1  

    new_vec ref 1  
    o1 ref 1

## optimized push

* Initial state  
  vec ref 1  
  o1 ref 1  

* Then

* outside of the function

  * full addref(vec)  
    vec ref 2  
    o1 ref 1  
  
  * full deref(vec)  
    vec ref 1  
    o1  ref 1  

## push with copy, addref simple at the end

* Initial state  
  vec ref 1  
  o1 ref 1  

* Then  
  new_vec ref 0  
  o1 ref 1  

  * addref simple  
  new_vec ref 1  
  o1 ref 1  
  
* outside of the function
  * fulladdref(new_vec)  
    new_vec ref 2  
    o1 ref 1  
  
  * full deref(vec)  
    vec ref 0  
    o1  ref 0  

    new_vec ref 1  
    o1 ref 0  

## push with copy, full addref at the end

* Initial state  
  vec ref 1  
  o1 ref 1  

* Then  
  new_vec ref 0  
  o1 ref 1  
  * full addref  
    new_vec ref 1  
    o1 ref 2  

    vec ref 1  
    o1 ref 2  
  
* outside of the function
  * fulladdref(new_vec)  
    new_vec ref 2  
    o1 ref 2  
  
  * full deref(vec)  
    vec ref 0  
    o1  ref 1  

    new_vec ref 2  
    o1 ref 1  
