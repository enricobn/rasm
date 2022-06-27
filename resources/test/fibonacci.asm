; calling function nprintln                       ; 
; calling function fib                            ; 
; calling function atoi                           ; 
; calling function argv                           ; 
    push    1                                     ; 
    call    argv                                  ; 
    add     esp,4                                 ; 
; end calling function argv                       ; 
    push    eax                                   ; 
    call    atoi                                  ; 
    add     esp,4                                 ; 
; end calling function atoi                       ; 
    push    eax                                   ; 
    call    fib                                   ; 
    add     esp,4                                 ; 
; end calling function fib                        ; 
    push    eax                                   ; 
    call    nprintln                              ; 
    add     esp,4                                 ; 
; end calling function nprintln                   ; 
    mov     ebx, 0                                ; 
    mov     eax, 1                                ; 
    int     80h                                   ; 
    ret                                           ; 
fib:                                              ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 
; inlining function if                            ; 
    push    dword [ebp+4+4]                       ; 
    push     lambda0                              ; 
    push     lambda1                              ; 
; inlining function lessOrEqual                   ; 
; To remove from stack  lessOrEqual 3             ; 

    mov     eax,[ebp+4+4]
    cmp     eax,1
    mov     eax,1 ; true
    jbe     $+7  ; Jump if Below or Equal (unsigned comparison)
    mov     eax,0 ; false
; end inlining function lessOrEqual               ; 
    push    eax                                   ; 
; To remove from stack  if 4                      ; 

    mov     eax,[ebp-16]
    cmp     eax, 0
    jz      $+7
    call    [ebp-12] ; true value
    jmp     $+5
    call    [ebp-8] ; false value
    add     esp,16                                ; 
; end inlining function if                        ; 
    pop     ebp                                   ; 
    ret                                           ; 
itn:                                              ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     eax,[ebp+8]
    pop     ebp                                   ; 
    ret                                           ; 
sprintln:                                         ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 
; calling function sprint                         ; 
    push     dword [ebp+4+4]                      ; 
    call    sprint                                ; 
    add     esp,4                                 ; 
; end calling function sprint                     ; 
; calling function println                        ; 
    call    println                               ; 
; end calling function println                    ; 
    pop     ebp                                   ; 
    ret                                           ; 
nprintln:                                         ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 
; calling function nprint                         ; 
    push     dword [ebp+4+4]                      ; 
    call    nprint                                ; 
    add     esp,4                                 ; 
; end calling function nprint                     ; 
; calling function println                        ; 
    call    println                               ; 
; end calling function println                    ; 
    pop     ebp                                   ; 
    ret                                           ; 
nprint:                                           ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

  push    esi
  push    eax
  push    ebx
  push    ecx
  push    edx
  mov     eax,[ebp+8] ; integer value to convert
  mov     esi,_rasm_buffer_10b
  add     esi,9
  mov     byte [esi],0    ; String terminator

  mov ebx,10
  xor ecx,ecx         ; initialize length
.next_digit:
  xor edx,edx         ; Clear edx prior to dividing edx:eax by ebx
  div ebx             ; eax /= 10
  add dl,'0'          ; Convert the remainder to ASCII
  dec esi             ; store characters in reverse order
  inc ecx             ; length

  mov [esi],dl
  test eax,eax
  jnz .next_digit     ; Repeat until eax==0

  mov    edx,ecx ; the length
  mov     ecx,esi ; the pointer to the first digit (not necessarily the start of the provided buffer)
  mov     ebx, 1  ; stdout
  mov     eax, 4  ; write
  int     80h

  pop    edx
  pop    ecx
  pop    ebx
  pop    eax
  pop    esi
    pop     ebp                                   ; 
    ret                                           ; 
println:                                          ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     eax, 0Ah    ; move 0Ah into eax - 0Ah is the ascii character for a linefeed
    push    eax         ; push the linefeed onto the stack so we can get the address
    push    esp         ; push the address of the current stack pointer where is the \n char for sprint
    call    sprint      ; call our sprint function
    pop     eax         ; remove the linefeed char from the stack
    pop     eax
    pop     ebp                                   ; 
    ret                                           ; 
sprint:                                           ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    push    edx
    push    ecx
    push    ebx
    push    eax
    mov     eax,[ebp+8]
    push    eax
    call    slen

    mov     edx, eax
    pop     eax

    mov     ecx, eax
    mov     ebx, 1
    mov     eax, 4
    int     80h

    pop     eax
    pop     ebx
    pop     ecx
    pop     edx
    pop     ebp                                   ; 
    ret                                           ; 
slen:                                             ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    push    ebx             ; Save ebx to the stack since we use it
    mov     eax, [ebp+8]  ; Get the parameter from the stack (4 the PC + 4 ebp) and put it in eax
    mov     ebx, eax

.nextchar:
    cmp     byte [eax], 0
    jz      .finished
    inc     eax
    jmp     .nextchar

.finished:
    sub     eax, ebx
    pop     ebx
    pop     ebp                                   ; 
    ret                                           ; 
nadd:                                             ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     eax, [ebp+8]
    add     eax, [ebp+12]
    pop     ebp                                   ; 
    ret                                           ; 
if:                                               ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     eax,[ebp+8]
    cmp     eax, 0
    jz      $+7
    call    [ebp+12] ; true value
    jmp     $+5
    call    [ebp+16] ; false value
    pop     ebp                                   ; 
    ret                                           ; 
lessOrEqual:                                      ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     eax,[ebp+8]
    cmp     eax,[ebp+12]
    mov     eax,1 ; true
    jbe     $+7  ; Jump if Below or Equal (unsigned comparison)
    mov     eax,0 ; false
    pop     ebp                                   ; 
    ret                                           ; 
less:                                             ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     eax,[ebp+8]
    cmp     eax,[ebp+12]
    mov     eax,1 ; true
    jb     $+7  ; Jump if Below or Equal (unsigned comparison)
    mov     eax,0 ; false
    pop     ebp                                   ; 
    ret                                           ; 
greater:                                          ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     eax,[ebp+8]
    cmp     eax,[ebp+12]
    mov     eax,1 ; true
    jg      $+7  ; Jump if greater (unsigned comparison)
    mov     eax,0 ; false
    pop     ebp                                   ; 
    ret                                           ; 
eq:                                               ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     eax,[ebp+8]
    cmp     eax,[ebp+12]
    mov     eax,1 ; true
    je      $+7  ; Jump if equals
    mov     eax,0 ; false
    pop     ebp                                   ; 
    ret                                           ; 
exit:                                             ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     ebx, [ebp+8]    ; Arg one: the status
    mov     eax, 1          ; Syscall number:
    int     0x80
    pop     ebp                                   ; 
    ret                                           ; 
assert:                                           ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 
; inlining function if                            ; 
    push    dword [ebp+4+4]                       ; 
    push     lambda2                              ; 
    push     lambda3                              ; 
; To remove from stack  if 3                      ; 

    mov     eax,[ebp+4+4]
    cmp     eax, 0
    jz      $+7
    call    [ebp-12] ; true value
    jmp     $+5
    call    [ebp-8] ; false value
    add     esp,12                                ; 
; end inlining function if                        ; 
    pop     ebp                                   ; 
    ret                                           ; 
argc:                                             ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    mov     eax,[_rasm_args]
    pop     ebp                                   ; 
    ret                                           ; 
argv:                                             ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    push ebx
    push ecx
    mov ebx, _rasm_args
    mov ecx,4           ; i add 4*i to the base address (_rasm_args)
    mov eax,[ebp+8]
    inc eax             ; we skip the length (argc)
    mul ecx
    add ebx,eax
    mov eax,[ebx]
    pop ecx
    pop ebx
    pop     ebp                                   ; 
    ret                                           ; 
atoi:                                             ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 

    push    ecx
    push    edx
    mov edx, [ebp+8] ; our string
    xor eax, eax ; zero a "result so far"
    .top:
    movzx ecx, byte [edx] ; get a character
    inc edx ; ready for next one
    cmp ecx, '0' ; valid?
    jb .done
    cmp ecx, '9'
    ja .done
    sub ecx, '0' ; "convert" character to number
    imul eax, 10 ; multiply "result so far" by ten
    add eax, ecx ; add in current digit
    jmp .top ; until done
.done:
    pop    edx
    pop    ecx
    pop     ebp                                   ; 
    ret                                           ; 
lambda0:                                          ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 
; inlining function nadd                          ; 
; calling function fib                            ; 
; inlining function nadd                          ; 
; To remove from stack  nadd 0                    ; 

    mov     eax, [ebp+4+16]
    add     eax, -2
; end inlining function nadd                      ; 
    push    eax                                   ; 
    call    fib                                   ; 
    add     esp,4                                 ; 
; end calling function fib                        ; 
    push    eax                                   ; 
; calling function fib                            ; 
; inlining function nadd                          ; 
; To remove from stack  nadd 1                    ; 

    mov     eax, [ebp+4+16]
    add     eax, -1
; end inlining function nadd                      ; 
    push    eax                                   ; 
    call    fib                                   ; 
    add     esp,4                                 ; 
; end calling function fib                        ; 
    push    eax                                   ; 
; To remove from stack  nadd 2                    ; 

    mov     eax, [ebp-8]
    add     eax, [ebp-4]
    add     esp,8                                 ; 
; end inlining function nadd                      ; 
    pop     ebp                                   ; 
    ret                                           ; 
lambda1:                                          ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 
; inlining function itn                           ; 
; To remove from stack  itn 0                     ; 

    mov     eax,[ebp+4+16]
; end inlining function itn                       ; 
    pop     ebp                                   ; 
    ret                                           ; 
lambda2:                                          ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 
; calling function sprintln                       ; 
    push    _rasm_s4                              ; 
    call    sprintln                              ; 
    add     esp,4                                 ; 
; end calling function sprintln                   ; 
; calling function exit                           ; 
    push    1                                     ; 
    call    exit                                  ; 
    add     esp,4                                 ; 
; end calling function exit                       ; 
    pop     ebp                                   ; 
    ret                                           ; 
lambda3:                                          ; 
    push    ebp                                   ; 
    mov     ebp,esp                               ; 
    pop     ebp                                   ; 
    ret                                           ; 