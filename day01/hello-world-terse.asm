section .data
hello           db  "Hello World!", 10      ; char *
hello_len       equ $-hello                 ; size_t

section .bss
    buffer resb 1024  ; reserve 1KB for input buffer

section .text
global _start
_start:
    mov rdi, 1        ; stdout fd
    mov rdx, hello_len; len of string
    mov rsi, hello    ; ptr to string
    mov rax, 1        ; write(2)
    syscall
    mov rax, 60       ; sys_exit
    xor rdi, rdi      ; exit code 0
    syscall
