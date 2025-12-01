section .data
    ; >> SOON-TO-BE-FUTURE COSMO
    ; initialized data
    ;
    ; okay, we're getting somewhat nonlinear here.
    ; (this text was written after i wrote the
    ; text in .text, after reading some of this:
    ; https://p403n1x87.github.io/getting-started-with-x86-64-assembly-on-linux.html)
    ;
    ; so, we can define a string like so:
; hello           db  "Hello World!", 10      ; char *
; hello_len       equ $-hello                 ; size_t
    ; (copied verbatim)
    ;
    ; i'm somewhat concerned. what does the 10 mean?
    ; what is `db`? what is `equ`?
    ;
    ; now i inject a newline:
; hello           db  "Hello World!\n", 10      ; char *
; hello_len       equ $-hello                 ; size_t
    ; maybe this will break things horrifically.
    ; << SOON-TO-BE-FUTURE COSMO
    ; >> EVEN FUTURER COSMO
    ; okay turns out we get a newline for free?
    ; and also we don't get string escaping in
    ; this interface. so we printed a literal
    ; slash n in the first version of this program.
    ; sucks to be us. anyway, even futurer cosmo
    ; has your back, we revert to the original copypaste.
hello           db  "Hello World!", 10      ; char *
hello_len       equ $-hello                 ; size_t
    ; << EVEN FUTURER COSMO

section .bss
    buffer resb 1024  ; reserve 1KB for input buffer

section .text
global _start
_start:
    ; >> PAST COSMO
    ; goal: write "Hello World!" to stdout.
    ; this will entail performing a syscall,
    ; with code 1, for sys_write.
    ; now, my understanding is that arguments are passed
    ; in these registers, in order:
    ; rdi, rsi, rdx, rcx, r8, r9
    ; the write() call takes a file descriptor,
    ; a pointer to the buffer, and a count.
    ; thus, i expect to load an fd into rdi
    ; a pointer into rsi, and a size into rdx.
    ;
    ; okay, let's go. stdout fd is 1.
    mov rdi, 1
    ; pointer idk for now.
    ; todo!()
    ; size of "Hello World!" is 12 bytes.
    ; mov rdx, 12
    ; actually, i'd like a newline
    ; mov rdx, 13
    ; >> FUTURE COSMO
    ; okay i commented that out, we have the
    ; size autocomputed now.
    mov rdx, hello_len
    ; and we have what i can only assume is
    ; the pointer to the string:
    mov rsi, hello
    ; update: correct!
    ; << FUTURE COSMO
    ; okay, back to figuring the pointer
    ; out.
    ;
    ; 
    ; then we have to syscall
    ; >> FUTURER COSMO
    ; you screwed up. you got confused by
    ; setting fd=1 and forgot to also set
    ; the syscall number. probably because
    ; the syscall number is also 1. go:
    mov rax, 1
    ; << FUTURER COSMO
    syscall
    ; << PAST COSMO
    ; >> VERBATIM
    ; now exit:
    mov rax, 60       ; sys_exit
    xor rdi, rdi      ; exit code 0
    syscall
    ; << VERBATIM
