.intel_syntax
.global main
main:
.main.L0:
    push %rbp
    mov %rbp, %rsp
    mov %r15, 1
    mov %r12, %r15
    jmp .L1

.main.L1:
    mov %r15, 1
    cmp %r15, 0
    jne .L2
    jmp .L3

.main.L2:
    mov %r13, %r12
    mov %r14, 1
    mov %r15, %r13
    add %r15, %r14
    mov %r12, %r15
    jmp .L1

.main.L3:
    mov %rsp, %rbp
    pop %rbp
    ret

