.global _start

.text
_start:
    mov r7, #4
    mov r0, #1
    ldr r1, =msg
    mov r2, #14
    svc #0

    mov r7, #1
    mov r0, #0
    svc #0

.data
msg:
    .ascii "Hello, World!\n"

