.global _start

.text
_start:
    sub sp, sp, #4          // allocate 4 bytes on stack

    // read 1 byte from stdin into [sp]
    mov r7, #3              // syscall: read
    mov r0, #0              // fd: stdin
    mov r1, sp              // buffer: stack pointer
    mov r2, #1              // count: 1 byte
    svc #0

    // write 1 byte from [sp] to stdout
    mov r7, #4              // syscall: write
    mov r0, #1              // fd: stdout
    mov r1, sp              // buffer: stack pointer
    mov r2, #1              // count: 1 byte
    svc #0

    add sp, sp, #4          // deallocate stack space

    // exit
    mov r7, #1
    mov r0, #0
    svc #0
