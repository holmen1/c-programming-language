.global _start

.text
_start:
    // write(1, msg, 14) - print "Hello, World!\n"
    mov r7, #4          // syscall: write
    mov r0, #1          // fd: stdout
    ldr r1, =msg        // buffer: address of msg
    mov r2, #14         // count: 14 bytes
    svc #0              // make syscall

    // exit(0)
    mov r7, #1          // syscall: exit
    mov r0, #0          // exit code: 0
    svc #0              // make syscall

.data
msg:
    .ascii "Hello, World!\n"

