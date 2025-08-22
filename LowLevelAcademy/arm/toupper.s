.global _start
.global capitalize

.text
_start:
    sub sp, sp, #4          // allocate 4 bytes on stack

    // read 1 byte from stdin into [sp]
    mov r7, #3              // syscall: read
    mov r0, #0              // fd: stdin
    mov r1, sp              // buffer: stack pointer
    mov r2, #1              // count: 1 byte
    svc #0

    ldrb r0, [sp]           // load byte into r0
    bl capitalize           // call capitalize(r0)
    strb r0, [sp]           // store result back to [sp]

    mov r0, #'\n'           // load newline character
    strb r0, [sp, #1]       // store newline at [sp+1]

    // write 2 bytes from [sp] to stdout
    mov r7, #4              // syscall: write
    mov r0, #1              // fd: stdout
    mov r1, sp              // buffer: stack pointer
    mov r2, #2             // count: 2 bytes
    svc #0

    add sp, sp, #4          // deallocate stack space

    // exit
    mov r7, #1
    mov r0, #0
    svc #0

// r0 = input char, returns capitalized char in r0
capitalize:
    cmp r0, #'a'            // if r0 < 'a'
    blt cap_end
    cmp r0, #'z'            // if r0 > 'z'
    bgt cap_end
    sub r0, r0, #32         // r0 = r0 - 32 (make uppercase)
cap_end:
    bx lr

//  $ arm-linux-gnueabihf-gcc -g -o io io.s -nostdlib -static
//  $ qemu-arm ./io
//  g
//  G

