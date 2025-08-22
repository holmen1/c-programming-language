.global _start
.section .text

_start:
    mov r0, #0          @ Initialize i to 0
    mov r1, #10         @ Load value 10 into r1 (loop limit)
    mov r2, #0          @ Initialize sum to 0

loop:
    cmp r0, r1          @ Compare i and 10
    bge end             @ Break loop if i >= 10
    add r2, r2, r0
    add r0, r0, #1

    b loop

end:
    // Semihosting call to write to the console
    mov r0, #0x04       // SYS_WRITE0
    ldr r1, =msg        // Pointer to the string
    svc 0x123456        // Semihosting call

    // Semihosting call to exit
    mov r0, #0x18       // AngelSWI 0x18
    ldr r1, =0x20026    // ADP_Stopped_ApplicationExit
    svc 0x123456        // Semihosting call

.section .data
msg:
    .asciz "Hello, World!\n"
