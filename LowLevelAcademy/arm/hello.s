.global _start

.text
_start:
    mov r0, #0x04           // Semihosting operation SYS_WRITE0
    ldr r1, =msg            // r1 = address of the null-terminated string
    svc 0x123456            // Semihosting trap

    mov r0, #0x18           // AngelSWI_ReportException (exit)
    ldr r1, =0x20026        // ADP_Stopped_ApplicationExit (reason code)
    svc 0x123456            // Semihosting trap

.data
msg:
    .asciz "Hello, World!\n"
