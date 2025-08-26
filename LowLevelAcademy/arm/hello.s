.global _start

.text
_start:
    mov r7, #4           //
    mov r0, #1
    ldr r1, =msg            // r1 = address of the null-terminated string
    mov r2, #14
    svc #0

    mov r7, #1           // AngelSWI_ReportException (exit)
    mov r1, #0            // ADP_Stopped_ApplicationExit (reason code)
    svc #0

.data
msg:
    .ascii "Hello, World!\n"

