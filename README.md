# c-programming-language


## Debug
```
gcc -std=c90 -g uncomment.c
```

### Run in GDB

```
gdb ./a.out
```

### Inside GDB:

```
(gdb) [b]reak main      # Set breakpoint where crash is suspected
(gdb) [b]reak n         # Set breakpoint on line
(gdb) [r]un < hello.txt # Start the program, redirecting file as input
(gdb) [n]ext            # Go to next instruction
(gdb) [s]tep            # Go to next instruction, diving into function
(gdb) [c]ontinue        # To next breakpoint
(gdb) [i]nfo locals     # List information of local variables
(gdb) [p]rint state     # Prints the value which the indicated expression evaluates to
(gdb) [p]rint *ptr      # - ´´ -
(gdb) [b]ack[t]race     # Show callstack
```
