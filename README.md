# c-programming-language


## Debug
```
gcc -std=c90 -g uncomment.c
```

### Run in GDB

```
gdb ./a.out
```

```
[b]reak main      # Set breakpoint where crash is suspected
[b]reak n         # Set breakpoint on line
[r]un < hello.txt # Start the program, redirecting file as input
[n]ext            # Go to next instruction
[s]tep            # Go to next instruction, diving into function
[c]ontinue        # To next breakpoint
[i]nfo locals     # List information of local variables
[p]rint state     # Prints the value which the indicated expression evaluates to
[p]rint *ptr      # - ´´ -
[b]ack[t]race     # Show callstack
x/10i $pc         # Show next 10 instructions from program counter
x/4xb 0xaddress   # Show 4 bytes in hexadecimal format
[Return]          # Repeat last command
fs next           # Focus on next window
q                 # Exit GDB
```
