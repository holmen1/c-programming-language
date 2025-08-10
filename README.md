# c-programming-language


## Overview

This repository is primarily a reread and practical exploration of the classic book "The C Programming Language" by Kernighan & Ritchie (K&R). It contains solved exercises from each chapter, with code examples and solutions designed to reinforce core C programming concepts.

In addition to the K&R exercises, the repository includes projects from the Low Level Academy, focusing on systems programming topics such as file I/O, networking, multithreading, and binary protocols. These projects provide hands-on experience with foundational techniques used in real-world C development.



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

## LSP

For advanced code navigation, auto-completion, and diagnostics in editors like Neovim (using plugins such as nvim-lspconfig or clangd), a `compile_commands.json` file is essential. This file provides the LSP with accurate build information, including include paths, compiler flags, and source files, enabling precise parsing and analysis of your C/C++ codebase.

Without `compile_commands.json`, features like go-to-definition, symbol search, and error highlighting may not work correctly or may be incomplete. You can generate this file using CMake (with `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON`) or tools like Bear for Makefile-based projects:

```bash
# With CMake:
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .

# With Bear (for Makefile projects):
bear -- make
```

Once generated, place `compile_commands.json` in your project root. Most LSP clients will automatically detect and use it for improved development experience in Neovim and other editors.