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


## Lessons Learned

### Header Ordering in C Network Programming

When compiling socket code across platforms (Linux, FreeBSD, macOS), **header include order matters**. 

#### The Problem
```c
// This works on Linux but fails on FreeBSD: 
#include <arpa/inet.h>
#include <unistd.h>
```
**Error**: `field 'address' has incomplete type` for `struct sockaddr_in`

#### The Solution
Follow the POSIX-standard order for network headers: 
```c
#include <sys/types.h>    // 1. Basic types (socklen_t, size_t)
#include <sys/socket. h>   // 2. Socket API (socket, bind, accept)
#include <netinet/in.h>   // 3. Internet structures (sockaddr_in)
#include <arpa/inet.h>    // 4. Internet operations (htons, inet_ntoa)
#include <unistd.h>       // 5. POSIX functions (close, read, write)
```

#### Why It Matters
- **Linux is lenient**:  Implicitly includes dependencies
- **FreeBSD is strict**: Follows POSIX standards exactly  
- **Portable code**:  Explicit includes work everywhere

**Rule of thumb**: If it compiles on FreeBSD, it will compile on Linux.  The reverse isn't always true. 

**Pro tip**: Check `man` pages for required headers:  `man 2 socket` shows which headers to include.
