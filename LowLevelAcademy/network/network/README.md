# Network Server Project - Makefile and Testing

This document describes the build system and testing infrastructure for the network server project.

## Project Build System

The project uses a dual-Makefile approach for clarity and separation of concerns:

- Makefile - Main build system for compiling and running the server
- Makefile.test - Dedicated testing framework for verifying server functionality

## Main Makefile

### Key Targets

| Target | Description |
|--------|-------------|
| `make` | Default target that builds the server |
| `make debug` | Build with debug symbols and no optimization |
| `make clean` | Remove compiled files and databases |

### Usage Examples

```bash
# Run server, create new database
./dbserver -n -f my.db -p 8080
```

```bash
# Run client, add employee
./dbcli -h 127.0.0.1 -p 8080 -a "Mats,46 Hallandsgatan,200"
```

## Testing Framework

The Makefile.test provides isolated tests for critical server functionality.


### Running Tests

```bash
# Run all tests
make -f Makefile.test test

# Run a specific test
make -f Makefile.test test-socket
```

==== Testing Basic Server-Client Interaction ====
# Clean up any existing processes or files
# Start server in background
1. Starting server...
   Server started with PID: 112743
# Add an employee
2. Adding employee with client...
   ✅ Client successfully added employee
# Verify database file exists
3. Checking database file...
   ✅ Database file created successfully (528 bytes)
   File contents:
00000000: 4c4c 4144 0001 0001 0000 0210 5465 7374  LLAD........Test
00000010: 2045 6d70 6c6f 7965 6500 0000 0000 0000   Employee.......
00000020: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000030: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000040: 0000 0000 0000 0000 0000 0000 0000 0000  ................
# Clean shutdown
4. Shutting down server...
kill: usage: kill [-s sigspec | -n signum | -sigspec] pid | jobspec ... or kill -l [sigspec]
   ✅ Server shut down cleanly
\n==== Test completed successfully! ====


# Demo
$ make -f Makefile.demo demo 
🧹 Cleaning up previous demo...
\n📦 EMPLOYEE DATABASE DEMO
==========================
🚀 Starting database server on port 8080...
✅ Server running (PID: 6976)
\n👥 Adding employees...
Received hello response from server. Protocol v100
Sent add request to server. Employee:Alice Johnson,123 Main St,40
Successfully added employee 'Alice Johnson,123 Main St,40' to server
Received hello response from server. Protocol v100
Sent add request to server. Employee:Bob Smith,456 Oak Ave,35
Successfully added employee 'Bob Smith,456 Oak Ave,35' to server
Received hello response from server. Protocol v100
Sent add request to server. Employee:Carol Davis,789 Pine Rd,42
Successfully added employee 'Carol Davis,789 Pine Rd,42' to server
\n📋 Listing all employees:
Received hello response from server. Protocol v100
Sent employee list request to server
Received employee list response from server. Count: 3
Alice Johnson, 123 Main St, 40
Bob Smith, 456 Oak Ave, 35
Carol Davis, 789 Pine Rd, 42
\n🗑️  Deleting an employee:
Received hello response from server. Protocol v100
Sent delete request to server. Employee:Bob Smith
Successfully deleted employee 'Bob Smith' from server
\n📋 Final employee list:
Received hello response from server. Protocol v100
Sent employee list request to server
Received employee list response from server. Count: 2
Alice Johnson, 123 Main St, 40
Carol Davis, 789 Pine Rd, 42
\n📊 Database file info:
   Size: 1.6K   File: demo.db
\n🛑 Shutting down server...
\n🎉 Demo completed! Database functionality showcased successfully.
