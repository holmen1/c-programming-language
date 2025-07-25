# Network Server Project

A networked employee database system with client-server architecture:

## Core Components

- **Server (`dbserver`)**: 
  - Poll-based architecture handling multiple simultaneous clients
  - State machine for connection management (NEW → HELLO → MSG)
  - Persistent database storage
  - Clean signal handling for graceful shutdown

- **Client (`dbcli`)**:
  - Command-line interface for database operations
  - Implements handshaking protocol
  - Supports add/list/delete operations

## Technical Implementation

- **Network Protocol**:
  - Custom binary protocol with version negotiation
  - State-based message handling
  - Connection-per-request model

- **Database Engine**:
  - Custom binary file format
  - CRUD operations for employee records
  - Atomic operations with proper error handling

- **Testing Framework**:
  - Automated test suite in Makefile.test
  - Validation of server-client interactions
  - Hexdump verification of database integrity

This project demonstrates solid systems programming skills including:
- Socket programming
- Non-blocking I/O with poll()
- Binary data protocols
- File I/O
- Process management
- Client-server architecture


### Usage Examples

```bash
# Server
$ ./bin/dbserver 
Filepath is a required argument
Usage: ./bin/dbserver -n -f <database_file>
Options:
	-n          create new database file
	-f <file>   (required) database file path
	-p <port>   (required) port to listen on
$ ./bin/dbserver -n -f my.db -p 8080
  DATABASE SERVER STARTED SUCCESSFULLY
  Listening on: 0.0.0.0:8080

  CONNECT CLIENT:
  ./bin/dbcli -h localhost -p 8080
```

```bash
# Client
$ ./bin/dbcli 
Usage: ./bin/dbcli -h <host> -p <port> [-a <addarg>]
Options:
	-h <host>   (required) host to connect to
	-p <port>   (required) port to connect to
	-a <addarg> Add a new employee with the given string format 'name,address,hours'
	-l List all employees in the database
	-d <name>    Delete employee with the given name
holmen1@thinkpad ~/repos/c-programming-language/LowLevelAcademy/network/network network* $ ./bin/dbcli -h 127.0.0.1 -p 8080 -l
Received hello response from server. Protocol v100
Sent employee list request to server
Received employee list response from server. Count: 0
holmen1@thinkpad ~/repos/c-programming-language/LowLevelAcademy/network/network network* $ ./bin/dbcli -h 127.0.0.1 -p 8080 -a "Name1,A Street,10"
Received hello response from server. Protocol v100
Sent add request to server. Employee:Name1,A Street,10
Successfully added employee 'Name1,A Street,10' to server
holmen1@thinkpad ~/repos/c-programming-language/LowLevelAcademy/network/network network* $ ./bin/dbcli -h 127.0.0.1 -p 8080 -a "Name2,B Street,20"
Received hello response from server. Protocol v100
Sent add request to server. Employee:Name2,B Street,20
Successfully added employee 'Name2,B Street,20' to server
holmen1@thinkpad ~/repos/c-programming-language/LowLevelAcademy/network/network network* $ ./bin/dbcli -h 127.0.0.1 -p 8080 -a "Name3,C Street,30"
Received hello response from server. Protocol v100
Sent add request to server. Employee:Name3,C Street,30
Successfully added employee 'Name3,C Street,30' to server
holmen1@thinkpad ~/repos/c-programming-language/LowLevelAcademy/network/network network* $ ./bin/dbcli -h 127.0.0.1 -p 8080 -l
Received hello response from server. Protocol v100
Sent employee list request to server
Received employee list response from server. Count: 3
Name1, A Street, 10
Name2, B Street, 20
Name3, C Street, 30
holmen1@thinkpad ~/repos/c-programming-language/LowLevelAcademy/network/network network* $ ./bin/dbcli -h 127.0.0.1 -p 8080 -d "Name2"
Received hello response from server. Protocol v100
Sent delete request to server. Employee:Name2
Successfully deleted employee 'Name2' from server
holmen1@thinkpad ~/repos/c-programming-language/LowLevelAcademy/network/network network* $ ./bin/dbcli -h 127.0.0.1 -p 8080 -l
Received hello response from server. Protocol v100
Sent employee list request to server
Received employee list response from server. Count: 2
Name1, A Street, 10
Name3, C Street, 30
```

```bash
Server
New connection from 127.0.0.1:51156
Client connected in slot 0 with fd 5
Client 5 sent valid hello request, moving to MSG state
Listing all employees
Client disconnected
New connection from 127.0.0.1:58776
Client connected in slot 0 with fd 5
Client 5 sent valid hello request, moving to MSG state
Adding employee: Name1,A Street,10
Client disconnected
Poll timeout - no activity
New connection from 127.0.0.1:45762
Client connected in slot 0 with fd 5
Client 5 sent valid hello request, moving to MSG state
Adding employee: Name2,B Street,20
Client disconnected
New connection from 127.0.0.1:50056
Client connected in slot 0 with fd 5
Client 5 sent valid hello request, moving to MSG state
Adding employee: Name3,C Street,30
Client disconnected
New connection from 127.0.0.1:55386
Client connected in slot 0 with fd 5
Client 5 sent valid hello request, moving to MSG state
Listing all employees
Client disconnected
New connection from 127.0.0.1:42210
Client connected in slot 0 with fd 5
Client 5 sent valid hello request, moving to MSG state
Deleting employee: Name2
Client disconnected
New connection from 127.0.0.1:35794
Client connected in slot 0 with fd 5
Client 5 sent valid hello request, moving to MSG state
Listing all employees
Client disconnected
```

## Testing Framework

The Makefile.test provides isolated tests for critical server functionality.


### Running Tests

```bash
# Run all tests
make -f Makefile.test test
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


### Running Demo
```bash
# Run all tests
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
```
