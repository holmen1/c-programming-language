# httpserver

## Build json parser
```
mkdir build && cd build
git clone https://github.com/DaveGamble/cJSON.git
```

```
cd cJSON
make all
```


```
# copy static library to make server portable if target do not have cJSON installed
mkdir -p ../../lib
cp libcjson.a ../../lib/
```


For development
```
$ sudo make install
mkdir -p /usr/local/lib /usr/local/include/cjson
cp -a cJSON.h /usr/local/include/cjson
cp -a libcjson.so libcjson.so.1 libcjson.so.1.7.18 /usr/local/lib
cp -a cJSON_Utils.h /usr/local/include/cjson
cp -a libcjson_utils.so libcjson_utils.so.1 libcjson_utils.so.1.7.18 /usr/local/lib
```

### FreeBSD patch

**Background**: On FreeBSD, the standard C library functions `isnan()` and `isinf()` may not be available or behave incorrectly during compilation of cJSON, causing build failures. The patch modifies `cJSON.c` line 607 to use GCC/Clang built-in functions (`__builtin_isnan` and `__builtin_isinf`) when available, providing better portability across BSD systems.

**How to apply**:

From the `build/` directory after cloning cJSON:
```bash
cd build/cJSON
patch -p4 < ../cjson_freebsd.patch
```

Or from the repository root:
```bash
c-programming-language $ patch -p1 < ./LowLevelAcademy/httpserver/build/cjson_freebsd.patch
```

Test with 
```bash
patch --dry-run -p1 < cjson_freebsd.patch
```
 to verify

## Build
In ```httpserver```
```
make all
```

## Usage

```
./bin/httpserver
```
curl http://localhost:3737 -v --output -: Sends a GET request to http://localhost:3737 (the running server on port 3737).  
-v: Enables verbose output, showing connection details, headers, and response.  
--output -: Outputs the response body to stdout (the terminal).

```bash
curl localhost:1337 -v --output -
* Host localhost:1337 was resolved.
* IPv6: ::1
* IPv4: 127.0.0.1
*   Trying [::1]:1337...
* connect to ::1 port 1337 from ::1 port 53922 failed: Connection refused
*   Trying 127.0.0.1:1337...
* Established connection to localhost (127.0.0.1 port 1337) from 127.0.0.1 port 50340
* using HTTP/1.x
> GET / HTTP/1.1
> Host: localhost:1337
> User-Agent: curl/8.17.0
> Accept: */*
>
* Request completely sent off
< HTTP/1.1 200 OK
< Content-Type: text/html
< Content-Length: 380
<
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Welcome</title>
</head>
<body>
    <h1>Welcome to Toy HTTP Server!</h1>
    <p>This is the index page.</p>

    <form action="/reverse" method="post">
        <input type="text" name="text" placeholder="Enter text to reverse">
        <button type="submit">Reverse</button>
    </form>
</body>
</html>
* Connection #0 to host localhost:1337 left intact
```


```
curl -X POST http://localhost:1337/reverse -d "text=yourtext"
txetruoy=txet
```