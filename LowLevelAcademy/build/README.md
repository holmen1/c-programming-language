```
make all
```

```
# copy static library
mkdir lib
cp ../build/cJSON/libcjson.a lib/
```





```
$ sudo make install
mkdir -p /usr/local/lib /usr/local/include/cjson
cp -a cJSON.h /usr/local/include/cjson
cp -a libcjson.so libcjson.so.1 libcjson.so.1.7.18 /usr/local/lib
cp -a cJSON_Utils.h /usr/local/include/cjson
cp -a libcjson_utils.so libcjson_utils.so.1 libcjson_utils.so.1.7.18 /usr/local/lib
```