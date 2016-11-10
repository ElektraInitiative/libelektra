Test1
```sh
# Mountpoint:/test
 # testcomment
	kdb set /test/a a
# RET:0
    kdb set /test/b b
# should yield 'a'
# STDOUT:a
kdb get /test/a
kdb get /test/a
a
# Expected:
# RET:1
# STDERR:Did not find key
kdb get /test/c
# should fail
# STDOUT:fail-
# RET:0
kdb get /test/d
```
Test 2
```sh
# Mountpoint:/test
kdb set /test/x x
kdb set /test/y y
kdb get /test/x 
x
kdb get /test/y
y
kdb export /test ini
x = x
y = y
kdb ls /test
```
Test 3
       ```sh
kdb set /test/z z
```
