Test1
```sh
# Backup-and-Restore:/test
# testcomment
kdb set /test/a a
kdb set /test/b b
# RET:0
# should yield 'a'
kdb get /test/a
# STDOUT:a
kdb get /test/a
#> a
kdb get /test/c
# Expected:
# RET:1
# STDERR:Did not find key
kdb get /test/d
# should fail
# STDOUT:fail-
# RET:0
kdb rm -r /test
```
Test 2

```sh
# Backup-and-Restore:/test
kdb set /test/x x
kdb set /test/y y
kdb get /test/x 
#> x
kdb get /test/y
#> y
kdb export /test ini
#> x = x
#> y = y
kdb ls /test
kdb rm -r /test
```


Test 3
```sh
#Backup-and-Restore:/test
ls

echo test

#> test

echo "test\nbla"
#> test

#> bla
cat `kdb file user`
```

heredoc
```sh
cat > /tmp/hereout << EOF \
line 1\
line 2\
EOF
cat /tmp/hereout
#> line 1
#> line 2
```

multi
```sh
cat /tmp/test \
ls \
echo test
```

sudo test
```sh
sudo cat `sudo kdb file system`
    sudo ls
cat `    sudo kdb file system`
ls \
sudo ls
```
