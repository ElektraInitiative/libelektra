## Example ##

this ins an example

```sh
# content of our ini file
$ cat /tmp/test.ini
# [section]
# key = value
#
# mount our ini file to /test
kdb mount /tmp/test.ini /test ini
# fetch /test/section/key
# RET: 0
kdb get /test/section/key
value
kdb umount /test
```
