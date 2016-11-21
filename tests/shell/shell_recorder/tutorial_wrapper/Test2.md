## Example ##

this ins an example

```sh
# Backup-and-Restore:/test
# content of our ini file
$ echo "[section]" > /tmp/test.ini
$ echo "key = value" >> /tmp/test.ini
$ cat /tmp/test.ini
[section]
key = value
#
# mount our ini file to /test
kdb mount /tmp/test.ini /test ini
# fetch /test/section/key
kdb get /test/section/key
value
# RET: 0
kdb umount /test
```
