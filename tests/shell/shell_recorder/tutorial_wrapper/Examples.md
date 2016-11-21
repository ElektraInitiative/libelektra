```sh
# Backup-and-Restore:/tmount
#kdb global-mount
#
# Mount and test for exit 0
#
kdb mount test.ini /tmount ini enum conditionals
# RET:0 
#
kdb setmeta /tmount/battery check/enum "'empty', 'full'"
#
# Set battery to an invalid value and test if an error is returned
# Expected return code: 5, expected elektra error: 121
#
kdb set /tmount/battery invalid
# RET:5
# ERRORS:121
#
# Set valid value. Expect return code 0
#
kdb set /tmount/battery empty
# RET:0
#
# 'condition' should be 'bad' if battery is 'empty', or 'good' 
# for any other value ('full')
#
kdb setmeta /tmount/condition assign/condition "(../battery=='empty') ? ('bad') : ('good')"
kdb get /tmount/condition
bad
#
# set battery to 'full'
#
kdb set /tmount/battery full
# 
kdb get /tmount/condition
good
#
# Test if correct value is written to the configuration file
# on kdb set
#
kdb set /tmount/condition
kdb get user/tmount/condition
good
kdb export user/tmount ini
# expected output
[]
battery = full
condition = good
#
# cleanup
#
kdb rm -r /tmount
kdb umount /tmount
```


```sh
# Backup-and-Restor:/tmount
kdb mount --with-recommends hosts /tmount hosts
# RET:0
#
# Create hosts file for testing
#
$ echo "127.0.0.1	localhost" > `kdb file /tmount`
$ echo "::1 	localhost" >> `kdb file /tmount`
#
# Check the file
#
$ cat `kdb file /tmount`
127.0.0.1	localhost
::1	localhost
#
# Check if the values are read correctly
#
kdb get /tmount/ipv4/localhost
127.0.0.1
kdb get /tmount/ipv6/localhost
::1
#
# Should both fail with error 51 and return 5 
#
kdb set /tmount/ipv4/localhost ::1
# RET:5
# ERRORS:51
kdb set /tmount/ipv6/localhost 127.0.0.1
# RET:5
#
# cleanup
#
kdb rm -r /tmount
kdb umount /tmount
```
