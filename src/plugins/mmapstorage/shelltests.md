# Additional shell tests for mmapstorage

This file contains important shell tests for mmapstorage which
do not fit well into the plugin README.


```sh
# Mount mmapstorage to `user/tests/mmapstorage`
sudo kdb mount config.mmap user/tests/mmapstorage mmapstorage

# Add some values via `kdb set`
kdb set user/tests/mmapstorage/test1 test1
kdb set user/tests/mmapstorage/test2 test2
kdb set user/tests/mmapstorage/test1/test3 test3

# List the configuration tree below `user/tests/mmapstorage`
kdb ls user/tests/mmapstorage
#> user/tests/mmapstorage/test1
#> user/tests/mmapstorage/test1/test3
#> user/tests/mmapstorage/test2

# Retrieve the new values
kdb get user/tests/mmapstorage/test1
#> test1
kdb get user/tests/mmapstorage/test2
#> test2
kdb get user/tests/mmapstorage/test1/test3
#> test3

kdb export user/tests/mmapstorage mmapstorage > backup.mmap
kdb rm -r user/tests/mmapstorage

kdb ls user/tests/mmapstorage
#>

kdb import user/tests/mmapstorage mmapstorage < backup.mmap

# List the configuration tree below `user/tests/mmapstorage`
kdb ls user/tests/mmapstorage
#> user/tests/mmapstorage/test1
#> user/tests/mmapstorage/test1/test3
#> user/tests/mmapstorage/test2

# Retrieve the new values
kdb get user/tests/mmapstorage/test1
#> test1
kdb get user/tests/mmapstorage/test2
#> test2
kdb get user/tests/mmapstorage/test1/test3
#> test3


# Undo modifications to the database
kdb rm -r user/tests/mmapstorage

# Unmount mmapstorage
sudo kdb umount user/tests/mmapstorage
```
