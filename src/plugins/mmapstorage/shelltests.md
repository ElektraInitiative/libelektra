# Additional shell tests for mmapstorage

This file contains important shell tests for mmapstorage which
do not fit well into the plugin README.

# Test kdb export & import (stat: pipe size known)

```sh
# Make temp file
kdb set system:/tests/mmaptempfile $(mktemp)

# Mount mmapstorage to `user:/tests/mmapstorage`
sudo kdb mount config.mmap user:/tests/mmapstorage mmapstorage

# Add some values via `kdb set`
kdb set user:/tests/mmapstorage/test1 test1
kdb set user:/tests/mmapstorage/test2 test2
kdb set user:/tests/mmapstorage/test1/test3 test3

# List the configuration tree below `user:/tests/mmapstorage`
kdb ls user:/tests/mmapstorage
#> user:/tests/mmapstorage/test1
#> user:/tests/mmapstorage/test1/test3
#> user:/tests/mmapstorage/test2

# Retrieve the new values
kdb get user:/tests/mmapstorage/test1
#> test1
kdb get user:/tests/mmapstorage/test2
#> test2
kdb get user:/tests/mmapstorage/test1/test3
#> test3

kdb export user:/tests/mmapstorage mmapstorage > $(kdb get system:/tests/mmaptempfile)
kdb rm -r user:/tests/mmapstorage

kdb ls user:/tests/mmapstorage
#>

kdb import user:/tests/mmapstorage mmapstorage < $(kdb get system:/tests/mmaptempfile)

# List the configuration tree below `user:/tests/mmapstorage`
kdb ls user:/tests/mmapstorage
#> user:/tests/mmapstorage/test1
#> user:/tests/mmapstorage/test1/test3
#> user:/tests/mmapstorage/test2

# Retrieve the new values
kdb get user:/tests/mmapstorage/test1
#> test1
kdb get user:/tests/mmapstorage/test2
#> test2
kdb get user:/tests/mmapstorage/test1/test3
#> test3


# Undo modifications to the database
kdb rm -r user:/tests/mmapstorage

# Unmount mmapstorage
sudo kdb umount user:/tests/mmapstorage

# Remove temp file
rm $(kdb get system:/tests/mmaptempfile)
kdb rm -r system:/tests/mmaptempfile
```

# Test kdb export & import (stat: pipe size unknown)

```sh
# Make temp file
kdb set system:/tests/mmaptempfile $(mktemp)

# Mount mmapstorage to `user:/tests/mmapstorage`
sudo kdb mount config.mmap user:/tests/mmapstorage mmapstorage

# Add some values via `kdb set`
kdb set user:/tests/mmapstorage/test1 test1
kdb set user:/tests/mmapstorage/test2 test2
kdb set user:/tests/mmapstorage/test1/test3 test3

# List the configuration tree below `user:/tests/mmapstorage`
kdb ls user:/tests/mmapstorage
#> user:/tests/mmapstorage/test1
#> user:/tests/mmapstorage/test1/test3
#> user:/tests/mmapstorage/test2

# Retrieve the new values
kdb get user:/tests/mmapstorage/test1
#> test1
kdb get user:/tests/mmapstorage/test2
#> test2
kdb get user:/tests/mmapstorage/test1/test3
#> test3

kdb export user:/tests/mmapstorage mmapstorage > $(kdb get system:/tests/mmaptempfile)
kdb rm -r user:/tests/mmapstorage

kdb ls user:/tests/mmapstorage
#>

cat $(kdb get system:/tests/mmaptempfile) | kdb import user:/tests/mmapstorage mmapstorage

# List the configuration tree below `user:/tests/mmapstorage`
kdb ls user:/tests/mmapstorage
#> user:/tests/mmapstorage/test1
#> user:/tests/mmapstorage/test1/test3
#> user:/tests/mmapstorage/test2

# Retrieve the new values
kdb get user:/tests/mmapstorage/test1
#> test1
kdb get user:/tests/mmapstorage/test2
#> test2
kdb get user:/tests/mmapstorage/test1/test3
#> test3


# Undo modifications to the database
kdb rm -r user:/tests/mmapstorage

# Unmount mmapstorage
sudo kdb umount user:/tests/mmapstorage

# Remove temp file
rm $(kdb get system:/tests/mmaptempfile)
kdb rm -r system:/tests/mmaptempfile
```
