# Additional shell tests for cache

This file contains important shell tests for mmapstorage which
do not fit well into the plugin README.

# Test kdb cp with cache and default resolver (refression test)

```sh
rm -rf $(dirname $(kdb file user))/multitest || $(exit 0)
mkdir -p $(dirname $(kdb file user))/multitest || $(exit 0)

echo "col1;col2" > $(dirname $(kdb file user))/multitest/first.csv
echo "l1c1;l2c2" >> $(dirname $(kdb file user))/multitest/first.csv
echo "l2c1;l2c2" >> $(dirname $(kdb file user))/multitest/first.csv

echo "" > $(dirname $(kdb file user))/multitest/empty.csv

kdb mount multitest/first.csv user/tests/multifile/first.csv csvstorage
kdb mount multitest/empty.csv user/tests/multifile/empty.csv csvstorage

kdb ls user/tests/multifile/first.csv
#> user/tests/multifile/first.csv/#0
#> user/tests/multifile/first.csv/#0/#0
#> user/tests/multifile/first.csv/#1
#> user/tests/multifile/first.csv/#1/#0
#> user/tests/multifile/first.csv/#2
#> user/tests/multifile/first.csv/#2/#0

kdb ls user/tests/multifile/empty.csv
#> user/tests/multifile/empty.csv/#0
#> user/tests/multifile/empty.csv/#0/#0

kdb cp -rf user/tests/multifile/first.csv user/tests/multifile/empty.csv

kdb ls user/tests/multifile/first.csv
#> user/tests/multifile/first.csv/#0
#> user/tests/multifile/first.csv/#0/#0
#> user/tests/multifile/first.csv/#1
#> user/tests/multifile/first.csv/#1/#0
#> user/tests/multifile/first.csv/#2
#> user/tests/multifile/first.csv/#2/#0

kdb ls user/tests/multifile/empty.csv
#> user/tests/multifile/empty.csv/#0
#> user/tests/multifile/empty.csv/#0/#0
#> user/tests/multifile/empty.csv/#1
#> user/tests/multifile/empty.csv/#1/#0
#> user/tests/multifile/empty.csv/#2
#> user/tests/multifile/empty.csv/#2/#0

rm -rf $(dirname $(kdb file user))/multitest
kdb umount user/tests/multifile/first.csv
kdb umount user/tests/multifile/empty.csv
```
