# Benchmarks

## `benchmark_storage`

The following table shows a summary of the results of a `benchmark_storage` run:
NOTE: `factor` is always `dump / quickdump` (bigger = `quickdump` is faster), this test was only run with version 1, see below for a comparison of with version 2

| operation        | dump (µs) | quickdump (µs) | factor |
| ---------------- | --------: | -------------: | -----: |
| write keyset     |     81661 |           4741 | 17.224 |
| read keyset      |     94782 |          44673 |  2.122 |
| iterate keyset   |       125 |            135 |  0.926 |
| delete keyset    |      1823 |           3518 |  0.518 |
| re-read keyset   |     92939 |          43749 |  2.124 |
| strcmp key name  |       609 |           1826 |  0.334 |
| strcmp key value |       684 |            761 |  0.899 |

## `benchmark_plugingetset`

A script was used to generate KeySets with `2`, `200`, `2,000`, `200,000` and `2,000,000` Keys. For keynames we used
increasing numbers (prefixed with `_` to keep correct order), each key had a randomized value of 15 characters.
Additionally we added 4 metakeys to each key. The names and values of these were created similarly.

The parent key used was `dir:/tests/bench`.

We then used [`hyperfine`](https://github.com/sharkdp/hyperfine) to compare the performance of `dump` and `quickdump`.

```
hyperfine --min-runs 25 'benchmark_plugingetset <path> <parent> dump' 'benchmark_plugingetset <path> <parent> quickdump'
```

This means we used the same key sets for each of the runs, but the key values shouldn't really matter. It also means
that no shared metadata (`keyCopyMeta`) was involved.

We also used a modified version of `benchmark_plugingetset` that returned after the `get` call, to test the `get`
performance by itself. Because `set` doesn't scale linearly because of the checks for `keyCopyMeta`, the largest
KeySet was only used for the `get` only version. (The test was aborted after the initial run of `dump` took longer than
all the runs for `quickdump` together.)

The complete results can be found in [this repository](https://github.com/ElektraInitiative/rawdata)
and the files used for these benchmarks are quite big and can be found in another
[separate repository](https://github.com/kodebach/eqd-bench).

### Raw data sizes

For reference in the file size tests below, here is a list of the raw data sizes of the keysets. These sizes are calculated as the
sum of: 1) length of all keynames 2) length of all key values 3) length of all metakeys 4) length of all meta values
All lengths include a null terminator, sizes are calculated once with (WP) and once without the parent key (WOP).

| no. of keys | raw size WP (B) | raw size WOP (B) |
| ----------- | --------------: | ---------------: |
| 2           |             252 |              220 |
| 200         |           25784 |            22584 |
| 2000        |          262786 |           229786 |
| 200000      |        26977790 |         23777790 |
| 20000000    |       273777792 |        241777792 |

### Version 1

#### File sizes

`factor` is `dump / quickdump` like above (bigger = `quickdump` has smaller files)

| no. of keys |  dump (B) | quickdump (B) | factor |
| ----------- | --------: | ------------: | -----: |
| 2           |       430 |           412 |   1.04 |
| 200         |     41206 |         40988 |   1.01 |
| 2000        |    415807 |        413788 |   1.00 |
| 200000      |  42377809 |      42177788 |   1.00 |
| 2000000     | 427777810 |     425777788 |   1.00 |

#### get and set

The values are mean ± standard deviation. `factor` is `dump / quickdump` like above

| no. of keys |         dump (s) |   quickdump (s) | factor |
| ----------- | ---------------: | --------------: | -----: |
| 2           |  0.0016 ± 0.0003 | 0.0006 ± 0.0001 |   2.67 |
| 200         |  0.0091 ± 0.0009 | 0.0018 ± 0.0000 |   5.06 |
| 2000        |  0.0770 ± 0.0030 | 0.0127 ± 0.0008 |   6.06 |
| 200000      | 10.7876 ± 4.2564 | 1.2640 ± 0.0061 |   8.53 |

#### get only

The values are mean ± standard deviation. `factor` is `dump / quickdump` like above

| no. of keys |         dump (s) |   quickdump (s) | factor |
| ----------- | ---------------: | --------------: | -----: |
| 2           |  0.0014 ± 0.0000 | 0.0005 ± 0.0000 |    2.8 |
| 200         |  0.0032 ± 0.0005 | 0.0013 ± 0.0002 |   2.46 |
| 2000        |  0.0179 ± 0.0002 | 0.0086 ± 0.0008 |   2.08 |
| 200000      |  1.6830 ± 0.0222 | 0.8515 ± 0.0048 |   1.98 |
| 2000000     | 17.1814 ± 0.2201 | 8.8808 ± 0.0344 |   1.93 |

### Version 2

Running the same `benchmark_plugingetset` tests with version 2 of the plugin yields:

#### File sizes

`factor` is `v1 / v2` like above (bigger is better)

| no. of keys | quickdump v1 (B) | quickdump v2 (B) | factor |
| ----------- | ---------------: | ---------------: | -----: |
| 2           |              412 |              380 |   1.08 |
| 200         |            40988 |            23788 |   1.72 |
| 2000        |           413788 |           381788 |   1.08 |
| 200000      |         42177788 |         38977788 |   1.08 |
| 2000000     |        425777788 |        393777788 |   1.08 |

#### get and set

The values are mean ± standard deviation. `factor` is `v1 / v2`, i.e. bigger is better

| no. of keys | quickdump v1 (s) | quickdump v2 (s) | factor |
| ----------- | ---------------: | ---------------: | -----: |
| 2           |  0.0006 ± 0.0001 |  0.0013 ± 0.0035 |   0.47 |
| 200         |  0.0018 ± 0.0000 |  0.0018 ± 0.0002 |   1.02 |
| 2000        |  0.0127 ± 0.0008 |  0.0112 ± 0.0021 |   1.13 |
| 200000      |  1.2640 ± 0.0061 |  1.2602 ± 0.0372 |   1.00 |
| 2000000     |                - | 13.2104 ± 0.1185 |      - |

#### get only

The values are mean ± standard deviation. `factor` is `v1 / v2` like above

| no. of keys | quickdump v1 (s) | quickdump v2 (s) | factor |
| ----------- | ---------------: | ---------------: | -----: |
| 2           |  0.0005 ± 0.0000 |  0.0006 ± 0.0001 |   0.88 |
| 200         |  0.0014 ± 0.0002 |  0.0010 ± 0.0001 |   1.33 |
| 2000        |  0.0086 ± 0.0008 |  0.0072 ± 0.0005 |   1.20 |
| 200000      |  0.8516 ± 0.0048 |  0.6413 ± 0.0082 |   1.33 |
| 2000000     |  8.8809 ± 0.0345 |  6.3756 ± 0.0443 |   1.39 |

### Version 3

Running the same `benchmark_plugingetset` tests with version 3 of the plugin yields:

#### File sizes

`factor` is `v1 / v2` like above (bigger is better)

| no. of keys | quickdump v2 (B) | quickdump v3 (B) | factor |
| ----------- | ---------------: | ---------------: | -----: |
| 2           |              380 |              240 |   1.58 |
| 200         |            23788 |            23788 |   1.00 |
| 2000        |           381788 |           241788 |   1.58 |
| 200000      |         38977788 |         24977788 |   1.56 |
| 2000000     |        393777788 |        253777788 |   1.55 |

#### get and set

The values are mean ± standard deviation. `factor` is `v2 / v3`, i.e. bigger is better

| no. of keys | quickdump v2 (s) | quickdump v3 (s) | factor |
| ----------- | ---------------: | ---------------: | -----: |
| 2           |  0.0013 ± 0.0035 |  0.0012 ± 0.0029 |   1.08 |
| 200         |  0.0018 ± 0.0002 |  0.0017 ± 0.0002 |   1.05 |
| 2000        |  0.0112 ± 0.0021 |  0.0106 ± 0.0011 |   1.06 |
| 200000      |  1.2602 ± 0.0372 |  1.1473 ± 0.0287 |   1.10 |
| 2000000     | 13.2104 ± 0.1185 | 11.5082 ± 0.2624 |   1.15 |

#### get only

The values are mean ± standard deviation. `factor` is `v2 / v3` like above

| no. of keys | quickdump v2 (s) | quickdump v3 (s) | factor |
| ----------- | ---------------: | ---------------: | -----: |
| 2           |  0.0006 ± 0.0001 |  0.0006 ± 0.0001 |   0.99 |
| 200         |  0.0010 ± 0.0001 |  0.0012 ± 0.0001 |   0.84 |
| 2000        |  0.0072 ± 0.0005 |  0.0070 ± 0.0004 |   1.03 |
| 200000      |  0.6413 ± 0.0082 |  0.6288 ± 0.0218 |   1.02 |
| 2000000     |  6.3756 ± 0.0443 |  6.2309 ± 0.0462 |   1.02 |
