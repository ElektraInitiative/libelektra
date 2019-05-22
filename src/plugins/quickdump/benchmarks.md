# Benchmarks

Note: these benchmarks were done with Version 1 of the plugin. Version 2 reuses the string buffer on get and stores key names relative
to the parent key. In theory the new version should be slightly quicker, but the benchmarks have not been repeated.

## `benchmark_storage`

The following table shows a summary of the results of a `benchmark_storage` run:
NOTE: `factor` is always `dump / quickdump`

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

The complete results can be viewed in the [benchmarks folder](benchmarks)

### get and set

The values are mean ± standard deviation. `factor` is `dump / quickdump` like above

| no. of keys |         dump (s) |   quickdump (s) | factor |
| ----------- | ---------------: | --------------: | -----: |
| 2           |  0.0016 ± 0.0003 | 0.0006 ± 0.0001 |   2.67 |
| 200         |  0.0091 ± 0.0009 | 0.0018 ± 0.0000 |   5.06 |
| 2000        |  0.0770 ± 0.0030 | 0.0127 ± 0.0008 |   6.06 |
| 200000      | 10.7876 ± 4.2564 | 1.2640 ± 0.0061 |   8.53 |

### get only

The values are mean ± standard deviation. `factor` is `dump / quickdump` like above

| no. of keys |         dump (s) |   quickdump (s) | factor |
| ----------- | ---------------: | --------------: | -----: |
| 2           |  0.0014 ± 0.0000 | 0.0005 ± 0.0000 |    2.8 |
| 200         |  0.0032 ± 0.0005 | 0.0013 ± 0.0002 |   2.46 |
| 2000        |  0.0179 ± 0.0002 | 0.0086 ± 0.0008 |   2.08 |
| 200000      |  1.6830 ± 0.0222 | 0.8515 ± 0.0048 |   1.98 |
| 2000000     | 17.1814 ± 0.2201 | 8.8808 ± 0.0344 |   1.93 |
