This directory is for benchmarking.

Instead of collecting all statistics in a single file,
commits are used per benchmark.

So to do a benchmark you first write the code and
go to a specific commit of Elektra. Then you can
run a benchmark. Afterwards you commit all information
you have collected.

This has the advantage that it is reproducible.
Everyone can go anytime back to the benchmark commits
and run the same benchmarks again.

For running benchmarks you can use on Unix:

```sh
make benchmark_<filename>_callgrind
```

which will run the `callgrind` tool of Valgrind on it.

The old STATISTICS file is no longer used and will be
removed with this commit.

# changetracking

The changetracking benchmark evaluates the performance with changetracking enabled in KDB.
For it to work, you first have to load at least one plugin that utilizes changetracking.

A very simple case would be the `dbus` plugin:

```sh
kdb set system:/hook/notification/send/plugins/#0 dbus
```

You can then watch its output with

```sh
dbus-monitor --session "interface='org.libelektra'"
```

The output of a single benchmark is a single semicolon seperated line `time it took kdbSet to insert all keys;time it took kdbSet to update the keys`.
The time is measured in microseconds.

This benchmark has several command-line options:

- `--key-count <keys>`: how many keys should be generated (default 5000)
- `--harmonize-names`: all key-values and -names have the same length
- `--verbose`: print how many keys have been generated and modified
- `--binary-tree`: build a binary tree of keys instead of a linear list

# mountpoint

The mountpoint benchmark evaluates the performance of storing, reading, modifying and deleting keys and metadata.
Only the time for the `kdbGet` and `kdbSet` calls are benchmarked.
The time for preparing the keys and KeySets is not a part of the benchmark.
The main purpose of this benchmark is to compare the performance of different backends and storage plugins.

Just create a mountpoint for the backends and/or plugins you want to benchmark and give this mountpoint or a path
below as an argument to the benchmark.

The benchmark takes one mandatory argument and several optional arguments.

```sh
benchmark_mountpoint [options] <parentKey>
```

Be aware that it is expected that no keys are present at or below `<parentKey>`.
If you specify a `<parent key>` that already contains keys, these keys get deleted by the benchmark.
They can also influence the results of the benchmark.

This optional arguemnts are:

- `--key-count <keys>`: how many keys should be generated (default 3)
  - The default value is just intended for testing of the benchmark finished successfully. For real benchmarks, much higher values are recommended to get meaningful results.
- `--harmonize-names`: all key-values and -names have the same length
- `--single-keysets`: store and persists each individual key separately (one `kdbSet` call per key).
  - This option only influences the first benchmark.
- `--with-meta`: also run benchmarks for metadata
- `--only-meta`: run only the benchmarks for metadata
- `--verbose`: print more details

This benchmark-suite contains eight individual benchmarks:

1. Store keys without metadata in a single KeySet and persist it with `kdbSet`.
   - If `--single-keysets` was specified, each Key is persisted with its own `kdbSet` call.
2. Read the stored keys from the data source into a KeySet with `kdbGet`.
3. Modify the values of all the keys in the KeySet and persist it with `kdbSet`.
4. Delete all the keys that were added to the KeySet during the benchmark and persist it with `kdbSet`.
   - Now the data source is in the same state as in the beginning of the test.

The following steps are performed if you called the benchmark with the arguemnt `--with-meta`. 5. Store one Key with the given number of metakeys in a KeySet and persist it with `kdbSet`. 6. Read that key from the data source back into a KeySet with `kdbGet`. 7. Modify the values of all the metakeys and persist them with `kdbSet`. 8. Delete the key that has all the metakeys attached to it and persist it with `kdbSet`.

# OPMPHM

The OPMPHM benchmarks need an external seed source. Use the `generate-seeds` script
to generate a file containing the seeds. The number of seeds vary, execute the
`benchmark_opmphm` without parameter to get the number of seeds.
Then execute:

```sh
cat <fileWithSeeds> | benchmark_opmphm <benchmark>
```

Example:

To run the OPMPHM build time benchmark you need 2008 seeds.
First generate the seeds:

```sh
scripts/generate-seeds 2008 mySeedFile
```

Then pass it to the benchmark:

```sh
cat mySeedFile | benchmark_opmphm opmphmbuildtime
```

# plugingetset

The `benchmark_plugingetset` is different than the other benchmarks. It doesn't do any benchmarking by itself.
Instead it simple takes 3 or 4 arguments:

```sh
benchmark_plugingetset <path> <parent> <plugin> [get]
```

. It then looks for the file `test.<plugin>.in` under the path `<path>`
and calls the `get` method of plugin `<plugin>` on this file with parent Key `<parent>`. Lastly it calls the `set` method of `<plugin>`
on the file `test.<plugin>.out` with parent Key `<parent>`, if you did not specify `get` as fourth argument.

`benchmark_plugingetset` can be used with `time` (or similar programs) to compare the speed of two (or more) storage plugins for specific files. The [benchmarking tutorial](../doc/tutorials/benchmarking.md) provides one example on how to do that.
