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

## OPMPHM

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

## plugingetset

The `benchmark_plugingetset` is different than the other benchmarks. It doesn't do any benchmarking by itself.
Instead it simple takes 3 or 4 arguments:

```sh
benchmark_plugingetset <path> <parent> <plugin> [get]
```

. It then looks for the file `test.<plugin>.in` under the path `<path>`
and calls the `get` method of plugin `<plugin>` on this file with parent Key `<parent>`. Lastly it calls the `set` method of `<plugin>`
on the file `test.<plugin>.out` with parent Key `<parent>`, if you did not specify `get` as fourth argument.

`benchmark_plugingetset` can be used with `time` (or similar programs) to compare the speed of two (or more) storage plugins for specific files. The [benchmarking tutorial](../doc/tutorials/benchmarking.md) provides one example on how to do that.
