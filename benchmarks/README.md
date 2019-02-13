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

   make benchmark_<filename>_callgrind

which will run the callgrind tool of Valgrind on it.

The old STATISTICS file is no longer used and will be
removed with this commit.

## OPMPHM

The OPMPHM benchmarks need an external seed source. Use the `generate-seeds` script
to generate a file containing the seeds. The number of seeds vary, execute the
`benchmark_opmphm` without parameter to get the number of seeds.
Then execute:

    cat <fileWithSeeds> | benchmark_opmphm <benchmark>

Example:

To run the OPMPHM build time benchmark you need 2008 seeds.
First generate the seeds:

	scripts/generate-seeds 2008 mySeedFile

Then pass it to the benchmark:

	cat mySeedFile | benchmark_opmphm opmphmbuildtime
