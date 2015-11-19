This directory is for benchmarking.

Instead of collecting all statistics in a single file,
commits are used per benchmark.

So to do a benchmark you first write the code and
go to a specific commit of Elektra. Then you can
run a benchmark. Afterwards you commit all information
you have collected.

This has the advantage that it is reproduceable.
Everyone can go anytime back to the benchmark commits
and run the same benchmarks again.

For running benchmarks you can use on unix:

   make benchmark

which will run valgrind on it.

The old STATISTICS file is no longer used and will be
removed with this commit.
