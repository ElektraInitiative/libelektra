# Benchmarks #

The goal of this benchmarks is to find strengths and weaknesses of both MySQL and Elektra regarding the implemented service and its functions. The benchmark tools allow for command-line arguments, which means they are very flexible.

Output produced by the benchmarks itself are generated before or after taking the times, but not in between.

## Elektra Benchmark ##

This benchmark basically only measures timings of the implemented service classes (storage and search).

To run the benchmark, use `./bin/benchmark_kdbrest_elektra` in the `build` directory (after build). It will give a usage hint with possible and required arguments.

## MySQL Benchmark ##

This benchmark tries to store the important and persistent models [User](../model_user.hpp) and [Entry](../model_entry.hpp) within a MySQL database. The tables for the models use the best-fitting column types and can additionally use (read-performance increasing) indexes when enabled through a command-line argument. All benchmark types are designed to be perfectly comparable to the solution based on Elektra.

The database credentials (host, user, password and database name) have to be specified as constants in the benchmark source before compilation.

To run the benchmark, use `./bin/benchmark_kdbrest_mysql` in the `build` directory (after build). It will give a usage hint with possible and required arguments.