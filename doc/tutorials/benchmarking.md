# Benchmarking

## Execution Time

One of the usual questions for a standard benchmark is, how much more or less time two or more different programs take to execute on the same hardware. This tutorial will introduce some tools and techniques that will help you to answer this question. For that purpose we compare the time it takes for certain YAML [storage plugins](storage-plugins.md) to translate YAML data into Elektra’s key set structure. Most of the techniques we describe here should be applicable too, if you want to compare the run-time of other parts of Elektra. If you want to know why a certain part of Elektra takes a long time to execute, then you might also be interested in the [profiling tutorial](profiling.md).

### Translating Elektra

If you have never translated the code base of Elektra before, then please take a look [here](../COMPILE.md) before you continue.

Usually you want to compare the execution time of the fastest version of a compiled binary. For that purpose it makes sense to change the CMake build type to `Release`, which means that the generated build system will optimize the code and strip debug symbols. You should also disable the logger and debug code. An example CMake command that uses [Ninja](https://ninja-build.org) as build tool could look like this:

```sh
mkdir build
cd build
cmake -GNinja ..                 \
      -DCMAKE_BUILD_TYPE=Release \
      -DENABLE_LOGGER=OFF        \
      -DENABLE_DEBUG=OFF         \
      -DPLUGINS=ALL
ninja
cd .. # Change working directory back to the root of repository
```

.

### Using the Plugin Benchmark Helper Tool

Elektra already includes a tool that helps you to benchmark the `get` and `set` methods of a certain [plugin](plugins.md) called [`benchmark_plugingetset`](../../benchmarks/README.md). To show you how to use `benchmark_plugingetset`, we create a file named `test.yamlcpp.in` with the following content:

```yaml
- You,
- Me, &
- The Violence
```

and save it in the folder `benchmarks/data`:

```sh
mkdir -p benchmarks/data
printf -- '- You,\n'       >  benchmarks/data/test.yamlcpp.in
printf -- '- Me, &\n'      >> benchmarks/data/test.yamlcpp.in
printf -- '- The Violence' >> benchmarks/data/test.yamlcpp.in
```

. As you can see the filename has to use the pattern:

```sh
test.$plugin.in
```

, where `$plugin` specifies the name of the plugin the benchmark tool should call. We can now call the `get` method of the plugin [YAML CPP][] using the following shell command

```sh
build/bin/benchmark_plugingetset benchmarks/data      user    yamlcpp       get
#                                       ↑              ↑        ↑           ↑
#                                parent directory  namespace  plugin   only use `get`
#                                 of config file                       plugin method
```

. If you can want you can also use the `time` utility to measure the execution time of the last command:

```sh
time build/bin/benchmark_plugingetset benchmarks/data user yamlcpp get
#>       0.00 real         0.00 user         0.00 sys
```

. As you can see in the output above a real configuration file that tests the performance of the [YAML CPP][] plugin should be much larger.

[yaml cpp]: ../../src/plugins/yamlcpp/README.md

### Comparing Execution Times

Now that you know how to execute `benchmark_plugingetset`, you can use it to compare the performance of different plugins. Since you usually want

- to run `benchmark_plugingetset` multiple times, and
- compare different plugins

it makes sense to use a benchmarking tool such as [hyperfine](https://github.com/sharkdp/hyperfine) for that task. For our tutorial we assume that you copied the file [`keyframes.yaml`](https://github.com/ElektraInitiative/rawdata/blob/master/YAML/Input/keyframes.yaml) to the locations

- `benchmarks/data/test.yamlcpp.in`, and
- `benchmarks/data/test.yanlr.in`

. You can do that using the following commands:

```sh
mkdir -p benchmarks/data
curl -L https://github.com/ElektraInitiative/rawdata/raw/master/YAML/Input/keyframes.yaml -o benchmarks/data/test.yamlcpp.in
cp benchmarks/data/test.yamlcpp.in benchmarks/data/test.yanlr.in
```

. Afterwards you can use:

```sh
hyperfine --warmup 3 'build/bin/benchmark_plugingetset benchmarks/data user yamlcpp get' \
                     'build/bin/benchmark_plugingetset benchmarks/data user yanlr get'

```

to compare the performance of the plugins. The output of this benchmark would look something like this:

```
Benchmark #1: build/bin/benchmark_plugingetset benchmarks/data user yamlcpp get
  Time (mean ± σ):      18.3 ms ±   0.9 ms    [User: 15.2 ms, System: 1.4 ms]
  Range (min … max):    17.1 ms …  21.2 ms    136 runs

Benchmark #2: build/bin/benchmark_plugingetset benchmarks/data user yanlr get
  Time (mean ± σ):      16.2 ms ±   0.9 ms    [User: 12.7 ms, System: 1.7 ms]
  Range (min … max):    14.8 ms …  20.0 ms    161 runs

Summary
  'build/bin/benchmark_plugingetset benchmarks/data user yanlr get' ran
    1.13 ± 0.08 times faster than 'build/bin/benchmark_plugingetset benchmarks/data user yamlcpp get'
```

. You can now remove the input files and the folder `benchmarks/data`:

```sh
rm benchmarks/data/test.yamlcpp.in
rm benchmarks/data/test.yanlr.in
rmdir benchmarks/data
```

.
