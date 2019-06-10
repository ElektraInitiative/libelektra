# Profiling

## Execution Time

One of the primary resources in computing is execution time. To keep usage of this resource type low, it makes sense to profile code and check which code paths in a progamm take the longest time to execute. There exist various tools to handle this kind of profiling. For this tutorial we will use

1. [Callgrind](http://valgrind.org/docs/manual/cl-manual.html) and the graphical frontend [KCacheGrind/QCacheGrind](https://kcachegrind.github.io/html/Home.html), and
2. [XRay][] and [FlameGraph][] to visualize the data produced by [XRay][]

.

[xray]: https://llvm.org/docs/XRay.html
[flamegraph]: https://github.com/brendangregg/FlameGraph

### Callgrind

#### Choosing the Correct Build Type

Since we want to improve the readability of the Callgrind output we choose a build type that includes debug symbols. The two obvious choices for the build type are:

- `RelWithDebInfo` (optimized build with debug symbols), and
- `Debug` (non-optimized build with debug symbols)

. We use `Debug` here, which should provide the most detailed profiling information.

#### Disabling `dlclose` Calls

For this tutorial we decided to profile the [YAy PEG][] plugin. Since Elektra loads plugin code via `dlopen` and Callgrind [does not support the function `dlclose` properly](https://stackoverflow.com/questions/16719395) we remove the `dlclose` calls in the file [`dl.c`](../../src/libs/loader/dl.c) temporarily. At the time of writing one option to do that is deleting

- a single line `dlclose` statement, and
- an `if`-statement that checks the return value of a `dlclose` call

. An unfortunate effect of this code update is that Elektra will now leak memory when it unloads a plugin. On the other hand, Callgrind will be able to add source code information about the [YAy PEG][] plugin to the profiling output.

[yay peg]: ../../src/plugins/yaypeg/README.md

#### Building Elektra

As we already described before we use the `Debug` build type for the profiling run. To make sure we test the actual performance of the [YAy PEG][] plugin we disable debug code and the logger. The following commands show one option to translate Elektra using this configuration, if we use [Ninja](https://ninja-build.org) as build tool:

```sh
mkdir build
cd build
cmake -GNinja ..               \
      -DCMAKE_BUILD_TYPE=Debug \
      -DENABLE_LOGGER=OFF      \
      -DENABLE_DEBUG=OFF       \
      -DPLUGINS=ALL
ninja
cd .. # Change working directory back to the root of repository
```

.

#### Profiling the Code

We use the tool [`benchmark_plugingetset`](../../benchmarks/README.md) to profile the execution time of [YAy PEG][]. The file [`keyframes.yaml`](https://github.com/ElektraInitiative/rawdata/blob/master/YAML/Input/keyframes.yaml) serves as input file for the plugin. Since `benchmark_plugingetset` requires a data file called

```sh
test.$plugin.in
```

, we save a copy of `keyframes.yaml` as `test.yaypeg.in` in the folder `benchmarks/data`:

```sh
mkdir -p benchmarks/data
curl -L https://github.com/ElektraInitiative/rawdata/raw/master/YAML/Input/keyframes.yaml -o benchmarks/data/test.yaypeg.in
```

. After that we call `benchmark_plugingetset` directly to make sure that everything works as expected:

```sh
build/bin/benchmark_plugingetset benchmarks/data user yaypeg get
```

. If the command above fails with a segmentation fault, then please check

- that the [build system](../COMPILE.md) included [YAy PEG][], and
- that your OS is able to locate the plugin (e.g. append the `lib` directory in the build folder to `LD_LIBRARY_PATH` on Linux)

. If `benchmark_plugingetset` executed successfully, then you can now use Callgrind to profile the command:

```sh
valgrind --tool=callgrind --callgrind-out-file=callgrind.out \
build/bin/benchmark_plugingetset benchmarks/data user yaypeg get
```

. The command above will create a file called `callgrind.out` in the root of the repository. You can now remove the input data and the folder `benchmarks/data`:

```sh
rm benchmarks/data/test.yaypeg.in
rmdir benchmarks/data
```

. If you use [Docker](../../scripts/docker/README.md) to translate Elektra, then you might want to fix the paths in the file `callgrind.out` before you continue:

```sh
# The tool `sponge` is part of the `moreutils` package: https://joeyh.name/code/moreutils
sed -E 's~/home/jenkins/workspace/(\.\./)*~~g' callgrind.out | sponge callgrind.out
```

. Now we can analyze the file `callgrind.out` with a graphical tool such as QCacheGrind:

```sh
qcachegrind&
```

. If everything worked as expected QCacheGrind should open the file `callgrind.out` and display a window that look similar to the one below:

![QCacheGrind](../images/qcachegrind.png)

. You can now select different parts of the call graph on the left to check which parts of the code take a long time to execute.

### XRay

[XRay][] is an extension for LLVM that adds profiling code to binaries. Profiling can be dynamically enabled and disabled via the environment variable `XRAY_OPTIONS`.

#### Choosing the Correct Build Type

Since [XRay][] currently requires LLVM we need to set the compiler appropriately. We use Clang 8 in our example.

```sh
export CC=clang-8
export CXX=clang++-8
```

. We enable the static build (`BUILD_STATIC=ON`) and disable the dynamic build (`BUILD_SHARED=OFF`), since [XRay currently does not support dynamic libraries](http://clang-developers.42468.n3.nabble.com/Xray-with-shared-libraries-td4061859.html). To enable Xray we use the compiler switch `-fxray-instrument`. To instrument every function we set the instruction threshold to `1` with `-fxray-instruction-threshold=1`.

```sh
export REPOSITORY_DIRECTORY="$PWD"
export BUILD_DIRECTORY="$REPOSITORY_DIRECTORY/build"
mkdir -p "$BUILD_DIRECTORY"
cd "$BUILD_DIRECTORY"
cmake -GNinja "$REPOSITORY_DIRECTORY"                                   \
      -DCMAKE_BUILD_TYPE=Release                                        \
      -DBUILD_SHARED=OFF                                                \
      -DBUILD_STATIC=ON                                                 \
      -DCOMMON_FLAGS='-fxray-instrument -fxray-instruction-threshold=1' \
      -DPLUGINS=ALL
```

We will analyze the [YAMBi plugin][yambi] below. Please make sure that the CMake command above includes the plugin:

```
…
-- Include Plugin yambi
…
```

. Now we can translate the code with [Ninja](https://ninja-build.org) and change the current directory back to the root of the repository:

```sh
ninja
cd "$REPOSITORY_DIRECTORY"
```

. In the next step we use [`benchmark_plugingetset`](../../benchmarks/README.md) to execute [YAMBi][] for the input file [`generated.yaml`][]. To do that we

1. create the folder `data` in the directory [`benchmarks`](../../benchmarks), and
2. download the file [`generated.yaml`][] as `test.yambi.in`

. The following commands show you how to do that:

```sh
mkdir -p benchmarks/data
curl -L https://github.com/ElektraInitiative/rawdata/raw/master/YAML/Input/generated.yaml -o benchmarks/data/test.yambi.in
```

. Now we first check if running [`benchmark_plugingetset`][] works without instrumentation:

```sh
"$BUILD_DIRECTORY/bin/benchmark_plugingetset" benchmarks/data user yambi get
```

. If everything worked correctly, then the command above should finish successfully and not produce any output. To instrument the binary we set the environment variable `XRAY_OPTIONS` to the value `xray_mode=xray-basic verbosity=1`.

```sh
export XRAY_OPTIONS='xray_mode=xray-basic patch_premain=true verbosity=1'
"$BUILD_DIRECTORY/bin/benchmark_plugingetset" benchmarks/data user yambi get
```

. The command above will print the location of the XRay log file to `stdterr`:

```
…
…XRay: Log file in 'xray-log.benchmark_plugingetset.gpcX3t'
…
```

. Now we can use the log file to analyze the runtime of the execution paths of the binary. To do that we first save the name of the log file in the variable `LOGFILE`. This way we do not need to repeat the filename every time in the commands below.

```sh
LOGFILE=$("$BUILD_DIRECTORY/bin/benchmark_plugingetset" benchmarks/data user yambi get 2>&1 |
          sed -nE "s/.*Log file in '(.*)'.*/\1/p")
```

. To list the 10 functions with the longest runtime we use the command `llvm-xray account`:

```sh
llvm-xray account "$LOGFILE" -top=10 -sort=sum -sortorder=dsc -instr_map "$BUILD_DIRECTORY/bin/benchmark_plugingetset"
#> Functions with latencies: 94
#>    funcid      count [      min,       med,       90p,       99p,       max]       sum  function
#>         1          1 [ 2.414035,  2.414035,  2.414035,  2.414035,  2.414035]  2.414035  <invalid>:0:0: main
#>      1340          1 [ 2.403729,  2.403729,  2.403729,  2.403729,  2.403729]  2.403729  <invalid>:0:0: elektraYambiGet
#> …
```

. We can also use the log file to create a [Flame Graph](http://www.brendangregg.com/flamegraphs.html). To do that we use the `llvm-xray stack` to create an input file for the tool [`flamegraph.pl`][flamegraph]

```sh
llvm-xray stack "$LOGFILE" -stack-format=flame -aggregation-type=time -all-stacks \
                -instr_map "$BUILD_DIRECTORY/bin/benchmark_plugingetset" > flamegraph.txt
```

. We then create the Flame Graph with the following command:

```sh
# Depending on how you installed Flame Graph the executable
# might also be called `flamegraph.pl` instead of `flamegraph`.
flamegraph flamegraph.txt > flamegraph.svg
```

. The image below shows one example how the picture could look like:

![Flame Graph](../images/flamegraph.svg)

. Additional information on how to use the data produced by [XRay][] is available [here](https://llvm.org/docs/XRayExample.html).

[yambi]: ../../src/plugins/yambi/README.md
[`generated.yaml`]: https://github.com/ElektraInitiative/rawdata/blob/master/YAML/Input/generated.yaml
