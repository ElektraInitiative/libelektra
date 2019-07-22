# 0.8.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes. Please make
sure to add some short tutorial (checked by
shell recorder) or asciinema for highlighted items.

Please add your name at the end of every contribution.
**Syntax:** _(your name)_

<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.8.<<VERSION>>.

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

You can also read the news [on our website](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

## Highlights

- <<HIGHLIGHT1>>
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

### Type (New Version)

The `type` plugin was completely rewritten in C. The old version is now called `cpptype`. _(Klemens Böswirth)_

The new `type` plugin also provides the functionality of the `enum` and the `boolean` plugin. These plugins are now considered obsolete and
you should use `type` instead.

A few notes on compatibility:

- the new `type` does not support the full feature set of `enum` and `boolean`, but it supports the features we consider useful.
- the new `type` doesn't support `FSType` and `empty`. These have been deprecated for a long time and there are good alternatives available.
- the new `type` supports `enum`, `wchar` and `wstring` as types, whereas the old `cpptype` would throw an error for these. In most cases
  this won't be a problem, but you should be aware of this breaking change.
- the new `type` does not support `check/type/min` and `check/type/max`, please use the `range` plugin.

To switch from `enum` to the new `type`, you have to add either `check/type=enum` or `type=enum`. Without a `check/type` or `type` metakey,
the `type` plugin will ignore the key. We now also support converting enum values to and from integer values (see
[README](https://www.libelektra.org/plugins/type)).

To switch from `boolean` to the new `type`, you don't have to do anything, if you used the default config. If you used a custom configuration
please take a look at the [README](https://www.libelektra.org/plugins/type).

### kdbEnsure

`kdbEnsure` is a new function in `elektra-kdb`. It can be used to ensure that a KDB instance meets certain clauses specified in a
contract. In principle this a very powerful tool that may be used for a lot of things. For now it only supports a few clauses concerning
plugins:

- You can specify that a plugin should be mounted globally. This can for example be used to enable the new [gopts](#gopts) plugin.
- Conversely you can also define that a plugin should not be mounted globally, e.g. to disable the `spec` plugin, which is enabled by default.
- Additionally you may want to enforce that a global plugin uses a certain configuration. For this case you can specify that the plugin
  should be remounted, i.e. unmounted and immediately mounted again.
- Because of the different architecture involved, for now only unmounting of non-global plugins is supported.

All changes made by `kdbEnsure` are purely temporary. They will only apply to the KDB handle passed to the function.

IMPORTANT: `kdbEnsure` only works, if the `list` plugin is mounted in all appropriate global positions.

Note: `kdbEnsure` right now ignores the `infos/recommends` and `infos/needs` metadata of plugins, so you have to explicitly take care of
dependencies. _(Klemens Böswirth)_

### Error Code Concept

With this release, we changed our messy error code system into a more structured and clean way. Similar to [SQLStates](https://www.ibm.com/support/knowledgecenter/en/SSGU8G_12.1.0/com.ibm.sqls.doc/ids_sqs_0809.htm) we changed to structure of our error codes and migrated them. Have a look into
the new [codes](../../src/error/specification). This allows us to easily extend the specification without breaking existing
codes and to avoid risking duplicated errors as we had before. _(Michael Zronek)_

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### Base64

- We fixed some warnings about implicit type conversions reported by [UBSan][]. _(René Schwaiger)_

[ubsan]: https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html

### Cache

- [cache](https://www.libelektra.org/plugins/cache) is a new global caching plugin. It uses [mmapstorage](https://www.libelektra.org/plugins/mmapstorage) as its storage backend and lazily stores keysets from previous ´kdbGet()´ calls. We added initial support for the default resolver and multifile resolver. _(Mihael Pranjić)_
- Add check of resolved filenames, fixes false cache hits. _(Mihael Pranjić)_
- Execute only `PROCGETSTORAGE` global plugins when we have a cache hit. _(Mihael Pranjić)_
- Fix two data loss bugs when using `cache` with `multifile` resolver. _(Mihael Pranjić)_
- Fix GCC ASAN build warnings #2820. _(Mihael Pranjić)_

### crypto / fcrypt

- Empty GPG key IDs in the plugin configuration are being ignored by the [crypto](https://www.libelektra.org/plugins/crypto) plugin and the [fcrypt](https://www.libelektra.org/plugins/fcrypt) plugin. Adding empty GPG key IDs would lead to an error when `gpg` is being invoked._(Peter Nirschl)_
- Apply Base64 encoding to the master password, which is stored within the plugin configuration. This fixes a problem that occurs if ini is used as default storage (see [2591](https://github.com/ElektraInitiative/libelektra/issues/2591))._(Peter Nirschl)_
- Fix compilation without deprecated OpenSSL APIs. Initialization and deinitialization is not needed anymore. _(Rosen Penev)_

### CSVStorage

- Support DOS newlines for the csvstorage plugin. _(Vlad - Ioan Balan)_

### filecheck

- We fixed some warnings about implicit type conversions reported by [UBSan][]. _(René Schwaiger)_

### gOpts

- The [gopts](https://www.libelektra.org/plugins/gopts) plugin simply retrieves the values of `argc`, `argv` and `envp` needed for
  [`elektraGetOpts`](https://www.libelektra.org/tutorials/command-line-options) and then makes the call. It is intended to be used as a
  global plugin, so that command-line options are automatically parsed when `kdbGet` is called. _(Klemens Böswirth)_
- The plugin works under WIN32 (via `GetCommandLineW` and `GetEnvironmentString`), MAC_OSX (`_NSGetArgc`, `_NSGetArgv`) and any system that
  either has a `sysctl(3)` function that accepts `KERN_PROC_ARGS` (e.g. FreeBSD) or when `procfs` is mounted and either `/proc/self` or
  `/proc/curproc` refers to the current process. If you need support for any other systems, feel free to add an implementation.

### INI

- Fixed `ini` when only the root key needs to be written. _(Mihael Pranjić)_
- Plugin writes to ini files without spaces around '=' anymore. Reading is still possible with and without spaces.
  _(Oleksandr Shabelnyk)_

### macaddr

- Added a plugin to handle MAC addresses. `kdbGet` converts a MAC address into a decimal 64-bit integer (with the most significant 16 bits always set to 0), if the format is supported. `kdbSet` restores the converted values back to there original form. _(Thomas Bretterbauer)_

### mINI

- We fixed compiler warnings reported by GCC 9 in the [unit test code](../../src/plugins/mini/testmod_mini.c) of the plugin. _(René Schwaiger)_

### mmapstorage

- [mmapstorage](https://www.libelektra.org/plugins/mmapstorage) is now able to persist the Global KeySet, which is used by the `cache` plugin. _(Mihael Pranjić)_
- Fixed support for `kdb import` and `kdb export`. _(Mihael Pranjić)_

### multifile

- Fixed segmentation fault in `kdbError()` function. _(Mihael Pranjić)_
- Added Global Keyset handle to storage plugin. _(Mihael Pranjić)_
- Disable cache when `ini` is used. _(Mihael Pranjić)_
- Fixed use of wrong resolver handle in the `kdbError()` function. _(Mihael Pranjić)_

### Quickdump

- [quickdump](https://www.libelektra.org/plugins/quickdump) is a new storage plugin. It implements a more concise form of the
  [dump](https://www.libelektra.org/plugins/dump) format, which is also quicker too read. Contrary to dump, quickdump only stores
  keynames relative to the parent key. This allows easy relocation of configurations. _(Klemens Böswirth)_
- quickdump now also uses an variable length integer encoding to further reduce file size. _(Klemens Böswirth)_

### Reference

- Fixed missing Metadata in README and METADATA.ini. _(Michael Zronek)_
- Update README.md web tool to show, how to test REST API on localhost. _(Dmytro Moiseiuk)_

### RGBColor

- New plugin to validate hex formatted colors (e.g. #fff or #abcd) and normalize them to rgba (4294967295 (= 0xffffffff) and 2864434397 (= 0xaabbccdd) respectively). It also has support for named colors according to the [extended color keywords](https://www.w3.org/TR/css-color-3/#svg-color) from CSS3.
  _(Philipp Gackstatter)_

### semlock

removed due to:

- constant pain
- never worked properly
- poor design
- no time in future to maintain
  _(Kurt Micheli)_

### spec

- The spec plugin was partly rewritten to better support specifications for arrays. This includes some breaking changes concerning the less
  used (and also less functional) parts of the plugin. To find out more about these changes take a look at the
  [README](../../src/plugins/spec/README.md). It now better reflects the actually implemented behaviour. _(Klemens Böswirth)_

### Specload

- The [specload](https://www.libelektra.org/plugins/specload) plugin is a special storage plugin. Instead of using a storage file
  it calls an external application to request its specification. For the transfer it relies on the
  [quickdump](https://www.libelektra.org/plugins/quickdump) plugin. _(Klemens Böswirth)_
- Currently changing the specification is only allowed in a very limited way. However, in future the plugin should allow overriding a
  specification in all cases where this can be done safely. NOTE: While the plugin technically allows some modifications, because of a
  problem with the resolver this cannot be used right now (see [limitations](https://www.libelektra.org/plugins/specload)).
- We also export `elektraSpecloadSendSpec` to abstract over the `quickdump` dependency. _(Klemens Böswirth)_

### Syslog

- We fixed an incorrect format specifier in a call to the `syslog` function. _(René Schwaiger)_

### unit

- New plugin to validate units of memory and normalize them into bytes. E.g. 20 KB (normalized to 20000 Byte).
  _(Marcel Hauri)_

### YAJL

- The plugin now allows setting a value to the mountpoint. This is represented as a top level value in JSON if no other key is present. _(Philipp Gackstatter)_

- The plugin no longer lists empty parent keys with `kdb ls`. _(Philipp Gackstatter)_

- The plugin signifies arrays with the metakey array according to the [array decision](../../doc/decisions/array.md). _(Philipp Gackstatter)_

- The plugin no longer produces additional `___dirdata` entries for empty array keys. See also issue [#2477](https://github.com/ElektraInitiative/libelektra/issues/2477). _(Philipp Gackstatter)_

### YAMBi

- [YAMBi](https://www.libelektra.org/plugins/yambi) is now able detect multiple syntax errors in a file. _(René Schwaiger)_
- The error message now includes more information about the location of syntax errors. For example, for the incorrect YAML input `config.yaml`:

  ```yaml
  key 1: - element 1
   - element 2
  key 2: scalar
         - element 3
  ```

  , the plugin prints an error message that includes the following text:

  ```
  config.yaml:2:2: syntax error, unexpected start of sequence, expecting end of map or key
                    - element 2
                    ^
  config.yaml:4:8: syntax error, unexpected start of sequence, expecting end of map or key
                          - element 3
                          ^
  ```

  . _(René Schwaiger)_

- [YAMBi](https://www.libelektra.org/plugins/yambi) now supports Elektra’s [boolean data type](../decisions/bool.md). _(René Schwaiger)_
- The plugin now handles YAML key-value pairs without a value at the end of a file correctly. _(René Schwaiger)_
- The plugin now converts YAML key-value pairs with empty value to null/empty keys. _(René Schwaiger)_
- [YAMBi](https://www.libelektra.org/plugins/yambi) now converts empty files to a key set containing an empty version of the parent key. _(René Schwaiger)_

### YAML CPP

- The plugin now handles keys that are part of a map, but use a basename ending with [array syntax](../tutorials/arrays.md) correctly. For example, in a key set that contains keys with the following names:

  ```
  user/array/#0
  user/array/#1
  user/map/#0
  user/map/key
  user/map/#1
  ```

  , `user/array/#0` and `user/array/#1` represent array elements, while `user/map/#0`, and `user/map/#1` do not, since the key set also contains the key `user/map/key`. The following [Markdown Shell Recorder][] snippet shows the new behavior of the plugin:

  ```sh
  kdb mount config.yaml user yamlcpp
  kdb set user/array/#0 one
  kdb set user/array/#1 two
  kdb set user/map/#0   three
  kdb set user/map/key  four
  kdb set user/map/#1   five
  kdb file user | xargs cat
  #> array:
  #>   - one
  #>   - two
  #> map:
  #>   "#0": three
  #>   "#1": five
  #>   key: four
  ```

  . _(René Schwaiger)_

- [YAML CPP][] now handles the conversion from and to [Elektra’s boolean type](../../doc/decisions/bool.md) properly. _(René Schwaiger)_
- The plugin converts “sparse” key sets properly. For example, for the key set that contains **only** the key:

  - `user/parent/#1/#2/map/#0` with the value `arr`

  and uses `user/parent` as parent key, YAML CPP stores the following YAML data:

  ```yaml
  - ~
  - - ~
    - ~
    - map:
        - arr
  ```

  . _(René Schwaiger)_

- [YAML CPP][] now supports mixed data (nested lists & sequences) better. For example, the plugin now correctly converts the YAML data

  ```yaml
  root:
    - element: one
    - element: two
  ```

  to the key set that contains the following keys:

  ```
  user/tests/yaml/root
  user/tests/yaml/root/#0/element
  user/tests/yaml/root/#1/element
  ```

  .

[markdown shell recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper
[yaml cpp]: ../../src/plugins/yamlcpp/README.md

### YAML Smith

<!-- prettier-ignore-start -->

- [YAML Smith](https://www.libelektra.org/plugins/yamlsmith) now converts keys that shares a common prefix correctly. For example, the last command in the script:

  ```sh
  kdb mount config.yaml user/tests/yaml yaml
  kdb set user/tests/yaml/common/one/#0 value
  kdb set user/tests/yaml/common/two/#0 first
  kdb set user/tests/yaml/common/two/#1 second
  kdb export user/tests/yaml yamlsmith
  ```

  now prints the following YAML data:

  ```yaml
  common:
    one:
      - "value"
    two:
      - "first"
      - "second"
  ```

  . _(René Schwaiger)_

<!-- prettier-ignore-end -->

- The plugin now converts Elektra’s boolean values (`0`, `1`) back to YAML’s boolean values (`true`, `false`). _(René Schwaiger)_

### Yan LR

- The build system now disables the plugin, if you installed a version of ANTLR 4 that does not support ANTLR’s C++ runtime (like ANTLR
  `4.5.x` or earlier). _(René Schwaiger)_
- We fixed an ambiguity in the [YAML grammar](https://master.libelektra.org/src/plugins/yanlr/YAML.g4). _(René Schwaiger)_
- The build system now regenerates the modified parsing code, every time we update the grammar file. _(René Schwaiger)_
- The plugin now reports the location of syntax errors correctly. _(René Schwaiger)_
- The lexer for the plugin now emits start tokens for maps at the correct location inside the token stream. This update fixes a problem, where the plugin sometimes reported incorrect error messages for the _first_ syntax error in a YAML file. _(René Schwaiger)_
- [Yan LR](https://www.libelektra.org/plugins/yanlr) now supports Elektra’s [boolean data type](../decisions/bool.md). _(René Schwaiger)_
- The plugin now handles YAML key-value pairs that contain no value at the end of a file correctly. _(René Schwaiger)_
- The plugin now converts YAML key-value pairs with empty value to null/empty keys. _(René Schwaiger)_
- The plugin converts “empty” YAML files to a key set that contains an empty version of the parent key. _(René Schwaiger)_

### YAwn

- [YAwn][] is now able to print error messages for multiple syntax errors. _(René Schwaiger)_
- We also improved the error messages of YAwn, which now also contain the input that caused a syntax error. For example, for the input

  ```yaml
  key: value
    - element
  ```

  the plugin prints an error message that contains the following text:

  ```
  config.yaml:2:3: Syntax error on input “start of sequence”
                     - element
                     ^
  ```

  . _(René Schwaiger)_

- The plugin now supports Elektra’s [boolean data type](../decisions/bool.md). _(René Schwaiger)_
- [YAwn][] handles YAML key-value pairs that contain no value at the end of a file correctly. _(René Schwaiger)_
- The plugin now converts YAML key-value pairs with empty value to null/empty keys. _(René Schwaiger)_
- [YAwn][] now stores empty files as a key set containing an empty parent key. _(René Schwaiger)_

[yawn]: https://www.libelektra.org/plugins/yawn

### YAy PEG

- [YAy PEG][] now also supports [PEGTL 2.8](https://github.com/taocpp/PEGTL/releases/tag/2.8.0). _(René Schwaiger)_
- The plugin now includes the input that could not be parsed in error messages. _(René Schwaiger)_
- We improved the error messages for certain errors slightly. For example, the error message for the input

  ```yaml
  "double quoted
  ```

  now includes the following text

  ```
  1:14: Missing closing double quote or incorrect value inside flow scalar
        "double quoted
                      ^
  ```

  . _(René Schwaiger)_

- [YAy PEG][] now supports compact mappings:

  ```yaml
  - key1: value1
    key2: value2
  ```

  and compact sequences:

  ```yaml
  - - element1
    - element2
  ```

  correctly. _(René Schwaiger)_

- The plugin now supports Elektra’s [boolean data type](../decisions/bool.md). _(René Schwaiger)_
- [YAy PEG][] now converts YAML key-value pairs with empty value to null/empty keys. _(René Schwaiger)_
- The plugin now translates an empty file to a key set that contains a single empty parent key. _(René Schwaiger)_

[yay peg]: https://www.libelektra.org/plugins/yaypeg

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

As always, the ABI and API of kdb.h is fully compatible, i.e. programs
compiled against an older 0.8 version of Elektra will continue to work
(ABI) and you will be able to recompile programs without errors (API).

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Core

- `kdbGet` now calls global postgetstorage plugins with the parent key passed to `kdbGet`, instead of a random mountpoint. _(Klemens Böswirth)_
- Fixed a double cleanup error (segmentation fault) when mounting global plugins. _(Mihael Pranjić)_
- Logging in Elektra was changed with this release. If Elektra is compiled with `ENABLE_LOGGER` enabled, we now log warnings and errors to
  stderr and everything except debug messages to syslog. If `ENABLE_DEBUG` is also enabled, debug messages are logged to syslog as well.
  Previously you had to make some manual changes to the code, to see most of the logging messages. _(Klemens Böswirth)_
- The logger does not truncate the file name incorrectly anymore, if `__FILE__` contains a relative (instead of an absolute) filepath. _(René Schwaiger)_
- Disabled any plugin execution when we have a cache hit or no update from backends. The old behaviour can be enabled for testing using `ENABLE_DEBUG` and adding the `"debugGlobalPositions"` meta key to the parentKey of the kdbGet invocation. _(Mihael Pranjić)_
- Removed `ingroup` from error messages to reduce verbosity. _(Michael Zronek)_
- Fixed minor problem when `kdb_long_double_t` is not available (e.g. mips32). _(Matthias Schoepfer)_
- Only add benchmarks if `BUILD_TEST` is set in cmake. _(Matthias Schoepfer)_
- We fixed some warnings about implicit conversion to `unsigned int` reported by [UBSan][]. _(René Schwaiger)_

### Ease

- The functions for reference resolving used in the [reference plugin](https://www.libelektra.org/plugins/reference) have been extracted
  into libease. This lets other parts of Elektra easily use references and ensures a consistent syntax for them. _(Klemens Böswirth)_

### <<Library2>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up to date with the multi-language support provided by Elektra.

- JNA is now not experimental anymore. _(Markus Raab)_
- gsettings is not default anymore. _(Markus Raab)_

- Add fix for creating the Key and KeySet objects in the HelloElektra.java file. _(Dmytro Moiseiuk)_
- We fixed a [warning about a deprecated default constructor](https://issues.libelektra.org/2670) in the C++ binding reported by GCC 9.0. _(René Schwaiger)_
- <<TODO>>

### <<Binding2>>

### <<Binding3>>

## Tools

- `kdb get -v` now displays if the resulting value is a default-value defined by the metadata of the key. _(Thomas Bretterbauer)_
- `kdb cp` now succeeds if the target-keys already have the same values as the source-keys. _(Thomas Bretterbauer)_
- `web-ui` issue #2441 is fixed in form of a workaround _(Josef Wechselauer)_
- `kdb import` does not fail anymore if executed more than once with the same target in the spec-namespace. _(Thomas Bretterbauer)_
- `kdb mount` avoid adding sync if sync is already provided. _(Markus Raab)_
- `kdb list-tools` now supports `KDB_EXEC_PATH` environment variables that contain spaces. _(René Schwaiger)_
- `gen-gpg-testkey` is added to the default tools list (see [#2668](https://github.com/ElektraInitiative/libelektra/issues/2668))._(Peter Nirschl)_
- <<TODO>>

- <<TODO>>

### Code generation

`kdb gen` is now no longer an external tool implemented via python, but rather a first class command of the `kdb` tool. For now it only
supports code generation for use with the highlevel API. Try it by running `kdb gen elektra <parentKey> <outputName>`, where `<parentKey>`
is the parent key of the specification to use and `<outputName>` is some prefix for the output files. If you don't have your specification
mounted, use `kdb gen -F <plugin>:<file> elektra <parentKey> <outputName>` to load it from `<file>` using plugin `<plugin>`.

.. _(Klemens Böswirth)_

## Scripts

- The `reformat-source` script now also formats `tests/shell/include_common.sh.in`. Additionally it ensures that the file is 1000 lines long,
  so that line numbers of files using it are easier to read. _(Klemens Böswirth)_
- The [clang-format wrapper script](../../scripts/reformat-source) now also checks the supported maximum version of Clang-Format. _(René Schwaiger)_
- The script [`reformat-shfmt`](https://master.libelektra.org/scripts/reformat-shfmt) now also reformats shell support files (`*.in`) in the [`scripts`](https://master.libelektra.org/scripts) folder. _(René Schwaiger)_
- The `reformat-*` scripts now allow you to specify a list of files that should be formatted. Only files actual suitable for the reformat script,
  will reformat. So e.g. calling `reformat-cmake src/include/kdbprivate.h` doesn't change any files. _(Klemens Böswirth)_
- The script `scripts/reformat-all` is a new convenience script that calls all other `reformat-*` scripts. _(Klemens Böswirth)_
- The script `scripts/pre-commit-check-formatting` can be used as a pre-commit hook, to ensure files are formatted before committing. _(Klemens Böswirth)_
- The [link checker](../../scripts/link-checker) now prints broken links to the standard error output. _(René Schwaiger)_
- We added a script, called [`benchmark-yaml`](../../scripts/benchmark-yaml.in) that compares the run-time of the YAML plugins:

  - [YAML CPP](https://www.libelektra.org/plugins/yamlcpp),
  - [Yan LR](https://www.libelektra.org/plugins/yanlr),
  - [YAMBi](https://www.libelektra.org/plugins/yambi),
  - [YAwn](https://www.libelektra.org/plugins/yambi), and
  - [YAy PEG](https://www.libelektra.org/plugins/yaypeg)

  for a certain input file with [hyperfine](https://github.com/sharkdp/hyperfine). _(René Schwaiger)_

## Benchmarks

- The benchmarking tool [`benchmark_plugingetset`](../../benchmarks/plugingetset.c) now also supports only executing the `get` method for the specified plugin. For example, to convert the data stored in the file `benchmarks/data/yaypeg.test.in` with the [YAy PEG plugin](https://www.libelektra.org/plugins/yaypeg) to a key set you can now use the following command:

  ```sh
  benchmark_plugingetset benchmarks/data user yaypeg get
  ```

  . _(René Schwaiger)_

## Documentation

### Style

- The documentation now uses [fenced code blocks](https://help.github.com/en/articles/creating-and-highlighting-code-blocks#syntax-highlighting) to improved the syntax highlighting of code snippets. _(René Schwaiger)_
- We added recommendations about the style of Markdown headers to our [coding guidelines](../CODING.md). _(René Schwaiger)_
- We now use [title case](https://en.wiktionary.org/wiki/title_case) for most headings in the documentation. _(René Schwaiger)_
- We added instructions on how to reformat code with

  - [Clang-Format](https://clang.llvm.org/docs/ClangFormat.html),
  - [cmake format](https://github.com/cheshirekow/cmake_format),
  - [Prettier](https://prettier.io), and
  - [shfmt](https://github.com/mvdan/sh)

  to the [coding guidelines](../CODING.md). _(René Schwaiger)_

### Tutorials

- We added a basic tutorial that tells you [how to write a (well behaved) storage plugin](../tutorials/storage-plugins.md). _(René Schwaiger)_
- Improved the `checkconf` section in the plugin tutorial. _(Peter Nirschl)_
- We added a [tutorial](../tutorials/benchmarking.md) on how to benchmark the execution time of plugins using [`benchmark_plugingetset`](../../benchmarks/README.md) and [hyperfine](https://github.com/sharkdp/hyperfine). _(René Schwaiger)_
- The new [profiling tutorial](../tutorials/profiling.md) describes how to determine the execution time of code using

  - [Callgrind](http://valgrind.org/docs/manual/cl-manual.html), and
  - [XRay](https://llvm.org/docs/XRay.html)

  . _(René Schwaiger)_

- For beginners we added a [tutorial](../tutorials/contributing-clion.md) that guides them through the process of contributing to libelektra. _(Thomas Bretterbauer)_
- Added a section on `elektraPluginGetGlobalKeySet` in the plugin tutorial. _(Vid Leskovar)_
- Added a step-by-step [tutorial](../tutorials/run_all_tests_with_docker.md) for beginners to run all tests with Docker. _(Oleksandr Shabelnyk)_
- Extend/improve java bindings related documentation in [tutorial](../tutorials/java-kdb.md) and [readme](../../src/bindings/jna/README.md). _(Oleksandr Shabelnyk)_

- Added a step-by-step [tutorial](../tutorials/run_reformatting_script_with_docker.md) for running reformatting scripts with Docker. _(Oleksandr Shabelnyk)_
- Covered Resolving Missing \*.so Library Error in [tutorial](../tutorials/contributing-clion.md). _(Oleksandr Shabelnyk)_
- Added a basic tutorial on [How-To: Write a Java Plugin](../tutorials/java-plugins.md) _(Dmytro Moiseiuk)_ and _(Miruna Orsa)_

### Spelling Fixes

- Write Elektra with capital letter in cascading tutorial. _(Vlad - Ioan Balan)_
- Add typo fix to the hello-elektra tutorial. _(Dmytro Moiseiuk)_
- Add typo fix to the Java kdb tutorial. _(Dominik Hofmann)_
- Fixed capitalization of the initial letter in Readme. _(Miruna Orsa)_
- Improved readability in README. _(Philipp Gackstatter)_
- We fixed some spelling mistakes in the documentation. _(René Schwaiger)_
- Fix typo in root README.md and 'build-in' => 'built-in' in several places _(Raphael Gruber)_
- Fixed typos in `cassandra.ini` _(arampaa)_

### Other

- The [Markdown Link Converter][] now uses the style

  ```
  filename:line:0
  ```

  instead of

  ```
  filename|line col 0|
  ```

  to show the location data for broken links. This is also the same style that Clang and GCC use when they display location information for
  compiler errors. This update has the advantage, that certain tools such as [TextMate](https://macromates.com) are able to convert the
  location data, providing additional features, such as clickable links to the error source. _(René Schwaiger)_

- The [Markdown Link Converter][] uses the index `1` for the first line number instead of `0`. This update fixes an off-by-one-error, when the user tries to use the error location data printed by the tool in a text editor. _(René Schwaiger)_
- We added a badge for [LGTM](https://lgtm.com) to the [main ReadMe file](https://master.libelektra.org/README.md). _(René Schwaiger)_
- Added [LCDproc](../../examples/spec/lcdproc) and [Cassandra](../../examples/spec/cassandra.ini) specification examples. These examples
  provide a good guideline for writing specifications for configurations. _(Michael Zronek)_
- Added a new error message format concept to be implemented soon. _(Michael Zronek)_
- Added a new error concept for error codes to be implemented soon. _(Michael Zronek)_
- Added error categorization guidelines to be used with the error concept. _(Michael Zronek)_
- Drastically improved the error message format. For more information look [here](../../doc/decisions/error_message_format.md). _(Michael Zronek)_
- Added a guideline for writing consistent and good error messages. For more information look [here](../../doc//dev/error-message.md). _(Michael Zronek)_
- Every `kdb` command now accepts `v` and `d` as option to show more information in case of warnings or errors. _(Michael Zronek)_
- Improved qt-gui error popup to conform with the new error message format. _(Raffael Pancheri)_
- We fixed the format specifiers in the [“Hello, Elektra” example](https://master.libelektra.org/examples/helloElektra.c). _(René Schwaiger)_
- Expanded the Python Tutorial to cover installation under Alpine Linux. _(Philipp Gackstatter)_
- We wrote a tutorial which is intended to [help newcomers contributing to libelektra](../tutorials/contributing-clion.md). _(Thomas Bretterbauer)_
- We fixed various broken links in the documentation. _(René Schwaiger)_
- Fix finding of jni.h library. _(Dmytro Moiseiuk)_
- Added license for asciinema. _(Anastasia @nastiaulian)_

- <<TODO>>

- <<TODO>>

- <<TODO>>

[markdown link converter]: https://master.libelektra.org/doc/markdownlinkconverter

## Tests

- We now test the [Directory Value Plugin](https://www.libelektra.org/plugins/directoryvalue) with additional test data. _(René Schwaiger)_
- The variables:

  - `SPEC_FOLDER`
  - `SYSTEM_FOLDER`
  - `USER_FOLDER`

  in the [inclusion file for shell test](../../tests/shell/include_common.sh.in) were set incorrectly, if the repository path contained space characters. _(René Schwaiger)_

- The [CFramework](https://master.libelektra.org/tests/cframework) now also compares the names of meta keys. _(René Schwaiger)_
- The [release notes check](../../scripts/run_check_release_notes) does not report an illegal number anymore, if the release notes were not updated at all. _(René Schwaiger)_
- We added a test for the keyhelper-class which checks if rebasePath calculates the new path for cascading target-keys correctly. _(Thomas Bretterbauer)_
- Enable MSR for the crypto and fcrypt tutorial ([#1981](https://github.com/ElektraInitiative/libelektra/issues/1981))._(Peter Nirschl)_
- We fixed the [Markdown Shell Recorder][] test for the command [`kdb get`](../help/kdb-get.md). _(René Schwaiger)_
- The tests

  - [`testscr_check_real_world`](../../tests/shell/check_real_world.sh),
  - [`testscr_check_resolver`](../../tests/shell/check_resolver.sh), and
  - [`testscr_check_spec`](../../tests/shell/check_spec.sh)

  now also works correctly, if the `user` and `system` directory file paths contain space characters. _(René Schwaiger)_

### Source Code Checks

- The formatting instructions printed by [`check_formatting`](https://master.libelektra.org/tests/shell/check_formatting.sh) now also work correctly, if

  - the `diff` output does not start with the test number added by CTest, and
  - you use a non-POSIX shell such as [`fish`](https://fishshell.com)

  . _(René Schwaiger)_

- We now check the source code of the repository with [LGTM][]. _(René Schwaiger)_
- We fixed various warnings about

  - missing or duplicated include guards,
  - undefined behavior,
  - incorrect format specifiers,
  - unnecessary statements,
  - short names for global variables, and
  - empty `if`-statements

  reported by [LGTM][]. _(René Schwaiger)_

[lgtm]: https://lgtm.com

## Build

### CMake

- The build system now rebuilds the [JNA binding](https://www.libelektra.org/bindings/jna) with Maven, if you change any of the Java source files of the binding. _(René Schwaiger)_
- `testshell_markdown_tutorial_crypto` is not compiled and executed if `gen-gpg-testkey` is not part of TOOLS. _(Peter Nirschl)_
- Plugin tests are now only added, if `BUILD_TESTING=ON`. _(Klemens Böswirth)_
- The symbol list for the static version is now exported directly from a CMake function. _(Klemens Böswirth)_

### Docker

#### Alpine Linux

- Our [Docker image for Alpine Linux](../../scripts/docker/alpine) now uses the base image for Alpine Linux 3.9. _(René Schwaiger)_
- We added [PEGTL](https://github.com/taocpp/PEGTL) to the [Alpine Docker image](../../scripts/docker/alpine). _(René Schwaiger)_

#### Debian

- We now use the default JDK on Debian sid, since the package `openjdk-8-jdk` is not available in the official unstable repositories anymore. _(René Schwaiger)_
- We added

  - [Bison](https://www.gnu.org/software/bison/), and
  - [YAEP](https://github.com/vnmakarov/yaep)

  to the [image for Debian sid](../../scripts/docker/debian/sid/Dockerfile). _(René Schwaiger)_

#### Ubuntu

- We added a [Dockerfile for Ubuntu Disco Dingo](../../scripts/docker/ubuntu/disco/Dockerfile). _(René Schwaiger)_

#### Other Updates

- The Docker images for

  - [Debian stretch](../../scripts/docker/debian/sid/Dockerfile), and
  - [Debian sid](../../scripts/docker/debian/sid/Dockerfile),

  now include the Python YAML library recommended by [cmake format](https://github.com/cheshirekow/cmake_format). _(René Schwaiger)_

### Vagrant

- The [Vagrant file for Ubuntu Artful Aardvark](../../scripts/vagrant/ubuntu/artful32/Vagrantfile) now installs the Python YAML library recommended by [cmake format](https://github.com/cheshirekow/cmake_format). _(René Schwaiger)_

## Infrastructure

### Cirrus

- We added the build job `🔗 Check`, which checks the documentation for broken links. _(René Schwaiger)_
- <<TODO>>
- <<TODO>>

### Jenkins

- We disabled the tests:

  - `testmod_crypto_botan`,
  - `testmod_crypto_openssl`,
  - `testmod_dbus`,
  - `testmod_dbusrecv`,
  - `testmod_fcrypt`,
  - `testmod_gpgme`, and
  - `testmod_zeromqsend`

  , since they are [known to fail in high load scenarios](https://issues.libelektra.org/2439). _(René Schwaiger)_

- We increased the automatic timeout for jobs that show no activity from 5 to 10 minutes. _(René Schwaiger)_
- We improved the exclusion patterns for the [Coveralls coverage analysis](https://coveralls.io/github/ElektraInitiative/libelektra). _(René Schwaiger)_
- We now again build the API docu of [master](https://doc.libelektra.org/api/master) and we now also build the API docu of [PRs](https://doc.libelektra.org/api/pr/). _(Markus Raab)_

### Restyled

- We added a [configuration file](../../.restyled.yaml) for [Restyled][]. Currently [Restyled][] monitors changes to Shell code in pull requests and fixes code that does not fit the [coding guideline](../CODING.md), by adding additional formatting commit to PRs. _(René Schwaiger)_

[restyled]: https://restyled.io

### Travis

- We removed the build job for the [Haskell binding](../../src/bindings/haskell/README.md) and [Haskell plugin](../../src/plugins/haskell/README.md). For more information, please take a look [here](https://issues.libelektra.org/2751). _(Klemens Böswirth)_
- We always use GCC 9 for the build job `🍏 GCC`. This update makes sure that the build job succeeds, even if Homebrew
  adds a new major version of the compiler. _(René Schwaiger)_
- We simplified our [Travis configuration file](../../.travis.yml), removing various unnecessary and unused code. In this process we also got rid of the caching directives, we previously used to speed up the Haskell build job `🍏 Haskell`. _(René Schwaiger)_

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- Added github build status badges to website _(hesirui)_
- We updated part of a test for the [snippet converter](https://www.libelektra.org/conversion). _(René Schwaiger)_
- Fixed anchor links on the website _(hesirui)_
- Added Docsearch to Website _(hesirui)_

- <<TODO>>

- <<TODO>>

- <<TODO>>

## Outlook

We are currently working on following topics:

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Statistics

Following authors made this release possible:

<<`scripts/git-release-stats 0.8.<<VERSION>>`>>

## Join the Initiative!

We welcome new contributors!
Read [here](https://www.libelektra.org/devgettingstarted/ideas) about how to get started.

As first step, you could give us feedback about these release notes.
Contact us via our [issue tracker](https://issues.libelektra.org).

## Get the Release!

You can download the release from [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz)
or [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz?raw=true)

The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums`>>

The release tarball is also available signed by Markus Raab using GnuPG from
[here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz.gpg) or on
[GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases//elektra-0.8.<<VERSION>>.tar.gz.gpg?raw=true)

Already built API-Docu can be found [here](https://doc.libelektra.org/api/0.8.<<VERSION>>/html/)
or on [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.8.<<VERSION>>).

## Stay tuned!

Subscribe to the
[RSS feed](https://www.libelektra.org/news/feed.rss)
to always get the release notifications.

If you also want to participate, or for any questions and comments
please contact us via our issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
