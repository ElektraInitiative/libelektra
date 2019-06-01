# CODING

This document provides an introduction in how the source code of
libelektra is organized and how and where to add functionality.

Make sure to read [DESIGN](/doc/DESIGN.md) together with this document.

## Folder structure

After you downloaded and unpacked Elektra you should see some folders.
The most important are:

- **src:** This directory contains the source of the libraries, tools and plugins.
- **doc:** General documentation for the project and the core library.
- **examples:** Examples on how to use the core library.
- **tests:** Contains the testing framework for the source (**src**).

## Source Code

libelektra is the ANSI/ISO C99-Core which coordinates the interactions
between the user and the plugins.

The plugins have all kinds of dependencies. It is the responsibility of
the plugins to find and check them using CMake. The same guidelines
apply to all code in the repository including the plugins.

`libloader` is responsible for loading the backend modules. It works on
various operating systems by using `libltdl`. This code is optimized
for static linking and win32.

kdb is the commandline-tool to access and initialize the Elektra database.

### General Guidelines

You are only allowed to break a guideline if there is a good reason
to do so. When you do, document the fact as comment next to the code.

Of course, all rules of good software engineering apply: Use meaningful
names and keep the software both testable and reusable.

The purpose of the guidelines is to have a consistent
style, not to teach programming.

If you see code that breaks guidelines do not hesitate to fix them. At least put a
TODO marker to make the places visible.

If you see inconsistency within the rules do not hesitate to talk about it with the
intent to add a new rule here.

See [DESIGN](/doc/DESIGN.md) document too, they complement each other.

### Code Comments

Code is not only for the computer, but it should be readable for humans, too.
Up-to-date code comments are essential to make code understandable for others.
Thus please use following techniques (in order of preference):

1. Comment functions with `/**/` and Doxygen, see below.

2. You should also add assertions to state what should be true at a specific
   position in the code. Their syntax is checked and they are automatically
   verified at run-time. So they are not only useful for people reading the
   code but also for tools. Assertions in Elektra are used by:

   `#include <kdbassert.h>`

   `ELEKTRA_ASSERT (condition, "formatted text to be printed when assert fails", ...)`

   Note: Do not use assert for user-APIs, always handle arguments of user-APIs like
   untrusted input.

3. If the "comment" might be useful to be printed during execution, use logging:

   `#include <kdblogger.h>`

   `ELEKTRA_LOG ("formatted text to be printed according to log filters", ...)`

   Read [HERE](/doc/dev/logging.md) for how to enable the logger.

4. Otherwise comment within source with `//` or with `/**/` for multi-line
   comments.

### Coding Style

- Limits

  - Functions should not exceed 100 lines.
  - Files should not exceed 1000 lines.
  - A line should not be longer than 140 characters.

  Split up when those limits are reached.
  Rationale: Readability with split windows.

- Indentation

  - Use tabs for indentation.
  - One tab equals 8 spaces.

- Blocks

  - Use blocks even for single line statements.
  - Curly braces go on a line on their own on the previous indentation level.
  - Avoid multiple variable declarations at one place.
  - Declare Variables as late as possible, preferable within blocks.

- Naming

  - Use camelCase for functions and variables.
  - Start types with upper-case, everything else with lower-case.
  - Prefix names with `elektra` for internal usage. External API either starts
    with `ks`, `key` or `kdb`.

- Whitespaces

  - Use space before and after equal when assigning a value.
  - Use space before round parenthesis ( `(` ).
  - Use space before and after `*` from Pointers.
  - Use space after `,` of every function argument.

The [reformat script](/scripts/reformat-source) can ensure most code style rules,
but it is obviously not capable of ensuring everything (e.g. naming conventions).
So do not give this responsibility out of hands entirely.

### C Guidelines

- The compiler shall not emit any warning (or error).
- Use goto only for error situations.
- Use `const` as much as possible.
- Use `static` methods if they should not be externally visible.
- C-Files have extension `.c`, Header files `.h`.
- Use internal functions: prefer to use `elektraMalloc`, `elektraFree`.

**Example:** [src/libs/elektra/kdb.c](/src/libs/elektra/kdb.c)

#### Clang Format

To guarantee consistent formatting we use [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html) (version `6.0` or version `7.0`) to format all C and C++ code in the repository. Since our build servers also check the style for every pull request you might want to make sure you reformat your C/C++ code changes with this tool.

To find out which version of `clang-format` a certain build server uses please check:

- the [Debian sid Docker image](../scripts/docker/debian/sid/Dockerfile),
- the [Travis configuration file ](../.travis.yml), and
- the [Cirrus configuration file](../.cirrus.yml)

and search for the relevant packages (`clang-format`, `llvm`). Currently we use

- clang-format `6.0` in the [Debian sid image](../scripts/docker/debian/sid/Dockerfile) on the Jenkins build server,
- clang-format `7.0` in the [Travis configuration file ](../.travis.yml), and
- clang-format `7.0` in the [Cirrus macOS](../.cirrus.yml) build jobs

.

##### Installation

###### macOS

On macOS you can install `clang-format` using [Homebrew][] either directly:

```sh
brew install clang-format
```

or by installing the whole [LLVM](http://llvm.org) infrastructure:

```sh
brew install llvm
```

. Please note, that both of these commands will install current versions of `clang-format` that might format code a little bit differently than Clang-Format `6.0` in certain edge cases. If you want you can also install Clang-Format `7.0` using LLVM `7.0`:

```
brew install llvm@7
```

.

[homebrew]: https://brew.sh

###### Debian

In Debian the package for Clang-Format `6.0` is called `clang-format-6.0`:

```sh
apt-get install clang-format-6.0
```

.

##### Usage

For the basic use cases you can use `clang-format` directly. To do that, just call the tool using the option `-i` and specify the name of the files you want to reformat. For example, if you want to reformat the file `src/bindings/cpp/include/kdb.hpp` you can use the following command:

```sh
# On some systems such as Debian the `cmake-format` executable also contains
# the version number. For those systems, please replace `clang-format`,
# with `clang-format-6.0` or `clang-format-7` in the command below.
clang-format -i src/bindings/cpp/include/kdb.hpp
```

. While this works fine, if you want to format only a small number of file, formatting multiple files can be quite tedious. For that purpose you can use the script [`reformat-source`](../scripts/reformat-source) that reformats all C and C++ code in Elektra’s code base

```sh
scripts/reformat-source # This script will probably take some seconds to execute
```

.

##### Tool Integration

If you work on Elektra’s code base regularly you might want to integrate the formatting step directly in your development setup. [ClangFormat’s homepage](https://clang.llvm.org/docs/ClangFormat.html) includes a list of integrations for various tools that should help you to do that. Even if this webpage does not list any integrations for your favorite editor or IDE, you can usually add support for external tools such as `clang-format` to advanced editors or IDEs pretty easily.

###### TextMate

While [TextMate][] supports `clang-format` for the current file directly via the keyboard shortcut <kbd>ctrl</kbd>+<kbd>⇧</kbd>+<kbd>H</kbd>, the editor does not automatically reformat the file on save. To do that

1. open the bundle editor (“Bundles” → “Edit Bundles”),
2. navigate to the “Reformat Code” item of the C bundle (“C” → “Menu Actions” → “Reformat Code”),
3. insert `callback.document.will-save` into the field “Semantic Class”, and
4. change the menu option for the field “Save” to “Nothing”

. After that change TextMate will reformat C and C++ code with `clang-format` every time you save a file.

[textmate]: https://macromates.com

### C++ Guidelines

- Everything as in C if not noted otherwise.
- Do not use goto at all, use RAII instead.
- Do not use raw pointers, use smart pointers instead.
- C++-Files have extension `.cpp`, Header files `.hpp`.
- Do not use `static`, but anonymous namespaces.
- Write everything within namespaces and do not prefix names.
- Oriented towards [more safe and modern usage](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md).

**Example:** [src/bindings/cpp/include/kdb.hpp](/src/bindings/cpp/include/kdb.hpp)

### CMake Guidelines

We use a similar style for CMake as we do for other code:

- The length of a functions should not exceed 100 lines.
- The length of a file should not exceed 1000 lines.
- A line should not be longer than 140 characters.
- Use tabs for indentation.
- One tab equals 8 spaces.
- Declare variables as late as possible, preferable within blocks.
- Add a space character before round parenthesis ( `(` ).
- Use lower case for command names (e.g. `set` instead of `SET`)

#### cmake format

We use [`cmake-format`](https://github.com/cheshirekow/cmake_format) to reformat code according to the guidelines given above. Since
`cmake-format` currently does not support tabs, we use the standard command `unexpand` to fix this issue. For example, to reformat the
file `CMakeLists.txt` in the root folder of the repository we use the following command:

```sh
# This command uses `sponge`, which is part of the [moreutils](https://joeyh.name/code/moreutils/) package.
cmake-format CMakeLists.txt | unexpand | sponge CMakeLists.txt
```

.

##### Installation

Since `cmake-format` is written in [Python](https://www.python.org) you usually install it via Python’s package manager `pip`:

```sh
# Install cmake format `0.4.5` with support for YAML config files
pip install cmake-format[yaml]==0.4.5
```

. Please make sure, that you install the correct version (`0.4.5`) of cmake format:

```sh
cmake-format --version
#> 0.4.5
```

, since otherwise the formatted code might look quite different.

We also use the [moreutils](https://joeyh.name/code/moreutils) in our [CMake formatting script](../scripts/reformat-cmake), which you can install on macOS using [Homebrew][]:

```sh
brew install moreutils
```

and on Debian using `apt-get`:

```
apt-get install moreutils
```

.

##### Usage

If you want to reformat the whole codebase you can use the script [`reformat-cmake`](../scripts/reformat-cmake):

```sh
scripts/reformat-cmake # Running this script for the whole code base takes some time.
```

. To reformat specific files add a list of file paths after the command:

```sh
# The command below reformats the file `cmake/CMakeLists.txt`.
scripts/reformat-cmake cmake/CMakeLists.txt
```

.

##### Tool Integration

If you work on CMake code quite often you probably want to integrate cmake format into your development workflow. The homepage of [cmake format](https://github.com/cheshirekow/cmake_format#integrations) list some integration options.

###### TextMate

While TextMate does not support cmake format directly, you can quickly create a command that applies `cmake-format` every time you save a CMake file yourself. The steps below show one option to do that.

1. Open the “Bundle Editor”: Press <kbd>^</kbd> + <kbd>⌥</kbd> + <kbd>⌘</kbd> + <kbd>B</kbd>
2. Create a new command:
   1. Press <kbd>⌘</kbd> + <kbd>N</kbd>
   2. Select “Command”
   3. Press the button “Create”
3. Configure your new command

   1. Use “Reformat Document” or a similar text as “Name”
   2. Enter `source.cmake` in the field “Scope Selector”
   3. Use <kbd>^</kbd> + <kbd>⇧</kbd> + <kbd>H</kbd> as “Key Equivalent”
   4. Copy the text `callback.document.will-save` into the field “Semantic Class”
   5. Select “Document” as “Input”
   6. Select “Replace Document” in the dropdown menu for the option “Output”
   7. Select “Line Interpolation” in the menu “Caret Placement”
   8. Copy the following code into the text field:

      ```sh
      #!/bin/bash

      set -o pipefail
      if ! "${TM_CMAKE_FORMAT:-cmake-format}" - |
          ${TM_CMAKE_FORMAT_FILTER:-tee};
      then
      	. "$TM_SUPPORT_PATH/lib/bash_init.sh"
      	exit_show_tool_tip
      fi
      ```

   9. Save your new command: <kbd>⌘</kbd> + <kbd>S</kbd>
   10. Store the value `unexpand` in the variable `TM_CMAKE_FORMAT_FILTER`. To do that save the text

   ```ini
   TM_CMAKE_FORMAT_FILTER = "unexpand"
   ```

   in a file called [`.tm_properties`](https://macromates.com/blog/2011/git-style-configuration) in the root of Elektra’s repository.

### Java / Groovy Guidelines

Please follow
[Google Java Style Guide](https://google.github.io/styleguide/javaguide.html)
for Java and Groovy (used by Jenkins) files.

Most notably use:

- 2 spaces for indentation
- Variable and function names in lowerCamelCase
- K & R style brackets

### Markdown Guidelines

- File Ending is `.md` or integrated within Doxygen comments
- Only use `#` characters at the left side of headers/titles
- Use [fences](https://help.github.com/en/articles/creating-and-highlighting-code-blocks) for code/examples
- Prefer fences which indicate the used language for better syntax highlighting
- Fences with sh are for the [shell recorder syntax](/tests/shell/shell_recorder/tutorial_wrapper)
- `README.md` and tutorials should be written exclusively with shell recorder syntax
  so that we know that the code in the tutorial produces output as expected
- Please use [**title-case**](https://en.wiktionary.org/wiki/title_case) for headings in the general documentation.
- For [man pages](help/) please use **only capital letters for subheadings** and only **small letters for the main header**. We use this header style to match the look and feel of man pages for Unix tools such as `ls` or `mkdir`.

#### Prettier

We use [`prettier`][] to format the documentation according to the guidelines given above.

Under certain **exceptional** circumstances you might want to prevent `prettier` from formatting certain parts of a Markdown file. To do
that you can

- enclose the Markdown code in `<!-- prettier-ignore-start -->` and `<!-- prettier-ignore-end -->` tags, or
- use `<!-- prettier-ignore -->` to disable formatting till the end of a file

.

[`prettier`]: https://prettier.io

##### Installation

###### macOS

On macOS you can install [`prettier`][] using [Homebrew][]:

```sh
brew install prettier
```

.

###### General

To install [`prettier`][] using Node’s package manager [npm](https://www.npmjs.com) you can use the command below

```sh
npm install --global prettier@1.17.1
```

.

##### Usage

You can format all Markdown files in the repository using the script [`reformat-markdown`](../scripts/reformat-markdown):

```sh
scripts/reformat-markdown
```

. To format only some files, please specify a list of filenames after the command:

```sh
scripts/reformat-markdown doc/CODING.md # Reformat this file
```

.

##### Tool Integration

The [homepage of Prettier][`prettier`] lists various options to integrate the tool into your workflow.

###### TextMate

To reformat a Markdown document in [TextMate][] every time you save it, please follow the steps listed below.

1. Open the “Bundle Editor”: Press <kbd>^</kbd> + <kbd>⌥</kbd> + <kbd>⌘</kbd> + <kbd>B</kbd>
2. Create a new command:
   1. Press <kbd>⌘</kbd> + <kbd>N</kbd>
   2. Select “Command”
   3. Press the button “Create”
3. Configure your new command

   1. Use “Reformat Document” or a similar text as “Name”
   2. Enter `text.html.markdown` in the field “Scope Selector”
   3. Use <kbd>^</kbd> + <kbd>⇧</kbd> + <kbd>H</kbd> as “Key Equivalent”
   4. Copy the text `callback.document.will-save` into the field “Semantic Class”
   5. Select “Document” as “Input”
   6. Select “Replace Input” in the dropdown menu for the option “Output”
   7. Select “Line Interpolation” in the menu “Caret Placement”
   8. Copy the following code into the text field:

      ```sh
      #!/bin/bash

      if ! "${TM_PRETTIER:-prettier}" --stdin --stdin-filepath "${TM_FILEPATH}"
      then
      	. "$TM_SUPPORT_PATH/lib/bash_init.sh"
      	exit_show_tool_tip
      fi
      ```

   9. Save your new command: <kbd>⌘</kbd> + <kbd>S</kbd>

### Shell Guidelines

- Please only use [POSIX](https://en.wikipedia.org/wiki/POSIX) functionality.

#### shfmt

We use [`shfmt`][] to format Shell files in the repository.

[`shfmt`]: https://github.com/mvdan/sh

##### Installation

###### macOS

You can install [`shfmt`] on macOS using [Homebrew][]:

```sh
brew install shfmt
```

.

###### General

[shfmt’s GitHub release page](https://github.com/mvdan/sh/releases) offers binaries for various operating systems. For example, to install the binary for the current user on Linux you can use the following command:

```sh
mkdir -p "$HOME/bin" && cd "$HOME/bin" && \
  curl -L "https://github.com/mvdan/sh/releases/download/v2.6.4/shfmt_v2.6.4_linux_amd64" -o shfmt && \
  chmod u+x shfmt
```

. Please note that you have to make sure, that your `PATH` includes `$HOME/bin`, if you use the command above:

```sh
export PATH=$PATH:"$HOME/bin"
```

.

##### Usage

We provide the script [`reformat-shfmt`](../scripts/reformat-shfmt) that formats the whole codebase with [`shfmt`][]:

```sh
scripts/reformat-shfmt
```

. You can also reformat specific files by listing filenames after the script:

```sh
scripts/reformat-shfmt scripts/reformat-shfmt # Reformat the source of `reformat-shfmt`
```

.

##### Tool Integration

The GitHub project page of [`shfmt`][] offers some options to integrate the tool into your development workflow [here](https://github.com/mvdan/sh#related-projects).

###### TextMate

The steps below show you how to create a [TextMate][] command that formats a documents with [`shfmt`][] every time you save it.

1. Open the “Bundle Editor”: Press <kbd>^</kbd> + <kbd>⌥</kbd> + <kbd>⌘</kbd> + <kbd>B</kbd>
2. Create a new command:
   1. Press <kbd>⌘</kbd> + <kbd>N</kbd>
   2. Select “Command”
   3. Press the button “Create”
3. Configure your new command

   1. Use “Reformat Document” or a similar text as “Name”
   2. Enter `source.shell` in the field “Scope Selector”
   3. Use <kbd>^</kbd> + <kbd>⇧</kbd> + <kbd>H</kbd> as “Key Equivalent”
   4. Copy the text `callback.document.will-save` into the field “Semantic Class”
   5. Select “Document” as “Input”
   6. Select “Replace Input” in the dropdown menu for the option “Output”
   7. Select “Line Interpolation” in the menu “Caret Placement”
   8. Copy the following code into the text field:

      ```sh
      #!/bin/bash

      if ! "${TM_SHFMT_FORMAT:-shfmt}" -s -sr; then
      	. "$TM_SUPPORT_PATH/lib/bash_init.sh"
      	exit_show_tool_tip
      fi
      ```

   9. Save your new command: <kbd>⌘</kbd> + <kbd>S</kbd>

### Doxygen Guidelines

`doxygen` is used to document the API and to build the html and pdf output.
We also support the import of Markdown pages. Doxygen 1.8.8 or later
is required for this feature (Anyways you can find the
[API Doc](https://doc.libelektra.org/api/latest/html/) online).
Links between Markdown files will be converted with the
[Markdown Link Converter](/doc/markdownlinkconverter/README.md).
**Markdown pages are used in the pdf, therefore watch which characters you use and
provide a proper encoding!**

- use `@` to start Doxygen tags
- Do not duplicate information available in git in Doxygen comments.
- Use `@copydoc`, `@copybrief` and `@copydetails` intensively (except for file headers).

### File Headers

Files should start with:

```c
	/**
	 * @file
	 *
	 * @brief <short statement about the content of the file>
	 *
	 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
	 */
```

Note:

- `@` `file` has _no_ parameters.
- `@` `brief` should contain a short statement about the content of the file and is needed
  so that your file gets listed at https://doc.libelektra.org/api/latest/html/files.html

The duplication of the filename, author and date is not needed, because
this information is tracked using git and [doc/AUTHORS.md](AUTHORS.md) already.
