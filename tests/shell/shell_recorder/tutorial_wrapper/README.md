# Markdown to Shell Recorder

The Markdown Shell Recorder extracts executable shell snippets from Markdown files and translate them into a suitable format for the [`shell_recorder`](../shell_recorder/README.md).

Snippets are shell commands inside a syntax block with additional checks (such as exit code, output, errors, etc) encoded as comments. These blocks start with ` ```sh ` and end with ` ``` `.

## Example

Let us look at a simple example first:

```sh
kdb set user:/tests/markdown/napalm death
#> Create a new key user:/tests/markdown/napalm with string "death"

kdb rm user:/tests/markdown/napalm

kdb rm system:/tests/markdown/babymetal
# RET: 11
# STDERR: Did not find the key
```

. The test above invokes three commands. The first command stores the value `death` in the key with the name
`user:/tests/markdown/napalm`. The special comment `#>` below the command specifies the expected output to the standard
output. This means the Markdown Shell Recorder expects the command

```
kdb set user:/tests/markdown/napalm death
```

to print the text

```
Create a new key user:/tests/markdown/napalm with string "death"
```

to the standard output. The second command in our test (`kdb rm /tests/markdown/napalm`) deletes the key we just created. Although there
are no special comments below the command, the Markdown Shell Recorder still checks the exit code of the command and reports a failure if
it is not `0`. If we expect another exit code we can use the special comment `# RET:` to specify the return code. This is what we did after
the third command, which will fail with exit code `1`, since it tries to delete a non-existing key. The Shell Recorder also checks the
value the last command prints to the standard error output, since we specified the expected text `Did not find the key` via the special
comment `# STDERR:`.

## Conventions

- Only add tests that store data below `/tests` (See also [TESTING.md](/doc/TESTING.md)).

## Add a Test

If you want to add a Markdown Shell Recorder tests for the `README.md` of your plugin, you can simply pass
`TEST_README` as argument to `add_plugin`.

To add other Markdown Shell Recorder tests for a certain Markdown file (such as an tutorial), use the CMake function `add_msr_test`:

```cmake
add_msr_test (name file)
```

. Note that test cases executed with `add_msr_test` use the **root of the source code repository as current working directory**.

The function `add_msr_test` also supports the argument `REQUIRED_PLUGINS` which allows you to specify which plugins need to be present in
order to run the Markdown Shell Recorder test.
If one of the specified plugins is missing, the test will not be added.

For example:

```
add_msr_test (tutorial_validation "${CMAKE_SOURCE_DIR}/doc/tutorials/validation.md" REQUIRED_PLUGINS validation)
```

adds the [validation tutorial](/doc/tutorials/validation.md) as Markdown Shell Recorder test and requires the plugin `validation` to be present.
If the plugin is missing, the test will not be added.

## Syntax

### Commands

- Lines not starting with a comment sign (`#`) are treated as (shell) commands. They are executed by the Shell Recorder.
- Commands starting with `sudo` will be executed without `sudo`.

#### Multi-line Commands

To extend a command over multiple lines add a backslash (`\`) at the end. Do not add a backlash at the last line of the multi-line command.
The test below shows some examples of multi-line commands.

```sh
echo Babymetal Death | \
  grep -o Death
#> Death

kdb set user:/tests/tempfile $(mktemp)
cat > $(kdb get user:/tests/tempfile) << EOF \
line 1\
line 2\
EOF
cat $(kdb get user:/tests/tempfile)
#> line 1
#> line 2

rm $(kdb get user:/tests/tempfile)
kdb rm user:/tests/tempfile
```

### Checks

All check start with a comment sign (`#`).

- `#> text`: The **text** after `#>` and the single space character afterwards is matched 1:1 against the command output. Multiple `#>` will be concatenated automatically using `⏎` (the Shell Recorder equivalent of `\n`). If you want to check for empty output you can also just use `#>` (without the single space character afterwards).

- `# RET: regex` This directive compares the return code (exit status) of the command to the value after `# RET:` . If not specified, the exit value is compared to `0`. The Shell Recorder uses **regular expressions** to compare the exit code, so an expression like `1|5` is also valid.

- `# ERROR: regex` Checks if the `kdb` command produced error `regex`. The text `regex` is a **regular expression** (e.g. `1|7` will check if the error `1` or the error `7` occurred).
  If you do not specify a regex, then the Markdown Shell Recorder will check if the command printed nothing to the standard error output.

- `# WARNINGS: csl` The Shell Recorder compares this **comma separated list** of numbers to the warnings thrown by a `kdb` command.

- `# STDOUT-REGEX: regex` The **regular expression** `regex` is matched against the output of the command. Newlines must be encoded as `⏎`.

### Comments

- All other lines starting with `#` (that aren’t checks) are treated as comments and will be ignored.

## Examples

For examples, please take a look at the ReadMe of plugins such as [YAMLCPP](/src/plugins/yamlcpp/README). The file [SyntaxCheck.md](SyntaxCheck.md) also contains some examples for the Markdown Shell Recorder syntax.

## Debugging

If a test case fails, a detailed protocol will be printed by the Shell Recorder.
By default `ctest` suppresses all output.
To print the output, use `-V` or `--output-on-failure`.
(`-V` additionally prints the executed command but does so for every executed test.)

### Interactive Debugging

Sometimes you want to inspect what happens at a specific line within the Shell Recorder.
This feature requires you to use either use

- `ctest --interactive-debug-mode 1` (with some limitations: you do not see what you type), or
- run the shell recorder directly, which can be done using (~e is the path to an Elektra checkout)

  ```bash
  cd build
  . ~e/scripts/dev/run_env
  tests/shell/shell_recorder/tutorial_wrapper/markdown_shell_recorder.sh path/to/file.md
  ```

. Once you started the Shell Recorder in either of these ways, you can simply use

```
interactive
```

as command to drop into a shell.
To drop out of the shell type `ctrl`+`D` or use `exit`.

By default `$SHELL` is used but you can also select the shell via an argument to `interactive`.
Note that the chosen shell needs to support `-i /dev/tty` as arguments.
The shells `dash`, `bash`, and `zsh` are known to work, `fish` only executes commands
after you dropped out of the shell.
