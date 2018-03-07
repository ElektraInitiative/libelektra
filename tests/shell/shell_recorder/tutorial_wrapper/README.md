# Markdown to Shell Recorder

The Markdown Shell Recorder extracts executable code snippets from Markdown files and translate them into a suitable format for [`shell_recorder`](../shell_recorder/README.md).

Snippets are shell commands inside a syntax block with additional checks (such as exit code, output, errors, etc) encoded as comments. These blocks start with ```` ```sh ````  and end with ```` ``` ````.

## Example

Let us look at a simple example test first:

```sh
kdb set /examples/markdown/napalm death
#> Using name user/examples/markdown/napalm
#> Create a new key user/examples/markdown/napalm with string "death"

kdb rm /examples/markdown/napalm

kdb rm /examples/markdown/babymetal
# RET: 1
# STDERR: Did not find the key
```

. The test above invokes three commands. The first command sets the [cascading key](/doc/tutorials/cascading.md)
`/examples/markdown/napalm` to the value `death`. The special comment `#> ` below the command specifies the expected output to the standard
output. This means the Markdown Shell Recorder expects the command

```
kdb set /examples/markdown/napalm death
```

to print the text

```
Using name user/examples/markdown/napalm
Create a new key user/examples/markdown/napalm with string "death"
```

to the standard output. The second command in our test (`kdb rm /examples/markdown/napalm`) deletes the key we just created. Although there
are no special comments below the command, the Markdown Shell Recorder still checks the exit code of the command and reports a failure if
it is not `0`. If we expect another exit code we can use the special comment `# RET:` to specify the return code. This is what we did after
the third command, which will fail with exit code `1`, since it tries to delete a non-existing key. The Shell Recorder also checks the
value the last command prints to the standard error output since, we specified the expected text `Did not find the key` via the special
comment `# STDERR:`.

## Syntax

### Commands

- Lines not starting with a comment sign (`#`) are treated as (shell) commands. They are  executed by the Shell Recorder.
- Commands starting with `sudo` will be executed without `sudo`.

#### Multiline Commands

To extend a command over multiple lines add a backslash (`\`) at the end. Do not add a backlash at the last line of the multiline command.
The test below shows some examples of multiline commands.

```sh
echo Babymetal Death | \
  grep -o Death
#> Death

kdb set user/examples/tempfile $(mktemp)
cat > $(kdb get user/examples/tempfile) << EOF \
line 1\
line 2\
EOF
cat $(kdb get user/examples/tempfile)
#> line 1
#> line 2

rm $(kdb get user/examples/tempfile)
kdb rm user/examples/tempfile
```

### Checks

All check start with a comment sign (`#`).

- `#> text`: The **text** after `#> ` is matched 1:1 against the command output. Multiple `#> ` will be concatenated automatically using `⏎` (the Shell Recorder equivalent of `\n`).

- `# RET: regex` This directive compares the return code (exit status) of the command to the value after `# RET:` . If not specified, the exit value is compared to `0`. The Shell Recorder uses **regular expressions** to compare the exit code, so an expression like `1|5` is also valid.

- `# ERROR: regex` Checks if the `kdb` command produced error `regex`. The text `regex` is a **regular expression** (e.g. `1|7` will check if the error `1` or the error `7` occurred).

- `# WARNINGS: csl` The Shell Recorder compares this **comma separated list** of numbers to the warnings thrown by a `kdb` command.

- `# STDOUT-REGEX: regex` The **regular expression** `regex` is matched against the output of the command. Newlines must be encoded as `⏎`.

## Comments

- All lines starting with `#` that aren’t checks are treated as comments and will be ignored.

## Examples

For examples, please take a look at a the ReadMe of plugins such as [YAMLCPP](/src/plugins/yamlcpp/README). The file [SyntaxCheck.md](SyntaxCheck.md) also contains some examples for the Markdown Shell Recorder syntax.
