# Markdown to Shell Recorder

The Markdown Shell Recorder extracts executable code snippets from Markdown files and translate them into a suitable format for [`shell_recorder`](../shell_recorder/README.md).

Snippets are shell commands inside a syntax block with additional checks (such as exit code, output, errors, etc) encoded as comments. These blocks start with ```` ```sh ````  and end with ```` ``` ````.

## Syntax

### Commands

- Lines not starting with a comment sign (`#`) are treated as (shell) commands. They are  executed by the Shell Recorder.
- Commands starting with `sudo` will be executed without `sudo`.
- To extend a command over multiple lines add a backslash (`\`) at the end. Do not add a backlash at the last line of the multiline command.

### Checks

All check start with a comment sign (`#`).

- `#> text`: The **text** after `#> ` is matched 1:1 against the command output. Multiple `#> ` will be concatenated automatically using `⏎` (the Shell Recorder equivalent of `\n`).

- `# STDOUT: text`: The **text** after this directive is matched 1:1 against the command output. Newlines must be encoded as `⏎`.

- `# RET: regex` This directive compares the return code (exit status) of the command to the value after `# RET:` . If not specified, the exit value is compared to `0`. The Shell Recorder uses **regular expressions** to compare the exit code, so an expression like `1|5` is also valid.

- `# ERROR: csl` Checks if the `kdb` command threw the exit code `csl`. The text `csl` is a **comma separated list** of numbers (e.g. `1`, `2,4`).

- `# WARNINGS: csl` The Shell Recorder compares this **comma separated list** of numbers to the warnings thrown by a `kdb` command.

- `# STDOUT-REGEX: regex` The **regular expression** `regex` is matched against the output of the command. Newlines must be encoded as `⏎`.

## Comments

- All lines starting with `#` that aren’t checks are treated as comments and will be ignored.

## Examples

For examples, please take a look at a the ReadMe of plugins such as [YAMLCPP](/src/plugins/yamlcpp/README). The file [SyntaxCheck.md](SyntaxCheck.md) also contains some examples for the Markdown Shell Recorder syntax.
