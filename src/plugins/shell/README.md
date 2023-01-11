- infos = Information about the shell plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = postgetstorage postcommit postrollback
- infos/status = nodep configurable experimental
- infos/description = executes shell commands

## Usage

The shell plugin executes shell commandos after set, get or error.

The configuration keys

- `execute/set`
- `execute/get`
- `execute/error`

are used to store the shell commands.

The configuration keys

- `execute/set/return`
- `execute/get/return`
- `execute/error/return`

can be compared against the return values of the shell commandos.

## Example

```sh
# Create temporary file
kdb set system:/tests/tempfile $(mktemp)

# Mount plugin and specify plugin configuration
sudo kdb mount shell.ini system:/tests/shell ni array= shell execute/set="echo set >> $(kdb get system:/tests/tempfile)"

cat $(kdb get system:/tests/tempfile)
#>

# Execute `set` command
kdb set system:/tests/shell ""
#> Create a new key system:/tests/shell with string ""

cat $(kdb get system:/tests/tempfile)
#> set

# Undo modifications
rm $(kdb get system:/tests/tempfile)
kdb rm -r system:/tests/shell
sudo kdb umount system:/tests/shell
```
