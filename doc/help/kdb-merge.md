# kdb-merge(1) -- Three-way merge of KeySets

## SYNOPSIS

`kdb merge [options] ourpath theirpath basepath resultpath`<br>

- ourpath:
  Path to the keyset to serve as `ours`<br>

- theirpath:
  path to the keyset to serve as `theirs`<br>

- basepath:
  path to the `base` keyset<br>

- resultpath:
  path without keys where the merged keyset will be saved<br>

## DESCRIPTION

Does a three-way merge between keysets.<br>
On success the resulting keyset will be saved to mergepath.<br>
On unresolved conflicts nothing will be changed.<br>

## THREE-WAY MERGE

The `kdb merge` command uses a three-way merge by default.<br>
A three-way merge is when three versions of a file (or in this case, KeySet) are compared in order to automatically merge the changes made to the KeySet over time.<br>
These three versions of the KeySet are:<br>

- `base`:
  The `base` KeySet is the original version of the KeySet.<br>

- `ours`:
  The `ours` KeySet represents the user's current version of the KeySet.<br>
  This KeySet differs from `base` for every key you changed.<br>

- `theirs`:
  The `theirs` KeySet usually represents the default version of a KeySet (usually the package maintainer's version).<br>
  This KeySet differs from `base` for every key someone has changed.<br>

The three-way merge works by comparing the `ours` KeySet and the `theirs` KeySet to the `base` KeySet. By looking for differences in these KeySets, a new KeySet called `result` is created that represents a merge of these KeySets.<br>

## CONFLICTS

Conflicts occur when a Key has a different value in all three KeySets.<br>
Conflicts in a merge can be resolved using a [strategy](#STRATEGIES) with the `-s` option.
To interactively resolve conflicts, use the `-i` option.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-f`, `--force`:
  Will remove existing keys from `resultpath` instead of failing.
- `-s`, `--strategy <name>`:
  Specify which strategy should be used to resolve conflicts.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.
- `-i`, `--interactive`
  Interactively resolve the conflicts.

## RETURN VALUE

- 0:
  Successful.

- 11:
  A conflict occurred during the merge.

## EXAMPLES

To complete a simple merge of three KeySets:<br>
`kdb merge user:/ours user:/theirs user:/base user:/result`<br>

To complete a merge whilst using the `ours` version of the KeySet to resolve conflicts:<br>
`kdb merge -s ours user:/ours user:/theirs user:/base user:/result`<br>

To complete a three-way merge and overwrite all current keys in the `resultpath`:<br>
`kdb merge -s cut user:/ours user:/theirs user:/base user:/result`<br>

## SEE ALSO

- [elektra-merge-strategy(7)](elektra-merge-strategy.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
