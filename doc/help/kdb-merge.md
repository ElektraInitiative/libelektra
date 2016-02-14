kdb-merge(1) -- Three-way merge of KeySets
==========================================

## SYNOPSIS

`kdb merge [options] ourpath theirpath basepath resultpath`  

* ourpath:
  Path to the keyset to serve as `ours`  

* theirpath:
  path to the keyset to serve as `theirs`  

* basepath:
  path to the `base` keyset  

* resultpath:
  path without keys where the merged keyset will be saved    

## DESCRIPTION

Does a three-way merge between keysets.  
On success the resulting keyset will be saved to mergepath.  
On unresolved conflicts nothing will be changed.  

## THREE-WAY MERGE

The `kdb merge` command uses a three-way merge by default.  
A three-way merge is when three versions of a file (or in this case, KeySet) are compared in order to automatically merge the changes made to the KeySet over time.  
These three versions of the KeySet are:  

* `base`:
  The `base` KeySet is the original version of the KeySet.  

* `ours`:
  The `ours` KeySet represents the user's current version of the KeySet.  
  This KeySet differs from `base` for every key you changed.  

* `theirs`:
  The `theirs` KeySet usually represents the default version of a KeySet (usually the package maintainer's version).  
  This KeySet differs from `base` for every key someone has changed.  

The three-way merge works by comparing the `ours` KeySet and the `theirs` KeySet to the `base` KeySet. By looking for differences  in these KeySets, a new KeySet called `result` is created that represents a merge of these KeySets.  

## CONFLICTS

Conflicts occur when a Key has a different value in all three KeySets.  
Conflicts in a merge can be resolved using a [strategy](#STRATEGIES) with the `-s` option.
To interactively resolve conflicts, use the `-i` option.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `s`, `--strategy <name>`:
  Specify which strategy should be used to resolve conflicts.
- `-v`, `--verbose`:
  Explain what is happening.
- `-i`, `--interactive`
  Interactively resolve the conflicts.


## EXAMPLES

To complete a simple merge of three KeySets:  
    `kdb merge user/ours user/theirs user/base user/result`  

To complete a merge whilst using the `ours` version of the KeySet to resolve conflicts:  
    `kdb merge -s ours user/ours user/theirs user/base user/result`  

To complete a three-way merge and overwrite all current keys in the `resultpath`:  
    `kdb merge -s cut user/ours user/theirs user/base user/result`  

## SEE ALSO

- [elektra-merge-strategy(7)](elektra-merge-strategy.md)
