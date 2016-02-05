# Bootstrap

## Issue

Currently the default backend (default.ecf) will also be used for bootstrapping. There are two problems with this approach:

1. Thus the default backend will first will be read with parentKey `system/elektra` and later with parentKey `system`, it needs to store absolute pathes and thus won't work with the current INI plugin
2. When `system` is large without mountpoints, everything is reread twice during bootstrapping.

## Constraints

- be compatible to mountpoints stored in `defaults.ecf`
- nice migration phase
- new setups should never bother about the compatibility mode

## Assumptions

- Bootstrap should be fast and not unnecessarily read large files

## Considered Alternatives

- Implement a hack so that `system/elektra` is actually read as `system`. (Will not solve problem 2.)
  - Its a hack.
  - Its confusing and does not play well with persistent data with relative key names.
- Split up without compatibility mode: would need to migrate all mountpoints by exporting (with old version!) and then importing (with new version!)
  - I consider this too error prone, people might easily forget to export with the old version and then discard their mountpoints unintentional.

## Decision

Split up the concepts of default (`default.ecf`) and bootstrap (`elektra.ecf`) backend.
During bootstrap only `elektra.ecf` is read.
The default backend reading `default.ecf` is only relevant as long as no root backend is mounted.

Algorithm:

1. try to get system/elektra using the file elektra.ecf (KDB_DB_INIT)
2. if it works, mount the init backend to system/elektra (non-fallback mode)
3. if it fails (== 0 or == -1), try default.ecf as fallback
4. if the fallback works (i.e. keys are present in system/elektra), mount the default backend to system/elektra (fallback mode)


## Argument

- Solves both problems
- Is fully compatible with any existing setup
- People can decide if and how to migrate


## Implications

- Fallback mode should be removed with 1.0
- added scripts/upgrade-bootstrap

## Related decisions

## Notes

to upgrade to new system, either:

- touch /etc/kdb/elektra.ecf (loses old mountpoints)
- or do kdb export system/elektra/mountpoints, kdb rm -r system/elektra/mountpoints, kdb import system/elektra/mountpoints
