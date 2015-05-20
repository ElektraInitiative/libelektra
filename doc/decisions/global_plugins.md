# Global Plugins

## Issue

- Checker plugins see only part of the configuration
- Notification does not happen once after final commit, but for every
    plugin
- Some plugins are not implementable, e.g. global locks

## Constraints

- Honor arrays
- Plugin interface should be the same

## Assumptions

## Considered Alternatives

## Decision

Implement global plugins. Configuration will be in arrays below

    system/elektra/global_mountpoints/
        prerollback
        postrollback
        preget
        postget
        preset
        setstorage
        precommit
        postcommit

Plugins state in contract that they work as global plugin, i.e.
do not need to work on individual config files.

## Argument

### Transformation

    [dir/a]
    foreign=/x
    transform/python=self+5
             /lua=..

- preget: fetch all foreign keys (kdbGet)
- postget: run transformation for all foreign keys

### Global lock

simplifies threading and process locking.

### Shell plugins

Run shell code at end of all plugins, e.g. doing

    git add
    git commit

## Implications

### Default global plugins

Its useful to have some important global plugins, e.g. locking by default.
Internal list to be used when no system/elektra/global_mountpoints/ exists


## Related decisions

## Notes

### Open Points

- How to test global plugins?
