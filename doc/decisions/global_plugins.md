# Global Plugins

## Issue

- Checker plugins see only part of the configuration
- Notification does not happen once after final commit, but for every
    plugin

## Constraints

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


## Argument

## Implications

## Related decisions

## Notes
