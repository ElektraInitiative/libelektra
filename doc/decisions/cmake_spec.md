# CMake Spec

## Issue

The compilation variants of plugins blow up the number of plugins.
Additionally there is the concept of default storage + resolver that is
needed for bootstrapping plugins.

## Constraints

- full default resolver need to be different than other default resolver
  for testing
- there is no standard resolver, they always should state their
  configuration

## Assumptions

- keep it not too difficult too configure, even though most people will
  go for the defaults

## Considered Alternatives

- many CMake variables for every case
  KDB_DEFAULT_STORAGE, KDB_DEFAULT_RESOLVER
  for dynamic and static cases
- Have a PLUGINS field that state what is the default dynamic and static
  plugin, e.g. ! for storage ? for resolver ds?resolver_b_u_b;ds!dump

## Decision


## Argument

## Implications

## Related decisions

## Notes
