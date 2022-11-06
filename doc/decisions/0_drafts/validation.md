# Validation

## Problem

Validation plugins operate as independent blackboxes.
For every backend each mounted validation plugin iterates
over the whole keyset, checks every key for its trigger metakey,
and validates the key.

Currently all needed validation plugins need to be specified at
mount-time - if additional validation is required, the backend
has to be remounted with the required plugins and plugin
configuration.

If validation of a key fails, each plugin decides on its own
how to handle the issue and proceed in ways that might be
different from what is expected or desired.

## Constraints

## Assumptions

While plugins should always fail and return an error if validation
fails on kdbSet, there are several different requirements
for what should happen on kdbGet and handle problems e.g.

- only issue warnings

  we want to read the whole configuration, but issue warnings
  if keys fail to validate instead

  problems are handled external by an application, user, ...

- remove invalid keys

  we want to read the whole configuration, but drop invalid keys

  invalid keys might be replaced by default values, requested
  from the user, ...

- fail with error

  we only want to read valid configurations, and fail with
  an error if the configuration is invalid

## Considered Alternatives

- Extend validation plugins to allow us to specify what should happen
  if a key fails to validate
- Export a validation function that allows us to use an additional plugin
  to decide what should be done

## Decision

Use a wrapper plugin to iterate over the keyset and delegate the validation
of each key to the corresponding validation plugin.

## Rationale

- Validation plugins don't need to know what should be done if the validation fails
- We can run multiple validations on every key and improve error messages
- Different ways of handling errors only need to be implemented once

## Implications

validation plugins have to export their validation routine

`static int validateKey(const Key * key, const Key * errorKey)`

returning 1 if validation succeeded, 0 on failure

## Related Decisions

## Notes
