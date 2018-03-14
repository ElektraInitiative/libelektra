# Deferred Plugin Calls

## Issue

Since some plugins may lazy-load encapsulated plugins at a later time a
mechanism for deferring function calls to this encapsulated plugins is needed.

For example when setting I/O bindings with `elektraIoSetBinding()` the exported
function `setIoBinding` is called for all globally mounted plugins.
The "list" does not export this functions and cannot implement every exported
function for its encapsulated plugins.

## Constraints

1. Plugins encapsulating other plugins shall be able to lazy-load them.
2. Callers shall not be entangled in encapsulating plugin details.
3. The encapsulation of the plugins shall not be broken.
4. The mechanism shall be generic (not only for I/O bindings).

## Assumptions

1. The called functions do not return a value (e.g. `set`, `open`, `close`, ...)

## Considered Alternatives

- Plugins like the "list" plugin export a function called
  `getEncapsulatedPlugins`. This would break encapsulation and
  break lazy-loading since to return the encapsulated plugin handles they have
  to be loaded.

## Decision

Encapsulating plugins export a function called `deferFunctionCall` with the
declaration `void deferFunctionCall (char * name, KeySet * parameters)`.
Encapsulating plugins shall save multiple deferred calls and call the exported
functions specified by `name` passing the `parameters` KeySet when a plugin is
initialized in the same order as received.
Encapsulated plugins that do not export a specified function are omitted.

## Argument

The solution allows to change the "list" plugin implementation without breaking
the `elektraIoSetBinding()` or `elektraNotificationOpen()`.

## Implications

## Related decisions

## Notes

Utility functions that help with managing deferred calls would be nice:

- `ElektraDeferredCallList * elektraDeferredCallCreateList (void)`
- `ElektraDeferredCall * elektraDeferredCallAdd (ElektraDeferredCallList * list, char * name, KeySet * parameters)`
- `void elektraDeferredCallsExecute (Plugin * plugin, ElektraDeferredCallList * list)`
