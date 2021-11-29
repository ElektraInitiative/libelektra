# Deferred Plugin Calls

## Issue

Calling exported functions is a primary way for coordinating plugins in Elektra.
Since some plugins encapsulate other plugins, exported functions from
encapsulated plugins become unavailable.

Since encapsulating plugins cannot implement and forward calls for every
function exported by encapsulated plugins a generic mechanism for function
calls to these encapsulated plugins is needed.

Since some plugins also lazy-load encapsulated plugins the call mechanism is
required to be able to defer these calls until the plugins are loaded.

For example when setting I/O bindings with `elektraIoSetBinding()` the exported
function `setIoBinding` is called for all globally mounted plugins.
Since global mounting is implemented using the "list" plugin which uses
lazy-loading for its plugins the exported functions from the plugins are
unavailable.

Other examples are the "dini" and "multifile" plugins which use multiple plugins
to support different file formats.
These plugins also "hide" functions exported by encapsulated plugins.

## Constraints

1. Plugins encapsulating other plugins shall be able to lazy-load them.
2. Callers shall not be entangled in encapsulating plugin details.
3. The encapsulation of the plugins shall not be broken.
4. The mechanism shall be generic.

## Assumptions

1. The called functions do not return a value (e.g. `set`, `open`, `close`, ...).
   Callbacks can be used as return channel (see "Implications")

## Considered Alternatives

- Encapsulating plugins export a function called `getEncapsulatedPlugins`.
  This would break encapsulation and break lazy-loading since to return the
  encapsulated plugin handles they have to be loaded.
- Compatible plugins export a function called `getInterfaces`.
  It returns a KeySet with well-known interface names like "notification" or
  "ioBinding" and the required functions (e.g. `open` & `close` for
  "notification" or `set` for "ioBinding").
  While normal plugins simply return their interfaces, encapsulating plugins
  collect interfaces from its plugins and combine them into a single KeySet.

  The example below shows a KeySet from a plugin encapsulating two plugins "A"
  and "B".
  Both plugins implement the "notification" interface, plugin "B" also
  implements the "ioBinding" interface.

  - `/notification/#0/open`: address of `openNotification` of plugin "A"
  - `/notification/#0/close`: address of `closeNotification` of plugin "A"
  - `/notification/#1/open`: address of `openNotification` of plugin "B"
  - `/notification/#1/close`: address of `closeNotification` of plugin "B"
  - `/ioBinding/#0/set`: address of `setIoBinding` of plugin "B"

  The upside of this approach is that it makes encapsulating plugins transparent
  to the caller - it does not know whether the plugin encapsulates other
  plugins.
  This approach breaks lazy-loading since for combining all interfaces the
  plugins have to be loaded and their `getInterfaces` functions have to be
  called.

## Decision

Encapsulating plugins export a function called `deferredCall` with the
declaration
`void elektraDeferredCall (Plugin * plugin, char * name, KeySet * parameters)`.
Encapsulating plugins shall save multiple deferred calls and call the exported
functions specified by `name` passing the `parameters` KeySet when a plugin is
initialized in the same order as received.

Plugins that support deferred calls shall have the following declaration for
their functions
`void somePluginFunction (Plugin * plugin, KeySet * parameters)`.
The calling developer is responsible for ensuring that the called functions have
a compatible declaration.
Encapsulated plugins that do not export a specified function name are omitted.

## Argument

The solution allows changing encapsulating plugin implementations without
breaking callers.

## Implications

The called function receive their parameters via a KeySet.

While called functions could return data using the `parameters` KeySet (or a
separate KeySet) there is no defined moment when the data can be collected.
Defining such a moment would break the lazy-loading constraint.
It is recommended to use callbacks passed as `parameters`.
Callback function declarations are not limited by this decision.

## Related Decisions

- Elektra's invoke functionality will be extended to also allow us to use
  deferred calls with new functions:

- `int elektraInvokeFunctionDeferred (ElektraInvokeHandle * handle, const char * elektraPluginFunctionName, KeySet * ks)`
  which defers a call if the plugin exports `deferredCall`.
- `void elektraInvokeExecuteDeferredCalls (ElektraInvokeHandle * handle, ElektraDeferredCallList * list)`
  which executes deferred calls for an encapsulated plugin loaded with invoke.

- Functions supporting deferred calls should allow for multiple calls (i.e.
  they should be idempotent).
  This leaves state at affected plugins and does avoid duplicating state (e.g.
  "was this function called for this plugin before?") in encapsulating
  plugins.

## Notes

Utility functions that help with managing deferred calls would be nice:

- `ElektraDeferredCallList * elektraDeferredCallCreateList (void)`
- `void elektraDeferredCallDeleteList (ElektraDeferredCallList * list)`
- `int elektraDeferredCallAdd (ElektraDeferredCallList * list, char * name, KeySet * parameters)`
- `void elektraDeferredCallsExecute (Plugin * plugin, ElektraDeferredCallList * list)`
