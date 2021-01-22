# Compilation Variants

To create different variants of the same feature, but avoid code
duplications within plugins, you have multiple options:

- Define a needs clause in a [contract](/doc/CONTRACT.ini) and reuse another
  plugin as it is. This should be preferred for filter and validation
  tasks.
- Have common code together in a helper library (or core library),
  see the CMake function `add_lib` for creating such a library
  (in the folder libs).
  This should be used for rather common functionality that might
  be useful for many plugins or even applications.
- Have configuration for plugins (See [elektraPluginGetConfig()](https://doc.libelektra.org/api/master/html/group__plugin.html)
  and dynamically switch with `if` according to the configuration.
  This should be preferred when you want to (de)activate some
  features of a plugin at run-time.
- Or use compilation variants to compile the plugin code multiple
  times with different `COMPILE_DEFINITIONS` (that are Macro definitions).
  This should be preferred when different macro definitions
  lead to different plugins.
  It should especially be used when the resulting plugins have different
  dependencies: it is possible to have different `LINK_LIBRARIES`.

The advantage of compilation variants are:

- No run-time overhead
- Can be used during bootstrapping (when no configuration is available)
- Different compilation variants can be built at once (no recompilation
  with different CMake flags required)
- Different compilation variants can have different dependencies
- Different compilation variants can be mounted without `#refnames`

## How to use It

To use compilation variants, add your plugin in the CMake Cache
Variable `PLUGINS` multiple times.
Then there can be an arbitrary number of variants.
As naming convention you should have a base name with an additional
variant appended with underscore, e.g.:

```cmake
myplugin_varianta;myplugin_variantb
```

In the CMakeLists.txt of your plugin, you have two options.
Option (A): When you can easily enlist every variant you
simply list all plugins one after the other (_outside of_ `if (DEPENDENCY_PHASE)`):

```cmake
add_plugin(myplugin_varianta
	SOURCES      <your sources for varianta here..>
	COMPILE_DEFINITIONS   VARIANTA ELEKTRA_VARIANT=varianta
	LINK_LIBRARIES <libraries for varianta>
	)
add_plugin(myplugin_variantb
	SOURCES      <your sources for variantb here..>
	COMPILE_DEFINITIONS   VARIANTB  ELEKTRA_VARIANT=variantb
	LINK_LIBRARIES <libraries for variantb>
	)
```

Option (B): If you cannot enlist every possible compilation variant,
you can iterate over all PLUGINS and check which names are requested.
Then you create a plugin for every name that matches:

```cmake
foreach (plugin ${PLUGINS})
	if (${plugin} MATCHES "myplugin_.*")
		# somehow process the variant names and include
		# or change sources and compile definitions
		# based on that.
		add_plugin(${plugin}
		SOURCES      <your sources here..>
		COMPILE_DEFINITIONS   <definitions here..>
			ELEKTRA_VARIANT=${plugin without prefix}
	LINK_LIBRARIES <libraries for variantb>
	if (${plugin} MATCHES "ALL")
		# handle categories of plugins
		add_plugin(myplugin_all1, ...)
		add_plugin(myplugin_all2, ...)
```

For the categories such as `ALL`, however, you need to automatically
append (using `add_plugin`) a useful set of plugins.

Note that every plugin needs to have
`ELEKTRA_VARIANT` differently set in `COMPILE_DEFINITIONS`, otherwise you will
get a linker error that `libelektra_<pluginname>_LTX_elektraPluginSymbol` has
multiple definitions.

Now every public function of the plugin conflicts with itself. To avoid
that, you can use:

- static functions, but they are only visible within one file.
  This should be preferred, when possible.
- use helper libraries using `add_lib` to share code
  between compilation variants
  (only if code is also potentially useful for other plugins/applications)
- Get a unique name for every variant using the macro
  `ELEKTRA_PLUGIN_FUNCTION(myplugin, open)` where myplugin is
  the name of the plugin and the second argument is how the function
  should be called.
- Including a readme for every variant (with `#ifdef` for different text)
  using the macro `#include ELEKTRA_README(myplugin)`

As a summary, you can have many plugins build out of the same source.
Using `pluginname_variantnames` many plugins will be compiled, each
with other `SOURCES` or `COMPILE_DEFINITIONS` and even `LINK_LIBRARIES`:
If you, e.g. just set
the variants name as macro you can use

```c
#ifdef varianta
#endif
```

within the code and can have two plugins: one (called `myplugin_varianta`)
compiled included the `#ifdef` the other (base variant called
`myplugin`) without.

Currently compilation variants are used in
[the resolver plugin](https://master.libelektra.org/src/plugins/resolver/resolver.c).
