# Compilation Variants

To create different variants of the same feature, but avoid code
duplications within plugins, you have multiple options:
- Define a needs clause in a [contract](contract.md) and reuse another
  plugin as it is. This should be preferred for filter and validation
  tasks.
- Have common code together in a helper library (or core library),
  see the CMake function add_lib for creating such a library
  (in the folder libs).
  This should be used for rather common functionality that might
  be useful for many plugins or even applications.
- Have configuration for plugins (See [elektraPluginGetConfig()]
  (http://doc.libelektra.org/api/latest/html/group__plugin.html)
  and dynamically switch with if according to the configuration.
  This should be preferred when you want to (de)activate some
  features of a plugin at runtime.
- Or use compilation variants to compile the plugin code multiple
  times with different `COMPILE_DEFINITIONS` (that are Macro definitions).
  This should be preferred when different macro definitions
  lead to different plugins.
  It should especially be used when the resulting plugins have different
  dependencies: it is possible to have different `LINK_LIBRARIES`.

The advantage of compilation variants are:
- No runtime overhead
- Can be used during Bootstrapping (when no configuration is available)
- Different compilation variants can be built at once (no recompilation
  with different cmake flags required)
- Different compilation variants can have different dependencies

To use compilation variants, add your plugin in the CMake Cache
Variable PLUGINS multiple times. There has to be the base variant,
called in the same name as the directory.
Then there can be an arbitrary number of variants that additional
have a name appended with underscore, e.g.:

	myplugin;myplugin_varianta;myplugin_variantb

In the CMakeLists.txt of your plugin, you need a loop over all PLUGINS
and create a plugin per compilation variant:

	foreach (plugin ${PLUGINS})
		if (${plugin} MATCHES "myplugin_.*")
			# somehow process the variant names and include
			# or change sources and compile definitions
			# based on that.
			add_plugin(${plugin}
			SOURCES      <your sources here..>
			COMPILE_DEFINITIONS   <definitions here..>
		LINK_LIBRARIES <libraries for variantb>

or simply list all plugins one after the other:

	if (${plugin} MATCHES "myplugin_varianta")
		# somehow process the variant names and include
		# or change sources and compile definitions
		# based on that.
		add_plugin(myplugin_varianta
		SOURCES      <your sources for varianta here..>
		COMPILE_DEFINITIONS   VARIANTA
		LINK_LIBRARIES <libraries for varianta>
		)
	if (${plugin} MATCHES "myplugin_variantb")
		# somehow process the variant names and include
		# or change sources and compile definitions
		# based on that.
		add_plugin(myplugin_variantb
		SOURCES      <your sources for variantb here..>
		COMPILE_DEFINITIONS   VARIANTB
		LINK_LIBRARIES <libraries for variantb>

Now every public function of the plugin conflicts with itself. To avoid
that, you can use:
- static functions, but they are only visible within one file.
  This should be preferred, when possible.
- use helper libraries using add_lib to share code
  between compilation variants
  (only if code is also potentially useful for other plugins/applications)
- Get a unique name for every variant using the macro
  `ELEKTRA_PLUGIN_FUNCTION(myplugin, open)` where myplugin is
  the name of the plugin and the second argument is how the function
  should be called.
- Including a readme for every variant (with #ifdef for different text)
  using the macro `#include ELEKTRA_README(myplugin)`


As a summary, you can have many plugins build out of the same source.
Using pluginname_variantnames many plugins will be compiled, each
with other SOURCES or `COMPILE_DEFINITIONS` and even `LINK_LIBRARIES`:
If you, e.g. just set
the variants name as macro you can use

	#ifdef varianta
	#endif

within the code and can have two plugins: one (called myplugin_varianta)
compiled included the `#ifdef` the other (base variant called
myplugin) without.

Currently compilation variants is used in
[the resolver plugin](http://libelektra.org/tree/master/src/plugins/resolver/resolver.c).
