# Compilation Variants

To create different variants of the same feature, but avoid code
duplications within plugins, you have multiple options:
- Define a needs clause in a [contract](contract.md) and reuse another
  plugin as it is.
- Have common code together in a helper library (or core library),
  see the CMake function add_plugin_helper for creating such a library.
- Have configuration for plugins (See [elektraPluginGetConfig()]
  (http://doc.libelektra.org/api/latest/html/group__plugin.html)
  and dynamically switch with if according to the configuration.
- Or use compilation variants to compile the plugin code multiple
  times with different COMPILE_DEFINITIONS (that are Macro definitions).

The compilation variants are the hardest to use, so you should
reconsider if another technique is more appropriate.

The advantage of compilation variants are:
- No runtime overhead
- Can be used during Bootstrapping (when no configuration is available)

To use compilation variants, add your plugin in the CMake Cache
Variable PLUGINS multiple times. There has to be the base variant,
called in the same name as the directory.
Then there can be an arbitrary number of variants that additional
have a name appended with underscore, e.g.:

	myplugin;myplugin_varianta;myplugin_variantb

In the CMakeLists.txt of your plugin, you need a loop over all PLUGINS
and create a plugin per compilation variant:

	foreach (plugin ${PLUGINS})
		if(${plugin} MATCHES "myplugin_.*")
			# somehow process the variant names and include
			# or change sources and compile definitions
			# based on that.
			add_plugin(${plugin}
			SOURCES      <your sources here..>
			COMPILE_DEFINITIONS   <definitions here..>

Now every public function of the plugin conflicts with itself. To avoid
that, you can use:
- static functions, but they are only visible within one file
- use helper libraries using add_plugin_helper to share code
  between compilation variants
- Get a unique name for every variant using the macro
  ```ELEKTRA_PLUGIN_FUNCTION(myplugin, open)``` where myplugin is
  the name of the plugin and the second argument is how the function
  should be called.
- Including a readme for every variant (with #ifdef for different text)
  using the macro ```#include ELEKTRA_README(myplugin)```


As a summary, you can have many plugins build out of the same source.
Using pluginname_variantnames many plugins will be compiled, each
with other SOURCES or COMPILE_DEFINITIONS. If you, e.g. just set
the variants name as macro you can use

	#ifdef varianta
	#endif

within the code and can have two plugins: one (called myplugin_varianta)
compiled included the ```#ifdef``` the other (base variant called
myplugin) without.

Currently compilation variants is used in [the resolver
plugin](/src/plugins/resolver/resolver.c).
