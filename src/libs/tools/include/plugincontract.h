{ "default", 64000 }, //< to be used as default, will be added by the build system for KDB_DEFAULT_STORAGE and KDB_DEFAULT_RESOLVER
	{ "recommended", 32000 },  //< in case of doubt, use this plugin
	{ "productive", 8000 },	   //< actively used in productive environments, not only by maintainer
	{ "maintained", 4000 },	   //< actively used and improved by maintainer (infos/author)
	{ "compatible", 2000 },	   //< will be compatible with its later versions
	{ "tested/unit", 1000 },   //< substantial plugin unittests (testmod)
	{ "tested/shell", 1000 },  //< substantial shell-recorder tests (README.md)
	{ "tested/fuzz", 1000 },   //< tested using a fuzzer tool like AFL
	{ "documented", 1000 },	   //< all features are described in documentation (README.md)
	{ "configurable", 0 },	   //< plugin configuration available to modify behavior
	{ "hook", 0 },		   //< suitable as hook plugin
	{ "internal", -250 },	   //< uses internal header files from Elektra
	{ "memleak", -250 },	   //< memleak in plugin or one of the libraries the plugin uses
	{ "experimental", -4000 }, //< plugin is in early stage, disabled in CMake by default
	{ "difficult", -8000 },	   //< the plugin is (unnecessarily) difficult to use
	{ "concept", -16000 },	   //< only a concept of how such a plugin could be done, needs proper rewrite
	{ "obsolete", -32000 },	   //< another plugin fulfils a similar functionality in a better way
	{ "discouraged", -64000 }, //< only use the plugin if you really know what you are doing
