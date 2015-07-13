/**
 * @mainpage The Elektra API
 *
 * @section overview Elektra Initiative Overview
 *
 * Elektra provides a universal and secure framework to store configuration
 * parameters in a global, hierarchical key database.  The core is a small
 * library implemented in C. The plugin-based framework fulfills many
 * configuration-related tasks to avoid any unnecessary code duplication
 * across applications while it still allows the core to stay without any
 * external dependency. Elektra abstracts from cross-platform-related issues
 * with an consistent API, and allows applications to be aware of other
 * applications' configurations, leveraging easy application integration.
 *
 * See the website for more information http://www.libelektra.org
 *
 *
 *
 * @section focus API docu
 *
 * This document occupies with the API implementation, documentation,
 * internals and plugins.
 * On the one hand it gives an overview and an introduction for
 * developers using Elektra, on the
 * other hand it gives an informal descriptions what methods must and may provide
 * to allow an alternative implementation of the API.
 *
 * The current version (for stable releases) of this document can be found at
 * http://doc.libelektra.org/api/current/html
 *
 * The latest version (from git master) of this document can be found at
 * http://doc.libelektra.org/api/latest/html
 *
 *
 *
 *
 * @section using Using the Elektra Library
 *
 * A C or C++ source file that wants to use Elektra should include:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * To link an executable with the Elektra library, one way is to
 * use the @c pkg-config tool:
 * @code
 * $ gcc -o application `pkg-config --cflags --libs elektra` application.c
 * @endcode
 *
 * Another way is to use CMake:
 * @code
 * find_package(Elektra REQUIRED)
 * include_directories (${ELEKTRA_INCLUDE_DIR})
 * target_link_libraries (application ${ELEKTRA_LIBRARIES})
 * @endcode
 *
 *
 *
 *
 * @section classes Elektra API
 *
 * The API was written in pure C because Elektra was designed to be useful
 * even for the most basic system programs.
 *
 * The API follows an object-oriented design, and there are 3 main classes
 * as shown by the figure:
 *
 * @image html classes.png "Elektra Classes"
 * @image latex classes.png "Elektra Classes"
 *
 * Some general things you can do with each class are:
 *
 * @link kdb KDB (Key Database) @endlink
 *   - @link kdbOpen() Open @endlink and @link kdbClose() Close @endlink the Key Database
 *   - @link kdbGet() Get @endlink and @link kdbSet() Set @endlink
 *     @link keyset KeySet @endlink in the Key Database
 *   - See @ref kdb "class documentation" for more
 *
 * @link key Key @endlink
 *   - @link keyNew Create @endlink and @link keyDel Delete @endlink
 *   - Get and Set key the @link keySetName() name @endlink
 *   - Get and Set @link keySetString() string @endlink or @link keySetBinary() binary @endlink values
 *   - Get and Set @link keymeta Meta Data @endlink
 *   - See @ref key "class documentation" for more
 *
 * @link keyset KeySet @endlink
 *   - @link ksNew Create @endlink and @link ksDel Delete @endlink
 *   - Append @link ksAppendKey() a single key @endlink or an
 *     entire @link ksAppend() KeySet @endlink
 *   - @link ksNext() Work with @endlink its @link ksCurrent() internal
 *     cursor @endlink
 *   - See @ref keyset "class documentation" for more
 *
 *
 *
 *
 *
 * @section namespace Namespaces
 *
 * There are 5 trees (=namespaces) of keys: @c spec, @c proc, @c dir, @c user and @c system
 * that are all unified (in the given order) in one cascading tree starting with @c /.
 *
 * The cascading tree is the logical tree to be used in applications.
 * The other trees are the physical ones that stem from configuration sources.
 * When using cascading key the best key will be searched at runtime,
 * which appears like a tree on its own.
 * See @ref cascading in the documentation of ksLookupByName() how the selection
 * of keys works.
 *
 * - The @c spec tree\n
 *   This tree specifies how the lookup should take place and also allows us to
 *   define defaults or document a key.
 *   The metadata of a key contains this information:
 *   - `override/#`: use these keys *in favour* of the key itself (note that
 *       `#` is the syntax for arrays, e.g. `#0` for the first element,
 *       `#_10` for the 11th and so on)
 *   - `namespace/#`: instead of using all namespaces in the predefined order,
 *       one can specify which namespaces should be searched in which order
 *   - `fallback/#`: when no key was found in any of the (specified) namespaces
 *       the `fallback`-keys will be searched
 *   - `default`: this value will be used if nothing else was found
 *
 * - The @c proc tree\n
 *   Is the only read-only tree. The configuration does not stem from the
 *   @link kdb KDB (Key Database) @endlink, but any other source, e.g. command-line arguments or environment.
 *
 * - The @c dir tree\n
 *   Allows us to have a per-directory overwrite of configuration files, e.g.
 *   for project specific settings.
 *
 * - The @c user" tree \n
 *   Used to store user-specific configurations, like the personal settings
 *   of a user to certain programs. The user subtree will always be favoured
 *   if present (except for security concerns the user subtree may not be considered).
 *
 * - The @c system tree\n
 *   It is provided to store system-wide configuration keys, that is,
 *   the last fallback for applications but the only resort for
 *   daemons and system services.
 *
 *
 *
 * @section rules Rules for Key Names
 *
 * When using Elektra to store your application's configuration and state,
 * please keep in mind the following rules:
 * - You are not allowed to create keys right under the root.
 *   They are reserved for more generic purposes.
 * - The keys for your application, called say @e myapp, should be created under
 *   @p /sw/org/myapp/#0/current
 *   - sw is for software
 *   - org is the organisation. For uniqueness a full reverse url encoded with '/' instead of '.' is useful.
 *   - @p #0 is the major version of the configuration
 *   - current is the default configuration profile.
 *   - That means you just need to kdbGet() @p /sw/org/myapp/#0/profile
 *     and then ksLookupByName() in @p /sw/org/myapp/#0/profile/key where
 *     profile is from command-line arguments and defaults to current.
 *
 *
 *
 * @section backendsoverview Backend Overview
 *
 * The core of Elektra does not store configuration itself to the
 * harddisk. Instead this work is delegated to backends.
 *
 * If you want to develop a backend, you should already have some experience
 * with Elektra from the user point of view. You should be familiar with
 * the data structures: @link key Key @endlink and @link keyset KeySet @endlink
 * Then you can start reading about Backends that are composed out of
 * @ref plugin.
 * To get started with writing plugins, first read our plugin tutorial in doc/tutorials!
 *
 *
 *
 *
 * @section glossary Glossary
 * - @b pop, used in @ref ksPop() and @ref KDB_O_POP means to remove
 *   a key from a keyset.
 * - @b delete, or abbr. del, used in @ref keyDel(), @ref ksDel() and @ref KDB_O_DEL means to free a key or keyset. The memory
 *   can be used for something else afterwards.
 * - @b remove  means that the key/value information in the physical database will be removed permanently.
 *
 */

