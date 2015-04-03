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
 * To link an executable with the Elektra library, the correct way is to
 * use the @c pkg-config tool:
 * @code
 * bash$ cc `pkg-config --libs elektra` -o myapp myapp.c
 * @endcode
 *
 *
 *
 *
 *
 * @section classes Elektra API
 *
 * The API was written in pure C because Elektra was designed to be useful
 * even for the most basic system programs, which are all made in C. Also,
 * being C, bindings to other languages can appear easily.
 *
 * The API follows an Object Oriented design, and there are 3 main classes
 * as shown by the figure:
 *
 * @image html classes.png "Elektra Classes"
 * @image latex classes.png "Elektra Classes"
 *
 * Some general things you can do with each class are:
 *
 * @link kdb KDB @endlink
 *   - @link kdb The four lowlevel functions @endlink
 *   - @link kdbOpen() Open @endlink and @link kdbClose() Close @endlink the Database
 *   - @link kdbGet() Get @endlink and @link kdbSet() Set @endlink
*      @link keyset KeySet @endlink in the Database
 *   - See @ref kdb "class documentation" for more
 *
 * @link key Key @endlink
 *   - Get and Set key properties like @link keySetName() name @endlink,
 *     @link keySetString() string @endlink or @link keySetBinary() binary @endlink values,
 *     @link keyGetMode() permissions @endlink,
 *     @link keyGetMTime() changed time @endlink and
 *     @link keyGetComment() comment @endlink
 *   - Test if it is a
 *     @link keyIsUser() @p user/ @endlink or @link keyIsSystem() @p system/
 *     @endlink key, etc
 *   - See @ref key "class documentation" for more
 *
 * @link keyset KeySet @endlink
 *   - Linked list of Key objects
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
 * @section keynames Key Names and Namespaces
 *
 * There are 2 trees of keys: @c system and @c user
 *
 * - The "system" Subtree
 *   It is provided to store system-wide configuration keys, that is,
 *   configurations that daemons and system services will use.
 *   But all other programs will also try to fetch system keys to have
 *   a fallback managed by the distributor or admin when the user does
 *   not have configuration for its own.
 *
 * - The "user" Subtree
 *   Used to store user-specific configurations, like the personal settings
 *   of a user to certain programs. The user subtree will always be favoured
 *   if present (except for security concerns the user subtree may not be considered).
 *   See @ref cascading in the documentation of ksLookupByName() how the selection
 *   of user and system keys works.
 *
 *
 *
 * @section rules Rules for Key Names
 *
 * When using Elektra to store your application's configuration and state,
 * please keep in mind the following rules:
 * - You are not allowed to create keys right under @p system or @p user.
 *   They are reserved for more generic purposes.
 * - The keys for your application, called say @e MyApp, should be created under
 *   @p system/sw/MyApp/current and @p user/sw/MyApp/current
 * - current is the default configuration profile, users may symlink
 *   to the profile they want.
 * - That means you just need to kdbGet() @p system/sw/MyApp/profile and @p user/sw/MyApp/profile
 *   and then ksLookupByName() in @p /sw/MyApp/profile while profile defaults to current,
 *   but may be changed by the user or admin. See @ref cascading to learn more about that feature.
 *
 *
 *
 *
 * @section backendsoverview Backend Overview
 *
 * The core of elektra does not store configuration itself to the
 * harddisk. Instead this work is delegated to backends.
 *
 * If you want to develop a backend, you should already have some experience
 * with Elektra from the user point of view. You should be familiar with
 * the data structures: @link key Key @endlink and @link keyset KeySet @endlink
 * Then you can start reading about Backends, which are composed out of
 * @ref plugin.
 *
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

