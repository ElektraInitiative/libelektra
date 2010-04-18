/**
 * @mainpage The Elektra API
 *
 * @section overview Elektra Initiative Overview
 *
 * Elektra is a universal hierarchical configuration store, with related goals like
 * GConf and the Windows Registry. It allows programs to read and save their configurations
 * with a consistent API, and allows them to be aware of other applications' configurations,
 * leveraging easy application integration. The whole point of it is to tie applications
 * together, so that they can co-operate and share their user-preferences.
 *
 * The developers are associated to unix philosophy and the very practical point consists of
 * writing a configuration library. Every software needs this functionality, it is not easy
 * to do it right and performant and we want to avoid any unnecessary code duplication.
 *
 * See the website for more information http://www.libelektra.org
 *
 * Please report all bugs related to interface, documentation or
 * implementation http://bugs.libelektra.org
 *
 *
 *
 *
 *
 * @section focus Major focal points
 *
 * 1. API implementation to access the key/value pairs namespace 
 * 2. Implement the API with a variety of Backends and Bindings 
 * 3. Definition of a standard key/value pair hierarchy, namespace and semantics 
 *
 * This document occupies with the API implementation, documentation, internals and backends.
 * On the one hand it gives an overview and an introduction for developers using elektra, on the
 * other hand it gives an informal descriptions what methods must and may provide
 * to allow an alternative implementation using this description.
 *
 *
 *
 *
 *
 * @section using Using the Elektra Library
 *
 * See http://www.libelektra.org/Tutorial for first Introduction.
 *
 * A C or C++ source file that wants to use Elektra should include:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * There is also a library that provides some
 * @ref stream "optional XML manipulation methods called KDB Tools", and to use
 * it you should include:
 * @code
 * #include <kdbtools.h>
 * @endcode
 *
 * To link an executable with the Elektra library, the correct way is to
 * use the @c pkg-config tool:
 * @code
 * bash$ cc `pkg-config --libs elektra` -o myapp myapp.c
 * @endcode
 *
 * Or if you need the options XML manipulation methods, you should use:
 * @code
 * bash$ cc `pkg-config --libs elektratools` -o myapp myapp.c
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
 * See http://www.libelektra.org/Bindings for Bindings.
 *
 * The API follows an Object Oriented design, and there are 3 main classes
 * as shown by the figure:
 *
 * @image html classes.png "Elektra Classes"
 *
 * Some general things you can do with each class are:
 *
 * @link kdb KDB @endlink
 *   - @link kdb The four lowlevel functions @endlink
 *   - @link kdbOpen() Open @endlink and @link kdbClose() Close @endlink the Database
 *   - @link kdbGet() Get @endlink and @link kdbSet() Set @endlink
*      @link keyset KeySet @endlink in the Database
 *   - Retrieve and commit individual @link kdbGetString() Key value @endlink
 *   - Create and delete regular, folder or symbolic link Keys
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
 *   - @link keyToStream() Export Keys to an XML representation @endlink
 *   - keyGenerate() and keyOutput() for per line and c-code output.
 *   - See @ref key "class documentation" for more
 *
 * @link keyset KeySet @endlink
 *   - Linked list of Key objects
 *   - Append @link ksAppendKey() a single key @endlink or an
 *     entire @link ksAppend() KeySet @endlink
 *   - @link ksNext() Work with @endlink its @link ksCurrent() internal
 *     cursor @endlink
 *   - @link ksFromXMLfile() Import @endlink and
 *     ksToStream() Export KeySets to an XML representation
 *   - ksGenerate() and ksOutput() for per line and c-code output.
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
 * Elektra itself cant store configuration to harddisk, this work is delegated
 * to the backends.
 *
 * - ... of users perspective
 *   If you are a @c user of elektra, you will need following information:
 *   - @ref backendhandle to access the information of KDB.
 *
 * - ... of developers perspective
 *   If you want to develop a backend, you should already have some experience
 *   with elektra from the user point of view. You should be familiar with
 *   the data structures: @link key Key @endlink, @link keyset KeySet @endlink,
 *   @link backendhandle KDB @endlink.
 *   Then you can start reading about @link backend Backends @endlink:
 *   - They provide storage needed by kdb functions
 *   - Dynamical kdbMount() and kdbUnmount() of backends in the global namespace
 *   - Need to implement
 *     kdbOpen_backend(), kdbClose_backend(), kdbGet_backend(), kdbSet_backend()
 *   - Use KDBEXPORT() to export your functions.
 *   - See @ref backend "class documentation" for more
 *
 *
 *
 *
 *
 * @section nomenclature Nomenclature
 * - @b pop, used in @ref ksPop() and @ref KDB_O_POP means to remove
 *   a key from a keyset.
 * - @b delete, or abbr. del, used in @ref keyDel(), @ref ksDel() and @ref KDB_O_DEL means to free a key or keyset. The memory
 *   can be used for something else afterwards.
 * - @b remove, used in @ref kdbRemove(), kdbRemoveKey(), @ref KDB_O_NOREMOVE and @ref KDB_O_REMOVEONLY
 *   means that the key/value information in the physical database will be removed permanently.
 *   The key operations keyNeedRemove() with its flag set in keyNew() by using @ref KEY_REMOVE or keyRemove() means the same,
 *   but will not have effects before kdbSet() was called with that keys.
 *
 */

