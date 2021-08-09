package org.libelektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import javax.annotation.Nullable;

/**
 * The JNA dynamic proxy interface to libelektra. This proxy is not intended to
 * be used directly by libelektra API consumers. Please see {@link KDB} and its
 * accompanying classes.
 */
interface Elektra extends Library {

	/**
	 * Singleton instance of the native library proxy.
	 */
	Elektra INSTANCE = Native.load ("elektra-kdb", Elektra.class);

	// KDB methods --------------------------------------------------------------

	/**
	 * Opens the session with the Key database.<br >
	 * <br >
	 * You must always call this method before retrieving or committing any keys to
	 * the database. In the end of the program, after using the key database, you
	 * must not forget to {@link #kdbClose(Pointer, Pointer) kdbClose()}.<br >
	 * <br >
	 * Get a {@code KDB handle} for every thread using Elektra. Don't share the
	 * handle across threads, and also not the pointer accessing it.<br >
	 * <br >
	 * You don't need {@link #kdbOpen(Pointer, Pointer) kdbOpen()} if you only want
	 * to manipulate plain in-memory {@code Key} or {@code KeySet} objects.
	 *
	 * @see #kdbGet(Pointer, Pointer, Pointer) kdbGet()
	 * @see #kdbClose(Pointer, Pointer) kdbClose()
	 * @see #keyNew(String, Object...) keyNew()
	 *
	 * @param contractKeySet TODO #3754 - documentation unclear - the contract that
	 *                       should be ensured before opening the KDB all data is
	 *                       copied and the KeySet can safely be used for e.g.
	 *                       kdbGet() later
	 * @param errorKey       {@link Pointer} to a valid {@code Key}, where issued
	 *                       errors and warnings will be made available.
	 * @return
	 *         <ul>
	 *         <li>{@link Pointer} to {@code KDB handle} on success</li>
	 *         <li>{@code null} on failure</li>
	 *         </ul>
	 */
	@Nullable Pointer kdbOpen (@Nullable Pointer contractKeySet, Pointer errorKey);

	/**
	 * Closes the session with the Key database.<br >
	 * <br >
	 * You must call this method when you finished your affairs with the key
	 * database. You can still manipulate {@code Key} and {@code KeySet} objects
	 * after calling {@code #kdbClose(Pointer, Pointer)}, but you must not use any
	 * {@code kdb*()} call afterwards.<br >
	 *
	 * @see #kdbOpen(Pointer, Pointer) kdbOpen()
	 * @see #keyNew(String, Object...) keyNew()
	 *
	 * @param handle   {@link Pointer} to a valid {@code KDB handle} as returned by
	 *                 {@link #kdbOpen(Pointer, Pointer) kdbOpen()}.
	 * @param errorKey {@link Pointer} to a valid {@code Key}, where issued errors
	 *                 and warnings will be made available.
	 * @return
	 *         <ul>
	 *         <li>{@code 0} on success</li>
	 *         <li>{@code -1} on {@code null} pointer passed</li>
	 *         </ul>
	 */
	int kdbClose (Pointer handle, Pointer errorKey);

	/**
	 * Retrieve keys in an atomic and universal way.<br >
	 * <br >
	 * The {@code returnKeySet} may already contain some keys, e.g. from previous
	 * {@link #kdbGet(Pointer, Pointer, Pointer) kdbGet()} calls. The new retrieved
	 * keys will be appended using the native function underlying
	 * {@link #ksAppendKey(Pointer, Pointer) ksAppendKey()}.
	 *
	 * @apiNote {@link #kdbGet(Pointer, Pointer, Pointer) kdbGet()} might retrieve
	 *          more keys than requested (that are not below parentKey). These keys
	 *          must be passed to when saving modifications via
	 *          {@link #kdbSet(Pointer, Pointer, Pointer) kdbSet()}, otherwise they
	 *          will be lost. This stems from the fact that the user has the only
	 *          copy of the whole configuration and backends only write
	 *          configuration that was passed to them. For example, if you get
	 *          {@code system:/mountpoint/interest} you will not only get all keys
	 *          below {@code system:/mountpoint/interest}, but also all keys below
	 *          {@code system:/mountpoint} (if {@code system:/mountpoint} is a
	 *          mountpoint as the name suggests, but
	 *          {@code system:/mountpoint/interest} is not a mountpoint). Make sure
	 *          to not touch or remove keys outside the keys of interest, because
	 *          others may need them!
	 *
	 * @implNote Optimization: In the first run of
	 *           {@link #kdbGet(Pointer, Pointer, Pointer) kdbGet()} all requested
	 *           (or more) keys are retrieved. On subsequent calls only the keys are
	 *           retrieved where something was changed inside the key database. The
	 *           other keys stay in the {@code KeySet} returned as passed.
	 *
	 * @see #kdbOpen(Pointer, Pointer) kdbOpen() which needs to be called before
	 * @see #ksLookup(Pointer, Pointer, int) ksLookup() and
	 * @see #ksLookupByName(Pointer, String, int) ksLookupByName() for powerful
	 *      lookups after the {@code KeySet} was retrieved
	 * @see #kdbSet(Pointer, Pointer, Pointer) kdbSet() to save the configuration
	 *      afterwards
	 * @see #kdbClose(Pointer, Pointer) kdbClose() to finish affairs with the key
	 *      database
	 *
	 * @param handle       {@link Pointer} to a valid {@code KDB handle} as returned
	 *                     by {@link #kdbOpen(Pointer, Pointer) kdbOpen()}.
	 * @param returnKeySet {@link Pointer} to a valid {@code KeySet} to be populated
	 *                     with all keys found. It will not be changed on error or
	 *                     if no update is required.
	 * @param parentKey    {@link Pointer} to a valid {@code Key}. It is used to add
	 *                     warnings and set an error information. Additionally, its
	 *                     name is a hint which keys should be retrieved (it is
	 *                     possible that more are retrieved, see API Note).
	 *                     <ul>
	 *                     <li>cascading keys (starting with "/") will retrieve the
	 *                     same path in all namespaces</li>
	 *                     <li>"/" will retrieve all keys.</li>
	 *                     </ul>
	 * @return
	 *         <ul>
	 *         <li>{@code 1} if the keys were retrieved successfully</li>
	 *         <li>{@code 0} if there was no update</li>
	 *         <li>{@code -1} on failure or {@code null} pointer passed</li>
	 *         </ul>
	 *         When a backend fails, {@link #kdbGet(Pointer, Pointer, Pointer)
	 *         kdbGet()} will return {@code -1} with all error and warning
	 *         information in the {@code parentKey} and {@code returnKeySet} left
	 *         unchanged.
	 */
	int kdbGet (Pointer handle, Pointer returnKeySet, Pointer parentKey);

	/**
	 * Set keys in an atomic and universal way.<br >
	 * <br >
	 * For the particularities of error handling, please see the documentation of
	 * the native library: TODO #3754 link to C API documentation
	 *
	 * @apiNote {@link #kdbGet(Pointer, Pointer, Pointer) kdbGet()} must be called
	 *          before {@link #kdbSet(Pointer, Pointer, Pointer) kdbSet()}:
	 *          initially (after {@link #kdbOpen(Pointer, Pointer) kdbOpen()}) and
	 *          also after conflict errors in
	 *          {@link #kdbSet(Pointer, Pointer, Pointer) kdbSet()}.
	 *
	 * @implNote Optimization: Each key is checked using the native function
	 *           underlying {@link #keyNeedSync(Pointer) keyNeedSync()} before being
	 *           actually committed. If no key of a backend needs to be synced any
	 *           affairs to backends are omitted and {@code 0} is returned.<br >
	 *           It is your responsibility to save the original keyset if you need
	 *           it afterwards.<br >
	 *           If you want to be sure to get a fresh keyset again, you need to
	 *           open a second handle to the key database using kdbOpen().
	 *
	 * @see #keyNeedSync(Pointer) keyNeedSync()
	 * @see #kdbOpen(Pointer, Pointer) kdbopen() and
	 * @see #kdbGet(Pointer, Pointer, Pointer) kdbGet() must be called first
	 * @see #kdbClose(Pointer, Pointer) kdbClose() must be called afterwards
	 *
	 * @param handle    {@link Pointer} to a valid {@code KDB handle} as returned by
	 *                  {@link #kdbOpen(Pointer, Pointer) kdbOpen()}.
	 * @param keySet    {@link Pointer} to a valid {@code KeySet} containing
	 *                  modified keys, otherwise no update is done.
	 * @param parentKey {@link Pointer} to a valid {@code Key}. It is used to add
	 *                  warnings and set an error information. Additionally, its
	 *                  name is an hint which keys should be committed (it is
	 *                  possible that more are changed).<br >
	 *                  With {@code parentKey} you can give an hint which part of
	 *                  the given {@code KeySet} is of interest to you. Then you
	 *                  promise to only modify or remove keys below this key. All
	 *                  others would be passed back as they were retrieved by
	 *                  {@link #kdbGet(Pointer, Pointer, Pointer) kdbGet()}.
	 *                  <ul>
	 *                  <li>cascading keys (starting with "/") will set the path in
	 *                  all namespaces</li>
	 *                  <li>"/" will commit all keys</li>
	 *                  <li>metanames will be rejected (error C01320)</li>
	 *                  <li>empty/invalid (error C01320)</li>
	 *                  </ul>
	 * @return
	 *         <ul>
	 *         <li>{@code 1} on success</li>
	 *         <li>{@code 0} if nothing had to be done, no changes in KDB</li>
	 *         <li>{@code -1} on failure or {@code null} pointer passed. No changes
	 *         in KDB, an error will be set on {@code parentKey} if possible</li>
	 *         </ul>
	 */
	int kdbSet (Pointer handle, Pointer keySet, Pointer parentKey);

	/**
	 * Sets up a contract for use with {@link #kdbOpen(Pointer, Pointer) kdbOpen()}
	 * that configures the {@code gopts} plugin.
	 *
	 * @param contractKeySet    {@link Pointer} to a valid {@code KeySet} into which
	 *                          the contract will be written.
	 * @param argsSize          Size of the {@code args} data.
	 * @param args              Continuous buffer containing all {@code argv}
	 *                          arguments separated (and terminated) by zero bytes.
	 *                          The whole buffer is copied, so the pointer only has
	 *                          to be valid for this function call.
	 * @param envSize           Size of the {@code env} data.
	 * @param env               Continuous buffer containing all environment
	 *                          variables separated (and terminated) by zero bytes
	 *                          The whole buffer is copied, so the pointer only has
	 *                          to be valid for this function call.
	 * @param parentKey         The parent key that should be used by {@code gopts}.
	 *                          Only the key name is copied. The key can be deleted
	 *                          immediately after calling this function.
	 * @param goptsConfigKeySet Configuration that is used to mount the
	 *                          {@code gopts} plugin. Only keys in the
	 *                          {@code user:/} namespace will be used.
	 * @return
	 *         <ul>
	 *         <li>{@code 0} on success</li>
	 *         <li>{@code -1} on {@code null} pointer passed</li>
	 *         </ul>
	 */
	int elektraGOptsContractFromStrings (Pointer contractKeySet, long argsSize, String args, long envSize, String env,
					     Pointer parentKey, @Nullable Pointer goptsConfigKeySet);

	// Key methods --------------------------------------------------------------

	static final String CASCADING_ROOT_KEY_NAME = "/";

	/**
	 * A practical way to fully create a Key object in one step.<br >
	 * <br >
	 * For examples and further particularities, please see the documentation of the
	 * native library: TODO #3754 link to C API documentation
	 *
	 * @see #keyDel(Pointer) keyDel()
	 *
	 * @param name A valid key name. Use {@link #CASCADING_ROOT_KEY_NAME} to get a
	 *             simple initialized, but really empty, object.
	 * @param args Argument flags, each followed by a corresponding value, if
	 *             appropriate.
	 * @return
	 *         <ul>
	 *         <li>A {@link Pointer} to a newly allocated and initialized
	 *         {@code Key} object on success</li>
	 *         <li>{@code null} on allocation error or if an invalid {@code name}
	 *         was passed (see {@link #keySetName(Pointer, String)
	 *         keySetName()}</li>
	 *         </ul>
	 */
	@Nullable Pointer keyNew (@Nullable String name, @Nullable Object... args);

	@Nullable Pointer keyDup (Pointer source, int flags);

	@Nullable Pointer keyCopy (Pointer dest, Pointer source, int flags);

	int keyDel (Pointer key);

	int keyIncRef (Pointer key);

	int keyDecRef (Pointer key);

	int keyGetRef (Pointer key);

	/* Meta Info */
	int keyRewindMeta (Pointer key);

	@Nullable Pointer keyNextMeta (Pointer key);

	@Nullable Pointer keyCurrentMeta (Pointer key);

	int keyCopyMeta (Pointer dest, Pointer source, String metaName);

	int keyCopyAllMeta (Pointer dest, Pointer source);

	@Nullable Pointer keyGetMeta (Pointer key, String metaName);

	int keySetMeta (Pointer key, String metaName, String newMetaString);

	/* Methods for Making Tests */
	int keyCmp (Pointer k1, Pointer k2);

	int keyNeedSync (Pointer key);

	int keyIsBelow (Pointer key, Pointer check);

	int keyIsBelowOrSame (Pointer key, Pointer check);

	int keyIsDirectlyBelow (Pointer key, Pointer check);

	int keyIsBinary (Pointer key);

	int keyIsString (Pointer key);

	/* Name Manipulation Methods */
	String keyName (Pointer key);

	int keyGetNameSize (Pointer key);

	int keySetName (Pointer key, String newname);

	Pointer keyUnescapedName (Pointer key);

	int keyGetUnescapedNameSize (Pointer key);

	String keyBaseName (Pointer key);

	int keyGetBaseNameSize (Pointer key);

	int keySetBaseName (Pointer key, String baseName);

	int keyAddBaseName (Pointer key, String baseName);

	/* Value Manipulation Methods */
	// byte[] keyValue(Pointer key);
	int keyGetValueSize (Pointer key);

	String keyString (Pointer key);

	int keySetString (Pointer key, String newString);

	int keyGetBinary (Pointer key, byte[] returnedBinary, int maxSize);

	int keySetBinary (Pointer key, @Nullable byte[] newBinary, int dataSize);

	// KeySet methods -----------------------------------------------------------

	/**
	 * Use to terminate var args array {@link #ksNew(int, Object...)}.
	 */
	static final Pointer KS_END = null;

	@Nullable Pointer ksNew (int alloc, Object... args);

	@Nullable Pointer ksDup (Pointer source);

	int ksCopy (Pointer dest, Pointer source);

	int ksDel (Pointer ks);

	int ksGetSize (Pointer ks);

	int ksAppendKey (Pointer ks, Pointer toAppend);

	int ksAppend (Pointer ks, Pointer toAppend);

	@Nullable Pointer ksCut (Pointer ks, Pointer cutpoint);

	Pointer elektraKsPopAtCursor (Pointer ks, int cursor);

	// TODO #3171 only imported for rewinding the internal iterator of key sets
	// being passed to plugins
	int ksRewind (Pointer ks);

	@Nullable Pointer ksHead (Pointer ks);

	@Nullable Pointer ksTail (Pointer ks);

	@Nullable Pointer ksAtCursor (Pointer ks, int cursor);

	/**
	 * Flag for use with {@link #ksLookup(Pointer, Pointer, int)} and
	 * {@link #ksLookupByName(Pointer, String, int)}. Represents no option set.
	 */
	static final int KDB_O_NONE = 0;

	/**
	 * Flag for use with {@link #ksLookup(Pointer, Pointer, int)}. Will cause the
	 * release of parameter {@code key} using {@link #keyDel(Pointer)}.
	 */
	static final int KDB_O_DEL = 1;

	/**
	 * Flag for use with {@link #ksLookup(Pointer, Pointer, int)} and
	 * {@link #ksLookupByName(Pointer, String, int)}. Will cause the found key to be
	 * removed from the key set.
	 */
	static final int KDB_O_POP = 1 << 1;

	@Nullable Pointer ksLookup (Pointer ks, Pointer key, int options);

	@Nullable Pointer ksLookupByName (Pointer ks, String name, int options);

	NativePlugin.ElektraPlugin elektraPluginOpen (String pluginName, Pointer modules, Pointer config, Pointer errorKey);

	NativePlugin.ElektraPlugin elektraPluginClose (String pluginName, Pointer errorKey);
}
