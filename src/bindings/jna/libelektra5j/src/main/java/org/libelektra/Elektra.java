package org.libelektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import org.libelektra.plugin.NativePlugin;

/**
 * The JNA interface to libelektra.
 */
public interface Elektra extends Library {

	/**
	 * Overall native binding to the elektra library
	 */
	Elektra INSTANCE = (Elektra) Native.loadLibrary ("elektra-kdb", Elektra.class);

	/**************************************
	 *
	 * KDB methods
	 *
	 **************************************/

	Pointer kdbOpen (Pointer contract, Pointer parent);

	int kdbClose (Pointer handle, Pointer errorPointer);

	int kdbGet (Pointer handle, Pointer returned, Pointer parentPointer);

	int kdbSet (Pointer handle, Pointer returned, Pointer parentPointer);

	int elektraGOptsContractFromStrings (Pointer contract, long argsSize, String args, long envSize, String env, Pointer parentKey,
					     Pointer goptsConfig);

	/**************************************
	 *
	 * Key methods
	 *
	 **************************************/

	Pointer keyNew (String name, Object... args);

	Pointer keyDup (Pointer source, int flags);

	int keyCopy (Pointer dest, Pointer source, int flags);

	int keyClear (Pointer key); // not needed

	int keyDel (Pointer key);

	int keyIncRef (Pointer key);

	int keyDecRef (Pointer key);

	int keyGetRef (Pointer key);

	/* Meta Info */
	int keyRewindMeta (Pointer key);

	Pointer keyNextMeta (Pointer key);

	Pointer keyCurrentMeta (Pointer key);

	int keyCopyMeta (Pointer dest, Pointer source, String metaName);

	int keyCopyAllMeta (Pointer dest, Pointer source);

	Pointer keyGetMeta (Pointer key, String metaName);

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

	int keyGetName (Pointer key, String returnedName, int maxSize); // not needed

	int keySetName (Pointer key, String newname);

	Pointer keyUnescapedName (Pointer key);

	int keyGetUnescapedNameSize (Pointer key);

	String keyBaseName (Pointer key); // not implemented

	int keyGetBaseNameSize (Pointer key); // not implemented

	int keyGetBaseName (Pointer key, String returned, int maxSize); // not needed

	int keySetBaseName (Pointer key, String baseName);

	int keyAddBaseName (Pointer key, String baseName);

	/* Value Manipulation Methods */
	// byte[] keyValue(Pointer key);
	int keyGetValueSize (Pointer key);

	String keyString (Pointer key);

	int keyGetString (Pointer key, String returnedString, int maxSize); // not needed

	int keySetString (Pointer key, String newString);

	// int keyGetBinary(Pointer key, byte[] returnedBinary, int maxSize);
	// int keySetBinary(Pointer key, byte[] newBinary, int dataSize);

	/**************************************
	 *
	 * KeySet methods
	 *
	 **************************************/

	Pointer ksNew (int alloc, Object... args);

	Pointer ksDup (Pointer source);

	int ksCopy (Pointer dest, Pointer source);

	int ksClear (Pointer ks); // not needed

	int ksDel (Pointer ks);

	int ksNeedSync (Pointer ks);

	int ksGetSize (Pointer ks);

	int ksAppendKey (Pointer ks, Pointer toAppend);

	int ksAppend (Pointer ks, Pointer toAppend);

	Pointer ksCut (Pointer ks, Pointer cutpoint);

	// TODO #3137 Also elektraKsPopAtCursor should replace the current ksPop. See also #3189.
	Pointer ksPop (Pointer ks);

	// deprecated for removal - (forRemoval = true) not set since not all build server are using JDK >=9 yet
	@Deprecated int ksRewind (Pointer ks);

	Pointer ksHead (Pointer ks);

	Pointer ksTail (Pointer ks);

	// TODO #3137 ksAtCursor needs to stay and just be renamed to ksAt. It doesn't use the internal cursor, but takes an external cursor
	// value.
	Pointer ksAtCursor (Pointer ks, int cursor);

	Pointer ksLookup (Pointer ks, Pointer key, int options);

	Pointer ksLookupByName (Pointer ks, String name, int options);

	NativePlugin.ElektraPlugin elektraPluginOpen (String pluginName, Pointer modules, Pointer config, Pointer errorKey);

	NativePlugin.ElektraPlugin elektraPluginClose (String pluginName, Pointer errorKey);
}
