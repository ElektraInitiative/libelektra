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
	Elektra INSTANCE = (Elektra) Native.loadLibrary("elektra-kdb", Elektra.class);

	/**************************************
	 *
	 * KDB methods
	 *
	 **************************************/

	Pointer kdbOpen(Pointer p);

	int kdbClose(Pointer handle, Pointer errorPointer);

	int kdbGet(Pointer handle, Pointer returned, Pointer parentPointer);

	int kdbSet(Pointer handle, Pointer returned, Pointer parentPointer);

	/**************************************
	 *
	 * Key methods
	 *
	 **************************************/

	Pointer keyNew(String name, Object... args);

	Pointer keyDup(Pointer source);

	int keyCopy(Pointer dest, Pointer source);

	int keyClear(Pointer key); // not needed

	int keyDel(Pointer key);

	int keyIncRef(Pointer key);

	int keyDecRef(Pointer key);

	int keyGetRef(Pointer key);

	/* Meta Info */
	int keyRewindMeta(Pointer key);

	Pointer keyNextMeta(Pointer key);

	Pointer keyCurrentMeta(Pointer key);

	int keyCopyMeta(Pointer dest, Pointer source, String metaName);

	int keyCopyAllMeta(Pointer dest, Pointer source);

	Pointer keyGetMeta(Pointer key, String metaName);

	int keySetMeta(Pointer key, String metaName, String newMetaString);

	/* Methods for Making Tests */
	int keyCmp(Pointer k1, Pointer k2);

	int keyNeedSync(Pointer key);

	int keyIsBelow(Pointer key, Pointer check);

	int keyIsBelowOrSame(Pointer key, Pointer check);

	int keyIsDirectlyBelow(Pointer key, Pointer check);

	int keyIsInactive(Pointer key);

	int keyIsBinary(Pointer key);

	int keyIsString(Pointer key);

	/* Name Manipulation Methods */
	String keyName(Pointer key);

	int keyGetNameSize(Pointer key);

	int keyGetName(Pointer key, String returnedName, int maxSize); // not needed

	int keySetName(Pointer key, String newname);

	Pointer keyUnescapedName(Pointer key);

	int keyGetUnescapedNameSize(Pointer key);

	int keyGetFullNameSize(Pointer key); // not implemented

	int keyGetFullName(Pointer key, String returnedName, int maxSize); // not implemented

	String keyBaseName(Pointer key); // not implemented

	int keyGetBaseNameSize(Pointer key); // not implemented

	int keyGetBaseName(Pointer key, String returned, int maxSize); // not needed

	int keySetBaseName(Pointer key, String baseName);

	int keyAddBaseName(Pointer key, String baseName);

	/* Value Manipulation Methods */
	// byte[] keyValue(Pointer key);
	int keyGetValueSize(Pointer key);

	String keyString(Pointer key);

	int keyGetString(Pointer key, String returnedString, int maxSize); // not needed

	int keySetString(Pointer key, String newString);

	// int keyGetBinary(Pointer key, byte[] returnedBinary, int maxSize);
	// int keySetBinary(Pointer key, byte[] newBinary, int dataSize);

	/**************************************
	 *
	 * KeySet methods
	 *
	 **************************************/

	Pointer ksNew(int alloc, Object... args);

	Pointer ksDup(Pointer source);

	int ksCopy(Pointer dest, Pointer source);

	int ksClear(Pointer ks); // not needed

	int ksDel(Pointer ks);

	int ksNeedSync(Pointer ks);

	int ksGetSize(Pointer ks);

	int ksAppendKey(Pointer ks, Pointer toAppend);

	int ksAppend(Pointer ks, Pointer toAppend);

	Pointer ksCut(Pointer ks, Pointer cutpoint);

	Pointer ksPop(Pointer ks);

	int ksRewind(Pointer ks);

	Pointer ksNext(Pointer ks);

	Pointer ksCurrent(Pointer ks);

	Pointer ksHead(Pointer ks);

	Pointer ksTail(Pointer ks);

	int ksGetCursor(Pointer ks);

	int ksSetCursor(Pointer ks, int cursor);

	Pointer ksAtCursor(Pointer ks, int cursor);

	Pointer ksLookup(Pointer ks, Pointer key, int options);

	Pointer ksLookupByName(Pointer ks, String name, int options);

	NativePlugin.ElektraPlugin elektraPluginOpen(String pluginName, Pointer modules, Pointer config, Pointer errorKey);
}
