package Elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class Key {
	public static final int KEY_END=0;
	public static final int KEY_NAME=1;
	public static final int KEY_VALUE=1<<1;
	public static final int KEY_OWNER=1<<2;
	public static final int KEY_COMMENT=1<<3;
	public static final int KEY_BINARY=1<<4;
	public static final int KEY_UID=1<<5;
	public static final int KEY_GID=1<<6;
	public static final int KEY_MODE=1<<7;
	public static final int KEY_ATIME=1<<8;
	public static final int KEY_MTIME=1<<9;
	public static final int KEY_CTIME=1<<10;
	public static final int KEY_SIZE=1<<11;
	public static final int KEY_FUNC=1<<12;
	public static final int KEY_DIR=1<<14;
	public static final int KEY_META=1<<15;
	public static final int KEY_NULL=1<<16;

	// basics, construction and destruction
	public static Key create(String name, Object... args) {
		return new Key(Elektra.INSTANCE.keyNew(name, args));
	}

	public Key(long p) {
		key = new Pointer(p);
		incRef();
	}

	public Key(Pointer p) {
		key = p;
		incRef();
	}

	public void release() {
		key = null;
	}

	protected void finalize() throws Throwable {
		decRef();
		Elektra.INSTANCE.keyDel(key);
	}

	// java's specials
	public String toString() {
		return getName();
	}

	// wrapped methods
	Key dup() {
		return new Key(Elektra.INSTANCE.keyDup(get()));
	}

	void copy(Key source) {
		Elektra.INSTANCE.keyCopy(get(), source.get());
	}

	public void incRef() {
		Elektra.INSTANCE.keyIncRef(key);
	}

	public void decRef() {
		Elektra.INSTANCE.keyDecRef(key);
	}

	public int getRef() {
		return Elektra.INSTANCE.keyGetRef(get());
	}

	public int rewindMeta() {
		return Elektra.INSTANCE.keyRewindMeta(get());
	}

	public Key nextMeta() {
		return new Key(Elektra.INSTANCE.keyNextMeta(get()));
	}

	public Key currentMeta() {
		return new Key(Elektra.INSTANCE.keyNextMeta(get()));
	}

	public int copyMeta(Key source, String metaName) {
		return Elektra.INSTANCE.keyCopyMeta(get(), source.get(),
				metaName);
	}

	public int copyAllMeta(Key source) {
		return Elektra.INSTANCE.keyCopyAllMeta(get(), source.get());
	}

	public Key getMeta(String metaName) {
		return new Key(Elektra.INSTANCE.keyGetMeta(get(), metaName));
	}

	public int setMeta(String metaName, String newMetaString) {
		return Elektra.INSTANCE.keySetMeta(get(), metaName,
				newMetaString);
	}

	public int cmp(Key other) {
		return Elektra.INSTANCE.keyCmp(get(), other.get());
	}

	public int rel(Key other) {
		return Elektra.INSTANCE.keyRel(get(), other.get());
	}

	public int needsSync() {
		return Elektra.INSTANCE.keyNeedSync(get());
	}

	public int isBelow(Key other) {
		return Elektra.INSTANCE.keyIsBelow(other.get(), get());
	}

	public int isBelowOrSame(Key other) {
		return Elektra.INSTANCE.keyIsBelowOrSame(other.get(), get());
	}

	public int isDirectBelow(Key other) {
		return Elektra.INSTANCE.keyIsDirectBelow(other.get(), get());
	}

	public int isInactive() {
		return Elektra.INSTANCE.keyIsInactive(get());
	}

	public int isBinary() {
		return Elektra.INSTANCE.keyIsBinary(get());
	}

	public int isString() {
		return Elektra.INSTANCE.keyIsString(get());
	}

	public String getName() {
		return Elektra.INSTANCE.keyName(key);
	}

	public int getNameSize() {
		return Elektra.INSTANCE.keyGetNameSize(get());
	}

	public int setName(String name) {
		return Elektra.INSTANCE.keySetName(get(), name);
	}

	public String getBaseName(String name) {
		return Elektra.INSTANCE.keyBaseName(get());
	}

	public int getBaseNameSize() {
		return Elektra.INSTANCE.keyGetBaseNameSize(get());
	}

	public int setBaseName(String baseName) {
		return Elektra.INSTANCE.keySetBaseName(get(), baseName);
	}

	public int addBaseName(String baseName) {
		return Elektra.INSTANCE.keyAddBaseName(get(), baseName);
	}

	int getValueSize() {
		return Elektra.INSTANCE.keyGetValueSize(get());
	}

	public String getString() {
		return Elektra.INSTANCE.keyString(key);
	}

	public int setString(String newString) {
		return Elektra.INSTANCE.keySetString(get(), newString);
	}


	// native pointer
	public Pointer get() {
		return key;
	}

	private Pointer key;
}
