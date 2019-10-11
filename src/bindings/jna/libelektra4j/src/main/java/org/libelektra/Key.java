package org.libelektra;

import com.sun.jna.Pointer;
import org.libelektra.exception.PluginMisbehaviorException;

/**
 * Key is an essential class that encapsulates key name , value and metainfo.
 */
public class Key implements Iterable<String> {

	private static final String WARNINGS = "warnings";
	// constants
	public static final int KEY_END = 0;
	public static final int KEY_NAME = 1;
	public static final int KEY_VALUE = 1 << 1;
	public static final int KEY_OWNER = 1 << 2;
	public static final int KEY_COMMENT = 1 << 3;
	public static final int KEY_BINARY = 1 << 4;
	public static final int KEY_UID = 1 << 5;
	public static final int KEY_GID = 1 << 6;
	public static final int KEY_MODE = 1 << 7;
	public static final int KEY_ATIME = 1 << 8;
	public static final int KEY_MTIME = 1 << 9;
	public static final int KEY_CTIME = 1 << 10;
	public static final int KEY_SIZE = 1 << 11;
	public static final int KEY_FUNC = 1 << 12;
	public static final int KEY_DIR = 1 << 14;
	public static final int KEY_META = 1 << 15;
	public static final int KEY_NULL = 1 << 16;
	public static final int KEY_CASCADING_NAME = 1 << 20;
	public static final int KEY_META_NAME = 1 << 21;

	/**
	 * Indicates a generic key exception occurred.
	 */
	public static class KeyException extends RuntimeException {

		private static final long serialVersionUID = 637936674538102511L;

	}

	/**
	 * Indicates an invalid key name has been used.
	 */
	public static class KeyInvalidName extends KeyException {

		private static final long serialVersionUID = -7659317176138893895L;

	}

	/**
	 * Indicates a key's type conversion failed.
	 */
	public static class KeyTypeConversion extends KeyException {

		private static final long serialVersionUID = -8648296754188373810L;

	}

	/**
	 * Indicates a key's type does not match its value.
	 */
	public static class KeyTypeMismatch extends KeyException {

		private static final long serialVersionUID = 8035874860117969698L;

	}

	private Pointer key;

	/**
	 * Helper constructor for duplication by pointer in long format
	 *
	 * @param p Pointer in long format
	 */
	protected Key(final long p) {
		key = new Pointer(p);
		incRef();
	}

	/**
	 * Helper constructor for duplication by pointer
	 *
	 * @param p Pointer as Pointer object
	 */
	protected Key(final Pointer p) {
		key = p;
		incRef();
	}

	/**
	 * Basic constructor of key class
	 *
	 * @param name Key name; first part of key-value pair
	 * @param args Arguments used for key value. Example:<br>
	 *             Key.KEY_VALUE, "custom key value", Key.KEY_END
	 * @return New key object
	 */
	protected static Key create(final String name, final Object... args) {
		return new Key(Elektra.INSTANCE.keyNew(name, args));
	}

	/**
	 * Basic constructor of key class
	 *
	 * @param name  Key name; first part of key-value pair
	 * @param value Key value; will be determine from the object by calling {@link Object#toString()}, null is
	 *                 supported too
	 * @param meta  Metadata that should be added to this key, null keys will be filtered away
	 * @return New key object
	 */
	public static Key create(final String name, final Object value, final Key... meta) {
		int size = 0;
		for (final Key m : meta) {
			if (m != null) {
				size++;
			}
		}
		// 3 -> KEY_VALUE, value, KEY_END, 4 -> one more for KEY_META
		size += size > 0 ? 4 : 3;
		final Object[] args = new Object[size];
		int cur = 0;
		args[cur++] = Integer.valueOf(KEY_VALUE);
		args[cur++] = value != null ? value.toString() : null;
		if (size > 3) {
			args[cur++] = Integer.valueOf(KEY_META);
			for (final Key m : meta) {
				args[cur++] = m;
			}
		}
		args[cur] = Integer.valueOf(KEY_END);
		return create(name, args);
	}

	/**
	 * Basic constructor of key class
	 *
	 * @param name Key name; first part of key-value pair
	 * @param meta Metadata that should be added to this key. Will filter null values.
	 * @return New key object
	 */
	public static Key create(final String name, final Key... meta) {
		return create(name, null, meta);
	}

	/**
	 * Clean-up method to release key reference
	 */
	public void release() {

		if (key != null) {
			decRef();
			if (getRef() == 0) {
				Elektra.INSTANCE.keyDel(key);
			}
		}
		key = null;
	}

	/**
	 * Clean-up method to inform underlying c-library about the release of the key reference in jna-binding
	 */
	@Override
	protected void finalize() throws Throwable {
		release();
	}

	/**
	 * Helper function that does null comparison
	 *
	 * @return Boolean if key is null
	 */
	public boolean isNull() {

		return key == null;
	}

	/**
	 * Basic java function that represents object as String
	 *
	 * @return Key name in String format
	 */
	@Override
	public String toString() {
		return getName();
	}

	/**
	 * Iterable interface function
	 *
	 * @return Custom KeyNameIterator
	 */
	@Override
	public java.util.Iterator<String> iterator() {
		return new KeyNameIterator(this);
	}

	/**
	 * Data type specific accessor function
	 *
	 * @return Key value in boolean format
	 */
	public boolean getBoolean() {
		return Boolean.parseBoolean(getString());
	}

	/**
	 * Data type specific accessor function
	 *
	 * @return Key value in byte format
	 */
	public byte getByte() {
		return Byte.parseByte(getString());
	}

	/**
	 * Data type specific accessor function
	 *
	 * @return Key value in short integer format
	 */
	public short getShort() {
		return Short.parseShort(getString());
	}

	/**
	 * Data type specific accessor function
	 *
	 * @return Key value in integer format
	 */
	public int getInteger() {
		return Integer.parseInt(getString());
	}

	/**
	 * Data type specific accessor function
	 *
	 * @return Key value in long integer format
	 */
	public long getLong() {
		return Long.parseLong(getString());
	}

	/**
	 * Data type specific accessor function
	 *
	 * @return Key value in float format
	 */
	public float getFloat() {
		return Float.parseFloat(getString());
	}

	/**
	 * Data type specific accessor function
	 *
	 * @return Key value in double format
	 */
	public double getDouble() {
		return Double.parseDouble(getString());
	}

	/**
	 * Data type specific setter function
	 *
	 * @param v Boolean value to set
	 */
	public void setBoolean(final boolean v) {
		setString(Boolean.toString(v));
	}

	/**
	 * Data type specific setter function
	 *
	 * @param v Byte value to set
	 */
	public void setByte(final byte v) {
		setString(Byte.toString(v));
	}

	/**
	 * Data type specific setter function
	 *
	 * @param v Short integer value to set
	 */
	public void setShort(final short v) {
		setString(Short.toString(v));
	}

	/**
	 * Data type specific setter function
	 *
	 * @param v Integer value to set
	 */
	public void setInteger(final int v) {
		setString(Integer.toString(v));
	}

	/**
	 * Data type specific setter function
	 *
	 * @param v Long integer value to set
	 */
	public void setLong(final long v) {
		setString(Long.toString(v));
	}

	/**
	 * Data type specific setter function
	 *
	 * @param v Float value to set
	 */
	public void setFloat(final float v) {
		setString(Float.toString(v));
	}

	/**
	 * Data type specific setter function
	 *
	 * @param v Double value to set
	 */
	public void setDouble(final double v) {
		setString(Double.toString(v));
	}

	/**
	 * Helper function to set proper error meta for key
	 *
	 * @param text Reason for the error
	 * @param args Custom arguments
	 */
	public void setError(final String text, final Object... args) {
		final StackTraceElement[] e = Thread.currentThread().getStackTrace();
		setMeta("error", "number description module file line function reason");
		setMeta("error/number", PluginMisbehaviorException.errorNumber());
		setMeta("error/description", "jni/java error");
		setMeta("error/module", e[1].getClassName() + " " + e[1].getMethodName());
		setMeta("error/file", e[1].getFileName());
		setMeta("error/line", Integer.toString(e[1].getLineNumber()));
		setMeta("error/mountpoint", getName());
		setMeta("error/configfile", getString());
		setMeta("error/reason", String.format(text, args));
	}

	/**
	 * Helper function to add warning meta for key
	 *
	 * @param text Reason for the warning
	 * @param args Custom arguments
	 */
	public void addWarning(final String text, final Object... args) {
		final StackTraceElement[] e = Thread.currentThread().getStackTrace();
		final Key k = getMeta(WARNINGS);
		final StringBuilder builder = new StringBuilder(WARNINGS + "/#");
		if (!k.isNull()) {
			builder.append(k.getString());
			builder.setCharAt(11, (char) (builder.charAt(11) + 1));
			if (builder.charAt(11) > '9') {
				builder.setCharAt(11, '0');
				builder.setCharAt(10, (char) (builder.charAt(10) + 1));
				if (builder.charAt(10) > '9') {
					builder.setCharAt(10, '0');
				}
			}
			setMeta(Key.WARNINGS, builder.substring(10));
		} else {
			builder.append("00");
			setMeta(Key.WARNINGS, "00");
		}
		setMeta(builder + "", "number description module file line function reason");
		setMeta(builder + "/number", PluginMisbehaviorException.errorNumber());
		setMeta(builder + "/description", "jni/java warning");
		setMeta(builder + "/module", e[1].getClassName() + " " + e[1].getMethodName());
		setMeta(builder + "/file", e[1].getFileName());
		setMeta(builder + "/line", Integer.toString(e[1].getLineNumber()));
		setMeta(builder + "/mountpoint", getName());
		setMeta(builder + "/configfile", getString());
		setMeta(builder + "/reason", String.format(text, args));
	}

	/*
	 * Wrapped methods
	 */

	/**
	 * Duplicates the key
	 *
	 * @return New Key object containing the same information as this key
	 */
	public Key dup() {
		return new Key(Elektra.INSTANCE.keyDup(get()));
	}

	/**
	 * Copies the information from the source key into this key. Does nothing if null is provided.
	 *
	 * @param source Source Key object containing the information to copy
	 */
	public void copy(final Key source) {
		if (source != null) {
			Elektra.INSTANCE.keyCopy(get(), source.get());
		}
	}

	/**
	 * Increments the reference counter for this key
	 */
	protected void incRef() {
		Elektra.INSTANCE.keyIncRef(key);
	}

	/**
	 * Decrements the reference counter for this key
	 */
	protected void decRef() {
		Elektra.INSTANCE.keyDecRef(key);
	}

	/**
	 * Gets the reference counter for this key
	 *
	 * @return Reference counter as integer
	 */
	public int getRef() {
		return Elektra.INSTANCE.keyGetRef(get());
	}

	/**
	 * Tries to rewind the meta information for this key
	 *
	 * @return 0 in case of no errors; 1 if key is not found; 2 if metakey is not found
	 */
	public int rewindMeta() {
		return Elektra.INSTANCE.keyRewindMeta(get());
	}

	/**
	 * Gets the next meta information for this key
	 *
	 * @return new Key object containing the next meta information
	 */
	public Key nextMeta() {
		return new Key(Elektra.INSTANCE.keyNextMeta(get()));
	}

	/**
	 * Gets the current meta information for this key
	 *
	 * @return new Key object containing the current meta information
	 */
	public Key currentMeta() {
		return new Key(Elektra.INSTANCE.keyNextMeta(get()));
	}

	/**
	 * Helper function to copy some meta information from a source Key to this key.
	 *
	 * @param source   Key object that is used as source
	 * @param metaName Key name of the meta to be copied
	 * @return 1 if meta was successfully copied, 0 if source doesn't contain the required meta and nothing had to be
	 * done, -1 in case of an
	 * error or if the source parameter was null
	 */
	public int copyMeta(final Key source, final String metaName) {
		if (source == null) {
			return -1;
		}
		return Elektra.INSTANCE.keyCopyMeta(get(), source.get(), metaName);
	}

	/**
	 * Helper function to copy all meta information from a source key to this key
	 *
	 * @param source Key object that is used as source
	 * @return 1 if meta was successfully copied, 0 if source doesn't contain any meta and nothing had to be done, -1
	 * in case of an error or
	 * if the source parameter was null
	 */
	public int copyAllMeta(final Key source) {
		if (source == null) {
			return -1;
		}
		return Elektra.INSTANCE.keyCopyAllMeta(get(), source.get());
	}

	/**
	 * Getter for meta information
	 *
	 * @param metaName Key name of meta information to be fetched
	 * @return New Key object containing the requested meta information
	 */
	public Key getMeta(final String metaName) {
		return new Key(Elektra.INSTANCE.keyGetMeta(get(), metaName));
	}

	/**
	 * Setter for meta information
	 *
	 * @param metaName      Key name of meta information to be set
	 * @param newMetaString Meta value to be set
	 * @return -1 in case of an error, 0 if no meta with given name is available for the key and value > 0
	 * representing the size of
	 * newMetaString if update successful
	 */
	public int setMeta(final String metaName, final String newMetaString) {
		return Elektra.INSTANCE.keySetMeta(get(), metaName, newMetaString);
	}

	/**
	 * Helper function to compare two keys. Compares the key name with normal String comparison.
	 *
	 * @param other Other Key object that is used in comparison
	 * @return 0 if key name is equal; -1 if this key name has lower alphabetical order than the other key; 1 if this
	 * key has higher
	 * alphabetical order
	 */
	public int cmp(final Key other) {
		if (other == null) {
			throw new IllegalArgumentException("other should be a key, not null");
		}
		return Elektra.INSTANCE.keyCmp(get(), other.get());
	}

	/**
	 * Helper function to check if synchronization is necessary
	 *
	 * @return 1 if needs sync, 0 if no change done and -1 in case of a null pointer
	 */
	public int needsSync() {
		return Elektra.INSTANCE.keyNeedSync(get());
	}

	/**
	 * Helper function to check if key is sub-key of other key
	 *
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is (non-direct) sub-key of other-key
	 */
	public boolean isBelow(final Key other) {
		if (other == null) {
			throw new IllegalArgumentException("other should be a key, not null");
		}
		return Elektra.INSTANCE.keyIsBelow(other.get(), get()) == 1;
	}

	/**
	 * Helper function to check if key is other key or sub-key of other key
	 *
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is other key or (non-direct) sub-key of other-key
	 */
	public boolean isBelowOrSame(final Key other) {
		if (other == null) {
			throw new IllegalArgumentException("other should be a key, not null");
		}
		return Elektra.INSTANCE.keyIsBelowOrSame(other.get(), get()) == 1;
	}

	/**
	 * Helper function to check if key is direct sub-key of other key
	 *
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is direct sub-key of other key ("child")
	 */
	public boolean isDirectBelow(final Key other) {
		if (other == null) {
			throw new IllegalArgumentException("other should be a key, not null");
		}
		return Elektra.INSTANCE.keyIsDirectlyBelow(other.get(), get()) == 1;
	}

	/**
	 * Helper function to check if key is set to inactive
	 *
	 * @return Boolean if this key is inactive
	 */
	public boolean isInactive() {
		return Elektra.INSTANCE.keyIsInactive(get()) == 1;
	}

	/**
	 * Helper function to check if key is binary key
	 *
	 * @return Boolean if this key is a binary key
	 */
	public boolean isBinary() {
		return Elektra.INSTANCE.keyIsBinary(get()) == 1;
	}

	/**
	 * Helper function to check if key is string key
	 *
	 * @return Boolean if this key is a string key
	 */
	public boolean isString() {
		return Elektra.INSTANCE.keyIsString(get()) == 1;
	}

	/**
	 * Helper function to get key name (key part of "key-value" pair)
	 *
	 * @return Key name as String
	 */
	public String getName() {
		return Elektra.INSTANCE.keyName(key);
	}

	/**
	 * Helper function to get key name size
	 *
	 * @return Length of key name
	 */
	public int getNameSize() {
		return Elektra.INSTANCE.keyGetNameSize(get());
	}

	/**
	 * Helper function to set key name
	 *
	 * @param name New key name to use
	 * @throws KeyInvalidName
	 */
	public void setName(final String name) throws KeyInvalidName {
		if (Elektra.INSTANCE.keySetName(get(), name) == -1) {
			throw new KeyInvalidName();
		}
	}

	/**
	 * Helper function to get key base name
	 *
	 * @return Key base name as String
	 */
	public String getBaseName() {
		return Elektra.INSTANCE.keyBaseName(get());
	}

	/**
	 * Helper function to get key base name length
	 *
	 * @return Length of key base name
	 */
	public int getBaseNameSize() {
		return Elektra.INSTANCE.keyGetBaseNameSize(get());
	}

	/**
	 * Helper function to set key base name; will replace current base name with new base name
	 *
	 * @param baseName New key base name to use
	 * @throws KeyInvalidName
	 */
	public void setBaseName(final String baseName) throws KeyInvalidName {
		if (Elektra.INSTANCE.keySetBaseName(get(), baseName) == -1) {
			throw new KeyInvalidName();
		}
	}

	/**
	 * Helper function to add key base name; will add given base name to current key so that new key is sub key of
	 * current key
	 *
	 * @param baseName New key base name to add
	 * @throws KeyInvalidName
	 */
	public void addBaseName(final String baseName) throws KeyInvalidName {
		if (Elektra.INSTANCE.keyAddBaseName(get(), baseName) == -1) {
			throw new KeyInvalidName();
		}
	}

	/**
	 * Helper function to get key value size/length
	 *
	 * @return Length of key value
	 */
	public int getValueSize() {
		return Elektra.INSTANCE.keyGetValueSize(get());
	}

	/**
	 * Helper function to get representation of key value
	 *
	 * @return Key value in String format
	 * @throws KeyTypeMismatch
	 */
	public String getString() throws KeyTypeMismatch {
		if (isBinary()) {
			throw new KeyTypeMismatch();
		}
		return Elektra.INSTANCE.keyString(key);
	}

	/**
	 * Helper function to set new key value
	 *
	 * @param newString New key value to set
	 * @return value > 0 representing saved bytes (+null byte), -1 in case of an error (null key)
	 */
	public int setString(final String newString) {
		return Elektra.INSTANCE.keySetString(get(), newString);
	}

	/**
	 * Native pointer used by JNA
	 *
	 * @return Native pointer object for this key
	 */
	public Pointer get() {
		return key;
	}
}
