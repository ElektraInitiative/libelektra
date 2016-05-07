package elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class Key implements java.lang.Iterable<String> {
	// constants
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
	public static final int KEY_CASCADING_NAME=1<<20;
	public static final int KEY_META_NAME=1<<21;

	// exceptions
	public class KeyException extends java.lang.RuntimeException{}
	public class KeyInvalidName extends KeyException{}
	public class KeyTypeConversion extends KeyException{}
	public class KeyTypeMismatch extends KeyException{}

	/**
	 * Basic constructor of key class
	 * @param name Key name; first part of key-value pair
	 * @param args Arguments used for key value. Example:<br>
	 * 		Key.KEY_VALUE, "custom key value", Key.KEY_END
	 * @return New key object
	 */
	public static Key create(String name, Object... args) {
		return new Key(Elektra.INSTANCE.keyNew(name, args));
	}

	/**
	 * Helper constructor for duplication by pointer in long format
	 * @param p Pointer in long format
	 */
	public Key(long p) {
		key = new Pointer(p);
		incRef();
	}

	/**
	 * Helper constructor for duplication by pointer
	 * @param p Pointer as Pointer object
	 */
	public Key(Pointer p) {
		key = p;
		incRef();
	}

	/**
	 * Clean-up method to release key reference
	 */
	public void release() {
		key = null;
	}

	/**
	 * Clean-up method to inform underlying c-library about the
	 * release of the key reference in jna-binding
	 */
	protected void finalize() throws Throwable {
		decRef();
		Elektra.INSTANCE.keyDel(key);
	}

	/**
	 * Helper function that does null comparison
	 * @return Boolean if key is null
	 */
	public boolean isNull() {
		return key == null;
	}

	/**
	 * Basic java function that represents object as String
	 * @return Key name in String format
	 */
	@Override
	public String toString() {
		return getName();
	}

	/**
	 * Iterable interface function
	 * @return Custom KeyNameIterator
	 */
	public java.util.Iterator<String> iterator() {
		return new KeyNameIterator(this);
	}

	/**
	 * Data type specific accessor function
	 * @return Key value in boolean format
	 */
	public boolean getBoolean() {
		return Boolean.parseBoolean(getString());
	}

	/**
	 * Data type specific accessor function
	 * @return Key value in byte format
	 */
	public Byte getByte() {
		return Byte.parseByte(getString());
	}

	/**
	 * Data type specific accessor function
	 * @return Key value in short integer format
	 */
	public Short getShort() {
		return Short.parseShort(getString());
	}

	/**
	 * Data type specific accessor function
	 * @return Key value in integer format
	 */
	public Integer getInteger() {
		return Integer.parseInt(getString());
	}

	/**
	 * Data type specific accessor function
	 * @return Key value in long integer format
	 */
	public Long getLong() {
		return Long.parseLong(getString());
	}

	/**
	 * Data type specific accessor function
	 * @return Key value in float format
	 */
	public Float getFloat() {
		return Float.parseFloat(getString());
	}

	/**
	 * Data type specific accessor function
	 * @return Key value in double format
	 */
	public Double getDouble() {
		return Double.parseDouble(getString());
	}

	/**
	 * Data type specific setter function
	 * @param v Boolean value to set
	 */
	public void setBoolean(boolean v) {
		setString(Boolean.toString(v));
	}

	/**
	 * Data type specific setter function
	 * @param v Byte value to set
	 */
	public void setByte(Byte v) {
		setString(Byte.toString(v));
	}

	/**
	 * Data type specific setter function
	 * @param v Short integer value to set
	 */
	public void setShort(Short v) {
		setString(Short.toString(v));
	}

	/**
	 * Data type specific setter function
	 * @param v Integer value to set
	 */
	public void setInteger(Integer v) {
		setString(Integer.toString(v));
	}

	/**
	 * Data type specific setter function
	 * @param v Long integer value to set
	 */
	public void setLong(Long v) {
		setString(Long.toString(v));
	}

	/**
	 * Data type specific setter function
	 * @param v Float value to set
	 */
	public void setFloat(Float v) {
		setString(Float.toString(v));
	}

	/**
	 * Data type specific setter function
	 * @param v Double value to set
	 */
	public void setDouble(Double v) {
		setString(Double.toString(v));
	}

	/**
	 * Helper function to set proper error meta for key
	 * @param text Reason for the error
	 * @param args Custom arguments
	 */
	public void setError(String text, Object... args) {
		StackTraceElement e[] = Thread.currentThread().getStackTrace();
		setMeta("error", "number description ingroup module file line function reason");
		setMeta("error/number", "102");
		setMeta("error/description", "jni/java error");
		setMeta("error/ingroup", "plugin");
		setMeta("error/module", e[1].getClassName() + " " +  e[1].getMethodName());
		setMeta("error/file", e[1].getFileName());
		setMeta("error/line", Integer.toString(e[1].getLineNumber()));
		setMeta("error/mountpoint", getName());
		setMeta("error/configfile", getString());
		setMeta("error/reason", String.format(text, args));
	}

	/**
	 * Helper function to add warning meta for key
	 * @param text Reason for the warning
	 * @param args Custom arguments
	 */
	public void addWarning(String text, Object... args) {
		StackTraceElement e[] = Thread.currentThread().getStackTrace();
		Key k = getMeta("warnings");
		StringBuffer buffer = new StringBuffer("warnings/#");
		if (!k.isNull()) {
			buffer.append(k.getString());
			buffer.setCharAt(11, (char)(buffer.charAt(11)+1));
			if (buffer.charAt(11) > '9')
			{
				buffer.setCharAt(11, '0');
				buffer.setCharAt(10, (char)(buffer.charAt(10)+1));
				if (buffer.charAt(10) > '9') {
					buffer.setCharAt(10, '0');
				}
			}
			setMeta("warnings", buffer.substring(10));
		} else {
			buffer.append("00");
			setMeta("warnings", "00");
		}
		setMeta(buffer+"", "number description ingroup module file line function reason");
		setMeta(buffer+"/number", "103");
		setMeta(buffer+"/description", "jni/java warning");
		setMeta(buffer+"/ingroup", "plugin");
		setMeta(buffer+"/module", e[1].getClassName() + " " +  e[1].getMethodName());
		setMeta(buffer+"/file", e[1].getFileName());
		setMeta(buffer+"/line", Integer.toString(e[1].getLineNumber()));
		setMeta(buffer+"/mountpoint", getName());
		setMeta(buffer+"/configfile", getString());
		setMeta(buffer+"/reason", String.format(text, args));
	}

	
	/*
	 * Wrapped methods
	 */

	/**
	 * Duplicates the key
	 * @return New Key object containing the same information as this key
	 */
	Key dup() {
		return new Key(Elektra.INSTANCE.keyDup(get()));
	}

	/**
	 * Copies the information from the source key into this key
	 * @param source Source Key object containing the information to copy
	 */
	void copy(Key source) {
		Elektra.INSTANCE.keyCopy(get(), source.get());
	}

	/**
	 * Increments the reference counter for this key
	 */
	public void incRef() {
		Elektra.INSTANCE.keyIncRef(key);
	}

	/**
	 * Decrements the reference counter for this key
	 */
	public void decRef() {
		Elektra.INSTANCE.keyDecRef(key);
	}

	/**
	 * Gets the reference counter for this key
	 * @return Reference counter as integer
	 */
	public int getRef() {
		return Elektra.INSTANCE.keyGetRef(get());
	}

	/**
	 * Tries to rewind the meta information for this key
	 * @return 0 in case of no errors; 1 if key is not found;
	 * 		2 if meta key is not found
	 */
	public int rewindMeta() {
		return Elektra.INSTANCE.keyRewindMeta(get());
	}

	/**
	 * Gets the next meta information for this key
	 * @return new Key object containing the next meta information
	 */
	public Key nextMeta() {
		return new Key(Elektra.INSTANCE.keyNextMeta(get()));
	}

	/**
	 * Gets the current meta information for this key
	 * @return new Key object containing the current meta information
	 */
	public Key currentMeta() {
		return new Key(Elektra.INSTANCE.keyNextMeta(get()));
	}

	/**
	 * Helper function to copy some meta information from a source Key
	 * to this key
	 * @param source Key object that is used as source
	 * @param metaName Key name of the meta to be copied
	 * @return 1 if meta was successfully copied, 0 if source doesn't
	 * 		contain the required meta and nothing had to be done,
	 * 		-1 in case of an error
	 */
	public int copyMeta(Key source, String metaName) {
		return Elektra.INSTANCE.keyCopyMeta(get(), source.get(),
				metaName);
	}

	/**
	 * Helper function to copy all meta information from a source key to
	 * this key
	 * @param source Key object that is used as source
	 * @return 1 if meta was successfully copied, 0 if source doesn't
	 * 		contain any meta and nothing had to be done, -1 in case of 
	 * 		an error
	 */
	public int copyAllMeta(Key source) {
		return Elektra.INSTANCE.keyCopyAllMeta(get(), source.get());
	}

	/**
	 * Getter for meta information
	 * @param metaName Key name of meta information to be fetched
	 * @return New Key object containing the requested meta information
	 */
	public Key getMeta(String metaName) {
		return new Key(Elektra.INSTANCE.keyGetMeta(get(), metaName));
	}

	/**
	 * Setter for meta information
	 * @param metaName Key name of meta information to be set
	 * @param newMetaString Meta value to be set
	 * @return -1 in case of an error, 0 if no meta with given name
	 * 		is available for the key and value > 0 representing the
	 * 		size of newMetaString if update successful
	 */
	public int setMeta(String metaName, String newMetaString) {
		return Elektra.INSTANCE.keySetMeta(get(), metaName,
				newMetaString);
	}

	/**
	 * Helper function to compare two keys. Compares the key name
	 * with normal String comparison.
	 * @param other Other Key object that is used in comparison
	 * @return 0 if key name is equal; -1 if this key name has lower
	 * 		alphabetical order than the other key; 1 if this key has
	 * 		higher alphabetical order
	 */
	public int cmp(Key other) {
		return Elektra.INSTANCE.keyCmp(get(), other.get());
	}

	/**
	 * Helper function to check for relation between keys
	 * @param other Other Key object that is used in relation check
	 * @return 0 if other is equal to this; > 0 if other is sub-key of this key;
	 * 		< 0 otherwise or in case of an error
	 */
	public int rel(Key other) {
		return Elektra.INSTANCE.keyRel(get(), other.get());
	}

	/**
	 * Helper function to check if synchronization is necessary
	 * @return 1 if needs sync, 0 if no change done and -1 in case of
	 * 		a null pointer
	 */
	public int needsSync() {
		return Elektra.INSTANCE.keyNeedSync(get());
	}

	/**
	 * Helper function to check if key is sub-key of other key
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is (non-direct) sub-key of other-key
	 */
	public boolean isBelow(Key other) {
		return Elektra.INSTANCE.keyIsBelow(other.get(), get()) == 1;
	}

	/**
	 * Helper function to check if key is other key or sub-key of other key
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is other key or (non-direct) sub-key of other-key
	 */
	public boolean isBelowOrSame(Key other) {
		return Elektra.INSTANCE.keyIsBelowOrSame(other.get(), get()) == 1;
	}

	/**
	 * Helper function to check if key is direct sub-key of other key
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is direct sub-key of other key ("child")
	 */
	public boolean isDirectBelow(Key other) {
		return Elektra.INSTANCE.keyIsDirectBelow(other.get(), get()) == 1;
	}

	/**
	 * Helper function to check if key is set to inactive
	 * @return Boolean if this key is inactive
	 */
	public boolean isInactive() {
		return Elektra.INSTANCE.keyIsInactive(get()) == 1;
	}

	/**
	 * Helper function to check if key is binary key
	 * @return Boolean if this key is a binary key
	 */
	public boolean isBinary() {
		return Elektra.INSTANCE.keyIsBinary(get()) == 1;
	}

	/**
	 * Helper function to check if key is string key
	 * @return Boolean if this key is a string key
	 */
	public boolean isString() {
		return Elektra.INSTANCE.keyIsString(get()) == 1;
	}

	/**
	 * Helper function to get key name (key part of "key-value" pair)
	 * @return Key name as String
	 */
	public String getName() {
		return Elektra.INSTANCE.keyName(key);
	}
 
	/**
	 * Helper function to get key name size
	 * @return Length of key name
	 */
	public int getNameSize() {
		return Elektra.INSTANCE.keyGetNameSize(get());
	}

	/**
	 * Helper function to set key name
	 * @param name New key name to use
	 * @throws KeyInvalidName
	 */
	public void setName(String name) throws KeyInvalidName {
		if (Elektra.INSTANCE.keySetName(get(), name) == -1) {
			throw new KeyInvalidName();
		}
	}

	/**
	 * Helper function to get key base name
	 * @return Key base name as String
	 */
	public String getBaseName() {
		return Elektra.INSTANCE.keyBaseName(get());
	}

	/**
	 * Helper function to get key base name length
	 * @return Length of key base name
	 */
	public int getBaseNameSize() {
		return Elektra.INSTANCE.keyGetBaseNameSize(get());
	}

	/**
	 * Helper function to set key base name; will replace current
	 * base name with new base name
	 * @param baseName New key base name to use
	 * @throws KeyInvalidName
	 */
	public void setBaseName(String baseName) throws KeyInvalidName {
		if (Elektra.INSTANCE.keySetBaseName(get(), baseName) == -1) {
			throw new KeyInvalidName();
		}
	}

	/**
	 * Helper function to add key base name; will add given base name
	 * to current key so that new key is sub key of current key
	 * @param baseName New key base name to add
	 * @throws KeyInvalidName
	 */
	public void addBaseName(String baseName) throws KeyInvalidName {
		if (Elektra.INSTANCE.keyAddBaseName(get(), baseName) == -1) {
			throw new KeyInvalidName();
		}
	}

	/**
	 * Helper function to get key value size/length
	 * @return Length of key value
	 */
	public int getValueSize() {
		return Elektra.INSTANCE.keyGetValueSize(get());
	}

	/**
	 * Helper function to get representation of key value
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
	 * @param newString New key value to set
	 * @return value > 0 representing saved bytes (+null byte),
	 * 		-1 in case of an error (null key)
	 */
	public int setString(String newString) {
		return Elektra.INSTANCE.keySetString(get(), newString);
	}

	/**
	 * Native pointer used by JNA
	 * @return Native pointer object for this key
	 */
	public Pointer get() {
		return key;
	}

	private Pointer key;
}
