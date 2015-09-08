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

	public boolean isNull() {
		return key == null;
	}

	// java's specials
	public String toString() {
		return getName();
	}

	public java.util.Iterator<String> iterator() {
		return new KeyNameIterator(this);
	}

	public boolean getBoolean() {
		return Boolean.parseBoolean(getString());
	}

	public Byte getByte() {
		return Byte.parseByte(getString());
	}

	public Short getShort() {
		return Short.parseShort(getString());
	}

	public Integer getInteger() {
		return Integer.parseInt(getString());
	}

	public Long getLong() {
		return Long.parseLong(getString());
	}

	public Float getFloat() {
		return Float.parseFloat(getString());
	}

	public Double getDouble() {
		return Double.parseDouble(getString());
	}

	public void setBoolean(boolean v) {
		setString(Boolean.toString(v));
	}

	public void setByte(Byte v) {
		setString(Byte.toString(v));
	}

	public void setShort(Short v) {
		setString(Short.toString(v));
	}

	public void setInteger(Integer v) {
		setString(Integer.toString(v));
	}

	public void setLong(Long v) {
		setString(Long.toString(v));
	}

	public void setFloat(Float v) {
		setString(Float.toString(v));
	}

	public void setDouble(Double v) {
		setString(Double.toString(v));
	}

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

	public boolean isBelow(Key other) {
		return Elektra.INSTANCE.keyIsBelow(other.get(), get()) == 1;
	}

	public boolean isBelowOrSame(Key other) {
		return Elektra.INSTANCE.keyIsBelowOrSame(other.get(), get()) == 1;
	}

	public boolean isDirectBelow(Key other) {
		return Elektra.INSTANCE.keyIsDirectBelow(other.get(), get()) == 1;
	}

	public boolean isInactive() {
		return Elektra.INSTANCE.keyIsInactive(get()) == 1;
	}

	public boolean isBinary() {
		return Elektra.INSTANCE.keyIsBinary(get()) == 1;
	}

	public boolean isString() {
		return Elektra.INSTANCE.keyIsString(get()) == 1;
	}

	public String getName() {
		return Elektra.INSTANCE.keyName(key);
	}

	public int getNameSize() {
		return Elektra.INSTANCE.keyGetNameSize(get());
	}

	public void setName(String name) throws KeyInvalidName {
		if (Elektra.INSTANCE.keySetName(get(), name) == -1) {
			throw new KeyInvalidName();
		}
	}

	public String getBaseName(String name) {
		return Elektra.INSTANCE.keyBaseName(get());
	}

	public int getBaseNameSize() {
		return Elektra.INSTANCE.keyGetBaseNameSize(get());
	}

	public void setBaseName(String baseName) throws KeyInvalidName {
		if (Elektra.INSTANCE.keySetBaseName(get(), baseName) == -1) {
			throw new KeyInvalidName();
		}
	}

	public void addBaseName(String baseName) throws KeyInvalidName {
		if (Elektra.INSTANCE.keyAddBaseName(get(), baseName) == -1) {
			throw new KeyInvalidName();
		}
	}

	public int getValueSize() {
		return Elektra.INSTANCE.keyGetValueSize(get());
	}

	public String getString() throws KeyTypeMismatch {
		if (isBinary()) {
			throw new KeyTypeMismatch();
		}
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
