package org.libelektra;

import static org.libelektra.ValidationUtil.argNotNull;

import com.sun.jna.Pointer;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import org.libelektra.exception.KDBClosedException;

/**
 * Represents a session with the Elektra key database
 *
 * @apiNote Close after usage, or simply use a try-with-resources statement
 */
public class KDB implements AutoCloseable {

  private Pointer pointer;

  /**
   * Opens a new KDB session
   *
   * @return New KDB session
   * @throws KDBException if opening the session fails - see specialization of {@link KDBException}
   */
  @Nonnull
  public static KDB open() throws KDBException {
    return openInternal(null);
  }

  /**
   * Opens KDB session using the specified {@code contract}
   *
   * @param contract Contract configuring the {@code gopts} plugin
   * @return New KDB session
   * @throws KDBException if opening the session fails - see specialization of {@link KDBException}
   * @throws IllegalStateException if {@code contract} has already been released
   * @throws IllegalArgumentException if {@code contract} is {@code null}
   * @see #goptsContract(String[], String[], Key, KeySet)
   * @see #goptsContract(KeySet, String[], String[], Key, KeySet)
   */
  @Nonnull
  public static KDB open(KeySet contract) throws KDBException {
    argNotNull(contract, "KeySet 'contract'");
    return openInternal(contract);
  }

  /**
   * Opens KDB session
   *
   * @param contract Contract configuring the {@code gopts} plugin
   * @return New KDB session
   * @throws KDBException if opening the session fails - see specializations of {@link KDBException}
   * @throws IllegalStateException if {@code contract} has already been released
   */
  @Nonnull
  private static KDB openInternal(@Nullable KeySet contract) throws KDBException {
    var errorKey = Key.create();
    var session =
        checkKDBPointer(
            Elektra.INSTANCE.elektraKdbOpen(
                contract == null ? null : contract.getPointer(), errorKey.getPointer()),
            errorKey);

    // errorKey is being released if no KDBException occurred
    errorKey.release();

    return session;
  }

  /**
   * Opens a new KDB session
   *
   * @param warningsKey Used to store warnings, which may occur during opening the session, in this
   *     key's meta data
   * @return New KDB session
   * @throws KDBException if opening the session fails - see specialization of {@link KDBException}
   * @throws IllegalStateException if {@code warningsKey} has already been released
   * @throws IllegalArgumentException if {@code warningsKey} is {@code null}
   * @see Key#create()
   */
  @Nonnull
  public static KDB open(Key warningsKey) throws KDBException {
    argNotNull(warningsKey, "Key 'warningsKey'");
    return checkKDBPointer(Elektra.INSTANCE.elektraKdbOpen(null, warningsKey.getPointer()), warningsKey);
  }

  /**
   * Opens KDB session using the specified {@code contract}
   *
   * @param contract Contract configuring the {@code gopts} plugin
   * @param warningsKey Used to store warnings, which may occur during opening the session, in this
   *     key's meta data
   * @return New KDB session
   * @throws KDBException if opening the session fails - see specialization of {@link KDBException}
   * @throws IllegalStateException if {@code contract} or {@code warningsKey} has already been
   *     released
   * @throws IllegalArgumentException if {@code contract} or {@code warningsKey} is {@code null}
   * @see #goptsContract(String[], String[], Key, KeySet)
   * @see #goptsContract(KeySet, String[], String[], Key, KeySet)
   */
  @Nonnull
  public static KDB open(KeySet contract, Key warningsKey) throws KDBException {
    argNotNull(contract, "KeySet 'contract'");
    argNotNull(warningsKey, "Key 'warningsKey'");
    return checkKDBPointer(
        Elektra.INSTANCE.elektraKdbOpen(contract.getPointer(), warningsKey.getPointer()), warningsKey);
  }

  @Nonnull
  private static KDB checkKDBPointer(@Nullable Pointer pointer, Key errorKey) throws KDBException {
    if (pointer == null) {
      throw KDBException.getMappedException(errorKey);
    }
    return new KDB(pointer);
  }

  /**
   * Closes the KDB session and frees native resources associated with it
   *
   * @throws KDBException if opening the session fails - see specialization of {@link KDBException}
   * @throws KDBClosedException if this session has already been closed
   */
  @Override
  public void close() throws KDBException {
    var errorKey = Key.create();
    close(errorKey);

    // errorKey is being released if no KDBException occurred
    errorKey.release();
  }

  /**
   * Closes the KDB session and frees native resources associated with it
   *
   * @param warningsKey Used to store warnings, which may occur during closing the session, in this
   *     key's meta data
   * @throws KDBException if opening the session fails - see specialization of {@link KDBException}
   * @throws KDBClosedException if this session has already been closed
   * @throws IllegalStateException if {@code parentKey} has already been released
   * @throws IllegalArgumentException if {@code warningsKey} is {@code null}
   * @see Key#create()
   */
  public void close(Key warningsKey) throws KDBException {
    argNotNull(warningsKey, "Key 'warningsKey'");
    if (Elektra.INSTANCE.elektraKdbClose(getPointer(), warningsKey.getPointer()) != 0) {
      throw KDBException.getMappedException(warningsKey);
    }
    pointer = null;
  }

  /**
   * Creates a contract {@link KeySet} for use with {@link KDB#open(KeySet)} that mounts and
   * configures the {@code gopts} plugin
   *
   * @param args Arguments that will be converted into {@code argc} and {@code argv} for {@code
   *     gopts}
   * @param env Environment variables that {@code gopts} will use
   * @param parentKey Parent key that should be used by {@code gopts}. Only the key name is copied.
   *     The key can be deleted immediately after calling this function.
   * @param goptsConfig Config used for mounting the {@code gopts} plugin
   * @return New {@link KeySet} containing the contract
   * @throws IllegalArgumentException if any of the arguments are {@code null}
   * @throws IllegalStateException if {@code goptsConfig} or {@code parentKey} has already been
   *     released
   * @throws IllegalArgumentException if any of the specified parameters is {@code null}
   */
  public static KeySet goptsContract(
      String[] args, String[] env, Key parentKey, KeySet goptsConfig) {
    var keySet = KeySet.create();
    goptsContract(keySet, args, env, parentKey, goptsConfig);
    return keySet;
  }

  /**
   * Writes a contract into a specified {@link KeySet} for use with {@link KDB#open(KeySet)} that
   * mounts and configures the {@code gopts} plugin
   *
   * @param contract Key set to write the contract to
   * @param args Arguments that will be converted into {@code argc} and {@code argv} for {@code
   *     gopts}
   * @param env Environment variables that {@code gopts} will use
   * @param parentKey Parent key that should be used by {@code gopts}. Only the key name is copied.
   *     The key can be deleted immediately after calling this function.
   * @param goptsConfig Config used for mounting the {@code gopts} plugin
   * @throws IllegalArgumentException if any of the arguments are {@code null}
   * @throws IllegalStateException if {@code contract}, {@code goptsConfig} or {@code parentKey} has
   *     already been released
   * @throws IllegalArgumentException if any of the specified parameters is {@code null}
   */
  public static void goptsContract(
      KeySet contract, String[] args, String[] env, Key parentKey, KeySet goptsConfig) {
    argNotNull(contract, "KeySet 'contract'");
    argNotNull(args, "String[] 'args'");
    argNotNull(env, "String[] 'env'");
    argNotNull(parentKey, "Key 'parentKey'");
    argNotNull(goptsConfig, "KeySet 'goptsConfig'");
    var argsString = compactStringArray(args);
    var envString = compactStringArray(env);

    Elektra.INSTANCE.elektraGOptsContractFromStrings(
        contract.getPointer(),
        argsString.length(),
        argsString,
        envString.length(),
        envString,
        parentKey.getPointer(),
        goptsConfig.getPointer());
  }

  @Nonnull
  private static String compactStringArray(String[] array) {
    var builder = new StringBuilder();
    for (String string : array) {
      builder.append(string).append('\0');
    }
    return builder.toString();
  }

  /**
   * Constructor associating a new {@link KDB} instance with a JNA pointer
   *
   * @param pointer JNA {@link Pointer} to KDB
   * @throws IllegalArgumentException if {@code pointer} is {@code null}
   */
  private KDB(Pointer pointer) {
    argNotNull(pointer, "Pointer 'pointer'");
    this.pointer = pointer;
  }

  /**
   * Fetches at least all keys that are sub-keys or children of sub-keys of the supplied parent key
   *
   * <p>Note: Resulting key set may contain more keys than requested
   *
   * @param parentKey Root key which name is used to fetch keys below. This key is also used to
   *     store warnings, which may occur during the operation, in this key's meta data.
   * @return New {@link KeySet} containing the fetched keys
   * @throws KDBException if loading keys fails - see specialization of {@link KDBException}
   * @throws KDBClosedException if this session has already been closed
   * @throws IllegalStateException if {@code parentKey} has already been released
   * @throws IllegalArgumentException {@code parentKey} is {@code null}
   */
  @Nonnull
  public KeySet get(Key parentKey) throws KDBException {
    var keySet = KeySet.create();
    get(keySet, parentKey);
    return keySet;
  }

  /**
   * Fetches at least all keys that are sub-keys or children of sub-keys of the supplied parent key
   *
   * <p>Note: Resulting key set may contain more keys than requested
   *
   * @param keySet {@link KeySet} used to store the fetched keys
   * @param parentKey Root key which name is used to fetch keys below it. This key is also used to
   *     store warnings, which may occur during the operation, in this key's meta data. It is
   *     recommended to use the most specific {@code parentKey} possible. (e.g. using {@code
   *     system:/} is rarely the most specific)
   * @return This {@link KDB} session, enabling a fluent interface
   * @throws KDBException if loading keys fails - see specialization of {@link KDBException}
   * @throws KDBClosedException if this session has already been closed
   * @throws IllegalStateException if {@code keySet} or {@code parentKey} has already been released
   * @throws IllegalArgumentException if {@code keySet} or {@code parentKey} is {@code null}
   * @see #get(Key)
   */
  public KDB get(KeySet keySet, Key parentKey) throws KDBException {
    argNotNull(keySet, "KeySet 'keySet'");
    argNotNull(parentKey, "Key 'parentKey'");
    checkKDBReturnValue(
        Elektra.INSTANCE.elektraKdbGet(getPointer(), keySet.getPointer(), parentKey.getPointer()),
        parentKey);
    return this;
  }

  /**
   * Will update changed keys of the given {@code keySet} in the backend. {@link #get(Key)} or
   * {@link #get(KeySet, Key)} has to be called before this function may be executed.
   *
   * @param keySet KeySet which contains keys to be updated in the backend
   * @param parentKey Specify which part of the given {@code keySet} is of interest for you. This
   *     key is also used to store warnings, which may occur during the operation, in this key's
   *     meta data. In general it is highly recommended, that you use the same {@code parentKey}
   *     used to fetch the {@code keySet} with {@link #get(Key)} or {@link #get(KeySet, Key)}. You
   *     promise to only modify or remove keys below this key. All others would be passed back as
   *     they were retrieved by {@code keySet} with {@link #get(Key)}. Cascading keys (starting with
   *     {@code /}) will set the path in all namespaces. A nameless key as created by {@link
   *     Key#create()} will commit all changes in the {@code keySet}. This parameter is an
   *     optimization to only save keys of mountpoints affected by the specified {@code parentKey}.
   *     This does not necessarily mean that only changes to keys below that {@code parentKey} are
   *     saved.
   * @return This {@link KDB} session, enabling a fluent interface
   * @throws KDBException if storing keys fails - see specialization of {@link KDBException}
   * @throws KDBClosedException if this session has already been closed
   * @throws IllegalStateException if {@code keySet} or {@code parentKey} has already been released
   * @throws IllegalArgumentException if {@code keySet} or {@code parentKey} is {@code null}
   */
  public KDB set(KeySet keySet, Key parentKey) throws KDBException {
    argNotNull(keySet, "KeySet 'keySet'");
    argNotNull(parentKey, "Key 'parentKey'");
    checkKDBReturnValue(
        Elektra.INSTANCE.elektraKdbSet(getPointer(), keySet.getPointer(), parentKey.getPointer()),
        parentKey);
    return this;
  }

  private static void checkKDBReturnValue(int returnValue, Key errorKey) throws KDBException {
    if (returnValue < 0) {
      throw KDBException.getMappedException(errorKey);
    }
  }

  /**
   * @return JNA pointer to the native pointer for this key set
   * @throws KDBClosedException if this {@link KDB} session has already been closed
   */
  @Nonnull
  protected Pointer getPointer() {
    if (pointer == null) {
      throw new KDBClosedException();
    }
    return pointer;
  }
}
