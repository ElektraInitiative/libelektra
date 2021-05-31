package org.libelektra;

import static org.libelektra.ValidationUtil.argNotNull;

import com.sun.jna.Pointer;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import org.libelektra.exception.KDBClosedException;
import org.libelektra.exception.KeyReleasedException;
import org.libelektra.exception.KeySetReleasedException;

/**
 * Represents a session with the Elektra key database
 *
 * @apiNote Close after usage, or simply use a try-with-resources statement
 */
public class KDB implements AutoCloseable
{

	private Pointer pointer;

	/**
	 * Opens a new KDB session
	 *
	 * @return New KDB session
	 * @throws KDBException             if opening the session fails - see
	 *                                  specialization of {@link KDBException}
	 * @throws KeyReleasedException     if {@code errorKey} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code errorKey} is {@code null}
	 */
	@Nonnull public static KDB open () throws KDBException
	{
		final Key errorKey = Key.create (Key.KEY_LOCAL_NAME);
		var session = checkKDBPointer (Elektra.INSTANCE.kdbOpen (null, errorKey.getPointer ()), errorKey);

		// errorKey is being released if no KDBException occurred
		errorKey.release ();

		return session;
	}

	/**
	 * Opens a new KDB session using the specified {@code errorKey} to store possible
	 * warnings and error information
	 *
	 * @param errorKey Used to store warnings and error information
	 * @return New KDB session
	 * @throws KDBException             if opening the session fails - see
	 *                                  specialization of {@link KDBException}
	 * @throws KeyReleasedException     if {@code errorKey} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code errorKey} is {@code null}
	 */
	@Nonnull public static KDB open (Key errorKey) throws KDBException
	{
		argNotNull (errorKey, "Key 'errorKey'");
		return checkKDBPointer (Elektra.INSTANCE.kdbOpen (null, errorKey.getPointer ()), errorKey);
	}

	/**
	 * Opens KDB session using the specified {@code errorKey} to store possible
	 * warnings and error information
	 *
	 * @param contract Contract that will be ensured by
	 *                 {@link Elektra#kdbOpen(Pointer, Pointer)}
	 * @param errorKey Used to store warnings and error information
	 * @return New KDB session
	 * @throws KDBException             if opening the session fails - see
	 *                                  specialization of {@link KDBException}
	 * @throws KeySetReleasedException  if {@code contract} has already been
	 *                                  released
	 * @throws KeyReleasedException     if {@code errorKey} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code contract} or {@code errorKey} is
	 *                                  {@code null}
	 */
	@Nonnull public static KDB open (KeySet contract, Key errorKey) throws KDBException
	{
		argNotNull (contract, "KeySet 'contract'");
		argNotNull (errorKey, "Key 'errorKey'");
		return checkKDBPointer (Elektra.INSTANCE.kdbOpen (contract.getPointer (), errorKey.getPointer ()), errorKey);
	}

	@Nonnull private static KDB checkKDBPointer (@Nullable Pointer pointer, Key errorKey) throws KDBException
	{
		if (pointer == null)
		{
			throw KDBException.getMappedException (errorKey);
		}
		return new KDB (pointer);
	}

	/**
	 * Creates a {@link KeySet contract} for use with {@link KDB#open(KeySet, Key)}
	 * that mounts and configures the {@code gopts} plugin
	 *
	 * @param contract    the KeySet into which the contract is written
	 * @param args        the arguments that will be converted into argc and argv
	 *                    for gopts
	 * @param env         the environment variables that gopts will use
	 * @param parentKey   the parent key that gopts will use
	 * @param goptsConfig the config KeySet used for mounting gopts
	 * @throws IllegalArgumentException if any of the arguments are {@code null}
	 * @throws KeySetReleasedException  if {@code contract} or {@code goptsConfig}
	 *                                  has already been released
	 * @throws KeyReleasedException     if {@code parentKey} has already been
	 *                                  released
	 * @throws IllegalArgumentException if any of the specified parameters is
	 *                                  {@code null}
	 */
	public static void goptsContract (KeySet contract, String[] args, String[] env, Key parentKey, KeySet goptsConfig)
	{
		argNotNull (contract, "KeySet 'contract'");
		argNotNull (args, "String[] 'args'");
		argNotNull (env, "String[] 'env'");
		argNotNull (parentKey, "Key 'parentKey'");
		argNotNull (goptsConfig, "KeySet 'goptsConfig'");
		String argsString = compactStringArray (args);
		String envString = compactStringArray (env);

		Elektra.INSTANCE.elektraGOptsContractFromStrings (contract.getPointer (), argsString.length (), argsString,
								  envString.length (), envString, parentKey.getPointer (),
								  goptsConfig.getPointer ());
	}

	@Nonnull private static String compactStringArray (String[] array)
	{
		StringBuilder builder = new StringBuilder ();
		for (String string : array)
		{
			builder.append (string).append ('\0');
		}
		return builder.toString ();
	}

	/**
	 * Constructor associating a new {@link KDB} instance with a JNA pointer
	 *
	 * @param pointer JNA {@link Pointer} to KDB
	 * @throws IllegalArgumentException if {@code pointer} is {@code null}
	 */
	private KDB (Pointer pointer)
	{
		argNotNull (pointer, "Pointer 'pointer'");
		this.pointer = pointer;
	}

	/**
	 * Fetches at least all keys that are sub-keys or children of sub-keys of the
	 * supplied parent key
	 *
	 * @param parentKey Root key which name is used to fetch keys below it
	 * @return New {@link KeySet} containing the fetched keys
	 * @throws KDBException             if loading keys fails - see specialization
	 *                                  of {@link KDBException}
	 * @throws KDBClosedException       if this session has already been closed
	 * @throws KeyReleasedException     if {@code parentKey} has already been
	 *                                  released
	 * @throws IllegalArgumentException {@code parentKey} is {@code null}
	 * @see KeySet#release()
	 */
	@Nonnull public KeySet get (Key parentKey) throws KDBException
	{
		var keySet = KeySet.create ();
		get (keySet, parentKey);
		return keySet;
	}

	/**
	 * Fetches at least all keys that are sub-keys or children of sub-keys of the
	 * supplied parent key
	 *
	 * @param keySet    {@link KeySet} used to store the fetched keys
	 * @param parentKey Root key which name is used to fetch keys below it
	 * @throws KDBException             if loading keys fails - see specialization
	 *                                  of {@link KDBException}
	 * @throws KDBClosedException       if this session has already been closed
	 * @throws KeySetReleasedException  if {@code keySet} has already been released
	 * @throws KeyReleasedException     if {@code parentKey} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code keySet} or {@code parentKey} is
	 *                                  {@code null}
	 */
	public void get (KeySet keySet, Key parentKey) throws KDBException
	{
		argNotNull (keySet, "KeySet 'keySet'");
		argNotNull (parentKey, "Key 'parentKey'");
		checkKDBReturnValue (Elektra.INSTANCE.kdbGet (getPointer (), keySet.getPointer (), parentKey.getPointer ()), parentKey);
	}

	/**
	 * Will update changed keys of the given key set in the backend. get() has to be
	 * called before this function may be executed.
	 *
	 * @param keySet   KeySet which contains keys to be updated in the backend
	 * @param errorKey Used to store warnings and error information
	 * @return This {@link KDB} session, enabling a fluent interface
	 * @throws KDBException            if storing keys fails - see specialization of
	 *                                 {@link KDBException}
	 * @throws KDBClosedException      if this session has already been closed
	 * @throws KeySetReleasedException if {@code keySet} has already been released
	 * @throws KeyReleasedException    if {@code errorKey} has already been released
	 */
	public KDB set (KeySet keySet, Key errorKey) throws KDBException
	{
		checkKDBReturnValue (Elektra.INSTANCE.kdbSet (getPointer (), keySet.getPointer (), errorKey.getPointer ()), errorKey);
		return this;
	}

	@Nonnull private static void checkKDBReturnValue (int returnValue, Key errorKey) throws KDBException
	{
		if (returnValue < 0)
		{
			throw KDBException.getMappedException (errorKey);
		}
	}

	/**
	 * Closes the KDB session and frees native resources associated with it
	 *
	 * @throws KDBException       if opening the session fails - see specialization
	 *                            of {@link KDBException}
	 * @throws KDBClosedException if this session has already been closed
	 */
	@Override public void close () throws KDBException
	{
		final Key errorKey = Key.create (Key.KEY_LOCAL_NAME);
		close (errorKey);

		// errorKey is being released if no KDBException occurred
		errorKey.release ();
	}

	/**
	 * Closes the KDB session and frees native resources associated with it
	 *
	 * @param errorKey Key holding error and warning information
	 * @throws KDBException             if opening the session fails - see
	 *                                  specialization of {@link KDBException}
	 * @throws KDBClosedException       if this session has already been closed
	 * @throws KeyReleasedException     if {@code parentKey} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code errorKey} is {@code null}
	 */
	public void close (Key errorKey) throws KDBException
	{
		argNotNull (errorKey, "Key 'errorKey'");
		if (Elektra.INSTANCE.kdbClose (getPointer (), errorKey.getPointer ()) != 0)
		{
			throw KDBException.getMappedException (errorKey);
		}
		pointer = null;
	}

	/**
	 * @return JNA pointer to the native pointer for this key set
	 * @throws KDBClosedException if this {@link KDB} session has already been
	 *                            closed
	 */
	@Nonnull protected Pointer getPointer ()
	{
		if (pointer == null)
		{
			throw new KDBClosedException ();
		}
		return pointer;
	}
}
