package org.libelektra;

import com.sun.jna.Pointer;
import org.libelektra.exception.KDBException;
import org.libelektra.exception.mapper.ExceptionMapperService;

/**
 * Represents session with the Key database. Close after usage, or simply use a try-with-resources statement.
 */
public class KDB implements AutoCloseable
{

	private final Pointer kdb;

	/**
	 * Helper constructor for duplication by pointer
	 *
	 * @param p Pointer to another KDB object
	 */
	public KDB (final Pointer p)
	{
		kdb = p;
	}

	/**
	 * Basic constructor of KDB class<br>
	 * Opens KDB session with the given errorKey to write possible warning and error information to
	 *
	 * @param errorKey used to store warning and error information
	 * @return New KDB session object
	 * @throws KDBException TODO #3754 detailed exception description (inlcuding appropriate subtypes)
	 */
	public static KDB open (final Key errorKey) throws KDBException
	{
		Pointer kdb = Elektra.INSTANCE.kdbOpen (null, errorKey.get ());

		if (kdb == null)
		{
			throw ExceptionMapperService.getMappedException (errorKey);
		}

		return new KDB (kdb);
	}

	/**
	 * Basic constructor of KDB class<br>
	 * Opens KDB session with the given errorKey to write possible warning and error information to
	 *
	 * @param contract the contract that will be ensured by kdbOpen()
	 * @param errorKey used to store warning and error information
	 * @return New KDB session object
	 * @throws KDBException TODO #3754 detailed exception description (inlcuding appropriate subtypes)
	 */
	public static KDB open (final KeySet contract, final Key errorKey) throws KDBException
	{
		Pointer kdb = Elektra.INSTANCE.kdbOpen (contract.get (), errorKey.get ());

		if (kdb == null)
		{
			throw ExceptionMapperService.getMappedException (errorKey);
		}

		return new KDB (kdb);
	}

	/**
	 * Clean-up function initiating closing of the KDB session
	 * @throws KDBException TODO #3754 detailed exception description (inlcuding appropriate subtypes)
	 */
	@Override public void close () throws KDBException
	{
		final Key k = Key.create ("");
		close (k);
	}

	/*
	 * Wrapped methods
	 */

	/**
	 * Will fetch at least all keys that are sub-keys or children of sub-keys of the supplied parent key.
	 *
	 * @param ks        KeySet where the fetched keys will be stored in
	 * @param parentKey Root key which name will be used to fetch keys below it
	 * @throws KDBException In case of an error when loading keys
	 */
	public void get (final KeySet ks, final Key parentKey) throws KDBException
	{
		final int ret = Elektra.INSTANCE.kdbGet (kdb, ks.get (), parentKey.get ());
		if (ret == -1)
		{
			throw ExceptionMapperService.getMappedException (parentKey);
		}
	}

	/**
	 * Will update changed keys of the given keyset in the backend. get() has to be called before this function may be
	 * executed.
	 *
	 * @param ks        KeySet which contains keys to be updated in the backend
	 * @param parentKey Is used to add warnings and set an error, if necessary
	 * @throws KDBException In case of an error when storing keys
	 */
	public void set (final KeySet ks, final Key parentKey) throws KDBException
	{
		final int ret = Elektra.INSTANCE.kdbSet (kdb, ks.get (), parentKey.get ());
		if (ret == -1)
		{
			throw ExceptionMapperService.getMappedException (parentKey);
		}
	}

	/**
	 * Clean-up method that closes the KDB session
	 *
	 * @param parentKey Key holding error and warning information
	 * @throws KDBException TODO #3754 detailed exception description (inlcuding appropriate subtypes)
	 */
	public void close (final Key parentKey) throws KDBException
	{
		final int ret = Elektra.INSTANCE.kdbClose (kdb, parentKey.get ());

		if (ret == -1)
		{
			throw ExceptionMapperService.getMappedException (parentKey);
		}
	}

	/**
	 * Creates a contract for use with {@link KDB#open} that mounts and configures the gopts plugin
	 *
	 * @param contract    the KeySet into which the contract is written
	 * @param args        the arguments that will be converted into argc and argv for gopts
	 * @param env         the environment variables that gopts will use
	 * @param parentKey   the parent key that gopts will use
	 * @param goptsConfig the config KeySet used for mounting gopts
	 *
	 * @throws IllegalArgumentException if any of the arguments are null
	 */
	public static void goptsContract (final KeySet contract, final String[] args, final String[] env, final Key parentKey,
					  final KeySet goptsConfig)
	{
		if (contract == null || args == null || env == null || parentKey == null || goptsConfig == null)
		{
			throw new IllegalArgumentException ("all arguments must be non-null");
		}

		StringBuilder argsBuilder = new StringBuilder ();
		for (String arg : args)
		{
			argsBuilder.append (arg).append ('\0');
		}
		String argsString = argsBuilder.toString ();

		StringBuilder envBuilder = new StringBuilder ();
		for (String e : env)
		{
			envBuilder.append (e).append ('\0');
		}
		String envString = envBuilder.toString ();

		final int ret = Elektra.INSTANCE.elektraGOptsContractFromStrings (contract.get (), argsString.length (), argsString,
										  envString.length (), envString, parentKey.get (),
										  goptsConfig.get ());

		if (ret == -1)
		{
			throw new AssertionError ("all arguments must be non-null");
		}
	}

	/**
	 * Native pointer being used by JNA
	 *
	 * @return Native pointer object
	 */
	protected Pointer get ()
	{
		return kdb;
	}
}
