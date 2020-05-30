package org.libelektra.app;

import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;
import org.libelektra.KDB;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.exception.KDBException;

class App
{

	private final static String MOUNT_SPACE = "user:/";
	private final static String KEY_PREFIX = MOUNT_SPACE + "/sw/clock/central/";

	public static void main (String[] args)
	{
		System.out.println ("Example started");
		Map<String, String> stringStringMap = loadConfigurationSettings ();
		System.out.println ("Result:");
		System.out.println (stringStringMap);
		System.out.println ("Example terminated");
		// do whatever you need with the value starting from here
	}

	private static Map<String, String> loadConfigurationSettings ()
	{
		// all keys we want to retrieve
		String[] keys = new String[] {
			"server/port",
			"spring/profiles/active",
			"spring/datasource/type",
			"spring/datasource/url",
			"spring/datasource/username",
			"spring/datasource/password",
			"spring/jpa/database-platform",
			"spring/jpa/database",
			"spring/mail/host",
			"spring/mail/port",
			"jhipster/mail/from",
			"jhipster/mail/base-url",
		};
		// read the keys
		return readKeys (keys);
	}

	private static Map<String, String> readKeys (String[] keys)
	{
		System.out.println ("Reading following keys:");
		for (String key : keys)
		{
			System.out.println (key);
		}
		// create a key without specifying a name, which is allowed
		Key key = Key.create ("");
		// open KDB with autoclose functionality
		// keep in mind this is an expensive operation, avoid calling it too frequently
		try (KDB kdb = KDB.open (key))
		{
			// create keyset
			KeySet keySet = KeySet.create ();
			// set mount space
			kdb.get (keySet, Key.create (MOUNT_SPACE));
			// fetch values
			return Arrays.stream (keys)
				.map (k -> {
					Key lookedUpValue = keySet.lookup (KEY_PREFIX + k);
					// if null (not found), set it to null value
					// if specification with default value is applied, this actually should never happen
					return new String[] { k, (lookedUpValue != null) ? lookedUpValue.getString () : null };
				})
				// collect to map
				.collect (Collectors.toMap (keyValue -> keyValue[0], keyValue -> keyValue[1]));
		}
		catch (KDBException e)
		{
			// a problem occured with kdb.get(keySet, Key.create(MOUNT_SPACE));
			e.printStackTrace ();
		}
		return null;
	}
}
