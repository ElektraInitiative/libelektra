package org.libelektra;

import static org.junit.Assert.*;

import java.util.Iterator;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.exception.KDBException;

public class GOptsTest
{

	private static final String BASE_KEY = "/tests/java/gopts";
	private static final String SPEC_BASE_KEY = "spec:" + BASE_KEY;

	private Key key, key2, parentKey;

	@Before public void setupSpec () throws KDBException
	{
		KeySet spec = KeySet.create (
			10, Key.create (SPEC_BASE_KEY, Key.KEY_META, "command", ""),
			Key.create (SPEC_BASE_KEY + "/printversion", Key.KEY_META, "description",
				    "print version information and exit (ignoring all other options/commands/parameters)", Key.KEY_META,
				    "opt", "v", Key.KEY_META, "opt/arg", "none", Key.KEY_META, "opt/long", "version"),
			Key.create (SPEC_BASE_KEY + "/getter", Key.KEY_META, "description", "get a key's value", Key.KEY_META, "command",
				    "get"),
			Key.create (SPEC_BASE_KEY + "/getter/verbose", Key.KEY_META, "description",
				    "print additional information about where the value comes from", Key.KEY_META, "opt", "v", Key.KEY_META,
				    "opt/long", "verbose", Key.KEY_META, "opt/arg", "none"),
			Key.create (SPEC_BASE_KEY + "/getter/keyname", Key.KEY_META, "description", "name of the key to read", Key.KEY_META,
				    "args", "indexed", Key.KEY_META, "args/index", "0"),
			Key.create (SPEC_BASE_KEY + "/setter", Key.KEY_META, "description", "set a key's value", Key.KEY_META, "command",
				    "set"),
			Key.create (SPEC_BASE_KEY + "/setter/verbose", Key.KEY_META, "description",
				    "print additional information about where the value will be stored", Key.KEY_META, "opt", "v",
				    Key.KEY_META, "opt/long", "verbose", Key.KEY_META, "opt/arg", "none"),
			Key.create (SPEC_BASE_KEY + "/setter/keyname", Key.KEY_META, "description", "name of the key to write",
				    Key.KEY_META, "args", "indexed", Key.KEY_META, "args/index", "0"),
			Key.create (SPEC_BASE_KEY + "/setter/value", Key.KEY_META, "description", "value to be written", Key.KEY_META,
				    "args", "indexed", Key.KEY_META, "args/index", "1"),
			Key.create (SPEC_BASE_KEY + "/dynamic/#", Key.KEY_META, "description", "dynamically call a user-supplied command",
				    Key.KEY_META, "args", "remaining"));

		Key specParent = Key.create (SPEC_BASE_KEY);
		try (final KDB kdb = KDB.open (specParent))
		{
			final KeySet ks = KeySet.create (10);
			kdb.get (ks, specParent);

			if (ks.cut (specParent).length () > 0)
			{
				throw new IllegalStateException ("Couldn't set up spec, keys exist!");
			}

			ks.append (spec);
			kdb.set (ks, specParent);
		}
	}

	@Test public void test_gopts () throws KDBException
	{
		String[] args = new String[] { "test", "get", "-v", "user:/" };
		String[] env = new String[0];

		KeySet config = KeySet.create (10);
		KeySet contract = KeySet.create (10);

		Key parentKey = Key.create (BASE_KEY);
		KDB.goptsContract (contract, args, env, parentKey, config);

		try (final KDB kdb = KDB.open (contract, parentKey))
		{
			KeySet ks = KeySet.create (10);

			kdb.get (ks, parentKey);

			assertFalse (ks.lookup (BASE_KEY).isNull ());
			assertEquals (ks.lookup (BASE_KEY).getString (), "getter");
			assertFalse (ks.lookup (BASE_KEY + "/getter/keyname").isNull ());
			assertEquals (ks.lookup (BASE_KEY + "/getter/keyname").getString (), "user:/");
			assertFalse (ks.lookup (BASE_KEY + "/getter/verbose").isNull ());
			assertEquals (ks.lookup (BASE_KEY + "/getter/verbose").getString (), "1");
		}
	}

	@After public void removeSpec () throws KDBException
	{
		Key specParent = Key.create (SPEC_BASE_KEY);
		try (final KDB kdb = KDB.open (specParent))
		{
			final KeySet ks = KeySet.create (10);
			kdb.get (ks, specParent);
			ks.cut (specParent);
			kdb.set (ks, specParent);
		}
	}
}
