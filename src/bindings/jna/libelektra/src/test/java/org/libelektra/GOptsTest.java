package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.libelektra.Elektra.KeyNewArgumentFlags.KEY_META;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.libelektra.exception.KDBException;

public class GOptsTest
{

	private static final String BASE_KEY = "/tests/java/gopts";
	private static final String SPEC_BASE_KEY = "spec:" + BASE_KEY;

	@Before public void setupSpec () throws KDBException
	{
		KeySet spec = KeySet.create (
			10, Key.create (SPEC_BASE_KEY, KEY_META, "command", ""),
			Key.create (SPEC_BASE_KEY + "/printversion", KEY_META, "description",
				    "print version information and exit (ignoring all other options/commands/parameters)", KEY_META, "opt",
				    "v", KEY_META, "opt/arg", "none", KEY_META, "opt/long", "version"),
			Key.create (SPEC_BASE_KEY + "/getter", KEY_META, "description", "get a key's value", KEY_META, "command", "get"),
			Key.create (SPEC_BASE_KEY + "/getter/verbose", KEY_META, "description",
				    "print additional information about where the value comes from", KEY_META, "opt", "v", KEY_META,
				    "opt/long", "verbose", KEY_META, "opt/arg", "none"),
			Key.create (SPEC_BASE_KEY + "/getter/keyname", KEY_META, "description", "name of the key to read", KEY_META, "args",
				    "indexed", KEY_META, "args/index", "0"),
			Key.create (SPEC_BASE_KEY + "/setter", KEY_META, "description", "set a key's value", KEY_META, "command", "set"),
			Key.create (SPEC_BASE_KEY + "/setter/verbose", KEY_META, "description",
				    "print additional information about where the value will be stored", KEY_META, "opt", "v", KEY_META,
				    "opt/long", "verbose", KEY_META, "opt/arg", "none"),
			Key.create (SPEC_BASE_KEY + "/setter/keyname", KEY_META, "description", "name of the key to write", KEY_META,
				    "args", "indexed", KEY_META, "args/index", "0"),
			Key.create (SPEC_BASE_KEY + "/setter/value", KEY_META, "description", "value to be written", KEY_META, "args",
				    "indexed", KEY_META, "args/index", "1"),
			Key.create (SPEC_BASE_KEY + "/dynamic/#", KEY_META, "description", "dynamically call a user-supplied command",
				    KEY_META, "args", "remaining"));

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

			assertTrue (ks.lookup (BASE_KEY).isPresent ());
			assertEquals (ks.lookup (BASE_KEY).get ().getString (), "getter");
			assertTrue (ks.lookup (BASE_KEY + "/getter/keyname").isPresent ());
			assertEquals (ks.lookup (BASE_KEY + "/getter/keyname").get ().getString (), "user:/");
			assertTrue (ks.lookup (BASE_KEY + "/getter/verbose").isPresent ());
			assertEquals (ks.lookup (BASE_KEY + "/getter/verbose").get ().getString (), "1");
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
