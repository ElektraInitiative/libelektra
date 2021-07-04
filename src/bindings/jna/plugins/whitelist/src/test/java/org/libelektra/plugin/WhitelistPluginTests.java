package org.libelektra.plugin;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.libelektra.Plugin.JNI_MODULE_CONTRACT_ROOT;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.libelektra.KDBException;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;
import org.libelektra.exception.SemanticValidationException;

public class WhitelistPluginTests
{

	private Plugin plugin;

	@Before public void setup ()
	{
		plugin = new WhitelistPlugin ();
		plugin.open (plugin.getConfig (), Key.createNameless ());
	}

	@After public void tearDown ()
	{
		plugin.close (Key.createNameless ());
	}

	@Test public void test_getJniModuleContractRoot_shouldPass () throws KDBException
	{
		var key = Key.create (JNI_MODULE_CONTRACT_ROOT);
		var keySet = KeySet.create ();
		int result = plugin.get (keySet, key);
		var infoProvides = keySet.lookup (JNI_MODULE_CONTRACT_ROOT + "/infos/provides").map (Key::getString).orElseThrow ();
		var infoPlacements = keySet.lookup (JNI_MODULE_CONTRACT_ROOT + "/infos/placements").map (Key::getString).orElseThrow ();
		var infoMetdata = keySet.lookup (JNI_MODULE_CONTRACT_ROOT + "/infos/metadata").map (Key::getString).orElseThrow ();

		assertEquals (Plugin.STATUS_SUCCESS, result);
		assertTrue (infoProvides.contains ("check"));
		assertTrue (infoPlacements.contains ("presetstorage"));
		assertTrue (infoMetdata.contains ("check/whitelist/#"));
	}

	@Test public void test_get_shouldPass () throws KDBException
	{
		int result = plugin.get (KeySet.create (), Key.createNameless ());

		assertEquals (Plugin.STATUS_NO_UPDATE, result);
	}

	@Test public void test_setInvalid_shouldPass () throws KDBException
	{
		var key = addSpecMetaData (Key.createNameless ()).setString ("not-allowed");
		int result = plugin.set (KeySet.create (), key);
		var errorNumber = key.getMeta ("error/number").map (Key::getString).orElseThrow ();

		assertEquals (Plugin.STATUS_ERROR, result);
		assertEquals (SemanticValidationException.ERROR_NUMBER, errorNumber);
	}

	@Test public void test_setValid_shouldPass () throws KDBException
	{
		Key key;
		int result;
		key = addSpecMetaData (Key.createNameless ()).setString ("allowed0");
		result = plugin.set (KeySet.create (), key);

		assertEquals (Plugin.STATUS_SUCCESS, result);
		assertTrue (key.getMeta ("error/number").isEmpty ());

		key = addSpecMetaData (Key.createNameless ()).setString ("allowed1");
		result = plugin.set (KeySet.create (), key);

		assertEquals (Plugin.STATUS_SUCCESS, result);
		assertTrue (key.getMeta ("error/number").isEmpty ());

		key = addSpecMetaData (Key.createNameless ()).setString ("allowed3");
		result = plugin.set (KeySet.create (), key);

		assertEquals (Plugin.STATUS_SUCCESS, result);
		assertTrue (key.getMeta ("error/number").isEmpty ());
	}

	private Key addSpecMetaData (Key key)
	{
		key.setMeta ("check/whitelist/#0", "allowed0");
		key.setMeta ("check/whitelist/#1", "allowed1");
		key.setMeta ("check/whitelist/#100", "allowed3");
		return key;
	}
}
