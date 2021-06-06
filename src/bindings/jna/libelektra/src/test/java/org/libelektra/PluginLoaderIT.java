package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.libelektra.exception.InstallationException;
import org.libelektra.plugin.Echo;
import org.libelektra.plugin.PropertiesStorage;
import org.libelektra.plugin.Return;

public class PluginLoaderIT
{

	private PluginLoader pluginLoader;

	@Before public void setup ()
	{
		pluginLoader = new PluginLoader ();
	}

	@Test public void loadEchoJavaPlugin_shouldWorkCorrectly () throws Exception
	{
		Plugin plugin = pluginLoader.loadJavaPlugin (Echo.PLUGIN_NAME);

		assertTrue (plugin instanceof Echo);
	}

	@Test public void loadReturnJavaPlugin_shouldWorkCorrectly () throws Exception
	{
		Plugin plugin = pluginLoader.loadJavaPlugin (Return.PLUGIN_NAME);

		assertTrue (plugin instanceof Return);
	}

	@Test public void loadPropertiesStorageJavaPlugin_shouldWorkCorrectly () throws Exception
	{
		Plugin plugin = pluginLoader.loadJavaPlugin (PropertiesStorage.PLUGIN_NAME);

		assertTrue (plugin instanceof PropertiesStorage);
	}

	@Test public void loadEchoJavaPluginTwice_shouldWorkCorrectly () throws Exception
	{
		Plugin plugin1 = pluginLoader.loadJavaPlugin (Echo.PLUGIN_NAME);
		Plugin plugin2 = pluginLoader.loadJavaPlugin (Echo.PLUGIN_NAME);

		assertEquals (plugin1, plugin2);
	}

	@Test (expected = InstallationException.class) public void loadNonExistingJavaPluginTwice_shouldThrowException () throws Exception
	{
		pluginLoader.loadJavaPlugin ("Does not exist");
	}

	@Test (expected = InstallationException.class)
	public void loadNonExistingElektraPluginTwice_shouldThrowException () throws Exception
	{
		pluginLoader.loadElektraPlugin ("Does not exist");
	}

	@Test public void loadDumpElektraPluginTwice_shouldWorkCorrectly () throws Exception
	{
		Plugin plugin1 = pluginLoader.loadElektraPlugin ("dump");
		Plugin plugin2 = pluginLoader.loadElektraPlugin ("dump");

		assertEquals (plugin1, plugin2);
	}

	@Test public void loadDumpElektraPlugin_shouldWorkCorrectly () throws Exception
	{
		Plugin plugin = pluginLoader.loadElektraPlugin ("dump");

		assertTrue (plugin instanceof NativePlugin);
	}
}