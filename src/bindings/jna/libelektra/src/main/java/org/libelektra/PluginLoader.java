package org.libelektra;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.libelektra.exception.InstallationException;
import org.libelektra.plugin.Echo;
import org.libelektra.plugin.PropertiesStorage;
import org.libelektra.plugin.Return;

/**
 * This class can be used to load Plugins from Elektra.
 * It also loads self implemented Java plugins.
 */
public class PluginLoader
{

	private Key errorKey;
	private final Map<String, Plugin> loadedJavaPlugins;
	private final Map<String, Plugin> loadedElektraPlugins;
	private KeySet modules;

	/**
	 * Instantiates a new PluginLoader with the possibility to add a custom error key
	 * @param errorKey The custom error key
	 */
	public PluginLoader (Key errorKey)
	{
		this.loadedElektraPlugins = new ConcurrentHashMap<> ();
		this.loadedJavaPlugins = new ConcurrentHashMap<> ();
		this.errorKey = errorKey;
	}

	/**
	 * Instantiates a new PluginLoader with a default error key which is empty
	 */
	public PluginLoader ()
	{
		this.loadedElektraPlugins = new ConcurrentHashMap<> ();
		this.loadedJavaPlugins = new ConcurrentHashMap<> ();
		this.errorKey = Key.create (Key.KEY_LOCAL_NAME);
		modules = KeySet.create ();
	}

	/**
	 * This plugin loads a Java Plugin.
	 *
	 * @param name the plugin name
	 * @return the Plugin
	 * @throws InstallationException if the plugin does not exist
	 */
	public Plugin loadJavaPlugin (String name) throws InstallationException
	{
		if (loadedJavaPlugins.containsKey (name))
		{
			return loadedJavaPlugins.get (name);
		}
		Plugin plugin = null;
		if (name.equals (Echo.PLUGIN_NAME))
		{
			plugin = new Echo ();
		}
		if (name.equals (PropertiesStorage.PLUGIN_NAME))
		{
			plugin = new PropertiesStorage ();
		}
		if (name.equals (Return.PLUGIN_NAME))
		{
			plugin = new Return ();
		}
		if (plugin != null)
		{
			loadedJavaPlugins.put (name, plugin);
			return plugin;
		}

		Key error = Key.create (Key.KEY_LOCAL_NAME);
		error.setMeta ("error/number", InstallationException.ERROR_NUMBER);
		error.setMeta ("error/reason", String.format ("I could not find java plugin '%s'", name));
		throw new InstallationException (error);
	}

	/**
	 * This plugin loads a Native Elektra Plugin.
	 *
	 * @param name the plugin name
	 * @return the Plugin
	 * @throws InstallationException if the plugin does not exist
	 */
	public Plugin loadElektraPlugin (String name) throws InstallationException
	{
		if (loadedElektraPlugins.containsKey (name))
		{
			return loadedElektraPlugins.get (name);
		}
		Plugin plugin = new NativePlugin (name, errorKey, modules);
		loadedElektraPlugins.put (name, plugin);
		return plugin;
	}
}
