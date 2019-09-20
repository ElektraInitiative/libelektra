package org.libelektra;

import org.libelektra.exception.InstallationException;
import org.libelektra.plugin.Echo;
import org.libelektra.plugin.NativePlugin;
import org.libelektra.plugin.PropertiesStorage;
import org.libelektra.plugin.Return;

/**
 * This class can be used to load Plugins from Elektra.
 * It also loads self implemented Java plugins.
 */
public class PluginLoader {

	private Key errorKey;

	/**
	 * Instantiates a new PluginLoader with the possibility to add a custom error key
	 * @param errorKey The custom error key
	 */
	public PluginLoader(Key errorKey) {
		this.errorKey = errorKey;
	}

	/**
	 * Instantiates a new PluginLoader with a default error key which is empty
	 */
	public PluginLoader() {
		this.errorKey = Key.create("");
	}

	/**
	 * This plugin loads a Java Plugin.
	 *
	 * @param name the plugin name
	 * @return the Plugin
	 * @throws InstallationException if the plugin does not exist
	 */
	public Plugin loadJavaPlugin(String name) throws InstallationException {
		if (name.equals(Echo.PLUGIN_NAME)) {
			return new Echo();
		}
		if (name.equals(PropertiesStorage.PLUGIN_NAME)) {
			return new PropertiesStorage();
		}
		if (name.equals(Return.PLUGIN_NAME)) {
			return new Return();
		}
		Key error = Key.create("");
		error.setMeta("error/number", InstallationException.errorNumber());
		error.setMeta("error/reason", String.format("I could not find java plugin '%s'", name));
		throw new InstallationException(error);
	}

	/**
	 * This plugin loads a Native Elektra Plugin.
	 *
	 * @param name the plugin name
	 * @return the Plugin
	 * @throws InstallationException if the plugin does not exist
	 */
	public Plugin loadElektraPlugin(String name) throws InstallationException {
		return new NativePlugin(name, errorKey);
	}
}
