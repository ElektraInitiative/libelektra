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

	public PluginLoader(Key errorKey) {
		this.errorKey = errorKey;
	}

	/**
	 * This plugin loads a Java/Native Elektra Plugin.
	 *
	 * @param name the plugin name
	 * @return the Plugin
	 * @throws InstallationException if the plugin does not exist
	 */
	public Plugin loadPlugin(String name) throws InstallationException {
		if (name.equals(new Echo().getName())) {
			return new Echo();
		}
		if (name.equals(new PropertiesStorage().getName())) {
			return new PropertiesStorage();
		}
		if (name.equals(new Return().getName())) {
			return new Return();
		}
		return new NativePlugin(name, errorKey);
	}
}
