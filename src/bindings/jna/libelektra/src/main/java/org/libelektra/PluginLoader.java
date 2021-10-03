package org.libelektra;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.libelektra.exception.InstallationException;

/** This class can be used to load plugins from Elektra. */
public class PluginLoader {

  private Key errorKey;
  private final Map<String, Plugin> loadedElektraPlugins;
  private KeySet modules;

  /**
   * Instantiates a new PluginLoader with the possibility to add a custom error key
   *
   * @param errorKey The custom error key
   */
  public PluginLoader(Key errorKey) {
    this.loadedElektraPlugins = new ConcurrentHashMap<>();
    this.errorKey = errorKey;
  }

  /** Instantiates a new PluginLoader with a default error key which is empty */
  public PluginLoader() {
    this.loadedElektraPlugins = new ConcurrentHashMap<>();
    this.errorKey = Key.create();
    modules = KeySet.create();
  }

  /**
   * This plugin loads a Native Elektra Plugin.
   *
   * @param name the plugin name
   * @return the Plugin
   * @throws InstallationException if the plugin does not exist
   */
  public Plugin loadElektraPlugin(String name) throws InstallationException {
    if (loadedElektraPlugins.containsKey(name)) {
      return loadedElektraPlugins.get(name);
    }
    Plugin plugin = new NativePlugin(name, errorKey, modules);
    loadedElektraPlugins.put(name, plugin);
    return plugin;
  }
}
