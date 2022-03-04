package org.libelektra.process;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import org.libelektra.Plugin;

/** Implements the child side of the protocol used by the process plugin. */
public class PluginProcess {

  public static void main(String[] args) {
    if (args.length != 1) {
      System.exit(1);
    }

    Class<Plugin> pluginClass;
    try {
      Class<?> clazz = Class.forName(args[0]);
      if (!Plugin.class.isAssignableFrom(clazz)) {
        System.exit(1);
        return;
      }
      //noinspection unchecked
      pluginClass = (Class<Plugin>) clazz;
    } catch (ClassNotFoundException e) {
      System.exit(1);
      return;
    }

    Plugin plugin;
    try {
      plugin = pluginClass.getConstructor().newInstance();
    } catch (InstantiationException
        | IllegalAccessException
        | InvocationTargetException
        | NoSuchMethodException e) {
      System.exit(1);
      return;
    }

    var protocol = new ProcessProtocol(plugin, System.in, System.out);

    try {
      if (!protocol.handshake()) {
        System.exit(1);
        return;
      }

      if (!protocol.run()) {
        System.exit(1);
      }
    } catch (IOException e) {
      System.exit(1);
    }
  }
}
