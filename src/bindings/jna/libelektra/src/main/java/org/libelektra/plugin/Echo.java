package org.libelektra.plugin;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

public class Echo implements Plugin {

  private static final String PLUGIN_NAME = "Echo";

  public Echo() {
    System.out.println("construct plugin");
  }

  @Override
  public int open(KeySet conf, Key errorKey) {
    System.out.println("open plugin");
    System.out.println(errorKey);
    System.out.println(errorKey.getString());
    System.out.println(conf);
    return STATUS_NO_UPDATE;
  }

  @Override
  public int get(KeySet keySet, Key parentKey) {
    System.out.println("get plugin");
    System.out.println(parentKey);
    System.out.println(parentKey.getString());
    System.out.println(keySet);
    var name = parentKey + "/infos/provides";
    System.out.println("name: " + name);
    keySet.append(Key.create(name, "java"));
    return STATUS_NO_UPDATE;
  }

  @Override
  public int set(KeySet keySet, Key parentKey) {
    System.out.println("set plugin");
    System.out.println(parentKey);
    System.out.println(parentKey.getString());
    System.out.println(keySet);
    return STATUS_NO_UPDATE;
  }

  @Override
  public int error(KeySet keySet, Key parentKey) {
    System.out.println("error plugin");
    System.out.println(parentKey);
    System.out.println(parentKey.getString());
    System.out.println(keySet);
    return STATUS_NO_UPDATE;
  }

  @Override
  public int close(Key parentKey) {
    System.out.println("close plugin");
    System.out.println(parentKey);
    System.out.println(parentKey.getString());
    return STATUS_NO_UPDATE;
  }

  @Override
  public String getName() {
    return PLUGIN_NAME;
  }
}
