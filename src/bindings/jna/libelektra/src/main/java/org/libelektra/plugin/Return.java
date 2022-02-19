package org.libelektra.plugin;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

public class Return implements Plugin {

  private static final String PLUGIN_NAME = "Return";

  @Override
  public int open(KeySet conf, Key errorKey) {
    return 0;
  }

  @Override
  public int get(KeySet keySet, Key parentKey) {
    return 10;
  }

  @Override
  public int set(KeySet keySet, Key parentKey) {
    return 20;
  }

  @Override
  public int error(KeySet keySet, Key parentKey) {
    return 30;
  }

  @Override
  public int close(Key parentKey) {
    return 0;
  }

  @Override
  public String getName() {
    return PLUGIN_NAME;
  }
}
