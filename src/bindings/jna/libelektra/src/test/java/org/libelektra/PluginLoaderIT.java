package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.libelektra.exception.InstallationException;

public class PluginLoaderIT {

  private PluginLoader pluginLoader;

  @Before
  public void setup() {
    pluginLoader = new PluginLoader();
  }

  @Test(expected = InstallationException.class)
  public void loadNonExistingElektraPluginTwice_shouldThrowException() throws Exception {
    pluginLoader.loadElektraPlugin("Does not exist");
  }

  @Test
  public void loadDumpElektraPluginTwice_shouldWorkCorrectly() throws Exception {
    Plugin plugin1 = pluginLoader.loadElektraPlugin("dump");
    Plugin plugin2 = pluginLoader.loadElektraPlugin("dump");

    assertEquals(plugin1, plugin2);
  }

  @Test
  public void loadDumpElektraPlugin_shouldWorkCorrectly() throws Exception {
    Plugin plugin = pluginLoader.loadElektraPlugin("dump");

    assertTrue(plugin instanceof NativePlugin);
  }
}
