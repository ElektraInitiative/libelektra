package org.libelektra;

import com.sun.jna.Callback;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import java.util.ArrayList;
import java.util.List;
import org.libelektra.exception.InstallationException;

/** This class can be used to load native Elektra plugins to be used by Java directly */
public class NativePlugin implements Plugin {

  private ElektraPlugin elektraPlugin;

  // TODO shouldn't natove plugin be closable: Elektra.INSTANCE.elektraPluginOpen
  // TODO has Plugin::open to be called for native plugin? and if not, because
  // elektra did that already, isn't Plugin the wrong interface
  // TODO is there a need for Java plugins directly being accessed via Java
  // binding and not through elektra? (don't think so - @markus2330 also don't
  // thinks so -> scheduled for removal)

  /**
   * Constructor for loading an Elektra plugin
   *
   * @param pluginName The plugin name
   * @param errorKey The errorKey
   * @param modules TODO #3754 add parameter description
   * @throws InstallationException if the plugin does not exist
   * @throws IllegalStateException if {@code modules} or {@code errorKey} has already been released
   */
  public NativePlugin(String pluginName, Key errorKey, KeySet modules)
      throws InstallationException {
    KeySet config = KeySet.create();
    elektraPlugin =
        Elektra.INSTANCE.elektraPluginOpen(
            pluginName, modules.getPointer(), config.getPointer(), errorKey.getPointer());
    if (elektraPlugin == null) {
      Key temporaryError = Key.create("user:/temporary/errorkey");
      temporaryError.setMeta("error/number", InstallationException.ERROR_NUMBER);
      temporaryError.setMeta(
          "error/reason", String.format("I could not find plugin '%s'", pluginName));
      throw new InstallationException(temporaryError);
    }
  }

  /**
   * Constructor for loading an Elektra plugin
   *
   * @param pluginName The plugin name
   * @param errorKey The errorKey
   * @param config TODO #3754 add parameter description and update other
   * @param modules TODO #3754 add parameter description and update other
   * @throws IllegalStateException if {@code modules}, {@code config} or {@code errorKey} has
   *     already been released
   */
  public NativePlugin(String pluginName, KeySet modules, KeySet config, Key errorKey) {
    elektraPlugin =
        Elektra.INSTANCE.elektraPluginOpen(
            pluginName, modules.getPointer(), config.getPointer(), errorKey.getPointer());
  }

  /**
   * Gets the config which was used to configure the plugin
   *
   * @return A KeySet containing the configuration of the plugin
   */
  public KeySet getConfig() {
    return new KeySet(elektraPlugin.config);
  }

  @Override
  public int open(KeySet conf, Key errorKey) {
    return kdbOpen(errorKey);
  }

  /**
   * Opens the session with the KeyDatabase
   *
   * @param errorKey must be a valid key, e.g. created with Key.create()
   * @return 0 if success or -1 otherwise
   * @throws IllegalStateException if {@code errorKey} has already been released
   */
  public int kdbOpen(Key errorKey) {
    return elektraPlugin.kdbOpen.invoke(elektraPlugin, errorKey.getPointer());
  }

  /**
   * Closes the session with the Key database.
   *
   * @param errorKey must be a valid key, e.g. created with Key.create()
   * @return 0 if success or -1 otherwise
   */
  @Override
  public int close(Key errorKey) {
    return elektraPlugin.kdbClose.invoke(elektraPlugin, errorKey.getPointer());
  }

  /**
   * Lets the plugin transform the given KeySet
   *
   * @param keySet The KeySet to transform
   * @param errorKey must be a valid key, e.g. created with Key.create()
   * @return 0 if success or -1 otherwise
   * @throws KDBException if return value was -1
   * @throws IllegalStateException if {@code keySet} or {@code errorKey} has already been released
   */
  @Override
  public int set(KeySet keySet, Key errorKey) throws KDBException {
    // TODO #3171 since internal cursor is not yet removed, we have to rewind it,
    // even if we already removed it from {@code KeySet} API
    Elektra.INSTANCE.ksRewind(keySet.getPointer());
    int returnValue =
        elektraPlugin.kdbSet.invoke(elektraPlugin, keySet.getPointer(), errorKey.getPointer());
    if (returnValue == -1) {
      throw KDBException.getMappedException(errorKey);
    }
    return returnValue;
  }

  /**
   * Writes into the given KeySet in the parameter
   *
   * @param keySet The KeySet you want returned
   * @param errorKey must be a valid key, e.g. created with Key.create()
   * @return 0 if success or -1 otherwise
   * @throws KDBException if return value was -1
   * @throws IllegalStateException if {@code keySet} or {@code errorKey} has already been released
   */
  @Override
  public int get(KeySet keySet, Key errorKey) throws KDBException {
    // TODO #3171 since internal cursor is not yet removed, we have to rewind it,
    // even if we already removed it from {@code KeySet} API
    Elektra.INSTANCE.ksRewind(keySet.getPointer());
    int returnValue =
        elektraPlugin.kdbGet.invoke(elektraPlugin, keySet.getPointer(), errorKey.getPointer());
    if (returnValue == -1) {
      throw KDBException.getMappedException(errorKey);
    }
    return returnValue;
  }

  /**
   * Called in case an error happened
   *
   * @param keySet The affected KeySet
   * @param errorKey must be a valid key, e.g. created with Key.create() and contains error
   *     information
   * @return 0 if success or -1 otherwise
   * @throws IllegalStateException if {@code keySet} or {@code errorKey} has already been released
   */
  @Override
  public int error(KeySet keySet, Key errorKey) {
    // TODO #3171 since internal cursor is not yet removed, we have to rewind it,
    // even if we already removed it from {@code KeySet} API
    Elektra.INSTANCE.ksRewind(keySet.getPointer());
    return elektraPlugin.kdbError.invoke(elektraPlugin, keySet.getPointer(), errorKey.getPointer());
  }

  /**
   * Returns the plugin name
   *
   * @return plugin name
   */
  @Override
  public String getName() {
    return elektraPlugin.name;
  }

  public static class ElektraPlugin extends Structure {

    public interface KdbOpen extends Callback {
      int invoke(ElektraPlugin elektraPlugin, Pointer errorKey);
    }

    public interface KdbClose extends Callback {
      int invoke(ElektraPlugin elektraPlugin, Pointer errorKey);
    }

    public interface KdbGet extends Callback {
      int invoke(ElektraPlugin handle, Pointer returned, Pointer parentKey);
    }

    public interface KdbInit extends Callback {
      int invoke(ElektraPlugin handle, Pointer returned, Pointer parentKey);
    }

    public interface KdbSet extends Callback {
      int invoke(ElektraPlugin handle, Pointer returned, Pointer parentKey);
    }

    public interface KdbError extends Callback {
      int invoke(ElektraPlugin handle, Pointer returned, Pointer parentKey);
    }

    public interface KdbCommit extends Callback {
      int invoke(ElektraPlugin handle, Pointer returned, Pointer parentKey);
    }

    public Pointer config;
    public KdbOpen kdbOpen;
    public KdbClose kdbClose;
    public KdbInit kdbInit;
    public KdbGet kdbGet;
    public KdbSet kdbSet;
    public KdbError kdbError;
    public KdbCommit kdbCommit;
    public String name;

    @Override
    protected List<String> getFieldOrder() {
      List<String> list = new ArrayList<>();
      list.add("config");
      list.add("kdbOpen");
      list.add("kdbClose");
      list.add("kdbInit");
      list.add("kdbGet");
      list.add("kdbSet");
      list.add("kdbError");
      list.add("kdbCommit");
      list.add("name");
      return list;
    }
  }
}
