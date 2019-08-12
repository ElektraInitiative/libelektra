package org.libelektra.plugin;

import com.sun.jna.Callback;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import org.libelektra.Elektra;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;
import org.libelektra.exception.InstallationException;
import org.libelektra.exception.KDBException;
import org.libelektra.exception.mapper.ExceptionMapperService;

import java.util.ArrayList;
import java.util.List;

/**
 * This class can be used to load native C/C++ written Elektra Plugins to be used by Java directly
 */
public class NativeElektraPlugin implements Plugin {

	private NativePlugin nativePlugin;

	public NativeElektraPlugin(String pluginName, Key errorKey) throws InstallationException {
		KeySet modules = KeySet.create();
		KeySet config = KeySet.create();
		nativePlugin = Elektra.INSTANCE.elektraPluginOpen(pluginName, modules.get(), config.get(), errorKey.get());
		if (nativePlugin == null) {
			Key temporaryError = Key.create("user/temporary/errorkey");
			temporaryError.setMeta("error/number", InstallationException.errorCode());
			temporaryError.setMeta("error/reason", String.format("I could not find plugin '%s'", pluginName));
			throw new InstallationException(temporaryError);
		}
	}

	public NativeElektraPlugin(String pluginName, KeySet modules, KeySet config, Key errorKey) {
		nativePlugin = Elektra.INSTANCE.elektraPluginOpen(pluginName, modules.get(), config.get(), errorKey.get());
	}

	/**
	 * Gets the config which was used to configure the plugin
	 * @return A KeySet containing the configuration of the plugin
	 */
	public KeySet getConfig() {
		return new KeySet(nativePlugin.config);
	}

	@Override
	public int kdbOpen(KeySet conf, Key errorKey) {
		return kdbOpen(errorKey);
	}

	/**
	 * Opens the session with the KeyDatabase
	 * @param errorKey must be a valid key, e.g. created with Key.create()
	 * @return 0 if success or -1 otherwise
	 */
	public int kdbOpen(Key errorKey) {
		return nativePlugin.kdbOpen.invoke(nativePlugin, errorKey.get());
	}

	/**
	 * Closes the session with the Key database.
	 * @param errorKey must be a valid key, e.g. created with Key.create()
	 * @return 0 if success or -1 otherwise
	 */
	public int kdbClose(Key errorKey) {
		return nativePlugin.kdbClose.invoke(nativePlugin, errorKey.get());
	}

	/**
	 * Lets the plugin transform the given KeySet
	 * @param keySet The KeySet to transform
	 * @param errorKey must be a valid key, e.g. created with Key.create()
	 * @return 0 if success or -1 otherwise
	 * @throws KDBException if return value was -1
	 */
	public int kdbSet(KeySet keySet, Key errorKey) throws KDBException {
		keySet.rewind();
		int returnValue = nativePlugin.kdbSet.invoke(nativePlugin, keySet.get(), errorKey.get());
		if (returnValue == -1) {
			throw ExceptionMapperService.getMappedException(errorKey);
		}
		return returnValue;
	}

	/**
	 * Writes into the given KeySet in the parameter
	 * @param keySet The KeySet you want returned
	 * @param errorKey must be a valid key, e.g. created with Key.create()
	 * @return 0 if success or -1 otherwise
	 * @throws KDBException if return value was -1
	 */
	public int kdbGet(KeySet keySet, Key errorKey) throws KDBException {
		keySet.rewind();
		int returnValue = nativePlugin.kdbGet.invoke(nativePlugin, keySet.get(), errorKey.get());
		if (returnValue == -1) {
			throw ExceptionMapperService.getMappedException(errorKey);
		}
		return returnValue;
	}

	/**
	 * Called in case an error happened
	 * @param keySet The affected KeySet
	 * @param errorKey ust be a valid key, e.g. created with Key.create() and contains error information
	 * @return 0 if success or -1 otherwise
	 */
	public int kdbError(KeySet keySet, Key errorKey) {
		keySet.rewind();
		return nativePlugin.kdbError.invoke(nativePlugin, keySet.get(), errorKey.get());
	}

	/**
	 * Returns the plugin name
	 * @return plugin name
	 */
	public String getName() {
		return nativePlugin.name;
	}

	public static class NativePlugin extends Structure {

		public interface KdbOpen extends Callback {
			int invoke(NativePlugin nativePlugin, Pointer errorKey);
		}

		public interface KdbClose extends Callback {
			int invoke(NativePlugin nativePlugin, Pointer errorKey);
		}

		public interface KdbGet extends Callback {
			int invoke(NativePlugin handle, Pointer returned, Pointer parentKey);
		}

		public interface KdbSet extends Callback {
			int invoke(NativePlugin handle, Pointer returned, Pointer parentKey);
		}

		public interface KdbError extends Callback {
			int invoke(NativePlugin handle, Pointer returned, Pointer parentKey);
		}

		public Pointer config;
		public KdbOpen kdbOpen;
		public KdbClose kdbClose;
		public KdbGet kdbGet;
		public KdbSet kdbSet;
		public KdbError kdbError;
		public String name;


		@Override
		protected List<String> getFieldOrder() {
			List<String> list = new ArrayList<>();
			list.add("config");
			list.add("kdbOpen");
			list.add("kdbClose");
			list.add("kdbGet");
			list.add("kdbSet");
			list.add("kdbError");
			list.add("name");
			return list;
		}
	}

}


