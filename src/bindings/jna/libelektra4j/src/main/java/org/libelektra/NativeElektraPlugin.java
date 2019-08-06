package org.libelektra;

import com.sun.jna.Callback;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import org.libelektra.exception.KDBException;
import org.libelektra.exception.mapper.ExceptionMapperService;

import java.util.ArrayList;
import java.util.List;


public class NativeElektraPlugin {

    private NativePlugin nativePlugin;

    public NativeElektraPlugin(String pluginName, Key errorKey) {
        KeySet modules = KeySet.create();
        KeySet config = KeySet.create();
        nativePlugin = Elektra.INSTANCE.elektraPluginOpen(pluginName, modules.get(), config.get(), errorKey.get());
    }

    public NativeElektraPlugin(String pluginName, KeySet modules, KeySet config, Key errorKey) {
        nativePlugin = Elektra.INSTANCE.elektraPluginOpen(pluginName, modules.get(), config.get(), errorKey.get());
    }

    public KeySet getConfig() {
        return new KeySet(nativePlugin.config);
    }

    public int kdbOpen(Key errorKey) {
        return nativePlugin.kdbOpen.invoke(nativePlugin, errorKey.get());
    }

    public int kdbClose(Key errorKey) {
        return nativePlugin.kdbClose.invoke(nativePlugin, errorKey.get());
    }

    public int kdbSet(KeySet keySet, Key errorKey) throws KDBException {
        keySet.rewind();
        int returnValue = nativePlugin.kdbSet.invoke(nativePlugin, keySet.get(), errorKey.get());
        if (returnValue == -1) {
            throw ExceptionMapperService.getMappedException(errorKey);
        }
        return returnValue;
    }

    public int kdbGet(KeySet keySet, Key errorKey) throws KDBException {
        keySet.rewind();
        int returnValue = nativePlugin.kdbGet.invoke(nativePlugin, keySet.get(), errorKey.get());
        if (returnValue == -1) {
            throw ExceptionMapperService.getMappedException(errorKey);
        }
        return returnValue;
    }

    public int kdbError(KeySet keySet, Key errorKey) {
        keySet.rewind();
        return nativePlugin.kdbError.invoke(nativePlugin, keySet.get(), errorKey.get());
    }

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


