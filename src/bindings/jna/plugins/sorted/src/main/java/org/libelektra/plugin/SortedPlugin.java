package org.libelektra.plugin;

import org.libelektra.KDBException;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

import javax.annotation.Nonnull;

public class SortedPlugin implements Plugin {
    @Nonnull
    @Override
    public String getName() {
        return null;
    }

    @Override
    public int open(KeySet config, Key errorKey) {
        throw new UnsupportedOperationException();
    }

    @Override
    public int get(KeySet keySet, Key parentKey) throws KDBException {
        return 0;
    }

    @Override
    public int set(KeySet keySet, Key parentKey) throws KDBException {
        return 0;
    }

    @Override
    public int error(KeySet keySet, Key parentKey) {
        throw new UnsupportedOperationException();
    }

    @Override
    public int close(Key parentKey) {
        throw new UnsupportedOperationException();
    }
}
