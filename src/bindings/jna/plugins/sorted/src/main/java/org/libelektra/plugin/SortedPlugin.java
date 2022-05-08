package org.libelektra.plugin;

import org.libelektra.KDBException;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

import javax.annotation.Nonnull;
import java.util.regex.Pattern;

public class SortedPlugin implements Plugin {

    private static final String PLUGIN_NAME = "Sorted";
    private static final Pattern META_SORTED = Pattern.compile("meta:/check/sorted");
    private static final Pattern META_SORTED_DIRECTION = Pattern.compile("meta:/check/sorted/direction");

    private enum Direction {
        ASC, DESC
    }

    @Nonnull
    @Override
    public String getName() {
        return PLUGIN_NAME;
    }

    @Override
    public int open(KeySet config, Key errorKey) {
        throw new UnsupportedOperationException();
    }

    @Override
    public int get(KeySet keySet, Key parentKey) throws KDBException {
        if (isPluginMetadataRequested(parentKey)) {
            SortedMetadata.appendAllTo(keySet);
        }

        // todo add validation and possibly a warning

        return STATUS_SUCCESS;
    }

    private boolean isPluginMetadataRequested(Key parentKey) {
        return parentKey.isBelowOrSame(Key.create(PROCESS_CONTRACT_ROOT));
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
