package org.libelektra.plugin;

import org.junit.Before;
import org.junit.Test;
import org.libelektra.KDBException;
import org.libelektra.Plugin;

import static org.junit.Assert.*;
import static org.libelektra.plugin.TestData.*;

public class SortedPluginTest {

    private Plugin plugin;

    @Before
    public void setup() {
        plugin = new SortedPlugin();
    }

    @Test
    public void getContract_shouldReturnCorrectInformation() throws KDBException {
        // given
        var key = givenProcessContractKey();
        var keySet = givenEmptyKeySet();

        // when
        int result = plugin.get(keySet, key);

        // then
        assertEquals(Plugin.STATUS_SUCCESS, result);
        assertTrue(
                getContractString(keySet, "/infos/provides")
                        .contains("check")
        );
        assertTrue(
                getContractString(keySet, "/infos/placements")
                        .contains("presetstorage")
        );
        assertTrue(
                getContractString(keySet, "/infos/placements")
                        .contains("postgetstorage")
        );
        assertTrue(
                getContractString(keySet, "/infos/metadata")
                        .contains("check/sorted")
        );
        assertTrue(
                getContractString(keySet, "/infos/metadata")
                        .contains("check/sorted/direction")
        );
    }

    @Test
    public void givenASortedKeySet_whenSet_shouldReturnSuccess() throws KDBException {
        // given
        var keySet = givenASortedSetByPrimitiveInt();
        var key = givenParentKey();

        // when
        int result = plugin.set(keySet, key);

        // then
        assertEquals(Plugin.STATUS_SUCCESS, result);
    }

    @Test
    public void givenAnUnsortedKeySet_whenSet_shouldReturnErrorAndSetErrorMeta() throws KDBException {
        // given
        var keySet = givenAnUnsortedSetByPrimitiveInt();
        var key = givenParentKey();

        // when
        int result = plugin.set(keySet, key);

        // then
        assertEquals(Plugin.STATUS_ERROR, result);
        assertTrue(key.getMeta("error").isPresent());
    }

    @Test
    public void givenASortedKeySet_whenGet_shouldReturnSuccess() throws KDBException {
        // given
        var keySet = givenASortedSetByPrimitiveInt();
        var key = givenParentKey();

        // when
        int result = plugin.get(keySet, key);

        // then
        assertEquals(Plugin.STATUS_SUCCESS, result);
    }

    @Test
    public void givenAnUnsortedKeySet_whenGet_shouldReturnSuccessAndSetWarningMeta() throws KDBException {
        // given
        var keySet = givenAnUnsortedSetByPrimitiveInt();
        var key = givenParentKey();

        // when
        int result = plugin.get(keySet, key);

        // then
        assertEquals(Plugin.STATUS_SUCCESS, result);
        assertTrue(key.getMeta("warnings").isPresent());
    }
}
