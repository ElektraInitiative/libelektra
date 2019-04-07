/**
 * @file
 *
 * @brief Source for macaddr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "macaddr.h"

#include <kdbhelper.h>
#include <regex.h>

int checkRegex(const char *mac, const char *regexString) {
}

int validateMac(Key *key, Key *parentKey) {
    Key *metaKey = keyGetMeta(metaKey, "check/macaddr");
    if (!metaKey) return 1;

    const char *mac = keyString(key);

    const char *regexColon = "^([0-9A-Fa-f]{2}:){5}([0-9A-Fa-f]{2})$";
    const char *regexHyphen = "^([0-9A-Fa-f]{2}-){5}([0-9A-Fa-f]{2})$";
    const char *regexHyphenSingle = "^([0-9A-Fa-f]{6}-)([0-9A-Fa-f]{6})$";

    const char *regexStrings[] = {regexColon, regexHyphen, regexHyphenSingle};

    int ret;
    int i = 0;
    do {
        ret = checkRegex(mac, regexStrings[i++]);
    } while (ret == 0 || i < 3);


}

int elektraMacaddrOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED) {
    // plugin initialization logic
    // this function is optional

    return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMacaddrClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED) {
    // free all plugin resources and shut it down
    // this function is optional

    return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMacaddrGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey) {
    if (!elektraStrCmp(keyName(parentKey), "system/elektra/modules/macaddr")) {
        KeySet *contract =
                ksNew(30, keyNew("system/elektra/modules/macaddr", KEY_VALUE, "macaddr plugin waits for your orders",
                                 KEY_END),
                      keyNew("system/elektra/modules/macaddr/exports", KEY_END),
                      keyNew("system/elektra/modules/macaddr/exports/open", KEY_FUNC, elektraMacaddrOpen, KEY_END),
                      keyNew("system/elektra/modules/macaddr/exports/close", KEY_FUNC, elektraMacaddrClose, KEY_END),
                      keyNew("system/elektra/modules/macaddr/exports/get", KEY_FUNC, elektraMacaddrGet, KEY_END),
                      keyNew("system/elektra/modules/macaddr/exports/set", KEY_FUNC, elektraMacaddrSet, KEY_END),
                      keyNew("system/elektra/modules/macaddr/exports/error", KEY_FUNC, elektraMacaddrError, KEY_END),
                      keyNew("system/elektra/modules/macaddr/exports/checkconf", KEY_FUNC, elektraMacaddrCheckConfig,
                             KEY_END),

#include ELEKTRA_README

                      keyNew("system/elektra/modules/macaddr/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
                      KS_END);
        ksAppend(returned, contract);
        ksDel(contract);

        return ELEKTRA_PLUGIN_STATUS_SUCCESS;
    }
    // get all keys

    return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraMacaddrSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED) {
    // set all keys
    // this function is optional
    ksRewind(returned);
    Key *cur;
    while ((cur = ksNext(returned)) != NULL) {
        const Key *meta = keyGetMeta(cur, "check/macaddr");
        if (!meta) continue;
        int rc = validateMac(cur, parentKey);
        if (!rc) return ELEKTRA_PLUGIN_STATUS_ERROR;
    }

    return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraMacaddrError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED) {
    // handle errors (commit failed)
    // this function is optional

    return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMacaddrCheckConfig(Key *errorKey ELEKTRA_UNUSED, KeySet *conf ELEKTRA_UNUSED) {
    // validate plugin configuration
    // this function is optional

    return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin *ELEKTRA_PLUGIN_EXPORT {
    // clang-format off
    return elektraPluginExport("macaddr",
                               ELEKTRA_PLUGIN_OPEN, &elektraMacaddrOpen,
                               ELEKTRA_PLUGIN_CLOSE, &elektraMacaddrClose,
                               ELEKTRA_PLUGIN_GET, &elektraMacaddrGet,
                               ELEKTRA_PLUGIN_SET, &elektraMacaddrSet,
                               ELEKTRA_PLUGIN_ERROR, &elektraMacaddrError,
                               ELEKTRA_PLUGIN_END);
}
