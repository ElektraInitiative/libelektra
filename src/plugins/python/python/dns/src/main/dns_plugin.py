import kdb
import socket

"""
Usage:
> sudo kdb mount file.ini /python python script=/path/to/dns_plugin.py
> kdb meta set user:/python/my_hostname check/dns ''
> kdb set user:/python/my_hostname www.libelektra.org
"""

META_DNS_NAME = "meta:/check/dns"


def get_ipv4_by_hostname(hostname) -> bool:
    return bool([
        i[4][0]  # address
        for i in socket.getaddrinfo(hostname, 0)
        if i[0] is socket.AddressFamily.AF_INET and i[1] is socket.SocketKind.SOCK_RAW
    ])


def check_key(key: kdb.Key):
    # we only check if Meta META_DNS_NAME is set
    if m := key.getMeta(META_DNS_NAME):
        if key.value != '':
            try:
                return get_ipv4_by_hostname(key.value)
            except Exception as e:
                return False

    return True


class ElektraPlugin(object):
    def __init__(self):
        pass

    def open(self, config: kdb.KeySet, errorKey):
        """
        returns:
        #  - nil or 0: no error
        #  - -1      : error during initialization
        """
        return 0

    def get(self, returned: kdb.KeySet, parentKey: kdb.Key):
        """
        #  - nil or 1 : on success
        #  -        0 : OK but nothing was to do
        #  -       -1 : failure
        """
        mod = "system:/elektra/modules/python"

        if parentKey.name == mod:
            returned.append(kdb.Key(mod, kdb.KEY_VALUE, "contract below"))
            returned.append(kdb.Key(mod + "/infos", kdb.KEY_VALUE, "contract below"))

            returned.append(kdb.Key(mod + "/infos/license", kdb.KEY_VALUE, "BSD"))
            returned.append(kdb.Key(mod + "/infos/provides", kdb.KEY_VALUE, "check"))
            returned.append(kdb.Key(mod + "/infos/status", kdb.KEY_VALUE, "maintained"))
            returned.append(kdb.Key(mod + "/infos/placements", kdb.KEY_VALUE, "postgetstorage presetstorage"))
            returned.append(kdb.Key(mod + "/infos/description", kdb.KEY_VALUE, "checks if name is resolvable"))
            return 1

        warning_list = []
        for k in returned:
            if not check_key(k):
                warning_list.append(k)
                print(f"Couldn't resolve domain name for key: {k}")

        if warning_list:
            parentKey.setMeta("warnings", str(len(warning_list)))
            c = 0
            for warn_key in warning_list:
                if c == 100:
                    c = 0
                if c > 9:
                    index = "#_" + str(c)
                else:
                    index = "#" + str(c)

                parentKey.setMeta(f"warnings/{index}/number", "C03200")
                parentKey.setMeta(f"warnings/{index}/description", "Validation Semantic")
                parentKey.setMeta(f"warnings/{index}/reason", f"Failed to resolve domain name for key {warn_key}")
                parentKey.setMeta(f"warnings/{index}/module", "python check/dns script")
                parentKey.setMeta(f"warnings/{index}/file", "unknown")
                parentKey.setMeta(f"warnings/{index}/line", "0")
                parentKey.setMeta(f"warnings/{index}/mountpoint", str(parentKey.name))
                parentKey.setMeta(f"warnings/{index}/configfile", str(parentKey.value))
                c += 1

            return -1

        return 1

    def set(self, returned: kdb.KeySet, parentKey: kdb.Key):
        """
        #  - nil or 1 : on success
        #           0 : on success with no changed keys in database
        #          -1 : failure
        """
        for k in returned:
            if not check_key(k):
                parentKey.setMeta("error", f"number description reason module")
                parentKey.setMeta("error/number", "C03200")
                parentKey.setMeta("error/description", "Validation Semantic")
                parentKey.setMeta("error/reason", f"Failed to resolve domain name for key {k}")
                parentKey.setMeta("error/module", "python check/dns script")
                parentKey.setMeta("error/file", "unknown")
                parentKey.setMeta("error/line", "0")
                parentKey.setMeta("error/mountpoint", str(parentKey.name))
                parentKey.setMeta("error/configfile", str(parentKey.value))
                print(f"Couldn't validate key {k}")
                return -1
        return 1

    def error(self, returned: kdb.KeySet, parentKey: kdb.Key):
        """
        #  - nil or 1 : on success
        #           0 : on success with no action
        #          -1 : failure
        """
        return 1

    def close(self, errorKey):
        return 0
