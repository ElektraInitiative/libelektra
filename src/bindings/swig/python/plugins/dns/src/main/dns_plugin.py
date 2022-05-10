import kdb
import socket


"""
Usage:
> sudo kdb mount file.ini /python python script=/path/to/filter_script.py
> sudo kdb meta-set user:/python/my_hostname check/dns ''
> sudo kdb set user:/python/my_hostname www.libelektra.org
"""

META_DNS_NAME = "check/dns"


def get_ipv4_by_hostname(hostname) -> bool:
	return bool([
		i[4][0]  # address
		for i in socket.getaddrinfo(hostname, 0)
		if i[0] is socket.AddressFamily.AF_INET and i[1] is socket.SocketKind.SOCK_RAW
	])


def check_key(key: kdb.Key):
	print(key, key.value)
	# we only check if Meta META_DNS_NAME is set
	if m := key.getMeta(META_DNS_NAME):
		print(m.value)
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
			returned.append(kdb.Key(mod+"/infos", kdb.KEY_VALUE, "contract below"))
			returned.append(kdb.Key(mod+"/infos/placements", kdb.KEY_VALUE, "postgetstorage presetstorage"))
			return 1
		return 1

	def set(self, returned: kdb.KeySet, parentKey: kdb.Key):
		"""
		#  - nil or 1 : on success
		#           0 : on success with no changed keys in database
		#          -1 : failure
		"""

		for k in returned:
			if not check_key(k):
				print(f"couldn't validate key {k}")
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