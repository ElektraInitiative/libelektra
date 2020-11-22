import kdb
import configparser

class ElektraPlugin(object):
	def __init__(self):
		pass

	def open(self, config, errorKey):
		print("[CLASS-PYTHON-C] open")
		return 0

	def get(self, returned, parentKey):
		print("[CLASS-PYTHON-C] get")
		mod = "system:/elektra/modules/python"
		if parentKey.name == mod:
			returned.append(kdb.Key(mod, kdb.KEY_VALUE, "contract below"))
			returned.append(kdb.Key(mod+"/infos", kdb.KEY_VALUE, "contract below"))
			returned.append(kdb.Key(mod+"/infos/provides",   kdb.KEY_VALUE, "storage"))
			returned.append(kdb.Key(mod+"/infos/placements", kdb.KEY_VALUE, "getstorage setstorage"))
			return 1
		config = configparser.ConfigParser()
		config.readfp(open(parentKey.value))
		for s in config.sections():
			for o in config.options(s):
				returned.append(kdb.Key(parentKey.name+"/"+s+"/"+o, kdb.KEY_VALUE, config.get(s, o)))
		return 1

	def set(self, returned, parentKey):
		print("[CLASS-PYTHON-C] set")
		return 1

	def error(self, returned, parentKey):
		print("[CLASS-PYTHON-C] error")
		return 1

	def close(self, errorKey):
		print("[CLASS-PYTHON-C] <-- close")
		return 0
