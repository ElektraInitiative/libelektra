import kdb

class ElektraPlugin(object):
	def __init__(self):
		pass

	def open(self, config, errorKey):
		return 0

	def get(self, returned, parentKey):
		mod = "system:/elektra/modules/python"
		if parentKey.name == mod:
			returned.append(kdb.Key(mod, kdb.KEY_VALUE, "contract below"))
			returned.append(kdb.Key(mod+"/infos", kdb.KEY_VALUE, "contract below"))
			returned.append(kdb.Key(mod+"/infos/placements", kdb.KEY_VALUE, "postgetstorage presetstorage"))
			return 1
		return 1

	def set(self, returned, parentKey):
		return 1

	def error(self, returned, parentKey):
		return 1

	def close(self, errorKey):
		return 0
