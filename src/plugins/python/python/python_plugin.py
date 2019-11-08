import kdb

class ElektraPlugin(object):
	def __init__(self):
		pass

	def open(self, config, errorKey):
		print("[CLASS-PYTHON-1] open -->")
		return 0

	def get(self, returned, parentKey):
		print("[CLASS-PYTHON-1] get")
		if parentKey.name == 'user:/from_c':
			returned.append(kdb.Key("user:/from_python"))
		return 1

	def set(self, returned, parentKey):
		print("[CLASS-PYTHON-1] set")
		return 1

	def error(self, returned, parentKey):
		print("[CLASS-PYTHON-1] error")
		return 1

	def close(self, errorKey):
		print("[CLASS-PYTHON-1] <-- close")
		return 0
