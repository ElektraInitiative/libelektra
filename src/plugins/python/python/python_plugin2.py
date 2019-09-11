class ElektraPlugin(object):
	def __init__(self):
		self.x = 1

	def open(self, config, errorKey):
		print("[CLASS-PYTHON-2] open -->")
		self.x = self.x + 1
		return 1

	def get(self, returned, parentKey):
		print("[CLASS-PYTHON-2] get")
		return 1

	def set(self, returned, parentKey):
		print("[CLASS-PYTHON-2] set")
		return 1

	def error(self, returned, parentKey):
		print("[CLASS-PYTHON-2] error")
		return 1

	def close(self, errorKey):
		print("[CLASS-PYTHON-2] <-- close")
		return 0
