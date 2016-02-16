class ElektraPlugin(object):
	def open(self, config, errorKey):
		return 0

	def get(self, returned, parentKey):
		return -1

	def set(self, returned, parentKey):
		return -1

	def error(self, returned, parentKey):
		return -1

	def close(self, errorKey):
		return 0
