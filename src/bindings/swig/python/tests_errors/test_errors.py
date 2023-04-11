import kdb
import kdb.errors
import unittest


class Errors(unittest.TestCase):
	def test__get_warnings__no_warnings__should_return_empty_array(self):
		# Arrange
		key = kdb.Key("/")

		# Act
		warnings = kdb.errors.get_warnings(key)

		# Assert
		assert warnings is not None
		assert len(warnings) == 0


	def test__get_warnings__should_return_warnings(self):
		# Arrange
		key = kdb.Key("/")
		key.setMeta("meta:/warnings", "")
		key.setMeta("meta:/warnings/#0", "")
		key.setMeta("meta:/warnings/#0/number", "1010")
		key.setMeta("meta:/warnings/#0/description", "the description 1")
		key.setMeta("meta:/warnings/#0/module", "the module 1")
		key.setMeta("meta:/warnings/#0/file", "the file 1")
		key.setMeta("meta:/warnings/#0/line", "123")
		key.setMeta("meta:/warnings/#0/mountpoint", "user:/mp1")
		key.setMeta("meta:/warnings/#0/configfile", "/home/root/file1.txt")
		key.setMeta("meta:/warnings/#0/reason", "this is only a test for the first warning")

		key.setMeta("meta:/warnings/#1", "")
		key.setMeta("meta:/warnings/#1/number", "2020")
		key.setMeta("meta:/warnings/#1/description", "the description 2")
		key.setMeta("meta:/warnings/#1/module", "the module 2")
		key.setMeta("meta:/warnings/#1/file", "the file 2")
		key.setMeta("meta:/warnings/#1/line", "456")
		key.setMeta("meta:/warnings/#1/mountpoint", "user:/mp2")
		key.setMeta("meta:/warnings/#1/configfile", "/home/root/file2.txt")
		key.setMeta("meta:/warnings/#1/reason", "this is only a test for the second warning")

		key.setMeta("meta:/warnings/#2", "")
		key.setMeta("meta:/warnings/#2/description", "this warning only has a description")

		# Act
		warnings: [kdb.errors.ElektraWarning] = kdb.errors.get_warnings(key)

		# Assert
		assert len(warnings) == 3

		w1: kdb.errors.ElektraWarning = warnings[0]
		assert w1.number == "1010"
		assert w1.description == "the description 1"
		assert w1.module == "the module 1"
		assert w1.file == "the file 1"
		assert w1.line == "123"
		assert w1.mountpoint == "user:/mp1"
		assert w1.configfile == "/home/root/file1.txt"
		assert w1.reason == "this is only a test for the first warning"

		w2: kdb.errors.ElektraWarning = warnings[1]
		assert w2.number == "2020"
		assert w2.description == "the description 2"
		assert w2.module == "the module 2"
		assert w2.file == "the file 2"
		assert w2.line == "456"
		assert w2.mountpoint == "user:/mp2"
		assert w2.configfile == "/home/root/file2.txt"
		assert w2.reason == "this is only a test for the second warning"

		w3: kdb.errors.ElektraWarning = warnings[2]
		assert w3.number == ""
		assert w3.description == "this warning only has a description"
		assert w3.module == ""
		assert w3.file == ""
		assert w3.line == ""
		assert w3.mountpoint == ""
		assert w3.configfile == ""
		assert w3.reason == ""

		# should still contain the warning metakeys
		assert key.getMeta("meta:/warnings/#0/line") is not None


	def test__get_error__no_error__should_return_none(self):
		# Arrange
		key = kdb.Key("/")

		# Act
		error = kdb.errors.get_error(key)

		# Assert
		assert error is None


	def test__get_error__should_return_error(self):
		# Arrange
		key = kdb.Key("/")
		key.setMeta("meta:/error", "")
		key.setMeta("meta:/error/number", "1010")
		key.setMeta("meta:/error/description", "the description 1")
		key.setMeta("meta:/error/module", "the module 1")
		key.setMeta("meta:/error/file", "the file 1")
		key.setMeta("meta:/error/line", "123")
		key.setMeta("meta:/error/mountpoint", "user:/mp1")
		key.setMeta("meta:/error/configfile", "/home/root/file1.txt")
		key.setMeta("meta:/error/reason", "this is only a test")

		# Act
		error = kdb.errors.get_error(key)

		# Assert
		assert error is not None
		assert error.number == "1010"
		assert error.description == "the description 1"
		assert error.module == "the module 1"
		assert error.file == "the file 1"
		assert error.line == "123"
		assert error.mountpoint == "user:/mp1"
		assert error.configfile == "/home/root/file1.txt"
		assert error.reason == "this is only a test"


if __name__ == '__main__':
	unittest.main()
