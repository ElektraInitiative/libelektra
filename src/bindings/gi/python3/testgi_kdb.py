import unittest
from gi.repository import GElektra as kdb

class Constants(unittest.TestCase):
	def setUp(self):
		pass

	def test_kdbconfig_h(self):
		self.assertIsInstance(kdb.DB_SYSTEM, str)
		self.assertIsInstance(kdb.DB_USER,   str)
		self.assertIsInstance(kdb.DB_HOME,   str)
		self.assertIsInstance(kdb.DEBUG,     int)
		self.assertIsInstance(kdb.VERBOSE,   int)

	def test_kdb_h(self):
		self.assertIsInstance(kdb.VERSION,       str)
		self.assertIsInstance(kdb.VERSION_MAJOR, int)
		self.assertIsInstance(kdb.VERSION_MINOR, int)
		self.assertIsInstance(kdb.VERSION_MICRO, int)
		self.assertIsNone(kdb.KS_END)

class KDB(unittest.TestCase):
	def setUp(self):
		pass

	'''
	disabled until elektra libraries can be loaded by
	the build system (and static build is gone)

	def test_ctor(self):
		self.assertIsInstance(kdb.KDB(), kdb.KDB)
		error = kdb.Key()
		self.assertIsInstance(kdb.KDB(error), kdb.KDB)

	def test_get(self):
		with kdb.KDB() as db:
			ks = kdb.KeySet()
			db.get(ks, "system/elektra")

			key = ks["system/elektra/version/constants/KDB_VERSION"]
			self.assertEqual(key.value, kdb.VERSION)

	def test_set(self):
		with kdb.KDB() as db:
			ks = kdb.KeySet(100)
			db.get(ks, "user/MyApp")

			try:
				key = ks["user/MyApp/mykey"]
			except KeyError:
				key = kdb.Key("user/MyApp/mykey")
				ks.append(key)
			key.value = "new_value"

			db.set(ks, "user/MyApp")

		with kdb.KDB() as db:
			ks = kdb.KeySet(100)
			db.get(ks, "user/MyApp")
			self.assertEqual(ks["user/MyApp/mykey"].value, "new_value")
	'''

if __name__ == '__main__':
	unittest.main()
