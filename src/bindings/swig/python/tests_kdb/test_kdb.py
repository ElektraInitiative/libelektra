import kdb, unittest

TEST_NS = "user:/tests/swig_py3"

class Constants(unittest.TestCase):
	def setUp(self):
		pass

	def test_kdbconfig_h(self):
		self.assertIsInstance(kdb.DB_SYSTEM, str)
		self.assertIsInstance(kdb.DB_USER,   str)
		self.assertIsInstance(kdb.DB_HOME,   str)
		self.assertIsInstance(kdb.DEBUG,     int)

	def test_kdb_h(self):
		self.assertIsInstance(kdb.VERSION,       str)
		self.assertIsInstance(kdb.VERSION_MAJOR, int)
		self.assertIsInstance(kdb.VERSION_MINOR, int)
		self.assertIsInstance(kdb.VERSION_PATCH, int)
		self.assertIsNone(kdb.KS_END)

class KDB(unittest.TestCase):
	def test_ctor(self):
		self.assertIsInstance(kdb.KDB(), kdb.KDB)
		error = kdb.Key()
		self.assertIsInstance(kdb.KDB(error), kdb.KDB)

	def test_get(self):
		with kdb.KDB() as db:
			ks = kdb.KeySet()
			db.get(ks, "system:/elektra")

			import os
			if os.getenv("CHECK_VERSION") is None:
				key = ks["system:/elektra/version/constants/KDB_VERSION"]
				self.assertEqual(key.value, kdb.VERSION)

	def test_set(self):
		with kdb.KDB() as db:
			ks = kdb.KeySet(100)
			db.get(ks, TEST_NS)

			try:
				key = ks[TEST_NS + "/mykey"]
			except KeyError:
				key = kdb.Key(TEST_NS + "/mykey")
				ks.append(key)
			key.value = "new_value"

			db.set(ks, TEST_NS)

		with kdb.KDB() as db:
			ks = kdb.KeySet(100)
			db.get(ks, TEST_NS)
			self.assertEqual(ks[TEST_NS + "/mykey"].value, "new_value")

	def test_diff(self):
		with kdb.KDB() as db:
			ks = kdb.KeySet(100)
			db.get(ks, TEST_NS)

			key = kdb.Key(TEST_NS + "/diffkey")
			key.value = "new_value"
			ks.append(key)

			diff: kdb.ElektraDiff = db.calculateChanges(ks, TEST_NS)

			self.assertFalse(diff.isEmpty())
			self.assertEqual(diff.getAddedKeys()[TEST_NS+"/diffkey"].value, "new_value")

	@classmethod
	def tearDownClass(cls):
		# cleanup
		with kdb.KDB() as db:
			ks = kdb.KeySet(100)
			db.get(ks, TEST_NS)
			ks.cut(kdb.Key(TEST_NS))
			db.set(ks, TEST_NS)

if __name__ == '__main__':
	unittest.main()
