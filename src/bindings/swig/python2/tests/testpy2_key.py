import kdb, unittest

class Key(unittest.TestCase):
	def setUp(self):
		self.key = kdb.Key("user/foo/bar",
			kdb.KEY_VALUE, "value",
			kdb.KEY_META,  "by",    "manuel",
			kdb.KEY_META,  "owner", "myowner"
			)

		self.bkey = kdb.Key("system/bkey",
			kdb.KEY_FLAGS, kdb.KEY_BINARY,
			kdb.KEY_VALUE, "bvalue\0\0",
			kdb.KEY_END,
			kdb.KEY_META,  "lost", "lost"
			)

	def test_ctor(self):
		self.assertIsInstance(self.key, kdb.Key)
		self.assertIsInstance(self.bkey, kdb.Key)

		k = kdb.Key("/cascading/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

		k = kdb.Key("spec/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

		k = kdb.Key("proc/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

		k = kdb.Key("dir/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

		k = kdb.Key("user/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

		k = kdb.Key("system/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

		k = kdb.Key()
		self.assertIsInstance(k, kdb.Key)
		self.assertFalse(k.isValid())

		k = kdb.Key("wrongname")
		self.assertIsInstance(k, kdb.Key)
		self.assertFalse(k.isValid())

		k = kdb.Key("user/foo")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

		k = kdb.Key(self.key)
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

		k = kdb.Key(self.key.dup())
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())
		self.assertEqual(k, self.key)
		k.name = "user/copied"
		self.assertNotEqual(k, self.key)

	def test_operator(self):
		self.assertNotEqual(self.key, self.bkey)
		self.assertEqual(kdb.Key(self.key), self.key)
		self.assertEqual(self.key, kdb.Key("user/foo/bar", kdb.KEY_META, "owner", "myowner"))
		self.assertEqual(kdb.Key(), kdb.Key())
		self.assertNotEqual(kdb.Key("user/key1"), kdb.Key("user/key2"))

		self.assertTrue(kdb.Key("user/key1") == kdb.Key("user/key1"))
		self.assertTrue(kdb.Key("user/key1") != kdb.Key("user/key2"))
		self.assertTrue(kdb.Key("user/key1") <  kdb.Key("user/key2"))
		self.assertTrue(kdb.Key("user/key1") <= kdb.Key("user/key2"))
		self.assertTrue(kdb.Key("user/key2") >  kdb.Key("user/key1"))
		self.assertTrue(kdb.Key("user/key2") >= kdb.Key("user/key1"))

		self.assertTrue(bool(self.key))
		self.assertTrue(bool(self.bkey))

		self.assertEqual(str(self.key),  "user/foo/bar")
		self.assertEqual(str(self.bkey), "system/bkey")

	def test_properties(self):
		self.assertEqual(self.key.name,      "user/foo/bar")
		self.assertEqual(self.key.value,     "value")
		self.assertEqual(self.key.basename,  "bar")
		self.assertEqual(self.key.fullname,  "user:myowner/foo/bar")

		self.assertEqual(self.bkey.name,     "system/bkey")
		self.assertEqual(self.bkey.value,    "bvalue\0\0")
		self.assertEqual(self.bkey.basename, "bkey")
		self.assertEqual(self.bkey.fullname, "system/bkey")

		k = kdb.Key("user/key1", kdb.KEY_VALUE, "value")
		self.assertFalse(k.isBinary())
		self.assertIsNone(k.getMeta("binary"))

		k.name     = "system/key2"
		k.basename = "key3"
		k.binary   = "bvalue\0\0"
		self.assertEqual(k.name,  "system/key3")
		self.assertEqual(k.value, "bvalue\0\0")
		self.assertTrue(k.isBinary())
		self.assertIsInstance(self.bkey.getMeta("binary"), kdb.Key)

		k = kdb.Key("user/key2")
		with self.assertRaises(kdb.KeyInvalidName):
			k.name = "foo"

	def test_functions(self):
		self.assertTrue(self.key.isUser())
		self.assertTrue(self.bkey.isSystem())
		self.assertTrue(self.key.isString())
		self.assertTrue(self.bkey.isBinary())
		self.assertTrue(self.key.isBelow(kdb.Key("user/foo")))

		k = kdb.Key("user/key1", kdb.KEY_VALUE, "value")
		self.assertEqual(k.get(), "value")

	def test_meta(self):
		self.assertIsInstance(self.key.getMeta("owner"), kdb.Key)
		self.assertEqual(self.key.getMeta("owner").name,  "owner")
		self.assertEqual(self.key.getMeta("owner").value, "myowner")
		self.assertEqual(self.key.getMeta("by").value,    "manuel")

		self.assertFalse(self.key.hasMeta("doesnt_exist"))
		self.assertIsNone(self.key.getMeta("doesnt_exist"))
		self.assertTrue(bool(self.bkey.getMeta("binary")))
		self.assertIsNone(self.bkey.getMeta("owner"))

		k = kdb.Key("user/key1")
		k.setMeta("foo", "bar")
		self.assertEqual(k.getMeta("foo").value, "bar")

		self.assertEqual(sum(1 for _ in self.key.getMeta()),  2)
		self.assertEqual(sum(1 for _ in self.bkey.getMeta()), 1)

	def test_python_copy(self):
		import copy
		k = copy.copy(self.key)
		self.assertEqual(k, self.key)
		k.name = "user/copied"
		self.assertNotEqual(k, self.key)

if __name__ == '__main__':
	unittest.main()
