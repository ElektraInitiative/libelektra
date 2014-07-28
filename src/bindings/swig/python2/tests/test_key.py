from __future__ import print_function
import kdb, unittest

class Key(unittest.TestCase):
	def setUp(self):
		self.key = kdb.Key("user/key",
			kdb.KEY_NAME,    "user/foo/bar",
			kdb.KEY_VALUE,   "value",
			kdb.KEY_OWNER,   "myowner",
			kdb.KEY_COMMENT, "mycomment",
			kdb.KEY_UID,     "123",
			kdb.KEY_GID,     456,
			kdb.KEY_MODE,    0o644,
			kdb.KEY_ATIME,   123,
			kdb.KEY_MTIME,   "456",
			kdb.KEY_CTIME,   789,
			kdb.KEY_DIR,
			kdb.KEY_META,    "by", "manuel",
			kdb.KEY_NULL
			)

		self.bkey = kdb.Key("system/bkey",
			kdb.KEY_BINARY,
			kdb.KEY_VALUE,   "bvalue\0\0",
			kdb.KEY_END,
			kdb.KEY_OWNER,   "lost"
			)

	def test_ctor(self):
		self.assertIsInstance(self.key, kdb.Key)
		self.assertIsInstance(self.bkey, kdb.Key)

		k = kdb.Key()
		self.assertIsInstance(k, kdb.Key)
		self.assertFalse(k.isValid())

		k = kdb.Key("wrongname")
		self.assertIsInstance(k, kdb.Key)
		self.assertFalse(k.isValid())

		k = kdb.Key("/wrongname")
		self.assertIsInstance(k, kdb.Key)
		self.assertFalse(k.isValid())

		k = kdb.Key("user/foo")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

		k = kdb.Key(self.key)
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isValid())

	def test_operator(self):
		self.assertNotEqual(self.key, self.bkey)
		self.assertEqual(kdb.Key(self.key), self.key)
		self.assertEqual(self.key, kdb.Key("user/foo/bar", kdb.KEY_OWNER, "myowner"))
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
		self.assertEqual(self.key.dirname,   "user/foo")
		self.assertEqual(self.key.fullname,  "user:myowner/foo/bar")

		self.assertEqual(self.bkey.name,     "system/bkey")
		self.assertEqual(self.bkey.value,    "bvalue\0\0")
		self.assertEqual(self.bkey.basename, "bkey")
		self.assertEqual(self.bkey.dirname,  "system")
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
		self.assertEqual(self.key.getMeta("owner").value,   "myowner")
		self.assertEqual(self.key.getMeta("owner").name,    "owner")
		self.assertEqual(self.key.getMeta("comment").value, "mycomment")
		self.assertEqual(self.key.getMeta("uid").value,     "123")
		self.assertEqual(self.key.getMeta("gid").value,     "456")
		self.assertEqual(self.key.getMeta("mode").value,    "755")
		self.assertEqual(self.key.getMeta("atime").value,   "123")
		self.assertEqual(self.key.getMeta("mtime").value,   "456")
		self.assertEqual(self.key.getMeta("ctime").value,   "789")
		self.assertEqual(self.key.getMeta("by").value,      "manuel")

		self.assertFalse(self.key.hasMeta("doesnt_exist"))
		self.assertIsNone(self.key.getMeta("doesnt_exist"))
		self.assertTrue(bool(self.bkey.getMeta("binary")))
		self.assertIsNone(self.bkey.getMeta("owner"))

		k = kdb.Key("user/key1")
		k.setMeta("foo", "bar")
		self.assertEqual(k.getMeta("foo").value, "bar")

		self.assertEqual(sum(1 for _ in self.key.getMeta()),  9)
		self.assertEqual(sum(1 for _ in self.bkey.getMeta()), 1)

if __name__ == '__main__':
	unittest.main()
