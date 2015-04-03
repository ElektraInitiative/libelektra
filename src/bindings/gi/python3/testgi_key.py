import unittest
from gi.repository import GElektra as kdb

class Key(unittest.TestCase):
	def setUp(self):
		self.key = kdb.Key("user/foo/bar",
			kdb.KEY_VALUE, "value",
			kdb.KEY_META,  "by",    "manuel",
			kdb.KEY_META,  "owner", "myowner"
			)

		self.bkey = kdb.Key("system/bkey",
			kdb.KEY_VALUE, b"bvalue\0\0",
			kdb.KEY_END,
			kdb.KEY_META,  "lost", "lost"
			)

	def test_ctor(self):
		self.assertIsInstance(self.key, kdb.Key)
		self.assertIsInstance(self.bkey, kdb.Key)

		k = kdb.Key("/cascading/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isvalid())

		k = kdb.Key("spec/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isvalid())

		k = kdb.Key("proc/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isvalid())

		k = kdb.Key("dir/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isvalid())

		k = kdb.Key("user/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isvalid())

		k = kdb.Key("system/key")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isvalid())

		k = kdb.Key()
		self.assertIsInstance(k, kdb.Key)
		self.assertFalse(k.isvalid())

		k = kdb.Key("wrongname")
		self.assertIsInstance(k, kdb.Key)
		self.assertFalse(k.isvalid())

		k = kdb.Key("user/foo")
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isvalid())

		k = kdb.Key(self.key)
		self.assertIsInstance(k, kdb.Key)
		self.assertTrue(k.isvalid())

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
		self.assertEqual(self.bkey.value,    b"bvalue\0\0")
		self.assertEqual(self.bkey.basename, "bkey")
		self.assertEqual(self.bkey.fullname, "system/bkey")

		k = kdb.Key("user/key1", kdb.KEY_VALUE, "value")
		self.assertFalse(k.isbinary())
		self.assertIsNone(k.getmeta("binary"))

		k.name     = "system/key2"
		k.basename = "key3"
		k.value    = b"bvalue\0\0"
		self.assertEqual(k.name,  "system/key3")
		self.assertEqual(k.value, b"bvalue\0\0")
		self.assertTrue(k.isbinary())
		self.assertIsInstance(self.bkey.getmeta("binary"), kdb.Key)

		k = kdb.Key("user/key2")
		with self.assertRaises(kdb.KeyInvalidName):
			k.name = "foo"

	def test_functions(self):
		self.assertTrue(self.key.isuser())
		self.assertTrue(self.bkey.issystem())
		self.assertTrue(self.key.isstring())
		self.assertTrue(self.bkey.isbinary())
		self.assertTrue(self.key.isbelow(kdb.Key("user/foo")))

		k = kdb.Key("user/key1", kdb.KEY_VALUE, "value")
		self.assertEqual(k.get(), "value")
		k.set(b"bvalue\0\0")
		self.assertEqual(k.get(), b"bvalue\0\0")

	def test_meta(self):
		self.assertIsInstance(self.key.getmeta("owner"), kdb.Key)
		self.assertEqual(self.key.getmeta("owner").name,  "owner")
		self.assertEqual(self.key.getmeta("owner").value, "myowner")
		self.assertEqual(self.key.getmeta("by").value,    "manuel")

		self.assertFalse(self.key.hasmeta("doesnt_exist"))
		self.assertIsNone(self.key.getmeta("doesnt_exist"))
		self.assertTrue(bool(self.bkey.getmeta("binary")))
		self.assertIsNone(self.bkey.getmeta("owner"))

		k = kdb.Key("user/key1")
		k.setmeta("foo", "bar")
		self.assertEqual(k.getmeta("foo").value, "bar")

		self.assertEqual(sum(1 for _ in self.key.getmeta()),  2)
		self.assertEqual(sum(1 for _ in self.bkey.getmeta()), 1)

if __name__ == '__main__':
	unittest.main()
