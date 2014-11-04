import unittest
from gi.repository import GElektra as kdb

class KeySet(unittest.TestCase):
	def setUp(self):
		self.ks = kdb.KeySet(100,
			kdb.Key("system/key1"),
			kdb.Key("system/key2"),
			kdb.Key("user/key3"),
			kdb.Key("user/key4"),
			kdb.KS_END,
			kdb.Key("user/lost")
			)

	def test_ctor(self):
		self.assertIsInstance(self.ks, kdb.KeySet)

		ks = kdb.KeySet(0)
		self.assertIsInstance(ks, kdb.KeySet)

		ks = kdb.KeySet(self.ks)
		self.assertIsInstance(ks, kdb.KeySet)

	def test_operator(self):
		self.assertEqual(len(self.ks),       4)
		self.assertEqual(len(kdb.KeySet(0)), 0)

		self.assertTrue(kdb.Key("user/key3") in self.ks)
		self.assertFalse(kdb.Key("user/foo") in self.ks)

		self.assertEqual(self.ks[0],  kdb.Key("system/key1"))
		self.assertEqual(self.ks[-1], kdb.Key("user/key4"))
		with self.assertRaises(IndexError):
			self.assertIsNone(self.ks[100])
		with self.assertRaises(IndexError):
			self.assertIsNone(self.ks[-100])

		self.assertEqual(self.ks[1:3], [ self.ks[1], self.ks[2] ])

		self.assertEqual(self.ks["user/key3"], kdb.Key("user/key3"))
		with self.assertRaises(KeyError):
			self.assertIsNone(self.ks["user/doesnt_exist"])

		self.assertEqual(self.ks[kdb.Key("system/key2")], kdb.Key("system/key2"))
		with self.assertRaises(KeyError):
			self.ks[kdb.Key("user/doesnt_exist")]

	def test_functions(self):
		self.assertEqual(self.ks.lookup("user/key3"), kdb.Key("user/key3"))
		self.assertEqual(self.ks.lookup(kdb.Key("system/key2")), kdb.Key("system/key2"))
		self.assertEqual(self.ks.lookup(0),  kdb.Key("system/key1"))
		self.assertEqual(self.ks.lookup(-1), kdb.Key("user/key4"))

		ks = kdb.KeySet(0)
		ks.append(kdb.Key("user/foo"))
		ks.append(kdb.Key("user/bar"))
		self.assertEqual(len(ks), 2)

		' test clear '
		ks = kdb.KeySet(0)
		ks.append(kdb.Key("user/test1"))
		ks.append(kdb.Key("user/test2"))
		ks.append(kdb.Key("user/test3"))
		ks.clear()
		self.assertEqual(len(ks), 0);

		' test pop '
		ks = kdb.KeySet(0)
		ks.append(kdb.Key("user/test1"))
		ks.append(kdb.Key("user/test2"))
		ks.append(kdb.Key("user/test3"))
		self.assertEqual(ks.pop(), kdb.Key("user/test3"))
		self.assertEqual(len(ks), 2)

		' test cut '
		ks = kdb.KeySet(0)
		ks.append(kdb.Key("user/level11"))
		ks.append(kdb.Key("user/level13"))
		ks.append(kdb.Key("user/level13/level21"))
		ks.append(kdb.Key("user/level13/level22/level31"))
		ks.append(kdb.Key("user/level13/level23"))
		ks.append(kdb.Key("user/level15"))

		cutresult = ks.cut(kdb.Key("user/level13"))

		self.assertEqual(len(cutresult), 4)
		self.assertTrue(kdb.Key("user/level13") in cutresult)
		self.assertTrue(kdb.Key("user/level13/level21") in cutresult)
		self.assertTrue(kdb.Key("user/level13/level22/level31") in cutresult)
		self.assertTrue(kdb.Key("user/level13/level23") in cutresult)

		self.assertEqual(len(ks), 2)
		self.assertTrue(kdb.Key("user/level11") in ks)
		self.assertTrue(kdb.Key("user/level15") in ks)

		cutresult = ks.cut(kdb.Key("user/does/not/exist"))
		self.assertEqual(len(cutresult), 0)
		self.assertEqual(len(ks), 2)

	def test_iterator(self):
		self.assertEqual(sum(1 for _ in self.ks),           4)
		self.assertEqual(sum(1 for _ in reversed(self.ks)), 4)
		self.assertEqual(sum(1 for _ in kdb.KeySet(0)),     0)

if __name__ == '__main__':
	unittest.main()
