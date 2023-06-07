import kdb, unittest

class ElektraDiff(unittest.TestCase):
	def test_calculate(self):
		old_keys = kdb.KeySet(0)
		old_keys.append(kdb.Key("user:/test/a"))
		old_keys.append(kdb.Key("user:/test/b"))

		new_keys = kdb.KeySet(0)
		new_keys.append(kdb.Key("user:/test/a"))

		diff: kdb.ElektraDiff = kdb.ElektraDiff.calculateDiff(new_keys, old_keys, kdb.Key("user:/test"))

		self.assertFalse(diff.isEmpty())
		self.assertIsNotNone(diff.getRemovedKeys()["user:/test/b"])

if __name__ == '__main__':
	unittest.main()
