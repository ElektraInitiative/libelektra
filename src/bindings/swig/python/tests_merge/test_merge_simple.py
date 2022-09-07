import kdb, kdb.merge, unittest

class MergeSimple(unittest.TestCase):
	def setUp(self):
		self.ksBase = kdb.KeySet(100,
								 kdb.Key("system:/test/key1", "k1"),
								 kdb.Key("system:/test/key2", "k2"),
								 kdb.KS_END,
								 )

	def test_load(self):
		ourKeys = kdb.KeySet(100,
							 kdb.Key("system:/test/key1", "k1"),
							 kdb.Key("system:/test/key3", "k3"),
							 kdb.KS_END,
							 )


		base = kdb.merge.MergeKeys(self.ksBase, kdb.Key("system:/test"))
		theirs = base
		ours = kdb.merge.MergeKeys(ourKeys, kdb.Key("system:/test"))

		merger = kdb.merge.Merger()
		result = merger.merge(base, ours, theirs, kdb.Key("system:/test"), kdb.merge.ConflictStrategy.ABORT)

		self.assertFalse(result.hasConflicts())

if __name__ == '__main__':
	unittest.main()
