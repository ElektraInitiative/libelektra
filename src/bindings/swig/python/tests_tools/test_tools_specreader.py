import kdb, kdb.tools, unittest

class SpecReader(unittest.TestCase):
	def test_spec_reader(self):
		spec = kdb.KeySet(1, kdb.Key("spec:/example/xx", { "mountpoint": "/example/xx" }))

		sr = kdb.tools.SpecReader()
		sr.readSpecification(spec)
		backends = sr.getBackends()
		self.assertIsInstance(backends, kdb.tools.SpecBackendBuilderMap)
		self.assertEqual(len(backends), 1)
		self.assertIsInstance(backends[ spec[0] ], kdb.tools.SpecBackendBuilder)
		self.assertEqual(backends.keys(), [ k for k in spec ])
		self.assertIsInstance(backends.values()[0], kdb.tools.SpecBackendBuilder)

if __name__ == '__main__':
	unittest.main()
