import kdb, kdb.tools, unittest

class PluginSpec(unittest.TestCase):
	def test_properties(self):
		config = kdb.KeySet(1, kdb.Key("system:/module", "without config"))
		spec = kdb.tools.PluginSpec("somename", config)
		self.assertEqual(spec.name,     "somename")
		self.assertEqual(spec.fullname, "somename#somename")
		self.assertEqual(spec.refName,  "somename")
		self.assertEqual(spec.config,   config)
		self.assertFalse(spec.refNumber)

		spec.name = "other"
		self.assertEqual(spec.name, "other")

		spec.refName = "myref"
		self.assertEqual(spec.refName, "myref")

		spec.fullname = "fullname"
		self.assertEqual(spec.fullname, "fullname#myref")

		spec.refNumber = 5
		self.assertTrue(spec.refNumber)

		config.append(kdb.Key("system:/modules/2"))
		self.assertNotEqual(spec.config, config)
		spec.config = config
		self.assertEqual(spec.config, config)

		with self.assertRaises(kdb.tools.BadPluginName):
			spec.fullname = "this is invalid"

if __name__ == '__main__':
	unittest.main()
