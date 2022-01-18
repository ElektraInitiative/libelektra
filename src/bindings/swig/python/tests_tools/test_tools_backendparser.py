import kdb, kdb.tools, unittest

class ParseArguments(unittest.TestCase):
	def setUp(self):
		self.ks = kdb.KeySet(2, kdb.Key("user:/c1", "v1"), kdb.Key("user:/c2", "v2"))

	def test_parse_arguments(self):
		args = kdb.tools.parseArguments("plugin c1=v1 c2=v2")
		self.assertEqual(len(args), 1)
		self.assertIsInstance(args[0], kdb.tools.PluginSpec)
		self.assertEqual(args[0].config, self.ks)

		# pass args without a plugin name
		with self.assertRaises(kdb.tools.ParseException):
			kdb.tools.parseArguments("c1=v1")

	def test_parse_plugin_arguments(self):
		args = kdb.tools.parsePluginArguments("c1=v1,c2=v2")
		self.assertEqual(args, self.ks)

if __name__ == '__main__':
	unittest.main()
