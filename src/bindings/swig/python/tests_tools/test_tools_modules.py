import kdb, kdb.tools, unittest

class Modules(unittest.TestCase):
	def test_load(self):
		modules = kdb.tools.Modules()
		config = kdb.KeySet(1, kdb.Key("system:/module", "without config"))
		spec = kdb.tools.PluginSpec("dump", config)
		plugin = modules.load(spec)
		self.assertIsInstance(plugin, kdb.tools.Plugin)
		self.assertEqual(plugin.name, "dump")
		self.assertEqual(config, plugin.getConfig())

		with self.assertRaises(kdb.tools.NoPlugin):
			modules.load(kdb.tools.PluginSpec("does_not_exist"))

if __name__ == '__main__':
	unittest.main()
