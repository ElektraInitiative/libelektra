import kdb, kdb.tools, unittest

class PluginDatabase(unittest.TestCase):
	def setUp(self):
		self.db = kdb.tools.ModulesPluginDatabase()

	def test_instance(self):
		plugins = self.db.listAllPlugins()
		self.assertTrue(len(plugins) >= 0)

	def test_pluginspec(self):
		spec = self.db.lookupProvides('mini')
		self.assertIsInstance(spec, kdb.tools.PluginSpec)
		self.assertEqual(spec.fullname, "mini#mini")

		self.assertEqual(self.db.status(spec), kdb.tools.ModulesPluginDatabase.real)
		self.assertEqual(self.db.status(kdb.tools.PluginSpec("does_not_exist")), kdb.tools.ModulesPluginDatabase.missing)

		specs = self.db.lookupAllProvides('mini')
		self.assertTrue(len(specs) >= 0)
		for spec in specs:
			self.assertIsInstance(spec, kdb.tools.PluginSpec)
			self.assertTrue(len(spec.fullname) > 0)

		specs = self.db.lookupAllProvidesWithStatus('mini')
		self.assertIsInstance(specs, kdb.tools.IntPluginSpecMap)
		self.assertTrue(len(specs) >= 0)
		self.assertIsInstance(specs.values()[0], kdb.tools.PluginSpec)
		for k in specs:
			self.assertIsInstance(specs[k], kdb.tools.PluginSpec)

if __name__ == '__main__':
	unittest.main()
