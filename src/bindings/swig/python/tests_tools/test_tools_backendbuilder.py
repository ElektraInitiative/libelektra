import kdb, kdb.tools, unittest

class BackendBuilder(unittest.TestCase):
	def test_instance(self):
		bb = kdb.tools.BackendBuilder()

		db = bb.getPluginDatabase()
		self.assertIsInstance(db, kdb.tools.PluginDatabase)
		plugins = db.listAllPlugins()
		self.assertTrue(len(plugins) >= 0)

	def test_mountbackendbuilder(self):
		bb = kdb.tools.MountBackendBuilder()
		self.assertFalse(bb.validated())
		bb.addPlugin(kdb.tools.PluginSpec("resolver"))
		self.assertFalse(bb.validated())
		bb.addPlugin(kdb.tools.PluginSpec("dump"))
		self.assertTrue(bb.validated())
		self.assertEqual([ x.name for x in bb ], ["resolver", "dump"])

if __name__ == '__main__':
	unittest.main()
