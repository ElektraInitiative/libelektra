#!/usr/bin/python3

import sys
import kdb
import unittest
from ctypes import c_char_p

base_key = "/tests/python/gopts"
spec_base_key = "spec:" + base_key

spec = kdb.KeySet(10, kdb.Key(spec_base_key, kdb.KEY_META, "command", ""),
		  kdb.Key(spec_base_key + "/printversion",
			  kdb.KEY_META, "description", "print version information and exit (ignoring all other options/commands/parameters)",
			  kdb.KEY_META, "opt", "v",
			  kdb.KEY_META, "opt/arg", "none",
			  kdb.KEY_META, "opt/long", "version"),
		  kdb.Key(spec_base_key + "/getter",
			  kdb.KEY_META, "description", "get a key's value",
			  kdb.KEY_META, "command", "get"),
		  kdb.Key(spec_base_key + "/getter/verbose",
			  kdb.KEY_META, "description", "print additional information about where the value comes from",
			  kdb.KEY_META, "opt", "v",
			  kdb.KEY_META, "opt/long", "verbose",
			  kdb.KEY_META, "opt/arg", "none"),
		  kdb.Key(spec_base_key + "/getter/keyname",
			  kdb.KEY_META, "description", "name of the key to read",
			  kdb.KEY_META, "args", "indexed",
			  kdb.KEY_META, "args/index", "0"),
		  kdb.Key(spec_base_key + "/setter",
			  kdb.KEY_META, "description", "set a key's value",
			  kdb.KEY_META, "command", "set"),
		  kdb.Key(spec_base_key + "/setter/verbose",
			  kdb.KEY_META, "description", "print additional information about where the value will be stored",
			  kdb.KEY_META, "opt", "v",
			  kdb.KEY_META, "opt/long", "verbose",
			  kdb.KEY_META, "opt/arg", "none"),
		  kdb.Key(spec_base_key + "/setter/keyname",
			  kdb.KEY_META, "description", "name of the key to write",
			  kdb.KEY_META, "args", "indexed",
			  kdb.KEY_META, "args/index", "0"),
		  kdb.Key(spec_base_key + "/setter/value",
			  kdb.KEY_META, "description", "value to be written",
			  kdb.KEY_META, "args", "indexed", kdb.KEY_META, "args/index", "1"),
		  kdb.Key(spec_base_key + "/dynamic/#",
			  kdb.KEY_META, "description", "dynamically call a user-supplied command",
			  kdb.KEY_META, "args", "remaining")
		  )

def setupSpec():
	with kdb.KDB() as db:
		ks = kdb.KeySet(10)
		db.get(ks, spec_base_key)
		if len(ks.cut(kdb.Key(spec_base_key))) > 0:
			print("ERROR: Couldn't setup spec, keys exist!", file=sys.stderr)
			exit(1)

		ks.extend(spec)
		db.set(ks, spec_base_key)

def removeSpec():
	with kdb.KDB() as db:
		ks = kdb.KeySet(10)
		db.get(ks, spec_base_key)
		ks.cut(kdb.Key(spec_base_key))
		db.set(ks, spec_base_key)

def array_index(index):
	return "_" * (len(str(index)) - 1) + str(index)

class GOpts(unittest.TestCase):
	def test_gopts(self):
		try:
			setupSpec()

			custom_argv = ["test", "get", "-v", "user:/"]
			custom_envp = []

			config = kdb.KeySet(0)
			contract = kdb.KeySet(0)
			kdb.goptsContract (contract, custom_argv, custom_envp, kdb.Key(base_key), config)

			with kdb.KDB(contract) as db:
				ks = kdb.KeySet(0)
				db.get(ks, base_key)

				self.assertTrue((base_key in ks))
				self.assertEqual(ks[base_key].value, "getter")
				self.assertEqual(ks[base_key+"/getter/keyname"].value, "user:/")
				self.assertTrue((base_key + "/getter/verbose" in ks))
				self.assertEqual(ks[base_key + "/getter/verbose"].value, "1")
		finally:
			removeSpec()

if __name__ == '__main__':
	unittest.main()
