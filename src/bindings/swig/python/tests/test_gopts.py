#!/usr/bin/python3

import sys
import kdb
import unittest

base_key = "/sw/org/kdbdummy/#0/current"
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
		setupSpec()
		with kdb.KDB() as db:
			error_key = kdb.Key(spec_base_key)
			rc = db.ensure(kdb.KeySet(2,
					kdb.Key("system:/elektra/ensure/plugins/global/gopts", kdb.KEY_VALUE, "remount")
					), error_key)
			if rc == 1:
				print("ERROR: Contract could not be ensured!\n", error_key.getMeta("error/reason").value, file=sys.stderr)
				removeSpec()
				exit(1)
			elif rc == -1:
				print("ERROR: Contract malformed/NULL pointer!\n", error_key.getMeta("error/reason").value, file=sys.stderr)
				removeSpec()
				exit(1)

			custom_argv = [ "get", "-v", "user:/"]
			custom_envp = []

			db.setupGOpts (base_key, len(custom_argv), custom_argv, custom_envp)

			ks = kdb.KeySet(0)
			db.get(ks, spec_base_key)
			
			if "proc:/elektra/gopts/help" in ks and ks["proc:/elektra/gopts/help"].value == "1":
				print (ks["proc:/elektra/gopts/help/message"].value)
				removeSpec()
				exit(0)
			
			self.assertTrue((base_key in ks))
			self.assertEqual(ks[base_key].value, "getter")
			self.assertEqual(ks[base_key+"/getter/keyname"].value, "user:/")
			self.assertTrue((base_key + "/getter/verbose" in ks))
			self.assertEqual(ks[base_key + "/getter/verbose"].value, "1")
		removeSpec()
