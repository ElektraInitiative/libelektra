#!/usr/bin/python3

import sys
import kdb

base_key = "/sw/org/kdbdummy/#0/current"
spec_base_key = "spec" + base_key

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

def get_python_interpreter_arguments():
	import ctypes
	argc = ctypes.c_int()
	argv = ctypes.POINTER(ctypes.c_wchar_p if sys.version_info >= (3, ) else ctypes.c_char_p)()
	ctypes.pythonapi.Py_GetArgcArgv(ctypes.byref(argc), ctypes.byref(argv))

	# Ctypes are weird. They can't be used in list comprehensions, you can't use `in` with them, and you can't
	# use a for-each loop on them. We have to do an old-school for-i loop.
	arguments = list()
	for i in range(argc.value):
		arguments.append(argv[i])

	return arguments

# we need to calculate the number of arguments given to the python interpreter itself
# this number is given to 'gopts' as an offset, so it only looks at the keys actually relvant to this script
argv_offset = len(get_python_interpreter_arguments()) - len(sys.argv)

setupSpec()
with kdb.KDB() as db:
	error_key = kdb.Key(spec_base_key)
	rc = db.ensure(kdb.KeySet(2,
			kdb.Key("system:/elektra/ensure/plugins/global/gopts", kdb.KEY_VALUE, "remount"), 
			kdb.Key("system:/elektra/ensure/plugins/global/gopts/config/offset", kdb.KEY_VALUE, str(argv_offset))
			), error_key)
	if rc == 1:
		print("ERROR: Contract could not be ensured!\n", error_key.getMeta("error/reason").value, file=sys.stderr)
		removeSpec()
		exit(1)
	elif rc == -1:
		print("ERROR: Contract malformed/NULL pointer!\n", error_key.getMeta("error/reason").value, file=sys.stderr)
		removeSpec()
		exit(1)
	
	ks = kdb.KeySet(0)
	db.get(ks, spec_base_key)
	
	if "proc/elektra/gopts/help" in ks and ks["proc/elektra/gopts/help"].value == "1":
		print (ks["proc/elektra/gopts/help/message"].value)
		removeSpec()
		exit(0)
	
	print("A real implementation would now")

	if base_key + "/printversion" in ks and ks[base_key + "/printversion"].value == "1":
		print("print version information\n")
		removeSpec()
		exit(0)

	if base_key in ks and ks[base_key].value == "getter":
		if base_key + "/getter/keyname" not in ks or len(ks[base_key + "/getter/keyname"].value) == 0:
			print ("report the error 'empty parameter: keyname'")
			removeSpec()
			exit(0)

		print("get the key '" + ks[base_key + "/getter/keyname"].value + "'")

		if base_key + "/getter/verbose" in ks and ks[base_key + "/getter/verbose"].value == "1":
			print ("print where the read key value comes from")

	elif base_key in ks and ks[base_key].value == "setter":
		if base_key + "/setter/keyname" not in ks or len(ks[base_key + "/setter/keyname"].value) == 0:
			print ("report the error 'missing parameter: keyname'")
			removeSpec()
			exit(0)

		if base_key + "/setter/value" not in ks:
			print ("report the error 'missing parameter: value'")
			removeSpec()
			exit(0)

		print ("set the key '" + ks[base_key + "/setter/keyname"].value + "' with the value '" + ks[base_key + "/setter/value"].value + "'")

		if base_key + "/setter/verbose" in ks and ks[base_key + "/setter/verbose"] == "1":
			print ("print where the key value is stored now")

	else:
		if base_key + "/dynamic" in ks and base_key + "/dynamic/#0" in ks:
			print ("dynamically invoke the command '" + ks[base_key + "/dynamic/#0"].value + "' with arguments:", end="")

			index = 1
			while True:
				try:
					print (" "+ks[base_key + "/dynamic/#" + array_index(index)].value, end="")
				except KeyError:
					break
				index += 1
			print ()
		else:
			print ("do nothing\n")

removeSpec()
