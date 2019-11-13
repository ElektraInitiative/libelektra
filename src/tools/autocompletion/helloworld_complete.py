import kdb
import sys

def find_auto_completion_options():
	start_of_word = None
	if (len(sys.argv) > 1):
		start_of_word = sys.argv[1]
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, 'spec/autocomplete')
		completion_options = []
		for key in ks: 
			command = str(key).split('/')[-1]
			opt_long = (key.getMeta(name="opt/long"))
			opt = (key.getMeta(name="opt"))
			if start_of_word != None:
				if not opt and opt and opt.value.startswith(start_of_word):
					completion_options.append(opt.value)
				if opt_long and opt_long.value.startswith(start_of_word):
					completion_options.append(opt_long.value)
			else:
				if opt_long:
					completion_options.append(opt_long.value)
				elif opt:
					completion_options.append(opt.value)

	output = ""
	for i in completion_options:
		output += " " + i
	print(output)

find_auto_completion_options()
