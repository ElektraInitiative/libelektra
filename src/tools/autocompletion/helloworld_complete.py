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
			if start_of_word == None or command.startswith(start_of_word):
				completion_options.append(command)

	output = ""
	for i in completion_options:
		output += " " + i
	print(output)

find_auto_completion_options()
