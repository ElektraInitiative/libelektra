import kdb
import sys

start_of_word = None

def get_start_of_word():
	global start_of_word
	if (len(sys.argv) > 1):
		start_of_word = sys.argv[1]

def set_start_of_word_and_run(input_start_of_word):
	global start_of_word
	start_of_word = input_start_of_word
	return find_auto_completion_options()


def find_auto_completion_options():
	global start_of_word
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, 'spec/autocomplete')
		completion_options = []
		for key in ks: 
			command = str(key).split('/')[-1]
			opt_long = (key.getMeta(name="opt/long"))
			opt = (key.getMeta(name="opt"))
			if start_of_word != None:
				if opt_long and opt_long.value.startswith(start_of_word):
					completion_options.append(opt_long.value)
				elif opt and opt.value.startswith(start_of_word):
					completion_options.append(opt.value)
			else:
				if opt_long:
					completion_options.append(opt_long.value)
				elif opt:
					completion_options.append(opt.value)

	output = ""
	for i in completion_options:
		output += " " + i
	return output

if __name__ == "__main__":
	get_start_of_word()
	print(find_auto_completion_options())
