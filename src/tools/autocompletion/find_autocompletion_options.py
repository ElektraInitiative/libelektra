import kdb
import sys
import subprocess

# last_word is used to store the last command typed
last_word = None
# start_of_input is used to store the string that should be completed
start_of_input = None
# mount_point is used to store the mount point the specification file is
# 		mounted at
# TODO: should not be hardcoded
mount_point = 'spec/autocomplete'

# ARGUMENTS for find_autocompletion_options.py
# argv[1] => start_of_input
# argv[2] => last_word

# gets arguments passed to the script and sets last_word and start_of_input
def get_command_line_arguments():
	global start_of_input, last_word
	if (len(sys.argv) > 1):
		start_of_input = sys.argv[1]
	if (len(sys.argv) > 2):
		last_word = sys.argv[2]

# input: input_start_of_input, the string that should be used to complete
#		input_last_word, the last typed string
# sets start of word and calls find_auto_completion_options
# used for testing
def set_input_and_run(input_start_of_input, input_last_word):
	global start_of_input, last_word
	start_of_input = input_start_of_input
	return find_auto_completion_options()

# depending on last_word calls completion for commands or arguments
# return: string seperated by \n, every line is one possible completion 
#		option
def find_auto_completion_options():
	global last_word
	key = get_key(last_word)
	completion = ""
	# case 1: last_word is a command => first get all completion options
	# 		for arguments, then completion options for commands in case
	#		another command starts with last_word
	if key != None:
		completion += complete_arguments() + '\n'
	# case 2: last_word is not a command => complete commands
	completion += complete_commands()
	return completion


# searches for commands starting with start_of_input
# return: string seperated by \n, every line is one possible completion 
#		option
def complete_commands():
	global start_of_input, mount_point
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, mount_point)
		root_key = ks.lookup(mount_point)
		root = root_key.getMeta('root').value
		len_path_mount_point = len(mount_point.split('/'))
		completion_options = []
		for key in ks:
			path = str(key).split('/')
			if len(path) <= len_path_mount_point or path[len_path_mount_point] != root:
				continue
			opt_long = (key.getMeta(name="opt/long"))
			opt = (key.getMeta(name="opt"))
			if start_of_input != None:
				if opt_long and opt_long.value.startswith(start_of_input):
					completion_options.append(opt_long.value)
				elif opt and opt.value.startswith(start_of_input):
					completion_options.append(opt.value)
			else:
				if opt_long:
					completion_options.append(opt_long.value)
				elif opt:
					completion_options.append(opt.value)
	output = '\n'.join(completion_options)
	return output

# searches for arguments starting with start_of_input
# return: string seperated by \n, every line is one possible completion 
#		option
def complete_arguments():
	pass

# input: command, a string that should be a shell command
# executes the shell command and returns the output
# IMPORTANT: the string that the command returns should be seperated
# 		by \n
# return: string seperated by \n, every line is one possible completion 
#		option
def execute_shell_command(command):
	global start_of_input
	complete_command = command if start_of_input == None else (command + ' ' + start_of_input)
	process = subprocess.Popen(complete_command.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()
	output = output.decode("utf-8")
	return output

# input: input_string, a string that could be a valid command or not
# checks if the typed string is a valid command
# return: the key belonging to the command, if no command found None
def get_key(input_string):
	global mount_point, last_key
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, mount_point)
		for key in ks:
			opt_long = (key.getMeta(name="opt/long"))
			opt = (key.getMeta(name="opt"))
			if (opt_long and opt_long.value == input_string) or (opt and opt.value == input_string):
				print(key)
				return key
	return None



def main():
	get_command_line_arguments()
	result = find_auto_completion_options()
	print(result)

if __name__ == "__main__":
	main()
