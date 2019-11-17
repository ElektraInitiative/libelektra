import kdb
import sys
import subprocess
import getopt

# last_word is used to store the last command typed
last_word = None
# start_of_input is used to store the string that should be completed
start_of_input = None
# mount_point is used to store the mount point the specification file is
# 		mounted at
# TODO: should not be hardcoded
mount_point = 'spec/autocomplete'

# ARGUMENTS for find_autocompletion_options.py
# -s ... start_of_input
# -l ... last_word

# gets arguments passed to the script and sets last_word and start_of_input
def get_command_line_arguments():
	global start_of_input, last_word
	try:
		opts, args = getopt.getopt(sys.argv[1:],'s:l:')
	except getopt.GetoptError:
		print('getting command line options failed')
		sys.exit(2)
	for opt, arg in opts:
		if opt == '-s':
			if arg.strip():
				start_of_input = arg.strip()
		elif opt == '-l':
			if arg.strip():
				last_word = arg.strip()
	

# input: input_start_of_input, the string that should be used to complete
#		input_last_word, the last typed string
# sets start of word and calls find_auto_completion_options
# used for testing
def set_input_and_run(input_start_of_input, input_last_word):
	global start_of_input, last_word
	start_of_input = input_start_of_input
	return find_auto_completion_options()

# depending on last_word calls completion for commands or arguments
# return: string seperated by a white-space, every line is one possible completion 
#		option
def find_auto_completion_options():
	global last_word, start_of_input
	key_start_of_input = get_key(start_of_input)
	key_last_word = get_key(last_word)
	completion = []
	# case: last_word is a command => first get all completion options
	# 		for arguments, then completion options for all commands
	if key_last_word != None:
		argument_completion = complete_arguments(key_last_word)
		completion.extend(argument_completion)
		# TODO - this should be changed as soon as opt/arg is added - for 
		#		opt/arg=required this should not be run 
		command_completion = []
		if start_of_input == None or len(start_of_input.strip()) <=0:
			command_completion = complete_commands(None)
		else:
			command_completion = complete_commands(start_of_input)
		completion.extend(command_completion)
	# case 2: last_word is not a command => complete commands
	else:
		command_completion = complete_commands(start_of_input)
		completion.extend(command_completion)
	completion = '\n'.join(completion)
	return completion

# input:  start_of_input
# searches for commands starting with start_of_input
# return: list of all possible commands for completion
def complete_commands(start_of_input):
	global mount_point
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
			opt_long = (key.getMeta(name='opt/long'))
			opt = (key.getMeta(name='opt'))
			if  start_of_input != None:
				if opt_long and opt_long.value.startswith( start_of_input):
					completion_options.append(opt_long.value)
				elif opt and opt.value.startswith(start_of_input):
					completion_options.append(opt.value)
			else:
				if opt_long:
					completion_options.append(opt_long.value)
				elif opt:
					completion_options.append(opt.value)
		k.close()
		return completion_options

# searches for arguments starting with start_of_input
# return: list of all possible arguments
def complete_arguments(key_path):
	global start_of_input
	completion_options = []
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, mount_point)
		key = ks.lookup(key_path)
		# opt/arg/check can contain the name of a type defined in the 
		#		specification file
		argument_type = key.getMeta(name='opt/arg/check')
		if argument_type:
			type_key = ks.lookup(mount_point + '/' + argument_type.value)
			if type_key:
				shell_command = type_key.getMeta(name='completion/shell')
				if shell_command:
					execute = shell_command.value
					if start_of_input != None:
						execute += ' '+start_of_input
					shell_command_completion = execute_shell_command(execute)
					completion_options.extend(shell_command_completion)
				# TODO: add more types of arguments
		k.close()
	return completion_options

# input: command, a string that should be a shell command
#		seperator, a char that seperates every result in the result 
# executes the shell command and returns the output
# return: list of strings with the result of the executed command
def execute_shell_command(command):
	global start_of_input
	complete_command = command if start_of_input == None else (command + ' ' + start_of_input)
	process = subprocess.Popen(complete_command.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()
	output = output.decode('utf-8')
	output = output.splitlines()
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
			opt_long = (key.getMeta(name='opt/long'))
			opt = (key.getMeta(name='opt'))
			if (opt_long and opt_long.value == input_string) or (opt and opt.value == input_string):
				return str(key)
		k.close()
	return None


if __name__ == '__main__':
	get_command_line_arguments()
	result = find_auto_completion_options()
	print(result)
