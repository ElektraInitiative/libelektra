import kdb
import sys
import subprocess
import getopt

# get name of program
root = None
# all the previous typed commands, options and arguments
typed = []
# last_word is used to store the last command typed
last_command = None
# start_of_current_input is used to store the string that should be completed
start_of_current_input = None
# mount_point is used to store the mount point the specification file is
# 		mounted at
# TODO: should not be hardcoded
mount_point = None

# ARGUMENTS for find_autocompletion_options.py
# -s ... start_of_current_input
# -l ... last_word

# gets arguments passed to the script and sets last_word and start_of_current_input
def get_command_line_arguments():
	global mount_point, root, typed, start_of_current_input, last_command
	try:
		opts, args = getopt.getopt(sys.argv[1:],'s:m:')
	except getopt.GetoptError:
		print('getting command line options failed')
		sys.exit(2)
	for opt, arg in opts:
		if opt == '-s':
			s = arg.strip()
			if s:
				start_of_current_input = s
		if opt == '-m':
			if arg.strip():
				mount_point = arg.strip()
	if mount_point is None:
		print('no mount point, pass mount point with -m')
		sys.exit(2)
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, mount_point)
		for i in ks:
			len_path_mount_point = len(mount_point.split('/'))
			len_path_key = len(str(i))
			if str(i) != mount_point and len_path_mount_point < len_path_key:
				key_split = str(i).split('/')
				root = key_split[len_path_mount_point]
				break
		if len(args) > 0:
			typed = args[0].split()
			for i in reversed(typed):
				if last_command is not None:
					break
				for j in ks:
					if str(j).split('/')[-1] == i:
						last_command = str(j)
						break
		k.close()
	if root is None:
		print('getting root failed')
		sys.exit(2)

	
# input: input_start_of_input, the string that should be used to complete
#		input_last_word, the last typed string
# sets start of word and calls find_auto_completion_options
# used for testing
def set_input_and_run(input_mount_point, input_root, input_start_of_current_input, input_last_command, input_typed):
	global mount_point, root, last_command, typed, start_of_current_input
	mount_point = input_mount_point
	root = input_root
	start_of_current_input = input_start_of_current_input
	last_command = input_last_command
	typed = input_typed
	return find_auto_completion_options()

# depending on last_word calls completion for commands or arguments
# return: string seperated by a \n, every line is one possible completion 
#		option
def find_auto_completion_options():
	global last_command, start_of_current_input
	completion = []
	completion.extend(complete_options())
	#completion.extend(complete_commands(start_of_current_input))
	completion = '\n'.join(completion)
	return completion


# input:  start_of_input
# searches for commands starting with start_of_input
# return: list of all possible commands for completion
def complete_commands(start_of_input):
	global mount_point, root
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, mount_point)
		len_path_mount_point = len(mount_point.split('/'))
		completion_options = []
		for key in ks:
			path = str(key).split('/')
			if len(path) <= len_path_mount_point or path[len_path_mount_point] != root:
				continue
			if start_of_input is None:
				completion_options.append(path[-1])
			elif path[-1].startswith(start_of_input):
				completion_options.append(path[-1])
		k.close()
		return completion_options

# searches for arguments starting with start_of_input
# return: list of all possible arguments
'''def complete_arguments(key_path):
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
	return completion_options'''

# searches for options starting with start_of_input belonging to last_word
# return: list of all possible arguments
def complete_options():
	global last_command
	completion_options = []
	if last_command is not None:
		completion_options.extend(complete_options_opt_array(last_command))
	else:
		completion_options.extend(complete_options_single())
	return completion_options

def complete_options_single():
	global mount_point, start_of_current_input
	completion_options = []
	if start_of_current_input is not None:
		start_of_word = start_of_current_input.replace('-', '')
		len_of_word = len(start_of_current_input.strip())
	k = kdb.KDB()
	ks = kdb.KeySet()
	k.get(ks, mount_point)
	for key in ks:
		path = str(key).split('/')
		meta_opt = key.getMeta(name='opt')
		if meta_opt and meta_opt.value.startswith('#'):
			len_opts = int((meta_opt.value)[1:])
			i = 0
			while i <= len_opts:
				i+=1
				opt = key.getMeta(name='opt/#{}'.format(i))
				opt_long = key.getMeta(name='opt/#{}/long'.format(i))
				if start_of_current_input is None or len_of_word == 0:
					if opt_long:
						
						completion_options.append('--' + opt_long.value)
					if opt:
						completion_options.append('-' + opt.value)
				else:
					if opt_long and opt_long.value.startswith(start_of_word):
						completion_options.append('--' + opt_long.value)
					if opt and opt.value.startswith(start_of_word):
						completion_options.append('-' + opt.value)
		elif meta_opt:
			# TODO no list
			pass
	k.close()
	return completion_options

def complete_options_opt_array(path):
	completion_options = []
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, mount_point)
		key = ks.lookup(path)
		meta_opt = key.getMeta(name='opt')
		len_opts = 0
		if meta_opt:
			len_opts = int((meta_opt.value)[1:])
		i = 0
		while i <= len_opts:
			i+=1
			opt = key.getMeta(name='opt/#{}'.format(i))
			opt_long = key.getMeta(name='opt/#{}/long'.format(i))
			if start_of_current_input is not None:
				if opt_long and opt_long.value.startswith(start_of_current_input.replace('-', '')):
					completion_options.append('--' + opt_long.value)
				elif opt and opt.value.startswith(start_of_current_input.replace('-', '')):
					completion_options.append('-' + opt.value)
			else:
				if opt_long:
					completion_options.append('--' + opt_long.value)
				elif opt:
					completion_options.append('-' + opt.value)
		k.close()
	return completion_options

# input: command, a string that should be a shell command
#		seperator, a char that seperates every result in the result 
# executes the shell command and returns the output
# return: list of strings with the result of the executed command
def execute_shell_command(command):
	global start_of_input
	complete_command = command if start_of_input is None else (command + ' ' + start_of_input)
	process = subprocess.Popen(complete_command.split(), stdout=subprocess.PIPE)
	output, error = process.communicate()
	output = output.decode('utf-8')
	output = output.splitlines()
	return output

# input: input_string, a string that could be a valid command or not
# checks if the typed string is a valid command
# return: the key belonging to the command, if no command found None
def get_key(input_string):
	global mount_point, last_word
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, mount_point)
		for key in ks:
			path = str(key).split('/')
			if path[-1] == last_word:
				return str(key)
		k.close()
	return None


if __name__ == '__main__':
	get_command_line_arguments()
	result = find_auto_completion_options()
	print(result)





