import kdb
import sys
import subprocess
import getopt

# name of program requesting completion
root = None
# list of previously typed commands, options and arguments
typed = []
# last command (not option or argument) typed
last_command = None
# string typed before pressing TAB
start_of_current_input = ''
# mount point of config for program
mount_point = None


# ARUGUMENTS
# not optional
# -m .. pass mount moint 
# optional
# -s .. pass string typed before pressing TAB
# in the end a string seperated by spaces can be passed
#
# gets arguments passed to script
# sets root, typed, last_command, start_of_current_input, mount_point
# fails when:
#	- error while getting command line options
#	- no mount point given
#	- no root was found
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
			m = arg.strip()
			if m:
				mount_point = m
	if mount_point is None:
		print('no mount point, pass mount point with -m')
		sys.exit(2)
	k = kdb.KDB()
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

# decides if commands, options or arguments should be completed
# at the moment only options will be completed
# TODO add completion for arguments
# TODO add completion for commands
# returns string with completions options seperated by \n
def find_auto_completion_options():
	global last_command, start_of_current_input
	completion = []
	arg_needed = False
	# TODO check if last command needs an argument
	# TODO check if last option needs an argument
	if arg_needed:
		# TODO implement me
		pass
	else:
		completion.extend(complete_args())
		completion.extend(complete_options())
	#completion.extend(complete_commands(start_of_current_input))
	completion = '\n'.join(completion)
	return completion

# input - command, a string that should be a shell command
# executes the shell command and returns the output
# returns list of strings with the result of the executed command
# ls does not work completely yet
def execute_shell_command(command):
	global start_of_current_input
	output = []
	complete_command = command + ' ' + start_of_current_input
	try:
		process = subprocess.Popen(complete_command.split(), stdout=subprocess.PIPE)
		output, error = process.communicate()
		p_status = process.wait()
		if p_status == 0:
			output = output.decode('utf-8')
			output = output.splitlines()
	except Exception as e:
		# TODO what to do here
		#print(e)
		pass
	return output

# complete arguments not belonging to specific options
# if a command to execute is given, will execute said command and returns the result
# at the moment only args = remaining is supported
# TODO add args = single
# TODO add args = multiple
#
# expect one argument
#	[path]
# 	args = single 
# only allowed once per specification file
# if args = single is used, multiple or remaining can't be used
#
# expect multiple arguments, specific amount
#	[path]
# 	args = multiple
#	args/index = 3
# index is the last index of the array
# args = multiple with args/index = 0 is equivalent to args = single
#
# expect multiple arguments, any amount
#	[path/#]
#	args = remaining
# only allowed once per specification file
# args = remaining and args = multiple can be used together, remaining are those that are left after
#	multiple have all been completed
#
# returns list of completion options for arguments
def complete_args():
	global start_of_current_input, mount_point
	completion_arguments = []
	word = start_of_current_input.strip()
	while word.startswith('-'):
		word = word[1:]
	k = kdb.KDB()
	ks = kdb.KeySet()
	k.get(ks, mount_point)
	for key in ks:
		args = key.getMeta(name='args')
		if not args:
			continue
		if args.value == 'remaining':
			shell_command = key.getMeta(name='completion/shell')
			if shell_command:
				completion_arguments.extend(execute_shell_command(shell_command.value))
	k.close()
	return completion_arguments

# decides what kind of options should be completed
# considers start_of_current_input
# for empty input will complete all short options
# for '-' will complete all short options
# for '--' will complete all long options
# for a string not starting with '-' or '--' will return empty list
# for a string starting with '-' will complete all matching short options
# for a string starting with '--' will complete all matching long options
# will return empty list if no options found
# at the moment will return empty list when last_command is not None
# TODO complete with last_command
# TODO complete with typed
# returns list of all possible options
def complete_options():
	global last_command, start_of_current_input
	completion_options = []
	# if no last command exists, only options specified as [program/option] will be
	# considered, in that block there may either be an option array
	# at the moment there is no way of extinguishing between [program/option] where
	# options are declared and [program/command] where options for that specific command
	# are specified, the program currently assumes that anything written after program/
	# is an option
	if last_command is None:
		one_hyphen = start_of_current_input.strip().startswith('-')
		two_hyphen = start_of_current_input.strip().startswith('--')
		if two_hyphen:
			one_hyphen = False
		if len(start_of_current_input.strip()) == 0 or one_hyphen:
			completion_options.extend(complete_short_options())
		elif two_hyphen:
			completion_options.extend(complete_long_options())
	else:
		# TODO implement me
		pass
	return completion_options

# searches for all short options starting with start_of_current_input
# adds short options in arrays and single short option
# skips over blocks where after opt=# is something other than an int
# returns list of all short options matching start_of_current_input
def complete_short_options():
	global start_of_current_input, mount_point
	completion_options = []
	k = kdb.KDB()
	ks = kdb.KeySet()
	k.get(ks, mount_point)
	for key in ks:
		opt = key.getMeta(name='opt')
		word = start_of_current_input.strip()
		while word.startswith('-'):
			word = word[1:]
		if opt and opt.value.startswith('#'):
			try:
				len_opts = int((opt.value)[1:])
			except:
				continue
			for i in range(len_opts+1):
				opt = key.getMeta(name='opt/#{}'.format(i))	
				if opt and opt.value.startswith(word):
					completion_options.append('-' + opt.value)
		elif opt:
			if opt.value.startswith(word):
				completion_options.append('-' + opt.value)
	k.close()
	return completion_options

# searches for all long options starting with start_of_current_input
# adds long options in arrays and single long option
# skips over blocks where after opt=# is something other than an int
# if more than two hyphens have been typed, complete will automatically remove them
# returns list of all long options matching start_of_current_input
def complete_long_options():
	global start_of_current_input, mount_point
	completion_options = []
	k = kdb.KDB()
	ks = kdb.KeySet()
	k.get(ks, mount_point)
	for key in ks:
		long_opt = key.getMeta(name='opt/long')
		opt = key.getMeta(name='opt')
		if long_opt:
			word = start_of_current_input.strip()
			while word.startswith('-'):
				word = word[1:]
			if long_opt.value.startswith(word):
				completion_options.append('--' + long_opt.value)
		elif opt and opt.value.startswith('#'):
			try:
				len_opts = int((opt.value)[1:])
			except:
				continue
			for i in range(len_opts+1):
				long_opt = None
				long_opt = key.getMeta(name='opt/#{}/long'.format(i))
				if long_opt and long_opt.value.startswith(word):
					completion_options.append('--' + long_opt.value)
	k.close()
	return completion_options

# pass the string typed before pressing TAB, this way it can be overwritten
# searches for list of completions options
# currently only considering
# TODO consider command before
# returns list of completion options for commands
def complete_commands(start_of_input):
	global mount_point, root
	with kdb.KDB() as k:
		ks = kdb.KeySet()
		k.get(ks, mount_point)
		len_path_mount_point = len(mount_point.split('/'))
		completion_commands = []
		for key in ks:
			path = str(key).split('/')
			if len(path) <= len_path_mount_point or path[len_path_mount_point] != root:
				continue
			if start_of_input is None:
				completion_commands.append(path[-1])
			elif path[-1].startswith(start_of_input):
				completion_commands.append(path[-1])
		k.close()
		return completion_commands

# input_start_of_input - string typed before pressing TAB
# input_root - name of the program
# input_start_of_current_input - string typed before pressing TAB
# input_last_command - last command (not option or argument) typed
# input_typed - list of previously typed commands, options and arguments
# 
# used for testing, this can be run from python: 
#
# from find_autocompletion_options import *
# set_input_and_run(...)
#
# for this import to work, the scripts need to be in the same directory
def set_input_and_run(input_mount_point, input_root, input_start_of_current_input, 
	input_last_command, input_typed):
	global mount_point, root, last_command, typed, start_of_current_input
	mount_point = input_mount_point
	root = input_root
	start_of_current_input = input_start_of_current_input
	last_command = input_last_command
	typed = input_typed
	return find_auto_completion_options()

if __name__ == '__main__':
	get_command_line_arguments()
	result = find_auto_completion_options()
	print(result)





