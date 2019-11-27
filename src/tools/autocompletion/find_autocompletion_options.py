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
	completion.extend(complete_options())
	#completion.extend(complete_commands(start_of_current_input))
	completion = '\n'.join(completion)
	return completion

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
		if opt and opt.value.startswith('#'):
			try:
				len_opts = int((opt.value)[1:])
			except:
				continue
			for i in range(len_opts+1):
				opt = key.getMeta(name='opt/#{}'.format(i))
				word = start_of_current_input.strip().replace('-', '')
				if opt and opt.value.startswith(word):
					completion_options.append('-' + opt.value)
		elif opt:
			word = start_of_current_input.strip().replace('-', '')
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
			word = start_of_current_input.strip().replace('-', '')
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
				word = start_of_current_input.strip().replace('-', '')
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

# TODO am I needed in this way
'''
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
'''

# TODO fixme
'''
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
'''

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


if __name__ == '__main__':
	get_command_line_arguments()
	result = find_auto_completion_options()
	print(result)





