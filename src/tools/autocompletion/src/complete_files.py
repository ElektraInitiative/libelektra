import os
import sys

# ARGUMENTS
# word can be passed, it is the current input to complete
# if word passed will return all files and directorys in working directory starting with argv[1]
# if no argument passed will return all files and directorys in working directory
# returns string seperated with \n as expected from all scripts being called by find_autocompletion_options.py
def complete(word):
	completion_options = []
	path = './'
	overwritten = False
	prefix = ''
	remove_prefix_word = word
	if word is not None:
		prefix = word.split('/')
		prefix = '/'.join(prefix[:-1])
		prefix += '/'
		if os.path.isdir(prefix):
			path = prefix
		overwritten = True
		remove_prefix_word = word.replace(prefix, '')
	assert os.path.isdir(path) or os.path.isfile(path)
	for item in os.listdir(path):
		if os.path.isfile(os.path.join(path, item)):
			file_path = os.path.join(path, item)
			if not overwritten:
				file_path = file_path[2:]
			remove_prefix_file_path = file_path.replace(prefix, '')
			if word is None or remove_prefix_file_path.startswith(remove_prefix_word):
				if len(path.split(os.path.sep)) >= len(file_path.split(os.path.sep)):
					completion_options.append(file_path)
		if os.path.isdir(os.path.join(path, item)):
			dir_path = os.path.join(path, item)
			if not overwritten:
				dir_path = dir_path[2:]
			remove_prefix_dir_path = dir_path.replace(prefix, '')
			if word is None or dir_path.startswith(word):	
				if len(path.split(os.path.sep))+1 >= len(dir_path.split(os.path.sep)):
					completion_options.append(dir_path + os.path.sep)
	return('\n'.join(completion_options))

if __name__ == '__main__':
	word = None
	if len(sys.argv) > 1:
		word = sys.argv[1]
	print(complete(word))

