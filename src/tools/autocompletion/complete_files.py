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
	if word is not None and (os.path.isfile(word) or os.path.isdir(word)):
		path = word
	for root, directorys, files in os.walk(path):
		for f in files:
			file_path = os.path.join(root, f)
			if len(file_path) > 0 and (word is None or file_path.startswith(word)):
				if len(path.split(os.path.sep)) >= len(file_path.split(os.path.sep)):
					if file_path.startswith('./'):
						file_path = file_path[2:]
					completion_options.append(file_path)
		for d in directorys:
			dir_path = os.path.join(root, d)
			if len(dir_path) > 0 and (word is None or dir_path.startswith(word)):
				if len(path.split(os.path.sep))+1 >= len(dir_path.split(os.path.sep)):
					if dir_path.startswith('./'):
						dir_path = dir_path[2:]
					completion_options.append(dir_path + os.path.sep)
	return('\n'.join(completion_options))

if __name__ == '__main__':
	word = None
	if len(sys.argv) > 1:
		word = sys.argv[1]
	print(complete(word))

