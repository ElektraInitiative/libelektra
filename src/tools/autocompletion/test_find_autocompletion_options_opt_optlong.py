from find_autocompletion_options import *
import string

# option -s, run silent, only see if passed or not

silent = False
tests_run = 0
passed = 0
failed = 0

all_commands = ['list-tools','get', 'set', 'ls', 'backup', 'plugin-info', 'plugin-list', 'check-env-dep', 'check', 'complete', 'convert', 'cp', 'editor', 'export', 'file', 'find', 'fstab', 'getmeta', 'global-mount', 'global-unmount', 'help', 'import', 'info', 'list-commands', 'list', 'lsmeta', 'merge', 'mount-list-all-files', 'mount', 'mountpoint-info', 'mv', 'remount', 'reset-elektra', 'reset', 'restore', 'rm', 'rmmeta', 'setmeta', 'sget', 'shell', 'spec-mount', 'stash', 'test', 'umount-all', 'umount', 'vset']

def run_test(start_of_input, name_of_test_case):
	global passed, failed, tests_run
	tests_run+=1
	# get the result from executing autocompletion
	result = sorted(set_input_and_run(start_of_input, None).split())
	if not silent:
		print('TESTCASE: ' + name_of_test_case)
		print('RESULTS:')
		print(result)
	# get the expected result to compare to
	expected_result = []
	for i in all_commands:
		if i.startswith(start_of_input):
			expected_result.append(i)
	expected_result = sorted(expected_result)
	if not silent:
		print('EXPECTED RESULTS:')
		print(expected_result)
	# compare length of result & expected result
	if len(result) != len(expected_result) and not silent:
		print('DIFFERENT LENGTHS OF RESULT & EXPECTED RESULT')
		print('LENGHT OF RESULT: ' + str(len(result)))
		print('LENGHT OF EXPECTED RESULT: ' + str(len(expected_result)))
	# see which are missing
	for i in expected_result:
		if i not in result:
			if not silent:
				print('MISSING: ' + i)
	# see which are too much
	for i in result:
		if i not in expected_result:
			if not silent:
				print('NOT EXPECTED: ' + i)
	if sorted(result) == sorted(expected_result):
		print('SUCCESS - ' + name_of_test_case)
		passed += 1
	else:
		print('FAILURE - ' + name_of_test_case)
		failed += 1

if __name__ == "__main__":
	for i in sys.argv:
		if i == '-s':
			silent = True
	# test 1: ask for all options
	run_test('', 'all')
	# test 2-27: every letter of the alphabet
	for i in list(string.ascii_lowercase):
		run_test(i, 'start with: '+i)
	# test 28: start with gl
	run_test('gl', 'start with: gl')
	# test 29: start with rm
	run_test('rm', 'start with: rm')
	# test 30: complete mount
	run_test('mount', 'start with: mount')

	print('\nTests run: ' + str(tests_run))
	if failed > 0:
		print('FAILURES: ' + str(failed))
		print('SUCCESSES: ' + str(passed))
	else:
		print('ALL TESTS SUCCEEDED')
