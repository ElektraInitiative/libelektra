from autocompletion import *
import string

# option -s, run silent, only see if passed or not

silent = False
tests_run = 0
passed = 0
failed = 0

#all_commands = ['list-tools','get', 'set', 'ls', 'backup', 'plugin-info', 'plugin-list', 'check-env-dep', 'check', 'complete', 'convert', 'cp', 'editor', 'export', 'file', 'find', 'fstab', 'getmeta', 'global-mount', 'global-unmount', 'help', 'import', 'info', 'list-commands', 'list', 'lsmeta', 'merge', 'mount-list-all-files', 'mount', 'mountpoint-info', 'mv', 'remount', 'reset-elektra', 'reset', 'restore', 'rm', 'rmmeta', 'setmeta', 'sget', 'shell', 'spec-mount', 'stash', 'test', 'umount-all', 'umount', 'vset']
all_commands = ['get', 'list-tools', 'ls', 'plugin-info', 'plugin-list', 'set']

def run_test(start_of_input, last_word, name_of_test_case):
	global passed, failed, tests_run
	tests_run+=1
	# get the result from executing autocompletion
	result = sorted(set_input_and_run('system:/spec/autocomplete/kdb', 'kdb', start_of_input, last_word, [], True, 'none', None, None).split())
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

	run_test('', None, 'all')
	for i in list(string.ascii_lowercase):
		run_test(i, None, 'start with: '+i)
	run_test('lis', None, 'start with: lis')
	run_test('rm', None, 'start with: rm')
	run_test('plugi', None, 'start with: plugi')

	print('\nTests run: ' + str(tests_run))
	if failed > 0:
		print('FAILURES: ' + str(failed))
		print('SUCCESSES: ' + str(passed))
	else:
		print('ALL TESTS SUCCEEDED')
