#/usr/bin/env bash

_helloworld_completions() {
	# completing for last input before a tab
	OUTPUT="$(python3 find_autocompletion_options.py ${COMP_WORDS[-1]})"
	COMPREPLY+=$OUTPUT
}

complete -F _helloworld_completions helloworld