#/usr/bin/env bash
IFS=$'\n'

_helloworld_completions() {
	# completing for last input before a tab
	COMPREPLY=()
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[COMP_CWORD-1]}"
	OUTPUT="$(python3 find_autocompletion_options.py ${cur})"
	#echo $OUTPUT
	COMPREPLY=($(compgen -W "${OUTPUT}"))
	#COMPREPLY+=$OUTPUT

}

complete -F _helloworld_completions helloworld