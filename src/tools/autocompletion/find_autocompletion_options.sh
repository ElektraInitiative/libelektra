#/usr/bin/env bash
IFS=$'\n'

_find_completions() 
{
	COMPREPLY=()
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[COMP_CWORD-1]}"
	OUTPUT="$(python3 find_autocompletion_options.py ${cur})"
	COMPREPLY=($(compgen -W "${OUTPUT}"))
}

# TODO the kdb should not hardcoded
complete -F _find_completions helloworld