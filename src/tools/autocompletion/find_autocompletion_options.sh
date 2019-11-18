#/usr/bin/env bash
IFS=$'\n'

_find_completions() {
	COMPREPLY=()
	local cur="${COMP_WORDS[COMP_CWORD]}"
	local prev="${COMP_WORDS[COMP_CWORD - 1]}"
	local cur_str=""
	local prev_str=""
	if ! [ -z "${cur}" ]; then
		cur_str="-s ${cur}"
	fi
	if ! [ -z "${prev}" ]; then
		prev_str="-l ${prev}"
	fi
	output="$(python3 find_autocompletion_options.py -m spec/tests/autocomplete ${cur_str} ${prev_str})"
	COMPREPLY=($(compgen -W "${output}"))
}
complete -F _find_completions kdb
