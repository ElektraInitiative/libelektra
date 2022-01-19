#/usr/bin/env bash
IFS=$'\n'

_kdb_find_completions() {
	COMPREPLY=()
	local cur="${COMP_WORDS[COMP_CWORD]}"
	local cur_str=""
	if ! [ -z "${cur}" ]; then
		cur_str="-s ${cur}"
	fi
	in=" "
	for ((i = 1; i < COMP_CWORD; i++)); do
		in+=" ${COMP_WORDS[i]}"
	done
	output="$(python3 src/autocompletion.py -m system:/spec/autocomplete/kdb ${cur_str} ${in})"
	COMPREPLY=($(compgen -W "${output}"))
}
complete -F _kdb_find_completions kdb
