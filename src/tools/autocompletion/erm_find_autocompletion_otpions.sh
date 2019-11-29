#/usr/bin/env bash
IFS=$'\n'
output_file="benchmark_results_erm"

_erm_find_completions() {
	COMPREPLY=()
	local cur="${COMP_WORDS[COMP_CWORD]}"
	local cur_str=""
	local run=false
	if ! [ -z "${cur}" ]; then
		cur_str="-s ${cur}"
	fi
	in=" "
	for ((i = 1; i < COMP_CWORD; i++)); do
		in+=" ${COMP_WORDS[i]}"
	done
	output="$(python3 find_autocompletion_options.py -m spec/tests/autocomplete/erm ${cur_str} ${in})"
	COMPREPLY=($(compgen -W "${output}"))
}
complete -F _erm_find_completions erm
