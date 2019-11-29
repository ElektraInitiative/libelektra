#/usr/bin/env bash
IFS=$'\n'
output_file='benchmark_results_erm'

_erm_find_completions_benchmark() {

	{ time _erm_find_completions ; } |& grep real | sed -E 's/[^0-9\.]+//g' | tr -d '\n' | (cat && echo " * 1000") | bc >> $output_file;
	_erm_find_completions
}

_erm_find_completions() {
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
	echo "EXECUTING -- python3 find_autocompletion_options.py -m spec/tests/autocomplete/erm ${cur_str} ${in}" >> $output_file;
	output="$(python3 find_autocompletion_options.py -m spec/tests/autocomplete/erm ${cur_str} ${in})"
	COMPREPLY=($(compgen -W "${output}"))
}
complete -F _erm_find_completions_benchmark erm
