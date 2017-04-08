replace_newline_return () {
	awk 1 ORS='⏎' | sed 's/.$//' | tr -d '\n'
}

regex_escape () {
	sed 's/\[/\\\[/g' | sed 's/\]/\\\]/g' | sed 's/\./\\\./g' | sed 's/\*/\\\*/g' | sed 's/\?/\\\?/g' \
		| sed 's/(/\\\(/g' | sed 's/)/\\\)/g'
}
