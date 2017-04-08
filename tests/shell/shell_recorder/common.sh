replace_newline_return () {
	awk 1 ORS='⏎' | sed 's/.$//' | tr -d '\n'
}

regex_escape () {
	sed 's/\[/\\\[/g' | sed 's/\]/\\\]/g' | sed 's/\./\\\./g' | sed 's/\*/\\\*/g' | sed 's/\?/\\\?/g' \
		| sed 's/(/\\\(/g' | sed 's/)/\\\)/g'
}

if [ -z "@USE_CMAKE_KDB_COMMAND@" ]; then
	KDBCOMMAND="@KDB_COMMAND@"
	export PATH="$PATH:`dirname \"$KDBCOMMAND\"`"
else
	KDBCOMMAND="kdb"
fi
