replace_newline_return () {
	awk '{if (NR>1) {printf("%s⏎", line);} line=$0;} END { printf("%s", line); }'
}

regex_escape () {
	sed 's/\\/\\\\/g' | sed 's/\[/\\\[/g' | sed 's/\]/\\\]/g' | sed 's/\./\\\./g' | sed 's/\*/\\\*/g' | sed 's/\?/\\\?/g' \
		| sed 's/(/\\\(/g' | sed 's/)/\\\)/g' |  sed 's/"/\\"/g'
}

if [ -z "@USE_CMAKE_KDB_COMMAND@" ]; then
	KDBCOMMAND="@KDB_COMMAND@"
	export PATH="`dirname \"$KDBCOMMAND\"`:$PATH"
else
	KDBCOMMAND="kdb"
fi
