replace_newline_return () {
	awk '{if (NR>1) {printf("%s⏎", line);} line=$0;} END { printf("%s", line); }'
}

regex_escape () {
	sed -e 's/\\/\\\\/g' -e 's/\[/\\\[/g' -e 's/\]/\\\]/g' -e 's/\./\\\./g' -e 's/\*/\\\*/g' -e 's/\?/\\\?/g' \
	    -e 's/(/\\\(/g' -e 's/)/\\\)/g' -e 's/"/\\"/g'
}

if [ -z "@USE_CMAKE_KDB_COMMAND@" ]; then
	KDBCOMMAND="@KDB_COMMAND@"
	export PATH="`dirname \"$KDBCOMMAND\"`:$PATH"
else
	KDBCOMMAND="kdb"
fi
