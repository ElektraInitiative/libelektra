#!/bin/bash
INFILE="$1"

@INCLUDE_COMMON@

BLOCKS=$(sed -n '/```sh/,/```\n/p' "$1")
BUF=
SHELL_RECORDER_ERROR=0

COMMAND=
RET=
ERRORS=
WARNINGS=
STDOUT=
STDOUTRE=
STDERR=
DIFF=
OUTBUF=
MOUNTPOINT=

writeBlock()
{
	OUTFILE="$1"
	[ -n "$RET" ] && echo "RET: $RET" >> "$TMPFILE" || { [ -z "$ERRORS" ] && echo 'RET: 0' >> "$TMPFILE"; }
	[ -n "$ERRORS" ] && echo "ERRORS: $ERRORS" >> "$TMPFILE"
	[ -n "$WARNINGS" ] && echo "WARNINGS: $WARNINGS" >> "$TMPFILE"
	[ -n "$DIFF" ] && echo "DIFF: $DIFF" >> "$TMPFILE"
	[ -n "$STDERR" ] && echo "STDERR: $STDERR" >> "$TMPFILE"
	if [ -n "$OUTBUF" ]; then echo "STDOUT: $OUTBUF" >> "$TMPFILE"
	elif [ -n "$STDOUT" ]; then echo "STDOUT: $STDOUT" >> "$TMPFILE"
	elif [ -n "$STDOUTRE" ]; then echo "STDOUT-REGEX: $STDOUTRE" >> "$TMPFILE"
	fi
	COMMAND=$(sed s/sudo\ //g <<< "$COMMAND")
	echo "< $COMMAND" >> "$TMPFILE"

	COMMAND=
	RET=
	ERRORS=
	WARNINGS=
	STDOUT=
	STDOUTRE=
	STDERR=
	DIFF=
	OUTBUF=
}

translate()
{
	TMPFILE=$(mktemp)
	MOUNTPOINT=$(echo "$BUF" | head -n 1)
	if grep -Eq 'Backup-and-Restore:' <<< "$MOUNTPOINT"; then echo "Mountpoint: $(cut -d ':' -f2 <<< "$MOUNTPOINT")" >> "$TMPFILE"
	else echo 'Mountpoint: /examples' >> "$TMPFILE"
	fi
	COMMAND=
	RET=
	ERRORS=
	WARNINGS=
	STDOUT=
	STDOUTRE=
	STDERR=
	DIFF=
	OUTBUF=
	MOUNTPOINT=
	IFS=''
	while read -r line;
	do
		if grep -Eq '^\s*#>' <<< "$line"; then
			output=$(sed -n 's/\s*#> \(.*\)/\1/p' <<< "$line")
			[ -z "$OUTBUF" ] && OUTBUF="$output" || OUTBUF="${OUTBUF}⏎$output"
		fi

		if grep -Eq "^(\s*)#" <<< "$line"; then
			directive=$(sed -n 's/\s*# \(.*\)/\1/p' <<< "$line")
			cmd=$(cut -d ':' -f1 <<< "$directive")
			arg=$(cut -d ':' -f2- <<< "$directive" | sed 's/[[:space:]]*//')

			case "$cmd" in
				RET)
					RET="$arg"
					;;
				STDOUT)
				    	STDOUT="$arg"
					;;
				STDOUT-REGEX)
					STDOUTRE="$arg"
					;;
				STDERR)
					STDERR="$arg"
					;;
				ERRORS)
					ERRORS="$arg"
					;;
				WARNINGS)
					WARNINGS="$arg"
					;;
				DIFF)
					DIFF="$arg"
					;;
				*)
					;;
			esac
			continue
		fi
		if [ -n "$line" ];
		then
			[ -n "$COMMAND" ] && writeBlock "$TMPFILE"
			COMMAND=$(grep -Eo '[^ \t].*' <<< "$line")
			[ "${line: -1}" == '\' ] && COMMAND="${COMMAND%?}"
			while [ "${line: -1}" == '\' ];
			do
				read -r line
				if [ "${line: -1}" == '\' ]; then COMMAND=$(printf '%s\\n%s' "$COMMAND" "${line%?}")
				else COMMAND=$(printf '%s\\n%s\\n' "$COMMAND" "$line")
				fi
			done
			continue
		fi
	done <<< "$BUF"
	writeBlock "$TMPFILE"
	"@CMAKE_CURRENT_BINARY_DIR@"/shell_recorder.sh "$TMPFILE" || SHELL_RECORDER_ERROR=1
	rm "$TMPFILE"
}
INBLOCK=0
IFS=''

MOUNTPOINTS_BACKUP=$("$KDBCOMMAND" mount)

while read -r line;
do
	grep -Eq '\s*```sh$' <<< "$line" && { INBLOCK=1; continue; }
	grep -Eq '\s*```$' <<< "$line" && INBLOCK=0
	[ $INBLOCK -eq 0 ] && continue
	[ -z "$BUF" ] && BUF="$line" || BUF=$(printf '%s\n%s' "$BUF" "$line")
done <<< "$BLOCKS"

translate

MOUNTPOINTS=$("$KDBCOMMAND" mount)

if [ "$MOUNTPOINTS_BACKUP" != "$MOUNTPOINT" ];
then
IFS='
'
	TOUMOUNT=$(diff <(echo "$MOUNTPOINTS_BACKUP") <(echo "$MOUNTPOINTS") | grep -Eo "^>.*")
	for line in $TOUMOUNT;
	do
		mp=$(sed -n 's/.*with name \(.*\)/\1/p' <<< "$line")
		"$KDBCOMMAND" umount "$mp"
	done
fi

exit "$SHELL_RECORDER_ERROR"
