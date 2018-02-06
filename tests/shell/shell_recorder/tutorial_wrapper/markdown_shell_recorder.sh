#!/usr/bin/env bash

@INCLUDE_COMMON@

# -- Functions -----------------------------------------------------------------------------------------------------------------------------

resetGlobals()
{
	COMMAND=
	RET=
	ERROR=
	WARNINGS=
	STDOUT=
	STDOUTRE=
	STDERR=
	OUTBUF=
	MOUNTPOINT=
}

writeBlock()
{
	OUTFILE="$1"
	[ -n "$RET" ] && printf 'RET: %s\n' $RET >> "$TMPFILE" || { [ -z "$ERROR" ] && printf 'RET: 0\n' >> "$TMPFILE"; }
	[ -n "$ERROR" ] && printf 'ERROR: %s\n' "$ERROR" >> "$TMPFILE"
	[ -n "$WARNINGS" ] && printf 'WARNINGS: %s\n' "$WARNINGS" >> "$TMPFILE"
	[ -n "$STDERR" ] && printf 'STDERR: %s\n' "$STDERR" >> "$TMPFILE"
	if [ -n "$OUTBUF" ]; then printf 'STDOUT: %s\n' "$OUTBUF" >> "$TMPFILE"
	elif [ -n "$STDOUTRE" ]; then printf 'STDOUT-REGEX: %s\n' "$STDOUTRE" >> "$TMPFILE"
	fi
	COMMAND=$(sed s/sudo\ //g <<< "$COMMAND")
	while read -r cmd; do
		printf '< %s\n' "$cmd" >> "$TMPFILE"
	done <<< "$COMMAND"
	resetGlobals
}

translate()
{
	TMPFILE=$(mktemp)
	MOUNTPOINT=$(printf '%s' "$BUF" | head -n 1)
	if grep -Eq 'Backup-and-Restore:' <<< "$MOUNTPOINT"; then
		printf 'Mountpoint: %s\n' "$(cut -d ':' -f2 <<< "$MOUNTPOINT" | sed 's/^[[:space:]]*//')" >> "$TMPFILE"
	else
		printf 'Mountpoint: /examples\n' >> "$TMPFILE"
	fi

	resetGlobals
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
				STDOUT-REGEX)
					STDOUTRE="$arg"
					;;
				STDERR)
					STDERR="$arg"
					;;
				ERROR)
					ERROR="$arg"
					;;
				WARNINGS)
					WARNINGS="$arg"
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
				if [ "${line: -1}" == '\' ]; then COMMAND=$(printf '%s\n%s' "$COMMAND" "${line%?}")
				else COMMAND=$(printf '%s\n%s\n' "$COMMAND" "$line")
				fi
			done
			continue
		fi
	done <<< "$BUF"
	writeBlock "$TMPFILE"
	"@CMAKE_CURRENT_BINARY_DIR@"/shell_recorder.sh "$TMPFILE" || SHELL_RECORDER_ERROR=1
	rm "$TMPFILE"
}

# -- Main ----------------------------------------------------------------------------------------------------------------------------------

resetGlobals

BLOCKS=$(sed -n '/```sh/,/```\n/p' "$1")
BUF=
SHELL_RECORDER_ERROR=0
INBLOCK=0
IFS=''

MOUNTPOINTS_BACKUP=$("$KDB" mount)

while read -r line;
do
	grep -Eq '\s*```sh$' <<< "$line" && { INBLOCK=1; continue; }
	grep -Eq '\s*```$' <<< "$line" && INBLOCK=0
	[ $INBLOCK -eq 0 ] && continue
	[ -z "$BUF" ] && BUF="$line" || BUF=$(printf '%s\n%s' "$BUF" "$line")
done <<< "$BLOCKS"

translate

MOUNTPOINTS=$("$KDB" mount)

if [ "$MOUNTPOINTS_BACKUP" != "$MOUNTPOINT" ];
then
IFS='
'
	TOUMOUNT=$(diff <(printf '%s' "$MOUNTPOINTS_BACKUP") <(printf '%s' "$MOUNTPOINTS") | grep -Eo "^>.*")
	for line in $TOUMOUNT;
	do
		mp=$(sed -n 's/.*with name \(.*\)/\1/p' <<< "$line")
		"$KDB" umount "$mp"
	done
fi

exit "$SHELL_RECORDER_ERROR"
