#!/usr/bin/env bash

@INCLUDE_COMMON@

# -- Functions -----------------------------------------------------------------------------------------------------------------------------

resetGlobals()
{
	COMMAND=
	RET=
	ERROR=
	WARNINGS=
	STDOUTRE=
	unset STDERR
	unset STDOUT
	MOUNTPOINT=
}

writeBlock()
{
	OUTFILE="$1"
	[ -n "$RET" ] && printf 'RET: %s\n' $RET >> "$TMPFILE" || { [ -z "$ERROR" ] && printf 'RET: 0\n' >> "$TMPFILE"; }
	[ -n "$ERROR" ] && printf 'ERROR: %s\n' "$ERROR" >> "$TMPFILE"
	[ -n "$WARNINGS" ] && printf 'WARNINGS: %s\n' "$WARNINGS" >> "$TMPFILE"
	[ -n "${STDERR+unset}" ] && printf 'STDERR: %s\n' "$STDERR" >> "$TMPFILE"
	if [ -n "${STDOUT+unset}" ]; then printf 'STDOUT: %s\n' "$STDOUT" >> "$TMPFILE"
	elif [ -n "$STDOUTRE" ]; then printf 'STDOUT-REGEX: %s\n' "$STDOUTRE" >> "$TMPFILE"
	fi
	COMMAND=$(sed s/sudo\ //g <<< "$COMMAND")
	while read -r cmd; do
		MATCH_COMMAND='(kdb[ \t]+(set(meta)?|rm)|kdbSet|keySetName)'
		MATCH_OPTIONS='([ \t]+-[-a-zA-Z]+)*'
		MATCH_SEPARATION='[ \t]+[''"]?'
		MATCH_NAMESPACE='(/[^/]+|[^/]+/[^/]+)'
		NAMESPACE=$(printf '%s' "$cmd" | sed -nE "s~.*$MATCH_COMMAND$MATCH_OPTIONS$MATCH_SEPARATION$MATCH_NAMESPACE.*~\5~p")
		if [ -n "$NAMESPACE" ] && printf '%s' "$NAMESPACE" | egrep -vq '(dir|system|spec|user)?/(tests|elektra)'; then
			printerr 'The command “%s” stores data outside of `/tests` at “%s”!\n' "$COMMAND" "$NAMESPACE"
			SHELL_RECORDER_ERROR=1
		fi
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
		printf 'Mountpoint: /tests\n' >> "$TMPFILE"
	fi

	resetGlobals
	while read -r line;
	do
		if grep -Eq '^\s*#>' <<< "$line"; then
			output=$(sed -E -e 's/([ ]*#>$)/\1 /' -e 's/[ ]*#> (.*)/\1/' <<< "$line")
			[ -z "$STDOUT" ] && STDOUT="$output" || STDOUT="${STDOUT}⏎$output"
		fi

		if grep -Eq "^(\s*)#" <<< "$line"; then
			directive=$(sed -E 's/[ ]*# (.*)/\1/' <<< "$line")
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

printf 'Input: %s\n' "$1"

BLOCKS=$(sed -n '/```sh/,/```\n/p' "$1")
BUF=
SHELL_RECORDER_ERROR=0
INBLOCK=0
IFS=''

while read -r line;
do
	grep -Eq '\s*```sh$' <<< "$line" && { INBLOCK=1; continue; }
	grep -Eq '\s*```$' <<< "$line" && INBLOCK=0
	[ $INBLOCK -eq 0 ] && continue
	[ -z "$BUF" ] && BUF="$line" || BUF=$(printf '%s\n%s' "$BUF" "$line")
done <<< "$BLOCKS"

translate

exit "$SHELL_RECORDER_ERROR"
