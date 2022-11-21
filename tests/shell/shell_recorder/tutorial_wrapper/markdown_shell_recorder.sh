#!/usr/bin/env sh

@INCLUDE_COMMON@

# -- Functions -----------------------------------------------------------------------------------------------------------------------------

resetGlobals() {
	COMMAND=
	RET=
	ERROR=
	WARNINGS=
	STDOUTRE=
	unset STDERR
	unset STDOUT
	MOUNTPOINT=
}

writeBlock() {
	OUTFILE="$1"
	[ -n "$RET" ] && printf 'RET: %s\n' $RET >> "$OUTFILE" || { [ -z "$ERROR" ] && printf 'RET: 0\n' >> "$OUTFILE"; }
	[ -n "$ERROR" ] && printf 'ERROR: %s\n' "$ERROR" >> "$OUTFILE"
	[ -n "$WARNINGS" ] && printf 'WARNINGS: %s\n' "$WARNINGS" >> "$OUTFILE"
	[ -n "${STDERR+unset}" ] && printf 'STDERR: %s\n' "$STDERR" >> "$OUTFILE"
	if [ -n "${STDOUT+unset}" ]; then
		printf 'STDOUT: %s\n' "$STDOUT" >> "$OUTFILE"
	elif [ -n "$STDOUTRE" ]; then
		printf 'STDOUT-REGEX: %s\n' "$STDOUTRE" >> "$OUTFILE"
	fi
	COMMAND=$(printf '%s' "$COMMAND" | sed s/sudo\ //g)
	CMDFILE=$(mktempfile_elektra)
	printf '%s\n' "$COMMAND" > "$CMDFILE"
	while read -r cmd; do
		MATCH_COMMAND='(kdb[ \t]+(set(meta)?|rm)|kdbSet|keySetName)'
		MATCH_OPTIONS='([ \t]+-[-a-zA-Z]+)*'
		MATCH_SEPARATION='[ \t]+[''"]?'
		MATCH_NAMESPACE='(/[^/]+|[^/]+/[^/]+)'
		NAMESPACE=$(printf '%s' "$cmd" | sed -nE "s~.*$MATCH_COMMAND$MATCH_OPTIONS$MATCH_SEPARATION$MATCH_NAMESPACE.*~\5~p")
		if [ -n "$NAMESPACE" ] && printf '%s' "$NAMESPACE" | grep -E -vq '(dir:|system:|spec:|user:)?/(tests|elektra)'; then
			printerr 'The command “%s” stores data outside of `/tests` at “%s”!\n' "$COMMAND" "$NAMESPACE"
			SHELL_RECORDER_ERROR=1
		fi
		printf '< %s\n' "$cmd" >> "$OUTFILE"
	done < "$CMDFILE"
	rm "$CMDFILE"
	resetGlobals
}

translate() {
	TMPFILE=$(mktemp)
	MOUNTPOINT=$(printf '%s' "$BUF" | head -n 1)
	if printf '%s' "$MOUNTPOINT" | grep -Eq 'Backup-and-Restore:'; then
		printf 'Mountpoint: %s\n' "$(printf '%s' "$MOUNTPOINT" | cut -d ':' -f2- | sed 's/^[[:space:]]*//')" >> "$TMPFILE"
		BUF=$(printf '%s\n' "$BUF" | sed '1d')
	else
		printf 'Mountpoint: /tests\n' >> "$TMPFILE"
	fi

	resetGlobals
	BUFFILE=$(mktempfile_elektra)
	printf '%s\n' "$BUF" > "$BUFFILE"
	while read -r line; do
		if printf '%s' "$line" | grep -Eq '^[[:space:]]*#>'; then
			output=$(printf '%s' "$line" | sed -E -e 's/([ ]*#>$)/\1 /' -e 's/[ ]*#> (.*)/\1/')
			[ -z "$STDOUT" ] && STDOUT="$output" || STDOUT="${STDOUT}⏎$output"
		fi

		if printf '%s' "$line" | grep -Eq '^([[:space:]]*)#'; then
			directive=$(printf '%s' "$line" | sed -E 's/[ ]*# (.*)/\1/')
			cmd=$(printf '%s' "$directive" | cut -d ':' -f1)
			arg=$(printf '%s' "$directive" | cut -d ':' -f2- | sed 's/[[:space:]]*//')

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
			*) ;;

			esac
			continue
		fi
		if [ -n "$line" ]; then
			[ -n "$COMMAND" ] && writeBlock "$TMPFILE"
			COMMAND=$(printf '%s' "$line" | grep -Eo '[^ \t].*')
			printf '%s' "$line" | grep -E -q '\\$' && COMMAND=$(printf '%s' "$COMMAND" | sed 's/.$//')
			while printf '%s' "$line" | grep -E -q '\\$'; do
				read -r line
				if printf '%s' "$line" | grep -E -q '\\$'; then
					COMMAND=$(printf '%s\n%s' "$COMMAND" $(printf '%s' "$line" | sed 's/.$//'))
				else
					COMMAND=$(printf '%s\n%s\n' "$COMMAND" "$line")
				fi
			done
			continue
		fi
	done < "$BUFFILE"
	rm "$BUFFILE"
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

BLOCKSFILE=$(mktempfile_elektra)
printf '%s\n' "$BLOCKS" > "$BLOCKSFILE"
while read -r line; do
	printf '%s' "$line" | grep -Eq '[[:space:]]*```sh$' && {
		INBLOCK=1
		continue
	}
	printf '%s' "$line" | grep -Eq '[[:space:]]*```$' && INBLOCK=0
	[ $INBLOCK -eq 0 ] && continue
	[ -z "$BUF" ] && BUF="$line" || BUF=$(printf '%s\n%s' "$BUF" "$line")
done < "$BLOCKSFILE"
rm "$BLOCKSFILE"

translate

exit "$SHELL_RECORDER_ERROR"
