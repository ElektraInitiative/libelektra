#!/bin/sh

@INCLUDE_COMMON@

set -f

# -- Functions -----------------------------------------------------------------------------------------------------------------------------

cleanup() {
	rm -f ./stdout ./stderr
	rm -rf "$EXPORT_DIR"
}

execute() {
	proto="$*"

	if [ -z "$Mountpoint" ]; then
		printf 'Error: no mountpoint specified in script\n'
		exit 1
	fi

	if [ "$BACKUP" -eq '1' ]; then
		if ! "$KDB" export "$MountpointRoot" dump > "$TMPFILE" 2> /dev/null; then
			printf 'ERROR: Failed to backup %s\nStopping test case.\n' "$MountpointRoot"
			exit 1
		fi
		BACKUP=0
		"$KDB" rm -r "$MountpointRoot" 2> /dev/null
	fi

	[ -z "$Storage" ] && Storage="dump"
	command=$(printf '%s' "$proto" | sed \
		-e "s~\$Mountpoint~${Mountpoint}~g" \
		-e "s~\$File~${DBFile}~g" \
		-e "s~\$Storage~${Storage}~g" \
		-e "s~\$MountArgs~${MountArgs}~g")

	printf '%s\n' "$command"

	[ -s "$OutFile" ] && printf '\n' >> "$OutFile"
	printf 'CMD: %s\n' "$command" >> "$OutFile"

	command=$(printf '%s' "$command" | sed "s~kdb\ ~\"$KDB\" ~g")

	# ===============
	# = INTERACTIVE =
	# ===============

	if [ x"$(first "$command")" = xinteractive ]; then
		S=$(second "$command")
		if [ -z "$S" ]; then
			S=$SHELL
		fi
		if [ -z "$S" ]; then
			S=bash
		fi
		echo "spawning interactive shell $S"
		$S -i /dev/tty
		return
	fi

	# ========
	# = EXEC =
	# ========

	sh -c "$command" 2> stderr 1> stdout

	RETVAL="$?"

	# =======
	# = RET =
	# =======

	printf 'RET: %s\n' "$RETVAL" >> "$OutFile"

	if [ -n "$RETCMP" ]; then
		executedTests=$((executedTests + 1))
		if ! printf '%s' "$RETVAL" | grep -Ewq "$RETCMP"; then
			printerr '\nERROR - RET:\nReturn value “%s” does not match “%s”\n\n' "$RETVAL" "$RETCMP"
			printf '=== FAILED return value does not match expected pattern %s\n' "$RETCMP" >> "$OutFile"
			numberErrors=$((numberErrors + 1))
		fi
	fi

	# ==========
	# = STDERR =
	# ==========

	STDERR=$(cat ./stderr)

	[ -n "$STDERR" ] && printf 'STDERR: %s\n' "$STDERR" >> "$OutFile"
	if [ -n "${STDERRCMP+unset}" ]; then
		executedTests=$((executedTests + 1))
		if ! printf '%s' "$STDERR" | replace_newline_return | grep -Eq "^$STDERRCMP\$"; then
			printerr '\nERROR - STDERR:\n“%s”\ndoes not match\n“%s”\n\n' "$STDERR" "$STDERRCMP"
			printf '=== FAILED stderr does not match expected pattern %s\n' "$STDERRCMP" >> "$OutFile"
			numberErrors=$((numberErrors + 1))
		fi
	fi

	# ==========
	# = STDOUT =
	# ==========

	STDOUT=$(cat ./stdout)

	[ -n "$STDOUT" ] && printf '%s\n' "STDOUT: $STDOUT" >> "$OutFile"
	if [ -n "${STDOUTCMP+unset}" ]; then
		executedTests=$((executedTests + 1))
		if ! printf '%s' "$STDOUT" | replace_newline_return | grep -Fqx -- "$STDOUTCMP" ||
			test -z "$STDOUTCMP" -a -n "$STDOUT"; then
			printerr '\nERROR - STDOUT:\n“%s”\ndoes not match\n“%s”\n\n' "$STDOUT" "$STDOUTCMP"
			printf '=== FAILED stdout does not match expected pattern %s\n' "$STDOUTCMP" >> "$OutFile"
			numberErrors=$((numberErrors + 1))
		fi
	fi

	# ================
	# = STDOUT-REGEX =
	# ================

	if [ -n "$STDOUTRECMP" ]; then
		executedTests=$((executedTests + 1))
		if ! printf '%s' "$STDOUT" | replace_newline_return | grep -Eq -- "$STDOUTRECMP"; then
			printerr '\nERROR - STDOUT:\n“%s”\ndoes not match\n“%s”\n\n' "$STDOUT" "$STDOUTRECMP"
			printf '=== FAILED stdout does not match expected pattern %s\n' "$STDOUTRECMP" >> "$OutFile"
			numberErrors=$((numberErrors + 1))
		fi
	fi

	# ============
	# = WARNINGS =
	# ============

	WARNINGS=$(printf '%s' "$STDERR" | sed -nE 's/.*warning (C[A-Z0-9]+):/\1/p' | tr '\n' ',' | sed 's/.$//')

	[ -n "$WARNINGS" ] && printf 'WARNINGS: %s\n' "$WARNINGS" >> "$OutFile"

	if [ -n "$WARNINGSCMP" ]; then
		executedTests=$((executedTests + 1))
		if ! printf '%s' "$WARNINGS" | replace_newline_return | grep -Eq -- "$WARNINGSCMP"; then
			printerr '\nERROR - WARNINGS:\n“%s”\ndoes not match\n“%s”\n\n' "$WARNINGS" "$WARNINGSCMP"
			printf '=== FAILED Warnings do not match expected pattern %s\n' "$WARNINGSCMP" >> "$OutFile"
			numberErrors=$((numberErrors + 1))
		fi
	fi

	# ==========
	# = ERROR =
	# ==========

	ERROR=$(printf '%s' "$STDERR" | sed -nE 's/.*error (C[A-Z0-9]+):/\1/p')

	[ -n "$ERROR" ] && printf 'ERROR: %s\n' "$ERROR" >> "$OutFile"
	if [ -n "$ERRORCMP" ]; then
		executedTests=$((executedTests + 1))
		if ! printf '%s' "$ERROR" | replace_newline_return | grep -Eq -- "$ERRORCMP"; then
			printerr '\nERROR - ERROR:\n“%s”\ndoes not match\n“%s”\n\n' "$ERROR" "$ERRORCMP"
			printf '=== FAILED Errors do not match expected pattern %s\n' "$ERRORCMP" >> "$OutFile"
			numberErrors=$((numberErrors + 1))
		fi
	fi
}

tail() {
	printf '%s' "$*" | cut -sd ' ' -f2-
}

first() {
	printf '%s' "$*" | cut -d ' ' -f1
}

second() {
	printf '%s' "$*" | cut -sd ' ' -f2
}

run_script() {
	while [ -n "$continuation" ] && line="$continuation" && continuation= || read -r line; do
		OP=
		cmd=$(first "$line")
		case "$cmd" in
		Mountpoint:)
			Mountpoint=$(second "$line")
			MountpointRoot=$(printf "$Mountpoint" | sed -E 's~(/?[^/]+).*~\1/~')
			;;
		File:)
			DBFile=$(second "$line")
			if [ "$DBFile" = "File:" ] || [ -z "$DBFile" ]; then
				DBFile=$(mktempfile_elektra)
			fi
			;;
		Storage:)
			Storage=$(second "$line")
			;;
		MountArgs:)
			MountArgs=$(tail "$line")
			;;
		RET:)
			RETCMP=$(tail "$line")
			;;
		ERROR:)
			ERRORCMP=$(tail "$line")
			;;
		WARNINGS:)
			WARNINGSCMP=$(tail "$line")
			;;
		STDOUT:)
			STDOUTCMP=$(tail "$line")
			;;
		STDOUT-REGEX:)
			STDOUTRECMP=$(tail "$line")
			;;
		STDERR:)
			STDERRCMP=$(printf '%s' "$line" | sed -E 's/[^:]+: ?(.*)/\1/')
			;;
		\<)
			OP="$cmd"
			[ "$ARG" ] && ARG="$ARG$NEWLINE"
			ARG="$ARG$(tail "$line")"
			read -r continuation
			# Check for multiline commands
			first "$continuation" | grep -q '<' && continue
			;;
		esac
		if [ "$OP" = "<" ]; then
			execute "$ARG"
			RETCMP=
			ERRORCMP=
			WARNINGSCMP=
			unset STDOUTCMP
			STDOUTRECMP=
			unset STDERRCMP
			ARG=
		fi
	done < "$FILE"
}

# -- Main ----------------------------------------------------------------------------------------------------------------------------------

trap cleanup EXIT INT QUIT TERM

# Parse optional argument `-p`
OPTIND=1
printProtocol='false'
while getopts "p" opt; do
	case "$opt" in
	p)
		printProtocol='true'
		;;
	esac
done
shift $((OPTIND - 1))

FILE=$1
Mountpoint=
DBFile=
Storage=
MountArgs=
DiffType=File
OutFile=$(mktempfile_elektra)

RETCMP=
ERRORCMP=
WARNINGSCMP=
unset STDOUTCMP
STDOUTRECMP=

BACKUP=0
TMPFILE=$(mktempfile_elektra)

# variables to count up errors and tests
numberErrors=0
executedTests=0

if [ "$#" -lt '1' ] || [ "$#" -gt '2' ]; then
	printf 'Usage: %s [-p] input_script [protocol to compare]\n\n' "$0"
	printf '       -p    print protocol file\n' "$0"
	rm "$OutFile"
	exit 0
fi

BACKUP=1

EXPORT_DIR="$(mktempdir_elektra)"
export_config "$EXPORT_DIR"
MOUNTPOINTS_BACKUP=$("$KDB" mount)

run_script

"$KDB" rm -r "$MountpointRoot" 2> /dev/null
"$KDB" import "$MountpointRoot" dump 2> /dev/null < "$TMPFILE"

EVAL=0

if [ "$#" -eq '1' ]; then
	printf 'shell_recorder %s RESULTS: %s test(s) done %s error(s).' "$1" "$executedTests" "$numberErrors"
	EVAL=$numberErrors
fi

if [ "$#" -eq '2' ]; then
	RESULT=$(diff -N --text "$2" "$OutFile" 2> /dev/null)
	if [ "$?" -ne '0' ]; then
		printerr '=======================================\nReplay test failed, protocols differ\n%s\n\n\n\n' "$RESULT"
		EVAL=1
	else
		printf '=======================================\nReplay test succeeded\n'
	fi
fi

if [ "$EVAL" -ne 0 ] || [ $printProtocol = 'true' ]; then
	printerr '\n\n—— Protocol ————————————————————————————————————————————————————\n'
	cat >&2 "$OutFile"
	printerr '————————————————————————————————————————————————————————————————\n'
fi
rm -f "$OutFile"

# We disable the cleanup procedure temporarily, since we still need the exported configuration,
# if the tests changed the configuration permanently.
trap - EXIT
export_check "$EXPORT_DIR" 'Test'
trap cleanup EXIT

rm "${TMPFILE}"
exit "$EVAL"
