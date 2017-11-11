#!/bin/sh

@INCLUDE_COMMON@

set -f

# -- Functions -----------------------------------------------------------------------------------------------------------------------------

cleanup()
{
	rm -f ./stdout ./stderr
}

execute()
{
	proto="$*"

	if [ -z "$Mountpoint" ];
	then
		printf 'Error: no mountpoint specified in script\n'
		exit 1
	fi

	if [ "$BACKUP" -eq '1' ];
	then
		if ! "$KDBCOMMAND" export "$Mountpoint" dump > "$TMPFILE" 2>/dev/null;
		then
			printf 'ERROR: Failed to backup %s\nStopping test case.\n' "$Mountpoint"
			exit 1
		fi
		BACKUP=0
		"$KDBCOMMAND" rm -r "$Mountpoint" 2>/dev/null
	fi

	[ -z "$Storage" ] && Storage="dump"
	command=$(printf '%s' "$proto" | sed -e "s~\$Mountpoint~${Mountpoint}~g" \
	                                     -e "s~\$File~${DBFile}~g"           \
	                                     -e "s~\$Storage~${Storage}~g"       \
	                                     -e "s~\$MountArgs~${MountArgs}~g")

	printf '%s\n' "$command"

	[ -s "$OutFile" ] && printf '\n' >> "$OutFile"
	printf 'CMD: %s\n' "$command" >> "$OutFile"

	sh -c -f "$command" 2>stderr 1>stdout

	RETVAL="$?"

# =======
# = RET =
# =======

	printf 'RET: %s\n' "$RETVAL" >> "$OutFile"

	if [ -n "$RETCMP" ];
	then
		nbTest=$(( nbTest + 1 ))
		if ! printf '%s' "$RETVAL" | grep -Ewq $RETCMP;
		then
			printf 'Return value “%s” does not match “%s”\n' "$RETVAL" "$RETCMP"
			printf '=== FAILED return value does not match expected pattern %s\n' "$RETCMP" >> "$OutFile"
			nbError=$(( nbError + 1 ))
		fi
	fi

# ==========
# = STDERR =
# ==========

	STDERR=$(cat ./stderr)


	[ -n "$STDERR" ] && printf 'STDERR: %s\n' "$STDERR" >> "$OutFile"
	if [ -n "$STDERRCMP" ];
	then
		nbTest=$(( nbTest + 1 ))
		if ! printf '%s' "$STDERR" | replace_newline_return | grep -Eq --text "$STDERRCMP";
		then
			printf '\nERROR - STDERR:\n“%s”\ndoes not match\n“%s”\n\n' "$STDERR" "$STDERRCMP"
			printf '=== FAILED stderr does not match expected pattern %s\n' "$STDERRCMP" >> "$OutFile"
			nbError=$(( nbError + 1 ))
		fi
	fi

# ==========
# = STDOUT =
# ==========

	STDOUT=$(cat ./stdout)

	[ -n "$STDOUT" ] && printf '%s\n' "STDOUT: $STDOUT" >> "$OutFile"
	if [ -n "$STDOUTCMP" ];
	then
		nbTest=$(( nbTest + 1 ))
		if ! printf '%s' "$STDOUT" | replace_newline_return | grep -Fqx --text "$STDOUTCMP";
		then
			printf '\nERROR - STDOUT:\n“%s”\ndoes not match\n“%s”\n\n' "$STDOUT" "$STDOUTCMP"
			printf '=== FAILED stdout does not match expected pattern %s\n' "$STDOUTCMP" >> "$OutFile"
			nbError=$(( nbError + 1 ))
		fi
	fi

# ================
# = STDOUT-REGEX =
# ================

	if [ -n "$STDOUTRECMP" ];
	then
		nbTest=$(( nbTest + 1 ))
		if !  printf '%s' "$STDOUT" | replace_newline_return | grep -Eq --text "$STDOUTRECMP";
		then
			printf '\nERROR - STDOUT:\n“%s”\ndoes not match\n“%s”\n\n' "$STDOUT" "$STDOUTRECMP"
			printf '=== FAILED stdout does not match expected pattern %s\n' "$STDOUTRECMP" >> "$OutFile"
			nbError=$(( nbError + 1 ))
		fi
	fi

# ============
# = WARNINGS =
# ============

	WARNINGS=$(printf '%s' "$STDERR" | sed -nE  's/.*Warning (number: |\(#)([0-9]+).*/\2/p' | tr '\n' ',' | sed 's/.$//')

	[ -n "$WARNINGS" ] && printf 'WARNINGS: %s\n' "$WARNINGS" >> "$OutFile"
	if [ -n "$WARNINGSCMP" ];
	then
		nbTest=$(( nbTest + 1 ))
		if ! printf '%s' "$WARNINGS" | replace_newline_return | grep -Eq --text "$WARNINGSCMP";
		then
			printf '\nERROR - WARNINGS:\n“%s”\ndoes not match\n“%s”\n\n' "$WARNINGS" "$WARNINGSCMP"
			printf '=== FAILED Warnings do not match expected pattern %s\n' "$WARNINGSCMP" >> "$OutFile"
			nbError=$(( nbError + 1 ))
		fi
	fi

# ==========
# = ERROR =
# ==========

	ERROR=$(printf '%s' "$STDERR" | sed -nE 's/.*error \(#([0-9]+).*/\1/p')

	[ -n "$ERROR" ] && printf 'ERROR: %s\n' "$ERROR" >> "$OutFile"
	if [ -n "$ERRORCMP" ];
	then
		nbTest=$(( nbTest + 1 ))
		if ! printf '%s' "$ERROR" | replace_newline_return | grep -Eq --text "$ERRORCMP";
		then
			printf '\nERROR - ERROR:\n“%s”\ndoes not match\n“%s”\n\n' "$ERROR" "$ERRORCMP"
			printf '=== FAILED Errors do not match expected pattern %s\n' "$ERRORCMP" >> "$OutFile"
			nbError=$(( nbError + 1 ))
		fi
	fi
}

tail()
{
	printf '%s' "$*" | cut -d ' ' -f2-
}

first() {
	printf '%s' "$*" | cut -d ' ' -f1
}

second() {
	printf '%s' "$*" | cut -d ' ' -f2
}

run_script()
{
	while read -r line;
	do
	OP=
	ARG=
	cmd=$(first "$line")
	case "$cmd" in
	Mountpoint:)
		Mountpoint=$(second "$line")
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
		STDERRCMP=$(tail "$line")
		;;
	\<)
		OP="$cmd"
		ARG=$(tail "$line")
		;;
	esac
	if [ "$OP" = "<" ];
	then
		execute "$ARG"
		RETCMP=
		ERRORCMP=
		WARNINGSCMP=
		STDOUTCMP=
		STDOUTRECMP=
		STDERRCMP=
	fi
	done < "$FILE"
}

# -- Main ----------------------------------------------------------------------------------------------------------------------------------

trap cleanup EXIT INT QUIT TERM

# Parse optional argument `-p`
OPTIND=1
keepProtocol='false'
while getopts "p" opt; do
	case "$opt" in
	p)
		keepProtocol='true'
		;;
	esac
done
shift $((OPTIND-1))

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
STDOUTCMP=
STDOUTRECMP=
STDERRCMP=

BACKUP=0
TMPFILE=$(mktempfile_elektra)

# variables to count up errors and tests
nbError=0
nbTest=0

if [ "$#" -lt '1' ] || [ "$#" -gt '2' ];
then
	printf 'Usage: %s [-p] input_script [protocol to compare]\n\n' "$0"
	printf '       -p    keep protocol file\n' "$0"
	rm "$OutFile"
	exit 0
fi

BACKUP=1

run_script

"$KDBCOMMAND" rm -r "$Mountpoint" 2>/dev/null
"$KDBCOMMAND" import "$Mountpoint" dump 2>/dev/null < "$TMPFILE"

EVAL=0

if [ "$#" -eq '1' ];
then
	printf 'shell_recorder %s RESULTS: %s test(s) done %s error(s).' "$1" "$nbTest" "$nbError"
	EVAL=$nbError
fi

if [ "$#" -eq '2' ];
then
	RESULT=$(diff -N --text "$2" "$OutFile" 2>/dev/null)
	if [ "$?" -ne '0' ];
	then
		printf '=======================================\nReplay test failed, protocols differ\n%s\n\n\n\n' "$RESULT"
		EVAL=1
	else
		printf '=======================================\nReplay test succeeded\n'
	fi
fi

if [ "$EVAL" -eq 0 ] && [ $keepProtocol == 'false' ]; then
	rm -f "$OutFile"
else
	>&2 printf '\n📕  Protocol File: %s\n' "$OutFile"
fi

rm "${TMPFILE}"
exit "$EVAL"
