#!/bin/bash
INFILE="$1"


BLOCKS=$(grep -oPz '(?s)```sh.*?```\n' "$1")
BUF=

COMMAND=
RET=
ERRORS=
WARNINGS=
STDOUT=
STDERR=
DIFF=
OUTBUF=
MOUNTPOINT=

writeBlock()
{
	OUTFILE="$1"
	if [ ! -z "$RET" ];
	then
		echo "RET: $RET" >> "$TMPFILE"
	else
	    if [ -z "$ERRORS" ];
	    then
		echo "RET: 0" >> "$TMPFILE"
	    fi
	fi
	if [ ! -z "$ERRORS" ];
	then
		echo "ERRORS: $ERRORS" >> "$TMPFILE"
	fi
	if [ ! -z "$WARNINGS" ];
	then
		echo "WARNINGS: $WARNINGS" >> "$TMPFILE"
	fi
	if [ ! -z "$DIFF" ];
	then
		echo "DIFF: $DIFF" >> "$TMPFILE"
	fi
	if [ ! -z "$STDERR" ];
	then
		echo "STDERR: $STDERR" >> "$TMPFILE"
	fi
	if [ ! -z "$OUTBUF" ];
	then
		tmp=$(awk -v RS="" '{gsub (/\n/,"⏎")}1' <<< "$OUTBUF")
		echo "STDOUT: $tmp" >> "$TMPFILE"
	else
		if [ ! -z "$STDOUT" ]
		then
			echo "STDOUT: $STDOUT" >> "$TMPFILE"
		fi
	fi
	echo "< $COMMAND" >> "$TMPFILE"
	COMMAND=
	RET=
	ERRORS=
	WARNINGS=
	STDOUT=
	STDERR=
	DIFF=
	OUTBUF=
}	
translate()
{
	TMPFILE=$(mktemp)
	MOUNTPOINT=$(echo "$BUF" | head -n 1)
	grep -Eq "Backup-and-Restore:" <<< "$MOUNTPOINT"
	if [ "$?" -eq 0 ];
	then
		MOUNTPOINT=$(echo "$MOUNTPOINT" | cut -d ':' -f2)
		echo "Mountpoint: $MOUNTPOINT" >> "$TMPFILE"
	else
		echo "Mountpoint: /" >> "$TMPFILE"
	fi
	COMMAND=
	RET=
	ERRORS=
	WARNINGS=
	STDOUT=
	STDERR=
	DIFF=
	OUTBUF=
	MOUNTPOINT=
	IFS=''
	while read line;
	do
	    grep -Eq "((^(\s)*kdb)|(^(\s)*sudo kdb))" <<< "$line"
		if [ "$?" -eq 0 ];
		then
			if [ ! -z "$COMMAND" ];
			then
				writeBlock "$TMPFILE"
			fi
			COMMAND=$(grep -Po "(?<=kdb ).*" <<<"$line")
			continue
		fi
		grep -Eq "^(\s*)#" <<< "$line"
		if [ "$?" -eq 0 ];
		then
			tmp=$(grep -Po "(?<=\# )(.*)" <<< "$line")
			cmd=$(cut -d ':' -f1 <<< "$tmp")
			arg=$(cut -d ':' -f2- <<< "$tmp")

			case "$cmd" in
				RET)
					RET="$arg"
					;;
				STDOUT)
					STDOUT="$arg"
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
		grep -Eq "^(\s)*\\$" <<< "$line"
		if [ "$?" -eq 0 ];
		then
			if [ ! -z "$COMMAND" ];
			then
				writeBlock "$TMPFILE"
			fi
			COMMAND=$(grep -Eo "[^ \\t].*" <<< "$line")
			continue
		fi
		if [ -z "$OUTBUF" ];
		then
			OUTBUF="$line"
		    else
			OUTBUF=$(echo -en "${OUTBUF}\n${line}")
		fi
	done <<<"$BUF"
	writeBlock "$TMPFILE"
	../shell_recorder.sh "$TMPFILE"
	#	 rm "$TMPFILE"
}

IFS=''
while read line;
do
	grep -Eq '(\s)*```sh$' <<<"$line"
	if [ "$?" -eq 0 ];
	then
		BUF=""
	else
		grep -Eq '(\s)*```$' <<<"$line"
		if [ "$?" -eq 0 ];
		then
			BUF=$(echo "$BUF")
			translate
			BUF=""
		else
			if [ -z "$BUF" ];
			then
				BUF="$line"
			else
				BUF=$(echo -en "${BUF}\n${line}")
			fi
		fi
	fi
done <<<"$BLOCKS"
