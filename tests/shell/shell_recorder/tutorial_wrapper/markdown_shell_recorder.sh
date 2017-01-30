#!/bin/bash
INFILE="$1"


BLOCKS=$(sed -n '/```sh/,/```\n/p' "$1")
BUF=

COMMAND=
RET=
ERRORS=
WARNINGS=
STDOUT=
STDOUTRE=
STDOUTGLOB=
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
		tmp=$(echo "$tmp" | sed 's/\[/\\\[/g' | sed 's/\]/\\\]/g' | sed 's/\./\\\./g' | sed 's/\*/\\\*/g' | sed 's/\?/\\\?/g')
		echo "STDOUT: $tmp" >> "$TMPFILE"
	    elif [ ! -z "$STDOUT" ];
	    then
		tmp=$(awk -v RS="" '{gsub (/\n/,"⏎")}1' <<< "$STDOUT")
		tmp=$(echo "$tmp" | sed 's/\[/\\\[/g' | sed 's/\]/\\\]/g' | sed 's/\./\\\./g' | sed 's/\*/\\\*/g' | sed 's/\?/\\\?/g')
		echo "STDOUT: $tmp" >> "$TMPFILE"
	    else
		if [ ! -z "$STDOUTRE" ]
		then
			echo "STDOUT-REGEX: $STDOUT" >> "$TMPFILE"
		    else
			if [ ! -z "$STDOUTGLOB" ];
			then
			    echo "STDOUT-GLOB: $STDOUT"
			fi
		fi
	fi
	COMMAND=$(sed s/sudo\ //g <<<"$COMMAND")
	echo "< $COMMAND" >> "$TMPFILE"

	COMMAND=
	RET=
	ERRORS=
	WARNINGS=
	STDOUT=
	STDOUTRE=
	STDOUTGLOB=
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
		echo "Mountpoint: /examples" >> "$TMPFILE"
	fi
	COMMAND=
	RET=
	ERRORS=
	WARNINGS=
	STDOUT=
	STDOUTRE=
	STDOUTGLOB=
	STDERR=
	DIFF=
	OUTBUF=
	MOUNTPOINT=
	IFS=''
	while read -r line;
	do
		grep -Eq "^(\s)*#>" <<< "$line"
		if [ -z "$OUTBUF" ];
		then
	    		tmp=$(sed -n 's/\(\s\)*#> \(.*\)/\2/p' <<<"$line")
			OUTBUF="$tmp"
		    else
	    		tmp=$(sed -n 's/\(\s\)*#> \(.*\)/\2/p' <<<"$line")
			OUTBUF=$(echo -en "${OUTBUF}\n${tmp}")
		fi

		grep -Eq "^(\s*)#" <<< "$line"
		if [ "$?" -eq 0 ];
		then
	    		tmp=$(sed -n 's/\(\s\)*# \(.*\)/\2/p' <<<"$line")
			cmd=$(cut -d ':' -f1 <<< "$tmp")
			arg=$(cut -d ':' -f2- <<< "$tmp")

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
				STDOUT-GLOB)
					STDOUTGLOB="$arg"
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
		if [ ! -z "$line" ];
		then
			if [ ! -z "$COMMAND" ];
			then
				writeBlock "$TMPFILE"
			fi
			COMMAND=$(grep -Eo "[^ \\t].*" <<< "$line")
			COMMAND=$(sed "s/^sudo\ //" <<< "$COMMAND")
			COMMAND=$(sed "s/\`[[:blank:]]*sudo\ /\`/" <<< "$COMMAND")
			if [ "${line: -1}" == "\\" ];
			then
			    COMMAND="${COMMAND::-1}"
			fi
			while [ "${line: -1}" == "\\" ];
			do
			    read -r line
			    line=$(sed "s/^sudo\ //" <<< "$line")
			    line=$(sed "s/\`[[:blank:]]*sudo\ /\`/" <<< "$line")
			    if [ "${line: -1}" == "\\" ];
			    then
				COMMAND=$(printf "%s\\\n%s" "$COMMAND" "${line::-1}")
			    else
				COMMAND=$(printf "%s\\\n%s\\\n" "$COMMAND" "$line")
			    fi
			done
			continue
		fi
	done <<<"$BUF"
	writeBlock "$TMPFILE"
	../shell_recorder.sh "$TMPFILE"
	rm "$TMPFILE"
}
INBLOCK=0
IFS=''
while read -r line;
do
	grep -Eq '(\s)*```sh$' <<<"$line"
	if [ "$?" -eq 0 ];
	then
	    	INBLOCK=1
		continue;	
	fi
	grep -Eq '(\s)*```$' <<<"$line"
	if [ "$?" -eq 0 ];
	then
	    INBLOCK=0
	    continue
	fi
	if [ $INBLOCK -eq 0 ];
	then
	    continue
	fi
	if [ -z "$BUF" ];
	then
		BUF="$line"
	else
		BUF=$(printf "%s\n%s" "${BUF}" "${line}")
	fi
done <<<"$BLOCKS"

translate

