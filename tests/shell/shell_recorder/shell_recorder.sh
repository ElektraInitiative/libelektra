#!/bin/sh

set -f

FILE=$1
Mountpoint=
DBFile=
Storage=
MountArgs=
DiffType=File
OutFile=$(mktemp -t elektraenv.XXXXXXXXX 2>/dev/null || mktemp -t 'elektraenv')

RETCMP=
ERRORSCMP=
WARNINGSCMP=
STDOUTCMP=
STDERRCMP=
DIFFCMP=

BACKUP=0
TMPFILE=$(mktemp -t elektraenv.XXXXXXXXX 2>/dev/null || mktemp -t 'elektraenv')

# variables to count up errors and tests
nbError=0
nbTest=0

if [ -z "@USE_CMAKE_KDB_COMMAND@" ]; then
        KDBCOMMAND="@KDB_COMMAND@"
else
        KDBCOMMAND="kdb"
fi

replace_newline_return () {
	awk -v RS="" '{gsub (/\n/,"⏎")}1'
}

execute()
{
    proto="$@"
    if [ -z "$Mountpoint" ];
    then
        echo "Error: no mountpoint specified in script"
        exit 1
    fi
    if [ -z "$DBFile" ];
    then
        DBFile=$("$KDBCOMMAND" file $Mountpoint 2>/dev/null)
    fi

    if [ "$BACKUP" -eq "1" ];
    then
        "$KDBCOMMAND" export $Mountpoint > "$TMPFILE" 2>/dev/null
        BACKUP=0
        "$KDBCOMMAND" rm -r $Mountpoint 2>/dev/null
    fi

    [ -z "$Storage" ] && Storage="dump"
    command=$(echo $proto | sed "s~\$Mountpoint~${Mountpoint}~g")
    command=$(echo $command | sed "s~\$File~${DBFile}~g")
    command=$(echo $command | sed "s~\$Storage~${Storage}~g")
    command=$(echo $command | sed "s~\$MountArgs~${MountArgs}~g")

    case "$DiffType" in
        File)
            rm "${DBFile}.1" 2>/dev/null
            cp "${DBFile}" "${DBFile}.1" 2>/dev/null
            ;;
        Ini)
            rm ./previousState 2>/dev/null
            "$KDBCOMMAND" export $Mountpoint simpleini > ./previousState 2>/dev/null
            ;;
        Dump)
            rm ./previousState 2>/dev/null
            "$KDBCOMMAND" export $Mountpoint dump > ./previousState 2>/dev/null
            ;;
    esac

    echo "$command"

    printf "%s\0" "CMD: $command" >> "$OutFile"

    sh -c -f "\"$KDBCOMMAND\" $command 2>stderr 1>stdout"

    RETVAL="$?"

    printf "%s\0" "RET: $RETVAL" >> "$OutFile"

    if [ ! -z "$RETCMP" ];
    then
        nbTest=$(( nbTest + 1 ))
        echo "$RETVAL" | grep -Ewq $RETCMP
        if [ "$?" -ne "0" ];
        then
            echo "Return value $RETVAL doesn't match $RETCMP"
            printf "%s\0" "=== FAILED return value doesn't match expected pattern $RETCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi

    DIFF=
    case "$DiffType" in
        File)
            DIFF=$(diff -N --text "${DBFile}" "${DBFile}.1" 2>/dev/null)
            ;;
        Ini)
            "$KDBCOMMAND" export $Mountpoint simpleini > ./newState 2>/dev/null
            DIFF=$(diff -N --text ./previousState ./newState 2>/dev/null)
            rm ./newState 2>/dev/null
            rm ./previousState 2>/dev/null
            ;;
        Dump)
            "$KDBCOMMAND" export $Mountpoint dump > ./newState 2>/dev/null
            DIFF=$(diff -N --text ./previousState ./newState 2>/dev/null)
            rm ./newState 2>/dev/null
            rm ./previousState 2>/dev/null
            ;;
    esac



    STDERR=$(cat ./stderr)


    printf "%s\0" "STDERR: $STDERR" >> "$OutFile"
    if [ ! -z "$STDERRCMP" ];
    then
        nbTest=$(( nbTest + 1 ))
        echo "$STDERR" | replace_newline_return | grep -Eq --text "$STDERRCMP"
        if [ "$?" -ne "0" ];
        then
            printf "\nERROR - STDERR:\n%s\ndoesn't match %s\n\n" "$STDERR" "$STDERRCMP"
	    printf "%s\0" "=== FAILED stderr doesn't match expected patter $STDERRCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi



    STDOUT=$(cat ./stdout)

    printf "%s\0" "STDOUT: $STDOUT" >> "$OutFile"
    if [ ! -z "$STDOUTCMP" ];
    then
        nbTest=$(( nbTest + 1 ))
        echo "$STDOUT" | replace_newline_return | grep -Eq --text "$STDOUTCMP"
        if [ "$?" -ne "0" ];
        then
            printf "\nERROR - STDOUT:\n%s\ndoesn't match %s\n\n" "$STDOUT" "$STDOUTCMP"
            printf "%s\0" "=== FAILED stdout doesn't match expected pattern $STDOUTCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi



    WARNINGS=$(echo "$STDERR" | sed -nE  "s/Warning number: (\d*)/\1/p" | tr '\n' ',')

    printf "%s\0" "WARNINGS: $WARNINGS" >> "$OutFile"
    if [ ! -z "$WARNINGSCMP" ];
    then
        nbTest=$(( nbTest + 1 ))
        echo "$WARNINGS" | replace_newline_return | grep -Eq --text "($WARNINGSCMP)"
        if [ "$?" -ne "0" ];
        then
            printf "\nERROR - WARNINGS:\n%s\ndoesn't match %s\n\n" "$WARNINGS" "$WARNINGSCMP"
            printf "%s\0" "=== FAILED Warnings don't match expected pattern $WARNINGSCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi




    ERRORS=$(echo "$STDERR" | sed -nE  "s/Error \(\#(\d*)/\1/p" | tr '\n' ',')


    printf "%s\0" "ERRORS: $ERRORS" >> "$OutFile"
    if [ ! -z "$ERRORSCMP" ];
    then
        nbTest=$(( nbTest + 1 ))
        echo "$ERRORS" | replace_newline_return | grep -Eq --text "($ERRORSCMP)"
        if [ "$?" -ne "0" ];
        then
            printf "\nERROR - ERRORS:\n%s\ndoesn't match %s\n\n" "$ERRORS" "$ERRORSCMP"
            printf "%s\0" "=== FAILED Errors don't match expected pattern $ERRORSCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi



    printf "%s\0" "DIFF: $DIFF" >> "$OutFile"
    if [ ! -z "$DIFFCMP" ];
    then
        nbTest=$(( nbTest + 1 ))
        echo "$DIFF" | replace_newline_return | grep -Eq --text "($DIFFCMP)"
        if [ "$?" -ne "0" ];
        then
	    printf "\nERROR - Changes to %s:\n%s\ndon't match %s\n\n" "$DBFile" "$DIFFCMP"
            printf "%s\0" "=== FAILED changes to database file ($DBFile) don't match $DIFFCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi


    echo >> "$OutFile"
}

run_script()
{
    while read line;
    do
        OP=
        ARG=
        cmd=$(echo $line|cut -d ' ' -f1)
        case "$cmd" in
            Mountpoint:)
                Mountpoint=$(echo $line|cut -d ' ' -f2)
                ;;
            File:)
                DBFile=$(echo $line|cut -d ' ' -f2)
                if [ "$DBFile" = "File:" -o -z "$DBFile" ]; then
			DBFile=$(mktemp -t elektraenv.XXXXXXXXX 2>/dev/null || mktemp -t 'elektraenv')
                fi
                ;;
            Storage:)
                Storage=$(echo $line|cut -d ' ' -f2)
                ;;
            MountArgs:)
                MountArgs=$(echo $line|cut -d ' ' -f2-)
                ;;
            Echo:)
                echo "$(echo $line|cut -d ' ' -f2-)"
                ;;
            DiffType:)
                DiffType=$(echo $line|cut -d ' ' -f2)
                ;;
            RET:)
                RETCMP=$(echo $line|cut -d ' ' -f2-)
                ;;
            ERRORS:)
                ERRORSCMP=$(echo $line|cut -d ' ' -f2-)
                ;;
            WARNINGS:)
                WARNINGSCMP=$(echo $line|cut -d ' ' -f2-)
                ;;
            STDOUT:)
                STDOUTCMP=$(echo $line|cut -d ' ' -f2-)
                ;;
            STDERR:)
                STDERRCMP=$(echo $line|cut -d ' ' -f2-)
                ;;
            DIFF:)
                DIFFCMP=$(echo $line|cut -d ' ' -f2-)
                ;;
            \<)
                OP="$cmd"
                ARG=$(echo $line|cut -d ' ' -f2-)
                ;;
        esac
        if [ "$OP" = "<" ];
        then
            execute "$ARG"
            RETCMP=
            ERRORSCMP=
            WARNINGSCMP=
            STDOUTCMP=
            STDERRCMP=
            DIFFCMP=
        fi
    done < "$FILE"
}

rm ./stdout 2>/dev/null
rm ./stderr 2>/dev/null

if [ "$#" -lt "1" ] || [ "$#" -gt "2" ];
then
    echo "Usage: ./shell_recorder inputscript [protocol to compare]"
    rm "$OutFile"
    exit 0
fi

BACKUP=1

echo "protocol file: $OutFile"

run_script

"$KDBCOMMAND" rm -r "$Mountpoint" 2>/dev/null
cat "$TMPFILE" | "$KDBCOMMAND" import $Mountpoint 2>/dev/null
rm "${DBFile}.1" 2>/dev/null

EVAL=0

if [ "$#" -eq "1" ];
then
    echo -n "shell_recorder $1 RESULTS: $nbTest test(s) done"
    echo " $nbError error(s)."
    EVAL=$nbError
fi

if [ "$#" -eq "2" ];
then
    RESULT=$(diff -N --text "$2" "$OutFile" 2>/dev/null)
    if [ "$?" -ne "0" ];
    then
        printf "%s\0" "=======================================\nReplay test failed, protocols differ"
        echo "$RESULT"
        printf "%s\0" "\n\n"
        EVAL=1
    else
        printf "%s\0" "=======================================\nReplay test succeeded"
    fi
fi

# this should be in temporary files, and/or in a trap exit
rm ./stdout 2>/dev/null
rm ./stderr 2>/dev/null

rm "${TMPFILE}"
exit "$EVAL"
