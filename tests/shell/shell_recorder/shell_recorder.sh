#!/bin/bash

FILE=$1
Mountpoint=
DBFile=
Storage=
MountArgs=
DiffType=File
OutFile=$(mktemp)

RETCMP=
ERRORSCMP=
WARNINGSCMP=
STDOUTCMP=
STDERRCMP=
DIFFCMP=

BACKUP=0
TMPFILE=$(mktemp)

# variables to count up errors and tests
nbError=0
nbTest=0

if [ -z "@USE_CMAKE_KDB_COMMAND@" ]; then
        KDBCOMMAND="@KDB_COMMAND@"
else
        KDBCOMMAND="kdb"
fi



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
        DBFile=$($KDBCOMMAND file $Mountpoint 2>/dev/null)
    fi

    if [ "$BACKUP" -eq "1" ];
    then
        $KDBCOMMAND export $Mountpoint > "$TMPFILE" 2>/dev/null
        BACKUP=0
        $KDBCOMMAND rm -r $Mountpoint 2>/dev/null
    fi

    [ -z "$Storage" ] && Storage="dump"
    command=$(echo ${proto//'$Mountpoint'/$Mountpoint})
    command=$(echo ${command//'$File'/$DBFile})
    command=$(echo ${command//'$Storage'/$Storage})
    command=$(echo ${command//'$MountArgs'/$MountArgs})

    case "$DiffType" in 
        File)
            rm "${DBFile}.1" 2>/dev/null
            cp "${DBFile}" "${DBFile}.1" 2>/dev/null
            ;;
        Ini)
            rm ./previousState 2>/dev/null
            $KDBCOMMAND export $Mountpoint simpleini > ./previousState 2>/dev/null
            ;;
        Dump)
            rm ./previousState 2>/dev/null
            $KDBCOMMAND export $Mountpoint dump > ./previousState 2>/dev/null
            ;;
    esac

    echo "$command"

    printf "CMD: %s\0\n" "$command" >> "$OutFile"

    bash -c "$KDBCOMMAND $command 2>stderr 1>stdout"

    RETVAL="$?"

    printf "RET: %s\0\n" "$RETVAL" >> "$OutFile"

    if [ ! -z "$RETCMP" ];
    then
        echo "$RETVAL" | grep -Ewq $RETCMP
        if [ "$?" -ne "0" ];
        then
            echo "Return value $RETVAL doesn't match $RETCMP"
            printf "=== FAILED return value doesn't match expected pattern %s\0\n" "$RETCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi

    DIFF=
    case "$DiffType" in
        File)
            DIFF=$(diff -N --text "${DBFile}" "${DBFile}.1" 2>/dev/null)
            ;;
        Ini)
            $KDBCOMMAND export $Mountpoint simpleini > ./newState 2>/dev/null
            DIFF=$(diff -N --text ./previousState ./newState 2>/dev/null)
            rm ./newState 2>/dev/null
            rm ./previousState 2>/dev/null
            ;;
        Dump)
            $KDBCOMMAND export $Mountpoint dump > ./newState 2>/dev/null
            DIFF=$(diff -N --text ./previousState ./newState 2>/dev/null)
            rm ./newState 2>/dev/null
            rm ./previousState 2>/dev/null
            ;;
    esac



    STDERR=$(cat ./stderr)

    printf "STDERR: %s\0\n" "$STDERR" >> "OutFile"
    if [ ! -z "$STDERRCMP" ];
    then
        echo "$STDERR" | grep -Eqz --text "$STDERRCMP"
        if [ "$?" -ne "0" ];
        then
            echo "STDERR doesn't match $STDERRCMP"
            printf "=== FAILED stderr doesn't match expected patter %s\0\n" "$STDERRCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi



    STDOUT=$(cat ./stdout)

    printf "STDOUT: %s\0\n" "$STDOUT" >> "$OutFile"
    if [ ! -z "$STDOUTCMP" ];
    then
        echo "$STDOUT" | grep -Eqz --text "$STDOUTCMP"
        if [ "$?" -ne "0" ];
        then
            echo "STDOUT doesn't match $STDOUTCMP"
            printf "=== FAILED stdout doesn't match expected pattern %s\0\n" "$STDOUTCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi



    WARNINGS=$(echo "$STDERR" | grep -Po "(?<=Warning number: )([[:digit:]]*)" | tr '\n' ',')

    printf "WARNINGS: %s\0\n" "$WARNINGS" >> "$OutFile"
    if [ ! -z "$WARNINGSCMP" ];
    then
        echo "$WARNINGS" | grep -Eqz --text "($WARNINGSCMP)"
        if [ "$?" -ne "0" ];
        then
            echo "WARNINGS doesn't match $WARNINGSCMP"
            printf "=== FAILED Warnings don't match expected pattern %s\0\n" "$WARNINGSCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi




    ERRORS=$(echo "$STDERR" | grep -Po "(?<=Error \(\#)([[:digit:]]*)" | tr '\n' ',')


    printf "ERRORS: %s\0\n" "$ERRORS" >> "$OutFile"
    if [ ! -z "$ERRORSCMP" ];
    then
        echo "$ERRORS" | grep -Eqz --text "($ERRORSCMP)"
        if [ "$?" -ne "0" ];
        then
            echo "ERRORS doesn't match $ERRORSCMP"
            printf "=== FAILED Errors don't match expected pattern %s\0\n" "$ERRORSCMP" >> "$OutFile"
            nbError=$(( nbError + 1 ))
        fi
    fi



    printf "DIFF: \0\n" "%s" >> "$OutFile"
    if [ ! -z "$DIFFCMP" ];
    then
        echo "$DIFF" | grep -Eqz --text "($DIFFCMP)"
        if [ "$?" -ne "0" ];
        then
            echo "Changes to $DBFile don't match $DIFFCMP"
            printf "=== FAILED changes to database file (%s) don't match %s\0\n" "$DBFile" "$DIFFCMP" >> "$OutFile"
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
        cmd=$(cut -d ' ' -f1 <<< $line)
        case "$cmd" in
            Mountpoint:)
                Mountpoint=$(cut -d ' ' -f2 <<< $line)
                ;;
            File:)
                DBFile=$(cut -d ' ' -f2 <<< $line)
                ;;
            Storage:)
                Storage=$(cut -d ' ' -f2 <<< $line)
                ;;
            MountArgs:)
                MountArgs=$(cut -d ' ' -f2- <<< $line)
                ;;
            Echo:)
                echo "$(cut -d ' ' -f2- <<< $line)"
                ;;
            DiffType:)
                DiffType=$(cut -d ' ' -f2 <<< $line)
                ;;
            RET:)
                RETCMP=$(cut -d ' ' -f2- <<< $line)
                ;;
            ERRORS:)
                ERRORSCMP=$(cut -d ' ' -f2- <<< $line)
                ;;
            WARNINGS:)
                WARNINGSCMP=$(cut -d ' ' -f2- <<< $line)
                ;;
            STDOUT:)
                STDOUTCMP=$(cut -d ' ' -f2- <<< $line)
                ;;
            STDERR:)
                STDERRCMP=$(cut -d ' ' -f2- <<< $line)
                ;;
            DIFF:)
                DIFFCMP=$(cut -d ' ' -f2- <<< $line)
                ;;
            \<)
                OP="$cmd"
                ARG=$(cut -d ' ' -f2- <<< $line)
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
            nbTest=$(( nbTest + 1 ))
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

$KDBCOMMAND rm -r "$Mountpoint" 2>/dev/null
cat "$TMPFILE" | $KDBCOMMAND import $Mountpoint 2>/dev/null
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
        printf "=======================================\nReplay test failed, protocols differ\n"
        echo "$RESULT"
        printf "\n"
        EVAL=1
    else
         printf "=======================================\nReplay test succeeded\n"
    fi
fi


rm ./stdout 2>/dev/null
rm ./stderr 2>/dev/null

rm "${TMPFILE}"
exit "$EVAL"
