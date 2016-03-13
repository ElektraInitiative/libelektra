#!/bin/bash

FILE=$1
Mountpoint=
DBFile=
Storage=
MountArgs=
DiffType=File

RETCMP=
ERRORSCMP=
WARNINGSCMP=
STDOUTCMP=
STDERRCMP=
DIFFCMP=

BACKUP=0
TMPFILE=$(mktemp)

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
        DBFile=$(kdb file $Mountpoint 2>/dev/null)
    fi

    if [ "$BACKUP" -eq "1" ];
    then
        kdb export $Mountpoint > $TMPFILE 2>/dev/null
        BACKUP=0
        kdb rm -r $Mountpoint 2>/dev/null
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
            kdb export $Mountpoint simpleini > ./previousState 2>/dev/null 
            ;;
        Dump)
            rm ./previousState 2>/dev/null
            kdb export $Mountpoint dump > ./previousState 2>/dev/null
            ;;
    esac

    echo "$command"

    echo -e "CMD: $command\0" >> ./protocol

    bash -c "kdb $command 2>stderr 1>stdout"

    RETVAL="$?"

    echo -e "RET: $RETVAL\0" >> ./protocol

    if [ ! -z "$RETCMP" ];
    then
        echo "$RETVAL" | grep -Ewq $RETCMP
        if [ "$?" -ne "0" ];
        then
            echo "Return value $RETVAL doesn't match $RETCMP"
            echo -e "=== FAILED return value doesn't match expected pattern $RETCMP\0" >> ./protocol
        fi
    fi

    DIFF=
    case "$DiffType" in
        File)
            DIFF=$(diff -N --text ${DBFile} "${DBFile}.1" 2>/dev/null)
            ;;
        Ini)
            kdb export $Mountpoint simpleini > ./newState 2>/dev/null
            DIFF=$(diff -N --text ./previousState ./newState 2>/dev/null)
            rm ./newState 2>/dev/null
            rm ./previousState 2>/dev/null
            ;;
        Dump)
            kdb export $Mountpoint dump > ./newState 2>/dev/null
            DIFF=$(diff -N --text ./previousState ./newState 2>/dev/null)
            rm ./newState 2>/dev/null
            rm ./previousState 2>/dev/null
            ;;
    esac



    STDERR=$(<./stderr)

    echo -e "STDERR: $STDERR\0" >> ./protocol
    if [ ! -z "$STDERRCMP" ];
    then
        echo "$STDERR" | grep -Eqz --text "$STDERRCMP"
        if [ "$?" -ne "0" ];
        then
            echo "STDERR doesn't match $STDERRCMP"
            echo -e "=== FAILED stderr doesn't match expected patter $STDERRCMP\0" >> ./protocol
        fi
    fi



    STDOUT=$(<./stdout)

    echo -e "STDOUT: $STDOUT\0" >> ./protocol
    if [ ! -z "$STDOUTCMP" ];
    then
        echo "$STDOUT" | grep -Eqz --text "$STDOUTCMP"
        if [ "$?" -ne "0" ];
        then
            echo "STDOUT doesn't match $STDOUTCMP"
            echo -e "=== FAILED stdout doesn't match expected pattern $STDOUTCMP\0" >> ./protocol
        fi
    fi



    WARNINGS=$(echo "$STDERR" | grep -Po "(?<=Warning number: )([[:digit:]]*)" | tr '\n' ',')

    echo -e "WARNINGS: $WARNINGS\0" >> ./protocol
    if [ ! -z "$WARNINGSCMP" ];
    then
        echo "$WARNINGS" | grep -Eqz --text "($WARNINGSCMP)"
        if [ "$?" -ne "0" ];
        then
            echo "WARNINGS doesn't match $WARNINGSCMP"
            echo -e "=== FAILED Warnings don't match expected pattern $WARNINGSCMP\0" >> ./protocol
        fi
    fi




    ERRORS=$(echo "$STDERR" | grep -Po "(?<=Error \(\#)([[:digit:]]*)" | tr '\n' ',')


    echo -e "ERRORS: $ERRORS\0" >> ./protocol
    if [ ! -z "$ERRORSCMP" ];
    then
        echo "$ERRORS" | grep -Eqz --text "($ERRORSCMP)"
        if [ "$?" -ne "0" ];
        then
            echo "ERRORS doesn't match $ERRORSCMP"
            echo -e "=== FAILED Errors don't match expected pattern $ERRORSCMP\0" >> ./protocol
        fi
    fi



    echo -e "DIFF: $DIFF\0" >> ./protocol
    if [ ! -z "$DIFFCMP" ];
    then
        echo "$DIFF" | grep -Eqz --text "($DIFFCMP)"
        if [ "$?" -ne "0" ];
        then
            echo "Changes to $DBFile don't match $DIFFCMP"
            echo -e "=== FAILED changes to database file ($DBFILE) don't match $DIFFCMP\0" >> ./protocol
        fi
    fi


    echo >> ./protocol 
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
        if [ "$OP" == "<" ];
        then
            execute $ARG
            RETCMP=
            ERRORSCMP=
            WARNINGSCMP=
            STDOUTCMP=
            STDERRCMP=
            DIFFCMP=
        fi
    done < $FILE
}

rm ./protocol 2>/dev/null
rm ./stdout 2>/dev/null
rm ./stderr 2>/dev/null

if [ "$#" -lt "1" ] || [ "$#" -gt "2" ];
then
    echo "Usage: ./shell_recorder inputscript [protocol to compare]"
    exit 0
fi

BACKUP=1

run_script

kdb rm -r $Mountpoint 2>/dev/null
cat "$TMPFILE" | kdb import $Mountpoint 2>/dev/null
rm "${DBFile}.1" 2>/dev/null

EVAL=0

if [ "$#" -eq "2" ];
then
    RESULT=$(diff -N --text "$2" ./protocol 2>/dev/null)
    if [ "$?" -ne "0" ];
    then
        echo -e "=======================================\nReplay test failed, protocols differ"
        echo "$RESULT"
        echo -en "\n\n"
        EVAL=1
    else
         echo -e "=======================================\nReplay test succeeded"
    fi
fi


rm ./stdout 2>/dev/null
rm ./stderr 2>/dev/null

rm ${TMPFILE}
exit "$EVAL"
