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

execute()
{
    proto="$@"
    [ -z "$Mountpoint" ] && Mountpoint="system/testmount/$(date +%N)"
    if [ -z "$DBFile" ];
    then
        DBFile=$(mktemp)
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
    bash -c "kdb $command 2>stderr 1>stdout"
        
    RETVAL="$?"
    if [ ! -z "$RETCMP" ];
    then
        bash -c "grep -Ewq \"$RETCMP\" <<< $RETVAL"
        [ "$?" -ne "0" ] && echo "Return value $RETVAL doesn't match $RETCMP"
    fi
    DIFF=
    case "$DiffType" in
        File)
            DIFF=$(diff --text ${DBFile} "${DBFile}.1" 2>/dev/null)
            ;;
        Ini)
            kdb export $Mountpoint simpleini > ./newState 2>/dev/null
            DIFF=$(diff --text ./previousState ./newState 2>/dev/null)
            rm ./newState 2>/dev/null
            rm ./previousState 2>/dev/null
            ;;
        Dump)
            kdb export $Mountpoint dump > ./newState 2>/dev/null
            DIFF=$(diff --text ./previousState ./newState 2>/dev/null)
            rm ./newState 2>/dev/null
            rm ./previousState 2>/dev/null
            ;;
    esac
    
    STDERR=$(<./stderr)
    if [ ! -z "$STDERRCMP" ];
    then
        echo "$STDERR" | grep -Eq "$STDERRCMP"
        [ "$?" -ne "0" ] && echo "STDERR doesn't match $STDERRCMP"
    fi
    STDOUT=$(<./stdout)
    if [ ! -z "$STDOUTCMP" ];
    then
        echo "$STDOUT" | grep -Eq "$STDOUTCMP"
        [ "$?" -ne "0" ] && echo "STDOUT doesn't match $STDOUTCMP"
    fi
    WARNINGS=$(echo "$STDERR" | grep -Po "(?<=Warning number: )([[:digit:]]*)" | tr '\n' ',')
    if [ ! -z "$WARNINGSCMP" ];
    then
        echo "$WARNINGS" | grep -Eq "($WARNINGSCMP)"
        [ "$?" -ne "0" ] && echo "WARNINGS doesn't match $WARNINGSCMP"
    fi

    ERRORS=$(echo "$STDERR" | grep -Po "(?<=Error \(\#)([[:digit:]]*)" | tr '\n' ',')
    if [ ! -z "$ERRORSCMP" ];
    then
        echo "$ERRORS" | grep -Eq "($ERRORSCMP)"
        [ "$?" -ne "0" ] && echo "ERRORS doesn't match $ERRORSCMP"
    fi
    echo -e "CMD: $command\0" >> ./protocol
    echo -e "RET: $RETVAL\0" >> ./protocol
    echo -e "ERRORS: $ERRORS\0" >> ./protocol
    echo -e "WARNINGS: $WARNINGS\0" >> ./protocol
    echo -e "STDOUT: $STDOUT\0" >> ./protocol
    echo -e "STDERR: $STDERR\0" >> ./protocol
    echo -e "DIFF: $DIFF\0" >> ./protocol
    echo >> ./protocol 
}

rm ./protocol 2>/dev/null
rm ./stdout 2>/dev/null
rm ./stderr 2>/dev/null

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

rm ./stdout 2>/dev/null
rm ./stderr 2>/dev/null
