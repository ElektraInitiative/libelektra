#!/bin/bash

TriggerPort=65432
MountPoint="user/prefs"
ConfigFile="myPrefs.js"
Decoy="/tmp/imnotreal.js"
AutoConfigScript="./firefox.cfg"
AutoConfigLauncher="./autoconfig.js"

PrefsFile=
FFLibDir=

initialize()
{
    kdb check prefs &>/dev/null

    hasPrefsPlugin=$?

    if [ "$hasPrefsPlugin" -ne 0 ]; 
    then
	echo "Error, prefs plugin not found"
	exit 1
    fi

    kdb mount-info &>/dev/null

    libDir=$(kdb get system/info/constants/package/libdir)
    if [ -z "$libDir" ];
    then
	echo "Error, elektra libdir not found"
	exit 1
    fi

    interceptFile="${libDir}/libelektraintercept.so"

    if [ ! -e "$interceptFile" ];
    then
	echo "Error, libelektraintercept.so not found"
	exit 1
    fi

    FFLibDir=$(find /usr/lib /usr/lib32 /usr/lib64 /usr/local/lib -name "firefox*" -type f | xargs dirname 2>/dev/null|sort|uniq)
    if [ -z "$FFLibDir" ];
    then
	echo "Error, couldn't find any Firefox library directory"
	exit 1
    fi
    resNo=$(echo "$FFLibDir" | wc -l)
    if [ "$resNo" -gt 1 ];
    then
	i=1;
	for line in ${FFLibDir};
	do
	    echo "$i) $line"
	    i=$((++i))
	done

	echo -n "Select your Firefox library directory: "
	read -r sel

	FFLibDir=$(sed "${sel}q;d" <<< "$FFLibDir")
    fi

    echo "Firefox library directory: $FFLibDir"
    echo

    Profile=$(find "${HOME}/.mozilla/firefox/" -maxdepth 1 -mindepth 1 -type d 2>/dev/null)
    if [ -z "$Profile" ];
    then
	echo "Couldn't finde Firefox profile directories in ${HOME}/.mozillla/Firefox/"
	echo -n "Please enter specify your Firefox profile directory: "
	read -r Profile
    fi
    if [ -z "$Profile" ];
    then
	echo "Invalid profile directory"
	exit 1
    fi

    resNo=$(echo "$Profile" | wc -l)
    if [ "$resNo" -gt 1 ];
    then
	i=1;
	for line in ${Profile};
	do
	    echo "$i) `basename $line`"
	    i=$((++i))
	done
	echo -n "Select your Firefox profile directory: "
	read -r sel
	Profile=$(sed "${sel}q;d" <<< "$Profile")
    fi

    echo "Firefox profile directory: `basename $Profile`"
    echo

    PrefsFile=$(readlink -f "${Profile}/prefs.js")
    echo "prefsFile: $PrefsFile"
    if [ ! -e "$PrefsFile" ];
    then
	echo "Error, prefs file $PrefsFile doesn't exist"
	exit 1
    fi

    pgrep firefox &>/dev/null
    if [ $? -eq 0 ];
    then
	echo "Warning, can't continue while Firefox is still running"
	read -p "Please close Firefox and press any key to continue "
    fi
}

setAutoPrefs()
{
    echo "user_pref(\"elektra.config.file\", \"${Decoy}\");" >> ${PrefsFile}
    echo "user_pref(\"elektra.config.reload_trigger_port\", ${TriggerPort});" >> ${PrefsFile}
}

setPreload()
{
    kdb set /preload/open 
    escapedDecoy=$(echo "$Decoy"|sed 's/\//\\\//g')
    kdb set "/preload/open/${escapedDecoy}" ""
    kdb set "/preload/open/${escapedDecoy}/generate" "$MountPoint"
    kdb set "/preload/open/${escapedDecoy}/generate/plugin" prefs
}

setPrefs()
{
    kdb mount "$ConfigFile" "$MountPoint" prefs shell execute/set="echo -n \"reload\"|nc 127.0.0.1 $TriggerPort"
    kdb setmeta "${MountPoint}/lock/a/lock/1" type string
    kdb set "${MountPoint}/lock/a/lock/1" "lock1"
    kdb setmeta "${MountPoint}/lock/a/lock/2" type string
    kdb set "${MountPoint}/lock/a/lock/2" "lock2"
    kdb setmeta "${MountPoint}/pref/a/default/1" type integer
    kdb set "${MountPoint}/pref/a/default/1" 1
    kdb setmeta "${MountPoint}/pref/a/default/2" type integer
    kdb set "${MountPoint}/pref/a/default/2" 2
    kdb setmeta "${MountPoint}/user/a/user/t" type boolean 
    kdb set "${MountPoint}/user/a/user/t" true
    kdb setmeta "${MountPoint}/user/a/user/f" type boolean
    kdb set "${MountPoint}/user/a/user/f" false

}

setupAutoConfig()
{
    set -x
    su -c "cp $AutoConfigScript ${FFLibDir}"
    su -c "cp $AutoConfigLauncher ${FFLibDir}/defaults/pref/"
    set +x
}

initialize

setAutoPrefs

setPreload &>/dev/null

setPrefs &>/dev/null

setupAutoConfig
