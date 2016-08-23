prefType=
prefString=

readPrefType()
{
    unset prefType
    unset prefString
    echo -e "1) lock\n2) default\n3) user\n0) Exit"
    read -s -n 1 sel
    case "$sel" in
	1)
	    prefType="lock"
	    prefString="lockPref"
	    ;;
	2)
	    prefType="pref"
	    prefString="defaultPref"
	    ;;
	3)
	    prefType="user"
	    prefString="userPref"
	    ;;
	*)
	    echo "Error, invalid selection"
	    return
	    ;;
    esac

}

prefSetup()
{
    kdb mount "$ConfigFile" "$MountPoint" prefs shell execute/set="echo -n \"reload\"|nc 127.0.0.1 $TriggerPort" &>/dev/null
    echo -e "Config Setup:\n\n1) Proxy\n0) Exit"
    read -n1 -s input
    while true;
    do
	case "$input" in
	    1)
    		( . ./setupProxy.sh)
		;;
	    0)
		exit 0
		;;
    	esac
	echo -e "\n\nConfig Setup:\n\n1) Proxy\n0) Exit"
	read -n1 -s input
    done
}

prefSetup

