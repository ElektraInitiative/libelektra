#!/bin/sh

# Svn Stuff
# $Id: addtestuser.sh 36 2004-06-12 12:19:09Z rayman $
# $LastChangedBy: rayman $

#adding a user with the name of test with a blank password and a group name test.

# We use uid 5000 and gid 5000 because it's unlike the user will already have that 
rg set system/users/test/passwd "x"
rg set system/users/test/gecos ""
rg set system/users/test/gid "5000"
rg set system/users/test/home "/"
rg set system/users/test/shell "/bin/sh"
rg set system/users/test/uid "5000"
rg ln system/users/test system/users/.ByID/5000
#shadow entries
#md5 of password "test"
rg set -m 0600 system/users/test/shadowPassword "$1$P9FiQ2aJ$qmGIqdjRjlawm9gNRNHEX/"
rg set -m 0600 system/users/test/passwdChangeAfter "99999"
rg set -m 0600 system/users/test/passwdChangeBefore "0"
rg set -m 0600 system/users/test/passwdDisableAfter -- "-1"
rg set -m 0600 system/users/test/passwdDisabledSince -- "-1"
rg set -m 0600 system/users/test/passwdWarnBefore "7"
rg set -m 0600 system/users/test/passwdReserved -- "-1"

#group entries
rg set system/groups/test/gid "5000"
rg set system/groups/test/passwd "x"
rg ln system/groups/test system/groups/.ByID/5000
