#!/bin/bash
kdb set system/elektra/globalplugins
kdb set system/elektra/globalplugins/postcommit list
kdb set system/elektra/globalplugins/postcommit/user
kdb set system/elektra/globalplugins/postcommit/user/placements
kdb set system/elektra/globalplugins/postcommit/user/placements/set "presetstorage precommit postcommit"
kdb set system/elektra/globalplugins/postcommit/user/placements/get "pregetstorage postgetstorage"
kdb set system/elektra/globalplugins/postcommit/user/placements/error "prerollback postrollback"
kdb set system/elektra/globalplugins/postcommit/user/plugins
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0 globalglob 
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/placements
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/placements/get "postgetstorage"

kdb set system/elektra/globalplugins/postcommit/user/plugins/#1 logger
kdb set system/elektra/globalplugins/postcommit/user/plugins/#1/placements
kdb set system/elektra/globalplugins/postcommit/user/plugins/#1/placements/set "presetstorage precommit postcommit"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#1/placements/get "pregetstorage postgetstorage"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#1/placements/error "prerollback postrollback"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#1/logfile "/tmp/mylogfile.log"

kdb set system/elektra/globalplugins/postcommit/user/plugins/#2 tracer 
kdb set system/elektra/globalplugins/postcommit/user/plugins/#2/placements
kdb set system/elektra/globalplugins/postcommit/user/plugins/#2/placements/set "presetstorage precommit postcommit"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#2/placements/get "pregetstorage postgetstorage"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#2/placements/error "prerollback postrollback"

kdb set system/elektra/globalplugins/postrollback list
kdb set system/elektra/globalplugins/pregetstorage list
kdb set system/elektra/globalplugins/postgetstorage list 
kdb set system/elektra/globalplugins/presetstorage list
kdb set system/elektra/globalplugins/prerollback list
