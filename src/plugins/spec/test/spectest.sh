#!/bin/bash
kdb set system/elektra/globalplugins
kdb set system/elektra/globalplugins/postcommit list
kdb set system/elektra/globalplugins/postcommit/user
kdb set system/elektra/globalplugins/postcommit/user/placements
kdb set system/elektra/globalplugins/postcommit/user/placements/set "presetstorage precommit postcommit"
kdb set system/elektra/globalplugins/postcommit/user/placements/get "pregetstorage postgetstorage"
kdb set system/elektra/globalplugins/postcommit/user/placements/error "prerollback postrollback"
kdb set system/elektra/globalplugins/postcommit/user/plugins
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0 spec 
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/placements
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/placements/set "presetstorage"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/placements/get "postgetstorage"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/conflict/get "WARNING"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/conflict/get/range "WARNING"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/conflict/set "WARNING"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/conflict/set/range "WARNING"
kdb set system/elektra/globalplugins/postcommit/user/plugins/#0/conflict/set/conflict "IGNORE"
kdb set system/elektra/globalplugins/postrollback list
kdb set system/elektra/globalplugins/precommit list
kdb set system/elektra/globalplugins/pregetstorage list
kdb set system/elektra/globalplugins/postgetstorage list 
kdb set system/elektra/globalplugins/presetstorage list
kdb set system/elektra/globalplugins/prerollback list

#kdb mount $PWD/spec.ini spec ni
#kdb mount $PWD/spectest.ini /testkey ni
#kdb export /testkey ni
