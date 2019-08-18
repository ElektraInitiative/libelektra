#!/bin/bash

# Merges three files using Elektra
# All files are written to a default_path

ourpath="user/elektra-cmerge/our"
theirpath="user/elektra-cmerge/their"
basepath="user/elektra-cmerge/base"
resultpath="user/elektra-cmerge/result"
resultfile="elektra-cmerge_result"

if [ "$#" -ne 4 ]; then
    echo "Usage is $0 our their base format"
    exit 1;
fi
format=$4
kdb rm -rf $ourpath
kdb rm -rf $theirpath
kdb rm -rf $basepath
kdb rm -rf $resultpath
cat $1 | kdb import $ourpath $format
cat $2 | kdb import $theirpath $format
cat $3 | kdb import $basepath $format
kdb cmerge $ourpath $theirpath $basepath $resultpath
kdb export $resultpath $format > $resultfile
kdb rm -r $ourpath
kdb rm -r $theirpath
kdb rm -r $basepath
kdb rm -r $resultpath
