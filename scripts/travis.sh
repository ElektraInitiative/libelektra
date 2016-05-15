#/bin/bash
set -e

WORKSPACE=/workspace
export HOME="$WORKSPACE/user"

if [ "${OS_RELEASE}" = "debian_unstable" ]
then
  export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64
fi

mkdir $WORKSPACE/build && \
  pushd $WORKSPACE/build && \
  cmake -DENABLE_COVERAGE=ON -DBUILD_STATIC=OFF -DBUILD_FULL=OFF -DBINDINGS=ALL -DPLUGINS=ALL -DBUILD_PDF=OFF \
    -DCMAKE_INSTALL_PREFIX=$WORKSPACE/install -DKDB_DB_SYSTEM=$WORKSPACE/kdbsystem \
    $WORKSPACE/code

make VERBOSE=1 -j3

make VERBOSE=1 -j3 run_all
