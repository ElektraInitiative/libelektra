- infos = Information about the jni plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements =
- infos/description =

## Introduction ##

Needs Java:

    JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64 cmake
    -DPLUGINS="resolver;dump;sync;jni" -DBUILD_TESTING=OFF
    -DBUILD_STATIC=OFF -DBUILD_FULL=OFF .

## Usage ##

