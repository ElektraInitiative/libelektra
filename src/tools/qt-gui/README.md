This is a technical preview of a new gui for Elektra.

To build it, make sure that you have Qt5 cmake files in your
CMAKE PREFIX PATH and that you have added qt-gui in TOOLS.
You can can also use ALL (will add all tools):

    cmake -DTOOLS=ALL -DCMAKE_PREFIX_PATH=/opt/Qt5.3.0/5.3/gcc_64/lib/cmake .


Dependencies: Qt5.3 and libdrm-dev
