# - Find python interpreter
# This module finds if Python interpreter is installed and determines where the
# executables are. This code sets the following variables:
#
#  PYTHON2INTERP_FOUND         - Was the Python executable found
#  PYTHON2_EXECUTABLE          - path to the Python interpreter
#
#  PYTHON2_VERSION_STRING      - Python version found e.g. 2.5.2
#  PYTHON2_VERSION_MAJOR       - Python major version found e.g. 2
#  PYTHON2_VERSION_MINOR       - Python minor version found e.g. 5
#  PYTHON2_VERSION_PATCH       - Python patch version found e.g. 2
#
# The Python2_ADDITIONAL_VERSIONS variable can be used to specify a list of
# version numbers that should be taken into account when searching for Python.
# You need to set this variable before calling find_package(Python2Interp).

#=============================================================================
# Copyright 2005-2010 Kitware, Inc.
# Copyright 2011 Bjoern Ricks <bjoern.ricks@gmail.com>
# Copyright 2012 Rolf Eike Beer <eike@sf-mail.de>
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

unset(_Python_NAMES)

set(_PYTHON2_VERSIONS 2.7)

if (Python2Interp_FIND_VERSION)
    if (Python2Interp_FIND_VERSION MATCHES "^[0-9]+\\.[0-9]+(\\.[0-9]+.*)?$")
        string(REGEX REPLACE "^([0-9]+\\.[0-9]+).*" "\\1" _PYTHON2_FIND_MAJ_MIN "${Python2Interp_FIND_VERSION}")
        string(REGEX REPLACE "^([0-9]+).*" "\\1" _PYTHON2_FIND_MAJ "${_PYTHON2_FIND_MAJ_MIN}")
        list(APPEND _Python_NAMES python${_PYTHON2_FIND_MAJ_MIN} python${_PYTHON2_FIND_MAJ})
        unset(_PYTHON2_FIND_OTHER_VERSIONS)
        if (NOT Python2Interp_FIND_VERSION_EXACT)
            foreach(_PYTHON2_V ${_PYTHON2${_PYTHON2_FIND_MAJ}_VERSIONS})
                if (NOT _PYTHON2_V VERSION_LESS _PYTHON2_FIND_MAJ_MIN)
                    list(APPEND _PYTHON2_FIND_OTHER_VERSIONS ${_PYTHON2_V})
                endif()
             endforeach()
        endif(NOT Python2Interp_FIND_VERSION_EXACT)
        unset(_PYTHON2_FIND_MAJ_MIN)
        unset(_PYTHON2_FIND_MAJ)
    else(Python2Interp_FIND_VERSION MATCHES "^[0-9]+\\.[0-9]+(\\.[0-9]+.*)?$")
        list(APPEND _Python_NAMES python${Python2Interp_FIND_VERSION})
        set(_PYTHON2_FIND_OTHER_VERSIONS ${_PYTHON2${Python2Interp_FIND_VERSION}_VERSIONS})
    endif(Python2Interp_FIND_VERSION MATCHES "^[0-9]+\\.[0-9]+(\\.[0-9]+.*)?$")
else(Python2Interp_FIND_VERSION)
    set(_PYTHON2_FIND_OTHER_VERSIONS ${_PYTHON2_VERSIONS})
endif(Python2Interp_FIND_VERSION)

list(APPEND _Python_NAMES python)

# Search for the current active python version first
find_program(PYTHON2_EXECUTABLE NAMES ${_Python_NAMES})

# Set up the versions we know about, in the order we will search. Always add
# the user supplied additional versions to the front.
set(_Python2_VERSIONS
  ${Python2_ADDITIONAL_VERSIONS}
  ${_PYTHON2_FIND_OTHER_VERSIONS}
  )

unset(_PYTHON2_FIND_OTHER_VERSIONS)
unset(_PYTHON2_VERSIONS)

# Search for newest python version if python executable isn't found
if (NOT PYTHON2_EXECUTABLE)
    foreach(_CURRENT_VERSION ${_Python2_VERSIONS})
      set(_Python_NAMES python${_CURRENT_VERSION})
      if (WIN32)
        list(APPEND _Python_NAMES python)
      endif()
      find_program(PYTHON2_EXECUTABLE
        NAMES ${_Python_NAMES}
        PATHS [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]
        )
    endforeach()
endif()

# determine python version string
if (PYTHON2_EXECUTABLE)
    execute_process(COMMAND "${PYTHON2_EXECUTABLE}" -c
                            "import sys; sys.stdout.write(';'.join([str(x) for x in sys.version_info[:3]]))"
                    OUTPUT_VARIABLE _VERSION
                    RESULT_VARIABLE _PYTHON2_VERSION_RESULT
                    ERROR_QUIET)
    if (NOT _PYTHON2_VERSION_RESULT)
        string(REPLACE ";" "." PYTHON2_VERSION_STRING "${_VERSION}")
        list(GET _VERSION 0 PYTHON2_VERSION_MAJOR)
        list(GET _VERSION 1 PYTHON2_VERSION_MINOR)
        list(GET _VERSION 2 PYTHON2_VERSION_PATCH)
        if (PYTHON2_VERSION_PATCH EQUAL 0)
            # it's called "Python 2.7", not "2.7.0"
            string(REGEX REPLACE "\\.0$" "" PYTHON2_VERSION_STRING "${PYTHON2_VERSION_STRING}")
        endif()
    else()
        # sys.version predates sys.version_info, so use that
        execute_process(COMMAND "${PYTHON2_EXECUTABLE}" -c "import sys; sys.stdout.write(sys.version)"
                        OUTPUT_VARIABLE _VERSION
                        RESULT_VARIABLE _PYTHON2_VERSION_RESULT
                        ERROR_QUIET)
        if (NOT _PYTHON2_VERSION_RESULT)
            string(REGEX REPLACE " .*" "" PYTHON2_VERSION_STRING "${_VERSION}")
            string(REGEX REPLACE "^([0-9]+)\\.[0-9]+.*" "\\1" PYTHON2_VERSION_MAJOR "${PYTHON2_VERSION_STRING}")
            string(REGEX REPLACE "^[0-9]+\\.([0-9])+.*" "\\1" PYTHON2_VERSION_MINOR "${PYTHON2_VERSION_STRING}")
            if (PYTHON2_VERSION_STRING MATCHES "^[0-9]+\\.[0-9]+\\.[0-9]+.*")
                string(REGEX REPLACE "^[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" PYTHON2_VERSION_PATCH "${PYTHON2_VERSION_STRING}")
            else()
                set(PYTHON2_VERSION_PATCH "0")
            endif()
        else()
            # sys.version was first documented for Python 1.5, so assume
            # this is older.
            set(PYTHON2_VERSION_STRING "1.4")
            set(PYTHON2_VERSION_MAJOR "1")
            set(PYTHON2_VERSION_MAJOR "4")
            set(PYTHON2_VERSION_MAJOR "0")
        endif()
    endif()
    unset(_PYTHON2_VERSION_RESULT)
    unset(_VERSION)
endif(PYTHON2_EXECUTABLE)

# handle the QUIETLY and REQUIRED arguments and set PYTHON2INTERP_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Python2Interp REQUIRED_VARS PYTHON2_EXECUTABLE VERSION_VAR PYTHON2_VERSION_STRING)

mark_as_advanced(PYTHON2_EXECUTABLE)
