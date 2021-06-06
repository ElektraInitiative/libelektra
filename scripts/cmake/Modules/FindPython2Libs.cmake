# ~~~
# - Find python libraries
# This module finds if Python is installed and determines where the
# include files and libraries are. It also determines what the name of
# the library is. This code sets the following variables:
#
#  PYTHON2LIBS_FOUND           - have the Python libs been found
#  PYTHON2_LIBRARIES           - path to the python library
#  PYTHON2_INCLUDE_PATH        - path to where Python.h is found (deprecated)
#  PYTHON2_INCLUDE_DIRS        - path to where Python.h is found
#  PYTHON2_DEBUG_LIBRARIES     - path to the debug library (deprecated)
#  PYTHON2LIBS_VERSION_STRING  - version of the Python libs found (since CMake 2.8.8)
#
# The Python2_ADDITIONAL_VERSIONS variable can be used to specify a list of
# version numbers that should be taken into account when searching for Python.
# You need to set this variable before calling find_package(Python2Libs).
#
# If you'd like to specify the installation of Python to use, you should modify
# the following cache variables:
#  PYTHON2_LIBRARY             - path to the python library
#  PYTHON2_INCLUDE_DIR         - path to where Python.h is found
# ~~~

# ~~~
# =============================================================================
# CMake - Cross Platform Makefile Generator
# Copyright 2000-2021 Kitware, Inc. and Contributors
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
#
# * Neither the name of Kitware, Inc. nor the names of Contributors
#   may be used to endorse or promote products derived from this
#   software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# ------------------------------------------------------------------------------
#
# The following individuals and institutions are among the Contributors:
#
# * Aaron C. Meadows <cmake@shadowguarddev.com>
# * Adriaan de Groot <groot@kde.org>
# * Aleksey Avdeev <solo@altlinux.ru>
# * Alexander Neundorf <neundorf@kde.org>
# * Alexander Smorkalov <alexander.smorkalov@itseez.com>
# * Alexey Sokolov <sokolov@google.com>
# * Alex Merry <alex.merry@kde.org>
# * Alex Turbov <i.zaufi@gmail.com>
# * Andreas Pakulat <apaku@gmx.de>
# * Andreas Schneider <asn@cryptomilk.org>
# * André Rigland Brodtkorb <Andre.Brodtkorb@ifi.uio.no>
# * Axel Huebl, Helmholtz-Zentrum Dresden - Rossendorf
# * Benjamin Eikel
# * Bjoern Ricks <bjoern.ricks@gmail.com>
# * Brad Hards <bradh@kde.org>
# * Christopher Harvey
# * Christoph Grüninger <foss@grueninger.de>
# * Clement Creusot <creusot@cs.york.ac.uk>
# * Daniel Blezek <blezek@gmail.com>
# * Daniel Pfeifer <daniel@pfeifer-mail.de>
# * Enrico Scholz <enrico.scholz@informatik.tu-chemnitz.de>
# * Eran Ifrah <eran.ifrah@gmail.com>
# * Esben Mose Hansen, Ange Optimization ApS
# * Geoffrey Viola <geoffrey.viola@asirobots.com>
# * Google Inc
# * Gregor Jasny
# * Helio Chissini de Castro <helio@kde.org>
# * Ilya Lavrenov <ilya.lavrenov@itseez.com>
# * Insight Software Consortium <insightsoftwareconsortium.org>
# * Jan Woetzel
# * Julien Schueller
# * Kelly Thompson <kgt@lanl.gov>
# * Konstantin Podsvirov <konstantin@podsvirov.pro>
# * Laurent Montel <montel@kde.org>
# * Mario Bensi <mbensi@ipsquad.net>
# * Martin Gräßlin <mgraesslin@kde.org>
# * Mathieu Malaterre <mathieu.malaterre@gmail.com>
# * Matthaeus G. Chajdas
# * Matthias Kretz <kretz@kde.org>
# * Matthias Maennich <matthias@maennich.net>
# * Michael Hirsch, Ph.D. <www.scivision.co>
# * Michael Stürmer
# * Miguel A. Figueroa-Villanueva
# * Mike Jackson
# * Mike McQuaid <mike@mikemcquaid.com>
# * Nicolas Bock <nicolasbock@gmail.com>
# * Nicolas Despres <nicolas.despres@gmail.com>
# * Nikita Krupen'ko <krnekit@gmail.com>
# * NVIDIA Corporation <www.nvidia.com>
# * OpenGamma Ltd. <opengamma.com>
# * Patrick Stotko <stotko@cs.uni-bonn.de>
# * Per Øyvind Karlsen <peroyvind@mandriva.org>
# * Peter Collingbourne <peter@pcc.me.uk>
# * Petr Gotthard <gotthard@honeywell.com>
# * Philip Lowman <philip@yhbt.com>
# * Philippe Proulx <pproulx@efficios.com>
# * Raffi Enficiaud, Max Planck Society
# * Raumfeld <raumfeld.com>
# * Roger Leigh <rleigh@codelibre.net>
# * Rolf Eike Beer <eike@sf-mail.de>
# * Roman Donchenko <roman.donchenko@itseez.com>
# * Roman Kharitonov <roman.kharitonov@itseez.com>
# * Ruslan Baratov
# * Sebastian Holtermann <sebholt@xwmw.org>
# * Stephen Kelly <steveire@gmail.com>
# * Sylvain Joubert <joubert.sy@gmail.com>
# * The Qt Company Ltd.
# * Thomas Sondergaard <ts@medical-insight.com>
# * Tobias Hunger <tobias.hunger@qt.io>
# * Todd Gamblin <tgamblin@llnl.gov>
# * Tristan Carel
# * University of Dundee
# * Vadim Zhukov
# * Will Dicharry <wdicharry@stellarscience.com>
#
# See version control history for details of individual contributions.
#
# The above copyright and license notice applies to distributions of
# CMake in source and binary form.  Third-party software packages supplied
# with CMake under compatible licenses provide their own copyright notices
# documented in corresponding subdirectories or source files.
#
# ------------------------------------------------------------------------------
#
# CMake was initially developed by Kitware with the following sponsorship:
#
#  * National Library of Medicine at the National Institutes of Health
#    as part of the Insight Segmentation and Registration Toolkit (ITK).
#
#  * US National Labs (Los Alamos, Livermore, Sandia) ASC Parallel
#    Visualization Initiative.
#
#  * National Alliance for Medical Image Computing (NAMIC) is funded by the
#    National Institutes of Health through the NIH Roadmap for Medical Research,
#    Grant U54 EB005149.
#
#  * Kitware, Inc.
# =============================================================================
# ~~~

include (CMakeFindFrameworks)
# Search for the python framework on Apple.
cmake_find_frameworks (Python)

set (_PYTHON21_VERSIONS 1.6 1.5)
set (
	_PYTHON22_VERSIONS
	2.7
	2.6
	2.5
	2.4
	2.3
	2.2
	2.1
	2.0)
set (_PYTHON23_VERSIONS 3.3 3.2 3.1 3.0)

if (Python2Libs_FIND_VERSION)
	if (Python2Libs_FIND_VERSION MATCHES "^[0-9]+\\.[0-9]+(\\.[0-9]+.*)?$")
		string (REGEX REPLACE "^([0-9]+\\.[0-9]+).*" "\\1" _PYTHON2_FIND_MAJ_MIN "${Python2Libs_FIND_VERSION}")
		string (REGEX REPLACE "^([0-9]+).*" "\\1" _PYTHON2_FIND_MAJ "${_PYTHON2_FIND_MAJ_MIN}")
		unset (_PYTHON2_FIND_OTHER_VERSIONS)
		if (Python2Libs_FIND_VERSION_EXACT)
			if (_PYTHON2_FIND_MAJ_MIN STREQUAL Python2Libs_FIND_VERSION)
				set (_PYTHON2_FIND_OTHER_VERSIONS "${Python2Libs_FIND_VERSION}")
			else (_PYTHON2_FIND_MAJ_MIN STREQUAL Python2Libs_FIND_VERSION)
				set (_PYTHON2_FIND_OTHER_VERSIONS "${Python2Libs_FIND_VERSION}" "${_PYTHON2_FIND_MAJ_MIN}")
			endif (_PYTHON2_FIND_MAJ_MIN STREQUAL Python2Libs_FIND_VERSION)
		else (Python2Libs_FIND_VERSION_EXACT)
			foreach (_PYTHON2_V ${_PYTHON2${_PYTHON2_FIND_MAJ}_VERSIONS})
				if (NOT _PYTHON2_V VERSION_LESS _PYTHON2_FIND_MAJ_MIN)
					list (APPEND _PYTHON2_FIND_OTHER_VERSIONS ${_PYTHON2_V})
				endif ()
			endforeach ()
		endif (Python2Libs_FIND_VERSION_EXACT)
		unset (_PYTHON2_FIND_MAJ_MIN)
		unset (_PYTHON2_FIND_MAJ)
	else (Python2Libs_FIND_VERSION MATCHES "^[0-9]+\\.[0-9]+(\\.[0-9]+.*)?$")
		set (_PYTHON2_FIND_OTHER_VERSIONS ${_PYTHON2${Python2Libs_FIND_VERSION}_VERSIONS})
	endif (Python2Libs_FIND_VERSION MATCHES "^[0-9]+\\.[0-9]+(\\.[0-9]+.*)?$")
else (Python2Libs_FIND_VERSION)
	set (_PYTHON2_FIND_OTHER_VERSIONS ${_PYTHON23_VERSIONS} ${_PYTHON22_VERSIONS} ${_PYTHON21_VERSIONS})
endif (Python2Libs_FIND_VERSION)

# Set up the versions we know about, in the order we will search. Always add the user supplied additional versions to the front.
set (_Python2_VERSIONS ${Python2_ADDITIONAL_VERSIONS} ${_PYTHON2_FIND_OTHER_VERSIONS})

unset (_PYTHON2_FIND_OTHER_VERSIONS)
unset (_PYTHON21_VERSIONS)
unset (_PYTHON22_VERSIONS)
unset (_PYTHON23_VERSIONS)

foreach (_CURRENT_VERSION ${_Python2_VERSIONS})
	string (REPLACE "." "" _CURRENT_VERSION_NO_DOTS ${_CURRENT_VERSION})
	if (WIN32)
		find_library (
			PYTHON2_DEBUG_LIBRARY
			NAMES python${_CURRENT_VERSION_NO_DOTS}_d python
			PATHS [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs/Debug
			      [HKEY_CURRENT_USER\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs/Debug
			      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs
			      [HKEY_CURRENT_USER\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs)
	endif (WIN32)

	find_library (
		PYTHON2_LIBRARY
		NAMES python${_CURRENT_VERSION_NO_DOTS} python${_CURRENT_VERSION}mu python${_CURRENT_VERSION}m python${_CURRENT_VERSION}u
		      python${_CURRENT_VERSION}
		PATHS [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs
		      [HKEY_CURRENT_USER\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs
		NO_SYSTEM_ENVIRONMENT_PATH # Avoid finding the .dll in the PATH.  We want the .lib.
	)

	# Look for the static library in the Python config directory
	find_library (
		PYTHON2_LIBRARY
		NAMES python${_CURRENT_VERSION_NO_DOTS} python${_CURRENT_VERSION}
		NO_SYSTEM_ENVIRONMENT_PATH # Avoid finding the .dll in the PATH.  We want the .lib.
		PATH_SUFFIXES python${_CURRENT_VERSION}/config # This is where the static library is usually located
	)

	# For backward compatibility, honour value of PYTHON2_INCLUDE_PATH, if PYTHON2_INCLUDE_DIR is not set.
	if (DEFINED PYTHON2_INCLUDE_PATH AND NOT DEFINED PYTHON2_INCLUDE_DIR)
		set (
			PYTHON2_INCLUDE_DIR
			"${PYTHON2_INCLUDE_PATH}"
			CACHE PATH "Path to where Python.h is found" FORCE)
	endif (DEFINED PYTHON2_INCLUDE_PATH AND NOT DEFINED PYTHON2_INCLUDE_DIR)

	set (PYTHON2_FRAMEWORK_INCLUDES)
	if (Python_FRAMEWORKS AND NOT PYTHON2_INCLUDE_DIR)
		foreach (dir ${Python_FRAMEWORKS})
			set (PYTHON2_FRAMEWORK_INCLUDES ${PYTHON2_FRAMEWORK_INCLUDES}
							${dir}/Versions/${_CURRENT_VERSION}/include/python${_CURRENT_VERSION})
		endforeach (dir)
	endif (Python_FRAMEWORKS AND NOT PYTHON2_INCLUDE_DIR)

	find_path (
		PYTHON2_INCLUDE_DIR
		NAMES Python.h
		PATHS ${PYTHON2_FRAMEWORK_INCLUDES}
		      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/include
		      [HKEY_CURRENT_USER\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/include
		PATH_SUFFIXES python${_CURRENT_VERSION}mu python${_CURRENT_VERSION}m python${_CURRENT_VERSION}u python${_CURRENT_VERSION})

	# For backward compatibility, set PYTHON2_INCLUDE_PATH.
	set (PYTHON2_INCLUDE_PATH "${PYTHON2_INCLUDE_DIR}")

	if (PYTHON2_INCLUDE_DIR AND EXISTS "${PYTHON2_INCLUDE_DIR}/patchlevel.h")
		file (STRINGS "${PYTHON2_INCLUDE_DIR}/patchlevel.h" python_version_str REGEX "^#define[ \t]+PY_VERSION[ \t]+\"[^\"]+\"")
		string (REGEX REPLACE "^#define[ \t]+PY_VERSION[ \t]+\"([^\"]+)\".*" "\\1" PYTHON2LIBS_VERSION_STRING
				      "${python_version_str}")
		unset (python_version_str)
	endif (PYTHON2_INCLUDE_DIR AND EXISTS "${PYTHON2_INCLUDE_DIR}/patchlevel.h")

	if (PYTHON2_LIBRARY AND PYTHON2_INCLUDE_DIR)
		break ()
	endif (PYTHON2_LIBRARY AND PYTHON2_INCLUDE_DIR)
endforeach (_CURRENT_VERSION)

mark_as_advanced (PYTHON2_DEBUG_LIBRARY PYTHON2_LIBRARY PYTHON2_INCLUDE_DIR)

# We use PYTHON2_INCLUDE_DIR, PYTHON2_LIBRARY and PYTHON2_DEBUG_LIBRARY for the cache entries because they are meant to specify the location
# of a single library. We now set the variables listed by the documentation for this module.
set (PYTHON2_INCLUDE_DIRS "${PYTHON2_INCLUDE_DIR}")
set (PYTHON2_DEBUG_LIBRARIES "${PYTHON2_DEBUG_LIBRARY}")

# These variables have been historically named in this module different from what SELECT_LIBRARY_CONFIGURATIONS() expects.
set (PYTHON2_LIBRARY_DEBUG "${PYTHON2_DEBUG_LIBRARY}")
set (PYTHON2_LIBRARY_RELEASE "${PYTHON2_LIBRARY}")
include (SelectLibraryConfigurations)
select_library_configurations (PYTHON2)

# SELECT_LIBRARY_CONFIGURATIONS() sets ${PREFIX}_FOUND if it has a library. Unset this, this prefix doesn't match the module prefix, they
# are different for historical reasons.
unset (PYTHON2_FOUND)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
	Python2Libs
	REQUIRED_VARS PYTHON2_LIBRARIES PYTHON2_INCLUDE_DIRS
	VERSION_VAR PYTHON2LIBS_VERSION_STRING)

# PYTHON2_ADD_MODULE(<name> src1 src2 ... srcN) is used to build modules for python. PYTHON2_WRITE_MODULES_HEADER(<filename>) writes a
# header file you can include in your sources to initialize the static python modules
function (PYTHON2_ADD_MODULE _NAME)
	get_property (_TARGET_SUPPORTS_SHARED_LIBS GLOBAL PROPERTY TARGET_SUPPORTS_SHARED_LIBS)
	option (PYTHON2_ENABLE_MODULE_${_NAME} "Add module ${_NAME}" TRUE)
	option (PYTHON2_MODULE_${_NAME}_BUILD_SHARED "Add module ${_NAME} shared" ${_TARGET_SUPPORTS_SHARED_LIBS})

	# Mark these options as advanced
	mark_as_advanced (PYTHON2_ENABLE_MODULE_${_NAME} PYTHON2_MODULE_${_NAME}_BUILD_SHARED)

	if (PYTHON2_ENABLE_MODULE_${_NAME})
		if (PYTHON2_MODULE_${_NAME}_BUILD_SHARED)
			set (PY_MODULE_TYPE MODULE)
		else (PYTHON2_MODULE_${_NAME}_BUILD_SHARED)
			set (PY_MODULE_TYPE STATIC)
			set_property (GLOBAL APPEND PROPERTY PY_STATIC_MODULES_LIST ${_NAME})
		endif (PYTHON2_MODULE_${_NAME}_BUILD_SHARED)

		set_property (GLOBAL APPEND PROPERTY PY_MODULES_LIST ${_NAME})
		add_library (${_NAME} ${PY_MODULE_TYPE} ${ARGN}) # TARGET_LINK_LIBRARIES(${_NAME} ${PYTHON2_LIBRARIES})

		if (PYTHON2_MODULE_${_NAME}_BUILD_SHARED)
			set_target_properties (${_NAME} PROPERTIES PREFIX "${PYTHON2_MODULE_PREFIX}")
			if (WIN32 AND NOT CYGWIN)
				set_target_properties (${_NAME} PROPERTIES SUFFIX ".pyd")
			endif (WIN32 AND NOT CYGWIN)
		endif (PYTHON2_MODULE_${_NAME}_BUILD_SHARED)

	endif (PYTHON2_ENABLE_MODULE_${_NAME})
endfunction (PYTHON2_ADD_MODULE)

function (PYTHON2_WRITE_MODULES_HEADER _filename)

	get_property (PY_STATIC_MODULES_LIST GLOBAL PROPERTY PY_STATIC_MODULES_LIST)

	get_filename_component (_name "${_filename}" NAME)
	string (REPLACE "." "_" _name "${_name}")
	string (TOUPPER ${_name} _nameUpper)
	set (_filename ${CMAKE_CURRENT_BINARY_DIR}/${_filename})

	set (_filenameTmp "${_filename}.in")
	file (WRITE ${_filenameTmp} "/*Created by cmake, do not edit, changes will be lost*/\n")
	file (
		APPEND ${_filenameTmp}
		"#ifndef ${_nameUpper}
#define ${_nameUpper}

#include <Python.h>

#ifdef __cplusplus
extern \"C\" {
#endif /* __cplusplus */

")

	foreach (_currentModule ${PY_STATIC_MODULES_LIST})
		file (APPEND ${_filenameTmp} "extern void init${PYTHON2_MODULE_PREFIX}${_currentModule}(void);\n\n")
	endforeach (_currentModule ${PY_STATIC_MODULES_LIST})

	file (
		APPEND ${_filenameTmp}
		"#ifdef __cplusplus
}
#endif /* __cplusplus */

")

	foreach (_currentModule ${PY_STATIC_MODULES_LIST})
		file (
			APPEND ${_filenameTmp}
			"int ${_name}_${_currentModule}(void) \n{\n  static char name[]=\"${PYTHON2_MODULE_PREFIX}${_currentModule}\"; return PyImport_AppendInittab(name, init${PYTHON2_MODULE_PREFIX}${_currentModule});\n}\n\n"
		)
	endforeach (_currentModule ${PY_STATIC_MODULES_LIST})

	file (APPEND ${_filenameTmp} "void ${_name}_LoadAllPythonModules(void)\n{\n")
	foreach (_currentModule ${PY_STATIC_MODULES_LIST})
		file (APPEND ${_filenameTmp} "  ${_name}_${_currentModule}();\n")
	endforeach (_currentModule ${PY_STATIC_MODULES_LIST})
	file (APPEND ${_filenameTmp} "}\n\n")
	file (
		APPEND ${_filenameTmp}
		"#ifndef EXCLUDE_LOAD_ALL_FUNCTION\nvoid CMakeLoadAllPythonModules(void)\n{\n  ${_name}_LoadAllPythonModules();\n}\n#endif\n\n#endif\n"
	)

	# with CONFIGURE_FILE() cmake complains that you may not use a file created using FILE(WRITE) as input file for CONFIGURE_FILE()
	execute_process (COMMAND ${CMAKE_COMMAND} -E copy_if_different "${_filenameTmp}" "${_filename}" OUTPUT_QUIET ERROR_QUIET)

endfunction (PYTHON2_WRITE_MODULES_HEADER)
