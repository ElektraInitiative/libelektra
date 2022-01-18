# ~~~
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

unset (_Python_NAMES)

set (_PYTHON2_VERSIONS 2.7)

if (Python2Interp_FIND_VERSION)
	if (Python2Interp_FIND_VERSION MATCHES "^[0-9]+\\.[0-9]+(\\.[0-9]+.*)?$")
		string (REGEX REPLACE "^([0-9]+\\.[0-9]+).*" "\\1" _PYTHON2_FIND_MAJ_MIN "${Python2Interp_FIND_VERSION}")
		string (REGEX REPLACE "^([0-9]+).*" "\\1" _PYTHON2_FIND_MAJ "${_PYTHON2_FIND_MAJ_MIN}")
		list (APPEND _Python_NAMES python${_PYTHON2_FIND_MAJ_MIN} python${_PYTHON2_FIND_MAJ})
		unset (_PYTHON2_FIND_OTHER_VERSIONS)
		if (NOT Python2Interp_FIND_VERSION_EXACT)
			foreach (_PYTHON2_V ${_PYTHON2${_PYTHON2_FIND_MAJ}_VERSIONS})
				if (NOT _PYTHON2_V VERSION_LESS _PYTHON2_FIND_MAJ_MIN)
					list (APPEND _PYTHON2_FIND_OTHER_VERSIONS ${_PYTHON2_V})
				endif ()
			endforeach ()
		endif (NOT Python2Interp_FIND_VERSION_EXACT)
		unset (_PYTHON2_FIND_MAJ_MIN)
		unset (_PYTHON2_FIND_MAJ)
	else (Python2Interp_FIND_VERSION MATCHES "^[0-9]+\\.[0-9]+(\\.[0-9]+.*)?$")
		list (APPEND _Python_NAMES python${Python2Interp_FIND_VERSION})
		set (_PYTHON2_FIND_OTHER_VERSIONS ${_PYTHON2${Python2Interp_FIND_VERSION}_VERSIONS})
	endif (Python2Interp_FIND_VERSION MATCHES "^[0-9]+\\.[0-9]+(\\.[0-9]+.*)?$")
else (Python2Interp_FIND_VERSION)
	set (_PYTHON2_FIND_OTHER_VERSIONS ${_PYTHON2_VERSIONS})
endif (Python2Interp_FIND_VERSION)

list (APPEND _Python_NAMES python)

# Search for the current active python version first
find_program (PYTHON2_EXECUTABLE NAMES ${_Python_NAMES})

# Set up the versions we know about, in the order we will search. Always add the user supplied additional versions to the front.
set (_Python2_VERSIONS ${Python2_ADDITIONAL_VERSIONS} ${_PYTHON2_FIND_OTHER_VERSIONS})

unset (_PYTHON2_FIND_OTHER_VERSIONS)
unset (_PYTHON2_VERSIONS)

# Search for newest python version if python executable isn't found
if (NOT PYTHON2_EXECUTABLE)
	foreach (_CURRENT_VERSION ${_Python2_VERSIONS})
		set (_Python_NAMES python${_CURRENT_VERSION})
		if (WIN32)
			list (APPEND _Python_NAMES python)
		endif ()
		find_program (
			PYTHON2_EXECUTABLE
			NAMES ${_Python_NAMES}
			PATHS [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath])
	endforeach ()
endif ()

# determine python version string
if (PYTHON2_EXECUTABLE)
	execute_process (
		COMMAND "${PYTHON2_EXECUTABLE}" -c "import sys; sys.stdout.write(';'.join([str(x) for x in sys.version_info[:3]]))"
		OUTPUT_VARIABLE _VERSION
		RESULT_VARIABLE _PYTHON2_VERSION_RESULT
		ERROR_QUIET)
	if (NOT _PYTHON2_VERSION_RESULT)
		string (REPLACE ";" "." PYTHON2_VERSION_STRING "${_VERSION}")
		list (GET _VERSION 0 PYTHON2_VERSION_MAJOR)
		list (GET _VERSION 1 PYTHON2_VERSION_MINOR)
		list (GET _VERSION 2 PYTHON2_VERSION_PATCH)
		if (PYTHON2_VERSION_PATCH EQUAL 0) # it's called "Python 2.7", not "2.7.0"
			string (REGEX REPLACE "\\.0$" "" PYTHON2_VERSION_STRING "${PYTHON2_VERSION_STRING}")
		endif ()
	else ()

		# sys.version predates sys.version_info, so use that
		execute_process (
			COMMAND "${PYTHON2_EXECUTABLE}" -c "import sys; sys.stdout.write(sys.version)"
			OUTPUT_VARIABLE _VERSION
			RESULT_VARIABLE _PYTHON2_VERSION_RESULT
			ERROR_QUIET)
		if (NOT _PYTHON2_VERSION_RESULT)
			string (REGEX REPLACE " .*" "" PYTHON2_VERSION_STRING "${_VERSION}")
			string (REGEX REPLACE "^([0-9]+)\\.[0-9]+.*" "\\1" PYTHON2_VERSION_MAJOR "${PYTHON2_VERSION_STRING}")
			string (REGEX REPLACE "^[0-9]+\\.([0-9])+.*" "\\1" PYTHON2_VERSION_MINOR "${PYTHON2_VERSION_STRING}")
			if (PYTHON2_VERSION_STRING MATCHES "^[0-9]+\\.[0-9]+\\.[0-9]+.*")
				string (REGEX REPLACE "^[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" PYTHON2_VERSION_PATCH
						      "${PYTHON2_VERSION_STRING}")
			else ()
				set (PYTHON2_VERSION_PATCH "0")
			endif ()
		else ()

			# sys.version was first documented for Python 1.5, so assume this is older.
			set (PYTHON2_VERSION_STRING "1.4")
			set (PYTHON2_VERSION_MAJOR "1")
			set (PYTHON2_VERSION_MAJOR "4")
			set (PYTHON2_VERSION_MAJOR "0")
		endif ()
	endif ()
	unset (_PYTHON2_VERSION_RESULT)
	unset (_VERSION)
endif (PYTHON2_EXECUTABLE)

# handle the QUIETLY and REQUIRED arguments and set PYTHON2INTERP_FOUND to TRUE if all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
	Python2Interp
	REQUIRED_VARS PYTHON2_EXECUTABLE
	VERSION_VAR PYTHON2_VERSION_STRING)

mark_as_advanced (PYTHON2_EXECUTABLE)
