# ~~~
# .rst:
# FindLua
# -------
#
#
#
# Locate Lua library This module defines
#
# ::
#
#   LUA_FOUND          - if false, do not try to link to Lua
#   LUA_LIBRARIES      - both lua and lualib
#   LUA_INCLUDE_DIR    - where to find lua.h
#   LUA_VERSION_STRING - the version of Lua found
#   LUA_VERSION_MAJOR  - the major version of Lua
#   LUA_VERSION_MINOR  - the minor version of Lua
#   LUA_VERSION_PATCH  - the patch version of Lua
#
#
#
# Note that the expected include convention is
#
# ::
#
#   #include "lua.h"
#
# and not
#
# ::
#
#   #include <lua/lua.h>
#
# This is because, the lua location is not standardized and may exist in
# locations other than lua/
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

unset (_lua_include_subdirs)
unset (_lua_library_names)

# this is a function only to have all the variables inside go away automatically
function (set_lua_version_vars)
	set (LUA_VERSIONS5 5.4 5.3 5.2 5.1 5.0)

	if (Lua_FIND_VERSION_EXACT)
		if (Lua_FIND_VERSION_COUNT GREATER 1)
			set (lua_append_versions ${Lua_FIND_VERSION_MAJOR}.${Lua_FIND_VERSION_MINOR})
		endif ()
	elseif (Lua_FIND_VERSION)

		if (NOT Lua_FIND_VERSION_MAJOR GREATER 5) # once there is a different major version supported this should become a loop
			if (Lua_FIND_VERSION_COUNT EQUAL 1)
				set (lua_append_versions ${LUA_VERSIONS5})
			else ()
				foreach (subver IN LISTS LUA_VERSIONS5)
					if (NOT subver VERSION_LESS ${Lua_FIND_VERSION})
						list (APPEND lua_append_versions ${subver})
					endif ()
				endforeach ()
			endif ()
		endif ()
	else ()
		set (lua_append_versions ${LUA_VERSIONS5}) # once there is a different major version supported this should become a loop
	endif ()

	foreach (ver IN LISTS lua_append_versions)
		string (REGEX MATCH "^([0-9]+)\\.([0-9]+)$" _ver "${ver}")
		list (APPEND _lua_include_subdirs include/lua${CMAKE_MATCH_1}${CMAKE_MATCH_2} include/lua${CMAKE_MATCH_1}.${CMAKE_MATCH_2}
		      include/lua-${CMAKE_MATCH_1}.${CMAKE_MATCH_2})
		list (APPEND _lua_library_names lua${CMAKE_MATCH_1}${CMAKE_MATCH_2} lua${CMAKE_MATCH_1}.${CMAKE_MATCH_2}
		      lua-${CMAKE_MATCH_1}.${CMAKE_MATCH_2})
	endforeach ()

	set (
		_lua_include_subdirs
		"${_lua_include_subdirs}"
		PARENT_SCOPE)
	set (
		_lua_library_names
		"${_lua_library_names}"
		PARENT_SCOPE)
endfunction (set_lua_version_vars)

set_lua_version_vars ()

function (verify_lua_executable_version)
	set (
		LUA_EXECUTABLE_VERSION_MATCHED
		FALSE
		PARENT_SCOPE)
	set (LUA_FIND_QUIETLY ON)

	find_program (
		LUA_EXECUTABLE NAMES "lua-${_LUA_VERSION_MAJOR}.${_LUA_VERSION_MINOR}" "lua${_LUA_VERSION_MAJOR}.${_LUA_VERSION_MINOR}"
				     "lua${_LUA_VERSION_MAJOR}${_LUA_VERSION_MINOR}" "lua" PATH)

	execute_process (
		COMMAND ${LUA_EXECUTABLE} "-v"
		OUTPUT_VARIABLE LUABIN_VERSION_STRING
		ERROR_VARIABLE LUABIN_VERSION_STRING
		OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE)

	if (LUA_EXECUTABLE AND (NOT LUABIN_VERSION_STRING STREQUAL ""))

		# Compare only MAJOR.MINOR
		string (SUBSTRING ${LUABIN_VERSION_STRING} "4" "3" LUABIN_VERSION_STRING)
		string (COMPARE EQUAL ${LUABIN_VERSION_STRING} "${_LUA_VERSION_MAJOR}.${_LUA_VERSION_MINOR}" VERSION_MATCHES)

		if (NOT VERSION_MATCHES)
			message (WARNING "Lua executable does not match lua library version")
			message ("Lua library: ${_LUA_VERSION_MAJOR}.${_LUA_VERSION_MINOR}")
			message ("Lua executable: ${LUABIN_VERSION_STRING}")
		else ()
			set (
				LUA_EXECUTABLE_VERSION_MATCHED
				TRUE
				PARENT_SCOPE)
		endif (NOT VERSION_MATCHES)
	else ()
		if (NOT LUA_FIND_QUIETLY)
			message (WARNING "Lua executable not found")
		endif (NOT LUA_FIND_QUIETLY)
	endif (LUA_EXECUTABLE AND (NOT LUABIN_VERSION_STRING STREQUAL ""))
endfunction (verify_lua_executable_version)

find_path (
	LUA_INCLUDE_DIR lua.h
	HINTS ENV LUA_DIR
	PATH_SUFFIXES ${_lua_include_subdirs} include/lua include
	PATHS ~/Library/Frameworks
	      /Library/Frameworks
	      /sw # Fink
	      /opt/local # DarwinPorts
	      /opt/csw # Blastwave
	      /opt)
unset (_lua_include_subdirs)

find_library (
	LUA_LIBRARY
	NAMES ${_lua_library_names} lua
	HINTS ENV LUA_DIR
	PATH_SUFFIXES lib
	PATHS ~/Library/Frameworks /Library/Frameworks /sw /opt/local /opt/csw /opt)
unset (_lua_library_names)

if (LUA_LIBRARY)

	if (UNIX
	    AND NOT APPLE
	    AND NOT BEOS) # include the math library for Unix
		find_library (LUA_MATH_LIBRARY m)
		set (LUA_LIBRARIES "${LUA_LIBRARY};${LUA_MATH_LIBRARY}")

	else () # For Windows and Mac, don't need to explicitly include the math library
		set (LUA_LIBRARIES "${LUA_LIBRARY}")
	endif ()
endif ()

if (LUA_INCLUDE_DIR AND EXISTS "${LUA_INCLUDE_DIR}/lua.h")

	# At least 5.[012] have different ways to express the version so all of them need to be tested. Lua 5.2 defines LUA_VERSION and
	# LUA_RELEASE as joined by the C preprocessor, so avoid those.
	file (STRINGS "${LUA_INCLUDE_DIR}/lua.h" lua_version_strings
	      REGEX "^#define[ \t]+LUA_(RELEASE[ \t]+\"Lua [0-9]|VERSION([ \t]+\"Lua [0-9]|_[MR])).*")

	string (REGEX REPLACE ".*;#define[ \t]+LUA_VERSION_MAJOR[ \t]+\"([0-9])\"[ \t]*;.*" "\\1" LUA_VERSION_MAJOR
			      ";${lua_version_strings};")
	if (LUA_VERSION_MAJOR MATCHES "^[0-9]+$")
		string (REGEX REPLACE ".*;#define[ \t]+LUA_VERSION_MINOR[ \t]+\"([0-9])\"[ \t]*;.*" "\\1" LUA_VERSION_MINOR
				      ";${lua_version_strings};")
		string (REGEX REPLACE ".*;#define[ \t]+LUA_VERSION_RELEASE[ \t]+\"([0-9])\"[ \t]*;.*" "\\1" LUA_VERSION_PATCH
				      ";${lua_version_strings};")
		set (LUA_VERSION_STRING "${LUA_VERSION_MAJOR}.${LUA_VERSION_MINOR}.${LUA_VERSION_PATCH}")
	else ()
		string (REGEX REPLACE ".*;#define[ \t]+LUA_RELEASE[ \t]+\"Lua ([0-9.]+)\"[ \t]*;.*" "\\1" LUA_VERSION_STRING
				      ";${lua_version_strings};")
		if (NOT LUA_VERSION_STRING MATCHES "^[0-9.]+$")
			string (REGEX REPLACE ".*;#define[ \t]+LUA_VERSION[ \t]+\"Lua ([0-9.]+)\"[ \t]*;.*" "\\1" LUA_VERSION_STRING
					      ";${lua_version_strings};")
		endif ()
		string (REGEX REPLACE "^([0-9]+)\\.[0-9.]*$" "\\1" LUA_VERSION_MAJOR "${LUA_VERSION_STRING}")
		string (REGEX REPLACE "^[0-9]+\\.([0-9]+)[0-9.]*$" "\\1" LUA_VERSION_MINOR "${LUA_VERSION_STRING}")
		string (REGEX REPLACE "^[0-9]+\\.[0-9]+\\.([0-9]).*" "\\1" LUA_VERSION_PATCH "${LUA_VERSION_STRING}")
	endif ()
	set (_LUA_VERSION_MAJOR "${LUA_VERSION_MAJOR}")
	set (_LUA_VERSION_MINOR "${LUA_VERSION_MINOR}")
	unset (lua_version_strings)
endif ()

verify_lua_executable_version () # LUA_VERSION_STRING available from here

include (FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set LUA_FOUND to TRUE if all listed variables are TRUE
find_package_handle_standard_args (
	Lua
	REQUIRED_VARS LUA_LIBRARIES LUA_INCLUDE_DIR
	VERSION_VAR LUA_VERSION_STRING)

mark_as_advanced (LUA_INCLUDE_DIR LUA_LIBRARY LUA_MATH_LIBRARY)
