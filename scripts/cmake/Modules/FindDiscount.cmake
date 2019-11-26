# ~~~
# -------------------------------------------------------------------------------
# Copyright (c) 2013-2013, Lars Baehren <lbaehren@gmail.com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
#
#  * Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# -------------------------------------------------------------------------------
# ~~~

# ~~~
# DISCOUNT is an implementation of John Gruber's Markdown markup language.
#
# DISCOUNT is free software written by David Parsons <orc@pell.chi.il.us>;
# it is released under a BSD-style license that allows you to do
# as you wish with it as long as you don't attempt to claim it as
# your own work.
#
# The following variables are set when DISCOUNT is found:
#  DISCOUNT_FOUND      = Set to true, if all components of DISCOUNT have been found.
#  DISCOUNT_INCLUDES   = Include path for the header files of DISCOUNT
#  DISCOUNT_LIBRARIES  = Link these to use DISCOUNT
#  DISCOUNT_LFLAGS     = Linker flags (optional)
# ~~~

if (NOT DISCOUNT_FOUND)

	if (NOT DISCOUNT_ROOT_DIR)
		set (DISCOUNT_ROOT_DIR ${CMAKE_INSTALL_PREFIX})
	endif (NOT DISCOUNT_ROOT_DIR)

	# ____________________________________________________________________________ Check for the header files

	find_path (
		DISCOUNT_INCLUDES
		NAMES mkdio.h
		HINTS ${DISCOUNT_ROOT_DIR} ${CMAKE_INSTALL_PREFIX}
		PATH_SUFFIXES include)

	# ____________________________________________________________________________ Check for the library

	find_library (
		DISCOUNT_LIBRARIES markdown
		HINTS ${DISCOUNT_ROOT_DIR} ${CMAKE_INSTALL_PREFIX}
		PATH_SUFFIXES lib)

	# ____________________________________________________________________________ Check for the executable

	find_program (
		MARKDOWN_EXECUTABLE markdown
		HINTS ${DISCOUNT_ROOT_DIR} ${CMAKE_INSTALL_PREFIX}
		PATH_SUFFIXES bin)

	# ____________________________________________________________________________ Actions taken when all components have been found

	find_package_handle_standard_args (DISCOUNT DEFAULT_MSG DISCOUNT_LIBRARIES DISCOUNT_INCLUDES MARKDOWN_EXECUTABLE)

	if (DISCOUNT_FOUND)

		# Update DISCOUNT_ROOT DIR
		get_filename_component (_name ${MARKDOWN_EXECUTABLE} NAME)
		string (REGEX REPLACE "/bin/${_name}" "" DISCOUNT_ROOT_DIR ${MARKDOWN_EXECUTABLE}) # Display variables
		if (NOT DISCOUNT_FIND_QUIETLY)
			message (STATUS "Found components for DISCOUNT")
			message (STATUS "DISCOUNT_ROOT_DIR   = ${DISCOUNT_ROOT_DIR}")
			message (STATUS "DISCOUNT_INCLUDES   = ${DISCOUNT_INCLUDES}")
			message (STATUS "DISCOUNT_LIBRARIES  = ${DISCOUNT_LIBRARIES}")
			message (STATUS "MARKDOWN_EXECUTABLE = ${MARKDOWN_EXECUTABLE}")
		endif (NOT DISCOUNT_FIND_QUIETLY)
	else (DISCOUNT_FOUND)
		if (DISCOUNT_FIND_REQUIRED)
			message (FATAL_ERROR "Could not find DISCOUNT!")
		endif (DISCOUNT_FIND_REQUIRED)
	endif (DISCOUNT_FOUND)

	# ____________________________________________________________________________ Mark advanced variables

	mark_as_advanced (DISCOUNT_ROOT_DIR DISCOUNT_INCLUDES DISCOUNT_LIBRARIES MARKDOWN_EXECUTABLE)

endif (NOT DISCOUNT_FOUND)
