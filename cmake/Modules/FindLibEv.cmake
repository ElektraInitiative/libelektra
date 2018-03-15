# EOS - The CERN Disk Storage System
# Copyright (C) 2015 CERN/Switzerland
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version. This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see http://www.gnu.org/licenses/.
# 
# Try to find libev
# Once done, this will define
#
# LIBEV_FOUND        - system has libev
# LIBEV_INCLUDE_DIRS - libev include directories
# LIBEV_LIBRARIES    - libraries needed to use libev

if(LIBEV_INCLUDE_DIRS AND LIBEV_LIBRARIES)
  set(LIBEV_FIND_QUIETLY TRUE)
else()
  find_path(
    LIBEV_INCLUDE_DIR
    NAMES ev.h
    HINTS ${LIBEV_ROOT_DIR}
    PATH_SUFFIXES include)

  find_library(
    LIBEV_LIBRARY
    NAME ev
    HINTS ${LIBEV_ROOT_DIR}
    PATH_SUFFIXES ${CMAKE_INSTALL_LIBDIR})

  set(LIBEV_INCLUDE_DIRS ${LIBEV_INCLUDE_DIR})
  set(LIBEV_LIBRARIES ${LIBEV_LIBRARY})

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(
    libev DEFAULT_MSG LIBEV_LIBRARY LIBEV_INCLUDE_DIR)

  mark_as_advanced(LIBEV_LIBRARY LIBEV_INCLUDE_DIR)
endif()
