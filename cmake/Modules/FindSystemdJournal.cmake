# Finds systemd-journal and its libraries
# Uses the same semantics as pkg_check_modules, i.e. LIBSYSTEMD_JOURNAL{_FOUND,_INCLUDE_DIR,_LIBRARIES}
#
# This is an adapted version of FindSystemd.cmake: 
# Copyright: Red Hat, Inc. 2013
# Author: Martin Briza <mbriza@redhat.com>
#
# Distributed under the BSD license. See COPYING-CMAKE-SCRIPTS for details.

if (LIBSYSTEMD_JOURNAL_INCLUDE_DIR AND LIBSYSTEMD_ID128_INCLUDE_DIR)
  # in cache already
  set (LIBSYSTEMD_JOURNAL_FOUND TRUE)
else (LIBSYSTEMD_JOURNAL_INCLUDE_DIR AND LIBSYSTEMD_ID128_INCLUDE_DIR)

  # try to find systemd-journal via pkg-config
  find_package(PkgConfig)

  if (PKG_CONFIG_FOUND)
    pkg_check_modules(_LIBSYSTEMD_JOURNAL_PC QUIET "libsystemd-journal")
    pkg_check_modules(LIBSYSTEMD_ID128 QUIET "libsystemd-id128")
  endif (PKG_CONFIG_FOUND)

  find_path (LIBSYSTEMD_JOURNAL_INCLUDE_DIR systemd/sd-journal.h
    ${_LIBSYSTEMD_JOURNAL_PC_INCLUDE_DIRS}
    /usr/include
    /usr/local/include
  )
  
  find_path (LIBSYSTEMD_ID128_INCLUDE_DIR systemd/sd-id128.h
    ${_LIBSYSTEMD_ID128_PC_INCLUDE_DIRS}
    /usr/include
    /usr/local/include
  )

  find_library (LIBSYSTEMD_JOURNAL_LIBRARIES NAMES systemd-journal systemd-id128
    PATHS
    ${_LIBSYSTEMD_JOURNAL_PC_LIBDIR}
  )

  


  if (LIBSYSTEMD_JOURNAL_INCLUDE_DIR AND LIBSYSTEMD_ID128_INCLUDE_DIR AND LIBSYSTEMD_JOURNAL_LIBRARIES)
    set (LIBSYSTEMD_JOURNAL_FOUND TRUE)
  endif (LIBSYSTEMD_JOURNAL_INCLUDE_DIR AND LIBSYSTEMD_ID128_INCLUDE_DIR AND LIBSYSTEMD_JOURNAL_LIBRARIES)

  if (LIBSYSTEMD_JOURNAL_FOUND)
    if (NOT LIBSYSTEMD_JOURNAL_FIND_QUIETLY)
      message(STATUS "Found systemd-journal: ${LIBSYSTEMD_JOURNAL_LIBRARIES}")
    endif (NOT LIBSYSTEMD_JOURNAL_FIND_QUIETLY)
  else (LIBSYSTEMD_JOURNAL_FOUND)
    if (LIBSYSTEMD_JOURNAL_FIND_REQUIRED)
      message(FATAL_ERROR "Could NOT find systemd-journal")
    endif (LIBSYSTEMD_JOURNAL_FIND_REQUIRED)
  endif (LIBSYSTEMD_JOURNAL_FOUND)

  mark_as_advanced(LIBSYSTEMD_JOURNAL_INCLUDE_DIR LIBSYSTEMD_ID128_INCLUDE_DIR LIBSYSTEMD_JOURNAL_LIBRARIES)

endif (LIBSYSTEMD_JOURNAL_INCLUDE_DIR AND LIBSYSTEMD_ID128_INCLUDE_DIR)
