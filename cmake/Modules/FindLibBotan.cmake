# Try to find the Botan library.
#
# Defines:
#
#  BOTAN_FOUND - set if the system has the Botan library
#  BOTAN_LIBRARIES - the linker libraries needed to use the Botan library
#  BOTAN_INCLUDE_DIRS - the path to the include files of the Botan library
#
# Copyright (c) 2017 Peter Nirschl <peter.nirschl@gmail.com>
#                                                                                                                    
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

if (NOT BOTAN_FOUND)
  include (FindPkgConfig)
  pkg_search_module (BOTAN QUIET botan-1.10 botan-1.9 botan-1.8 botan)
endif ()

if (BOTAN_FOUND)
  # try to compile and link a minimal sample program against libbotan  
  try_compile (HAS_BOTAN_4SURE
    "${CMAKE_BINARY_DIR}"
    "${PROJECT_SOURCE_DIR}/src/plugins/crypto/compile_botan.cpp"
    CMAKE_FLAGS
      -DINCLUDE_DIRECTORIES:STRING=${BOTAN_INCLUDE_DIRS}
      -DLINK_LIBRARIES:PATH=${BOTAN_LIBRARIES}
  )

  if (NOT HAS_BOTAN_4SURE)
    message (STATUS "Botan compile/linker test failed")
    set (BOTAN_FOUND OFF)
    set (BOTAN_INCLUDE_DIR "")
    set (BOTAN_LIBRARIES "")
  endif ()

  unset (HAS_BOTAN_4SURE)
endif ()
