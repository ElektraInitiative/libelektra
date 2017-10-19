#
# Much faster is:
# xargs rm < install_manifest.txt
#

set(MANIFEST "${CMAKE_BINARY_DIR}/install_manifest.txt")

if (NOT EXISTS "${MANIFEST}")
	message (FATAL_ERROR "Cannot find install manifest: ${MANIFEST}")
endif (NOT EXISTS "${MANIFEST}")

#message (MANIFEST IS ${MANIFEST})

# =========
# = Files =
# =========

file (READ "${MANIFEST}" files)
string (APPEND files "\n/usr/local/lib/python2.7/site-packages/elektra_gen-${KDB_VERSION}-py2.7.egg-info")
string (REGEX REPLACE "\n" ";" files "${files}")
foreach (file ${files})
	message(STATUS "Uninstalling $ENV{DESTDIR}${file}")
	if (IS_SYMLINK "$ENV{DESTDIR}${file}" OR EXISTS "$ENV{DESTDIR}${file}")
		exec_program(
			"${CMAKE_COMMAND}" ARGS "-E remove \"$ENV{DESTDIR}${file}\""
			OUTPUT_VARIABLE rm_out
			RETURN_VALUE rm_retval
			)
		if (NOT "${rm_retval}" STREQUAL 0)
			message (FATAL_ERROR "Problem when removing $ENV{DESTDIR}${file}")
		endif (NOT "${rm_retval}" STREQUAL 0)
	else (IS_SYMLINK "$ENV{DESTDIR}${file}" OR EXISTS "$ENV{DESTDIR}${file}")
		message (STATUS "File $ENV{DESTDIR}${file} does not exist.")
	endif (IS_SYMLINK "$ENV{DESTDIR}${file}" OR EXISTS "$ENV{DESTDIR}${file}")
endforeach (file)

# ===============
# = Directories =
# ===============

function(remove_directories directories)
	foreach (directory ${directories})
		set (dir "$ENV{DESTDIR}${directory}")
		if (EXISTS "${dir}")
			message(STATUS "Uninstalling directory ${dir}")
			exec_program (
				"${CMAKE_COMMAND}"
				ARGS "-E remove_directory \"${dir}\""
				OUTPUT_VARIABLE rm_out
				RETURN_VALUE rm_retval)

			if (NOT "${rm_retval}" STREQUAL 0)
				message (FATAL_ERROR "Problem when removing ${dir}")
			endif (NOT "${rm_retval}" STREQUAL 0)
		endif (EXISTS "${dir}")
	endforeach (directory ${directories})
endfunction(remove_directories)

set (DIRECTORIES
	"${CMAKE_INSTALL_PREFIX}/include/elektra"
	"${CMAKE_INSTALL_PREFIX}/lib/cmake/Elektra"
	"${CMAKE_INSTALL_PREFIX}/lib/elektra"
	"${CMAKE_INSTALL_PREFIX}/share/doc/elektra"
	"${CMAKE_INSTALL_PREFIX}/share/doc/elektra-api"
	"${CMAKE_INSTALL_PREFIX}/share/elektra"
	"${CMAKE_INSTALL_PREFIX}/share/share/elektra"
	"/usr/local/lib/python2.7/site-packages/support"
)

remove_directories ("${DIRECTORIES}")

# The following directories might be empty. The order of the directories is important, since we remove them in the given order.
# A directory that occurs later in the list might be empty, since we removed all its subdirectories before.
set (REMOVAL_CANDIDATES
	"${CMAKE_INSTALL_PREFIX}/bin"
	"${CMAKE_INSTALL_PREFIX}/include"
	"${CMAKE_INSTALL_PREFIX}/lib/cmake"
	"${CMAKE_INSTALL_PREFIX}/lib/lua/5.2"
	"${CMAKE_INSTALL_PREFIX}/lib/lua"
	"${CMAKE_INSTALL_PREFIX}/lib/pkgconfig"
	"${CMAKE_INSTALL_PREFIX}/lib/python2.7/site-packages"
	"${CMAKE_INSTALL_PREFIX}/lib/python2.7"
	"${CMAKE_INSTALL_PREFIX}/share/appdata"
	"${CMAKE_INSTALL_PREFIX}/share/applications"
	"${CMAKE_INSTALL_PREFIX}/share/doc"
	"${CMAKE_INSTALL_PREFIX}/share/icons/hicolor/scalable/apps"
	"${CMAKE_INSTALL_PREFIX}/share/icons/hicolor/scalable"
	"${CMAKE_INSTALL_PREFIX}/share/icons/hicolor"
	"${CMAKE_INSTALL_PREFIX}/share/icons"
	"${CMAKE_INSTALL_PREFIX}/share/java"
	"${CMAKE_INSTALL_PREFIX}/share/man/man1"
	"${CMAKE_INSTALL_PREFIX}/share/man/man3"
	"${CMAKE_INSTALL_PREFIX}/share/man/man7"
	"${CMAKE_INSTALL_PREFIX}/share/man"
	"${CMAKE_INSTALL_PREFIX}/share"
	"/usr/share/bash-completion/completions"
	"/usr/share/bash-completion"
	"/usr/share/zsh/vendor-completions"
	"/usr/share/zsh"
	"/usr/share"
)

foreach (directory ${REMOVAL_CANDIDATES})
	set (dir "$ENV{DESTDIR}${directory}")
	file (GLOB content "${dir}/*")
	list (LENGTH content size)
	if (size EQUAL 0)
		remove_directories ("${directory}")
	endif (size EQUAL 0)
endforeach (directory in ${REMOVAL_CANDIDATES})
