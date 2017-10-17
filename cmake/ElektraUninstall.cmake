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
	"/usr/local/include/elektra"
	"/usr/local/lib/cmake/Elektra"
	"/usr/local/lib/elektra"
	"/usr/local/share/doc/elektra"
	"/usr/local/share/doc/elektra-api"
	"/usr/local/share/elektra"
	"/usr/local/share/share/elektra"
	"/usr/local/lib/python2.7/site-packages/support"
)

remove_directories ("${DIRECTORIES}")

# The following directories might be empty. The order of the directories is important, since we remove them in the given order.
# A directory that occurs later in the list might be empty, since we removed all its subdirectories before.
set (REMOVAL_CANDIDATES
	"/usr/local/bin"
	"/usr/local/include"
	"/usr/local/lib/cmake"
	"/usr/local/lib/lua/5.2"
	"/usr/local/lib/lua"
	"/usr/local/lib/pkgconfig"
	"/usr/local/lib/python2.7/site-packages"
	"/usr/local/lib/python2.7"
	"/usr/local/share/appdata"
	"/usr/local/share/applications"
	"/usr/local/share/doc"
	"/usr/local/share/icons/hicolor/scalable/apps"
	"/usr/local/share/icons/hicolor/scalable"
	"/usr/local/share/icons/hicolor"
	"/usr/local/share/icons"
	"/usr/local/share/java"
	"/usr/local/share/man/man1"
	"/usr/local/share/man/man3"
	"/usr/local/share/man/man7"
	"/usr/local/share/man"
	"/usr/local/share"
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
