# ~~~
# Much faster is:
# xargs rm < install_manifest.txt extra_install_manifest.txt
# ~~~

set (MANIFEST "${CMAKE_BINARY_DIR}/install_manifest.txt")
set (EXTRA_MANIFEST "${CMAKE_BINARY_DIR}/extra_install_manifest.txt")

if (NOT EXISTS "${MANIFEST}")
	message (FATAL_ERROR "Cannot find install manifest: ${MANIFEST}")
endif (NOT EXISTS "${MANIFEST}")

# message (MANIFEST IS ${MANIFEST})
file (READ "${MANIFEST}" files)

file (READ "${EXTRA_MANIFEST}" extra_files)
string (APPEND files "${extra_files}")

# ==========
# = Python =
# ==========

set (PYTHON_GET_MODULES_DIR_COMMAND
     "from distutils.sysconfig import get_python_lib; print(get_python_lib(True, prefix='${CMAKE_INSTALL_PREFIX}'))")

find_package (PythonInterp 3 QUIET)
find_package (PythonLibs 3 QUIET)

if (PYTHONINTERP_FOUND)
	execute_process (
		COMMAND ${PYTHON_EXECUTABLE} -c "${PYTHON_GET_MODULES_DIR_COMMAND}"
		OUTPUT_VARIABLE PYTHON_SITE_PACKAGES
		OUTPUT_STRIP_TRAILING_WHITESPACE)
	string (APPEND files
		"\n${PYTHON_SITE_PACKAGES}/elektra_gen-${KDB_VERSION}-py${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}.egg-info"
		"\n${PYTHON_SITE_PACKAGES}/__pycache__/kdb.cpython-${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR}.pyc")
endif (PYTHONINTERP_FOUND)

# =========
# = Files =
# =========

string (REGEX REPLACE "\n" ";" files "${files}")
foreach (file ${files})
	message (STATUS "Uninstalling $ENV{DESTDIR}${file}")
	if (IS_SYMLINK "$ENV{DESTDIR}${file}" OR EXISTS "$ENV{DESTDIR}${file}")
		execute_process (
			COMMAND "${CMAKE_COMMAND}" -E remove "$ENV{DESTDIR}${file}"
			OUTPUT_VARIABLE rm_out
			RESULT_VARIABLE rm_retval)
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

function (remove_directories directories)
	foreach (directory ${directories})
		set (dir "$ENV{DESTDIR}${directory}")
		if (EXISTS "${dir}")
			message (STATUS "Uninstalling directory ${dir}")
			execute_process (
				COMMAND "${CMAKE_COMMAND}" -E remove_directory "${dir}"
				OUTPUT_VARIABLE rm_out
				RESULT_VARIABLE rm_retval)

			if (NOT "${rm_retval}" STREQUAL 0)
				message (FATAL_ERROR "Problem when removing ${dir}")
			endif (NOT "${rm_retval}" STREQUAL 0)
		endif (EXISTS "${dir}")
	endforeach (directory ${directories})
endfunction (remove_directories)

set (
	DIRECTORIES
	"${CMAKE_INSTALL_PREFIX}/include/elektra"
	"${CMAKE_INSTALL_PREFIX}/lib/cmake/Elektra"
	"${CMAKE_INSTALL_PREFIX}/lib/elektra"
	"${CMAKE_INSTALL_PREFIX}/share/doc/elektra"
	"${CMAKE_INSTALL_PREFIX}/share/doc/elektra-api"
	"${CMAKE_INSTALL_PREFIX}/share/elektra"
	"${CMAKE_INSTALL_PREFIX}/share/share/elektra")
if (${PYTHON_SITE_PACKAGES})
	list (APPEND DIRECTORIES "${PYTHON_SITE_PACKAGES}/support")
endif (${PYTHON_SITE_PACKAGES})

remove_directories ("${DIRECTORIES}")

# The following directories might be empty. The order of the directories is important, since we remove them in the given order. A directory
# that occurs later in the list might be empty, since we removed all its subdirectories before.
set (
	REMOVAL_CANDIDATES
	"${CMAKE_INSTALL_PREFIX}/bin"
	"${CMAKE_INSTALL_PREFIX}/include"
	"${CMAKE_INSTALL_PREFIX}/lib/cmake"
	"${CMAKE_INSTALL_PREFIX}/lib/lua/5.2"
	"${CMAKE_INSTALL_PREFIX}/lib/lua"
	"${CMAKE_INSTALL_PREFIX}/lib/pkgconfig"
	"${CMAKE_INSTALL_PREFIX}/lib/python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}/site-packages/__pycache__"
	"${CMAKE_INSTALL_PREFIX}/lib/python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}/site-packages/kdb"
	"${CMAKE_INSTALL_PREFIX}/lib/python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}/site-packages"
	"${CMAKE_INSTALL_PREFIX}/lib/python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}"
	"${CMAKE_INSTALL_PREFIX}/lib/ruby/site_ruby"
	"${CMAKE_INSTALL_PREFIX}/lib/ruby"
	"${CMAKE_INSTALL_PREFIX}/lib"
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
	"/usr/share")

foreach (directory ${REMOVAL_CANDIDATES})
	set (dir "$ENV{DESTDIR}${directory}")
	file (GLOB content "${dir}/*")
	list (LENGTH content size)
	if (size EQUAL 0)
		remove_directories ("${directory}")
	endif (size EQUAL 0)
endforeach (directory in ${REMOVAL_CANDIDATES})
