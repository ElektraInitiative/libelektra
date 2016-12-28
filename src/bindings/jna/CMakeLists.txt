#versioning for libelektra.jar
set(CMAKE_JAVA_TARGET_VERSION ${KDB_VERSION})

find_package(Java)
include(UseJava)

if (CMAKE_MAJOR_VERSION EQUAL 2 AND CMAKE_MINOR_VERSION EQUAL 8)
	if (CMAKE_PATCH_VERSION LESS 11)
		#2.8.11 needed for add_jar INCLUDE_JARS
		remove_binding(jna "cmake 2.8.11 needed for add_jar")
		return()
	endif()
endif()

if (Java_VERSION_MAJOR GREATER 0 AND Java_VERSION_MINOR GREATER 7)
	file(GLOB files ./libelektra4j/src/main/java/org/libelektra/*.java)
	file(GLOB pfiles ./libelektra4j/src/main/java/org/libelektra/plugin/*.java)

# find_jar(<VAR>
#          name | NAMES name1 [name2 ...]
#          [PATHS path1 [path2 ... ENV var]]
#          [VERSIONS version1 [version2]]
#          [DOC "cache documentation string"]
#         )

	find_jar(jna
		NAMES jna
		PATHS /usr/share/java ./libelektra4j
		)

	if (jna)
		# should work, too
		# http://www.cmake.org/cmake/help/v3.0/module/UseJava.html
		#set(CMAKE_JAVA_INCLUDE_PATH ${jna})
		add_jar(libelektra
			INCLUDE_JARS ${jna}
			SOURCES ${files} ${pfiles}
			VERSION 1
			OUTPUT_NAME libelektra4j
			)

		install_jar(libelektra share/java)

	else(jna)
		remove_binding(jna "jna jar file not found, searched in standard paths and /usr/share/java")
	endif(jna)

else()
	remove_binding(jna "Java is not greater version 8, but ${Java_VERSION} was found")
endif()
