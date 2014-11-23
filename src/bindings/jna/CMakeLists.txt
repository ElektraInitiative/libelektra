cmake_minimum_required (VERSION 2.8.11)
#for add_jar INCLUDE_JARS

find_package(Java)
include(UseJava)

if (Java_VERSION_MAJOR GREATER 0 AND Java_VERSION_MINOR GREATER 7)
	file(GLOB files elektra/*.java)
	file(GLOB pfiles elektra/plugin/*.java)

# find_jar(<VAR>
#          name | NAMES name1 [name2 ...]
#          [PATHS path1 [path2 ... ENV var]]
#          [VERSIONS version1 [version2]]
#          [DOC "cache documentation string"]
#         )

	find_jar(jna
		NAMES jna
		PATHS /usr/share/java
		)

	# should work, too
	# http://www.cmake.org/cmake/help/v3.0/module/UseJava.html
	#set(CMAKE_JAVA_INCLUDE_PATH ${jna})

	add_jar(elektrajar
		INCLUDE_JARS ${jna}
		SOURCES ${files} ${pfiles}
		VERSION 1
		OUTPUT_NAME elektra.jar
		)

	install_jar(elektrajar share/java)

else()
	remove_binding(jna "Java is not greater version 8, but ${Java_VERSION} was found")
endif()
