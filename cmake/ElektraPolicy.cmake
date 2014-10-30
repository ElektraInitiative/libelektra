if (POLICY CMP0026)
	cmake_policy(SET CMP0026 OLD) # reinvestigate at 2.8.11 or later
endif()

if (POLICY CMP0046)
	cmake_policy(SET CMP0046 OLD) # new behaviour seems stupid, of course
	# dependency is not yet available if it's generated later?
endif()

