find_program (CARGO_EXECUTABLE cargo)

if (CARGO_EXECUTABLE)
	add_binding (rust ONLY_SHARED)

	file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")

	# Configure include paths s.t. headers for bindings can be found
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra-sys/build.rs.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/build.rs" @ONLY)

	# Configure Cargo.toml to insert the kdb version
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra-sys/Cargo.toml.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/Cargo.toml" @ONLY)

	# Configure Cargo.toml to insert the kdb version
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra/Cargo.toml.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra/Cargo.toml" @ONLY)

	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/example/Cargo.toml.in" "${CMAKE_CURRENT_BINARY_DIR}/example/Cargo.toml" @ONLY)

	# Call cargo to build the bindings in release mode
	add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/target/release/libelektra.rlib"
			    COMMAND ${CARGO_EXECUTABLE}
				    build
				    --release
				    --quiet
			    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
			    DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/build.rs" "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/Cargo.toml")

	# Call cargo to build the example in release mode
	add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/target/release/rust_bindings_example"
			    COMMAND ${CARGO_EXECUTABLE}
				    build
				    --release
				    --quiet
			    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR/example}
			    DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/example/Cargo.toml")

	add_custom_target (rust ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/target/release/libelektra.rlib")

	if ((NOT ENABLE_ASAN) AND BUILD_SHARED)
		add_test (NAME test_rust_elektra COMMAND ${CARGO_EXECUTABLE} test WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")

		# The test executables need to know where they can find the dynamic libraries
		set_property (TEST test_rust_elektra PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib" "RUST_BACKTRACE=1")
		set_property (TEST test_rust_elektra
			      PROPERTY LABELS
				       kdbtests
				       bindings
				       memleak)
	endif ()

else ()
	exclude_binding (rust, "cargo not found")
	return ()
endif ()
