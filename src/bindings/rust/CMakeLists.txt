find_program (CARGO_EXECUTABLE
	      cargo)

if (CARGO_EXECUTABLE)
	add_binding (rust)

	# Configure include paths s.t. headers for bindings can be found
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra-sys/build.rs.in"
			"${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/build.rs"
			@ONLY)

	# Configure Cargo.toml to insert the kdb version
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra-sys/Cargo.toml.in"
			"${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/Cargo.toml"
			@ONLY)

	file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/elektra-sys" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")

	add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/target/release/libelektra_sys.rlib"
			    COMMAND ${CARGO_EXECUTABLE}
				    build
				    --release
				    --quiet
				WORKING_DIRECTORY elektra-sys
				DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/build.rs"
				    "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/Cargo.toml")

	add_custom_target (rust
				ALL
			   	DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/target/release/libelektra_sys.rlib")

	add_test (NAME test_cargo_elektra_sys
				COMMAND ${CARGO_EXECUTABLE} test
				WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys")

else ()
	exclude_binding (rust,
			 "cargo not found")
	return ()
endif ()
