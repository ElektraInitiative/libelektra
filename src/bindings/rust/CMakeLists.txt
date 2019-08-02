find_program (CARGO_EXECUTABLE cargo)

if (CARGO_EXECUTABLE)
	add_binding (rust)

	file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")

	# Configure include paths s.t. headers for bindings can be found
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra-sys/build.rs.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/build.rs" @ONLY)

	# Configure Cargo.toml to insert the kdb version
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra-sys/Cargo.toml.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/Cargo.toml" @ONLY)

	# Configure Cargo.toml to insert the kdb version
	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/elektra/Cargo.toml.in" "${CMAKE_CURRENT_BINARY_DIR}/elektra/Cargo.toml" @ONLY)

	add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/target/release/libelektra.rlib"
                COMMAND ${CARGO_EXECUTABLE} build --release
			    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
				DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/build.rs" "${CMAKE_CURRENT_BINARY_DIR}/elektra-sys/Cargo.toml")

	add_custom_target (rust ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/target/release/libelektra.rlib")

	add_test (NAME test_cargo_elektra COMMAND ${CARGO_EXECUTABLE} test WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")

	set_property (TEST test_cargo_elektra PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

	set_property (TEST test_cargo_elektra PROPERTY LABELS kdbtests bindings)

else ()
	exclude_binding (rust, "cargo not found")
	return ()
endif ()
