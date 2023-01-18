if (BUILD_TESTING)
	add_test (NAME "test_bindings_elixir" COMMAND sh -c "cd \"${CMAKE_CURRENT_BINARY_DIR}\" && mix test")
	set_property (TEST "test_bindings_elixir" PROPERTY LABELS memleak bindings)
endif (BUILD_TESTING)
