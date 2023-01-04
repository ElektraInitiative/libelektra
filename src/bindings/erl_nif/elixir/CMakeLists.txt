if (BUILD_TESTING)
	add_test (NAME "bindings-elixir" COMMAND sh -c "cd ${CMAKE_CURRENT_BINARY_DIR} && mix test")
endif (BUILD_TESTING)
