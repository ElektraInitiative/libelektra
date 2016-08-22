

file (GLOB TESTS *.rb)
foreach (file ${TESTS})
        get_filename_component (name ${file} NAME_WE)
        add_test (
                NAME ${name}
                COMMAND ${RUBY_EXECUTABLE} ${file} 
        )

        # set RUBYLIB to find newly built binding lib
        set_property (
                TEST ${name}
                PROPERTY ENVIRONMENT
                "RUBYLIB=${CMAKE_CURRENT_BINARY_DIR}/.."
        )
    
endforeach (file ${TESTS})
