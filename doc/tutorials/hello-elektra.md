# Hello, Elektra

This basic tutorial shows you how to compile and run a very basic Elektra application. For this tutorial we assume that you installed [Elektra](/doc/INSTALL.md) and [CMake](https://cmake.org) on your machine. We also assume that you work a Unix based OS like Linux or macOS.

1. Create a folder called `Hello` somewhere on your disk
2. Copy the file `examples/helloElektra.c` to the folder `Hello` you just created
3. Save a file with the following content

   ```cmake
   cmake_minimum_required(VERSION 3.0)

   find_package(Elektra REQUIRED)

   if (ELEKTRA_FOUND)
       message (STATUS "Elektra ${ELEKTRA_VERSION} found")
       include_directories (${ELEKTRA_INCLUDE_DIR})

       add_executable (hello helloElektra.c)
       target_link_libraries (hello ${ELEKTRA_LIBRARIES})
   else (ELEKTRA_FOUND)
       message (FATAL_ERROR "Elektra not found")
   endif (ELEKTRA_FOUND)
   ```

   as `CMakeLists.txt` in the folder `Hello`.

4. Open a shell and change into the directory `Hello`
5. Create a build directory inside `Hello`, change into the build directory, and run Cmake:

   ```sh
   mkdir build
   cd build
   cmake ..
   ```

   . If everything worked until now, then CMake should print messages that look something like this:

   ```
   -- The C compiler identification is Clang 13.0.1
   -- The CXX compiler identification is Clang 13.0.1
   -- Check for working C compiler: usr/bin/cc
   -- Check for working C compiler: usr/bin/cc -- works
   -- Detecting C compiler ABI info
   -- Detecting C compiler ABI info - done
   -- Detecting C compile features
   -- Detecting C compile features - done
   -- Check for working CXX compiler: usr/bin/c++
   -- Check for working CXX compiler: usr/bin/c++ -- works
   -- Detecting CXX compiler ABI info
   -- Detecting CXX compiler ABI info - done
   -- Detecting CXX compile features
   -- Detecting CXX compile features - done
   -- Elektra 0.11.0 found
   -- Configuring done
   -- Generating done
   -- Build files have been written to: Hello/build
   ```

6. Now it’s time to build your application. For that step run `make` inside the folder `Hello/build`:

   ```sh
   make
   ```

   . If the last step completed successfully, then the build directory now contains the application `hello`.

7. You can now run your Elektra application by calling `./hello` inside the build directory. The output of the application should look something like this:

   ```
   Open key database
   Retrieve key set
   Number of key-value pairs: 0
   Add key user:/test/hello
   Number of key-value pairs: 1

   hello, elektra

   Delete key-value pairs inside memory
   Close key database
   ```

8. You can now change the content of `helloElektra.c`. If you want to compile and execute the updated code, then repeat steps 6 and 7.
