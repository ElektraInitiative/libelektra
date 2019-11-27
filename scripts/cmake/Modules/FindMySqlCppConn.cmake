# ~~~
# Find the libmysqlcppconn includes and library
#
#  MYSQLCPPCONN_INCLUDE_DIR  - Where to find libmysqlcppconn include sub-directory.
#  MYSQLCPPCONN_LIBRARY      - Path to libmysqlcppconn library.
#  MYSQLCPPCONN_FOUND        - True if libmysqlcppconn found.
# ~~~

if (MYSQLCPPCONN_INCLUDE_DIR) # Already in cache, be silent.
	set (MYSQLCPPCONN_FIND_QUIETLY TRUE)
endif (MYSQLCPPCONN_INCLUDE_DIR)

find_path (MYSQLCPPCONN_INCLUDE_DIR cppconn/driver.h PATHS /usr/include /usr/local/include)
find_library (
	MYSQLCPPCONN_LIBRARY
	NAMES mysqlcppconn
	PATHS /usr/lib /usr/lib64 /usr/local/lib)

# Handle the QUIETLY and REQUIRED arguments and set MYSQLCPPCONN_FOUND to TRUE if all listed variables are TRUE.
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (MySqlCppConn DEFAULT_MSG MYSQLCPPCONN_LIBRARY MYSQLCPPCONN_INCLUDE_DIR)

mark_as_advanced (MYSQLCPPCONN_LIBRARY MYSQLCPPCONN_INCLUDE_DIR)
