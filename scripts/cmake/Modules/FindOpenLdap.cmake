# - Find OpenLDAP C Libraries
#
# OPENLDAP_ROOT_DIR - path to the OpenLDAP installation prefix
# OPENLDAP_FOUND - True if found. OPENLDAP_INCLUDE_DIRS - Path to the openldap include directory OPENLDAP_LIBRARIES - Paths to the ldap and
# lber libraries

# Source: https://fossies.org/linux/ceph/cmake/modules/FindOpenLdap.cmake
# Source: https://github.com/ClickHouse/ClickHouse/blob/17a245aa4de9f0028966e5c61578a3aae0716a33/cmake/Modules/FindOpenLDAP.cmake

if(OPENLDAP_ROOT_DIR)
	find_path (OPENLDAP_INCLUDE_DIRS NAMES "ldap.h" "lber.h" PATHS "${OPENLDAP_ROOT_DIR}" PATH_SUFFIXES "include" NO_DEFAULT_PATH)
	find_library (OPENLDAP_LDAP_LIBRARY NAMES "ldap" PATHS "${OPENLDAP_ROOT_DIR}" PATH_SUFFIXES "lib" NO_DEFAULT_PATH)
	find_library (OPENLDAP_LBER_LIBRARY NAMES "lber" PATHS "${OPENLDAP_ROOT_DIR}" PATH_SUFFIXES "lib" NO_DEFAULT_PATH)
else()
	find_path (OPENLDAP_INCLUDE_DIRS NAMES ldap.h lber.h PATHS /usr/include /opt/local/include /usr/local/include)
	find_library (OPENLDAP_LDAP_LIBRARY NAMES "ldap")
	find_library (OPENLDAP_LBER_LIBRARY NAMES "lber")
endif()

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (OpenLdap DEFAULT_MSG OPENLDAP_INCLUDE_DIRS OPENLDAP_LDAP_LIBRARY OPENLDAP_LBER_LIBRARY)

set (OPENLDAP_LIBRARIES ${OPENLDAP_LDAP_LIBRARY} ${OPENLDAP_LBER_LIBRARY})

mark_as_advanced (OPENLDAP_INCLUDE_DIRS OPENLDAP_LDAP_LIBRARY OPENLDAP_LBER_LIBRARY)
