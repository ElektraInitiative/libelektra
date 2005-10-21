#
# Autoconf script for Elektra
# 
# Yannick Lecaillez <yl@itioweb.com>
#
# $Id$

AC_PREREQ(2.59)
AC_INIT(elektra, 0.5.1, http://bugs.libelektra.org)
AC_CONFIG_SRCDIR(src/libelektra/libkdb.c)
AM_INIT_AUTOMAKE
AC_CONFIG_HEADER([config.h])

# set some custom path 

# usr/lib path
AC_ARG_WITH(ulibdir,
	    [AC_HELP_STRING(--with-ulibdir=<path to lib>,
	     [Set the path for usr lib.])],
	     [[ulibdir=$withval]],
	     [[ulibdir='${prefix}/lib']]
	     )
AC_SUBST(ulibdir)

# sgml/elektra-0.1.1
AC_ARG_WITH(kdbschema,
	    [AC_HELP_STRING(--with-kdbschemadir=<relative path to kdb schema>,
	     [Set the path for elektra.xsd.])],
	     [[kdbschemadir=$withval]],
	     [[kdbschemadir='/sgml/elektra-0.1.1']]
	     )
AC_DEFINE_UNQUOTED([KDB_SCHEMA_PATH], DATADIR "${kdbschemadir}", path for elektra.xsd)


# Default backend selection (Set to 'filesys' as default)
AC_ARG_WITH(backend,
	    [AC_HELP_STRING(--with-default-backend=<backend>,
	     [Set backend elektra will be linked to.])],
	     [[default_backend=$withval]],
	     [[default_backend=filesys]]
	     )

# Select docbook.xsl
AC_ARG_WITH(docbook,
	    [AC_HELP_STRING(--with-docbook-xsl=<path to docbook.xsl>,
	     [Set path to docbook.xsl used for generate manpage.])],
	     [[default_docbook=$withval]],
	     [[default_docbook=/usr/share/sgml/docbook/xsl-stylesheets/]]
	     )
AC_SUBST(default_docbook)


# Be typo friendly, check if this backend really exist 
AC_CHECK_FILE([src/backends/$default_backend/$default_backend.c],
	      AC_SUBST(default_backend),
	      AC_MSG_ERROR([Can't find backend $default_backend]))

#
# ltdl Stuff
#
dnl building of the convenience library
dnl and set LIBLTDL accordingly
AC_LIBLTDL_CONVENIENCE
dnl Substitute LTDLINCL and LIBLTDL in the Makefiles
AC_SUBST(LTDLINCL)
AC_SUBST(LIBLTDL)
dnl Check for dlopen support
AC_LIBTOOL_DLOPEN

#
# Checks for needed programs.
#
AC_PROG_CC
AC_PROG_LN_S
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL
PKG_PROG_PKG_CONFIG
AC_CONFIG_SUBDIRS(libltdl)

# Check needed programs for generate doc

# xsltproc
AC_PATH_PROG(xsltproc, xsltproc, "no")
if test x$default_docbook != xno; then
	# xslproc was found, check for stylesheet
	AC_CHECK_FILE([$default_docbook],
		      [[dbroot=$default_docbook]],
		      [[dbroot="no"]])
	AC_SUBST(dbroot)
fi 
AM_CONDITIONAL(HAVE_XSLTPROC, test x$xsltproc != xno)
AM_CONDITIONAL(HAVE_STYLESHEET, test x$dbroot != xno)

# man2html
AC_PATH_PROG(man2html, man2html, "no")
AM_CONDITIONAL(HAVE_MAN2HTML, test x$man2html != xno)

# Doxygen
AC_PATH_PROG(doxygen, doxygen, "no")
AM_CONDITIONAL(HAVE_DOXYGEN, test x$doxygen != xno)

#
# Checks for libraries.
#

# Check for icon
AM_ICONV()

# Check for libxml (Allow compilation of libelektratools)
AM_PATH_XML2(, [have_xml="yes"], [have_xml="no"])
AM_CONDITIONAL(HAVE_XML, test x$have_xml = xyes)

# Check for libdb (Allow compilation of libelektra-berkeley.so)
AC_CHECK_LIB(db, db_create, [have_db="yes"], [have_db="no"])
AM_CONDITIONAL(HAVE_DB, test x$have_db = xyes)

# Check for libGconf (Allow compilation of libelektra-gconf.so)
PKG_CHECK_MODULES(gconf, gconf-2.0, [have_gconf="yes"], [have_gconf="no"])
AM_CONDITIONAL(HAVE_GCONF, test x$have_gconf = xyes)

#
# Checks for header files.
#
AC_HEADER_STDC
AC_CHECK_HEADERS([langinfo.h locale.h mntent.h stdlib.h string.h strings.h unistd.h])

#
# Checks for typedefs, structures, and compiler characteristics.
#
AC_C_CONST
AC_TYPE_UID_T
AC_TYPE_MODE_T
AC_TYPE_SIZE_T

#
# Checks for library functions.
#
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset nl_langinfo setenv setlocale strcasecmp strchr strrchr strtol])


#
# Output
#
AC_CONFIG_FILES([Makefile])
AC_OUTPUT([src/Makefile
	   
	   src/libelektra/Makefile
	   
	   src/libelektratools/Makefile
	   
	   src/libregistry/Makefile
	   
	   src/kdb/Makefile
	   
	   src/backends/Makefile
	   src/backends/berkeleydb/Makefile
	   src/backends/filesys/Makefile
	   src/backends/fstab/Makefile
	   src/backends/ini/Makefile
	   src/backends/gconf/Makefile
	   src/backends/template/Makefile

	   src/include/Makefile

	   example/Makefile

	   doc/Makefile
	   
	   scripts/Makefile

	   xmldtd/Makefile
	   xmlschema/Makefile
	   
	   elektra.pc

	   elektra.spec
	   ])

