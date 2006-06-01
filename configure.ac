#
# Autoconf script for Elektra
# 
# Yannick Lecaillez <yl@itioweb.com>
#
# $Id$

AC_PREREQ(2.59)
AC_REVISION($Revision$)
AC_INIT(elektra, 0.6.1, http://bugs.libelektra.org)
AC_CANONICAL_SYSTEM
AC_LIBTOOL_WIN32_DLL
AC_CONFIG_SRCDIR(src/libelektra/kdb.c)
AM_INIT_AUTOMAKE
AC_CONFIG_HEADER([src/include/config.h])

# Debuging version ?
# CFLAGS="${CFLAGS=}"
dnl this macro is used to get the arguments supplied
dnl to the configure script (./configure --enable-debug)
dnl Check if we have enabled debug support.
AC_MSG_CHECKING(whether to enable debugging)
AC_ARG_ENABLE(debug, [  --enable-debug turns on debugging
	      [default="no"]],enable_debug="yes", enable_debug="no")
dnl Yes, shell scripts can be used
if test "x$enable_debug" = "xyes"; then
	CFLAGS="-g -ggdb -DDEBUG -O0"
	AC_MSG_RESULT(yes)
else
	AC_MSG_RESULT(no)
fi

# set some custom path 

# usr/lib path
echo "LIBDIR=$libdir"
AC_ARG_WITH(ulibdir,
	    [AC_HELP_STRING(--with-ulibdir=<path to lib>,
	     [Set the path for usr lib.])],
	     [[ulibdir=$withval]],
	     [[ulibdir='${prefix}/'`basename $libdir`]]
	     )
AC_SUBST(ulibdir)

# backend dlopended libraries path
AC_ARG_WITH(backenddir,
	    [AC_HELP_STRING(--with-backenddir=<path where backend libraries are>,
	     [Set the path for backend libraries.])],
	     [[backenddir=$withval]],
	     [[backenddir='${libdir}/elektra']]
	     )
AC_SUBST(backenddir)

# /usr/share/doc path
AC_ARG_WITH(docdir,
	    [AC_HELP_STRING(--with-docdir=<path where doc will be installed>,
	     [Set the path for documentation.])],
	     [[docdir=$withval]],
	     [[docdir='${datadir}/doc/elektra']]
	     )
AC_SUBST(docdir)

# /usr/share/doc/elektra-api
AC_ARG_WITH(develdocdir,
	    [AC_HELP_STRING(--with-develdocdir=<path where elektra-api doc will be installed>,
	     [Set the path for elektra api documentation.])],
	     [[develdocdir=$withval]],
	     [[develdocdir='${datadir}/doc/elektra-devel']]
	     )
AC_SUBST(develdocdir)

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

# Enable compilation of experimental things (unfinished
# backends at this time)
AC_ARG_ENABLE(experimental,
	      [AC_HELP_STRING(--enable-experimental, [Specify to compile or not experimental parts (could cause compilation failure)])],
	      [experimental=no])
AM_CONDITIONAL(EXPERIMENTAL, test x$experimental = xyes)

# Select docbook.xsl
AC_ARG_WITH(docbook,
	    [AC_HELP_STRING(--with-docbook=<path to docbook.xsl>,
	     [Set path to docbook.xsl used for generate manpage.])],
	     [[default_docbook=$withval]],
	     [[default_docbook=/usr/share/sgml/docbook/xsl-stylesheets/]]
	     )
AC_SUBST(default_docbook)


# Be typo friendly, check if this backend really exist 
# This test is needed because we can't check for existance 
# of files when cross-compiling and hence just assume they exist.
# Note that this test could probably also be used in the docbook checking part.
if test "x$cross_compiling" != "xyes"; then
AC_CHECK_FILE([src/backends/$default_backend/$default_backend.c],
	      AC_SUBST(default_backend),
	      AC_MSG_ERROR([Can't find backend $default_backend]))
fi

#Check whether we are compiling for win32 
case $host_os in
mingw* | pw32*)
  # If compiling for windows we need HAVE_WIN32 defined
	AC_DEFINE(HAVE_WIN32, 1, [Define if you are compiling for win32])	
esac

# Disable libtool static lib
AC_DISABLE_STATIC

#
# Checks for needed programs.
#
AC_PROG_CC
AC_PROG_LN_S

# Don't use libltdl stuff for WIN32
case $host_os in
	pw32* | mingw*)
		;;
	*)
		AC_LIBLTDL_CONVENIENCE
		dnl Substitute LTDLINCL and LIBLTDL in the Makefiles
		AC_SUBST(LTDLINCL)
		AC_SUBST(LIBLTDL)
		AC_LIBTOOL_DLOPEN
		AC_CONFIG_SUBDIRS(libltdl)
		;;
esac
PKG_PROG_PKG_CONFIG
AC_PROG_LIBTOOL

# Check needed programs for generate doc

# xsltproc
AC_PATH_PROG(xsltproc, xsltproc, "no")
# Check if have style-sheet too
if test "x$default_docbook" != "xno"; then
	AC_CHECK_FILE([$default_docbook],
		      [[dbroot=$default_docbook]],
		      [[dbroot="no"]])
else
	dbroot="no"
fi

AC_SUBST(dbroot)
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

# Check for iconv
# If iconv usage is disabled there is no need to check for iconv.
# This also avoids linking against libiconv, 
# if the system requires this for a working iconv function.
dnl Check if the user wants to disable iconv support and 
dnl hence disable any charset conversions.
dnl It is disabled with --disable-iconv
AC_MSG_CHECKING(whether to disable iconv usage)
AC_ARG_ENABLE(iconv, [  --disable-iconv turns off automatic UTF-8 conversions
        [default="no"]],disable_iconv="yes", disable_iconv="no")
if test "x$disable_iconv" = "xyes"; then
	AC_MSG_RESULT(yes)
else
	AC_MSG_RESULT(no)
	AM_ICONV()
fi

# Check for libxml (Allow compilation of libelektratools)
AM_PATH_XML2(, [have_xml="yes"], [have_xml="no"])
AM_CONDITIONAL(HAVE_XML, [test x$have_xml = xyes])

# Check for libdb (Allow compilation of libelektra-berkeley.so)
AC_CHECK_LIB(db, db_create, [have_db="yes"], [have_db="no"])
AM_CONDITIONAL(HAVE_DB, [test x$have_db = xyes])

# Check for libGconf (Allow compilation of libelektra-gconf.so)
PKG_CHECK_MODULES(gconf, gconf-2.0, [have_gconf="yes"], [have_gconf="no"])
AM_CONDITIONAL(HAVE_GCONF, [test x$have_gconf = xyes])

#
# Checks for header files.
#
AC_HEADER_STDC
AC_CHECK_HEADERS([langinfo.h locale.h mntent.h stdlib.h string.h strings.h unistd.h regex.h pwd.h grp.h])

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
#AC_FUNC_MALLOC
#AC_FUNC_MEMCMP
#AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset nl_langinfo setenv setlocale strcasecmp strchr strrchr index rindex strtol ctime_r getuid getgid])


#
# Output
#
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([elektra.spec])
AC_OUTPUT([src/Makefile

	   src/libloader/Makefile

	   src/libelektra/Makefile

	   src/libelektratools/Makefile

	   src/libregistry/Makefile

	   src/kdb/Makefile

	   src/backends/Makefile
	   src/backends/berkeleydb/Makefile
	   src/backends/filesys/Makefile
	   src/backends/fstab/Makefile
	   src/backends/gconf/Makefile
	   src/backends/template/Makefile
	   src/backends/ini/Makefile
	   src/backends/daemon/Makefile

	   src/bindings/Makefile
	   src/bindings/cpp/Makefile

	   src/include/Makefile

	   examples/Makefile

	   doc/Makefile
	   doc/images/Makefile
	   doc/standards/Makefile

	   scripts/Makefile

	   xmldtd/Makefile
	   xmlschema/Makefile

	   elektra.pc
	   elektratools.pc
	   ])

