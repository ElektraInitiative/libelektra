#
# Autoconf script for Elektra
#
# Markus Raab <elektra@markus-raab.org>
# Yannick Lecaillez <sizon5@gmail.com>
#

AC_PREREQ(2.59)
AC_REVISION([$Revision$])

m4_define(VERSION_MAJOR, [0])
m4_define(VERSION_MINOR, [7])
m4_define(VERSION_MICRO, [0])

AC_INIT(elektra, VERSION_MAJOR.VERSION_MINOR.VERSION_MICRO, http://bugs.libelektra.org)

AC_SUBST(KDB_VERSION, "VERSION_MAJOR.VERSION_MINOR.VERSION_MICRO")
AC_SUBST(KDB_VERSION_MAJORMINOR, "VERSION_MAJOR.VERSION_MINOR")
AC_SUBST(KDB_VERSION_MAJOR, "VERSION_MAJOR")
AC_SUBST(KDB_VERSION_MINOR, "VERSION_MINOR")
AC_SUBST(KDB_VERSION_MICRO, "VERSION_MICRO")
AC_SUBST(PACKAGE_URL, [http://www.libelektra.org/])

AC_SUBST(ELEKTRA_VERSION_API, [3:0:0])
AC_SUBST(ELEKTRATOOLS_VERSION_API, [2:0:0])
AC_SUBST(BACKEND_VERSION_API, [0:0:0])

AM_MAINTAINER_MODE




#
# Os Specific checks
#

# get host_os variable
AC_CANONICAL_SYSTEM
AC_CANONICAL_TARGET

dnl workaround an incompatible install on solaris
INSTALL='\$(install_sh)'
AC_SUBST(INSTALL)

AC_CONFIG_SRCDIR([src/libelektra/kdb.c])
AM_INIT_AUTOMAKE([1.9])
AC_CONFIG_HEADER([src/include/config.h])

dnl select POSIX extensions, useful on solaris, for example
AC_USE_SYSTEM_EXTENSIONS

#Check whether we are compiling for win32
case $host_os in
mingw* | pw32*)
  # If compiling for windows we need HAVE_WIN32 defined
	AC_DEFINE(HAVE_WIN32, 1, [Define if you are compiling for win32])
esac

# Disable libtool static lib
#AC_DISABLE_STATIC

AM_CONDITIONAL([ENABLE_STATIC],[test x$enable_static = xyes])











#
# Compile and test specific flags.
#

dnl this macro is used to get the arguments supplied
dnl to the configure script (./configure --enable-debug)
dnl Check if we have enabled debug support.
AC_MSG_CHECKING(whether to enable debugging and take control over compiler flags)
AC_ARG_ENABLE(debug, [AC_HELP_STRING([--enable-debug],
		[turn on debug symbols and enable debug messages @<:@default=no@:>@])])

if test "x$enable_debug" = "xyes";
then
	AC_MSG_RESULT(yes)

	dnl remove the optimization flag
	dnl gcc has -g -O2 as default (if none given)
	CFLAGS=`echo "$CFLAGS" | sed -e 's/-O[0-9]*[ ]*//g'`
	CXXFLAGS=`echo "$CFLAGS" | sed -e 's/-O[0-9]*[ ]*//g'`

	CFLAGS=`echo "$CFLAGS" | sed -e 's/-g[ ]*//g'`
	CXXFLAGS=`echo "$CFLAGS" | sed -e 's/-g[ ]*//g'`

	dnl STD flags will only be included where no POSIX is required
	CSTDFLAGS="-std=c99 -ansi -pedantic -Wall -Wsign-compare -Wfloat-equal -Wformat-security"
	COPTFLAGS="-O0"
	CDBGFLAGS="-g -ggdb"

	CXXSTDFLAGS="-std=c++98 -ansi -pedantic -Wall -Wsign-compare -Wfloat-equal -Wformat-security"
	CXXOPTFLAGS="-O0"
	CXXDBGFLAGS="-g -ggdb"

	AC_DEFINE([DEBUG], 1,[enables debug output messages])
	AC_DEFINE([VERBOSE], 0,[change here to be very verbose])
else
	AC_MSG_RESULT(no)

	AC_DEFINE([DEBUG], 0,[disable debug output messages])
	AC_DEFINE([VERBOSE], 0,[disable verbose output messages])
fi
#take control over FLAGS
#AM_CONDITIONAL (USE_FLAGS, [test "x$enable_debug" = "xyes" ])
AM_CONDITIONAL(DEBUG, test x$enable_debug = xyes)


# Enable compilation of experimental things (unfinished
# backends at this time)
AC_ARG_ENABLE(experimental,
	      [AC_HELP_STRING([--enable-experimental],
	      [Specify to compile or not experimental parts (could cause compilation failure) @<:@default=no@:>@])],
	      [elektra_experimental=$enableval],
	      [elektra_experimental=no])
# for the c++ binding
AM_CONDITIONAL(EXPERIMENTAL, [ test "x$elektra_experimental" = "xyes" ])


# Run self-tests under valgrind?
AC_MSG_CHECKING([whether self tests are run under valgrind])
AC_ARG_ENABLE(valgrind-tests,
	AS_HELP_STRING([--enable-valgrind-tests],
	[run self tests under valgrind @<:@default=no@:>@]))
if test "x$enable_valgrind_tests" = "xyes";
then
	AC_MSG_RESULT(yes)
	AC_CHECK_PROGS(VALGRIND, valgrind)
else
	AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(VALGRINDTESTS, [ test "x$VALGRIND" != "x" ])


dnl thanks to the wine project, author: Aaron Arvey
dnl Check for --enable-gcov and add appropriate flags for gcc
dnl Note that these extra switches are NOT applied to the loader
AC_MSG_CHECKING(whether to enable gcov)
AC_ARG_ENABLE(gcov, AC_HELP_STRING([--enable-gcov],
		[turn on code coverage analysis tools @<:@default=no@:>@]))
if test "x$enable_gcov" = "xyes";
then
	AC_MSG_RESULT(yes)
	AC_CHECK_PROGS(LCOV, lcov, false)
	AC_CHECK_PROGS(GENHTML, genhtml, false)

	CDBGFLAGS="$CDBGFLAGS -fprofile-arcs -ftest-coverage"
	CXXDBGFLAGS="$CXXDBGFLAGS -fprofile-arcs -ftest-coverage"

	LDFLAGS="$LDFLAGS -lgcov"

	dnl Turn off optimization so code coverage tool
	dnl can get accurate line numbers
	CFLAGS=`echo "$CFLAGS" | sed -e 's/-O[0-9]*[ ]*//g'`
	CXXFLAGS=`echo "$CFLAGS" | sed -e 's/-O[0-9]*[ ]*//g'`
	COPTFLAGS="-O0"
	CXXOPTFLAGS="-O0"
else
	AC_MSG_RESULT(no)
fi

AC_SUBST(CSTDFLAGS)
AC_SUBST(COPTFLAGS)
AC_SUBST(CDBGFLAGS)

AC_SUBST(CXXSTDFLAGS)
AC_SUBST(CXXOPTFLAGS)
AC_SUBST(CXXDBGFLAGS)









#
# set some custom path
#

# usr/lib path
AC_SUBST(libdir)
echo "LIBDIR=$libdir"

AC_ARG_WITH(ulibdir,
	    [AC_HELP_STRING([--with-ulibdir=ULIBDIR>],
	     [Set the path for usr lib.])],
	     [ulibdir=$withval],
	     [ulibdir='${prefix}/'`basename $libdir`]
	     )
AC_SUBST(ulibdir)

# backend dlopended libraries path
AC_ARG_WITH(backenddir,
	    [AC_HELP_STRING([--with-backenddir=<path where backend libraries are>],
	     [Set the path for backend libraries. @<:@LIBDIR/elektra@@:>@])],
	     [backenddir=$withval],
	     [backenddir='${libdir}/elektra']
	     )
AC_SUBST(backenddir)

# high level backend dlopended libraries path
AC_ARG_WITH(hlvlbackenddir,
	    [AC_HELP_STRING([--with-hlvl-backenddir=<path where high level backend libraries are>],
	     [Set the path for high level backend libraries. @<:@ULIBDIR/elektra@:>@])],
	     [hlvlbackenddir=$withval],
	     [hlvlbackenddir='${ulibdir}']
	     )
AC_SUBST(hlvlbackenddir)

# /usr/share/doc path
AC_ARG_WITH(docdir,
	    [AC_HELP_STRING([--with-docdir=<path where doc will be installed>],
	     [Set the path for documentation. @<:@DATADIR/doc/elektra@:>@])],
	     [docdir=$withval],
	     [docdir='${datadir}/doc/elektra']
	     )
AC_SUBST(docdir)

# /usr/share/doc/elektra-api
AC_ARG_WITH(develdocdir,
	    [AC_HELP_STRING([--with-develdocdir=<path where elektra-api doc will be installed>],
	     [Set the path for elektra api documentation. @<:@DATADIR/doc/elektra-devel@:>@])],
	     [develdocdir=$withval],
	     [develdocdir='${datadir}/doc/elektra-devel']
	     )
AC_SUBST(develdocdir)

# Select docbook.xsl
AC_ARG_WITH(docbook,
	    [AC_HELP_STRING([--with-docbook=<path to docbook.xsl>],
	     [Set path to docbook.xsl used for generate manpage. @<:@/usr/share/sgml/docbook/xsl-stylesheets@:>@])],
	     [default_docbook=$withval],
	     [default_docbook=/usr/share/sgml/docbook/xsl-stylesheets]
	     )
AC_SUBST(default_docbook)

# sgml/elektra-<VERSION>
AC_ARG_WITH(kdbschema,
	    [AC_HELP_STRING([--with-kdbschemadir=<relative path to kdb schema>],
	     [Set the path for elektra.xsd. DATADIR will be prefixed. @<:@/sgml/elektra-$PACKAGE_VERSION@:>@])],
	     [kdbschemadir=$withval],
	     [kdbschemadir="/sgml/elektra-$PACKAGE_VERSION"]
	     )
AC_DEFINE_UNQUOTED([KDB_SCHEMA_PATH], DATADIR "${kdbschemadir}", path for elektra.xsd)


# Default backend selection (Set to 'filesys' as default)
AC_ARG_WITH(backend,
	    [AC_HELP_STRING([--with-default-backend=<backend>],
	     [Set backend elektra will be linked to. @<:@filesys@:>@])],
	     [default_backend=$withval],
	     [default_backend=filesys]
	     )
AC_SUBST(default_backend)

# Default daemon backend selection (Set to 'berkeleydb' as default)
AC_ARG_WITH(dbackend,
	    [AC_HELP_STRING([--with-default-dbackend=<daemon backend>],
	     [Set the default backend for the kdbd daemon to use. @<:@berkeleydb@:>@])],
	     [default_dbackend=$withval],
	     [default_dbackend=berkeleydb]
	     )
AC_SUBST(default_dbackend)







#
# Checks for needed programs.
#

AC_PROG_CC
AM_PROG_CC_C_O

AC_PROG_CXX

AC_PROG_LN_S
AC_PROG_SED
AC_CHECK_PROG(NM, nm, nm, nm-is-missing)
AC_PROG_MAKE_SET

PKG_PROG_PKG_CONFIG

# Check needed programs for generate doc

# xsltproc
AC_PATH_PROG(xsltproc, xsltproc, "no")
# Check if have style-sheet too
if test "x$default_docbook" != "xno" && test "x$cross_compiling" = "xno"; then
	AC_CHECK_FILE([$default_docbook/manpages/docbook.xsl],
		      [dbroot=$default_docbook],
		      [dbroot="no"])
	if test "x$default_docbook" != "xno"; then
		AC_CHECK_FILE([$default_docbook/template/titlepage.xsl],
			      [dbroot=$default_docbook],
			      [dbroot="no"])
	fi
else
	dbroot="no"
fi

AC_SUBST(dbroot)
AM_CONDITIONAL(HAVE_XSL, test x$xsltproc != xno -a x$dbroot != xno)

# man2html
AC_PATH_PROG(man2html, man2html, "no")
AM_CONDITIONAL(HAVE_MAN2HTML, test x$man2html != xno)

# Doxygen
AC_PATH_PROG(doxygen, doxygen, "no")
AM_CONDITIONAL(HAVE_DOXYGEN, test x$doxygen != xno)







#
# Checks for header files.
#

AC_HEADER_STDC
AC_CHECK_HEADERS([ ctype.h stdlib.h unistd.h errno.h time.h stdio.h ])
AC_CHECK_HEADERS([ langinfo.h locale.h ])
AC_HEADER_TIME

#types
AC_CHECK_HEADERS([ inttypes.h limits.h ])
AC_HEADER_STDBOOL

#posix files and functions (needed by filesys and libhelper)
AC_HEADER_DIRENT
AC_CHECK_HEADERS([ fcntl.h ])






#
# libltld checks
#

#copied out from libsox

AC_ARG_WITH(libltdl,
    AC_HELP_STRING([--without-libltdl],
        [Don't try to use libltdl for external dynamic library support]))

using_libltdl=no
if test "$with_libltdl" != "no"; then
	dnl Disable libltdl support when building only static libraries
	if test "$enable_shared" != "no"; then
		using_libltdl=yes
	fi

	dnl Force off when building on w32, we have specific code for that
	dnl in libloader.
	case $host_os in
		pw32* | mingw*)
			using_libltdl=no
			;;
	esac

	dnl Force off using libltdl on targets that are know to have
	dnl problems.
	case $target in
		*cygwin*)
			using_libltdl=no
			;;
	esac
fi

if test "$using_libltdl" != "no"; then
  AC_LIBLTDL_INSTALLABLE
  AC_LIBTOOL_DLOPEN
fi
AC_PROG_LIBTOOL
AC_SUBST(LIBTOOL_DEPS)
AC_CONFIG_SUBDIRS(libltdl)
if test "$using_libltdl" != "no"; then
  AC_SUBST(LIBLTDL)
  AC_SUBST(LTDLINCL)
  AC_CHECK_HEADERS(ltdl.h, AC_DEFINE([HAVE_LIBLTDL], 1, [Define to 1 if you have libltdl]), [using_libltdl=no])
fi
AM_CONDITIONAL(HAVE_LIBLTDL, test x$using_libltdl = xyes)








#
# Checks for libraries.
#


# Check for iconv
# If iconv usage is enabled we need to check for iconv.
# This may also result in linking against libiconv,
# if the system requires this for a working iconv function.
dnl Check if the user wants to enable iconv support and
dnl hence enable charset conversions.
dnl Default: dont enable it
# AC_MSG_CHECKING(whether to enable iconv)
# AC_ARG_ENABLE(iconv,
# 		[AC_HELP_STRING([--enable-iconv],
# 		[turns on automatic UTF-8 conversions. @<:@default=no@:>@])])
#
# if test "x$enable_iconv" = "xyes"; then
# 	AC_MSG_RESULT(yes)
# 	AM_ICONV()
# else
# 	AC_MSG_RESULT(no)
# fi

privatelibs=""

# Check for libxml (Allow compilation of libelektratools)
REQUIRES_LIBXML2="libxml2 >= 2.6.0"
AC_SUBST(REQUIRES_LIBXML2)
PKG_CHECK_MODULES([libxml2],
      [${REQUIRES_LIBXML2}],
      [elektra_have_xml=yes],
      [elektra_have_xml=no]
)
AC_SUBST(libxml2_CFLAGS)
AC_SUBST(libxml2_LIBS)
AM_CONDITIONAL([HAVE_XML],[test "$elektra_have_xml" = "yes"])
if test x$elektra_have_xml = xyes; then
	elektratools=libelektratools
	privatelibs=$privatelibs "$libxml2_LIBS"
fi

AC_SUBST(elektratools)








#
# Backends enable and disable logic.
#

BACKENDS=""
TESTBACKENDS=""

# $1 is the backend name
# $2 is the check for prerequisite
# $3 is the default value YES or NO
AC_DEFUN([ALLOW_BACKEND],
[
	TESTBACKENDS="$TESTBACKENDS
	$1	check: $2,	default: $3"
	if test "$3" == YES; then
		BACKENDS="$BACKENDS $1";
	fi
	AC_ARG_ENABLE([$1],
		AC_HELP_STRING([--enable-$1], [compile $1 backend @<:@default=$3@:>@]),
		[if test "$enableval" = yes; then
			if $2; then
				BACKENDS="$BACKENDS $1";
			else
				#thus only default backends are in BACKENDS and
				#they don't have prerequisites, don't remove
				AC_MSG_WARN([not building backend $1, prerequisite missing])
			fi
		else
			#removed backend out of BACKENDS list
			BACKENDS=$(echo "$BACKENDS" | sed 's/[[:space:]]*$1//')
		fi
	])
])

ALLOW_BACKEND(filesys, true, YES)

ALLOW_BACKEND(hosts, true, YES)

ALLOW_BACKEND(ini, true, NO)

elektra_have_db=no
# Check for libdb (Allow compilation of libelektra-berkeley.so)
AC_CHECK_HEADERS([db.h],[
  AC_CHECK_LIB([db], [db_create], [elektra_have_db="yes"])
])
ALLOW_BACKEND(berkeleydb, [test x$elektra_have_db = xyes], NO)


# Check for libGconf (Allow compilation of libelektra-gconf.so)
PKG_CHECK_MODULES(gconf, gconf-2.0, [elektra_have_gconf="yes"], [elektra_have_gconf="no"])
ALLOW_BACKEND(gconf, [test x$elektra_have_gconf = xyes], NO)

elektra_glibc_mntent=no
AC_CHECK_HEADERS([mntent.h],[
   AC_TRY_LINK([#include <stdio.h>
#include <mntent.h> ],
[
struct mntent *m;
FILE *f;
char *fsname, *dir, *type, *opts;
int freq, passno;
f=setmntent("/etc/fstab", "r");
m=getmntent(f);
fsname=m->mnt_fsname;
dir=m->mnt_dir;
type=m->mnt_type;
opts=m->mnt_opts;
freq=m->mnt_freq;
passno=m->mnt_passno;
],
dnl DEFINE is not used in the code
   [ AC_DEFINE(GETMNTENT_GLIBC, 1, 
     [Define this if mntent follows glibc conventions])
   elektra_glibc_mntent=yes],
   []
)])


AC_MSG_CHECKING([if mntent follows the glibc conventions])
if test x$elektra_glibc_mntent = xyes; then
   AC_MSG_RESULT(yes)
else
   AC_MSG_RESULT(no)
fi
ALLOW_BACKEND(fstab, [test x$elektra_glibc_mntent = xyes], NO)

AC_CHECK_HEADERS([pwd.h],
   [ AC_DEFINE(HAVE_PWD_H, 1, 
     [Define this if pwd.h is present on your system])
   elektra_pwd_h=yes]
)
ALLOW_BACKEND(passwd, [test x$elektra_pwd_h = xyes], NO)

# daemon depends on struct ucred in ipc.c
elektra_ucred=no
AC_CHECK_MEMBERS([struct ucred.pid], [elektra_ucred=yes], [elektra_ucred=no],
                 [#include <sys/socket.h>])
ALLOW_BACKEND(daemon, [test x$elektra_ucred = xyes], NO)

AC_SUBST(BACKENDS)

if test x$enable_static = xyes; then
	backend_static_libs=
	for backend in $BACKENDS; do
		backend_static_libs="$backend_static_libs ../backends/${backend}/libelektra-${backend}.a"
	done
fi

AC_SUBST(privatelibs)
AC_SUBST(backend_static_libs)







#
# Bindings enable and disable logic.
#

BINDINGS=""

# $1 is the binding name
# $2 is the check for prerequisite
# $3 is the default value YES or NO
AC_DEFUN([ALLOW_BINDING],
[
	if test "$3" == YES; then
		BINDINGS="$BINDINGS $1";
	fi
	AC_ARG_ENABLE([$1],
		AC_HELP_STRING([--enable-$1], [compile $1 binding  @<:@default=$3@:>@]),
		[if test "$enableval" = yes; then
			if $2; then
				BINDINGS="$BINDINGS $1";
			else
				#thus only default bindings are in BINDINGS and
				#they don't have prerequisites, don't remove
				AC_MSG_WARN([not building binding $1, prerequisite missing])
			fi
		else
			#removed bindings out of BINDINGS list
			BINDINGS=$(echo "$BINDINGS" | sed 's/[[:space:]]*$1//')
		fi
	])
])

ALLOW_BINDING(cpp, true, YES)

#AC_CHECK_PROGS(SCHEME, scheme, false)
#ALLOW_BINDING(scheme, [test "x$SCHEME" = xscheme], NO)

AC_CHECK_PROGS(PYTHON, python, false)
ALLOW_BINDING(python, [test "x$PYTHON" = xpython], NO)

AC_SUBST(BINDINGS)

# inspired from daemontools
AC_CHECK_HEADERS([signal.h],[
   AC_TRY_LINK([#include <signal.h>],
[
sigset_t ss;
 
sigemptyset(&ss);
sigaddset(&ss,SIGCHLD);
sigprocmask(SIG_SETMASK,&ss,(sigset_t *) 0);
],
    [AC_DEFINE([HASSIGPROCMASK],[],[POSIX signal available])])
    AC_TRY_LINK([#include <signal.h>],
[
int sig;
struct sigaction sa;;
sa.sa_handler = f;
sa.sa_flags = 0;
sigemptyset(&sa.sa_mask);
sigaction(sig,&sa,(struct sigaction *) 0); 
],
    [AC_DEFINE([HASSIGACTION],[],[sigaction available])])
])








#
# Checks for typedefs, structures, and compiler characteristics.
#

AC_C_CONST
AC_TYPE_UID_T
AC_CHECK_TYPE(gid_t, int)
AC_CHECK_TYPE(time_t, unsigned long)
AC_TYPE_MODE_T

AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T

AC_MSG_CHECKING([For a working getopt])
AC_TRY_LINK([#include <unistd.h>],
[
int opt;
char** myargv;
opt=getopt(1,myargv,"a");
if (optind == 2) {opt = 3;};
],
[
  AC_DEFINE([HASGETOPT],[],[Working getopt])
  AC_MSG_RESULT(yes)
],
[AC_MSG_RESULT(no)])










#
# Checks for library functions.
#
#AC_FUNC_MALLOC
#AC_FUNC_MEMCMP
#AC_FUNC_REALLOC

AC_FUNC_CHOWN
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_GETMNTENT
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_FUNC_STAT

#old version
AC_CHECK_FUNCS([memset nl_langinfo setenv setlocale strcasecmp strchr strrchr index rindex strtol ctime_r getuid getgid sigprocmask sigaction])
#for testing suite
AC_CHECK_FUNCS([clearenv setenv])
#generated by autoscan, TODO. merge together
AC_CHECK_FUNCS([ftruncate getmntent gettimeofday memmove memset mkdir nl_langinfo putenv rmdir setenv setlocale socket strcasecmp strchr strndup strrchr strstr strtol])







#
# Output
#
AC_CONFIG_FILES([
	Makefile
	elektra.spec
	src/Makefile

	src/libhelper/Makefile
	src/libloader/Makefile

	src/libelektra/Makefile
	src/libelektra/exportobjects.sh

	src/libelektratools/Makefile

	src/preload/Makefile
	src/kdb/Makefile

	src/backends/Makefile
	src/backends/berkeleydb/Makefile
	src/backends/filesys/Makefile
	src/backends/fstab/Makefile
	src/backends/passwd/Makefile
	src/backends/hosts/Makefile
	src/backends/gconf/Makefile
	src/backends/template/Makefile
	src/backends/ini/Makefile
	src/backends/daemon/Makefile

	src/bindings/Makefile
	src/bindings/cpp/Makefile
	src/bindings/cpp/tests/Makefile
	src/bindings/python/Makefile

	src/include/Makefile

	doc/Makefile
	doc/images/Makefile
	doc/standards/Makefile

	scripts/Makefile

	xmlschema/Makefile

	elektra.pc
	elektratools.pc
	elektracpp.pc

	benchmarks/Makefile
	examples/Makefile
	tests/Makefile
	])
AC_OUTPUT

echo "--------------------------------"
echo " libelektra                     "
echo "--------------------------------"
echo ""
echo "Test backends: $TESTBACKENDS"
echo ""
echo "Compile backends: $BACKENDS"
echo "Compile bindings: $BINDINGS"
echo -n "Compile libelektratools: "
if test "x$elektra_have_xml" = "xyes"; then
	echo "yes"
else
	echo "no"
fi
echo ""
echo "type: make && make install"

