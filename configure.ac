#
# Autoconf script for Elektra
# 
# Yannick Lecaillez <sizon5@gmail.com>
#
# $Id$

AC_PREREQ(2.59)
AC_REVISION($Revision$)
AC_INIT(elektra, 0.6.3, http://bugs.libelektra.org)
AC_CANONICAL_SYSTEM
AC_LIBTOOL_WIN32_DLL
AC_CONFIG_SRCDIR(src/libelektra/kdb.c)
AM_INIT_AUTOMAKE
AC_CONFIG_HEADER([src/include/config.h])

elektra_empty_CFLAGS=no
if test "x$CFLAGS" = "x"; then
	AC_GNU_SOURCE
	elektra_empty_CFLAGS=yes
fi
AC_MSG_NOTICE([Empty CFLAGS: $elektra_empty_CFLAGS])

dnl select POSIX extensions, usefull on solaris, for example
gl_USE_SYSTEM_EXTENSIONS

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

# high level backend dlopended libraries path
AC_ARG_WITH(hlvlbackenddir,
	    [AC_HELP_STRING(--with-hlvl-backenddir=<path where high level backend libraries are>,
	     [Set the path for high level backend libraries.])],
	     [[hlvlbackenddir=$withval]],
	     [[hlvlbackenddir='${ulibdir}/elektra']]
	     )
AC_SUBST(hlvlbackenddir)

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

# Default daemon backend selection (Set to 'berkeleydb' as default)
AC_ARG_WITH(dbackend,
	    [AC_HELP_STRING(--with-default-dbackend=<daemon backend>,
	     [Set the default backend for the kdbd daemon to use.])],
	     [[default_dbackend=$withval]],
	     [[default_dbackend=berkeleydb]]
	     )
AC_SUBST(default_dbackend)

# Enable compilation of experimental things (unfinished
# backends at this time)
AC_ARG_ENABLE(experimental,
	      [AC_HELP_STRING(--enable-experimental, [Specify to compile or not experimental parts (could cause compilation failure)])],
	      [elektra_experimental=$enableval],
	      [elektra_experimental=no])
# for the c++ binding
AM_CONDITIONAL(EXPERIMENTAL, [test x$elektra_experimental = xyes])

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
dnl if test "x$cross_compiling" != "xyes"; then
dnl AC_CHECK_FILE([src/backends/$default_backend/$default_backend.c],
dnl 	      AC_SUBST(default_backend),
dnl 	      AC_MSG_ERROR([Can't find backend $default_backend]))
dnl fi
AC_SUBST(default_backend)

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
AC_CHECK_PROG(NM, nm, nm, nm-is-missing)

# add -Wall if compiler is gcc and the CFLAGS variable is empty
if test $elektra_empty_CFLAGS = yes -a "x$CC" = xgcc; then
	CFLAGS="$CFLAGS -Wall -std=c99 -pedantic"
fi

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
AM_CONDITIONAL(HAVE_XSL, test x$xsltproc != xno -a x$dbroot != xno)

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

backends="filesys"
privatelibs=""

# Check for libxml (Allow compilation of libelektratools)
AM_PATH_XML2(, [elektra_have_xml="yes"], [elektra_have_xml="no"])
dnl AM_CONDITIONAL(HAVE_XML, [test x$elektra_have_xml = xyes])
if test "x$elektra_have_xml" = "xyes"; then
	elektratools=libelektratools
	privatelibs=$privatelibs" -lxml2"
fi

AC_SUBST(elektratools)

AC_CHECK_TYPE([u_int], [],
[
	AC_DEFINE(u_int, [unsigned int], [If system does not define u_int, define a reasonable substitute.])
])

AC_CHECK_TYPE([u_long], [],
[
	AC_DEFINE(u_long, [unsigned long], [If system does not define u_long, define a reasonable substitute.])
])

elektra_have_db=no
# Check for libdb (Allow compilation of libelektra-berkeley.so)
AC_CHECK_HEADERS([db.h],[
  AC_CHECK_LIB([db], [db_create], [elektra_have_db="yes"])
])
dnl AM_CONDITIONAL(HAVE_DB, [test x$elektra_have_db = xyes])
if test "x$elektra_have_db" = "xyes"; then
	backends=$backends" berkeleydb"
	privatelibs=$privatelibs" -ldb"
fi


# Check for libGconf (Allow compilation of libelektra-gconf.so)
PKG_CHECK_MODULES(gconf, gconf-2.0, [elektra_have_gconf="yes"], [elektra_have_gconf="no"])
dnl AM_CONDITIONAL(HAVE_GCONF, [test x$elektra_have_gconf = xyes])
if test "x$elektra_have_gconf" = "xyes" -a "x$elektra_experimental" = 'xyes'; 
then
	backends=$backends" gconf"
	privatelibs=$privatelibs" -lgconf-2"
fi

if test "x$elektra_experimental" = 'xyes'; then
	backends=$backends" ini"
fi

AC_SUBST(privatelibs)

#
# Checks for header files.
#
AC_HEADER_STDC
AC_CHECK_HEADERS([langinfo.h locale.h stdlib.h string.h strings.h unistd.h regex.h pwd.h grp.h])

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
dnl not used
   [ AC_DEFINE(GETMNTENT_GLIBC, 1, 
     [Define this if mntent follows glibc conventions])
   elektra_glibc_mntent=yes],
   []
)])


AC_MSG_CHECKING([if mntent follows the glibc conventions])
if test x$elektra_glibc_mntent = xyes; then
   backends=$backends" fstab"
   AC_MSG_RESULT(yes)
else
   AC_MSG_RESULT(no)
fi
   
AM_CONDITIONAL(HAVE_GLIBC_MNTENT, [test x$elektra_glibc_mntent = xyes])

# daemon depends on struct ucred in ipc.c
elektra_ucred=no
AC_CHECK_MEMBERS([struct ucred.pid],
  [
    elektra_ucred=yes
    backends=$backends" daemon"
  ],
  [],
  [#include <sys/socket.h>])
dnl AM_CONDITIONAL(HAVE_UCRED, [test x$elektra_ucred = xyes])

backend_static_libs=
for elektra_tmp_backend in $backends; do
	backend_static_libs="$backend_static_libs ../backends/$elektra_tmp_backend/libelektra-${elektra_tmp_backend}.a"
done

AC_SUBST(backends)
AC_SUBST(backend_static_libs)

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
AC_TYPE_MODE_T
AC_TYPE_SIZE_T

# taken from ircd
AC_CHECK_TYPE([u_int32_t], [],
[
	AC_CHECK_TYPE([uint32_t],
	[
		AC_DEFINE(u_int32_t, [uint32_t], [If system does not define u_int32_t, define a reasonable substitute.])
	],
	[
		AC_MSG_WARN([system has no u_int32_t or uint32_t, default to unsigned long int])
		AC_DEFINE(u_int32_t, [unsigned long int], [If system does not define u_int32_t, define to unsigned long int here.])
	])
])

AC_CHECK_TYPE([u_int8_t], [],
[
	AC_CHECK_TYPE([uint8_t],
	[
		AC_DEFINE(u_int8_t, [uint8_t], [If system does not define u_int8_t, define a reasonable substitute.])
	],
	[
		AC_MSG_WARN([system has no u_int8_t or uint8_t, default to unsigned char])
		AC_DEFINE(u_int8_t, [unsigned char], [If system does not define u_int8_t, define to unsigned char here.])
	])
])

AC_CHECK_TYPE(ssize_t, [], 
[
	AC_DEFINE(ssize_t, int, [Type of sizes or error indications.])
])

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
[
AC_MSG_RESULT(no)
])

#
# Checks for library functions.
#
#AC_FUNC_MALLOC
#AC_FUNC_MEMCMP
#AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset nl_langinfo setenv setlocale strcasecmp strchr strrchr index rindex strtol ctime_r getuid getgid sigprocmask sigaction])



INSTALL='\$(install_sh)'
AC_SUBST(INSTALL)

#
# Output
#
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([elektra.spec])
AC_OUTPUT([src/Makefile

	   src/libloader/Makefile

	   src/libelektra/Makefile
	   src/libelektra/exportobjects.sh

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

	   tests/Makefile
	   ])

echo "--------------------------------"
echo " libelektra                     "
echo "--------------------------------"
echo ""
echo "Compile backends: $backends"
echo -n "Compile libelektratools: "
if test x$elektra_have_xml = "xyes"; then
	echo "yes"
else
	echo "no"
fi
echo ""
echo "type: make && make install"

