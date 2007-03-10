Summary:       A key/value pair database to store software configurations
Name:          elektra
Version:       0.6.10
Release:       1%{?dist}
Group:         System Environment/Libraries
License:       BSD
URL:           http://www.libelektra.org
Source0:       http://downloads.sourceforge.net/%{name}/%{name}-%{version}.tar.gz
Patch0:        elektra-0.6.10-nostart.patch
BuildRoot:     %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
# doxygen: to build the API documentation
# libxslt, docbook-style-xsl: to build man pages from docbook with xsltproc program
# db4-devel: for the Berkeley DB backend
# libxml2-devel: for the kdbtools library build
BuildRequires: doxygen db4-devel libxml2-devel docbook-style-xsl libxslt
# gettext-devel: for the autoconf ICONV macro only

Requires(post): /sbin/ldconfig
Requires(post):  /sbin/chkconfig
Requires(preun): /sbin/chkconfig, /sbin/service


%description
Elektra provides a universal and secure framework to store configuration
parameters in a hierarchical key-value pair mechanism, instead of each
program using its own text configuration files. This allows any program
to read and save its configuration with a consistent API, and allows
them to be aware of other applications' configurations, permitting
easy application integration. While architecturally similar to other OS
registries, Elektra does not have most of the problems found in those
implementations.

This package also contains a Berkeley DB backend for Elektra, to let 
Elektra use Berkeley DB databases to store its keys and daemon which can
be used as a proxy for access to the keys.


%package devel
Summary:      Include files and API documentation to build elektrified programs
Group:        Development/System
Requires:     pkgconfig 
Requires:     %{name} = %{version}-%{release}

%description devel
Elektra provides a universal and secure framework to store configuration
parameters in a hierarchical key-value pair tree.

This package contains development specific things as include files to 
create elektrified programs.

%package static
Summary:      Elektra utility statically compiled
Group:        Applications/System
#Requires:     %{name} = %{version}-%{release}

%description static
This package contains the elektra utility compiled statically.


%package static-devel
Summary:      Elektra library statically compiled
Group:        Development/System
Requires:     %{name}-devel = %{version}-%{release}


%description static-devel
This package contains the elektra library compiled statically.


# GConf2-devel: for the testing GConf2 backend for Elektra

#%description backend-gconf

#This package contains a GConf backend for Elektra, to let Elektra use a GConf
#daemon to store its keys.


%prep
%setup -q
%patch0 -p1 -b .nostart

%build
%configure \
     --includedir=%{_includedir}/elektra \
     --program-prefix=%{name}- \
     --disable-rpath \
     --with-ulibdir=%{_libdir} \
     --with-docbook=%{_datadir}/sgml/docbook/xsl-stylesheets
make %{?_smp_mflags}

# Not used for now
# --disable-xmltest
# --with-hlvl-backenddir
# --with-kdbschemadir
# --with-default-backend
# --with-default-dbackend
# --with-docbook


%install
rm -rf $RPM_BUILD_ROOT

make DESTDIR=$RPM_BUILD_ROOT install

# Remove statically linked kdb - 
#rm $RPM_BUILD_ROOT%{_bindir}/elektra-kdb_static

# file in profile.d is sourced, remove shebang and execute bits
sed -i -e 's;#!/bin/sh;;' $RPM_BUILD_ROOT%{_sysconfdir}/profile.d/elektra-elektraenv.sh
chmod a-x $RPM_BUILD_ROOT%{_sysconfdir}/profile.d/elektra-elektraenv.sh

#Fix symlink should be relative - Be aware of SONAME changes!
ln -fs ../libelektratools.so.1  $RPM_BUILD_ROOT%{_libdir}/elektra/libelektratools.so

## doc fixes
rm -rf __doc __doc-devel
mkdir __doc
mv  $RPM_BUILD_ROOT%{_datadir}/doc/elektra/* __doc
rm -rf $RPM_BUILD_ROOT%{_datadir}/doc/elektra
mkdir __doc-devel
mv  $RPM_BUILD_ROOT%{_datadir}/doc/elektra-devel/* __doc-devel
rm -rf $RPM_BUILD_ROOT%{_datadir}/doc/elektra-devel

## man fixes (from program prefix)
mv $RPM_BUILD_ROOT%{_mandir}/man5/elektra-elektra.5 $RPM_BUILD_ROOT%{_mandir}/man5/elektra.5
mv $RPM_BUILD_ROOT%{_mandir}/man7/elektra-elektra.7 $RPM_BUILD_ROOT%{_mandir}/man7/elektra.7

mkdir -p $RPM_BUILD_ROOT%{_initrddir}
mv $RPM_BUILD_ROOT%{_sysconfdir}/init.d/elektra-kdbd $RPM_BUILD_ROOT%{_initrddir}
rmdir $RPM_BUILD_ROOT%{_sysconfdir}/init.d/

# for the socket and the pidfile
mkdir -p $RPM_BUILD_ROOT%{_localstatedir}/run/elektra-kdbd/

%clean
rm -rf $RPM_BUILD_ROOT


%post
/sbin/ldconfig
if [ "$1" -eq "1" ]; then
  # Create basic key structure for apps
  elektra-kdb set -t dir system/sw
  elektra-kdb set system/sw/kdb/schemapath "%{_datadir}/sgml/elektra-0.1.1/elektra.xsd"
fi

/sbin/chkconfig --add elektra-kdbd

%preun
if [ $1 = 0 ]; then
        /sbin/service elektra-kdbd stop >/dev/null 2>&1 || :
        /sbin/chkconfig --del elektra-kdbd
fi

%postun -p /sbin/ldconfig


%files
%defattr(-,root,root,-)
%doc AUTHORS COPYING ChangeLog README INSTALL
%doc __doc/*
%{_initrddir}/elektra-kdbd
%{_bindir}/elektra-kdb
%{_sbindir}/elektra-kdbd
%exclude %{_libdir}/elektra/*.la
%{_libdir}/elektra
%exclude %{_libdir}/libelektra*.la
%{_libdir}/libelektra*.so.*
%config %{_sysconfdir}/profile.d/*.sh
%{_mandir}/man1/*
%{_mandir}/man7/*
%{_mandir}/man5/*
%{_datadir}/sgml/
%dir %{_localstatedir}/run/elektra-kdbd/

%files devel
%defattr(-,root,root,-)
%doc __doc-devel/*
%{_includedir}/elektra
%{_libdir}/libelektra.so
%{_libdir}/libelektratools.so
%{_libdir}/pkgconfig/*
%{_mandir}/man3/*

%files static
%defattr(-,root,root,-)
%{_bindir}/elektra-kdb_static

%files static-devel
%defattr(-,root,root,-)
%{_libdir}/*.a


%changelog
* Sat Mar 10 2007 Patrice Dumas <pertusus@free.fr> 0.6.10-1
- update to 0.6.10
- use canonical scriptlets
- minor cleanups
- fix kdbd initscript

* Thu Feb 23 2007 kwizart < kwizart at gmail.com > - 0.6.8-1
- Update to 0.6.8
- Add init.d script elektra-kdbd change name to elektrad
- Add chckconfig in pre and preun
- Warning "segmentation error" on /usr/sbin/elektra-kdbd 
- TODO - PAM implementation if needed

* Thu Feb 22 2007 kwizart < kwizart at gmail.com > - 0.6.4-5
- Removed requires profile.d directory
- Fix rm __doc __doc-devel
- Recover static package with elektra-kdb_static
- Fix config file need to be replaced
- Exclude *.la from libdir/elektra and libdir
- Fix man names resulting from program prefix

* Wed Feb 21 2007 kwizart < kwizart at gmail.com > - 0.6.4-4
- Merge static package to "" package (needed for recovery).
- Revert Fix for sourced files in /etc/profile.d (config)
- Fix __doc files same as __doc-devel (missed)
- Fix include path in elektra sub directory at configure step
- Fix post step according to name changes

* Tue Feb 20 2007 kwizart < kwizart at gmail.com > - 0.6.4-3
- Fix ldconfig postun in pre
- Fix configure in /bin /sbin to be /usr/bin /usr/sbin
  we can revert back from this point if needed
- Don't remove profile.d requires anymore
- Let make install doc in place en then correct if needed
- Create static-devel package

* Sun Oct 15 2006 Patrice Dumas <pertusus@free.fr> 0.6.4-2
- split a subpackage for static libraries

* Sat Oct  7 2006 Patrice Dumas <pertusus@free.fr> 0.6.4-1
- Update to 0.6.4, cleanups

* Thu Jun 02 2006 Avi Alkalay <avi@unix.sh> 0.6.3-1
- Initial packaging

