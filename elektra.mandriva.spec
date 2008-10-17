%define name    elektra 
%define version 0.6.3
%define release %mkrel 1 

Name:          %{name}
Version:       %{version}
Release:       %{release}
Source:        http://aleron.dl.sourceforge.net/sourceforge/elektra/%{name}-%{version}.tar.bz2
Group:         System Environment/Libraries
License:       BSD
URL:           http://elektra.sourceforge.net
BuildRoot:     %{_tmppath}/%{name}-%{version}-%{release}-buildroot
BuildRequires: doxygen docbook-style-xsl db4-devel libGConf1-devel libxml2-devel libtool libxslt-proc
Summary:       A key/value pair database to store software configurations

%define DTDVERSION 0.1.1

%description
The Elektra Project provides a framework to store generic configuration data
in an hierarchical key-value pair database, instead of a human-readable only
text file.

This way any software can read/save his configuration using a consistent API.
Also, applications can be aware of other applications configurations,
leveraging easy application integration.


%package devel
Summary:      Include files and API documentation for Elektra Project
Group:        Development/System
Requires:     elektra = %{version}-%{release}

%description devel
The Elektra Project provides a framework to store generic configuration data
in an hierarchical key-value pair database, instead of a human-readable only
text file.

This way any software can read/save his configuration using a consistent API.
Also, applications can be aware of other applications configurations,
leveraging easy application integration.

This package contains the include files and API manual pages to use the Elektra
API in C.

It also provides the framework to create storage backends to libelektra.so


%package backend-gconf
Summary:      A GConf backend for Elektra
Group:        System Environment/Libraries
Requires:     elektra

%description backend-gconf
The Elektra Project provides a framework to store generic configuration data
in an hierarchical key-value pair database, instead of a human-readable only
text file.

This way any software can read/save his configuration using a consistent API.
Also, applications can be aware of other applications configurations,
leveraging easy application integration.

This package contains a GConf backend for Elektra, to let Elektra use a GConf
daemon to store its keys.

%package backend-berkeleydb
Summary:      Include files and API documentation for Elektra Project
Group:        System Environment/Libraries
Requires:     elektra


%description backend-berkeleydb
The Elektra Project provides a framework to store generic configuration data
in an hierarchical key-value pair database, instead of a human-readable only
text file.

This way any software can read/save his configuration using a consistent API.
Also, applications can be aware of other applications configurations,
leveraging easy application integration.

This package contains a Berkeley DB backend for Elektra, to let Elektra use
Berkeley DB databases to store its keys.

%debug_package

%prep
%setup

%build
%configure \
     --bindir=/bin \
     --sbindir=/sbin \
     --libdir=/%{_lib} \
     --with-docdir=%{_defaultdocdir}/elektra-%{version} \
     --with-develdocdir=%{_defaultdocdir}/elektra-devel-%{version} \
     --disable-xmltest \
     --with-docbook=%{_datadir}/sgml/docbook/xsl-stylesheets
%make

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install
# Move .a files to -devel package
mv $RPM_BUILD_ROOT/%{_lib}/libelektra.a $RPM_BUILD_ROOT/%{_libdir}

# Prepare devel files
rm $RPM_BUILD_ROOT/%{_lib}/libelektra.so
ln -sf ../../%{_lib}/libelektra.so.2 $RPM_BUILD_ROOT/%{_libdir}/libelektra.so

# Remove old .la files
#rm $RPM_BUILD_ROOT/usr/lib/libelektra-*.a
rm $RPM_BUILD_ROOT/%{_lib}/*.la
rm $RPM_BUILD_ROOT/%{_lib}/elektra/*.la
rm $RPM_BUILD_ROOT/%{_libdir}/*.la

# Remove a file that conflicts with other packages
rm $RPM_BUILD_ROOT/%{_mandir}/man3/key.3*

# Remove documentation from 'make install', to let RPM package it allone
rm -rf $RPM_BUILD_ROOT/%{_defaultdocdir}
rm -rf scripts/Makefile*
rm -rf examples/Makefile*
rm -rf examples/.deps
rm -rf doc/standards/Makefile*
mv doc/elektra-api/html doc/elektra-api/api-html

%clean
rm -rf $RPM_BUILD_ROOT

%post
/sbin/ldconfig
# Backwards compatibility, from the Linux Registry days
# if [ -d /etc/registry -a ! -d /etc/kdb ]; then
#	mv /etc/registry /etc/kdb
#	ln -s kdb /etc/registry
# fi

# Create basic key structure for apps
kdb set -t dir system/sw
kdb set system/sw/kdb/current/schemapath "%{_datadir}/sgml/elektra-0.1.1/elektra.xsd"


%postun -p /sbin/ldconfig



%files
%defattr(-,root,root,-)
/bin/*
/sbin/*
/%{_lib}/*elektra.so*
%dir /%{_lib}/elektra
/%{_lib}/elektra/*elektra-daemon.so*
/%{_lib}/elektra/*elektra-filesys.so*
/%{_lib}/elektra/*elektra-default.so*
/%{_lib}/elektra/*elektra-ddefault.so*
/%{_lib}/elektra/*elektra-fstab.so*
#/%{_lib}/elektra/*elektra-ini.so*
%{_libdir}/*elektratools.so.*
%{_libdir}/elektra/*elektratools.so
%{_sysconfdir}/profile.d/*
%doc AUTHORS COPYING ChangeLog README INSTALL
%doc scripts doc/standards
%{_mandir}/man1/*
%{_mandir}/man7/*
%{_mandir}/man5/*
%{_datadir}/sgml/*

%files devel
%defattr(-,root,root,-)
%{_includedir}/*
%{_libdir}/*.a
%{_libdir}/libelektra.so
%{_libdir}/libelektratools.so
%{_libdir}/pkgconfig/*
%doc examples doc/elektra-api/api-html
%{_mandir}/man3/*

# %files backend-gconf
# %defattr(-,root,root,0755)
# %{_prefix}/%{_libdir}/*gconf.so*

%files backend-berkeleydb
%defattr(-,root,root,0755)
/%{_lib}/elektra/*berkeleydb.so*


%changelog
* Sat Aug 6 2006 Yannick Lecaillez <sizon5@gmail.com> 0.6.3
- Adapted to Mandriva from the spec for Fedora made by Avi Alkalay
