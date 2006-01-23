%define name    elektra 
%define version 0.5.0.5 
%define release %mkrel 2 

Name:          %{name}
Version:       %{version}
Release:       %{release}
Source:        http://aleron.dl.sourceforge.net/sourceforge/elektra/%{name}-%{version}.tar.gz
Group:         System Environment/Libraries
License:       BSD
URL:           http://elektra.sourceforge.net
BuildRoot:     %{_tmppath}/%{name}-%{version}-%{release}-buildroot
BuildRequires: doxygen docbook-style-xsl db4-devel libGConf2-devel libxml2-devel automake autoconf libtool libxslt-proc
Summary:       A key/value pair database to store software configurations

%define DTDVERSION _DTDVERSION_

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
# ./configure --build=i586-mandriva-linux-gnu --prefix=/usr --exec-prefix=/usr --bindir=/usr/bin --sbindir=/usr/sbin --sysconfdir=/etc --datadir=/usr/share --includedir=/usr/include --libdir=/usr/lib --libexecdir=/usr/lib --localstatedir=/var/lib --sharedstatedir=/usr/com --mandir=/usr/share/man --infodir=/usr/share/info --x-includes=/usr/X11R6/include --x-libraries=/usr/X11R6/lib
%define _prefix /usr
%define _exec_prefix /
%define _sysconfdir /etc
%define _includedir /usr/include
%define _mandir /usr/share/man
%configure2_5x
# ./configure i586-mandriva-linux-gnu --prefix=/usr --exec-prefix=/ --sysconfdir=/etc --includedir=/usr/include --mandir=/usr/share/man
%make
# make DESTDIR=$RPM_BUILD_ROOT

%install
rm -rf $RPM_BUILD_ROOT
%makeinstall
rm $RPM_BUILD_ROOT/lib/libelektra-*.a
rm $RPM_BUILD_ROOT/lib/libregistry*.a
mv $RPM_BUILD_ROOT/lib/libelektra.a $RPM_BUILD_ROOT/usr/lib
rm $RPM_BUILD_ROOT/lib/*.la
rm $RPM_BUILD_ROOT/usr/lib/*.la
ln -s ../../lib/libelektra.so $RPM_BUILD_ROOT/usr/lib

%clean
rm -rf $RPM_BUILD_ROOT

%post
/sbin/ldconfig
# Backwards compatibility, from the Linux Registry days
if [ -d /etc/registry -a ! -d /etc/kdb ]; then
	mv /etc/registry /etc/kdb
	ln -s kdb /etc/registry
fi

# Create basic key structure for apps
kdb set -t dir system/sw
kdb set system/sw/kdb/current/schemapath "%{_datadir}/sgml/elektra-%{DTDVERSION}/elektra.xsd"


%postun -p /sbin/ldconfig



%files
%defattr(-,root,root,0755)
%{_libdir}/*elektra.so*
%{_libdir}/*registry.so*
%{_libdir}/*elektra-filesys.so*
%{_libdir}/*elektra-default.so*
%{_libdir}/*elektra-fstab.so*
%{_libdir}/*elektra-ini.so*
%{_prefix}/%{_libdir}/*elektratools.so*
%{_prefix}/%{_libdir}/*elektra.so*
/bin/*
%{_sysconfdir}/profile.d/*
%doc %{_docdir}/%{name}
%doc %{_mandir}/man1/*
%doc %{_mandir}/man7/*
%doc %{_mandir}/man5/*
%{_datadir}/sgml/*


%files devel
%defattr(-,root,root,0755)
%{_includedir}/*
/usr/lib/*.a
/usr/lib/pkgconfig/*
%doc %{_docdir}/%{name}-devel
%doc %{_mandir}/man3/*



%files backend-gconf
%defattr(-,root,root,0755)
/usr/lib/*gconf.so*

%files backend-berkeleydb
%defattr(-,root,root,0755)
/lib/*berkeleydb.so*


%changelog
* Mon Jan 23 2006 Yannick Lecaillez <yl@itioweb.com>
- Add symbolink link of /liblibelektra.so to /usr/lib
