Name:          elektra
Version:       0.5.0.5
%define myRelease 1
%define vepoch 1
Release:       0.mdk.%{vepoch}.%{myRelease}
Source:        http://aleron.dl.sourceforge.net/sourceforge/elektra/%{name}-%{version}.tar.gz
Group:         System Environment/Libraries
License:       BSD
URL:           http://elektra.sourceforge.net
BuildRoot:     %{_tmppath}/%{name}-%{version}-build
BuildRequires: doxygen docbook-style-xsl db4-devel libGConf2-devel libxml2-devel automake autoconf libtool /usr/bin/xsltproc
Summary:       A key/value pair database to store software configurations

%define DTDVERSION _DTDVERSION_

# A buggy version of rpmbuild
%define _sysconfdir /etc

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
./configure i586-mandriva-linux-gnu --prefix=/usr --exec-prefix=/ --sysconfdir=/etc --includedir=/usr/include --mandir=/usr/share/man
make DESTDIR=$RPM_BUILD_ROOT

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf $RPM_BUILD_ROOT
#rm -rf $RPM_BUILD_DIR/%{name}




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
/lib/*elektra.so*
/lib/*elektra-filesys.so*
/lib/*elektra-default.so*
/lib/*elektra-fstab.so*
/lib/*elektra-ini.so*
%{_libdir}/*elektratools.so*
/bin/*
%{_sysconfdir}/profile.d/*
# %doc %{_docdir}/%{name}
%doc %{_mandir}/man1/*
%doc %{_mandir}/man7/*
%doc %{_mandir}/man5/*
%{_datadir}/sgml/*



%files devel
%defattr(-,root,root,0755)
%{_includedir}/*
%{_libdir}/*.a

## These will be soonly generated
# %{_libdir}/pkgconfig/*
# %doc %{_docdir}/%{name}-devel
# %doc %{_mandir}/man3/*



%files backend-gconf
%defattr(-,root,root,0755)
/lib/*gconf.so*

%files backend-berkeleydb
%defattr(-,root,root,0755)
/lib/*berkeleydb.so*



%changelog
