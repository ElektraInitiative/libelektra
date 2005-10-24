Name: nss-registry
Version: 0.3
Release: 1
Source:    http://download.berlios.de/nss-registry/%{name}-%{version}.tar.gz
Group:     System Environment/Libraries
Copyright: GPL
Vendor: Jens Andersen <rayman@skumler.net>
Packager: Avi Alkalay <avi@unix.sh>
URL: http://nss-registry.skumler.net
BuildRoot:   %{_tmppath}/%{name}-%{version}-build
BuildRequires: registry-devel
Summary: A name system module that uses the Registry for users, shadow and groups database

%description
A name system module that uses the Registry for users, shadow and groups database


%prep
%setup
%build
./configure
make all

%install
make prefix=${RPM_BUILD_ROOT} slibdir=${RPM_BUILD_ROOT}/lib install


%clean
rm -rf $RPM_BUILD_ROOT/*




%files
%defattr(-,root,root,0755)
/lib/*
/sbin/*


