#
# This file and all modifications and additions to the pristine
# package are under the same license as the package itself.
#

# norootforbuild

Name:           %{name}
Version:	%{version}
Release:	%{release}
Summary:	SparForte programming language
Group:		Development/Languages/Other
License:	GPL-2.0+
Url:		http://www.pegasoft.ca/sparforte.html
Source:		http://www.pegasoft.ca/downloads/%{name}-%{version}.tar.gz
BuildRoot:      %{buildroot}
AutoReqProv:    on

%description

SparForte is an interepreted programming language with a shell, intrinsic
database efatures and a web template engine.  It is intended for fast,
scalable, well-designed projects.  Documentation is available on the
project website

%prep
%setup -q

%build
%configure --released
make max
strip src/spar

%install
%makeinstall

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)
%doc doc
%attr(0444,root,root) /usr/share/man/man1/spar.1.gz
/usr/bin/spar

%changelog
* Sun Jun 29 2014 Ken Burtch <ken@pegasoft.ca>
- 20140629-1 initial version of package spec %{version}

