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
License:	GPL
Url:		http://www.pegasoft.ca/sparforte.html
Source:		http://www.pegasoft.ca/downloads/%{name}-%{version}.tar.gz
Vendor:		PegaSoft Canada
BuildRoot:      %{buildroot}
AutoReqProv:    on

%description

SparForte is an interepreted programming language with a shell, intrinsic database efatures and a web template engine.  It is intended for fast, scalable, well-designed projects.  Documentation is available on the project website.

Authors:
--------
    The SparForte Team

%prep
%setup -q

%build
%configure
make max
strip src/spar

%install
%makeinstall

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc CHANGES README COPYING INSTALL
%doc doc
%attr(0444,root,root) /usr/share/man/man1/spar.1.gz
/usr/bin/spar

%changelog
* Sun Mar 23 2011 Ken Burtch <ken@pegasoft.ca>
- 20110323-1 initial version of package spec %{version}

