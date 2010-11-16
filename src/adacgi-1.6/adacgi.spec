# Copyright (C) 1999, 2000
# by Juergen Pfeifer <juergen@gnuada.org>
# Ada for Linux Team (ALT)
#
# Permission is hereby granted, free of charge, to any person obtaining a  
# copy of this software and associated documentation files (the            
# "Software"), to deal in the Software without restriction, including      
# without limitation the rights to use, copy, modify, merge, publish,      
# distribute, distribute with modifications, sublicense, and/or sell       
# copies of the Software, and to permit persons to whom the Software is    
# furnished to do so, subject to the following conditions:                 
#                                                                          
# The above copyright notice and this permission notice shall be included  
# in all copies or substantial portions of the Software.                   
#                                                                          
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   
# IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    
# OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    
# THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               
#                                                                          
# Except as contained in this notice, the name(s) of the above copyright   
# holders shall not be used in advertising or otherwise to promote the     
# sale, use or other dealings in this Software without prior written       
# authorization.          
#
# ---------------------------------------------------------------------------
# Author: Juergen Pfeifer <juergen@gnuada.org>
# ---------------------------------------------------------------------------
# GNU Ada RPM Packaging for David Wheelers CGI package
#
# This specification file is generated using a m4 based macrosystem.
# Please don't change it directly but get instead the macro system
# and change the input files of this system. 
# ---------------------------------------------------------------------------
#
%define        PKG_TARGET_OS       Linux
%define        TARGET_OS           linux
%define        TARGET_ARCH         i386
%define        COMPAT_TARGET_ARCH  i386 i486 i586 i686
%define        DEF_USER            bin
%define        DEF_GROUP           bin
%define        CFG_PREFIX          /usr
#
%define        AUTHOR_NAME         Juergen Pfeifer
%define        AUTHOR_MAIL	   juergen.pfeifer@gmx.net
#
%define        GNAT_BASE_VERSION   3.12
%define        GNAT_PKG_VERSION    3.12p
%define        GNAT_PKG_RELEASE    10
%define        GCC_VERSION         2.8.1
%define        GDB_VERSION         4.17
%define        GLIBC_VERSION	   2.1
%define        GNAT_VERSION        3.12p
%define        BASE_VERSION        gcc-%{GCC_VERSION}
#
%define        BASE_NAME           gnat
%define        GNAT_PKG_NAME       gnat
%define        GNATRT_PKG_NAME     gnat-3.12p-runtime
%define        GNATRT_PKG_VERSION  1
#
%define        ADA_ROOT            /lib/ada
%define        ADA_BASE            %{CFG_PREFIX}%{ADA_ROOT}
%define        SHARE_BASE	   /share/ada
%define        ADA_SHARE           %{CFG_PREFIX}%{SHARE_BASE}
#
%define        HTML_BASE	   %{SHARE_BASE}/html
%define        ADA_HTML	           %{CFG_PREFIX}%{HTML_BASE}
#
%define        GNAT_SO_MAJOR       1
%define        GNAT_SO_MINOR       %{GNAT_PKG_RELEASE}
#
%define        RPM_C_FLAGS         -O3 -m486
%define        RPM_ADA_FLAGS       -gnatpn
#
%define        THREADLIBS          -lpthread
#
%define        DOC_BASE            %{CFG_PREFIX}/doc
#
%define        GNAT_ALIAS          %{BASE_NAME}
%define        GNAT_NAME           %{GNAT_ALIAS}-%{GNAT_VERSION}
%define        GNAT_SO             %{GNAT_NAME}
#
%define        GNARL_ALIAS         gnarl
%define        GNARL_NAME          %{GNARL_ALIAS}-%{GNAT_VERSION}
%define        GNARL_SO            %{GNARL_NAME}
#
%define        AINC_NAME           adainclude
%define        AINC_DIR            %{ADA_BASE}/%{AINC_NAME}
#
%define        ALIB_NAME           adalib
%define        ALIB_DIR            %{ADA_BASE}/%{ALIB_NAME}
#
%define        ETC_NAME            etc
%define        ETC_DIR             %{ADA_BASE}/%{ETC_NAME}
%define        EMACS_DIR           %{ADA_ROOT}/%{ETC_NAME}/adamode
%define        ADAMODE_DIR         %{CFG_PREFIX}%{EMACS_DIR}
%define        INFO_NAME           %{ADA_ROOT}/info
%define        INFO_DIR            %{CFG_PREFIX}%{INFO_NAME}
#
%define        ASIS_ALIAS          asis
%define        ASIS_NAME           %{ASIS_ALIAS}-%{GNAT_VERSION}
%define        ASIS_SO             %{ASIS_NAME}
%define        ASIS_DIR            %{ADA_BASE}/%{ASIS_NAME}
#
%define        GLADE_ALIAS         garlic
%define        GLADE_NAME          garlic-%{GNAT_VERSION}
%define        GLADE_SO            garlic-%{GNAT_VERSION}
%define        GLADE_DIR           %{ADA_BASE}/%{GLADE_NAME}
#
%define        FLORIST_ALIAS       florist
%define        FLORIST_NAME        %{FLORIST_ALIAS}-%{GNAT_VERSION}
%define        FLORIST_SO          %{FLORIST_NAME}
#
%define        GNAT_GCC            gnatgcc
#
%define PKG_NAME    adacgi
%define PKG_VERSION 1.6
%define PKG_RELEASE 1
# 
# If there is a runtime package, it has this name
%define PKGRT_NAME  adacgi-runtime
#
# Name of development package in the old scheme
%define OLD_DEVPKG_NAME adacgi-devel-3.12p
%define OLD_RTNAME      adacgi-3.12p
#
# ---------------------------------------------------------------------------
# We restrict the use of this package script at the moment to Intel and
# Linux. For sure there are details (compiler options, shared library
# building) that must be fixed for other architectures and OSes.
#
ExclusiveArch: %{COMPAT_TARGET_ARCH}
ExclusiveOS:   %{PKG_TARGET_OS}
Vendor:        Ada for Linux Team (ALT)
Packager:      Juergen Pfeifer
Distribution:  ALT (Ada for Linux Team)
Version:       %{PKG_VERSION}
Release:       %{PKG_RELEASE}
Name:          %{PKG_NAME}
BuildRoot:     %{_builddir}/%{PKG_NAME}_glibc%{GLIBC_VERSION}-%{version}-root
Autoreq:       No
Autoreqprov:   No
# ---------------------------------------------------------------------------
#
# This are the official sources
Source0:       http://www.dwheeler.com/adacgi/latest/adacgi-%{version}.tar.gz
#
#  ---------------------------------------------------------------------------
Copyright:     GPL (modified for runtime)
URL:           http://www.dwheeler.com
Summary:       Ada95 CGI Programming Interface
Summary(de):   Ada95 Interface fu%r CGI Programmierung
Group:         Libraries
Requires:      %{GNAT_PKG_NAME} = %{GNAT_PKG_VERSION}
Provides:      %{name}
Obsoletes:     %{name}
Conflicts:     %{name}-%{GNAT_VERSION}
Prefix:        %{CFG_PREFIX}
# I don't have this icon:
# Icon:          adasmall.gif
#
%description
This is David A. Wheeler's package for implementing web programs in Ada95.
This is an Ada95 binding to CGI (the Common Gateway Interface), a common
interface between web servers and specialized web server applications.
You only need to install this package if you're compiling Ada95 programs
which use this binding.
%description -l de
Dies ist David A. Wheeler's Paket fu%r die Implementierung von Web Programmen
in Ada95. Es ist ein Ada95 Binding fu%r CGI (das Common Gateway Interface),
eine allgemeine Schnittstelle zwischen Web-Servern und Web-Server Anwendungen.
Sie brauchen dieses Paket nur zu installieren wenn Sie Ada95 Programme schreiben
wollen, die dieses Binding benutzen.
#
#-----------------------------------------------------------------------------
%prep
#=============================================================================
#
%setup
#
#-----------------------------------------------------------------------------
%build
#=============================================================================
#
# Get the name of the Operating System in lower case
OS=%{TARGET_OS}
#
PATH=/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin
if [ "${GNAT_ROOT}" != "" ]; then
   PATH="${GNAT_ROOT}/bin:${PATH}"
fi
export PATH
#
%define ALT_BUILD_DOC  $RPM_BUILD_ROOT/doc
%define ALT_BUILD_HTML $RPM_BUILD_ROOT%{ADA_HTML}
#
# Create the required directory structure for the binary package
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{CFG_PREFIX}/info
mkdir -p $RPM_BUILD_ROOT%{CFG_PREFIX}/bin
mkdir -p $RPM_BUILD_ROOT%{CFG_PREFIX}/lib
mkdir -p $RPM_BUILD_ROOT%{CFG_PREFIX}/doc
mkdir -p $RPM_BUILD_ROOT%{AINC_DIR}
mkdir -p $RPM_BUILD_ROOT%{ALIB_DIR}
mkdir -p $RPM_BUILD_ROOT%{GLADE_DIR}
mkdir -p $RPM_BUILD_ROOT%{ASIS_DIR}
mkdir -p $RPM_BUILD_ROOT%{ETC_DIR}
mkdir -p $RPM_BUILD_ROOT%{INFO_DIR}
mkdir -p $RPM_BUILD_ROOT%{ADA_SHARE}
mkdir -p %{ALT_BUILD_DOC}
mkdir -p %{ALT_BUILD_HTML}
#
mkdir -p %{ALT_BUILD_DOC}/sample
mkdir -p %{ALT_BUILD_HTML}/adacgi
#
%{GNAT_GCC} -c  %{RPM_C_FLAGS} %{RPM_ADA_FLAGS} cgi.adb
#
#-----------------------------------------------------------------------------
%install
#=============================================================================
OS=%{TARGET_OS}
#
#
cp -pf cgi.ad?     $RPM_BUILD_ROOT%{AINC_DIR}
cp -pf cgi.ali     $RPM_BUILD_ROOT%{ALIB_DIR}
cp -pf cgi.o       $RPM_BUILD_ROOT%{ALIB_DIR}
rm -f  cgi.ad?
#
cp -pf  README      %{ALT_BUILD_DOC}/
cp -pf  COPYING     %{ALT_BUILD_DOC}/
cp -pf  *.txt       %{ALT_BUILD_DOC}/
cp -pf  cgi.html    %{ALT_BUILD_DOC}/
cp -pf  cgi.html    %{ALT_BUILD_HTML}/cgi-doc.html
cp -pf  *.ad?       %{ALT_BUILD_DOC}/sample    
cp -pf  makefile    %{ALT_BUILD_DOC}/sample    
cp -pf  screen.html %{ALT_BUILD_DOC}/sample    
rm -f   %{ALT_BUILD_DOC}/sample/cgi.ad?
#
pushd $RPM_BUILD_ROOT%{AINC_DIR}
${GNAT_ROOT:-/usr}/bin/gnathtml -I../%{ALIB_NAME} \
  -o%{ALT_BUILD_HTML}/adacgi *.ad[bs]
popd
#
if [ -d ${RPM_BUILD_ROOT}%{AINC_DIR} ]; then
  find ${RPM_BUILD_ROOT}%{AINC_DIR} -type f -name '*.ad?' -exec chmod 0444 {} \;
fi
if [ -d ${RPM_BUILD_ROOT}%{ALIB_DIR} ]; then
  find ${RPM_BUILD_ROOT}%{ALIB_DIR} -type f -name '*.ali' -exec chmod 0444 {} \;
  find ${RPM_BUILD_ROOT}%{ALIB_DIR} -type f -name '*.o'   -exec chmod 0444 {} \;
  find ${RPM_BUILD_ROOT}%{ALIB_DIR} -type f -name '*.a'   -exec chmod 0444 {} \;
fi
if [ -d ${RPM_BUILD_ROOT}%{CFG_PREFIX}/lib ]; then
  find ${RPM_BUILD_ROOT}%{CFG_PREFIX}/lib -type f -name '*.so*' -exec chmod 0755 {} \;
fi
if [ -d ${RPM_BUILD_ROOT}/doc ]; then
  find ${RPM_BUILD_ROOT}/doc -type f -exec chmod 0444 {} \;
  find ${RPM_BUILD_ROOT}/doc -type d -exec chmod 0755 {} \;
fi
#  ---------------------------------------------------------------------------
%pre
if [ -f /etc/profile.d/%{BASE_NAME}-rts.sh ]; then
  source /etc/profile.d/%{BASE_NAME}-rts.sh
  if [ ! -z ${GNAT_ROOT} ]; then
    if [ "${GNAT_ROOT}" != "${RPM_INSTALL_PREFIX}" ]; then
      echo "%{BASE_NAME} was installed with --prefix=${GNAT_ROOT}" >&2
      echo "You must use the same prefix with this package." >&2
      exit 1
    fi
  else
    if [ "${RPM_INSTALL_PREFIX}" != "%{CFG_PREFIX}" ]; then
      echo "%{BASE_NAME} was installed without the --prefix option." >&2
      echo "You must do this also with this package." >&2
      exit 1
    fi
  fi
fi
#  ---------------------------------------------------------------------------
%post
etc_prof="/etc/profile.d"
ETC_DIR="${RPM_INSTALL_PREFIX}%{ADA_ROOT}/%{ETC_NAME}"
#
htmldir=${RPM_INSTALL_PREFIX}/share/ada/html
pkgfile=${htmldir}/packages

html_insert() {
  mkdir -p ${htmldir} 2>/dev/null
  if [ -f $pkgfile ]; then
    awk -F, '{printf "%s,\n",$1;}' < $pkgfile | grep -q "$1,"
    if [ $? -eq 0 ]; then
       sed -e "/^$1,/d" < $pkgfile >${pkgfile}.tmp
       mv -f ${pkgfile}.tmp ${pkgfile}
    fi
  fi
  echo "$1,$2,$3" >> $pkgfile
}

html_remove() {
   sed -e "/^$1,/d" < ${pkgfile} > ${pkgfile}.tmp
   mv -f ${pkgfile}.tmp ${pkgfile}
}

html_generate() {
  insmarker="<!--PACKAGES-->"
  indexfile=${htmldir}/index.html
  touch ${pkgfile}.in
  if [ -f ${pkgfile} ]; then
    awk -F, '{printf "<li><a href=\"%s\" TARGET=\"%s\">%s</a></li>\n",$2,$1,$3;}' < ${pkgfile} > ${pkgfile}.in
  fi
  if [ -f ${indexfile}.in ]; then
    cat > ${pkgfile}.sed <<EOF
/^${insmarker}/r ${pkgfile}.in
/^${insmarker}/d
EOF
    sed -f ${pkgfile}.sed < ${indexfile}.in > ${indexfile}
    rm -f ${pkgfile}.sed
  fi
  rm -f ${pkgfile}.in
}
html_insert "adacgi"  "cgi-doc.html"     "CGI Binding Documentation"
html_insert "adacgi2" "adacgi/index.htm" "CGI Binding Packages"
html_generate
#  ---------------------------------------------------------------------------
%postun
etc_prof="/etc/profile.d"
ETC_DIR="${RPM_INSTALL_PREFIX}%{ADA_ROOT}/%{ETC_NAME}"
#
htmldir=${RPM_INSTALL_PREFIX}/share/ada/html
pkgfile=${htmldir}/packages

html_insert() {
  mkdir -p ${htmldir} 2>/dev/null
  if [ -f $pkgfile ]; then
    awk -F, '{printf "%s,\n",$1;}' < $pkgfile | grep -q "$1,"
    if [ $? -eq 0 ]; then
       sed -e "/^$1,/d" < $pkgfile >${pkgfile}.tmp
       mv -f ${pkgfile}.tmp ${pkgfile}
    fi
  fi
  echo "$1,$2,$3" >> $pkgfile
}

html_remove() {
   sed -e "/^$1,/d" < ${pkgfile} > ${pkgfile}.tmp
   mv -f ${pkgfile}.tmp ${pkgfile}
}

html_generate() {
  insmarker="<!--PACKAGES-->"
  indexfile=${htmldir}/index.html
  touch ${pkgfile}.in
  if [ -f ${pkgfile} ]; then
    awk -F, '{printf "<li><a href=\"%s\" TARGET=\"%s\">%s</a></li>\n",$2,$1,$3;}' < ${pkgfile} > ${pkgfile}.in
  fi
  if [ -f ${indexfile}.in ]; then
    cat > ${pkgfile}.sed <<EOF
/^${insmarker}/r ${pkgfile}.in
/^${insmarker}/d
EOF
    sed -f ${pkgfile}.sed < ${indexfile}.in > ${indexfile}
    rm -f ${pkgfile}.sed
  fi
  rm -f ${pkgfile}.in
}
html_remove adacgi
html_remove adacgi2
html_generate
#  ---------------------------------------------------------------------------
%files
%defattr(-,%{DEF_USER},%{DEF_GROUP})
%doc %{ALT_BUILD_DOC}/*
%dir %{CFG_PREFIX}
%dir %{ADA_BASE}
%dir %{AINC_DIR}
%dir %{ALIB_DIR}
%dir %{ADA_HTML}
%dir %{ADA_HTML}/adacgi
%{AINC_DIR}/*
%{ALIB_DIR}/*
%{ADA_HTML}/*.html
%{ADA_HTML}/adacgi/*
#
#  ---------------------------------------------------------------------------
%clean
rm -rf $RPM_BUILD_ROOT
#  ---------------------------------------------------------------------------

rm -rf $RPM_BUILD_DIR/adacgi
#
%changelog
* Wed Nov 1 2000 David A. Wheeler <dwheeler@dwheeler.com>
- Released version 1.6.

* Tue Mar 14 2000 Juergen Pfeifer <juergen@gnuada.org>
- Published as Release 1 of Version 1.5

* Wed Mar 01 2000 Juergen Pfeifer <juergen@gnuada.org>
- Fixed documentation generation in spec file

* Sun Feb 27 2000 Juergen Pfeifer <juergen@gnuada.org>
- Published as Release 6

* Wed Feb 23 2000 Juergen Pfeifer <juergen@gnuada.org>
- Grand package renaming

* Mon Jan 03 2000 Juergen Pfeifer <juergen@gnuada.org>
- Published as Release 5

* Wed Dec 29 1999 Juergen Pfeifer <juergen@gnuada.org>
- Inlined HTML generation into the installation scripts to eliminate
  installation order dependencies.
  
* Mon Nov 15 1999 Juergen Pfeifer <juergen@gnuada.org>
- Published as Release 4

* Sun Nov 07 1999 Juergen Pfeifer <juergen@gnuada.org>
- Added gnathtml generated package docu
- Integrate AdaCGI HTML page into overall HTML docu. 

* Sun Oct 31 1999 Juergen Pfeifer <juergen@gnuada.org>
- Fixed build architecture handling
- Published as Release 3

* Fri Oct 29 1999 Juergen Pfeifer <juergen@gnuada.org>
- Published as Release 2 (3.12p based)

* Tue Sep 14 1999 David A. Wheeler <dwheeler@dwheeler.com>
- Updated to adacgi 1.4:
  + merged in Juergen Pfeifer's patch,
  + added cookie support
  + Expanded documentation
  + License change to be LGPL-like

* Wed Mar 31 1999 Juergen Pfeifer <juergen@gnuada.org>
- Changed description
- Published as Release 1

* Wed Mar 10 1999 Juergen Pfeifer <juergen@gnuada.org>
- Created
