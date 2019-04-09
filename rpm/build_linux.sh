#!/bin/bash

declare -r VERSION=2.2.1
declare -r RELEASE=1

declare -r ROOTDIR=`pwd`
declare -r TAR_FILE="$ROOTDIR""/SOURCES/sparforte-$VERSION.tar.gz"
declare -r SPEC_FILE="$ROOTDIR""/SPECS/sparforte_redhat.spec"

declare NOSIGN

echo "$TAR_FILE"

# Sanity tests

if [ ! -d SOURCES ] ; then
   echo "wrong directory - can't find rpm/SOURCES directory"
   exit 192
fi
if [ "$LOGNAME" = "root" ] ; then
   echo "should not be root"
   exit 192
fi
if [ ! -f ~/.gnupg/gpg.conf ] ; then
   echo "Warning: GPG configuration file not found"
   NOSIGN=1
fi

echo "Build RPMS for SparForte $VERSION-$RELEASE? (y/n)"
read REPLY
if [ "$REPLY" != "y" ] ; then
   exit 192
fi
if [ -z "$NOSIGN" ] ; then
   echo "Sign with GPG? (y/n)"
   read REPLY
   if [ "$REPLY" != "y" ] ; then
      NOSIGN=1
   fi
fi

# Create macros file with defaults for building the rpm
# (Otherwise, rpmbuild will default to /usr/src/packages instead of
# this build directory)

echo "%_topdir	$ROOTDIR" > ~/.rpmmacros
echo "%buildroot	$ROOTDIR/BUILDROOT" >> ~/.rpmmacros
echo "%name	sparforte" >> ~/.rpmmacros
echo "%version	$VERSION" >> ~/.rpmmacros
echo "%release	$RELEASE" >> ~/.rpmmacros

# Copy Sources to build directory

echo "Copying sources..."

# 1. Grab a copy of the entire source tree, docs, tests, etc.

cd SOURCES
rm -r *
cd ../..
make distclean
tar cfz "$TAR_FILE" * --exclude "rpm"
cd rpm

# 2. Repackage the tar under the directory sparforte-x.y

mkdir "sparforte-$VERSION"
cd "sparforte-$VERSION"
tar xfz "$TAR_FILE"
cd ..
tar cfz "$TAR_FILE" "sparforte-$VERSION"

# 3. Discard extra files

rm -rf "sparforte-$VERSION"

# Build the RPM

pwd
echo "Building..."
if [ -z "$NOSIGN" ] ; then
   rpmbuild -ba --sign --clean "$SPEC_FILE"
else
   rpmbuild -vv -ba --clean "$SPEC_FILE"
fi
if [ $? -ne 0 ] ; then
   exit $?
fi

# Verify RPM

echo "-----------------------------------------------------------------------"
echo "Verifying (current install)..."
rpm -Vp RPMS/x86_64/sparforte-$VERSION*
echo
echo "rpmlint..."
rpmlint --info RPMS/x86_64/sparforte-$VERSION*

# Cleanup

cd SOURCES
rm -fr *
cd ../BUILD
rm -fr *
cd ..

