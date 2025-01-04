#!/bin/sh

# Package a SparForte source distribution in a tar file.
# by Ken O. Burtch
# January 2025

# This exists to avoid the occasional copying errors when manually
# building a release.

# Sanity checks

if [ ! -d "src" ] ; then
   echo "Run in the project root directory"
   exit 192
fi
if [ ! -x "src/spar" ] ; then
   echo "SparForte must be built first"
   exit 192
fi


# Defaults

MAKE="gmake"
BUILD_COMPRESSION="j"
BUILD_NUMBER="1"

# Build options

echo -n "Make program? ($MAKE):"
read REPLY
if [ "$REPLY" != "" ] ; then
   MAKE="$REPLY"
fi

VERSION=`src/spar -e '? System.System_Version'`

BUILD_DIR="sparforte-""$VERSION""-""$BUILD_NUMBER""-src"
BUILD_TARBALL="$BUILD_DIR"".tar.bz2"

START_DIR=`pwd`

if [ -d "../""$BUILD_DIR" ] ; then
   echo "Build directory '$BUILD_DIR' already exists"
   exit 192
fi

echo
echo "Build directory: $BUILD_DIR"
echo "Build archive:   $BUILD_TARBALL"
sleep 5
echo

# Rebuild SparForte

"$MAKE" distclean
if [ $? -ne 0 ] ; then
   echo "make distclean failed"
   exit 192
fi
./configure $BUILD_CONFIG_OPTIONS
if [ $? -ne 0 ] ; then
   echo "Error: configure for release failed"
   exit 192
fi
"$MAKE" all
if [ $? -ne 0 ] ; then
   echo "Error: make all failed"
   exit 192
fi
cd src
"$MAKE" clean
if [ $? -ne 0 ] ; then
   echo "make distclean failed"
   exit 192
fi
# srctar will do a distclean
"$MAKE" srctar
if [ $? -ne 0 ] ; then
   echo "Error: make srctar failed"
   exit 192
fi
cd "$START_DIR"

# Copying and rebuilding in the build directory

echo
echo "Copying to '$BUILD_DIR'..."
mkdir "../""$BUILD_DIR"
cp "spar.tgz" "../""$BUILD_DIR""/"
cd "../""$BUILD_DIR"
tar xfz "spar.tgz"
if [ $? -ne 0 ] ; then
   echo "Error: tar extract failed"
   exit 192
fi

# Check for stray binaries and temp files before bintar
# These should already be removed by make but let's be
# sure
TMP=`find src -type f -size +300k -print | grep -v "^src/spar$" | grep -v ".o" | fgrep -v ".ali" | fgrep parser_pen.adb"`
if [ -n "$TMP" ] ; then
   echo "Error: Unaccounted for large files:"
   echo "$TMP"
   exit 192
fi
TMP=`find src -type f -name '*.tmp' -print`
if [ -n "$TMP" ] ; then
   echo "Error: Unaccounted for tmp files:"
   echo "$TMP"
   exit 192
fi
TMP=`find src -type f -name '*.out' -print`
if [ -n "$TMP" ] ; then
   echo "Error: Unaccounted for out files:"
   echo "$TMP"
   exit 192
fi
TMP=`find src -type f -name '*.t' -print`
if [ -n "$TMP" ] ; then
   echo "Error: Unaccounted for t files:"
   echo "$TMP"
   exit 192
fi
TMP=`find src -type f -name '*.tgz' -print`
if [ -n "$TMP" ] ; then
   echo "Error: Unaccounted for tgz files:"
   echo "$TMP"
   exit 192
fi

# Test build

echo
echo "Test build..."
./configure $BUILD_CONFIG_OPTIONS
if [ $? -ne 0 ] ; then
   echo "Error: configure for release failed"
   exit 192
fi
"$MAKE" all
if [ $? -ne 0 ] ; then
   echo "Error: make all failed"
   exit 192
fi



# Copy to archive

cd "$START_DIR"
echo
echo "Clearing and copying..."
rm -rf "../""$BUILD_DIR"
if [ $? -ne 0 ] ; then
   echo "rm build directory $BUILD_DIR failed"
   exit 192
fi
echo
echo "Copying to '$BUILD_DIR'..."
mkdir "../""$BUILD_DIR"
cp "spar.tgz" "../""$BUILD_DIR""/"
cd "../""$BUILD_DIR"
tar xfz "spar.tgz"
if [ $? -ne 0 ] ; then
   echo "Error: tar extract failed"
   exit 192
fi
rm "spar.tgz"
if [ $? -ne 0 ] ; then
   echo "Error: rm tar failed"
   exit 192
fi
cd ..

#

echo "Creating distribution archive '$BUILD_TARBALL'"
tar cf"$BUILD_COMPRESSION" "$BUILD_TARBALL" "$BUILD_DIR"
if [ $? -ne 0 ] ; then
   echo "Error: tar create $BUILD_TARBALL failed"
   exit 192
fi
tar tf"$BUILD_COMPRESSION" "$BUILD_TARBALL" > /dev/null
if [ $? -ne 0 ] ; then
   echo "Error: tar test '$BUILD_TARBALL' failed"
   exit 192
fi
cd "$START_DIR"
rm "spar.tgz"
if [ $? -ne 0 ] ; then
   echo "Error: rm tar failed"
   exit 192
fi
cd ..

# Upload

echo
echo -n "Upload to another host (y/n)"
read REPLY
if [ "$REPLY" = "y" ] ; then
   echo -n "scp account? "
   read SCP_ACCOUNT
   echo -n "scp directory? "
   read SCP_DIR
   set -x
   /usr/bin/scp "$BUILD_TARBALL" "$SCP_ACCOUNT"":""$SCP_DIR""/""$BUILD_TARBALL"
   set +x
   if [ $? -ne 0 ] ; then
      echo "Error: scp failed"
      exit 192
   fi
fi
echo "OK"

# Summary

echo
echo "Summary"
echo
FILE_SIZE=`ls -lhdFq "$BUILD_TARBALL" | cut -d' ' -f 5 | sed 's/ /&nbsp;/g'`
RELEASE_DATE=`date +'%Y/%m/%d'`
echo "Build Options: $BUILD_CONFIG_OPTIONS"
FILE_RESULT=`file "$BUILD_TARBALL" 2>&1`
TMP=`echo "$FILE_RESULT" | fgrep "bzip"`
if [ -z "$TMP" ] ; then
   echo "$FILE_RESULT"
   echo "Error: Archive may have used the wrong compression"
   exit 192
fi
echo "--- Website link ---"
echo "</tr><tr>"
echo "  <td class=\"download_left\"><strong>Sources</strong><br>Linux / FreeBSD / OS/X / etc.</td>"
echo "  <td class=\"download_left\">)Source Code and Docs<br>($RELEASE_DATE)</td>"
echo "  <td class=\"download_right\">$VERSION</td>"
echo "  <td class=\"download_right\">$FILE_SIZE</td>"
echo "  <td class=\"download_left\"><a href="downloads/$BUILD_TARBALL">Download bzip tar</a></td>"
echo "---"

echo "Done"

