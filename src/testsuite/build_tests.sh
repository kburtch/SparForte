#!/bin/bash
#
# Build System Tests
# ---------------------------------------------------------------------------

cd ../..

# Make without Berkeley DB
# ---------------------------------------------------------------------------

echo "Build without Berkeley DB..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-bdb
make all
if [ $? -ne 0 ] ; then
   echo "failed"
fi
TMP=`src/spar -e "f : btree_io.file; ? btree_io.is_open( f );"`
if [ "$TMP" = "false" ] ; then
   echo "without-bdb failed"
   exit 192
fi

# Make both MySQL and PostgreSQL
# ---------------------------------------------------------------------------

echo "Build with everything (both databases)..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1
make all
if [ $? -ne 0 ] ; then
   echo "failed"
fi
TMP=`src/spar -e "? mysql.is_connected;"`
if [ "$TMP" != "false" ] ; then
   echo "defaults failed"
   exit 192
fi
TMP=`src/spar -e "? db.is_connected;"`
if [ "$TMP" != "false" ] ; then
   echo "defaults failed"
   exit 192
fi

# Make without MySQL
# ---------------------------------------------------------------------------

echo "Build without MySQL..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-mysql
make all
if [ $? -ne 0 ] ; then
   echo "failed"
fi
TMP=`src/spar -e "? mysql.is_connected;"`
if [ "$TMP" = "false" ] ; then
   echo "without-mysql failed"
   exit 192
fi
TMP=`src/spar -e "? db.is_connected;"`
if [ "$TMP" != "false" ] ; then
   echo "without-mysql failed"
   exit 192
fi

# Make without PostgreSQL
# ---------------------------------------------------------------------------

echo "Build without PostgreSQL..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-postgres
make all
if [ $? -ne 0 ] ; then
   echo "failed"
fi
TMP=`src/spar -e "? mysql.is_connected;"`
if [ "$TMP" != "false" ] ; then
   echo "without-postgres failed"
   exit 192
fi
TMP=`src/spar -e "? db.is_connected;"`
if [ "$TMP" = "false" ] ; then
   echo "without-postgres failed"
   exit 192
fi

# Make without MySQL and PostgreSQL
# ---------------------------------------------------------------------------

echo "Build without MySQL and without PostgreSQL..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-mysql --without-postgres
make all
if [ $? -ne 0 ] ; then
   echo "failed"
fi

TMP=`src/spar -e "? mysql.is_connected;"`
if [ "$TMP" = "false" ] ; then
   echo "without-mysql without-postgres failed"
   exit 192
fi
TMP=`src/spar -e "? db.is_connected;"`
if [ "$TMP" = "false" ] ; then
   echo "without-mysql without-postgres failed"
   exit 192
fi

# Make without Readline
# ---------------------------------------------------------------------------

echo "Build without GNU Readline..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-readline
make all
if [ $? -ne 0 ] ; then
   echo "without-readline failed"
   exit 192
fi
# TODO: readline not checked by running spar.  an easy way??

# Make without Sound
# ---------------------------------------------------------------------------

echo "Build without Sound..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-sound
make all
if [ $? -ne 0 ] ; then
   echo "without-sound failed"
   exit 192
fi
# TODO: sound not checked by running spar.

# Make without OpenGL
# ---------------------------------------------------------------------------

echo "Build with SDL but without OpenGL..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-opengl
make all
if [ $? -eq 0 ] ; then
   echo "without-opengl should fail without --without-sdl"
   exit 192
fi
# TODO: opengl not checked by running spar.

echo "Build without SDL or OpenGL..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-opengl --without-sdl
make all
if [ $? -ne 0 ] ; then
   echo "without-sdl and without-opengl failed"
   exit 192
fi
# TODO: opengl not checked by running spar.

# Make without SDL
# ---------------------------------------------------------------------------

echo "Build without SDL..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-sdl
make all
if [ $? -ne 0 ] ; then
   echo "without-sdl failed"
   exit 192
fi
# TODO: pcre not checked by running spar.

# Make without PCRE
# ---------------------------------------------------------------------------

echo "Build without PCRE..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-pcre
make all
if [ $? -ne 0 ] ; then
   echo "without-pcre failed"
   exit 192
fi
# TODO: pcre not checked by running spar.

# Make without L8N
# ---------------------------------------------------------------------------

echo "Build without L10N..."
echo "-----------------------------------------------------------------------"
echo
make distclean
./configure --jobs=1 --without-l10n
make all
if [ $? -ne 0 ] ; then
   echo "without-pcre failed"
   exit 192
fi
# TODO: pcre not checked by running spar.

# Cleanup

make distclean
echo "BUILD SYSTEM TESTS ARE OK"
exit 0

