#!/bin/bash
#
# Build System Tests
# ---------------------------------------------------------------------------

cd ../..

# Make without Berkeley DB
# ---------------------------------------------------------------------------

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

make distclean
./configure --jobs=1 --without-readline
make all
if [ $? -ne 0 ] ; then
   echo "without-readline failed"
fi
# TODO: readline not checked by running spar.  an easy way??

# Make without Sound
# ---------------------------------------------------------------------------

make distclean
./configure --jobs=1 --without-sound
make all
if [ $? -ne 0 ] ; then
   echo "without-sound failed"
fi
# TODO: sound not checked by running spar.

# Make without OpenGL
# ---------------------------------------------------------------------------

make distclean
./configure --jobs=1 --without-opengl
make all
if [ $? -ne 0 ] ; then
   echo "without-opengl failed"
fi
# TODO: opengl not checked by running spar.

# Make without SDL
# ---------------------------------------------------------------------------

make distclean
./configure --jobs=1 --without-sdl
make all
if [ $? -ne 0 ] ; then
   echo "without-sdl failed"
fi
# TODO: pcre not checked by running spar.

# Make without PCRE
# ---------------------------------------------------------------------------

make distclean
./configure --jobs=1 --without-pcre
make all
if [ $? -ne 0 ] ; then
   echo "without-pcre failed"
fi
# TODO: pcre not checked by running spar.

# Make without L8N
# ---------------------------------------------------------------------------

make distclean
./configure -jobs=1 --without-l10n
make all
if [ $? -ne 0 ] ; then
   echo "without-pcre failed"
fi
# TODO: pcre not checked by running spar.

# Cleanup

make distclean
echo "BUILD SYSTEM TESTS ARE OK"
exit 0

