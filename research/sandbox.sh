#!/bin/bash

SANDBOX_PATH="sandbox"

SPAR_PATH="/usr/local/bin/spar_repl"

# SparForte dependencies

function copy_libraries {
echo "$1" | while read LIB_LINE ; do
  LIB_ALIAS=`echo "$LIB_LINE" | cut -d= -f1`
  LIB_PATH=`echo "$LIB_LINE" | cut -d= -f2`
  LIB_PATH=`echo "$LIB_PATH" | cut -d\( -f1 | cut -c2- | tr -d ' '`
  LIB_DIRNAME=`dirname "$LIB_PATH"`
  LIB_BASENAME=`basename "$LIB_PATH"`
  if [ -n "$LIB_PATH" ] ; then
     echo "$LIB_ALIAS"
     echo "  $LIB_PATH"
     echo "  $LIB_DIRNAME"
     echo "  $LIB_BASENAME"
     # not sure if libraries need to be moved...
     TMP=`echo "$LIB_PATH" | cut -c 1`
     if [ "$TMP" != "/" ] ; then
        LIB_PATH="/""$LIB_PATH"
     fi
     cp -p --parents "$LIB_PATH" "$SANDBOX_PATH""/"
  fi
done
}

# Clean sandbox

if [ -d "$SANDBOX_PATH" ] ; then
   rmdir "$SANDBOX_PATH"
   mkdir "$SANDBOX_PATH"
fi

# Install SparForte dependences

LIB_LIST=`ldd "$SPAR_PATH"`
copy_libraries "$LIB_LIST"

# Also need tput dependencies

LIB_LIST=`ldd /usr/bin/tput`
copy_libraries "$LIB_LIST"

if [ ! -d "$SANDBOX_PATH""/tmp" ] ; then
   mkdir "$SANDBOX_PATH""/tmp"
   chmod 1777 "$SANDBOX_PATH""/tmp"
fi
if [ ! -d "$SANDBOX_PATH""/bin" ] ; then
   mkdir "$SANDBOX_PATH""/bin"
fi
if [ ! -d "$SANDBOX_PATH""/usr/share/terminfo/x" ] ; then
   mkdir "$SANDBOX_PATH""/usr/share"
   mkdir "$SANDBOX_PATH""/usr/share/terminfo"
   mkdir "$SANDBOX_PATH""/usr/share/terminfo/x"
   cp -v /usr/share/terminfo/x/xterm "$SANDBOX_PATH""/usr/share/terminfo/x/"
fi

cp -v "$SPAR_PATH" "$SANDBOX_PATH""/bin"
cp -v /usr/bin/tput "$SANDBOX_PATH""/bin"

# chroot sandbox /bin/spar_repl -r

