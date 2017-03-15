#!/bin/bash

SANDBOX_PATH="sandbox"

#mkdir "$SANDBOX_PATH"

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
     cp -p --parents "$LIB_PATH" "$SANDBOX_PATH""/"
  fi
done
}

LIB_LIST=`ldd spar`
copy_libraries "$LIB_LIST"

# Also need tput dependencies

LIB_LIST=`ldd /usr/bin/tput`
copy_libraries "$LIB_LIST"

mkdir "$SANDBOX_PATH""/bin"
cp spar "$SANDBOX_PATH""/bin"
cp /usr/bin/tput "$SANDBOX_PATH""/bin"

