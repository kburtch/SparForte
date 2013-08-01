#!/bin/sh

#  SETUP
#
# Setup the test environment
# ---------------------------------------------------------------------------

setup() {
  test -f ./test.txt && rm ./test.txt
  test -f ./test2.txt && rm ./test2.txt

  if test ! -f ./write_only.txt ; then
     touch ./write_only.txt
     chmod 222 ./write_only.txt
  fi

  if test ! -f ./exec_only.txt ; then
     touch ./exec_only.txt
     chmod 111 ./exec_only.txt
  fi

  mkdir cdtest
  chmod 755 cdtest

  export FOOBAR="foobar"
  export FOOBAR1="foobar"
  export FOOBAR2="foobar"
  export FOOBAR3="foobar"
  export FOOBAR4="foobar"
  export FOOBAR_INT="98"
  export FOOBAR_STRING="\"foobar\""
  export FOOBAR_INT_ARRAY="[32]"
  export FOOBAR_RECORD="{\"s\":\"foo\"}"

  export FOOBAR_UNCHECKED="foobar"
  export FOOBAR_INT_UNCHECKED="98"
  export FOOBAR_STRING_UNCHECKED="\"foobar\""
  export FOOBAR_INT_ARRAY_UNCHECKED="[32]"
  export FOOBAR_RECORD_UNCHECKED="{\"s\":\"foo\"}"
}

#  TEAR DOWN
#
# Destory the test environment
# ---------------------------------------------------------------------------

teardown() {
  test -f ./test.txt && rm ./test.txt
  test -f ./test2.txt && rm ./test2.txt

  if [ -f ./write_only.txt ] ; then
     chmod 722 ./write_only.txt
     rm ./write_only.txt
  fi

  if [ -f ./exec_only.txt ] ; then
     chmod 711 ./exec_only.txt
     rm ./exec_only.txt
  fi

  if [ -d cdtest ] ; then
     rmdir cdtest
  fi

  # test tmp files use "test" prefix

  if [ -f test?????? ] ; then
     rm test??????
  fi
}

#  BAD TEST
#
# Run the test script with no parameters.  Declare FOOBAR.  If there's a
# core dump or a success code is returned, abort with the error.
# ---------------------------------------------------------------------------

bad_test() {
  echo "Running $1..."
  setup
  ../spar --debug "$1" < /dev/null > /dev/null
  RESULT=$?
  teardown
  TMP=`ls core 2>/dev/null`
  test -f ./test.txt && rm ./test.txt
  if [ ! -z "$TMP" ] ; then
     rm core
     echo
     echo "--- $1 FAILED WITH CORE DUMP ---"
     echo "Test was:"
     cat "$1"
     exit 1
  fi
  if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     exit 1
  fi
}

#  BAD TEST
#
# Run the test script with no parameters.  Declare FOOBAR.  Pass 3 parameters
# to the script.  If there's a core dump or a success code is returned, abort
# with the error.
# ---------------------------------------------------------------------------

bad_test_wparam() {
  echo "Running $1..."
  setup
  ../spar --debug "$1" a b c < /dev/null > /dev/null
  RESULT=$?
  teardown
  TMP=`ls core 2>/dev/null`
  test -f ./test.txt && rm ./test.txt
  if [ ! -z "$TMP" ] ; then
     rm core
     echo
     echo "--- $1 FAILED WITH CORE DUMP ---"
     echo "Test was:"
     cat "$1"
     exit 1
  fi
  if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     exit 1
  fi
}

#  BAD TEST TESTMODE
#
# Run the test script with no parameters is SparForte testing mode.
# Declare FOOBAR.  If there's a core dump or a success code is returned,
# abort with the error.
# ---------------------------------------------------------------------------

bad_test_testmode() {
  echo "Running $1..."
  setup
  ../spar --debug --test "$1" < /dev/null > /dev/null
  RESULT=$?
  teardown
  TMP=`ls core 2>/dev/null`
  test -f ./test.txt && rm ./test.txt
  if [ ! -z "$TMP" ] ; then
     rm core
     echo
     echo "--- $1 FAILED WITH CORE DUMP ---"
     echo "Test was:"
     cat "$1"
     exit 1
  fi
  if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     exit 1
  fi
}

#  BAD TEST
#
# Run the test script with no parameters.  Declare FOOBAR.  If there's a
# non-success code is returned, abort with the error.  Do it with syntax-only
# and normally.
# ---------------------------------------------------------------------------

good_test() {
  echo "Running $1..."
  setup
  ../spar --debug --check "$1" a b c
  RESULT=$?
  teardown
  test -f ./test.txt && rm ./test.txt
  if [ $RESULT -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     exit 1
  fi
  setup
  ../spar --debug "$1" a b c
  RESULT=$?
  teardown
  test -f ./test.txt && rm ./test.txt
  if [ $RESULT -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     exit 1
  fi
}

# ---------------------------------------------------------------------------
# SPARFORTE TEST SUITE
# ---------------------------------------------------------------------------

if [ "$1" = "-h" ] ; then
   echo "Bush test suite"
   echo
   echo "usage: $0 [-b]"
   echo " -b - bad syntax tests only"
   exit
fi

# Cleanup temp file (if it exists)

test -f ./test.txt && rm ./test.txt

# Create special test files

if test ! -f ./write_only.txt ; then
   touch ./write_only.txt
   chmod 222 ./write_only.txt
fi

if test ! -f ./exec_only.txt ; then
   touch ./exec_only.txt
   chmod 111 ./exec_only.txt
fi

# Run the good tests (if not -b (bad-only)

if [ "$1" != "-b" ] ; then
   good_test "goodtest.bush"
   good_test "goodtest2.bush"
fi

# Search testsuite* directories and run bad tests stored there

echo
echo "Testing bad scripts:"

ls -d testsuite* | (while read DIR ; do
   ls "$DIR"/badtest* | ( while read FILE; do
      if [ "$DIR" = "testsuite9_bourne" ] ; then
         bad_test_wparam "$FILE"
      else
         bad_test "$FILE"
      fi
      RESULT=$?
      if [ $RESULT -ne 0 ] ; then
         exit $RESULT
      fi
   done )
   RESULT=$?
   if [ $RESULT -ne 0 ] ; then
      exit $RESULT
   fi
done )
RESULT=$?
if [ $RESULT -ne 0 ] ; then
   exit $RESULT
fi

# Run test requiring --test mode

ls testmodesuite/badtest* | (while read FILE ; do
   bad_test_testmode "$FILE"
   RESULT=$?
   if [ $RESULT -ne 0 ] ; then
      exit $RESULT
   fi
done )

# Switch tests

setup
../spar -c
RESULT=$?
teardown
if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     exit 1
fi

echo
echo "CONGRATULATIONS! ALL TESTS WERE SUCCESSFUL"
exit 0
