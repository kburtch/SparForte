#!/bin/bash
#
# Test Interfaces to Third-party software
# ---------------------------------------------------------------------------

# Note: I don't use my regular SCRIPT variable as I wanted this as
# simple as possible to run on as many Bourne-compatible shells as
# possible.

do_cgi_good_test() {
   # Fake a minimal CGI environment

   export GATEWAY_INTERFACE="CGI/1.1"
   export REQUEST_METHOD="GET"
   export QUERY_STRING="first_key=first value&second_key=second value"
   export REQUEST_URL="/example/path/cgi_test.html"
   export SERVER_NAME="example.com"
   export SCRIPT_NAME="/example/cgi_test.html"

   TESTSET="cgi_good_test.sp"
   if [ ! -f "$TESTSET" ] ; then
      echo "Failed - $TESTSET is missing"
   fi
   # Cookie string will be returned, so discard them if they exist.
   RESULT=`../../spar --test --debug ./$TESTSET 2>&1 | sed '/^Set-Cookie:/d'`
   if [ $? -ne 0 ] ; then
      echo "Failed - $TESTSET Failed"
      echo "$RESULT"
      exit 192
   # under Ubuntu 20.04, tput is producing error output on Jenkins because
   # there's no terminal.  These are breaking the tests.  Disabling the
   # no output tests for now.
   #elif [ -n "$RESULT" ] ; then
   #   echo "Failed - $TESTSET Failed"
   #   echo "$RESULT"
   #   exit 192
   else
      echo "OK - $TESTSET"
   fi

   unset REQUEST_METHOD
   unset GATEWAY_INTERFACE
   unset QUERY_STRING
   unset REQUEST_URL
   unset SERVER_NAME
   unset SCRIPT_NAME
}

do_btree_good_test() {
TESTSET="btree_good_test.sp"
if [ ! -f "$TESTSET" ] ; then
   echo "Failed - $TESTSET is missing"
fi
RESULT=`../../spar --test --debug ./$TESTSET 2>&1`
if [ $? -ne 0 ] ; then
   echo "Failed - $TESTSET Failed"
   echo "$RESULT"
   exit 192
elif [ -n "$RESULT" ] ; then
   echo "Failed - $TESTSET Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $TESTSET"
fi
}

do_hash_good_test() {
TESTSET="hash_good_test.sp"
if [ ! -f "$TESTSET" ] ; then
   echo "Failed - $TESTSET is missing"
fi
RESULT=`../../spar --test --debug ./$TESTSET 2>&1`
if [ $? -ne 0 ] ; then
   echo "Failed - $TESTSET Failed"
   echo "$RESULT"
   exit 192
elif [ -n "$RESULT" ] ; then
   echo "Failed - $TESTSET Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $TESTSET"
fi
}

do_memcache_good_test() {
TESTSET="memcache_good_test.sp"
if [ ! -f "$TESTSET" ] ; then
   echo "Failed - $TESTSET is missing"
fi
RESULT=`../../spar --test --debug ./$TESTSET 2>&1`
if [ $? -ne 0 ] ; then
   echo "Failed - $TESTSET Failed"
   echo "$RESULT"
   exit 192
elif [ -n "$RESULT" ] ; then
   echo "Failed - $TESTSET Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $TESTSET"
fi
}

# A minimal test of SDL using example programs in an
# X virtual frame buffer
# Doesn't check the output because "Press Return" is sent
# back.

do_sdl_xvfb_test() {
TESTSET="sdl_xvfb.sh"
if [ ! -f "$TESTSET" ] ; then
   echo "Failed - $TESTSET is missing"
fi
RESULT=`bash ./$TESTSET 2>&1`
if [ $? -ne 0 ] ; then
   echo "Failed - $TESTSET Failed"
   echo "$RESULT"
   exit 192
else
   #echo "$RESULT" # DEBUG
   echo "OK - $TESTSET"
fi
}

echo "$0: Testing thrid-party software..."
echo "$0: "`date`

cd third_party

do_cgi_good_test
# exit 192
do_btree_good_test
do_hash_good_test
do_memcache_good_test
do_sdl_xvfb_test

# ---------------------------------------------------------------------------

echo "$0: "`date`
echo "THIRD PARTY TESTS ARE OK"
exit 0

