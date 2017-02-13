#!/bin/bash
#
# Test Interfaces to Third-party software
# ---------------------------------------------------------------------------

# Note: I don't use my regular SCRIPT variable as I wanted this as
# simple as possible to run on as many Bourne-compatible shells as
# possible.

echo "$0: Testing thrid-party software..."
echo "$0: "`date`

cd third_party

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

# ---------------------------------------------------------------------------

echo "$0: "`date`
echo "THIRD PARTY TESTS ARE OK"
exit 0

