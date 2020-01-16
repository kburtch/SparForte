#!/bin/sh
#
# Run the SparForte regression tests.
# by Ken O. Burtch
#
# Warning: Control characters in this file may be corrupted if Git is used
# with auto eol conversion.
#-----------------------------------------------------------------------------

# Command line option flags
OPT_FAIL=
OPT_GPROF=

# Set to 1 if any test failed
HAD_FAILURE=

# Temporary files for capturing output
if [ -d "/dev/shm" ]; then
   TMP1="/dev/shm/runtests.tmp"
   TMP2="/dev/shm/runtests2.tmp"
else
   TMP1=`pwd`"/runtests.tmp"
   TMP2=`pwd`"/runtests2.tmp"
fi

# ---------------------------------------------------------------------------
# JUNIT Support
#
# These functions write a test report file in JUNIT format, JUNIT_RESULT.
#
# See
#   http://help.catchsoftware.com/display/ET/JUnit+Format
#-----------------------------------------------------------------------------

# The report file
JUNIT_RESULT=`pwd`"/junit_result.xml"

# The results of individual test cases before they are added to the report
if [ -d "/dev/shm" ]; then
   JUNIT_CASES="/dev/shm/junit_cases.xml"
else
   JUNIT_CASES=`pwd`"/junit_cases.xml"
fi

# Global JUNIT variables
JUNIT_OPEN=""
JUNIT_TEST_CNT=0
JUNIT_FAILURE_CNT=0

# JUNIT test case variables
JUNIT_CASE_OPEN=
JUNIT_CASE_NAME="undefined"
JUNIT_CASE_CLASS="undefined"
JUNIT_CASE_FILE="undefined"
JUNIT_CASE_LINE="undefined"
JUNIT_CASE_ASSERTION_CNT="undefined"
JUNIT_CASE_FAILURE_CNT="undefined"
JUNIT_CASE_ERROR_CNT="undefined"
JUNIT_CASE_START_TIME="undefined"
JUNIT_CASE_FAILURE_MSG=
JUNIT_CASE_ERROR_MSG=
JUNIT_CASE_SKIPPED=

# JUNIT test suite variables
JUNIT_SUITE_OPEN=
JUNIT_SUITE_NAME="undefined"
JUNIT_SUITE_FNAME="undefined"
JUNIT_SUITE_TEST_CNT="undefined"
JUNIT_SUITE_ASSERTION_CNT="undefined"
JUNIT_SUITE_FAILURE_CNT="undefined"
JUNIT_SUITE_ERROR_CNT="undefined"
JUNIT_SUITE_START_TIME="undefined"


# START JUNIT
#
# Start the JUNIT result file
# ---------------------------------------------------------------------------

start_junit() {
  JUNIT_OPEN=1
  JUNIT_CASE_OPEN=
  JUNIT_SUITE_OPEN=

  # Clear old files

  if test -f "$JUNIT_RESULT" ; then
     rm "$JUNIT_RESULT"
  fi
  #if test -f "$JUNIT_CASE_SUMMARY" ; then
  #   rm "$JUNIT_CASE_SUMMARY"
  #fi
  if test -f "$TMP1" ; then
     rm "$TMP1"
  fi
  if test -f "$TMP2" ; then
     rm "$TMP2"
  fi

  # Write header

  echo '<?xml version="1.0" encoding="UTF-8"?>'  > "$JUNIT_RESULT"
  echo '<testsuites>' >> "$JUNIT_RESULT"
}


# START JUNIT CASE
#
# Start a JUNIT test case
#
# $1 = the path the the file
# $2 = the class (i.e. the associated test suite)
# ---------------------------------------------------------------------------

start_junit_case() {
   JUNIT_CASE_NAME=`basename "$1"`
   JUNIT_CASE_CLASS="$2"
   JUNIT_CASE_FILE="$1"
   JUNIT_CASE_LINE=1
   JUNIT_CASE_ASSERTION_CNT=1
   JUNIT_CASE_FAILURE_CNT=0
   JUNIT_CASE_ERROR_CNT=0
   JUNIT_CASE_START_TIME=`date '+%s'`
   JUNIT_CASE_OPEN=1
   JUNIT_CASE_FAILURE_MSG=
   JUNIT_CASE_ERROR_MSG=
   JUNIT_CASE_SKIPPED=
}

# END JUNIT CASE
#
# Finish a JUNIT test case
#
# Example:
#      <testcase name="testBarChart_constructor" class="BarChartTest" file="/var/
#lib/jenkins/workspace/pallet_website_yii/tests/unit/BarChartTest.php" line="32" 
#assertions="1" time="0.015803"/>
# ---------------------------------------------------------------------------

end_junit_case() {
   # Increment grand total of tests
   let "JUNIT_TEST_CNT=JUNIT_TEST_CNT+1"

   # Adjust the test suite totals based on the case results
   let "JUNIT_SUITE_TEST_CNT=JUNIT_SUITE_TEST_CNT+JUNIT_CASE_TEST_CNT"
   let "JUNIT_SUITE_ASSERTION_CNT=JUNIT_SUITE_ASSERTION_CNT+JUNIT_CASE_ASSERTION_CNT"
   let "JUNIT_SUITE_FAILURE_CNT=JUNIT_SUITE_FAILURE_CNT+JUNIT_CASE_FAILURE_CNT"
   let "JUNIT_SUITE_ERROR_CNT=JUNIT_SUITE_ERROR_CNT+JUNIT_CASE_ERROR_CNT"
   # Calculate the time the tests took
   JUNIT_CASE_END_TIME=`date '+%s'`
   let "JUNIT_CASE_DURATION=JUNIT_CASE_END_TIME-JUNIT_CASE_START_TIME"
   # Write the test case with totals
   echo "  <testcase name=\"$JUNIT_CASE_NAME\" class=\"$JUNIT_CASE_CLASS\" file=\"$JUNIT_CASE_FILE\" line=\"$JUNIT_CASE_LINE\" assertions=\"$JUNIT_CASE_ASSERTION_CNT\" failures=\"$JUNIT_CASE_FAILURE_CNT\" errors=\"$JUNIT_CASE_ERROR_CNT\" time=\"$JUNIT_CASE_DURATION\">" >> "$JUNIT_CASES"
  if [ ! -z "$JUNIT_CASE_FAILURE_MSG" ] ; then
     echo "    <failure message=\"test failure\">$JUNIT_CASE_FAILURE_MSG</failure>" >> "$JUNIT_CASES"
  fi
  if [ ! -z "$JUNIT_CASE_ERROR_MSG" ] ; then
     echo "    <error message=\"test error\">$JUNIT_CASE_ERROR_MSG</error>" >> "$JUNIT_CASES"
  fi 
  if [ ! -z "$JUNIT_CASE_SKIPPED" ] ; then
     echo "    <skipped />" >> "$JUNIT_CASES"
  fi 
  # TODO: skipped, error
  echo "  </testcase>" >> "$JUNIT_CASES"
  JUNIT_CASE_OPEN=
}

# START JUNIT SUITE
#
# Start a JUNIT test suite
#
# $1 = the name of the test suite
# $2 = the path to the file
# ---------------------------------------------------------------------------

start_junit_suite() {
   JUNIT_SUITE_OPEN=1
   JUNIT_SUITE_NAME="$1"
   JUNIT_SUITE_FNAME="$2"
   JUNIT_SUITE_TEST_CNT=0
   JUNIT_SUITE_ASSERTION_CNT=0
   JUNIT_SUITE_FAILURE_CNT=0
   JUNIT_SUITE_ERROR_CNT=0
   JUNIT_SUITE_START_TIME=`date '+%s'`
#   if test -f "$JUNIT_SUITE_RESULT" ; then
#      rm "$JUNIT_SUITE_RESULT"
#   fi
   # Clear any old test cases
   if test -f "$JUNIT_CASES" ; then
      rm "$JUNIT_CASES"
   fi
}

# END JUNIT SUITE
#
# Finish a JUNIT test suite
#
# Example:
#    <testsuite name="BarChartTest" file="/var/lib/jenkins/workspace/pallet_websi
# te_yii/tests/unit/BarChartTest.php" tests="47" assertions="83" failures="0" erro
# rs="0" time="0.294474">
# ---------------------------------------------------------------------------

end_junit_suite() {
   JUNIT_SUITE_OPEN=

   # Calculate the time to run the suite
   JUNIT_SUITE_END_TIME=`date '+%s'`
   let "JUNIT_SUITE_DURATION=JUNIT_SUITE_END_TIME-JUNIT_SUITE_START_TIME"

   # Display the suite summary with the individual test cases
   echo "<testsuite name=\"$JUNIT_SUITE_NAME\" file=\"$JUNIT_SUITE_FNAME\" tests=\"$JUNIT_SUITE_TEST_CNT\" assertions=\"$JUNIT_SUITE_ASSERTION_CNT\" failures=\"$JUNIT_SUITE_FAILURE_CNT\" errors=\"$JUNIT_SUITE_ERROR_CNT\" time=\"$JUNIT_SUITE_DURATION\">" >> "$JUNIT_RESULT"
   if [ -f "$JUNIT_CASES" ] ; then
      cat "$JUNIT_CASES" >> "$JUNIT_RESULT"
      rm "$JUNIT_CASES"
   fi
   echo "</testsuite>" >> "$JUNIT_RESULT"
}

# END JUNIT
#
# Finish the JUNIT result file
# ---------------------------------------------------------------------------

end_junit() {
  if [ ! -z "$JUNIT_CASE_OPEN" ] ; then
     end_junit_case
  fi
  if [ ! -z "$JUNIT_SUITE_OPEN" ] ; then
     end_junit_suite
  fi
  if [ ! -z "$JUNIT_OPEN" ] ; then
     echo '</testsuites>' >> "$JUNIT_RESULT"
     JUNIT_OPEN=
  fi
  # Cleanup temp files
  if test -f "$TMP1" ; then
     rm "$TMP1"
  fi
  if test -f "$TMP2" ; then
     rm "$TMP2"
  fi
}

# JUNIT FAIL
#
# Record a test failed with an error
# ---------------------------------------------------------------------------

junit_fail() {
  HAD_ERRORS=1
  if [ ! -z "$JUNIT_CASE_OPEN" ] ; then
     let "JUNIT_FAILURE_CNT=JUNIT_FAILURE_CNT+1"
     let "JUNIT_CASE_FAILURE_CNT=JUNIT_CASE_FAILURE_CNT+1"
     if [ ! -z "$1" ] ; then
        JUNIT_CASE_FAILURE_MSG="$1"
     else
        JUNIT_CASE_FAILURE_MSG="undefined failure"
     fi
  fi
}

junit_fail_core_dump() {
  junit_fail "CORE DUMP"
}

junit_fail_exception() {
  junit_fail "EXCEPTION - $1"
}

junit_fail_bad_status() {
  junit_fail "Bad status $1"
}

# JUNIT ERROR
#
# There was an error running the test
# ---------------------------------------------------------------------------

junit_skipped() {
  HAD_ERRORS=1
  if [ ! -z "$1" ] ; then
     JUNIT_CASE_ERROR_MSG="$1"
  else
     JUNIT_CASE_ERROR_MSG="undefined error"
  fi
}

# JUNIT SKIPPED
#
# Record a test was skipped
# ---------------------------------------------------------------------------

junit_skipped() {
   JUNIT_CASE_SKIPPED=1
}

# ---------------------------------------------------------------------------
# Test Helper Functions
# ---------------------------------------------------------------------------


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
  start_junit_case "$1" "bad_test"
  setup
  OUTPUT=`../spar --debug "$1" < /dev/null 2>&1`
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
     junit_fail_core_dump
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  TMP=`echo "$OUTPUT" | fgrep "_ERROR"`
  if [ ! -z "$TMP" ] ; then
     echo
     echo "--- $1 FAILED WITH ADA EXCEPTION ---"
     echo "Test was:"
     cat "$1"
     echo "Output was:"
     echo "$OUTPUT"
     junit_fail_exception "$OUTPUT"
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
}

#  BAD TEST
#
# Run the test script with parameters.  Declare FOOBAR.  Pass 3 parameters
# to the script.  If there's a core dump or a success code is returned, abort
# with the error.
# ---------------------------------------------------------------------------

bad_test_wparam() {
  echo "Running $1..."
  start_junit_case "$1" "bad_test_wparam"
  setup
  OUTPUT=`../spar --debug "$1" a b c < /dev/null 2>&1`
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
     junit_fail_core_dump
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  TMP=`echo "$OUTPUT" | fgrep "_ERROR"`
  if [ ! -z "$TMP" ] ; then
     echo
     echo "--- $1 FAILED WITH ADA EXCEPTION ---"
     echo "Test was:"
     cat "$1"
     echo "Output was:"
     echo "$OUTPUT"
     junit_fail_core_dump
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
}

#  BAD TEST TESTMODE
#
# Run the test script with no parameters is SparForte testing mode.
# Declare FOOBAR.  If there's a core dump or a success code is returned,
# abort with the error.
# ---------------------------------------------------------------------------

bad_test_testmode() {
  echo "Running $1..."
  start_junit_case "$1" "bad_test_testmode"
  setup
  OUTPUT=`../spar --debug --test "$1" < /dev/null 2>&1`
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
     junit_fail_core_dump
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  TMP=`echo "$OUTPUT" | fgrep "_ERROR"`
  if [ ! -z "$TMP" ] ; then
     echo
     echo "--- $1 FAILED WITH ADA EXCEPTION ---"
     echo "Test was:"
     cat "$1"
     echo "Output was:"
     echo "$OUTPUT"
     junit_fail_core_dump
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
}

#  BAD TEST GCC ERRORS
#
# Run the test script with no parameters is SparForte testing mode.
# Declare FOOBAR.  If there's a core dump or a success code is returned,
# abort with the error.
# ---------------------------------------------------------------------------

bad_test_gcc_errors() {
  echo "Running $1..."
  start_junit_case "$1" "bad_test_gcc_errors"
  setup
  OUTPUT=`../spar --debug -gcc-errors "$1" < /dev/null 2>&1`
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
     junit_fail_core_dump
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  TMP=`echo "$OUTPUT" | fgrep "_ERROR"`
  if [ ! -z "$TMP" ] ; then
     echo
     echo "--- $1 FAILED WITH ADA EXCEPTION ---"
     echo "Test was:"
     echo "Output was:"
     echo "$OUTPUT"
     cat "$1"
     junit_fail_core_dump
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  TMP=`echo "$OUTPUT" | fgrep "in script"`
  if [ ! -z "$TMP" ] ; then
     echo
     echo "--- $1 TEST FAILED - not in GCC error format ---"
     echo "Test was:"
     cat "$1"
     junit_fail "not in GCC error format"
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
echo "OK"
}

#  GOOD TEST
#
# Run the test script with no parameters.  Declare FOOBAR.  If there's a
# non-success code is returned, abort with the error.  Do it with syntax-only
# and normally.
# ---------------------------------------------------------------------------

good_test() {
  echo "Running $1..."
  # We don't really have assertions for the good tests, so treat all
  # lines as assertions.
  start_junit_case "$1_check" "good_test"
  JUNIT_CASE_ASSERTION_CNT=`wc -l < "$1"`
  setup
  ../spar --coding --debug --check "$1" a b c
  RESULT=$?
  teardown
  test -f ./test.txt && rm ./test.txt
  if [ $RESULT -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
  start_junit_case "$1" "good_test"
  JUNIT_CASE_ASSERTION_CNT=`wc -l < "$1"`
  setup
  ../spar --coding --debug "$1" a b c
  RESULT=$?
  teardown
  test -f ./test.txt && rm ./test.txt
  if [ $RESULT -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
  if [ ! -z "$OPT_GPROF" ] ; then
     echo "gprof profile of goodtest.sp"
     gprof -p ../spar
  fi
}

#  GOOD TEST IN DIR
#
# Run the test script with no parameters.  Declare FOOBAR.  If there's a
# non-success code is returned, abort with the error.  Do it with syntax-only
# and normally.  Unlike regular good_test, this assumes you are in the
# subdirectory...this is necessary from some of the "separate" tests.
# ---------------------------------------------------------------------------

good_test_in_dir() {
  echo "Running $1..."
  # We don't really have assertions for the good tests, so treat all
  # lines as assertions.
  start_junit_case "$1_check" "good_test_in_dir"
  JUNIT_CASE_ASSERTION_CNT=`wc -l < "$1"`
  #setup
  ../../spar --coding --debug --check "$1" a b c
  RESULT=$?
  #teardown
  test -f ./test.txt && rm ./test.txt
  if [ $RESULT -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
  start_junit_case "$1" "good_test_in_dir"
  JUNIT_CASE_ASSERTION_CNT=`wc -l < "$1"`
  #setup
  ../../spar --coding --debug "$1" a b c
  RESULT=$?
  #teardown
  test -f ./test.txt && rm ./test.txt
  if [ $RESULT -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
}

#  GOOD TEST TEST
#
# Run the test script with no parameters.  Declare FOOBAR.  If there's a
# non-success code is returned, abort with the error.  Do it with syntax-only
# and normally.  Unlike regular good_test, this runs in testing mode.
# ---------------------------------------------------------------------------

good_test_test() {
  echo "Running $1..."
  # We don't really have assertions for the good tests, so treat all
  # lines as assertions.
  start_junit_case "$1_check" "test_test"
  JUNIT_CASE_ASSERTION_CNT=`wc -l < "$1"`
  #setup
  ../../spar --test --debug --check "$1" a b c
  RESULT=$?
  #teardown
  test -f ./sparforte_test.xml && rm ./sparforte_test.xml
  if [ $RESULT -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
  start_junit_case "$1" "test_test"
  JUNIT_CASE_ASSERTION_CNT=`wc -l < "$1"`
  #setup
  ../../spar --test --debug "$1" a b c
  RESULT=$?
  #teardown
  test -f ./sparforte_test.xml && rm ./sparforte_test.xml
  if [ $RESULT -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
}

#  HELP TEST IN DIR
#
# Run the test script with no parameters.  Declare FOOBAR.  If there's a
# non-success code is returned, abort with the error.  Do it with syntax-only
# and normally.  Unlike regular good_test, this assumes you are in the
# subdirectory...this is necessary from some of the "separate" tests.
# ---------------------------------------------------------------------------

help_test_in_dir() {
  echo "Running $1..."
  start_junit_case "$1" "help_test_in_dir"
  ../../spar --coding --debug "$1" a b c > "./test.txt"
  RESULT=$?
  if [ $RESULT -ne 0 ] ; then
     cat "./test.txt"
  fi 
  test -f "./test.txt" && rm "./test.txt"
  if [ $RESULT -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
  fi
  end_junit_case
}

# TEST TEMPLATE
#
# Test a pragma template template.
# ---------------------------------------------------------------------------

test_template() {
  echo "$1 ..."
  start_junit_case "$1" "test_template"
  TMP=`../../spar --coding $1`
  STATUS=$?
  RESULT=$STATUS
  if [ $STATUS -ne 0 ] ; then
     echo "template $1 failed - status code $STATUS"
     junit_fail_bad_status $STATUS
  elif [ "$TMP" != "$2" ] ; then
     echo "expected output did not match actual output"
     echo "expected output '$2'"
     echo "actual output   '$TMP'"
     junit_fail "expected output did not match - '$2' vs '$TMP'"
     RESULT=1
  fi
  end_junit_case
  if [ $RESULT -ne 0 ] ; then
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit $RESULT
     fi
  fi
  return $RESULT
}


# ---------------------------------------------------------------------------
# SPARFORTE TEST SUITE
# ---------------------------------------------------------------------------

if [ "$1" = "-h" ] ; then
   echo "SparForte test suite"
   echo
   echo "usage: $0 [-b|-f|-g]"
   echo " -b - bad syntax tests only"
   echo " -f - fail on first error"
   echo " -g - gprof profiling results"
   exit 0
fi
if [ "$1" = "-f" ] ; then
   OPT_FAIL=1
elif [ "$1" = "-g" ] ; then
   OPT_GPROF=1
elif [ "$1" != "-b" ] ; then
   echo "Unexpected switch $1"
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

start_junit

# Run the good tests (if not -b (bad-only)

if [ "$1" != "-b" ] ; then
   # goodtest.bush checks itself for read-only
   chmod -w goodtest.sp
   start_junit_suite "good_tests" "goodtest.sp"
   good_test "goodtest.sp"
   good_test "goodtest2.sp"
   end_junit_suite
fi

echo
echo "Testing unit testing..."

cd testtests

good_test_test "goodtest200_unittest.sp"
good_test_test "goodtest201_unittest_junit.sp"
good_test_test "badtest300_unittest.sp"
good_test_test "badtest301_unittest_json.sp"

echo
echo "Testing web templates..."

cd ..
cd templates

start_junit_suite "test_template"  "goodtest.sp"

# Warning: Control characters in EXPECTED assignment
EXPECTED="Status: 200 OK
Content-type: text/html

OK"

test_template "template1" "$EXPECTED"
test_template "template2" "$EXPECTED"
test_template "template3" "$EXPECTED"

# status code output suppression

# Warning: Control characters in EXPECTED assignment
EXPECTED="Status: 200 OK
Content-type: text/html
"

test_template "template4" "$EXPECTED"

# Warning: Control characters in EXPECTED assignment
EXPECTED="Status: 200 OK
Content-type: text/html

body {
  font: 12px arial;
}"

test_template "template5" "$EXPECTED"

# Warning: Control characters in EXPECTED assignment
EXPECTED="Status: 200 OK
Content-type: text/html

{"'"'"message"'"'":"'"'"OK"'"'"}"

test_template "template6" "$EXPECTED"

# Warning: Control characters in EXPECTED assignment
EXPECTED="Status: 200 OK
Content-type: text/html

OK"

test_template "template7" "$EXPECTED"

EXPECTED="Status: 404 Not Found
Content-type: text/html
Location: redirect.html

OK"

test_template "template8" "$EXPECTED"
end_junit_suite

cd - 2>/dev/null

# Search helpsuite* directories and run bad tests stored there
#
# These are for code coverage.

echo
echo "Testing built-in help scripts:"

start_junit_suite "help_test_in_dir" "run_tests.sp"

ls -d helpsuite* > "$TMP1"
while read DIR ; do
   cd "$DIR"
   ls helptest* > "$TMP2"
   while read FILE; do
      help_test_in_dir "$FILE"
      RESULT=$?
      if [ $RESULT -ne 0 ] ; then
         if [ ! -z "$OPT_FAIL" ] ; then
            end_junit
            exit $RESULT
         fi
      fi
   done  < "$TMP2"
   RESULT=$?
   if [ $RESULT -ne 0 ] ; then
      end_junit
      exit $RESULT
   fi
   cd - 2>/dev/null
done < "$TMP1"
RESULT=$?
if [ $RESULT -ne 0 ] ; then
   if [ ! -z "$OPT_FAIL" ] ; then
      end_junit
      exit $RESULT
   fi
fi
end_junit_suite

# Search goodsuite* directories and run bad tests stored there
#
# These good tests normally test different structures such as
# include files.  We need to change to the directory first.

echo
echo "Testing good suite scripts:"

start_junit_suite "good_test_in_dir" "run_tests.sp"

ls -d goodsuite* > "$TMP1"
while read DIR ; do
   cd "$DIR"
   ls goodtest* > "$TMP2"
   while read FILE; do
      good_test_in_dir "$FILE"
      RESULT=$?
      if [ $RESULT -ne 0 ] ; then
         if [ ! -z "$OPT_FAIL" ] ; then
            end_junit
            exit $RESULT
         fi
      fi
   done < "$TMP2"
#   RESULT=$?
#   if [ $RESULT -ne 0 ] ; then
#      end_junit
#      exit $RESULT
#   fi
   cd - 2>/dev/null
done < "$TMP1"
#RESULT=$?
#if [ $RESULT -ne 0 ] ; then
#   if [ ! -z "$OPT_FAIL" ] ; then
#      end_junit
#      exit $RESULT
#   fi
#fi
end_junit_suite

# Search testsuite* directories and run bad tests stored there

echo
echo "Testing bad scripts:"

ls -d testsuite* > "$TMP1"
while read DIR ; do
   start_junit_suite "$DIR" "run_tests.sp"
   ls "$DIR"/badtest* > "$TMP2"
   while read FILE; do
      if [ "$DIR" = "testsuite9_bourne" ] ; then
         bad_test_wparam "$FILE"
      else
         bad_test "$FILE"
      fi
      RESULT=$?
      if [ $RESULT -ne 0 ] ; then
         if [ ! -z "$OPT_FAIL" ] ; then
            end_junit
            exit $RESULT
         fi
      fi
   done < "$TMP2"
   #RESULT=$?
   end_junit_suite
   #if [ $RESULT -ne 0 ] ; then
   #   if [ ! -z "$OPT_FAIL" ] ; then
   #      end_junit
   #      exit $RESULT
   #   fi
   #fi
done < "$TMP1"
#RESULT=$?
#if [ $RESULT -ne 0 ] ; then
#   junit_fail
#   if [ ! -z "$OPT_FAIL" ] ; then
#      end_junit
#      exit $RESULT
#   fi
#fi

# Run test requiring --test mode

start_junit_suite "bad_test_testmode" "run_tests.sp"
ls testmodesuite/badtest* > "$TMP1"
while read FILE ; do
   bad_test_testmode "$FILE"
   RESULT=$?
   if [ $RESULT -ne 0 ] ; then
      if [ ! -z "$OPT_FAIL" ] ; then
         end_junit
         exit $RESULT
      fi
   fi
done < "$TMP1"
end_junit_suite

# Run test requiring --gcc-errors mode

start_junit_suite "bad_test_gcc_errors" "run_tests.sp"
ls testgccerrsuite/badtest* > "$TMP1"
while read FILE ; do
   bad_test_gcc_errors "$FILE"
   RESULT=$?
   if [ $RESULT -ne 0 ] ; then
      if [ ! -z "$OPT_FAIL" ] ; then
         end_junit
         exit $RESULT
      fi
   fi
done < "$TMP1"
end_junit_suite

# Switch tests

start_junit_suite "switch_tests" "run_tests.sp"
start_junit_case "minus_c_switch" "switch_tests"
setup
../spar -c
RESULT=$?
teardown
if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- minus_c_switch TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
fi
echo "OK"
end_junit_case
start_junit_case "minus_h_switch" "switch_tests"
setup
../spar -h
RESULT=$?
teardown
if [ $RESULT -ne 0 ] ; then
     echo
     echo "--- minus_h_switch TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
fi
echo "OK"
end_junit_case
start_junit_case "minus_V_switch" "switch_tests"
setup
../spar -V
RESULT=$?
teardown
if [ $RESULT -ne 0 ] ; then
     echo
     echo "--- minus_V_switch TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
fi
echo "OK"
end_junit_case
start_junit_case "minus_L_switch" "switch_tests"
setup
../spar -L libtest libtest.sp
RESULT=$?
teardown
if [ $RESULT -ne 0 ] ; then
     echo
     echo "--- minus_L_switch TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
fi
echo "OK"
end_junit_case
start_junit_case "minus_L_switch" "switch_tests"
setup
../spar -L foobar -L libtest libtest.sp
RESULT=$?
teardown
if [ $RESULT -ne 0 ] ; then
     echo
     echo "--- minus_L_switch TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
fi
echo "OK"
end_junit_case
start_junit_case "minus_L_switch" "switch_tests"
setup
../spar -L libtest -L foobar libtest.sp
RESULT=$?
teardown
if [ $RESULT -ne 0 ] ; then
     echo
     echo "--- minus_L_switch TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     junit_fail_bad_status $RESULT
     if [ ! -z "$OPT_FAIL" ] ; then
        end_junit
        exit 1
     fi
fi
end_junit_case
end_junit_suite

# TODO: this must be done any any exit
end_junit

echo
if [ -z "$HAD_FAILURE" ] ; then
   echo "CONGRATULATIONS! ALL $JUNIT_TEST_CNT TESTS WERE SUCCESSFUL"
   exit 0
else
   echo "THERE WERE $JUNIT_FAILURE_CNT ERRORS."
   exit 1
fi

