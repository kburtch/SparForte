#!/bin/bash

bad_test() {
  echo "Running $1..."
  ../spar --debug "$1" < /dev/null > /dev/null
  RESULT=$?
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

bad_test_wparam() {
  echo "Running $1..."
  ../spar --debug "$1" a b c < /dev/null > /dev/null
  RESULT=$?
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

good_test() {
  echo "Running $1..."
  ../spar --debug "$1" a b c
  if [ $? -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     exit 1
  fi
  ../spar --debug -n "$1" a b c
  if [ $? -ne 0 ] ; then
     echo "--- $1 TEST FAILED - status code $? ---"
     exit 1
  fi
}

# BUSH TEST SUITE

if [ "$1" = "-h" ] ; then
   echo "Bush test suite"
   echo
   echo "usage: $0 [-b]"
   echo " -b - bad syntax tests only"
   exit
fi

test -f ./test.txt && rm ./test.txt

# Tar may not save these since they have no read permission
# Rebuild them as needed

if test ! -f ./write_only.txt ; then
   touch ./write_only.txt
   chmod 222 ./write_only.txt
fi

if test ! -f ./exec_only.txt ; then
   touch ./exec_only.txt
   chmod 111 ./exec_only.txt
fi

if [ "$1" != "-b" ] ; then
   good_test "goodtest.bush"
   good_test "goodtest2.bush"
fi
echo
echo "Testing bad scripts:"
bad_test "badtest1.bush"
bad_test "badtest2.bush"
bad_test "badtest3.bush"
bad_test "badtest4.bush"
bad_test "badtest5.bush"
bad_test "badtest6.bush"
bad_test "badtest7.bush"
bad_test "badtest8.bush"
bad_test "badtest9.bush"
bad_test "badtest10.bush"
bad_test "badtest11.bush"
bad_test "badtest12.bush"
bad_test "badtest13.bush"
bad_test "badtest14.bush"
bad_test "badtest15.bush"
bad_test "badtest16.bush"
bad_test "badtest17.bush"
bad_test "badtest18.bush"
bad_test "badtest19.bush"
bad_test "badtest20.bush"
bad_test "badtest21.bush"
bad_test "badtest22.bush"
bad_test "badtest23.bush"
bad_test "badtest24.bush"
bad_test "badtest25.bush"
bad_test "badtest26.bush"
bad_test "badtest27.bush"
bad_test "badtest28.bush"
bad_test "badtest29.bush"
bad_test "badtest30.bush"
bad_test "badtest31.bush"
bad_test "badtest32.bush"
bad_test "badtest33.bush"
bad_test "badtest34.bush"
bad_test "badtest35.bush"
bad_test "badtest36.bush"
bad_test "badtest37.bush"
bad_test "badtest38.bush"
bad_test "badtest39.bush"
bad_test "badtest40.bush"
bad_test "badtest41.bush"
bad_test "badtest42.bush"
bad_test "badtest43.bush"
bad_test "badtest44.bush"
bad_test "badtest45.bush"
bad_test "badtest46.bush"
bad_test "badtest47.bush"
bad_test "badtest48.bush"
bad_test "badtest49.bush"
bad_test "badtest50.bush"
bad_test "badtest51.bush"
bad_test "badtest52.bush"
bad_test "badtest53.bush"
bad_test "badtest54.bush"
bad_test "badtest55.bush"
bad_test "badtest56.bush"
bad_test "badtest57.bush"
bad_test "badtest58.bush"
bad_test "badtest59.bush"
bad_test "badtest60.bush"
bad_test "badtest61.bush"
bad_test "badtest62.bush"
bad_test "badtest63.bush"
bad_test "badtest64.bush"
bad_test "badtest65.bush"
bad_test "badtest66.bush"
bad_test "badtest67.bush"
bad_test "badtest68.bush"
bad_test "badtest69.bush"
bad_test "badtest70.bush"
bad_test "badtest71.bush"
bad_test "badtest72.bush"
bad_test "badtest73.bush"
bad_test "badtest74.bush"
bad_test "badtest75.bush"
bad_test "badtest76.bush"
bad_test "badtest77.bush"
bad_test "badtest78.bush"
bad_test "badtest79.bush"
bad_test "badtest80.bush"
bad_test "badtest81.bush"
bad_test "badtest82.bush"
bad_test "badtest83.bush"
bad_test "badtest84.bush"
bad_test "badtest85.bush"
bad_test "badtest86.bush"
bad_test "badtest87.bush"
bad_test "badtest88.bush"
bad_test "badtest89.bush"
bad_test "badtest90.bush"
bad_test "badtest91.bush"
bad_test "badtest92.bush"
bad_test "badtest93.bush"
bad_test "badtest94.bush"
bad_test "badtest95.bush"
bad_test "badtest96.bush"
bad_test "badtest97.bush"
bad_test "badtest98.bush"
bad_test "badtest99.bush"
bad_test "badtesta0.bush"
bad_test "badtesta1.bush"
bad_test "badtesta2.bush"
bad_test "badtesta3.bush"
bad_test "badtesta4.bush"
bad_test "badtesta5.bush"
bad_test "badtesta6.bush"
bad_test "badtesta7.bush"
bad_test "badtesta8.bush"
bad_test "badtesta9.bush"
bad_test "badtestb0.bush"
bad_test "badtestb1.bush"
bad_test "badtestb2.bush"
bad_test "badtestb3.bush"
bad_test "badtestb4.bush"
bad_test "badtestb5.bush"
bad_test "badtestb6.bush"
bad_test "badtestb7.bush"
bad_test "badtestb8.bush"
bad_test "badtestb9.bush"
bad_test "badtestc0.bush"
bad_test "badtestc1.bush"
bad_test "badtestc2.bush"
bad_test "badtestc3.bush"
bad_test "badtestc4.bush"
bad_test "badtestc5.bush"
bad_test "badtestc6.bush"
bad_test "badtestc7.bush"
bad_test "badtestc8.bush"
bad_test "badtestc9.bush"
bad_test "badtestd0.bush"
bad_test "badtestd1.bush"
bad_test "badtestd2.bush"
bad_test "badtestd3.bush"
bad_test "badtestd4.bush"
bad_test "badtestd5.bush"
bad_test "badtestd6.bush"
bad_test "badtestd7.bush"
bad_test "badtestd8.bush"
bad_test "badtestd9.bush"
bad_test "badteste0.bush"
bad_test "badteste1.bush"
bad_test "badteste2.bush"
bad_test "badteste3.bush"
bad_test "badteste4.bush"
bad_test "badteste5.bush"
bad_test "badteste6.bush"
bad_test "badteste7.bush"
bad_test "badteste8.bush"
bad_test "badteste9.bush"
bad_test "badtestf0.bush"
bad_test "badtestf1.bush"
bad_test "badtestf2.bush"
bad_test "badtestf3.bush"
bad_test "badtestf4.bush"
bad_test "badtestf5.bush"
bad_test "badtestf6.bush"
bad_test "badtestf7.bush"
bad_test "badtestf8.bush"
bad_test "badtestf9.bush"
bad_test "badtestg0.bush"
bad_test "badtestg1.bush"
bad_test "badtestg2.bush"
bad_test "badtestg3.bush"
bad_test "badtestg4.bush"
bad_test "badtestg5.bush"
bad_test "badtestg6.bush"
bad_test "badtestg7.bush"
bad_test "badtestg8.bush"
bad_test "badtestg9.bush"
bad_test "badtesth0.bush"
bad_test "badtesth1.bush"
bad_test "badtesth2.bush"
bad_test "badtesth3.bush"
bad_test "badtesth4.bush"
bad_test "badtesth5.bush"
bad_test "badtesth6.bush"
bad_test "badtesth7.bush"
bad_test "badtesth8.bush"
bad_test "badtesth9.bush"
bad_test "badtesti0.bush"
bad_test "badtesti1.bush"
bad_test "badtesti2.bush"
bad_test "badtesti3.bush"
bad_test "badtesti4.bush"
bad_test "badtesti5.bush"
bad_test "badtesti6.bush"
bad_test "badtesti7.bush"
bad_test "badtesti8.bush"
bad_test "badtesti9.bush"
bad_test "badtestj0.bush"
bad_test "badtestj1.bush"
bad_test "badtestj2.bush"
bad_test "badtestj3.bush"
bad_test "badtestj4.bush"
bad_test_wparam "badtestj5.bush"
bad_test_wparam "badtestj6.bush"
bad_test_wparam "badtestj7.bush"
bad_test "badtestj8.bush"
bad_test "badtestj9.bush"
bad_test "badtestk0.bush"
bad_test "badtestk1.bush"
bad_test "badtestk2.bush"
bad_test "badtestk3.bush"
bad_test "badtestk4.bush"
bad_test "badtestk5.bush"
bad_test "badtestk6.bush"
bad_test "badtestk7.bush"
bad_test "badtestk8.bush"
bad_test "badtestk9.bush"
bad_test "badtestl0.bush"
bad_test "badtestl1.bush"
bad_test "badtestl2.bush"
bad_test "badtestl3.bush"
bad_test "badtestl4.bush"
bad_test "badtestl5.bush"
bad_test "badtestl6.bush"
bad_test "badtestl7.bush"
bad_test "badtestl8.bush"
bad_test "badtestl9.bush"
bad_test "badtestm0.bush"
bad_test "badtestm1.bush"
bad_test "badtestm2.bush"
bad_test "badtestm3.bush"
bad_test "badtestm4.bush"
bad_test "badtestm5.bush"
bad_test "badtestm6.bush"
bad_test "badtestm7.bush"
bad_test "badtestm8.bush"
bad_test "badtestm9.bush"
bad_test "badtestn0.bush"
bad_test "badtestn1.bush"
bad_test "badtestn2.bush"
bad_test "badtestn3.bush"
bad_test "badtestn4.bush"
bad_test "badtestn5.bush"
bad_test "badtestn6.bush"
bad_test "badtestn7.bush"
bad_test "badtestn8.bush"
bad_test "badtestn9.bush"
bad_test "badtesto0.bush"
bad_test "badtesto1.bush"
bad_test "badtesto2.bush"
bad_test "badtesto3.bush"
bad_test "badtesto4.bush"
bad_test "badtesto5.bush"
bad_test "badtesto6.bush"
bad_test "badtesto7.bush"
bad_test "badtesto8.bush"
bad_test "badtesto9.bush"
bad_test "badtestp0.bush"
bad_test "badtestp1.bush"
bad_test "badtestp2.bush"
bad_test "badtestp3.bush"
bad_test "badtestp4.bush"
bad_test "badtestp5.bush"
bad_test "badtestp6.bush"
bad_test "badtestp7.bush"
bad_test "badtestp8.bush"
bad_test "badtestp9.bush"
bad_test "badtestq0.bush"
bad_test "badtestq1.bush"
bad_test "badtestq2.bush"
bad_test "badtestq3.bush"
bad_test "badtestq4.bush"
bad_test "badtestq5.bush"
bad_test "badtestq6.bush"
bad_test "badtestq7.bush"
bad_test "badtestq8.bush"
bad_test "badtestq9.bush"
bad_test "badtestr0.bush"
bad_test "badtestr1.bush"
bad_test "badtestr2.bush"
bad_test "badtestr3.bush"
bad_test "badtestr4.bush"
bad_test "badtestr5.bush"
bad_test "badtestr6.bush"
bad_test "badtestr7.bush"
bad_test "badtestr8.bush"
bad_test "badtestr9.bush"
bad_test "badtests0.bush"
bad_test "badtests1.bush"
bad_test "badtests2.bush"
bad_test "badtests3.bush"
bad_test "badtests4.bush"
bad_test "badtests5.bush"
bad_test "badtests6.bush"
bad_test "badtests7.bush"
bad_test "badtests8.bush"
bad_test "badtests9.bush"
bad_test "badtestt0.bush"
bad_test "badtestt1.bush"
bad_test "badtestt2.bush"
bad_test "badtestt3.bush"
bad_test "badtestt4.bush"
bad_test "badtestt5.bush"
bad_test "badtestt6.bush"
bad_test "badtestt7.bush"
bad_test "badtestt8.bush"
bad_test "badtestt9.bush"
bad_test "badtestu0.bush"
bad_test "badtestu1.bush"
bad_test "badtestu2.bush"
bad_test "badtestu3.bush"
bad_test "badtestu4.bush"
bad_test "badtestu5.bush"
bad_test "badtestu6.bush"
bad_test "badtestu7.bush"
bad_test "badtestu8.bush"
bad_test "badtestu9.bush"
bad_test "badtestv0.bush"
bad_test "badtestv1.bush"
bad_test "badtestv2.bush"
bad_test "badtestv3.bush"
bad_test "badtestv4.bush"
bad_test "badtestv5.bush"
bad_test "badtestv6.bush"
bad_test "badtestv7.bush"
bad_test "badtestv8.bush"
bad_test "badtestv9.bush"
bad_test "badtestw0.bush"
bad_test "badtestw1.bush"
bad_test "badtestw2.bush"
bad_test "badtestw3.bush"
bad_test "badtestw4.bush"
bad_test "badtestw5.bush"
bad_test "badtestw6.bush"
bad_test "badtestw7.bush"
bad_test "badtestw8.bush"
bad_test "badtestw9.bush"

# Switch tests

../spar -n
if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     exit 1
fi
../spar -c
if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     exit 1
fi

../spar -c -n goodtest.bush a b c
if [ $RESULT -eq 0 ] ; then
     echo
     echo "--- $1 TEST FAILED - status code $RESULT ---"
     echo "Test was:"
     cat "$1"
     exit 1
fi

# Remove stray temp files

rm test??????
test -f t.t && rm t.t

echo
echo "CONGRATULATIONS! ALL TESTS WERE SUCCESSFUL"
exit 0
