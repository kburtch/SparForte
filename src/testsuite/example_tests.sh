#!/bin/bash
#
# Test Some of the Example Programs
# ---------------------------------------------------------------------------

# Note: I don't use my regular SCRIPT variable as I wanted this as
# simple as possible to run on as many Bourne-compatible shells as
# possible.

cd ../..
echo "$0: Testing some example programs..."
echo "$0: "`date`

EXAMPLE="minimal.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=""
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="aplusb.sp"
RESULT=`echo "1 2" | src/spar --test examples/$EXAMPLE`
EXPECTED="1 2
 3"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="bases.sp"
RESULT=`src/spar --test examples/$EXAMPLE 11`
EXPECTED="hex=B
dec=11
oct=13
bin=1011"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="arraycat.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 1 2 3 4 5 6"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="arraycat_unstructured.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 1 2 3 4 5 6"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="arrayloop.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="aA1
bB2
cC3"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="arraysum.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 55
 3628800"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="arraysum_ss.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 55
 3628800"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="bitarith.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="A and B =  170
A or  B =  255
A xor B =  85

A << B =  256
A >> B =  64
A >>> B =  64
A rotl B =  256
A rotr B =  64"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="bottles.sp"
RESULT=`src/spar --test examples/$EXAMPLE | tail -3`
EXPECTED="Take one down, pass it around
 0 bottles of beer on the wall"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="calcrms.sp"
RESULT=`src/spar --test examples/$EXAMPLE | cut -c1-7`
EXPECTED=" 6.2048"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="charcode.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="character code 97 = character a
character a = character code 97"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="combinations.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 0 1 2
 0 1 3
 0 1 4
 0 2 3
 0 2 4
 0 3 4
 1 2 3
 1 2 4
 1 3 4
 2 3 4"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="distance.sp"
RESULT=`src/spar --test examples/$EXAMPLE <<HERE
1
2
3
4
HERE`
EXPECTED=" Starting Latitude: 1
Starting Longitude: 2
   Ending Latitude: 3
  Ending Longitude: 4

Distance between the two places is
  195.57030 miles"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="doors.sp"
RESULT=`src/spar --test examples/$EXAMPLE | tail -2`
EXPECTED=" 99 is Closed
 100 is Open"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="dotproduct.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 3"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="dynamic_css.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
RESULT=`echo "$RESULT" | tr -d '\n\r'`
EXPECTED="Content-type: text/cssbody {  margin: 0;  padding: 0;  background-color: #FFFFFF;  font: 12px arial;}"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="factorial.sp"
RESULT=`src/spar --test examples/$EXAMPLE 5`
EXPECTED=" 120"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="fibonacci.sp"
RESULT=`src/spar --test examples/$EXAMPLE 6`
EXPECTED=" 8"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="fizzbuzz.sp"
RESULT=`src/spar --test examples/$EXAMPLE | tail -n 3`
EXPECTED=" 98
Fizz
Buzz"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="gray.sp"
RESULT=`src/spar --test examples/$EXAMPLE | tail -n 3`
EXPECTED="    29 2#11101# 2#10011#      29
    30 2#11110# 2#10001#      30
    31 2#11111# 2#10000#      31"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="gss.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 3
 5
 6
-2
-1
 4"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="incstr.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="12346"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="int_sort.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 0
 1
 2
 3
 4
 5
 6
 7
 8"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="mapping.sp"
RESULT=`src/spar --test examples/$EXAMPLE | tail -n 4`
EXPECTED=" 9.00000000000000E+00 maps to: -1.00000000000000E-01
-1.00000000000000E-01 maps back to:  9.00000000000000E+00
 1.00000000000000E+01 maps to:  0.00000000000000E+00
 0.00000000000000E+00 maps back to:  1.00000000000000E+01"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="minsort.sp"
RESULT=`src/spar --test examples/$EXAMPLE <<HERE
1
3
2
HERE`
RESULT=`echo "$RESULT" | tail -n 2`
EXPECTED=" 2
 3"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="monte.sp"
RESULT=`src/spar --test examples/$EXAMPLE | tail -n 1 | cut -c1-16`
EXPECTED="  1_000_000: 3.1"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="monty.sp"
RESULT=`src/spar --test examples/$EXAMPLE | cut -d: -f1`
EXPECTED="Stay
Switch"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="neartime.sp"
RESULT=`src/spar --test examples/$EXAMPLE | cut -c1-4`
EXPECTED="It's"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="nonsquares.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="palindrome.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="this is a test : false
ablewasiereisawelba : true"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="perfect.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 6 : true
 18 : false
 28 : true"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="prime.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 6 : false
 7 : true
 8 : false"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="printargs.sp"
RESULT=`src/spar --test examples/$EXAMPLE 1 2 3`
EXPECTED="The command is 'src/spar'
Argument 1 is '1'
Argument 2 is '2'
Argument 3 is '3'"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="radices.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 180154659
 4009
 666
 4095"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

# This one is really slow.
#EXAMPLE="randdist.sp"
#RESULT=`src/spar examples/$EXAMPLE | tail -n 1 | cut -d. -f1`
#EXPECTED="heth  6"
#if [ "$RESULT" != "$EXPECTED" ] ; then
#   echo "Failed - $EXAMPLE Failed"
#   echo "$RESULT"
#   exit 192
#else
#   echo "OK - $EXAMPLE"
#fi

EXAMPLE="rle.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="rot13.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="Ovt swbeqf irk dhvpx jnygm alzcu!"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="sieve.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED=" 2
 3
 5
 7
 11
 13
 17
 19"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="sleep.sp"
RESULT=`echo "1" | src/spar --test examples/$EXAMPLE | tail -n 4`
EXPECTED="Sleeping...
Awake!
Sleeping...
Awake!"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="stringcase.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="ALPHABETA
alphabeta
Alphabeta"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="sumseries.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
echo "$RESULT"
EXPECTED="Sum of F(x) from 1 to 1000 is 1.64393456668165E+00"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="sumsq.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
echo "$RESULT"
EXPECTED=" 0
 133"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="ternary_logic.sp"
RESULT=`src/spar --test examples/$EXAMPLE | wc -l`
EXPECTED="27"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="time_function.sp"
RESULT=`src/spar --test examples/$EXAMPLE | cut -c1-6`
EXPECTED="sum(4)"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="tmpfile.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="Creating a temporary file
Reading a temporary file
File contains: Hello World
Discarding a temporary file"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="trig.sp"
RESULT=`src/spar --test examples/$EXAMPLE`
EXPECTED="Sin    7.07106781186547E-01 7.07106781186547E-01
Cos    7.07106781186547E-01 7.07106781186548E-01
Tan    1.00000000000000E+00 9.99999999999998E-01
Cot    1.00000000000000E+00 1.00000000000000E+00
Arcsin 4.50000000000000E+01 7.85398163397448E-01
Arccos 4.50000000000000E+01 7.85398163397448E-01
Arctan 45 7.85398163397448E-01
Arccot 45 7.85398163397449E-01"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="twelve_days.sp"
RESULT=`src/spar --test examples/$EXAMPLE | wc -l`
EXPECTED="114"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi

EXAMPLE="yorn.sp"
RESULT=`echo "y" | src/spar --test examples/$EXAMPLE`
echo "$RESULT"
EXPECTED="Your answer? (Y/N) Y"
if [ "$RESULT" != "$EXPECTED" ] ; then
   echo "Failed - $EXAMPLE Failed"
   echo "$RESULT"
   exit 192
else
   echo "OK - $EXAMPLE"
fi
# ---------------------------------------------------------------------------

echo "$0: "`date`
echo "EXAMPLE TESTS ARE OK"
exit 0

