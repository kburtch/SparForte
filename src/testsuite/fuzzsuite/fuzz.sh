#!/bin/bash
#
# Very rudimentary fuzzing test to inject random data into SparForte.
# May 11, 2017
# Ken Burtch
# ----------------------------------------------------------------------------

ASCII[0]="\000"
ASCII[1]="\001"
ASCII[2]="\002"
ASCII[3]="\003"
ASCII[4]="\004"
ASCII[5]="\005"
ASCII[6]="\006"
ASCII[7]="\007"
ASCII[8]="\010"
ASCII[9]="\011"
ASCII[10]="\012"
ASCII[11]="\013"
ASCII[12]="\014"
ASCII[13]="\015"
ASCII[14]="\016"
ASCII[15]="\017"
ASCII[16]="\020"
ASCII[17]="\021"
ASCII[18]="\022"
ASCII[19]="\023"
ASCII[20]="\024"
ASCII[21]="\025"
ASCII[22]="\026"
ASCII[23]="\027"
ASCII[24]="\030"
ASCII[25]="\031"
ASCII[29]="\032"
ASCII[30]="\033"
ASCII[31]="\034"
ASCII[32]=" "
ASCII[33]="!"
ASCII[34]="\""
ASCII[35]="#"
ASCII[36]="$"
ASCII[37]="%"
ASCII[38]="&"
ASCII[39]="'"
ASCII[40]="("
ASCII[41]=")"
ASCII[42]="*"
ASCII[43]="+"
ASCII[44]=","
ASCII[45]="-"
ASCII[46]="."
ASCII[47]="/"

# spar -e test: empty string

STR=''
../../spar -e "$STR"

# spar -e test: single characters

( CNT=0
  while [ "$CNT" -le 47 ] ; do
     STR=`echo -e "${ASCII[$CNT]}"`
     OUTPUT=`../../spar -e "$STR" 2>&1`
     STATUS=$?
     if [ "$CNT" -eq 45 ] ; then
        # a minus, then we expect option missing
        EXPECTED=1
     else
        EXPECTED=192
     fi
     if [ "$STATUS" -ne "$EXPECTED" ] ; then
        echo "Test failed: expected status $EXPECTED but got $STATUS"
        echo "$OUTPUT"
        exit 192
     fi
     let "CNT++"
  done
)
echo "Fuzz Test OK"
exit 0


