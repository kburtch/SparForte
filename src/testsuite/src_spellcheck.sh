#!/bin/sh

# Spell check comments in a SparForte Ada source file
# Ken O. Burtch
# January 2025
#
# The aspell command is very difficult to work with.  It is intended
# to work interactively and non-interactive is only available with
# ispell backwards compatibility (pipe mode).  The personal dictionary
# has a specific format and must be located in the home_dir.  The
# output is ispell compatibility.

# Additionally, it is difficult to recognize commented out code from
# genuine comments.  Comments often refer to functions or variables.
# Only the first suspicious spelling on a line is reported.
#
# Documentation can be checked (from the doc/ directory with:
# aspell --mode=html --home-dir=../src/testsuite --personal='aspell_dict.pws'
#   check file.html

TMP="/tmp/spellcheck.$$"
TMP2="/tmp/spellcheck2.$$"
PERSONAL_DICT="aspell_dict.pws"

if [ ! -r "$PERSONAL_DICT" ] ; then
   echo "$0: Cannot read personal dict '$PERSONAL_DICT'"
   exit 192
fi

SOURCE_FILE="$1"

cd ..

if [ ! -r "$SOURCE_FILE" ] ; then
   echo "$0: Cannot read source file '$SOURCE_FILE'"
   exit 192
fi

echo "Spell checking '$SOURCE_FILE' ..."

grep -FHn -- '--' "$SOURCE_FILE" > "$TMP"

( CNT=0 ; while read LINE; do
   # Split the line into location and text
   LOCATION=`echo "$LINE" | cut -d: -f1-2`
   ORIGINAL_TEXT=`echo "$LINE" | cut -d: -f3-`


   # Remove everything up to the comment symbol
   TEXT=`echo "$ORIGINAL_TEXT" | sed 's/^.*--//g'`

   # Skip lines that look like Ada source code, ada.text_io
   # often commented out but left in place for debugging,  or
   # parser syntax descriptions.
   GREP_RESULT=`echo "$LINE" | grep -F "put_line"`
   if [ -z "$GREP_RESULT" ] ; then
      GREP_RESULT=`echo "$LINE" | grep -F ":="`
   fi
   if [ -z "$GREP_RESULT" ] ; then
      GREP_RESULT=`echo "$LINE" | grep -F "DEBUG"`
   fi
   if [ -z "$GREP_RESULT" ] ; then
      GREP_RESULT=`echo "$LINE" | grep -F "ada.text_io"`
   fi
   if [ -z "$GREP_RESULT" ] ; then
      GREP_RESULT=`echo "$LINE" | grep -F "Syntax: "`
   fi
   if [ -z "$GREP_RESULT" ] ; then
      GREP_RESULT=`echo "$LINE" | grep -F "CO"`  # Commented out
   fi

   if [ -z "$GREP_RESULT" ] ; then
      echo "$TEXT" > "$TMP2"
      ASPELL_RESULT=`aspell --home-dir="testsuite/" --personal="$PERSONAL_DICT" pipe < "$TMP2" 2>/dev/null | grep '^\&' | cut -d' ' -f2 | sed 's/\ \&//g'`
      rm "$TMP2"
      if [ -n "$ASPELL_RESULT" ] ; then
         CNT=$((CNT+1))
         printf "%5d %-20s %-15s %s" "$CNT" "$LOCATION" "$ASPELL_RESULT" "$ORIGINAL_TEXT"
         echo
      fi
   fi
done ) < "$TMP"

# Cleanup
echo "Done"
rm "$TMP"
if [ -f "$TMP2" ] ; then
   rm "$TMP2"
fi

