#!/bin/bash
#
# Run SparForte SDL examples using an X-Windows frame buffer so as to test
# on systems without a desktop (e.g. Jenkins)
#
# Ken Burtch
# ----------------------------------------------------------------------------
shopt -s -o nounset

declare -i RESULT=0    # shell return status code
declare -i XVFB_PID=0  # Xvfb process id
# set +x

# Check to see if Xfvb is running on DISPLAY 99
# Otherwise, start it with the given display specifications and get the
# process id.

if [ -f "/tmp/.X99-lock" ] ; then
   echo "ERROR: Xvfb is running already"
   RESULT=1
else
   /usr/bin/Xvfb :99 -screen 0 1024x768x8 &
   if [ $RESULT -eq 0 ] ; then
      RESULT=$?
      if [ $RESULT -eq 0 ] ; then
         XVFB_PID=$!
         echo "Xvfb PID: $XVFB_PID"
      fi 
   fi
fi

# If Xvfb is running, run the example programs.  If any program should
# fail, skip the rest.  At the end, shut down the frame buffer.

if [ $XVFB_PID -ne 0 ] ; then
   echo "Running"
   export DISPLAY=":99"
   echo | ../../spar ../../../examples/chessboard
   if [ $RESULT -eq 0 ] ; then
       echo | ../../spar ../../../examples/mandel
   fi
   if [ $RESULT -eq 0 ] ; then
       echo | ../../spar ../../../examples/geometry
   fi
   if [ $RESULT -eq 0 ] ; then
       echo | ../../spar ../../../examples/opengl_first.sp
   fi
   if [ $RESULT -eq 0 ] ; then
       echo | ../../spar ../../../examples/opengl_second.sp
   fi
   if [ $RESULT -eq 0 ] ; then
      RESULT=$?
   fi
   echo "Shutting down Xvfb"
   kill $XVFB_PID
fi

# Return success or failure.

exit $RESULT


