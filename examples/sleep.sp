#!/usr/local/bin/bush

pragma annotate( "sleep_demo" );
pragma annotate( "" );
pragma annotate( "Write a program that does the following in this order:" );
pragma annotate( "" );
pragma annotate( "* Input an amount of time to sleep in whatever units are" );
pragma annotate( "most natural for your language (milliseconds, seconds," );
pragma annotate( "ticks, etc.). This unit should be noted in comments or" );
pragma annotate( "in a description." );
pragma annotate( "* Print 'Sleeping...'" );
pragma annotate( "* Sleep the main thread for the given amount of time." );
pragma annotate( "* Print 'Awake!'" );
pragma annotate( "* End." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Sleep" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

procedure sleep_demo is
  in_val : duration;
begin
  ? "Number of seconds to sleep?";
  in_val := numerics.value( get_line );

  -- Using delay
  ? "Sleeping...";
  delay in_val;
  ? "Awake!";

  -- Using Linux/UNIX sleep
  ? "Sleeping...";
  sleep "$in_val" ;
  ? "Awake!";
end sleep_demo;

