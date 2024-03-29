#!/usr/local/bin/spar

pragma annotate( summary, "sleep_demo" );
pragma annotate( description, "Write a program that does the following in this order:" );
pragma annotate( description, "" );
pragma annotate( description, "* Input an amount of time to sleep in whatever units are" );
pragma annotate( description, "most natural for your language (milliseconds, seconds," );
pragma annotate( description, "ticks, etc.). This unit should be noted in comments or" );
pragma annotate( description, "in a description." );
pragma annotate( description, "* Print 'Sleeping...'" );
pragma annotate( description, "* Sleep the main thread for the given amount of time." );
pragma annotate( description, "* Print 'Awake!'" );
pragma annotate( description, "* End." );
pragma annotate( category, "scripting" );
pragma annotate( see_also, "http://rosettacode.org/wiki/Sleep" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma software_model( nonstandard );

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

-- VIM editor formatting instructions
-- vim: ft=spar

