#!/usr/local/bin/spar

pragma annotate( summary, "time_function" )
       @( description, "Write a program which uses a timer (with the least " )
       @( description, "granularity available on your system) to time how " )
       @( description, "long a function takes to execute." )
       @( see_also, "http://rosettacode.org/wiki/Time_a_function" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure time_function is

  procedure sample_function( num : in out integer ) is
  begin
    for i in 1..1000 loop
       num := @+1;
    end loop;
  end sample_function;

  start_time : calendar.time;
  end_time   : calendar.time;
  seconds    : duration;

  procedure time_sample_function is
    sample_param : integer := 4;
  begin
    start_time := calendar.clock;
    sample_function( sample_param );
    end_time := calendar.clock;
    seconds := end_time - start_time;
  end time_sample_function;

begin
  time_sample_function;
  put_line( "sum(4) takes:" & strings.image( seconds ) & " seconds." );
  command_line.set_exit_status( 0 );
end time_function;

-- VIM editor formatting instructions
-- vim: ft=spar

