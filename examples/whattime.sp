#!/usr/local/bin/spar

pragma annotate( summary, "usage: whattime [timezone]" )
       @( description, "Show the time for different time zones" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

procedure whattime is

  procedure usage is
  begin
     put( "usage: " ) @ ( source_info.file );
     put_line( " [timezone]" );
     command_line.set_exit_status( 0 );
  end usage;

  default_TZ : constant array( 1..6 ) of string := (
      "Europe/Dublin", "Europe/Berlin", "US/Eastern", "US/Central",
      "US/Mountain", "US/Pacific" );
  -- default time zones to display

  TZ : string;
  pragma export( shell, TZ );
  -- shell variable containing timezone, by convention, for date

begin

  -- Usage

  if $# > 1 then
     usage;
     return;
  elsif $# = 1 then
     if $1 = "-h" or $1 = "--help" then
        usage;
        return;
     end if;
  end if;

  -- If a timezone was given, use it

  if $# = 1 then
    TZ := $1;
    date;
  else

  -- Otherwise, show the time in the default time zones

    for i in arrays.first( default_TZ )..arrays.last( default_TZ ) loop
       TZ := default_TZ( i );
       put( TZ ) @ ( ( (20 -strings.length( TZ ) ) * ' ' ) & ": " );
       date;
    end loop;
  end if;

  command_line.set_exit_status( 0 );

end whattime;

-- VIM editor formatting instructions
-- vim: ft=spar

