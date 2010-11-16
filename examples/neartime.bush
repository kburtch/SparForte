#!/usr/local/bin/bush

-- neartime
--
-- Display the time in a "relaxed" format
-- based on http://www.zazzybob.com/bin/neartime.html
-- Created by Ken O. Burtch

pragma annotate( "neartime" );
pragma annotate( "" );
pragma annotate( "Display the time in a 'relaxed' format" );
pragma annotate( "usage: neartime" );

pragma restriction( no_external_commands );

procedure neartime is

  procedure usage is
  begin
     put( "usage: " ) @ ( source_info.file );
     command_line.set_exit_status( 0 );
  end usage;

  now    : calendar.time := calendar.clock;
  hour   : natural := numerics.floor( calendar.seconds( now ) / 3600 );
  minute : natural := numerics.floor( ( calendar.seconds( now ) - hour * 3600 ) / 60 );

  nearbefore : string;
  nearafter  : string;
  nearhour   : string;
  nearampm   : string;

begin

  -- Usage

  if $# /= 0 then
     usage;
     return;
  end if;

  -- 12 Hour Time

  if minute >= 38 then
    -- add an hour
    hour := @ + 1;
    if hour >=  24 then
      hour := 00;
    end if;
  end if;

  -- Hour as English Text

  case hour is
  when 00|12|24 => nearhour := "Twelve" ;
  when 01|13 =>  nearhour := "One" ;
  when 02|14 =>  nearhour := "Two" ;
  when 03|15 =>  nearhour := "Three" ;
  when 04|16 =>  nearhour := "Four" ;
  when 05|17 =>  nearhour := "Five" ;
  when 06|18 =>  nearhour := "Six" ;
  when 07|19 =>  nearhour := "Seven" ;
  when 08|20 =>  nearhour := "Eight" ;
  when 09|21 =>  nearhour := "Nine" ;
  when 10|22 =>  nearhour := "Ten" ;
  when 11|23 =>  nearhour := "Eleven" ;
  when others =>
    put_line( standard_error, "unknown time" );
  end case;

  -- Determine the time of day

  if hour < 12 then
    nearampm := "in the morning";
  elsif hour < 17 then
    nearampm := "in the afternoon";
  elsif hour < 20 then
    nearampm := "in the evening";
  else
    nearampm := "at night";
  end if;

  -- Approximate the minutes

  if minute = 0 then
    nearbefore := "exactly ";
    nearafter := "o'clock ";
  elsif minute < 8 then
    nearbefore := "just after ";
    nearafter := "o'clock ";
  elsif minute < 15 then
    nearbefore := "almost a quarter past ";
    nearafter := "";
  elsif minute = 15 then
    nearbefore := "a quarter past ";
    nearafter := "";
  elsif minute < 23 then
    nearbefore := "just after a quarter past ";
    nearafter := "";
  elsif minute < 30 then
    nearbefore := "almost half past ";
    nearafter := "";
  elsif minute = 30 then
    nearbefore := "exactly half past ";
    nearafter := "";
  elsif minute < 38 then
    nearbefore := "just after half past ";
    nearafter := "";
  elsif minute < 45 then
    nearbefore := "almost a quarter to ";
    nearafter := "";
  elsif minute < 53 then
    nearbefore := "just after a quarter to ";
    nearafter := "";
  elsif minute <= 59 then
    nearbefore := "almost ";
    nearafter := "o'clock ";
  end if;

  -- Display the time

  put_line( "It's " & nearbefore & nearhour & ' ' & nearafter & nearampm );

  command_line.set_exit_status( 0 );

end neartime;

-- VIM editor formatting instructions -- vim: ft=bush

