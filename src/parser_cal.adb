------------------------------------------------------------------------------
-- Calendar Package Parser                                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  This is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

--with text_io;use text_io;

with ada.strings.unbounded,
    pegasoft,
    scanner.calendar,
    scanner.communications,
    parser_params,
    spar_os;
use ada.strings.unbounded,
    pegasoft,
    scanner,
    scanner.calendar,
    scanner.communications,
    parser_params,
    spar_os;

package body parser_cal is

------------------------------------------------------------------------------
-- Calendar package identifiers
--
-- Calendar_time type (cal_time_t) is exposed for the main parser
------------------------------------------------------------------------------

cal_year_number_t  : identifier;
cal_month_number_t : identifier;
cal_day_number_t   : identifier;
cal_day_duration_t : identifier;

cal_clock_t        : identifier;
cal_year_t         : identifier;
cal_month_t        : identifier;
cal_day_t          : identifier;
cal_seconds_t      : identifier;
cal_split_t        : identifier;
cal_time_of_t      : identifier;
cal_to_julian_t    : identifier;
cal_to_time_t      : identifier;
cal_day_of_week_t  : identifier;


-----------------------------------------------------------------------------
--  PARSE CAL CLOCK
--
-----------------------------------------------------------------------------

procedure ParseCalClock( result : out unbounded_string; kind : out identifier ) is
begin
  kind := cal_time_t;
  expect( cal_clock_t );
  result := to_unbounded_string( clock'img );
end ParseCalClock;


-----------------------------------------------------------------------------
--  PARSE CAL YEAR
--
-----------------------------------------------------------------------------

procedure ParseCalYear( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := cal_year_number_t;
  expect( cal_year_t );
  ParseSingleNumericParameter( cal_year_t, expr_val, expr_type, cal_time_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( year( time( to_numeric( expr_val ) ) )'img );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseCalYear;


-----------------------------------------------------------------------------
--  PARSE CAL MONTH
--
-----------------------------------------------------------------------------

procedure ParseCalMonth( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := cal_month_number_t;
  expect( cal_month_t );
  ParseSingleNumericParameter( cal_month_t, expr_val, expr_type, cal_time_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( month( time( to_numeric( expr_val ) ) )'img );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseCalMonth;


-----------------------------------------------------------------------------
--  PARSE CAL DAY
--
-----------------------------------------------------------------------------

procedure ParseCalDay( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := cal_day_number_t;
  expect( cal_day_t );
  ParseSingleNumericParameter( cal_day_t, expr_val, expr_type, cal_time_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( day( time( to_numeric( expr_val ) ) )'img );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseCalDay;


-----------------------------------------------------------------------------
--  PARSE CAL SECONDS
--
-----------------------------------------------------------------------------

procedure ParseCalSeconds( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := cal_day_duration_t;
  expect( cal_seconds_t );
  ParseSingleNumericParameter( cal_seconds_t, expr_val, expr_type, cal_time_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( numericValue( seconds( time( to_numeric( expr_val ) ) ) )'img );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseCalSeconds;


-----------------------------------------------------------------------------
--  PARSE CAL SPLIT
--
-----------------------------------------------------------------------------

procedure ParseCalSplit is
   date_val  : unbounded_string;
   date_type : identifier;
   id1_ref  : reference;
   id2_ref  : reference;
   id3_ref  : reference;
   id4_ref  : reference;
   year     : year_number;
   month    : month_number;
   day      : day_number;
   seconds  : day_duration;
begin
  expect( cal_split_t );
  ParseFirstNumericParameter( cal_split_t, date_val, date_type, cal_time_t );
  ParseNextOutParameter( cal_split_t, id1_ref, cal_year_number_t );
  ParseNextOutParameter( cal_split_t, id2_ref, cal_month_number_t );
  ParseNextOutParameter( cal_split_t, id3_ref, cal_day_number_t );
  ParseLastOutParameter( cal_split_t, id4_ref, cal_day_duration_t );
  if isExecutingCommand then
     begin
       Split( time( to_numeric( date_val ) ), year, month, day, seconds );
       AssignParameter( id1_ref, to_unbounded_string( year'img ) );
       AssignParameter( id2_ref, to_unbounded_string( month'img ) );
       AssignParameter( id3_ref, to_unbounded_string( day'img ) );
       AssignParameter( id4_ref, to_unbounded_string( seconds'img ) );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseCalSplit;


-----------------------------------------------------------------------------
--  PARSE CAL TIME OF
--
-----------------------------------------------------------------------------

procedure ParseCalTimeOf( result : out unbounded_string; kind : out identifier ) is
  year_val   : unbounded_string;
  year_type  : identifier;
  month_val  : unbounded_string;
  month_type : identifier;
  day_val    : unbounded_string;
  day_type   : identifier;
  secs_val   : unbounded_string;
  secs_type  : identifier;
begin
  kind := cal_time_t;
  expect( cal_time_of_t );
  ParseFirstNumericParameter( cal_time_of_t, year_val, year_type, cal_year_number_t );
  ParseNextNumericParameter( cal_time_t, month_val, month_type, cal_month_number_t );
  ParseNextNumericParameter( cal_time_t, day_val, day_type, cal_day_number_t );
  ParseLastNumericParameter( cal_time_t, secs_val, secs_type, cal_day_duration_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string(
           Time_Of( year_number( to_numeric( year_val ) ),
                    month_number( to_numeric( month_val ) ),
                    day_number( to_numeric( day_val ) ),
                    day_duration( to_numeric( secs_val ) ) )'img );
     exception when time_error =>
       err( +"time error: illegal time value" );
     when constraint_error =>
       err( +"constraint error: values out of range" );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseCalTimeOf;


-----------------------------------------------------------------------------
--  PARSE CAL TO JULIAN
--
-----------------------------------------------------------------------------

procedure ParseCalToJulian( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := long_integer_t;
  expect( cal_to_julian_t );
  ParseSingleNumericParameter( cal_to_julian_t, expr_val, expr_type, cal_time_t );
  if isExecutingCommand then
     declare
       -- Communications of the ACM, October 1968
       Year   : year_number;
       Month  : month_number;
       day    : day_number;
       DayDur : day_duration;
       I      : long_float;
       J      : long_float;
       K      : long_float;
       Julian : long_float;
       Term1  : long_float;
       Term2  : long_float;
       Term3  : long_float;
     begin
       Split( time( to_numeric( expr_val ) ), Year, Month, Day, DayDur );
       I := long_float( Year );
       J := long_float( Month );
       K := long_float( Day );
       Term1 := long_float'truncation( 1461.0 * ( I + 4800.0 +
                long_float'truncation( (J-14.0) / 12.0 ) ) / 4.0 );
       Term2 := long_float'truncation( 367.0 * ( J - 2.0 -
                long_float'truncation( ( J-14.0 ) / 12.0 ) * 12.0 )
                / 12.0 );
       Term3 := 3.0 *
                long_float'truncation( (
                long_float'truncation( ( I + 4900.0 +
                long_float'truncation( ( J-14.0 ) / 12.0 )
                ) / 100.0 )
                ) / 4.0 );
       Julian := K - 32075.0 + Term1 + Term2 - Term3;
       result := to_unbounded_string( long_integer( Julian )'img );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseCalToJulian;


-----------------------------------------------------------------------------
--  PARSE CAL TO TIME
--
-----------------------------------------------------------------------------

procedure ParseCalToTime( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := cal_time_t;
  expect( cal_to_time_t );
  ParseSingleNumericParameter( cal_to_time_t, expr_val, expr_type, long_integer_t );
  if isExecutingCommand then
     declare
       -- Communications of the ACM, October 1968
       Julian : long_float;
       Year   : Year_Number;
       Month  : Month_Number;
       Day    : Day_Number;
       I      : long_float;
       J      : long_float;
       K      : long_float;
       L      : long_float;
       N      : long_float;
     begin
       Julian := long_float( to_numeric( expr_val ) );
       L := Julian + 68569.0;
       N := long_float'truncation( 4.0*L / 146097.0 );
       L := L - long_float'truncation( ( 146097.0 * N + 3.0 ) / 4.0 );
       I := long_float'truncation( ( 4000.0*(L+1.0) ) / 1461001.0 );
       L := L - long_float'truncation( 1461.0*I / 4.0) + 31.0;
       J := long_float'truncation( 80.0*L / 2447.0 );
       K := L - long_float'truncation( 2447.0*J / 80.0 );
       L := long_float'truncation( J / 11.0 );
       J := J+2.0-(12.0*L);
       I := 100.0 * (N-49.0) + I + L;
       Year := Year_Number( I );
       Month := Month_Number( J );
       Day := Day_Number( K );
       result := to_unbounded_string( long_long_integer( time_of( Year, Month, Day, 0.0 ) ) 'img );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseCalToTime;


-----------------------------------------------------------------------------
--  PARSE CAL DAY OF WEEK
--
-----------------------------------------------------------------------------

procedure ParseCalDayOfWeek( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := integer_t;
  expect( cal_day_of_week_t );
  ParseSingleNumericParameter( cal_day_of_week_t, expr_val, expr_type, cal_time_t );
  if isExecutingCommand then
     declare
       Year   : year_number;
       Month  : month_number;
       day    : day_number;
       DayDur : day_duration;
       wday   : integer;
     begin
       Split( time( to_numeric( expr_val ) ), Year, Month, Day, DayDur );
       C_day_of_week( wday, Year, Month, Day );
       result := to_unbounded_string( wday'img );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseCalDayOfWeek;

-----------------------------------------------------------------------------

procedure StartupCalendar is
begin
  declareNamespace( "calendar" );
  declareIdent( cal_time_t, "calendar.time", variable_t, typeClass );
  declareIdent( cal_year_number_t, "calendar.year_number", integer_t,
    typeClass );
  declareIdent( cal_month_number_t, "calendar.month_number", integer_t,
    typeClass );
  declareIdent( cal_day_number_t, "calendar.day_number", integer_t,
    typeClass );
  declareIdent( cal_day_duration_t, "calendar.day_duration", duration_t,
    typeClass );

  declareFunction( cal_clock_t, "calendar.clock", ParseCalClock'access );
  declareFunction( cal_year_t, "calendar.year", ParseCalYear'access );
  declareFunction( cal_month_t, "calendar.month", ParseCalMonth'access );
  declareFunction( cal_day_t, "calendar.day", ParseCalDay'access );
  declareFunction( cal_seconds_t, "calendar.seconds", ParseCalSeconds'access );
  declareProcedure( cal_split_t, "calendar.split", ParseCalSplit'access );
  declareFunction( cal_time_of_t, "calendar.time_of", ParseCalTimeOf'access );
  declareFunction( cal_to_julian_t, "calendar.to_julian", ParseCalToJulian'access );
  declareFunction( cal_to_time_t, "calendar.to_time", ParseCalToTime'access );
  declareFunction( cal_day_of_week_t, "calendar.day_of_week", ParseCalDayOfWeek'access );
  declareNamespaceClosed( "calendar" );
end StartupCalendar;

procedure ShutdownCalendar is
begin
  null;
end ShutdownCalendar;

end parser_cal;
