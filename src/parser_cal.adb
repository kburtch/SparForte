------------------------------------------------------------------------------
-- Calendar Package Parser                                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2025 Free Software Foundation              --
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
    symbol_table,
    message_strings,
    scanner.calendar,
    scanner.communications,
    parser_params,
    spar_os;
use ada.strings.unbounded,
    pegasoft,
    symbol_table,
    message_strings,
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

cal_julian_date_t  : identifier;


-----------------------------------------------------------------------------
--  PARSE CAL CLOCK
--
-- Syntax: t := calendar.clock [tagged tag]
-- Return the current time (that is, the calendar date and clock time, or the
-- timestamp for this moment in time) as a calendar.time value.
-----------------------------------------------------------------------------

procedure ParseCalClock( result : out storage; kind : out identifier ) is
begin
  kind := cal_time_t;
  expect( cal_clock_t );
  ParseMetaLabelSuffix( result.metaLabel );
  result.value := to_unbounded_string( clock'img );
end ParseCalClock;


-----------------------------------------------------------------------------
--  PARSE CAL YEAR
--
-- Syntax: y := calendar.year( t )
-- Return the year value of the given time as a number between 1901 and 2099.
-----------------------------------------------------------------------------

procedure ParseCalYear( result : out storage; kind : out identifier ) is
  expr      : storage;
  expr_type : identifier;
  subprogramId : constant identifier := cal_year_t;
begin
  kind := cal_year_number_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type, cal_time_t );
  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          result := storage'(
             to_unbounded_string( year( time( to_numeric( expr.value ) ) )'img ),
             expr.metaLabel
          );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseCalYear;


-----------------------------------------------------------------------------
--  PARSE CAL MONTH
--
-- Syntax: m := calendar.month( t )
-- Return the month of the given time between 1 and 12.
-----------------------------------------------------------------------------

procedure ParseCalMonth( result : out storage; kind : out identifier ) is
  expr      : storage;
  expr_type : identifier;
  subprogramId : constant identifier := cal_month_t;
begin
  kind := cal_month_number_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type, cal_time_t );
  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          result := storage'(
             to_unbounded_string( month( time( to_numeric( expr.value ) ) )'img ),
             expr.metaLabel
          );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseCalMonth;


-----------------------------------------------------------------------------
--  PARSE CAL DAY
--
-- Syntax: d := calendar.day( t )
-- Return the day of the given time as a number between 1 and 31.
-----------------------------------------------------------------------------

procedure ParseCalDay( result : out storage; kind : out identifier ) is
  expr      : storage;
  expr_type : identifier;
  subprogramId : constant identifier := cal_day_t;
begin
  kind := cal_day_number_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type, cal_time_t );
  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          result := storage'(
             to_unbounded_string( day( time( to_numeric( expr.value ) ) )'img ),
             expr.metaLabel
          );
        exception when others =>
          err_exception_raised;
        end;
    end if;
  end if;
end ParseCalDay;


-----------------------------------------------------------------------------
--  PARSE CAL SECONDS
--
-- Syntax: s := calendar.seconds( t )
-- Return the seconds into the day of the given time as a floating point number
-- between 0.0 and 86,400.0.
-----------------------------------------------------------------------------

procedure ParseCalSeconds( result : out storage; kind : out identifier ) is
  expr      : storage;
  expr_type : identifier;
  subprogramId : constant identifier := cal_seconds_t;
begin
  kind := cal_day_duration_t;
  expect( subprogramId );
  ParseSingleNumericParameter( subprogramId, expr, expr_type, cal_time_t );
  if isExecutingCommand then
     if metaLabelOk( expr ) then
        begin
          result := storage'(
             to_unbounded_string( numericValue( seconds( time( to_numeric( expr.value ) ) ) )'img ),
             expr.metaLabel
          );
        exception when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseCalSeconds;


-----------------------------------------------------------------------------
--  PARSE CAL SPLIT
--
-- Syntax: calendar.split( t, y, m, d, s )
-- Return the year, month, day and seconds value for the given time.
-----------------------------------------------------------------------------

procedure ParseCalSplit is
   dateExpr : storage;
   date_type : identifier;
   id1_ref  : reference;
   id2_ref  : reference;
   id3_ref  : reference;
   id4_ref  : reference;
   year     : year_number;
   month    : month_number;
   day      : day_number;
   seconds  : day_duration;
   subprogramId : constant identifier := cal_split_t;
begin
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, dateExpr, date_type, cal_time_t );
  ParseNextOutParameter( subprogramId, id1_ref, cal_year_number_t );
  ParseNextOutParameter( subprogramId, id2_ref, cal_month_number_t );
  ParseNextOutParameter( subprogramId, id3_ref, cal_day_number_t );
  ParseLastOutParameter( subprogramId, id4_ref, cal_day_duration_t );
  if isExecutingCommand then
     if metaLabelOk( dateExpr ) then
        begin
          Split( time( to_numeric( dateExpr.value ) ), year, month, day, seconds );
          AssignParameter( id1_ref, storage'( to_unbounded_string( year'img ), dateExpr.metaLabel ));
          AssignParameter( id2_ref, storage'( to_unbounded_string( month'img ), dateExpr.metaLabel ));
          AssignParameter( id3_ref, storage'( to_unbounded_string( day'img ), dateExpr.metaLabel ));
          AssignParameter( id4_ref, storage'( to_unbounded_string( seconds'img ), dateExpr.metaLabel ));
        exception when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseCalSplit;


-----------------------------------------------------------------------------
--  PARSE CAL TIME OF
--
-- Syntax: t := time_of( y,m,d,s )
-- Create a time from year, month, day and seconds values.
-----------------------------------------------------------------------------

procedure ParseCalTimeOf( result : out storage; kind : out identifier ) is
  yearExpr   : storage;
  year_type  : identifier;
  monthExpr  : storage;
  month_type : identifier;
  dayExpr    : storage;
  day_type   : identifier;
  secsExpr   : storage;
  secs_type  : identifier;
  subprogramId : constant identifier := cal_time_of_t;
begin
  kind := cal_time_t;
  expect( subprogramId );
  ParseFirstNumericParameter( subprogramId, yearExpr, year_type, cal_year_number_t );
  ParseNextNumericParameter( subprogramId, monthExpr, month_type, cal_month_number_t );
  ParseNextNumericParameter( subprogramId, dayExpr, day_type, cal_day_number_t );
  ParseLastNumericParameter( subprogramId, secsExpr, secs_type, cal_day_duration_t );
  if isExecutingCommand then
     -- this is a bit ugly because I do not have a 4-way version of metaLabelOk
     if metaLabelOk( yearExpr, monthExpr, dayExpr, secsExpr ) then
        begin
          result := storage'( to_unbounded_string(
              Time_Of( year_number( to_numeric( yearExpr.value ) ),
                       month_number( to_numeric( monthExpr.value ) ),
                       day_number( to_numeric( dayExpr.value ) ),
                       day_duration( to_numeric( secsExpr.value ) ) )'img ),
                 resolveEffectiveMetaLabel( yearExpr, monthExpr, dayExpr, secsExpr )
              );
        exception when time_error =>
          err( +"time error: illegal time value" );
        when constraint_error =>
          err( +"constraint error: values out of range" );
        when others =>
          err_exception_raised;
        end;
     end if;
  end if;
end ParseCalTimeOf;


-----------------------------------------------------------------------------
--  PARSE CAL TO JULIAN
--
-- Syntax: j := calendar.to_julian( t )
-- Return the number of days since at noon Universal Time on
-- Monday, January 1, 4713 BC.
-----------------------------------------------------------------------------

procedure ParseCalToJulian( result : out storage; kind : out identifier ) is
  expr      : storage;
  expr_type : identifier;
begin
  kind := cal_julian_date_t;
  expect( cal_to_julian_t );
  ParseSingleNumericParameter( cal_to_julian_t, expr, expr_type, cal_time_t );
  if isExecutingCommand then
     if metaLabelOk( expr ) then
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
         Split( time( to_numeric( expr.value ) ), Year, Month, Day, DayDur );
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
         result := storage'( to_unbounded_string( long_integer( Julian )'img ), expr.metaLabel );
       exception when others =>
         err_exception_raised;
       end;
     end if;
  end if;
end ParseCalToJulian;


-----------------------------------------------------------------------------
--  PARSE CAL TO TIME
--
-- Syntax: t := to_time( j )
-- Convert a Julian date to a Calendar time.
-----------------------------------------------------------------------------

procedure ParseCalToTime( result : out storage; kind : out identifier ) is
  expr      : storage;
  expr_type : identifier;
begin
  kind := cal_time_t;
  expect( cal_to_time_t );
  ParseSingleNumericParameter( cal_to_time_t, expr, expr_type, cal_julian_date_t );
  if isExecutingCommand then
     if metaLabelOk( expr ) then
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
         Julian := long_float( to_numeric( expr.value ) );
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
         result := storage'(
           to_unbounded_string( long_long_integer( time_of( Year, Month, Day, 0.0 ) ) 'img ),
           expr.metaLabel );
       exception when others =>
         err_exception_raised;
       end;
     end if;
  end if;
end ParseCalToTime;


-----------------------------------------------------------------------------
--  PARSE CAL DAY OF WEEK
--
-- Syntax: d := calendar.day_of_week( t )
-- Return the day of the week of the given time as a number of days since
-- Sunday, an integer between 0 and 6.
-----------------------------------------------------------------------------

procedure ParseCalDayOfWeek( result : out storage; kind : out identifier ) is
  expr      : storage;
  expr_type : identifier;
begin
  kind := integer_t;
  expect( cal_day_of_week_t );
  ParseSingleNumericParameter( cal_day_of_week_t, expr, expr_type, cal_time_t );
  if isExecutingCommand then
     if metaLabelOk( expr ) then
        declare
          Year   : year_number;
          Month  : month_number;
          day    : day_number;
          DayDur : day_duration;
          wday   : integer;
        begin
          Split( time( to_numeric( expr.value ) ), Year, Month, Day, DayDur );
          C_day_of_week( wday, Year, Month, Day );
          result := storage'( to_unbounded_string( wday'img ), expr.metaLabel );
        exception when others =>
          err_exception_raised;
        end;
     end if;
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
  declareIdent( cal_julian_date_t, "calendar.julian_date", long_integer_t,
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
