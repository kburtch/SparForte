------------------------------------------------------------------------------
-- Calendar Package Parser                                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2011 Free Software Foundation              --
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

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

package parser_cal is

------------------------------------------------------------------------------
-- Calendar package identifiers
------------------------------------------------------------------------------

cal_time_t         : identifier;
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

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupCalendar;
procedure ShutdownCalendar;

------------------------------------------------------------------------------
-- PARSE THE CALENDAR PACKAGE
------------------------------------------------------------------------------

procedure ParseCalClock( result : out unbounded_string; kind : out identifier );
procedure ParseCalYear( result : out unbounded_string; kind : out identifier );
procedure ParseCalMonth( result : out unbounded_string; kind : out identifier );
procedure ParseCalDay( result : out unbounded_string; kind : out identifier );
procedure ParseCalSeconds( result : out unbounded_string; kind : out identifier );
procedure ParseCalSplit;
procedure ParseCalTimeOf( result : out unbounded_string; kind : out identifier );
procedure ParseCalToJulian( result : out unbounded_string; kind : out identifier );
procedure ParseCalToTime( result : out unbounded_string; kind : out identifier );
procedure ParseCalDayOfWeek( result : out unbounded_string; kind : out identifier );

end parser_cal;
