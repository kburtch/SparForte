------------------------------------------------------------------------------
-- Localization package                                                     --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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

with interfaces.C.strings,
     ada.strings.unbounded;
use  interfaces.C,
     interfaces.C.strings,
     ada.strings.unbounded;

package body l8n is

------------------------------------------------------------------------------
--
-- Linux localization functions (from c_l8n.c)
--
------------------------------------------------------------------------------

-- #if L8N

function langinfo_codeset return chars_ptr;
pragma import( C, langinfo_codeset, "langinfo_codeset" );

function langinfo_d_t_fmt return chars_ptr;
pragma import( C, langinfo_d_t_fmt, "langinfo_d_t_fmt" );

function langinfo_d_fmt return chars_ptr;
pragma import( C, langinfo_d_fmt, "langinfo_d_fmt" );

function langinfo_t_fmt return chars_ptr;
pragma import( C, langinfo_t_fmt, "langinfo_t_fmt" );

function langinfo_day_1 return chars_ptr;
pragma import( C, langinfo_day_1, "langinfo_day_1" );

function langinfo_day_2 return chars_ptr;
pragma import( C, langinfo_day_2, "langinfo_day_2" );

function langinfo_day_3 return chars_ptr;
pragma import( C, langinfo_day_3, "langinfo_day_3" );

function langinfo_day_4 return chars_ptr;
pragma import( C, langinfo_day_4, "langinfo_day_4" );

function langinfo_day_5 return chars_ptr;
pragma import( C, langinfo_day_5, "langinfo_day_5" );

function langinfo_day_6 return chars_ptr;
pragma import( C, langinfo_day_6, "langinfo_day_6" );

function langinfo_day_7 return chars_ptr;
pragma import( C, langinfo_day_7, "langinfo_day_7" );

function langinfo_abday_1 return chars_ptr;
pragma import( C, langinfo_abday_1, "langinfo_abday_1" );

function langinfo_abday_2 return chars_ptr;
pragma import( C, langinfo_abday_2, "langinfo_abday_2" );

function langinfo_abday_3 return chars_ptr;
pragma import( C, langinfo_abday_3, "langinfo_abday_3" );

function langinfo_abday_4 return chars_ptr;
pragma import( C, langinfo_abday_4, "langinfo_abday_4" );

function langinfo_abday_5 return chars_ptr;
pragma import( C, langinfo_abday_5, "langinfo_abday_5" );

function langinfo_abday_6 return chars_ptr;
pragma import( C, langinfo_abday_6, "langinfo_abday_6" );

function langinfo_abday_7 return chars_ptr;
pragma import( C, langinfo_abday_7, "langinfo_abday_7" );

function langinfo_mon_1 return chars_ptr;
pragma import( C, langinfo_mon_1, "langinfo_mon_1" );

function langinfo_mon_2 return chars_ptr;
pragma import( C, langinfo_mon_2, "langinfo_mon_2" );

function langinfo_mon_3 return chars_ptr;
pragma import( C, langinfo_mon_3, "langinfo_mon_3" );

function langinfo_mon_4 return chars_ptr;
pragma import( C, langinfo_mon_4, "langinfo_mon_4" );

function langinfo_mon_5 return chars_ptr;
pragma import( C, langinfo_mon_5, "langinfo_mon_5" );

function langinfo_mon_6 return chars_ptr;
pragma import( C, langinfo_mon_6, "langinfo_mon_6" );

function langinfo_mon_7 return chars_ptr;
pragma import( C, langinfo_mon_7, "langinfo_mon_7" );

function langinfo_mon_8 return chars_ptr;
pragma import( C, langinfo_mon_8, "langinfo_mon_8" );

function langinfo_mon_9 return chars_ptr;
pragma import( C, langinfo_mon_9, "langinfo_mon_9" );

function langinfo_mon_10 return chars_ptr;
pragma import( C, langinfo_mon_10, "langinfo_mon_10" );

function langinfo_mon_11 return chars_ptr;
pragma import( C, langinfo_mon_11, "langinfo_mon_11" );

function langinfo_mon_12 return chars_ptr;
pragma import( C, langinfo_mon_12, "langinfo_mon_12" );

function langinfo_abmon_1 return chars_ptr;
pragma import( C, langinfo_abmon_1, "langinfo_abmon_1" );

function langinfo_abmon_2 return chars_ptr;
pragma import( C, langinfo_abmon_2, "langinfo_abmon_2" );

function langinfo_abmon_3 return chars_ptr;
pragma import( C, langinfo_abmon_3, "langinfo_abmon_3" );

function langinfo_abmon_4 return chars_ptr;
pragma import( C, langinfo_abmon_4, "langinfo_abmon_4" );

function langinfo_abmon_5 return chars_ptr;
pragma import( C, langinfo_abmon_5, "langinfo_abmon_5" );

function langinfo_abmon_6 return chars_ptr;
pragma import( C, langinfo_abmon_6, "langinfo_abmon_6" );

function langinfo_abmon_7 return chars_ptr;
pragma import( C, langinfo_abmon_7, "langinfo_abmon_7" );

function langinfo_abmon_8 return chars_ptr;
pragma import( C, langinfo_abmon_8, "langinfo_abmon_8" );

function langinfo_abmon_9 return chars_ptr;
pragma import( C, langinfo_abmon_9, "langinfo_abmon_9" );

function langinfo_abmon_10 return chars_ptr;
pragma import( C, langinfo_abmon_10, "langinfo_abmon_10" );

function langinfo_abmon_11 return chars_ptr;
pragma import( C, langinfo_abmon_11, "langinfo_abmon_11" );

function langinfo_abmon_12 return chars_ptr;
pragma import( C, langinfo_abmon_12, "langinfo_abmon_12" );

function langinfo_radixchar return chars_ptr;
pragma import( C, langinfo_radixchar, "langinfo_radixchar" );

function langinfo_thousep return chars_ptr;
pragma import( C, langinfo_thousep, "langinfo_thousep" );

function langinfo_yesexpr return chars_ptr;
pragma import( C, langinfo_yesexpr, "langinfo_yesexpr" );

function langinfo_noexpr return chars_ptr;
pragma import( C, langinfo_noexpr, "langinfo_noexpr" );

function langinfo_crncystr return chars_ptr;
pragma import( C, langinfo_crncystr, "langinfo_crncystr" );

-- #end if;

function codeset return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_codeset ) );
end codeset;

function d_t_fmt return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_d_t_fmt ) );
end d_t_fmt;

function d_fmt return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_d_fmt ) );
end d_fmt;

function t_fmt return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_t_fmt ) );
end t_fmt;

function day_1 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_day_1 ) );
end day_1;

function day_2 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_day_2 ) );
end day_2;

function day_3 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_day_3 ) );
end day_3;

function day_4 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_day_4 ) );
end day_4;

function day_5 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_day_5 ) );
end day_5;

function day_6 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_day_6 ) );
end day_6;

function day_7 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_day_7 ) );
end day_7;

function abday_1 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abday_1 ) );
end abday_1;

function abday_2 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abday_2 ) );
end abday_2;

function abday_3 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abday_3 ) );
end abday_3;

function abday_4 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abday_4 ) );
end abday_4;

function abday_5 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abday_5 ) );
end abday_5;

function abday_6 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abday_6 ) );
end abday_6;

function abday_7 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abday_7 ) );
end abday_7;

function mon_1 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_1 ) );
end mon_1;

function mon_2 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_2 ) );
end mon_2;

function mon_3 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_3 ) );
end mon_3;

function mon_4 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_4 ) );
end mon_4;

function mon_5 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_5 ) );
end mon_5;

function mon_6 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_6 ) );
end mon_6;

function mon_7 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_7 ) );
end mon_7;

function mon_8 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_8 ) );
end mon_8;

function mon_9 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_9 ) );
end mon_9;

function mon_10 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_10 ) );
end mon_10;

function mon_11 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_11 ) );
end mon_11;

function mon_12 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_12 ) );
end mon_12;

function abmon_1 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_1 ) );
end abmon_1;

function abmon_2 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_2 ) );
end abmon_2;

function abmon_3 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_3 ) );
end abmon_3;

function abmon_4 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_4 ) );
end abmon_4;

function abmon_5 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_5 ) );
end abmon_5;

function abmon_6 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_6 ) );
end abmon_6;

function abmon_7 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_7 ) );
end abmon_7;

function abmon_8 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_8 ) );
end abmon_8;

function abmon_9 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_9 ) );
end abmon_9;

function abmon_10 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_10 ) );
end abmon_10;

function abmon_11 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_11 ) );
end abmon_11;

function abmon_12 return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_abmon_12 ) );
end abmon_12;

function radixchar return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_radixchar ) );
end radixchar;

function thousep return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_thousep ) );
end thousep;

function yesexpr return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_yesexpr ) );
end yesexpr;

function noexpr return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_noexpr ) );
end noexpr;

function crncystr return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_crncystr ) );
end crncystr;

end l8n;

