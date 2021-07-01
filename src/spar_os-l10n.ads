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

package spar_os.l10n is

type locale_type is new int;

LC_ALL : constant locale_type;
pragma import( C, LC_ALL, "l10n_LC_ALL" );

LC_COLLATE : constant locale_type;
pragma import( C, LC_COLLATE, "l10n_LC_COLLATE" );

LC_MESSAGES : constant locale_type;
pragma import( C, LC_MESSAGES, "l10n_LC_MESSAGES" );

LC_MONETARY : constant locale_type;
pragma import( C, LC_MONETARY, "l10n_LC_MONETARY" );

LC_CTYPE : constant locale_type;
pragma import( C, LC_CTYPE, "l10n_LC_CTYPE" );

LC_TIME : constant locale_type;
pragma import( C, LC_TIME, "l10n_LC_TIME" );

procedure setlocale(category : locale_type; locale : string );

function codeset return unbounded_string;

function d_t_fmt return unbounded_string;

function d_fmt return unbounded_string;

function t_fmt return unbounded_string;

function day_1 return unbounded_string;

function day_2 return unbounded_string;

function day_3 return unbounded_string;

function day_4 return unbounded_string;

function day_5 return unbounded_string;

function day_6 return unbounded_string;

function day_7 return unbounded_string;

function abday_1 return unbounded_string;

function abday_2 return unbounded_string;

function abday_3 return unbounded_string;

function abday_4 return unbounded_string;

function abday_5 return unbounded_string;

function abday_6 return unbounded_string;

function abday_7 return unbounded_string;

function mon_1 return unbounded_string;

function mon_2 return unbounded_string;

function mon_3 return unbounded_string;

function mon_4 return unbounded_string;

function mon_5 return unbounded_string;

function mon_6 return unbounded_string;

function mon_7 return unbounded_string;

function mon_8 return unbounded_string;

function mon_9 return unbounded_string;

function mon_10 return unbounded_string;

function mon_11 return unbounded_string;

function mon_12 return unbounded_string;

function abmon_1 return unbounded_string;

function abmon_2 return unbounded_string;

function abmon_3 return unbounded_string;

function abmon_4 return unbounded_string;

function abmon_5 return unbounded_string;

function abmon_6 return unbounded_string;

function abmon_7 return unbounded_string;

function abmon_8 return unbounded_string;

function abmon_9 return unbounded_string;

function abmon_10 return unbounded_string;

function abmon_11 return unbounded_string;

function abmon_12 return unbounded_string;

function decimal_point return unbounded_string;

function thousands_sep return unbounded_string;

function yesexpr return unbounded_string;

function noexpr return unbounded_string;

function currency_symbol return unbounded_string;

function amstr return unbounded_string;

function pmstr return unbounded_string;

function t_fmt_ampm return unbounded_string;

function positive_sign return unbounded_string;

function negative_sign return unbounded_string;

function int_curr_symbol return unbounded_string;

end spar_os.l10n;

