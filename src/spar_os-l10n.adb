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

package body spar_os.l10n is

------------------------------------------------------------------------------
--
-- Linux localization functions (from c_l10n.c)
--
------------------------------------------------------------------------------

function setlocale( category : int; locale : chars_ptr ) return chars_ptr;
pragma import( C, setlocale, "setlocale" );

function getlocale( category : int ) return chars_ptr;
pragma import( C, getlocale, "getlocale" );

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

function langinfo_decimal_point return chars_ptr;
pragma import( C, langinfo_decimal_point, "langinfo_decimal_point" );

function langinfo_thousands_sep return chars_ptr;
pragma import( C, langinfo_thousands_sep, "langinfo_thousands_sep" );

function langinfo_yesexpr return chars_ptr;
pragma import( C, langinfo_yesexpr, "langinfo_yesexpr" );

function langinfo_noexpr return chars_ptr;
pragma import( C, langinfo_noexpr, "langinfo_noexpr" );

function langinfo_currency_symbol return chars_ptr;
pragma import( C, langinfo_currency_symbol, "langinfo_currency_symbol" );

function langinfo_amstr return chars_ptr;
pragma import( C, langinfo_amstr, "langinfo_amstr" );

function langinfo_pmstr return chars_ptr;
pragma import( C, langinfo_pmstr, "langinfo_pmstr" );

function langinfo_t_fmt_ampm return chars_ptr;
pragma import( C, langinfo_t_fmt_ampm, "langinfo_t_fmt_ampm" );

function langinfo_positive_sign return chars_ptr;
pragma import( C, langinfo_positive_sign, "langinfo_positive_sign" );

function langinfo_negative_sign return chars_ptr;
pragma import( C, langinfo_negative_sign, "langinfo_negative_sign" );

function langinfo_int_curr_symbol return chars_ptr;
pragma import( C, langinfo_int_curr_symbol, "langinfo_int_curr_symbol" );

function langinfo_grouping return chars_ptr;
pragma import( C, langinfo_grouping, "langinfo_grouping" );

function langinfo_frac_digits return chars_ptr;
pragma import( C, langinfo_frac_digits, "langinfo_frac_digits" );

function langinfo_int_frac_digits return chars_ptr;
pragma import( C, langinfo_int_frac_digits, "langinfo_int_frac_digits" );

function langinfo_p_cs_precedes return chars_ptr;
pragma import( C, langinfo_p_cs_precedes, "langinfo_p_cs_precedes" );

function langinfo_p_sep_by_space return chars_ptr;
pragma import( C, langinfo_p_sep_by_space, "langinfo_p_sep_by_space" );

function langinfo_p_sign_posn return chars_ptr;
pragma import( C, langinfo_p_sign_posn, "langinfo_p_sign_posn" );

function langinfo_n_cs_precedes return chars_ptr;
pragma import( C, langinfo_n_cs_precedes, "langinfo_n_cs_precedes" );

function langinfo_n_sep_by_space return chars_ptr;
pragma import( C, langinfo_n_sep_by_space, "langinfo_n_sep_by_space" );

function langinfo_n_sign_posn return chars_ptr;
pragma import( C, langinfo_n_sign_posn, "langinfo_n_sign_posn" );

function langinfo_mon_decimal_point return chars_ptr;
pragma import( C, langinfo_mon_decimal_point, "langinfo_mon_decimal_point" );

function langinfo_mon_thousands_sep return chars_ptr;
pragma import( C, langinfo_mon_thousands_sep, "langinfo_mon_thousands_sep" );

function langinfo_mon_grouping return chars_ptr;
pragma import( C, langinfo_mon_grouping, "langinfo_mon_grouping" );

function langinfo_era return chars_ptr;
pragma import( C, langinfo_era, "langinfo_era" );

function langinfo_era_year return chars_ptr;
pragma import( C, langinfo_era_year, "langinfo_era_year" );

function langinfo_era_d_t_fmt return chars_ptr;
pragma import( C, langinfo_era_d_t_fmt, "langinfo_era_d_t_fmt" );

function langinfo_era_d_fmt return chars_ptr;
pragma import( C, langinfo_era_d_fmt, "langinfo_era_d_fmt" );

function langinfo_era_t_fmt return chars_ptr;
pragma import( C, langinfo_era_t_fmt, "langinfo_era_t_fmt" );

procedure setlocale(category : locale_category; locale : string ) is
  locale_ptr : chars_ptr;
  result_ptr : chars_ptr;
begin
  locale_ptr := New_String( locale );
  result_ptr := setlocale( int( category ), locale_ptr );
  free( locale_ptr );
end setlocale;

function getlocale(category : locale_category ) return unbounded_string is
begin
   return to_unbounded_string( value( getlocale( int( category ) )  ) );
end getlocale;

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

function decimal_point return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_decimal_point ) );
end decimal_point;

function thousands_sep return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_thousands_sep ) );
end thousands_sep;

function yesexpr return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_yesexpr ) );
end yesexpr;

function noexpr return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_noexpr ) );
end noexpr;

function currency_symbol return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_currency_symbol ) );
end currency_symbol;

function amstr return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_amstr ) );
end amstr;

function pmstr return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_pmstr ) );
end pmstr;

function t_fmt_ampm return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_t_fmt_ampm ) );
end t_fmt_ampm;

function positive_sign return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_positive_sign ) );
end positive_sign;

function negative_sign return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_negative_sign ) );
end negative_sign;

function int_curr_symbol return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_int_curr_symbol ) );
end int_curr_symbol;

function grouping return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_grouping ) );
end grouping;

function frac_digits return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_frac_digits ) );
end frac_digits;

function int_frac_digits return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_int_frac_digits ) );
end int_frac_digits;

function p_cs_precedes return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_p_cs_precedes ) );
end p_cs_precedes;

function p_sep_by_space return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_p_sep_by_space ) );
end p_sep_by_space;

function p_sign_posn return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_p_sign_posn ) );
end p_sign_posn;

function n_cs_precedes return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_n_cs_precedes ) );
end n_cs_precedes;

function n_sep_by_space return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_n_sep_by_space ) );
end n_sep_by_space;

function n_sign_posn return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_n_sign_posn ) );
end n_sign_posn;

function mon_decimal_point return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_decimal_point ) );
end mon_decimal_point;

function mon_thousands_sep return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_thousands_sep ) );
end mon_thousands_sep;

function mon_grouping return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_mon_grouping ) );
end mon_grouping;

function era return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_era ) );
end era;

function era_year return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_era_year ) );
end era_year;

function era_d_t_fmt return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_era_d_t_fmt ) );
end era_d_t_fmt;

function era_d_fmt return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_era_d_fmt ) );
end era_d_fmt;

function era_t_fmt return unbounded_string is
begin
  return to_unbounded_string( value( langinfo_era_t_fmt ) );
end era_t_fmt;

end spar_os.l10n;

