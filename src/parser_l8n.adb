------------------------------------------------------------------------------
-- Localization Package Parser                                              --
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

with ada.strings.unbounded,
     l8n,
     world,
     scanner,
     parser_params;
use  ada.strings.unbounded,
     l8n,
     world,
     scanner,
     parser_params;

package body parser_l8n is

------------------------------------------------------------------------------
-- Localization package identifiers
------------------------------------------------------------------------------

l8n_codeset_t : identifier;
l8n_d_t_fmt_t : identifier;
l8n_d_fmt_t   : identifier;
l8n_t_fmt_t   : identifier;
l8n_day_t     : identifier;
l8n_abday_t   : identifier;
l8n_mon_t     : identifier;
l8n_abmon_t   : identifier;
l8n_radixchar_t : identifier;
l8n_thousep_t : identifier;
l8n_yesexpr_t : identifier;
l8n_noexpr_t  : identifier;
l8n_crncystr_t: identifier;


---------------------------------------------------------
--
-- PARSE THE L8N PACKAGE
--
---------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE L8N CODESET
--
-- Syntax: l8n.codeset;
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NCodeSet( result : out unbounded_string; kind : out identifier ) is
begin
  kind := uni_string_t;
  expect( l8n_codeset_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     expect( symbol_t, ")" );
  end if;
  result := codeset;
end ParseL8NCodeSet;


-----------------------------------------------------------------------------
--  PARSE L8N D T FMT (Date and Time Format)
--
-- Syntax: l8n.d_fmt;
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NDTFmt( result : out unbounded_string; kind : out identifier ) is
begin
  kind := uni_string_t;
  expect( l8n_d_t_fmt_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     result := d_t_fmt;
  end if;
end ParseL8NDTFmt;


-----------------------------------------------------------------------------
--  PARSE L8N D FMT (Date Format)
--
-- Syntax: l8n.d_fmt;
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NDFmt( result : out unbounded_string; kind : out identifier ) is
begin
  kind := uni_string_t;
  expect( l8n_d_fmt_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     result := d_fmt;
  end if;
end ParseL8NDFmt;


-----------------------------------------------------------------------------
--  PARSE L8N T FMT (Time Format)
--
-- Syntax: l8n.t_fmt;
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NTFmt( result : out unbounded_string; kind : out identifier ) is
begin
  kind := uni_string_t;
  expect( l8n_t_fmt_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     result := t_fmt;
  end if;
end ParseL8NTFmt;


-----------------------------------------------------------------------------
--  PARSE L8N DAY (Day of Week)
--
-- Syntax: l8n.day( d );
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NDay( result : out unbounded_string; kind : out identifier ) is
  dayExpr : unbounded_string;
  dayKind : identifier;
  day     : positive;
begin
  kind := uni_string_t;
  expect( l8n_day_t );
  ParseSingleNumericParameter( dayExpr, dayKind, positive_t );
  if isExecutingCommand then
     begin
        day := positive( to_numeric( dayExpr ) );
        case day is
        when 1 => result := day_1;
        when 2 => result := day_2;
        when 3 => result := day_3;
        when 4 => result := day_4;
        when 5 => result := day_5;
        when 6 => result := day_6;
        when 7 => result := day_7;
        when others =>
          err( "day is 1..7" );
        end case;
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseL8NDay;


-----------------------------------------------------------------------------
--  PARSE L8N AB DAY (Abbreviated Day of Week)
--
-- Syntax: l8n.abday( d );
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NAbDay( result : out unbounded_string; kind : out identifier ) is
  dayExpr : unbounded_string;
  dayKind : identifier;
  day     : positive;
begin
  kind := uni_string_t;
  expect( l8n_abday_t );
  ParseSingleNumericParameter( dayExpr, dayKind, positive_t );
  if isExecutingCommand then
     begin
        day := positive( to_numeric( dayExpr ) );
        case day is
        when 1 => result := abday_1;
        when 2 => result := abday_2;
        when 3 => result := abday_3;
        when 4 => result := abday_4;
        when 5 => result := abday_5;
        when 6 => result := abday_6;
        when 7 => result := abday_7;
        when others =>
          err( "day is 1..7" );
        end case;
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseL8NAbDay;


-----------------------------------------------------------------------------
--  PARSE L8N MON (Month)
--
-- Syntax: l8n.mon( d );
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NMon( result : out unbounded_string; kind : out identifier ) is
  monExpr : unbounded_string;
  monKind : identifier;
  mon     : positive;
begin
  kind := uni_string_t;
  expect( l8n_mon_t );
  ParseSingleNumericParameter( monExpr, monKind, positive_t );
  if isExecutingCommand then
     begin
        mon := positive( to_numeric( monExpr ) );
        case mon is
        when 1 => result := mon_1;
        when 2 => result := mon_2;
        when 3 => result := mon_3;
        when 4 => result := mon_4;
        when 5 => result := mon_5;
        when 6 => result := mon_6;
        when 7 => result := mon_7;
        when 8 => result := mon_8;
        when 9 => result := mon_9;
        when 10 => result := mon_10;
        when 11 => result := mon_11;
        when 12 => result := mon_12;
        when others =>
          err( "month is 1..12" );
        end case;
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseL8NMon;


-----------------------------------------------------------------------------
--  PARSE L8N AB MON (Abbreviated Month)
--
-- Syntax: l8n.abmon( d );
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NAbMon( result : out unbounded_string; kind : out identifier ) is
  monExpr : unbounded_string;
  monKind : identifier;
  mon     : positive;
begin
  kind := uni_string_t;
  expect( l8n_abmon_t );
  ParseSingleNumericParameter( monExpr, monKind, positive_t );
  if isExecutingCommand then
     begin
        mon := positive( to_numeric( monExpr ) );
        case mon is
        when 1 => result := abmon_1;
        when 2 => result := abmon_2;
        when 3 => result := abmon_3;
        when 4 => result := abmon_4;
        when 5 => result := abmon_5;
        when 6 => result := abmon_6;
        when 7 => result := abmon_7;
        when 8 => result := abmon_8;
        when 9 => result := abmon_9;
        when 10 => result := abmon_10;
        when 11 => result := abmon_11;
        when 12 => result := abmon_12;
        when others =>
          err( "month is 1..12" );
        end case;
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseL8NAbMon;


-----------------------------------------------------------------------------
--  PARSE L8N RADIX CHAR
--
-- Syntax: l8n.radixchar;
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NRadixChar( result : out unbounded_string; kind : out identifier ) is
begin
  kind := uni_string_t;
  expect( l8n_radixchar_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     expect( symbol_t, ")" );
  end if;
  result := radixchar;
end ParseL8NRadixChar;


-----------------------------------------------------------------------------
--  PARSE L8N THOU SEP (Thousands Separator)
--
-- Syntax: l8n.thousep;
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NThouSep( result : out unbounded_string; kind : out identifier ) is
begin
  kind := uni_string_t;
  expect( l8n_thousep_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     expect( symbol_t, ")" );
  end if;
  result := thousep;
end ParseL8NThouSep;


-----------------------------------------------------------------------------
--  PARSE L8N YES EXPR (Yes Expression)
--
-- Syntax: l8n.yesexpr;
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NYesExpr( result : out unbounded_string; kind : out identifier ) is
begin
  kind := uni_string_t;
  expect( l8n_yesexpr_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     expect( symbol_t, ")" );
  end if;
  result := yesexpr;
end ParseL8NYesExpr;


-----------------------------------------------------------------------------
--  PARSE L8N NO EXPR (No Expression)
--
-- Syntax: l8n.noexpr;
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NNoExpr( result : out unbounded_string; kind : out identifier ) is
begin
  kind := uni_string_t;
  expect( l8n_noexpr_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     expect( symbol_t, ")" );
  end if;
  result := noexpr;
end ParseL8NNoExpr;


-----------------------------------------------------------------------------
--  PARSE L8N CRNCY STR (Currency String)
--
-- Syntax: l8n.crncystr;
-- Source: N/A
-----------------------------------------------------------------------------

procedure ParseL8NCrncyStr( result : out unbounded_string; kind : out identifier ) is
begin
  kind := uni_string_t;
  expect( l8n_crncystr_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     expect( symbol_t, ")" );
  end if;
  result := crncystr;
end ParseL8NCrncyStr;


-------------------------------------------------------------------------------
--
-- Housekeeping
--
-------------------------------------------------------------------------------


procedure StartupL8N is
begin
  declareNamespace( "l8n" );

  declareFunction( l8n_codeset_t, "l8n.codeset", ParseL8NCodeSet'access );
  declareFunction( l8n_d_t_fmt_t, "l8n.d_t_fmt", ParseL8NDTFmt'access );
  declareFunction( l8n_d_fmt_t, "l8n.d_fmt", ParseL8NDFmt'access );
  declareFunction( l8n_t_fmt_t, "l8n.t_fmt", ParseL8NTFmt'access );
  declareFunction( l8n_day_t, "l8n.day", ParseL8NDay'access );
  declareFunction( l8n_abday_t, "l8n.abday", ParseL8NAbDay'access );
  declareFunction( l8n_mon_t, "l8n.mon", ParseL8NMon'access );
  declareFunction( l8n_abmon_t, "l8n.abmon", ParseL8NAbMon'access );
  declareFunction( l8n_radixchar_t, "l8n.radixchar", ParseL8NRadixChar'access );
  declareFunction( l8n_thousep_t, "l8n.thousep", ParseL8NThouSep'access );
  declareFunction( l8n_yesexpr_t, "l8n.yesexpr", ParseL8NYesExpr'access );
  declareFunction( l8n_noexpr_t, "l8n.noexpr", ParseL8NNoExpr'access );
  declareFunction( l8n_crncystr_t, "l8n.crncystr", ParseL8NCrncyStr'access );

  declareNamespaceClosed( "l8n" );

end StartupL8N;

procedure ShutdownL8N is
begin
  null;
end ShutdownL8N;

end parser_l8n;
