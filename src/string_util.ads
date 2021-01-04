------------------------------------------------------------------------------
-- STRING UTIL                                                              --
--                                                                          --
-- Part of SparForte                                                        --
-- Designed and Programmed by Ken O. Burtch                                 --
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

with ada.strings.unbounded, ada.calendar;
use ada.strings.unbounded;

package string_util is

------------------------------------------------------------------------------
-- The string util package contains supplimental string handling not included
-- with the standard ada.strings.unbounded package.
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Basic String Handling
------------------------------------------------------------------------------

procedure FixSpacing( s : in out unbounded_string; inside : boolean := true );
-- remove leading and trailing spaces, as well as any double-spaces inside
-- inside = true to remove inside


------------------------------------------------------------------------------
-- Basic String Testing
------------------------------------------------------------------------------

function TypoOf( BadString, GoodString : unbounded_string ) return boolean;
-- 80% of all typos are single insertions, deletions, exchanges, or subs.

function Is_Control( s : unbounded_string ) return boolean;
-- true if string is completely control characters

function Is_Graphic( s : unbounded_string ) return boolean;
-- true if string is completely printable characters

function Is_Letter( s : unbounded_string ) return boolean;
-- true if string is completely letter characters

function Is_Lower( s : unbounded_string ) return boolean;
-- true if string is completely lower-case characters

function Is_Upper( s : unbounded_string ) return boolean;
-- true if string is completely Upper-case characters

function Is_Basic( s : unbounded_string ) return boolean;
-- true if string is completely basic characters

function Is_Digit( s : unbounded_string ) return boolean;
-- true if string is completely digit characters

function Is_Hexadecimal_Digit( s : unbounded_string ) return boolean;
-- true if string is completely hexadecimal digit characters

function Is_Alphanumeric( s : unbounded_string ) return boolean;
-- true if string is completely hexadecimal digit characters

function Is_Special( s : unbounded_string ) return boolean;
-- true if string is completely special characters

function Is_Date( s : unbounded_string ) return boolean;
-- true if string is formatted as a slashed date (doesn't enforce
-- a particular order of month, date, year).

function Is_Fixed( s : unbounded_string ) return boolean;
-- true if string is completely fixed point characters


------------------------------------------------------------------------------
-- Basic String Conversions
------------------------------------------------------------------------------

function ToLower( s : unbounded_string ) return unbounded_string;
-- convert string to lower case

function ToUpper( s : unbounded_string ) return unbounded_string;
-- convert string to upper case

function ToProper( s : unbounded_string ) return unbounded_string;
-- convert string to proper (mixed or title) case

function ToBasic( s : unbounded_string ) return unbounded_string;
-- convert string to basic (non-special) characters

function ToEscaped( s : unbounded_string ) return unbounded_string;
-- convert special characters in string to ASCII codes

function ToJSONEscaped( s : unbounded_string ) return unbounded_string;
-- convert special characters in string to JSON escape codes

function ToJSONUnescaped( s : unbounded_string ) return unbounded_string;
-- convert JSON escape codes in string back to actual characters

function AorAN( s : unbounded_string ) return unbounded_string;
-- return the word s with a leading "a " or "an " depending on whether or
-- not it begins with a vowel

procedure Split( s : unbounded_string; left, right : out unbounded_string;
width : natural );
-- gracefully split s into left and right near split position width

function ToCSV( s : unbounded_string ) return unbounded_string;
-- convert s to CSV

------------------------------------------------------------------------------
-- String Field Handling
------------------------------------------------------------------------------

function stringField( s : unbounded_string; delimiter : character; f : natural )
return unbounded_string;
-- return the fth field delimited by delimiter

function stringCSVField( s1 : unbounded_string; delimiter : character;
f : natural; allowSingleQuotes : boolean := false ) return unbounded_string;
-- return the fth field delimited by delimiter (typically a comma) but
-- allow the delimiter to be escaped by double quote marks

procedure replaceField( s : in out unbounded_string; delimiter : character;
f : natural; field : string );
-- replace the fth field delimited by delimiter with field

procedure replaceCSVField( s : in out unbounded_string; delimiter : character;
f : natural; field : string );
-- replace the fth field delimited by delimiter (typically a comma) with field
-- allow the delimiter to be escaped by double quote marks

function stringLookup( s, t : unbounded_string; delimiter : character )
return unbounded_string;
-- Treat s (source) as a series of field pairs.  Return the right-hand pair
-- member associated with t (target), or a null string if none exists.  If
-- source or target is a null string, a null string is also returned.

function getDateString( ct : ada.calendar.time ) return unbounded_string;
-- Convert the calendar time to a human readable string in the format
-- mm/dd hh:mm:ss


------------------------------------------------------------------------------
-- Operating System String Handling
------------------------------------------------------------------------------

function dirname( s : unbounded_string ) return unbounded_string;
-- return the directory portion of a pathname string (OS dependant!)

function basename( s : unbounded_string ) return unbounded_string;
-- return the file portion of a pathname string (OS dependant!)

------------------------------------------------------------------------------
-- Operating System String Handling
------------------------------------------------------------------------------

function toSecureData( s : string ) return string;
-- return a string if not running in maintenance mode
-- (used in securing data shown in error messages)

end string_util;

