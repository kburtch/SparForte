------------------------------------------------------------------------------
-- PEGASOFT STRINGS                                                         --
--                                                                          --
-- Part of SparForte                                                        --
-- Designed and Programmed by Ken O. Burtch                                 --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2022 Free Software Foundation            --
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

pragma suppress( index_check );
pragma suppress( range_check );

--with text_io; use text_io;

with spar_os, Ada.Characters.Handling;
with world;
use  Ada.Characters.Handling,
     world;

package body pegasoft.strings is

------------------------------------------------------------------------------
-- Misc String Handling
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  FIX SPACKING
--
-- Remove leading and trailing spaces, as well as any double-spaces inside
------------------------------------------------------------------------------

procedure FixSpacing( s : in out unbounded_string; inside : boolean := true ) is
   i  : integer;
begin
  while length(s) > 0 loop
    exit when Element( s, 1 ) /= ' ';
    Delete( s, 1, 1 );
  end loop;
  while length(s) > 0 loop
    exit when Element( s, length(s) ) /= ' ';
    Delete( s, length(s), length(s) );
  end loop;
  if inside then
     i := 1;
     while i < length(s) loop
       if Element( s, i ) = ' ' and then Element( s, i+1 ) = ' ' then
          Delete( s, i, i );
          i := i - 1;
       end if;
       i := i + 1;
     end loop;
  end if;
end FixSpacing;


------------------------------------------------------------------------------
--  ESCAPE SPACES WITH BACKSLASHES
--
-- Place backslashes in front of spaces (or backslashes).
------------------------------------------------------------------------------

function escapeSpacesWithBackslashes( original : unbounded_string ) return unbounded_string is
  i : natural := 1;
  inBackslash : boolean := false;
  s : unbounded_string := original;
begin
  while i <= length( s ) loop
    if not inBackslash and element( s, i ) = ' ' then
       inBackslash := true;
       insert( s, i, "\" );
    elsif not inBackslash and element( s, i ) = '\' then
       inBackslash := true;
       insert( s, i, "\" );
    elsif inBackslash then
       inBackslash := false;
    end if;
    i := i + 1;
  end loop;
  return s;
end escapeSpacesWithBackslashes;

function escapeSpacesWithBackslashes( original : string ) return string is
begin
  return to_string( escapeSpacesWithBackslashes( to_unbounded_string( original ) ) );
end escapeSpacesWithBackslashes;


------------------------------------------------------------------------------
--  UNESCAPE WITH BACKSLASHES
--
-- Remove escaping backslashes.  If there is a trailing backslash, it is
-- left in.
------------------------------------------------------------------------------

function unescapeWithBackslashes( original : unbounded_string ) return unbounded_string is
  i : natural := 1;
  inBackslash : boolean := false;
  s : unbounded_string := original;
begin
  while i < length( s ) loop
    if not inBackslash and element( s, i ) = '\' then
       inBackslash := true;
       delete( s, i, i );
    elsif inBackslash then
       inBackslash := false;
    end if;
    i := i + 1;
  end loop;
  return s;
end unescapeWithBackslashes;

function unescapeWithBackslashes( s : string ) return string is
begin
  return to_string( unescapeWithBackslashes( to_unbounded_string( s ) ) );
end unescapeWithBackslashes;


------------------------------------------------------------------------------
--  TYPO OF
--
-- 80% of all typos are single insertions, deletions, exchanges, or subs.
------------------------------------------------------------------------------

function TypoOf( BadString, GoodString : unbounded_string ) return boolean is
  TempStr : unbounded_string;
  BadLen  : integer;
  GoodLen : integer;
  IsTypo  : boolean;
  TempChar : character;
begin
  IsTypo := false;
  BadLen := length( BadString );
  GoodLen := length( GoodString );

  if BadString = GoodString then -- identical?
     return false;
  elsif BadLen < 4 or GoodLen < 4 then -- too short to test reliably?
     return false;
  end if;

  -- Single Insertion
  if BadLen = GoodLen+1 then
     for i in 1..BadLen loop
         if Delete( BadString, i, i ) = GoodString then
            IsTypo := true;
         end if;
     end loop;
  end if;

  -- Single Deletion
  if BadLen = GoodLen-1 then
     for i in 1..GoodLen loop
         if BadString = Delete( GoodString, i, i ) then
            IsTypo := true;
         end if;
     end loop;
  end if;

  -- Single Exchange
  if BadLen = GoodLen and not IsTypo then
     TempStr := BadString;
     for i in 1..BadLen-1 loop
         TempChar := Element( TempStr, i );
         Replace_Element( TempStr, i, Element( TempStr, i+1 ) );
         Replace_Element( TempStr, i+1, TempChar );
         if TempStr = GoodString then
            IsTypo := true;
         end if;
         Replace_Element( TempStr, i+1, Element( TempStr, i ) );
         Replace_Element( TempStr, i, TempChar );
    end loop;
  end if;

  -- Single Substitution
  if BadLen = GoodLen and not IsTypo then
     for i in 1..BadLen loop
         if Delete( BadString, i, i ) = Delete( GoodString, i, i ) then
            IsTypo := true;
         end if;
     end loop;
  end if;

  return IsTypo;

end TypoOf;


------------------------------------------------------------------------------
-- Basic String Testing
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  IS CONTROL
--
-- true if string is completely control characters
------------------------------------------------------------------------------

function Is_Control( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Control( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Control;


------------------------------------------------------------------------------
--  IS GRAPHIC
--
-- true if string is completely printable characters
------------------------------------------------------------------------------

function Is_Graphic( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Graphic( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Graphic;


------------------------------------------------------------------------------
--  IS LETTER
--
-- true if string is completely letter characters
------------------------------------------------------------------------------

function Is_Letter( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Letter( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Letter;


------------------------------------------------------------------------------
--  IS LOWER
--
-- true if string is completely lower-case characters
------------------------------------------------------------------------------

function Is_Lower( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Lower( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Lower;


------------------------------------------------------------------------------
--  IS UPPER
--
-- true if string is completely Upper-case characters
------------------------------------------------------------------------------

function Is_Upper( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Upper( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Upper;


------------------------------------------------------------------------------
--  IS BASIC
--
-- true if string is completely basic characters
------------------------------------------------------------------------------

function Is_Basic( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Basic( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Basic;


------------------------------------------------------------------------------
--  IS DIGIT
--
-- true if string is completely digit characters
------------------------------------------------------------------------------

function Is_Digit( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Digit( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Digit;


------------------------------------------------------------------------------
--  IS HEXADECIMAL DIGIT
--
-- true if string is completely hexadecimal digit characters
------------------------------------------------------------------------------

function Is_Hexadecimal_Digit( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Hexadecimal_Digit( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Hexadecimal_Digit;


------------------------------------------------------------------------------
--  IS ALPHANUMERIC
--
-- true if string is completely hexadecimal digit characters
------------------------------------------------------------------------------

function Is_Alphanumeric( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Alphanumeric( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Alphanumeric;


------------------------------------------------------------------------------
--  IS SPECIAL
--
-- true if string is completely special symbol characters
------------------------------------------------------------------------------

function Is_Special( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  for i in 1..length( s ) loop
      if not Is_Special( Element( s, i ) ) then
         flag := false;
         exit;
      end if;
  end loop;
  return flag;
end Is_Special;


------------------------------------------------------------------------------
--  IS DATE
--
-- true if string looks like a slashed date
------------------------------------------------------------------------------

function Is_Date( s : unbounded_string ) return boolean is
  flag : boolean := true;
begin
  if length( s ) = 8 then
     if element( s, 3 ) /= '/' or element(s, 6) /= '/' then
        flag := false;
     elsif not is_digit( element( s, 1 ) ) or
           not is_digit( element( s, 2 ) ) or
           not is_digit( element( s, 4 ) ) or
           not is_digit( element( s, 5 ) ) or
           not is_digit( element( s, 7 ) ) or
           not is_digit( element( s, 8 ) ) then
        flag := false;
     end if;
  elsif length( s ) = 10 then
     if element( s, 3 ) /= '/' or element( s, 6) /= '/' then
        flag := false;
     elsif not is_digit( element( s, 1 ) ) or
           not is_digit( element( s, 2 ) ) or
           not is_digit( element( s, 4 ) ) or
           not is_digit( element( s, 5 ) ) or
           not is_digit( element( s, 7 ) ) or
           not is_digit( element( s, 8 ) ) or
           not is_digit( element( s, 9 ) ) or
           not is_digit( element( s, 10 ) ) then
        flag := false;
     end if;
  else
     flag := false;
  end if;
  return flag;
end Is_Date;


------------------------------------------------------------------------------
--  IS FIXED
--
-- true if string is a fixed point number
------------------------------------------------------------------------------

function Is_Fixed( s : unbounded_string ) return boolean is
  flag : boolean := true;
  dotcnt : natural := 0;
  ch   : character;
begin
  if length( s ) = 0 then
     flag := false;
  else
     if element( s, 1 ) = '.' then
        flag := false;
     end if;
     if element( s, length( s ) ) = '.' then
        flag := false;
     end if;
     for i in 1..length( s ) loop
         ch := element(s, i );
         if  ch = '.' then
             dotcnt := dotcnt + 1;
         elsif not Is_Digit( ch ) then
            flag := false;
            exit;
         end if;
     end loop;
     if dotcnt /= 1 then
        flag := false;
     end if;
  end if;
  return flag;
end Is_Fixed;


------------------------------------------------------------------------------
-- Basic String Conversions
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  TO LOWER
--
-- Convert to lower-case
------------------------------------------------------------------------------

function ToLower( s : unbounded_string ) return unbounded_string is
  ch : character;
  newstr : unbounded_string;
begin
  newstr := s;
  for i in 1..length( s ) loop
      ch := Element( s, i );
      if ch >= 'A' and ch <= 'Z' then
         ch := character'val( character'pos( ch ) + 32 );
         Replace_Element( newstr, i, ch );
      end if;
  end loop;
  return newstr;
end ToLower;


------------------------------------------------------------------------------
--  TO UPPER
--
-- Convert to upper-case
------------------------------------------------------------------------------

function ToUpper( s : unbounded_string ) return unbounded_string is
  ch : character;
  newstr : unbounded_string;
begin
  newstr := s;
  for i in 1..length( s ) loop
      ch := Element( s, i );
      if ch >= 'a' and ch <= 'z' then
         ch := character'val( character'pos( ch ) - 32 );
         Replace_Element( newstr, i, ch );
      end if;
  end loop;
  return newstr;
end ToUpper;


------------------------------------------------------------------------------
--  TO PROPER
--
-- Convert to proper-case (title case)
------------------------------------------------------------------------------

function ToProper( s : unbounded_string ) return unbounded_string is
  ch        : character;
  newstr    : unbounded_string;
  upperFlag : boolean := true;
begin
  newstr := s;
  for i in 1..length( s ) loop
      ch := Element( s, i );
      if upperFlag then
         if ch >= 'a' and ch <= 'z' then
            ch := character'val( character'pos( ch ) - 32 );
            Replace_Element( newstr, i, ch );
         end if;
      elsif ch >= 'A' and ch <= 'Z' then
         ch := character'val( character'pos( ch ) + 32 );
         Replace_Element( newstr, i, ch );
      end if;
      upperFlag := (ch = ' ') or (ch = '(') or
                   (ch = '-') or (ch = '[') or
                   (ch = '+') or (ch = '{') or
                   (ch = '/'); -- for C's benefit, not on underscore
  end loop;
  return newstr;
end ToProper;


------------------------------------------------------------------------------
--  TO BASIC
--
-- Convert to basic characters
------------------------------------------------------------------------------

function ToBasic( s : unbounded_string ) return unbounded_string is
  ch : character;
  newstr : unbounded_string;
begin
  newstr := s;
  for i in 1..length( s ) loop
      ch := To_Basic( Element( s, i ) );
      Replace_Element( newstr, i, ch );
  end loop;
  return newstr;
end ToBasic;


------------------------------------------------------------------------------
--  TO ESCAPED
--
-- Create a printable string by marking unprintable characters with
-- "[# ascii-code]"
------------------------------------------------------------------------------

function ToEscaped( s : unbounded_string ) return unbounded_string is
  ch : character;
  newstr : unbounded_string;
begin
  for i in 1..length( s ) loop
      ch := Element( s, i );
      if ch < ' ' or ch > '~' then
         newstr := newstr & "[#" & character'pos( ch )'img & "]";
      else
         newstr := newstr & ch;
      end if;
  end loop;
  return newstr;
end ToEscaped;


------------------------------------------------------------------------------
--  TO JSON ESCAPED
--
-- convert special characters in string to JSON escape codes
------------------------------------------------------------------------------

function ToJSONEscaped( s : unbounded_string ) return unbounded_string is
  item : unbounded_string;
  ch : character;
begin
  for i in 1..length( s ) loop
      ch := element( s, i );
      if ch = '"' then
         item := item & "\""";
      elsif ch = '\' then
         item := item & "\\";
      elsif ch = '/' then
         item := item & "\/";
      elsif ch = ASCII.BS then
         item := item & "\b";
      elsif ch = ASCII.FF then
         item := item & "\f";
      elsif ch = ASCII.LF then
         item := item & "\n";
      elsif ch = ASCII.CR then
         item := item & "\r";
      elsif ch = ASCII.HT then
         item := item & "\t";
      else
         item := item & ch;
      end if;
  end loop;
  return item;
end ToJSONEscaped;

------------------------------------------------------------------------------
--  TO JSON UNESCAPED
--
-- convert JSON escape codes in string back to actual characters
------------------------------------------------------------------------------

function ToJSONUnescaped( s : unbounded_string ) return unbounded_string is
  item : unbounded_string;
  ch   : character;
  k    : natural := 1;
begin
  item := null_unbounded_string;
  while k <= length( s ) loop
     ch := element( s, k );
     if ch = '\' then
        k := k + 1;
        exit when k > length( s ); -- don't crash on trailing backslash
        ch := element( s, k );
        if ch = '"' then
           item := item & '"';
        elsif ch = '\' then
           item := item & '\';
        elsif ch = '/' then
           item := item & '/';
        elsif ch =  'b' then
           item := item & ASCII.BS;
        elsif ch = 'f' then
           item := item & ASCII.FF;
        elsif ch = 'n' then
           item := item & ASCII.LF;
        elsif ch = 'r' then
           item := item & ASCII.CR;
        elsif ch = 't' then
           item := item & ASCII.HT;
        end if;
     else
        item := item & ch;
     end if;
      k := k + 1;
  end loop;
  return item;
end ToJSONUnescaped;


------------------------------------------------------------------------------
--  A OR AN
--
-- There are no strict rules, as "a' or "an" depend on the sound of the
-- following word more than the spelling.  Even then, there
-- are exceptions, such as whether h-words sound like a vowel, or acronyms.
------------------------------------------------------------------------------

function AorAN( s : unbounded_string ) return unbounded_string is
  ch  : character := Element( s, 1 );
  ch2 : string(1..2);
begin
  if length( s ) > 0 then
     ch := Element( s, 1 );
  end if;
  if length( s ) > 1 then
     ch2 := ch & Element( s, 2 );
  end if;

  if ch2 = "an" or ch2 = "un" or ch2 = "us" then
     return "a " & s;
  end if;
  if ch='a' or ch='e' or ch='i' or ch='o' or ch='u' or
     ch='A' or ch='E' or ch='I' or ch='O' or ch='U' then
     return "an " & s;
  end if;
  return "a " & s;
end AorAN;


------------------------------------------------------------------------------
--  SPLIT
--
-- Split a string around a space, no longer than width
------------------------------------------------------------------------------

procedure Split( s : unbounded_string; left, right : out unbounded_string;
width : natural ) is
  i    : natural := width;
begin
  if i = 0 then
     left := null_unbounded_string;
     right := s;
     return;
  end if;
  while (i > 0) and (i <= length( s )) loop
     exit when element( s, i ) = ' ';
     i := i-1;
  end loop;
  if i > length( s ) then
     left := s;
     right := null_unbounded_string;
     return;
  elsif i = 0 then
     i := width;
  end if;
  left := to_unbounded_string( slice( s, 1, i ) );
  right := to_unbounded_string( slice( s, i+1, length( s ) ) );
end Split;

------------------------------------------------------------------------------
--  LEVENSHTEIN DISTANE
--
-- A measure of the similarity between two strings.
-- Taken from Rosetta Code website, May 28/2021.
------------------------------------------------------------------------------

function Levenshtein_Distance (S, T : String) return Natural is
   D : array (0 .. S'Length, 0 .. T'Length) of Natural;
begin
   for I in D'Range (1) loop
      D (I, 0) := I;
   end loop;
   for I in D'Range (2) loop
      D (0, I) := I;
   end loop;
   for J in T'Range loop
      for I in S'Range loop
         if S (I) = T (J) then
            D (I, J) := D (I - 1, J - 1);
         else
            D (I, J) :=
               Natural'Min
                 (Natural'Min (D (I - 1, J) + 1, D (I, J - 1) + 1),
                  D (I - 1, J - 1) + 1);
         end if;
      end loop;
   end loop;
   return D (S'Length, T'Length);
end Levenshtein_Distance;


------------------------------------------------------------------------------
--  SOUNDEX
--
-- A representation of the English sound of a string.
-- Taken from Rosetta Code website, May 29/2021.
-- Modified for bugs.
------------------------------------------------------------------------------

function Soundex (instr : String) return String is
   str  : String := To_Upper(instr);
   output : String := "0000";
   spos : Integer := str'First+1;  opos : Positive := 2;
   map  : array(0..255) of Character := (others => ' ');
   last : Integer := str'First;
begin
   if instr'length = 0 then
      return "";
   end if;
   map(65..90) := " 123 12- 22455 12623 1-2 2";
   for i in str'Range loop
      str(i) := map(Character'Pos(to_basic(str(i))));
   end loop;
   output(1) := str(str'First);
   while (opos <= 4 and spos <= str'Last) loop
      if str(spos) /= '-' and str(spos) /= ' ' then
         if (str(spos-1) = '-' and last = spos-2) and then
           (str(spos) = str(spos-2)) then
            null;
         elsif (str(spos) = output(opos-1) and last = spos-1) then
            last := spos;
         else
            output(opos) := str(spos);
            opos := opos + 1;
            last := spos;
         end if;
      end if;
      spos := spos + 1;
   end loop;
   output(1) := To_Upper(instr(instr'First));
   return output;
end Soundex;


------------------------------------------------------------------------------
--  WORD COUNT
--
-- count the number of "words"
------------------------------------------------------------------------------

function WordCount(instr : unbounded_string) return natural is
  str    : constant unbounded_string := ToBasic( instr );
  inWord : boolean := false;
  wcount : natural := 0;
  ch     : character;
begin
  for i in 1..length( str ) loop
      ch := element( str, i );
      if not inWord and is_letter( ch ) then
         inWord := true;
      elsif inWord and not is_letter( ch ) then
         inWord := false;
         wcount := wcount + 1;
      end if;
  end loop;
  if inWord then
     wcount := wcount + 1;
  end if;
  return wcount;
end WordCount;


------------------------------------------------------------------------------
--  TO CSV
--
-- convert s to CSV.  For our purposes, only all numbers will not be quoted.
------------------------------------------------------------------------------

function ToCSV( s : unbounded_string ) return unbounded_string is
  csv : unbounded_string;
  needsQuotes : boolean := false;
  ch : character;
begin
  for i in 1..length( s ) loop
      ch := element( s, i );
      if not ( is_digit( ch ) or ch = '.' ) then
         needsQuotes := true;
      end if;
      if ch = ASCII.Quotation then
         csv := csv & ASCII.Quotation & ASCII.Quotation;
      else
         csv := csv & ch;
      end if;
  end loop;
  -- just to make it clear there's no data, quote it
  if length( csv ) = 0 then
     needsQuotes := true;
  end if;
  if needsQuotes then
     csv := ASCII.Quotation & csv & ASCII.Quotation;
  end if;
  return csv;
end ToCSV;


------------------------------------------------------------------------------
--  CHAR INT TO RESULT
--
-- Convert the first character of result to an signed byte image
------------------------------------------------------------------------------

function charIntToResult( result : unbounded_string ) return unbounded_string is
  type unsigned_byte is mod 256;
  pos : unsigned_byte;
  mask_127 : constant unsigned_byte := 127;
  signedResult : integer;
begin
  if result = null_unbounded_string then
     return to_unbounded_string("0");
  else
     pos := character'pos( element( result, 1 ) );
     if pos > 127 then
        signedResult := -1 - integer( (pos and mask_127) xor mask_127 );
        return to_unbounded_string( signedResult'img );
     else
        return to_unbounded_string( pos'img );
     end if;
  end if;
end charIntToResult;


------------------------------------------------------------------------------
-- String Field Handling
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  STRING FIELD
--
-- return the fth field delimited by delimiter
------------------------------------------------------------------------------

function stringField( s : unbounded_string; delimiter : character; f : natural )
return unbounded_string is
  firstPos    : natural := 1;
  delimCnt    : natural := 0;
  returnStr   : unbounded_string;
begin
  if f = 0 or length( s ) = 0 then
     return null_unbounded_string;
  end if;
  for i in 1..length( s ) loop
      if Element( s, i ) = delimiter then
         delimCnt := delimCnt + 1;
         if delimCnt = f then
            begin
              returnStr := to_unbounded_string( Slice( s, firstPos, i-1 ) );
            exception when others =>
              returnStr := null_unbounded_string;
            end;
            return returnStr;
         end if;
         firstPos := i+1;
      end if;
  end loop;
  if delimCnt+1 < f then
     return null_unbounded_string;
  else
     return to_unbounded_string( Slice( s, firstPos, length( s ) ) );
  end if;
end stringField;


------------------------------------------------------------------------------
--  STRING FIELD
--
-- return the fth field delimited by delimiter (typically a comma) but
-- allow the delimiter to be escaped by double quote marks
-- if allowSingleQuotes is true, allow the field to be enclosed by single
-- quotes as well as double quotes.
------------------------------------------------------------------------------

function stringCSVField( s1 : unbounded_string; delimiter : character;
f : natural; allowSingleQuotes : boolean := false ) return unbounded_string is
  firstPos    : natural := 1;
  delimCnt    : natural := 0;
  inQuotes    : boolean := false;
  returnStr   : unbounded_string;

  function stripQuotes( s : string ) return unbounded_string is
    -- strip enclosing single or double quotes (RFC 4180)
  begin
    if s'length > 1 then
       if s(s'first) = s(s'last) then
          if s(s'first)='"' then
             return to_unbounded_string( s(s'first+1..s'last-1) );
          -- Single quotes are not a part of RFC 4180, but some allow
          elsif allowSingleQuotes then
             if s(s'first)=''' then
                return to_unbounded_string( s(s'first+1..s'last-1) );
             end if;
          end if;
       end if;
    end if;
    return to_unbounded_string( s );
  end stripQuotes;

  function stripEOL( s : unbounded_string ) return unbounded_string is
    -- strip ending CRLF, if it exists (RFC 4180)
    len : constant natural := length( s );
  begin
    if length( s ) > 2 then
      if Element(s, len-1) = ASCII.CR and Element(s,len) = ASCII.LF then
         return head( s, len-2 );
      end if;
    end if;
    return s;
  end stripEOL;

  s : unbounded_string;

begin
  if f = 0 or length( s1 ) = 0 then
     return null_unbounded_string;
  end if;
  s := stripEOL( s1 );
  for i in 1..length( s ) loop
      if Element( s, i ) = delimiter and not inQuotes then
         delimCnt := delimCnt + 1;
         if delimCnt = f then
            begin
              returnStr := stripQuotes( Slice( s, firstPos, i-1 ) );
            exception when others =>
              returnStr := null_unbounded_string;
            end;
            return returnStr;
         end if;
         firstPos := i+1;
      elsif Element( s, i ) = '"' then
         inQuotes := not inQuotes;
      end if;
  end loop;
  if delimCnt+1 < f then
     return null_unbounded_string;
  else
     return stripQuotes (Slice( s, firstPos, length( s ) ) );
  end if;
end stringCSVField;


------------------------------------------------------------------------------
--  REPLACE FIELD
--
-- replace the fth field delimited by delimiter (typically a comma)
------------------------------------------------------------------------------

procedure replaceField( s : in out unbounded_string; delimiter : character;
f : natural; field : string ) is
  firstPos    : natural := 1;
  lastPos     : natural := 1;
  delimCnt    : natural := 0;
begin
  if f = 0 or length( s ) = 0 then
     return;
  end if;
  for i in 1..length( s ) loop
      if Element( s, i ) = delimiter then
         delimCnt := delimCnt + 1;
         if delimCnt = f then
            lastPos := i-1;
            exit;
         end if;
         firstPos := i+1;
      end if;
  end loop;
  if delimCnt+1 < f then
     return;
  elsif delimCnt = f-1 then
     lastPos := length( s );
  end if;
  begin
    s := Delete( s, firstPos, lastPos );
  exception when others =>
    return;
  end;
  Insert( s, firstPos, field );
end replaceField;


------------------------------------------------------------------------------
--  REPLACE CSV FIELD
--
-- replace the fth field delimited by delimiter (typically a comma)
------------------------------------------------------------------------------

procedure replaceCSVField( s : in out unbounded_string; delimiter : character;
f : natural; field : string; allowSingleQuotes : boolean:= false ) is
  firstPos    : natural := 1;
  lastPos     : natural := 1;
  delimCnt    : natural := 0;
  inQuotes    : boolean := false;

  function attachQuotes( s : unbounded_string ) return string is
  begin
    if index( s, "" & delimiter ) > 0 then
       if allowSingleQuotes then
          return to_string("'" & s) & "'";
       else
          return to_string(ASCII.Quotation & s) & ASCII.Quotation;
       end if;
    else
       return to_string( s );
    end if;
  end attachQuotes;

begin
  if f = 0 or length( s ) = 0 then
     return;
  end if;
  for i in 1..length( s ) loop
      if Element( s, i ) = delimiter and not inQuotes then
         delimCnt := delimCnt + 1;
         if delimCnt = f then
            lastPos := i-1;
            exit;
         end if;
         firstPos := i+1;
      elsif Element( s, i ) = '"' then
         if not allowSingleQuotes then
            inQuotes := not inQuotes;
         end if;
      elsif Element( s, i ) = ''' then
         if allowSingleQuotes then
            inQuotes := not inQuotes;
         end if;
      end if;
  end loop;
  if delimCnt+1 < f then
     return;
  elsif delimCnt = f-1 then
     lastPos := length( s );
  end if;
  begin
    s := Delete( s, firstPos, lastPos );
  exception when others =>
    return;
  end;
  Insert( s, firstPos, attachQuotes( to_unbounded_string( field ) ) );
end replaceCSVField;


------------------------------------------------------------------------------
--  STRING LOOKUP
--
-- Treat s (source) as a series of field pairs.  Return the right-hand pair
-- member associated with t (target), or a null string if none exists.  If
-- source or target is a null string, a null string is also returned.
------------------------------------------------------------------------------

function stringLookup( s, t : unbounded_string; delimiter : character )
 return unbounded_string is
  left      : unbounded_string;
  returnStr : unbounded_string;
  firstPos  : natural;
  i         : integer := 1;
begin
  if length( s ) = 0 or length( t ) = 0 then             -- null string(s)?
     return null_unbounded_string;                       -- user error
  end if;
  while i <= length( s ) loop                            -- look for left item
     firstPos := i;                                      -- item starts here
     while i <= length( s ) loop                         -- look for right item
       exit when Element( s, i ) = delimiter;
       i := i + 1;
     end loop;
     left := to_unbounded_string( Slice( s, firstPos, i-1 ) );
     if left = t then                                    -- one we want?
        i := i + 1;                                      -- skip delimiter
        firstPos := i;                                   -- right starts here
        while i <= length( s ) loop                      -- look for end of rgt
           exit when Element( s, i ) = delimiter;        -- end of item
           i := i + 1;                                   -- keep looking
        end loop;
        begin                                            -- extract it
           returnStr := to_unbounded_string( Slice( s, firstPos, i-1 ) );
        exception when others =>
           returnStr := null_unbounded_string;
        end;
        return returnStr;                           -- return it
     else                                           -- not the one?
        i := i + 1;                                 -- skip delimiter
        while i <= length( s ) loop                 -- look for end of rgt
           exit when Element( s, i ) = delimiter;    -- end of item
           i := i + 1;                               -- keep looking
        end loop;
    end if;
    i := i + 1;                                          -- try next char
  end loop;
  return null_unbounded_string;
end stringLookup;


------------------------------------------------------------------------------
--  REPLACE ALL
--
-- Replace all occurrences of a substring.
------------------------------------------------------------------------------

function replaceAll( str_val, needle_val, newstr_val : unbounded_string; sensitive : boolean ) return unbounded_string is
  result : unbounded_string;
begin
  result := str_val;
  if sensitive then
     -- case sensitive
      declare
        needle     : constant string  := to_string( needle_val );
        needle_len : constant natural := length( needle_val );
        newstr     : constant string  := to_string( newstr_val );
        newstr_len : constant natural := length( newstr_val );
        first : positive;
        pos   : natural;
      begin
        first := 1;
        loop
           pos := index( result, needle, first );
        exit when pos = 0;
           replace_slice( result, pos, pos + needle_len - 1, newstr );
           first := pos + newstr_len;
        end loop;
      end;
  else
      -- case insensitive
      declare
        needle     : constant string  := to_string( ToLower( needle_val ) );
        needle_len : constant natural := length( needle_val );
        newstr     : constant string  := to_string( newstr_val );
        newstr_len : constant natural := length( newstr_val );
        searchstr  : unbounded_string  := ToLower( str_val );
        first : positive;
        pos   : natural;
      begin
        first := 1;
        loop
           pos := index( searchstr, needle, first );
        exit when pos = 0;
           replace_slice( result, pos, pos + needle_len - 1, newstr );
           replace_slice( searchstr, pos, pos + needle_len - 1, newstr );
           first := pos + newstr_len;
        end loop;
      end;
  end if;
  return result;
end replaceAll;


------------------------------------------------------------------------------
--  GET DATE STRING
--
-- Convert the calendar time to a human readable string in the format
-- mm/dd hh:mm:ss
------------------------------------------------------------------------------

function getDateString( ct : ada.calendar.time ) return unbounded_string is
   year    : ada.calendar.year_number;
   month   : ada.calendar.month_number;
   day     : ada.calendar.day_number;
   seconds : ada.calendar.day_duration;
   hours   : ada.calendar.day_duration;
   minutes : ada.calendar.day_duration;

   function to2digits( s : string ) return unbounded_string is
      tempStr : unbounded_string;
      dotPos  : natural;
   begin
      tempStr := to_unbounded_string( s );
      delete( tempStr, 1, 1 );
      dotPos := index( tempStr, "." );
      if dotPos > 0 then
         tempStr := head( tempStr, dotPos-1 );
      end if;
      if length( tempStr ) < 2 then
         tempStr := "0" & tempStr;
      end if;
      return tempStr;
   end to2digits;

   timeStr : unbounded_string;

begin

   -- break up the time value

   ada.calendar.Split( ct, year, month, day, seconds );
   hours := duration( float'truncation( float( seconds / (60 * 60) ) ) );
   seconds := seconds - hours * (60* 60);
   minutes := duration( float'truncation( float( seconds / ( 60 ) ) ) );
   seconds := seconds - (minutes * 60);

   -- format it as a string

   timeStr := to2digits( month'img );
   timeStr := timeStr & "/";
   timeStr := timeStr & to2digits( day'img );
   timeStr := timeStr & " ";
   timeStr := timeStr & to2digits( hours'img );
   timeStr := timeStr & ":";
   timeStr := timeStr & to2digits( minutes'img );
   timeStr := timeStr & ":";
   timeStr := timeStr & to2digits( seconds'img );

   return timeStr;
end getDateString;


------------------------------------------------------------------------------
-- Operating System String Handling
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  DIRNAME
--
------------------------------------------------------------------------------

function dirname( s : unbounded_string ) return unbounded_string is
begin
  if length( s ) = 0 then
     return to_unbounded_string( "." );
  end if;
  for i in reverse 2..length( s ) loop
      if element( s, i ) = spar_os.directory_delimiter then
         return to_unbounded_string( slice( s, 1, i-1 ) );
      end if;
  end loop;
  -- root directory
  if element( s, 1 ) = spar_os.directory_delimiter then
     return to_unbounded_string( spar_os.directory_delimiter & "" );
  end if;
  -- else current directory
  return to_unbounded_string( "." );
end dirname;


------------------------------------------------------------------------------
--  BASENAME
--
------------------------------------------------------------------------------

function basename( s : unbounded_string ) return unbounded_string is
begin
  if length( s ) = 0 then
     return null_unbounded_string;
  end if;
  for i in reverse 1..length( s )-1 loop
      if element( s, i ) = spar_os.directory_delimiter then
         return to_unbounded_string( slice( s, i+1, length( s ) ) );
      end if;
  end loop;
  -- root directory
  if element( s, length( s ) ) = spar_os.directory_delimiter then
     return to_unbounded_string( "." );
  end if;
  -- no directory
  return s;
end basename;


------------------------------------------------------------------------------
--  TO SECURE DATA
--
-- If in mantenance mode, return "secured data" instead of the string.
------------------------------------------------------------------------------

function toSecureData( s : string ) return string is
-- return a string if not running in maintenance mode
-- (used in securing data shown in error messages)
begin
  if not maintenanceOpt then
     return s;
  end if;
  return "<secured data>";
end toSecureData;

end pegasoft.strings;

