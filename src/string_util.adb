------------------------------------------------------------------------------
-- STRING UTIL                                                              --
--                                                                          --
-- Part of SparForte                                                        --
-- Designed and Programmed by Ken O. Burtch                                 --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------
-- CVS: $Id: string_util.adb,v 1.2 2005/02/11 02:59:31 ken Exp $

pragma suppress( index_check );
pragma suppress( range_check );

with bush_os, Ada.Characters.Handling;
use  Ada.Characters.Handling;

package body string_util is

------------------------------------------------------------------------------
-- Misc String Handling
------------------------------------------------------------------------------

procedure FixSpacing( s : in out unbounded_string; inside : boolean := true ) is
-- remove leading and trailing spaces, as well as any double-spaces inside
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

function TypoOf( BadString, GoodString : unbounded_string ) return boolean is
-- 80% of all typos are single insertions, deletions, exchanges, or subs.
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

function Is_Control( s : unbounded_string ) return boolean is
-- true if string is completely control characters
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

function Is_Graphic( s : unbounded_string ) return boolean is
-- true if string is completely printable characters
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

function Is_Letter( s : unbounded_string ) return boolean is
-- true if string is completely letter characters
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

function Is_Lower( s : unbounded_string ) return boolean is
-- true if string is completely lower-case characters
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

function Is_Upper( s : unbounded_string ) return boolean is
-- true if string is completely Upper-case characters
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

function Is_Basic( s : unbounded_string ) return boolean is
-- true if string is completely basic characters
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

function Is_Digit( s : unbounded_string ) return boolean is
-- true if string is completely digit characters
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

function Is_Hexadecimal_Digit( s : unbounded_string ) return boolean is
-- true if string is completely hexadecimal digit characters
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

function Is_Alphanumeric( s : unbounded_string ) return boolean is
-- true if string is completely hexadecimal digit characters
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

function Is_Special( s : unbounded_string ) return boolean is
-- true if string is completely hexadecimal digit characters
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

function Is_Date( s : unbounded_string ) return boolean is
-- true if string looks like a slashed date
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

function Is_Fixed( s : unbounded_string ) return boolean is
-- true if string is a fixed point number
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

function ToEscaped( s : unbounded_string ) return unbounded_string is
-- Create a printable string by marking unprintable characters with
-- "[# ascii-code]"
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


function ToJSONEscaped( s : unbounded_string ) return unbounded_string is
-- convert special characters in string to JSON escape codes
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

function AorAN( s : unbounded_string ) return unbounded_string is
  ch : character := Element( s, 1 );
begin
  if ch='a' or ch='e' or ch='i' or ch='o' or ch='u' or
     ch='A' or ch='E' or ch='I' or ch='O' or ch='U' then
     return "an " & s;
  end if;
  return "a " & s;
end AorAN;

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
-- String Field Handling
------------------------------------------------------------------------------

function stringField( s : unbounded_string; delimiter : character; f : natural )
return unbounded_string is
-- return the fth field delimited by delimiter
  firstPos    : natural := 1;
  currentPos  : natural := 1;
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

function stringCSVField( s : unbounded_string; delimiter : character;
f : natural ) return unbounded_string is
-- return the fth field delimited by delimiter (typically a comma) but
-- allow the delimiter to be escaped by double quote marks
  firstPos    : natural := 1;
  currentPos  : natural := 1;
  delimCnt    : natural := 0;
  inQuotes    : boolean := false;
  returnStr   : unbounded_string;

  function stripQuotes( s : string ) return unbounded_string is
  begin
    if s'length > 1 then
       if s(s'first) = s(s'last) and s(s'first)='"' then
          return to_unbounded_string( s(s'first+1..s'last-1) );
       end if;
    end if;
    return to_unbounded_string( s );
  end stripQuotes;

begin
  if f = 0 or length( s ) = 0 then
     return null_unbounded_string;
  end if;
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

procedure replaceField( s : in out unbounded_string; delimiter : character;
f : natural; field : string ) is
  firstPos    : natural := 1;
  lastPos     : natural := 1;
  currentPos  : natural := 1;
  delimCnt    : natural := 0;
  returnStr   : unbounded_string;
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

procedure replaceCSVField( s : in out unbounded_string; delimiter : character;
f : natural; field : string ) is
  firstPos    : natural := 1;
  lastPos     : natural := 1;
  currentPos  : natural := 1;
  delimCnt    : natural := 0;
  returnStr   : unbounded_string;
  inQuotes    : boolean := false;

  function attachQuotes( s : unbounded_string ) return string is
  begin
    if index( s, ""&delimiter ) > 0 then
       return to_string(ASCII.Quotation & s) & ASCII.Quotation;
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
         inQuotes := not inQuotes;
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

function stringLookup( s, t : unbounded_string; delimiter : character )
 return unbounded_string is
-- Treat s (source) as a series of field pairs.  Return the right-hand pair
-- member associated with t (target), or a null string if none exists.  If
-- source or target is a null string, a null string is also returned.
  left      : unbounded_string;
  returnStr : unbounded_string;
  firstPos  : natural;
  i         : integer := 1;
  result    : unbounded_string := null_unbounded_string;
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
-- GET DATE STRING
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

function dirname( s : unbounded_string ) return unbounded_string is
begin
  if length( s ) = 0 then
     return to_unbounded_string( "." );
  end if;
  for i in reverse 2..length( s ) loop
      if element( s, i ) = bush_os.directory_delimiter then
         return to_unbounded_string( slice( s, 1, i-1 ) );
      end if;
  end loop;
  -- root directory
  if element( s, 1 ) = bush_os.directory_delimiter then
     return to_unbounded_string( bush_os.directory_delimiter & "" );
  end if;
  -- else current directory
  return to_unbounded_string( "." );
end dirname;

function basename( s : unbounded_string ) return unbounded_string is
begin
  if length( s ) = 0 then
     return null_unbounded_string;
  end if;
  for i in reverse 1..length( s )-1 loop
      if element( s, i ) = bush_os.directory_delimiter then
         return to_unbounded_string( slice( s, i+1, length( s ) ) );
      end if;
  end loop;
  -- root directory
  if element( s, length( s ) ) = bush_os.directory_delimiter then
     return to_unbounded_string( "." );
  end if;
  -- no directory
  return s;
end basename;

end string_util;

