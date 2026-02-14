------------------------------------------------------------------------------
-- Conversion from characters to byte code                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2026 Free Software Foundation              --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Source_Info;

package body world.byte_conversion is

-----------------------------------------------------------------------------
-- TO HIGH ASCII
--
-- Set the high bit (bit 8) on a low ASCII (7 bit) character.  Also, same for
-- an identifier to a high ASCII character.
-----------------------------------------------------------------------------

function toHighASCII( ch : character ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if ch > ASCII.DEL then
      raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot set high bit on character" & character'pos( ch )'img;
   end if;
   return character'val( 128+character'pos( ch ) );
end toHighASCII;

function toHighASCII( id : identifier ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if id > 127 then
      raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot set high bit on identifier number" & id'img;
   end if;
   return character'val( 128+integer(id) );
end toHighASCII;

-----------------------------------------------------------------------------
-- TO BYTE CODE
--
-- Encodes an id number into one or two bytes.  Zero is reserved as the
-- end-of-line marker.  The second byte may exceed ASCII 128.  When an
-- identifier is one byte, ch2 is ASCII.NUL
--
-- First    Second Values   Comment
--   1..127 -      -        reserved for ASCII
-- 128..223 -      1..96    first 96 reserved identifiers, one byte
-- 224..255 1..255 96..8192 values  remaining identifiers, two bytes
--
-- The objective is to allow for more than 128 reserved words, while at the
-- same time, keeping the most common reserved words in a single byte.
-----------------------------------------------------------------------------

procedure toByteCode( id : reservedWordRange; ch1, ch2 : out character ) is
begin
  if id <= 96 then
     ch1 := character'val( 128 + integer(id-1) );
     ch2 := ASCII.NUL;
  else
     ch1 := character'val( 224 + ((integer(id-1)- 32) mod 32) );
     ch2 := character'val( (integer(id-1)-32) / 32 + 1 );
  end if;
exception when constraint_error =>
  ch1 := ASCII.DEL;
  ch2 := ASCII.DEL;
  raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot encode natural number" & id'img;
end toByteCode;

function toByteCode( id : reservedWordRange ) return string is
  ch1, ch2 : character;
begin
  toByteCode( id, ch1, ch2 );
  if ch2 = ASCII.NUL then
     return ch1 & "";
  end if;
  return ch1 & ch2;
end toByteCode;

-----------------------------------------------------------------------------
-- TO IDENTIFIER
--
-- Decode a two-character byte code sequence into an identifier.  See
-- toByteCode for the format the characters.  advance is the number of
-- characters decoded (1 = ch1 only, 2 = both characters ).
-----------------------------------------------------------------------------

procedure toIdentifier( ch1, ch2 : character; id : out reservedWordRange; advance : out integer ) is
  i1 : constant reservedWordRange := character'pos( ch1 );
  i2 : reservedWordRange;
begin
  if i1 < 128 then
    raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: byte code sequence is not an identifier: " & i1'img;
  elsif i1 < 224 then
     id := reservedWordRange( i1 - 127 );
     advance := 1;
  else
     i2 := character'pos( ch2 );
     id := reservedWordRange( i1 - 223 ) + 32 * i2;
     advance := 2;
  end if;
end toIdentifier;

-----------------------------------------------------------------------------
-- TO LOW ASCII
--
-- Clear the high bit (bit 8) on a high ASCII (7 bit) character.  Also, same
-- for an identifier to a low ASCII character.
-----------------------------------------------------------------------------

function toLowASCII( ch : character ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if ch <= ASCII.DEL then
      raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot clear high bit on character" & character'pos( ch )'img;
   end if;
   return character'val( character'pos( ch )-128 );
end toLowASCII;

function toLowASCII( id : identifier ) return character is
   pragma suppress( RANGE_CHECK );
   -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
   -- exception.  We'll do the range checking manually as a work around...
begin
   if id <= 128 then
      raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location & ": Internal error: cannot clear high bit on identifier number" & id'img;
   end if;
   return character'val( integer(id)-128 );
end toLowASCII;

end world.byte_conversion;

