------------------------------------------------------------------------------
-- Conversion from characters to byte code                                  --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with pegasoft.gen_list;

package world.byte_conversion is

function toHighASCII( ch : character ) return character;
function toHighASCII( id : identifier ) return character;
-- add 128 to a character

function toLowASCII( ch : character ) return character;
function toLowASCII( id : identifier ) return character;
-- subtract 128 from a character

-- This is a two-character natural in the byte code with first byte
-- having >= 128 and no zeros.  This is the maximum possible number
-- of reserved words.

subtype byteCodeIdentifier is positive range 1..8192;

subtype byteCodeNatural    is natural;

procedure toByteCode( id : reservedWordRange; ch1, ch2 : out character );
function toByteCode( id : reservedWordRange ) return string;
pragma inline( toByteCode );
-- Note: probably won't inline due to exception handler and unconstrained
-- string type

procedure toIdentifier( ch1, ch2 : character; id : out reservedWordRange; advance : out integer );
pragma inline( toIdentifier );

end world.byte_conversion;
