------------------------------------------------------------------------------
-- Convertings strings, numbers and enums to SparForte values               --
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

package body value_conversion is

function to_numeric( id : identifier ) return numericValue is
-- Look up an identifier's value and return it as a long float
-- (Spar's numeric representation).
begin
   return to_numeric( identifiers( id ).store.value );
end to_numeric;

function to_spar_boolean( AdaBoolean : boolean ) return unbounded_string is
  -- convert an Ada Boolean into a SparForte Boolean (a string containing
  -- the position, no leading blank).
begin
  return To_Unbounded_String( integer'image( boolean'pos( AdaBoolean ) )(2)&"" );
end to_spar_boolean;

end value_conversion;

