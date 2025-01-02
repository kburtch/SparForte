------------------------------------------------------------------------------
-- PEGASOFT parent package                                                --
--                                                                          --
-- Part of SparForte                                                        --
-- Designed and Programmed by Ken O. Burtch                                 --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2024 Free Software Foundation              --
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

package body pegasoft is

------------------------------------------------------------------------------
--  TO NUMERIC
--
-- Convert an unbounded string to a long float (Spar's numeric representation)
------------------------------------------------------------------------------

function to_numeric( s : unbounded_string ) return numericValue is
begin
  if Element( s, 1 ) = '-' then                               -- leading -?
     return numericValue'value( to_string( s ) );             -- OK for 'value
  elsif Element( s, 1 ) = ' ' then                            -- leading space?
     return numericValue'value( to_string( s ) );             -- OK for 'value
  else                                                        -- otherwise add
     return numericValue'value( " " & to_string( s ) );       -- space & 'value
  end if;
end to_numeric;


------------------------------------------------------------------------------
--  TO UNBOUNDED STRING
--
-- Convert a long_float (SparForte's numeric representation) to an
-- unbounded string.  If the value is representable as an integer,
-- it is returned without a decimal part.
------------------------------------------------------------------------------

function to_unbounded_string( f : numericValue ) return unbounded_string is
  f_trunc : constant numericValue := numericValue'truncation( f );
begin

  -- integer value?  Try to return without a decimal part
  -- provided it will fit into a long float's mantissa.

   if f - f_trunc = 0.0 then
      -- There's no guarantee that a long_long_integer will fit into
      -- a long_float's mantissa, so we'll use a decimal type.
      if f <= numericValue( integerOutputType'last ) and
         f >= numericValue( integerOutputType'first ) then
         return to_unbounded_string( long_long_integer( f )'img );
      end if;
   end if;

  -- Otherwise, return a long float using 'image

   return to_unbounded_string( numericValue'image( f ) );
end to_unbounded_string;

end pegasoft;
