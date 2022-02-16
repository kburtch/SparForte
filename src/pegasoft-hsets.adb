------------------------------------------------------------------------------
-- Hashed Sets and Extensions                                               --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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

--with text_io;use text_io;

package body pegasoft.hsets is

type hash_integer is mod 2**32;

------------------------------------------------------------------------------
--  STRING HASHED SETS HASH
--
-- FVN hash (see parser_numerics)
------------------------------------------------------------------------------

function String_Hashed_Sets_Hash( value : unbounded_string ) return Ada.Containers
.Hash_Type is

  hash   : hash_integer := 16#711c9dc5#; -- was 16#8
  v      : hash_integer;
  limit  : hash_integer;
begin
  limit := hash_integer( Ada.Containers.Hash_Type'last );
  for data in 1..length(value)-3 loop
      v := character'pos( element(value, data) ) +
           character'pos( element(value, data+1) ) * 256 +     -- 8
           character'pos( element(value, data+2) ) * 65536 +   -- 16
           character'pos( element(value, data+3) ) * 16777216; -- 24
       hash := hash xor v;
       hash := hash * 16#01000193#;
  end loop;
  hash := (hash mod limit);
  return Ada.Containers.Hash_Type( hash );
end String_Hashed_Sets_Hash;


------------------------------------------------------------------------------
--
-- Extensions
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  APPEND
--
------------------------------------------------------------------------------

--procedure append( m : in out string_hashed_maps.map; k, e : unbounded_string ) is
--begin
--  String_Hashed_Maps.Include( m, k, String_Hashed_Maps.Element( m, k ) & e );
--end append;


------------------------------------------------------------------------------
--  PREPEND
--
------------------------------------------------------------------------------

--procedure prepend( m : in out string_hashed_maps.map; k, e : unbounded_string ) is
--begin
--  String_Hashed_Maps.Include( m, k, e & String_Hashed_Maps.Element( m, k ) );
--end prepend;


------------------------------------------------------------------------------
--  INCREMENT
--
------------------------------------------------------------------------------

--procedure increment( m : in out string_hashed_maps.map; k : unbounded_string; n : long_float ) is
--  floatVal : long_float;
--begin
--  floatVal := long_float( to_numeric( String_Hashed_Maps.Element( m, k ) ) ) + n;
--  String_Hashed_Maps.Include( m, k, to_unbounded_string( floatVal'img ) );
--end increment;


------------------------------------------------------------------------------
--  DECREMENT
--
------------------------------------------------------------------------------

--procedure decrement( m : in out string_hashed_maps.map; k : unbounded_string; n : long_float ) is
--  floatVal : long_float;
--begin
--  floatVal := long_float( to_numeric( String_Hashed_Maps.Element( m, k ) ) ) - n;
--  String_Hashed_Maps.Include( m, k, to_unbounded_string( floatVal'img ) );
--end decrement;


------------------------------------------------------------------------------
--  EXTRACT
--
------------------------------------------------------------------------------

--function extract( m : in out string_hashed_maps.map; k : unbounded_string ) return unbounded_string is
--  result : unbounded_string;
--begin
--  result := String_Hashed_Maps.Element( m, k );
--  String_Hashed_Maps.Delete( m, k );
--  return result;
--end extract;

end pegasoft.hsets;
