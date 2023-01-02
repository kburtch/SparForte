------------------------------------------------------------------------------
-- Numeric Extension Functions                                              --
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

with ada.numerics.float_random;


package body pegasoft.numerics is

random_generator : ada.numerics.float_random.generator;


------------------------------------------------------------------------------
--
-- Extensions
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  RND
--
-- Return a uniformly distributed integer between 1 and max
------------------------------------------------------------------------------

function rnd( max : positive ) return positive is
  randomFloat : float;
  result : positive;
begin
    -- Kludge: Random produces 0..1.0, but 1.0 is too large.  Probably a
    -- better way to handle this...
    loop
       randomFloat := Ada.Numerics.Float_Random.Random( random_generator );
        exit when randomFloat /= 1.0;
     end loop;
     result := positive(  1.0 +
        float'truncation( float( max ) *
        randomFloat ) );
     return result;
end rnd;


------------------------------------------------------------------------------
--  HASH OF
--
--
------------------------------------------------------------------------------

function hash_of( val : unbounded_string; limit : hash_integer ) return hash_integer is
   hash : hash_integer := 5381;
begin
   for i in 1..length( val) loop
       hash := (hash*37 + hash) + character'pos(element(val,i));
   end loop;
   hash := (hash mod limit) + 1;
   return hash;
end hash_of;


------------------------------------------------------------------------------
--  SDBM HASH OF
--
--
------------------------------------------------------------------------------

function sdbm_hash_of( val : unbounded_string; limit : hash_integer ) return hash_integer is
   hash : hash_integer := 0;
begin
   for i in 1..length(val) loop
       hash := character'pos(element(val,i)) + (hash*64) + (hash*65536) - hash;
   end loop;
   hash := (hash mod limit) + 1;
   return hash;
end sdbm_hash_of;


------------------------------------------------------------------------------
--  FNV HASH OF
--
--
------------------------------------------------------------------------------

function fnv_hash_of( val : unbounded_string; limit : hash_integer ) return hash_integer is
   hash   : hash_integer := 16#811c9dc5#;
   k      : hash_integer;
begin
   for data in 1..length(val)-3 loop
       k := character'pos( element(val, data) ) +
            character'pos( element(val, data+1) ) * 256 +     -- 8
            character'pos( element(val, data+2) ) * 65536 +   -- 16
            character'pos( element(val, data+3) ) * 16777216; -- 24
       hash := hash xor k;
       hash := hash * 16#01000193#;
   end loop;
   hash := (hash mod limit) + 1;
   return hash;
end fnv_hash_of;


------------------------------------------------------------------------------
--  MURMUR HASH OF
--
--
------------------------------------------------------------------------------

function murmur_hash_of( val : unbounded_string; limit : hash_integer ) return hash_integer is
    seed : constant hash_integer := 16#811c9dc5#;
    m : constant hash_integer := 16#5bd1e995#;
    r : constant hash_integer := 24;
    hash : hash_integer;
    data : integer := 1;
    k    : hash_integer;
    s    : constant string := to_string( val );
    len  : integer := s'length;
  begin
     -- this seed could be more elegant - random seed here xor len
     hash := hash_integer( s'length ) xor seed;
     while len >= 4 loop
        k := character'pos( s(data) ) +
             character'pos( s(data+1) ) * 256 +      -- 8
             character'pos( s(data+2) ) * 65536 +    -- 16
             character'pos( s(data+3) ) * 16777216;  -- 24
        k := k * m;
        k := k xor (k / (2**integer(r)));
        k := k * m;
        hash := hash * m;
        hash := hash xor k;

        data := data + 4;
        len := len - 4;
     end loop;

     -- Handle the last few bytes of the input array

     case len is
     when 3 => hash := hash xor ( character'pos( s(data+2) ) * 65536 );
               hash := hash xor ( character'pos( s(data+1) ) * 256 );
               hash := hash xor ( character'pos( s(data) ) );
               hash := hash * m;
     when 2 => hash := hash xor ( character'pos( s(data+1) ) * 256 );
               hash := hash xor ( character'pos( s(data) ) );
               hash := hash * m;
     when 1 => hash := hash xor ( character'pos( s(data) ) );
               hash := hash * m;
     when others => null;
     end case;

     -- Do a few final mixes of the hash to ensure the last few
     -- bytes are well-incorporated.

     hash := hash xor ( hash / 8192 ); -- 13
     hash := hash * m;
     hash := hash xor ( hash / 32768 ); -- 15

     hash := (hash mod limit) + 1;

   return hash;
end murmur_hash_of;

begin
   Ada.Numerics.Float_Random.Reset( random_generator );-- reset RND generator
end pegasoft.numerics;
