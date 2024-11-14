------------------------------------------------------------------------------
-- Numeric Extension Functions                                              --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2023 Free Software Foundation              --
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

with ada.numerics.generic_elementary_functions;
with ada.numerics.generic_complex_types;

package pegasoft.numerics is

type hash_integer is mod 2**32;

package elementary_functions is new ada.numerics.generic_elementary_functions( numericValue );
-- elementary math functions

package complex_types is new ada.numerics.generic_complex_types( numericValue );
-- complex numbers and their functions

------------------------------------------------------------------------------
--
-- Extensions
--
------------------------------------------------------------------------------

function fnv_hash_of( val : unbounded_string; limit : hash_integer ) return hash_integer;

function hash_of( val : unbounded_string; limit : hash_integer ) return hash_integer;

function murmur_hash_of( val : unbounded_string; limit : hash_integer ) return hash_integer;

function rnd( max : positive ) return positive;

function sdbm_hash_of( val : unbounded_string; limit : hash_integer ) return hash_integer;

function shannon_entropy_of( s : unbounded_string ) return numericValue;

end pegasoft.numerics;
