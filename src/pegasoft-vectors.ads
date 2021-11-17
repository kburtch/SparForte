------------------------------------------------------------------------------
-- Vectors and Extensions                                                   --
--                                                                          --
-- Part of SparForte                                                        --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded,
     Ada.Containers.Vectors;

use  Ada.Strings.Unbounded;

package pegasoft.vectors is

-- Vectors
--
-- A vector is a singly linked list.

type vector_index is new natural;

package vector_string_lists is new Ada.Containers.Vectors(
   vector_index,
   unbounded_string,
   "="
);

------------------------------------------------------------------------------
--
-- Extensions
--
------------------------------------------------------------------------------

procedure append( v : in out vector_string_lists.vector; i :vector_index; e : unbounded_string );

procedure prepend( v : in out vector_string_lists.vector; i :vector_index; e : unbounded_string );

procedure increment( v : in out vector_string_lists.vector; i : vector_index; n : long_float );

procedure decrement( v : in out vector_string_lists.vector; i : vector_index; n : long_float );


-- function extract( m : in out string_hashed_maps.map; k : unbounded_string ) return unbounded_string;

end pegasoft.vectors;
