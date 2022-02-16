------------------------------------------------------------------------------
-- Hashed Maps and Extensions                                               --
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

with Ada.Strings.Unbounded,
     Ada.Containers.Hashed_Maps;

use  Ada.Strings.Unbounded;

package pegasoft.hmaps is

-- Hashed Maps
--
-- A hash map is a hash in Perl or a Python dictionary

function String_Hashed_Maps_Hash( key : unbounded_string ) return Ada.Containers
.Hash_Type;

package string_hashed_maps is
  new Ada.Containers.Hashed_Maps
    (Key_Type        => unbounded_string,
     Element_Type    => unbounded_string,
     Hash            => String_Hashed_Maps_Hash,
     Equivalent_Keys => "=");

------------------------------------------------------------------------------
--
-- Extensions
--
------------------------------------------------------------------------------

procedure append( m : in out string_hashed_maps.map; k, e : unbounded_string );

procedure prepend( m : in out string_hashed_maps.map; k, e : unbounded_string );

procedure increment( m : in out string_hashed_maps.map; k : unbounded_string; n : long_float );

procedure decrement( m : in out string_hashed_maps.map; k : unbounded_string; n : long_float );

procedure extract( m : in out string_hashed_maps.map; k : unbounded_string; result :out unbounded_string );

end pegasoft.hmaps;
