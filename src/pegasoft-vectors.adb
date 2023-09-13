------------------------------------------------------------------------------
-- Vectors and Extensions                                                   --
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

--with text_io;use text_io;

package body pegasoft.vectors is


------------------------------------------------------------------------------
--
-- Extensions
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  APPEND
--
------------------------------------------------------------------------------

procedure append( v : in out vector_string_lists.vector; i : vector_index; e : unbounded_string ) is
  the_string : unbounded_string;
begin
  the_string := Vector_String_Lists.Element( v, i );
  the_string := the_string & e;
  Vector_String_Lists.Replace_Element( v, i, the_string );
end append;


------------------------------------------------------------------------------
--  PREPEND
--
------------------------------------------------------------------------------

procedure prepend( v : in out vector_string_lists.vector; i : vector_index; e : unbounded_string ) is
  the_string : unbounded_string;
begin
  the_string := Vector_String_Lists.Element( v, i );
  the_string := e & the_string;
  Vector_String_Lists.Replace_Element( v, i, the_string );
end prepend;


------------------------------------------------------------------------------
--  INCREMENT
--
------------------------------------------------------------------------------

procedure increment( v : in out vector_string_lists.vector; i : vector_index; n : numericValue ) is
  the_string : unbounded_string;
begin
  the_string := Vector_String_Lists.Element( v, i );
  the_string := to_unbounded_string( to_numeric( the_string ) + n );
  Vector_String_Lists.Replace_Element( v, i, the_string );
end increment;


------------------------------------------------------------------------------
--  DECREMENT
--
------------------------------------------------------------------------------

procedure decrement( v : in out vector_string_lists.vector; i : vector_index; n : numericValue ) is
  the_string : unbounded_string;
begin
  the_string := Vector_String_Lists.Element( v, i );
  the_string := to_unbounded_string( to_numeric( the_string ) - n );
  Vector_String_Lists.Replace_Element( v, i, the_string );
end decrement;

------------------------------------------------------------------------------
--  EXTRACT
--
------------------------------------------------------------------------------


end pegasoft.vectors;
