------------------------------------------------------------------------------
-- Singly Linked Lists Package Parser                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2014 Free Software Foundation              --
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

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

package parser_vectors is

------------------------------------------------------------------------------
-- Containers package identifiers
------------------------------------------------------------------------------

--containers_count_type_t : identifier;
--containers_hash_type_t  : identifier;

------------------------------------------------------------------------------
-- Doubly Linked Lists package identifiers
------------------------------------------------------------------------------

vectors_vector_t        : identifier;
vectors_cursor_t        : identifier;

vectors_new_vector_t    : identifier;
vectors_clear_t         : identifier;
vectors_to_vector_t     : identifier;
vectors_capacity_t      : identifier;
vectors_reserve_capacity_t : identifier;
vectors_length_t        : identifier;
vectors_set_length_t    : identifier;
vectors_is_empty_t      : identifier;
vectors_append_t        : identifier;
vectors_prepend_t       : identifier;
vectors_first_index_t   : identifier;
vectors_last_index_t    : identifier;
vectors_element_t       : identifier;
vectors_first_element_t : identifier;
vectors_last_element_t  : identifier;
--doubly_length_t        : identifier;
--doubly_append_t        : identifier;
--doubly_prepend_t       : identifier;
--doubly_first_element_t : identifier;
--doubly_last_element_t  : identifier;
--doubly_delete_first_t  : identifier;
--doubly_delete_last_t   : identifier;

--doubly_new_cursor_t    : identifier;
--doubly_first_t         : identifier;
--doubly_last_t          : identifier;
--doubly_next_t          : identifier;
--doubly_previous_t      : identifier;
--doubly_element_t       : identifier;
--doubly_replace_element_t : identifier;
--doubly_insert_before_t : identifier;
--doubly_insert_before_and_mark_t : identifier;
--doubly_delete_t        : identifier;
--doubly_contains_t      : identifier;
--doubly_find_t          : identifier;
--doubly_reverse_find_t  : identifier;

--doubly_reverse_elements_t : identifier;
--doubly_flip_t          : identifier;
--doubly_assign_t        : identifier;
--doubly_move_t          : identifier;
--doubly_swap_t          : identifier;
--doubly_swap_links_t    : identifier;
--doubly_splice_t        : identifier;
--doubly_has_element_t   : identifier;

--doubly_assemble_t      : identifier;
--doubly_disassemble_t   : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupVectors;
procedure ShutdownVectors;

end parser_vectors;
