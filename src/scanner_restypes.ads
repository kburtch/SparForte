------------------------------------------------------------------------------
-- Scanner resource types                                                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2019 Free Software Foundation              --
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

with world,
     scanner;
use  world,
     scanner;

package scanner_restypes is

------------------------------------------------------------------------------
-- Resource Using Types (Temporary)
--
-- These are temporarily stored here, though they belong to other packages,
-- as they are needed in the main parser for declarations of parameterized
-- generic types.  They are put here to avoid importing the entire package
-- they belong to in the main parser.
------------------------------------------------------------------------------

btree_file_t          : identifier;
btree_cursor_t        : identifier;

dht_table_t           : identifier;

doubly_list_t         : identifier;
doubly_cursor_t       : identifier;

hash_file_t          : identifier;
hash_cursor_t        : identifier;

end scanner_restypes;

