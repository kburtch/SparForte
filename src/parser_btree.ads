------------------------------------------------------------------------------
-- Berkeley DB BTree File Package Parser                                    --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2015 Free Software Foundation              --
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

package parser_btree is

------------------------------------------------------------------------------
-- BTree package identifiers
------------------------------------------------------------------------------

btree_file_t          : identifier;
btree_cursor_t        : identifier;

btree_new_file_t      : identifier;
btree_clear_t         : identifier;

btree_create_t        : identifier;
btree_close_t         : identifier;
btree_open_t          : identifier;
btree_delete_t        : identifier;
btree_is_open_t       : identifier;
btree_name_t          : identifier;

btree_set_t           : identifier;
btree_get_t           : identifier;
btree_has_element_t   : identifier;
btree_remove_t        : identifier;
btree_increment_t     : identifier;
btree_decrement_t     : identifier;
btree_add_t           : identifier;
btree_replace_t       : identifier;
btree_append_t        : identifier;
btree_prepend_t       : identifier;

btree_new_cursor_t    : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupBTree;
procedure ShutdownBTree;

end parser_btree;

