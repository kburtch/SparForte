------------------------------------------------------------------------------
-- Dynmaic Hash Tables Package Parser                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2013 Free Software Foundation              --
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

package parser_dht is

------------------------------------------------------------------------------
-- Dynmaic Hash Tables package identifiers
------------------------------------------------------------------------------

dht_table_t         : identifier;

dht_new_table_t     : identifier;
dht_set_t           : identifier;
dht_reset_t         : identifier;
dht_get_t           : identifier;
dht_has_element_t   : identifier;
dht_remove_t        : identifier;
dht_get_first_t     : identifier;
dht_get_next_t      : identifier;

dht_add_t           : identifier;
dht_append_t        : identifier;
dht_prepend_t       : identifier;
dht_replace_t       : identifier;
dht_increment_t     : identifier;
dht_decrement_t     : identifier;

--dht_assemble_t      : identifier;
--dht_disassemble_t   : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupDHT;
procedure ShutdownDHT;

end parser_dht;
