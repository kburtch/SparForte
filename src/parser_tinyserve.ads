------------------------------------------------------------------------------
-- TinyServe  Package Parser                                                --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

package parser_tinyserve is

------------------------------------------------------------------------------
-- TinyServe package identifiers
------------------------------------------------------------------------------

tinyserve_socket_server_t       : identifier;
tinyserve_client_id_t           : identifier;

tinyserve_new_socket_server_t   : identifier;
tinyserve_startup_t             : identifier;
tinyserve_shutdown_t            : identifier;
tinyserve_manage_connections_t  : identifier;
tinyserve_get_next_client_t     : identifier;
tinyserve_get_listener_socket_t : identifier;
tinyserve_count_clients_t       : identifier;
tinyserve_get_fdset_size_t      : identifier;
tinyserve_client_might_not_block_on_write_t : identifier;
tinyserve_establish_t           : identifier;
tinyserve_is_timeout_t          : identifier;
tinyserve_close_t               : identifier;
tinyserve_drop_t                : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupTinyserve;
procedure ShutdownTinyserve;

end parser_tinyserve;
