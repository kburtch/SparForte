------------------------------------------------------------------------------
-- Memcache Package Parser                                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2011 Free Software Foundation              --
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

with ada.strings.unbounded, ada.numerics.float_random, world;
use ada.strings.unbounded, world;

package parser_memcache is

------------------------------------------------------------------------------
-- Numerics package identifiers
------------------------------------------------------------------------------

memcache_cluster_t               : identifier;
memcache_cluster_type_t          : identifier;
memcache_cluster_type_normal_t   : identifier;

memcache_is_valid_memcache_key_t : identifier;
memcache_new_cluster_t           : identifier;
memcache_clear_servers_t         : identifier;
memcache_register_server_t       : identifier;
memcache_set_cluster_name_t      : identifier;
memcache_set_cluster_type_t      : identifier;
memcache_set_t                   : identifier;
memcache_add_t                   : identifier;
memcache_replace_t               : identifier;
memcache_append_t                : identifier;
memcache_prepend_t               : identifier;
memcache_get_t                   : identifier;
memcache_delete_t                : identifier;
memcache_stats_t                 : identifier;
memcache_version_t               : identifier;
memcache_flush_t                 : identifier;

highread_new_cluster_t           : identifier;
highread_clear_servers_t         : identifier;
highread_register_server_t       : identifier;
highread_set_cluster_name_t      : identifier;
highread_set_cluster_type_t      : identifier;
highread_set_t                   : identifier;
highread_add_t                   : identifier;
highread_replace_t               : identifier;
highread_append_t                : identifier;
highread_prepend_t               : identifier;
highread_get_t                   : identifier;
highread_delete_t                : identifier;
highread_stats_t                 : identifier;
highread_version_t               : identifier;
highread_flush_t                 : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupMemcache;
procedure ShutdownMemcache;

------------------------------------------------------------------------------
-- PARSE THE NUMERICS PACKAGE
------------------------------------------------------------------------------

procedure ParseMemcacheIsValidMemcacheKey( result : out unbounded_string );
procedure ParseMemcacheNewCluster( result : out unbounded_string );
procedure ParseMemcacheRegisterServer;
procedure ParseMemcacheClearServers;
procedure ParseMemcacheSetClusterName;
procedure ParseMemcacheSetClusterType;
procedure ParseMemcacheSet;
procedure ParseMemcacheAdd;
procedure ParseMemcacheReplace;
procedure ParseMemcacheAppend;
procedure ParseMemcachePrepend;
procedure ParseMemcacheGet( result : out unbounded_string );
procedure ParseMemcacheDelete;
procedure ParseMemcacheStats( result : out unbounded_string );
procedure ParseMemcacheVersion( result : out unbounded_string );
procedure ParseMemcacheFlush;

end parser_memcache;
