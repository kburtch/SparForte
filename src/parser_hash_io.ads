------------------------------------------------------------------------------
-- Berkeley DB Hash File Package Parser                                     --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2016 Free Software Foundation              --
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

package parser_hash_io is

------------------------------------------------------------------------------
-- Hash package identifiers
------------------------------------------------------------------------------

hash_file_t          : identifier;
hash_cursor_t        : identifier;
hash_db_error_t      : identifier;

--btree_DB_OK_t         : identifier;
--btree_DB_BUFFER_SMALL_t         : identifier;
--btree_DB_DONOTINDEX_t         : identifier;
--btree_DB_FOREIGN_CONFLICT_t         : identifier;
--btree_DB_KEYEMPTY_t         : identifier;
--btree_DB_KEYEXIST_t         : identifier;
--btree_DB_LOCK_DEADLOCK_t         : identifier;
--btree_DB_LOCK_NOTGRANTED_t         : identifier;
--btree_DB_LOG_BUFFER_FULL_t         : identifier;
--btree_DB_NOSERVER_t         : identifier;
--btree_DB_NOSERVER_HOME_t         : identifier;
--btree_DB_NOSERVER_ID_t         : identifier;
--btree_DB_NOTFOUND_t         : identifier;
--btree_DB_OLD_VERSION_t         : identifier;
--btree_DB_PAGE_NOTFOUND_t         : identifier;
--btree_DB_REP_DUPMASTER_t         : identifier;
--btree_DB_REP_HANDLE_DEAD_t         : identifier;
--btree_DB_REP_HOLDELECTION_t         : identifier;
--btree_DB_REP_IGNORE_t         : identifier;
--btree_DB_REP_ISPERM_t         : identifier;
--btree_DB_REP_JOIN_FAILURE_t         : identifier;
--btree_DB_REP_LEASE_EXPIRED_t         : identifier;
--btree_DB_REP_LOCKOUT_t         : identifier;
--btree_DB_REP_NEWSITE_t         : identifier;
--btree_DB_REP_NOTPERM_t         : identifier;
--btree_DB_REP_UNAVAIL_t         : identifier;
--btree_DB_RUNRECOVERY_t         : identifier;
--btree_DB_SECONDARY_BAD_t         : identifier;
--btree_DB_VERIFY_BAD_t         : identifier;
--btree_DB_VERSION_MISMATCH_t         : identifier;

hash_new_file_t      : identifier;
hash_clear_t         : identifier;

-- TODO
hash_will_raise_t    : identifier;
hash_last_error_t    : identifier;
hash_raise_exceptions_t : identifier;

hash_create_t        : identifier;
hash_close_t         : identifier;
hash_open_t          : identifier;
hash_delete_t        : identifier;
hash_is_open_t       : identifier;
hash_name_t          : identifier;
hash_flush_t         : identifier;
hash_truncate_t      : identifier;

hash_set_t           : identifier;
hash_get_t           : identifier;
hash_has_element_t   : identifier;
hash_remove_t        : identifier;
hash_increment_t     : identifier;
hash_decrement_t     : identifier;
hash_add_t           : identifier;
hash_replace_t       : identifier;
hash_append_t        : identifier;
hash_prepend_t       : identifier;

hash_new_cursor_t    : identifier;
hash_open_cursor_t   : identifier;
hash_close_cursor_t  : identifier;
hash_get_first_t     : identifier;
hash_get_last_t      : identifier;
hash_get_next_t      : identifier;
hash_get_previous_t  : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupHashIO;
procedure ShutdownHashIO;

end parser_hash_io;

