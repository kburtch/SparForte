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

package parser_btree_io is

------------------------------------------------------------------------------
-- BTree package identifiers
------------------------------------------------------------------------------

btree_file_t          : identifier;
btree_cursor_t        : identifier;
btree_db_error_t      : identifier;

btree_DB_OK_t         : identifier;
btree_DB_BUFFER_SMALL_t         : identifier;
btree_DB_DONOTINDEX_t         : identifier;
btree_DB_FOREIGN_CONFLICT_t         : identifier;
btree_DB_KEYEMPTY_t         : identifier;
btree_DB_KEYEXIST_t         : identifier;
btree_DB_LOCK_DEADLOCK_t         : identifier;
btree_DB_LOCK_NOTGRANTED_t         : identifier;
btree_DB_LOG_BUFFER_FULL_t         : identifier;
btree_DB_NOSERVER_t         : identifier;
btree_DB_NOSERVER_HOME_t         : identifier;
btree_DB_NOSERVER_ID_t         : identifier;
btree_DB_NOTFOUND_t         : identifier;
btree_DB_OLD_VERSION_t         : identifier;
btree_DB_PAGE_NOTFOUND_t         : identifier;
btree_DB_REP_DUPMASTER_t         : identifier;
btree_DB_REP_HANDLE_DEAD_t         : identifier;
btree_DB_REP_HOLDELECTION_t         : identifier;
btree_DB_REP_IGNORE_t         : identifier;
btree_DB_REP_ISPERM_t         : identifier;
btree_DB_REP_JOIN_FAILURE_t         : identifier;
btree_DB_REP_LEASE_EXPIRED_t         : identifier;
btree_DB_REP_LOCKOUT_t         : identifier;
btree_DB_REP_NEWSITE_t         : identifier;
btree_DB_REP_NOTPERM_t         : identifier;
btree_DB_REP_UNAVAIL_t         : identifier;
btree_DB_RUNRECOVERY_t         : identifier;
btree_DB_SECONDARY_BAD_t         : identifier;
btree_DB_VERIFY_BAD_t         : identifier;
btree_DB_VERSION_MISMATCH_t         : identifier;

btree_new_file_t      : identifier;
btree_clear_t         : identifier;

-- TODO
btree_will_raise_t    : identifier;
btree_last_error_t    : identifier;
btree_raise_exceptions_t : identifier;

btree_create_t        : identifier;
btree_close_t         : identifier;
btree_open_t          : identifier;
btree_delete_t        : identifier;
btree_is_open_t       : identifier;
btree_name_t          : identifier;
btree_flush_t         : identifier;

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
btree_open_cursor_t   : identifier;
btree_close_cursor_t  : identifier;
btree_get_first_t     : identifier;
btree_get_last_t      : identifier;
btree_get_next_t      : identifier;
btree_get_previous_t  : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupBTree;
procedure ShutdownBTree;

end parser_btree_io;

