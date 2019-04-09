------------------------------------------------------------------------------
-- Berkeley DB Status Codes                                                 --
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

with world;
use  world;

package parser_bdb is

------------------------------------------------------------------------------
-- BTree package identifiers
------------------------------------------------------------------------------

bdb_db_error_t            : identifier;
bdb_DB_OK_t               : identifier;
bdb_DB_BUFFER_SMALL_t     : identifier;
bdb_DB_DONOTINDEX_t       : identifier;
bdb_DB_FOREIGN_CONFLICT_t : identifier;
bdb_DB_KEYEMPTY_t         : identifier;
bdb_DB_KEYEXIST_t         : identifier;
bdb_DB_LOCK_DEADLOCK_t    : identifier;
bdb_DB_LOCK_NOTGRANTED_t  : identifier;
bdb_DB_LOG_BUFFER_FULL_t  : identifier;
bdb_DB_NOSERVER_t         : identifier;
bdb_DB_NOSERVER_HOME_t    : identifier;
bdb_DB_NOSERVER_ID_t      : identifier;
bdb_DB_NOTFOUND_t         : identifier;
bdb_DB_OLD_VERSION_t      : identifier;
bdb_DB_PAGE_NOTFOUND_t    : identifier;
bdb_DB_REP_DUPMASTER_t    : identifier;
bdb_DB_REP_HANDLE_DEAD_t  : identifier;
bdb_DB_REP_HOLDELECTION_t : identifier;
bdb_DB_REP_IGNORE_t       : identifier;
bdb_DB_REP_ISPERM_t       : identifier;
bdb_DB_REP_JOIN_FAILURE_t  : identifier;
bdb_DB_REP_LEASE_EXPIRED_t : identifier;
bdb_DB_REP_LOCKOUT_t      : identifier;
bdb_DB_REP_NEWSITE_t      : identifier;
bdb_DB_REP_NOTPERM_t      : identifier;
bdb_DB_REP_UNAVAIL_t      : identifier;
bdb_DB_RUNRECOVERY_t      : identifier;
bdb_DB_SECONDARY_BAD_t    : identifier;
bdb_DB_VERIFY_BAD_t       : identifier;
bdb_DB_VERSION_MISMATCH_t : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupBDB;
procedure ShutdownBDB;

end parser_bdb;

