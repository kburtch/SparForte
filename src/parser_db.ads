------------------------------------------------------------------------------
-- Database Package Parser                                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2020 Free Software Foundation              --
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

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

package parser_db is

------------------------------------------------------------------------------
-- Database package identifiers
--
-- These are shared with other database packages.
------------------------------------------------------------------------------

db_column_index_type_t : identifier;
db_tuple_index_type_t : identifier;
db_tuple_count_type_t : identifier;

db_mode_type_t       : identifier;
db_read_t            : identifier;
db_write_t           : identifier;
db_read_write_t      : identifier;

db_database_type_t     : identifier;
db_engine_postgresql_t : identifier;
db_engine_mysql_t      : identifier;
db_engine_oracle_t     : identifier;
db_engine_sybase_t     : identifier;
db_engine_db2_t        : identifier;

db_trace_mode_type_t : identifier;
db_trace_none_t      : identifier;
db_trace_db_t        : identifier;
db_trace_apq_t       : identifier;
db_trace_full_t      : identifier;

db_fetch_mode_type_t  : identifier;
db_sequential_fetch_t : identifier;
db_random_fetch_t     : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupDB;
procedure ShutdownDB;

------------------------------------------------------------------------------
-- Command Line SQL
------------------------------------------------------------------------------

procedure DoSQLSelect( sqlcmd : unbounded_string );
procedure DoSQLStatement( sqlcmd : unbounded_string );

end parser_db;
