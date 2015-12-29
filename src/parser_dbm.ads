------------------------------------------------------------------------------
-- Database Package Parser                                                  --
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

package parser_dbm is

------------------------------------------------------------------------------
-- Database package identifiers
------------------------------------------------------------------------------

dbm_connection_t : identifier;
dbm_query_t : identifier;

dbm_column_index_type_t : identifier;
dbm_tuple_index_type_t : identifier;
dbm_tuple_count_type_t : identifier;

--db_trace_mode_type_t : identifier;
--db_trace_none_t      : identifier;
--db_trace_db_t        : identifier;
--db_trace_apq_t       : identifier;
--db_trace_full_t      : identifier;

dbm_mode_type_t       : identifier;
dbm_read_t            : identifier;
dbm_write_t           : identifier;
dbm_read_write_t      : identifier;

--db_fetch_mode_type_t  : identifier;
--db_sequential_fetch_t : identifier;
--db_random_fetch_t     : identifier;

--db_database_type_t     : identifier;
--db_engine_postgresql_t : identifier;
--db_engine_mysql_t      : identifier;
--db_engine_oracle_t     : identifier;
--db_engine_sybase_t     : identifier;
--db_engine_db2_t        : identifier;

dbm_new_connection_t : identifier;
dbm_new_query_t     : identifier;
dbm_connect_t      : identifier;
dbm_disconnect_t   : identifier;
dbm_is_connected_t : identifier;
dbm_reset_t        : identifier;
dbm_error_message_t: identifier;
dbm_notice_message_t: identifier;
dbm_in_abort_state_t: identifier;
dbm_options_t      : identifier;
dbm_will_rollback_on_finalize_t : identifier;
dbm_set_rollback_on_finalize_t : identifier;
dbm_open_db_trace_t: identifier;
dbm_close_db_trace_t: identifier;
dbm_set_trace_t    : identifier;
dbm_is_trace_t     : identifier;
dbm_clear_t        : identifier;
dbm_prepare_t      : identifier;
dbm_append_t       : identifier;
dbm_append_line_t  : identifier;
dbm_append_quoted_t: identifier;
dbm_execute_t      : identifier;
dbm_execute_checked_t : identifier;
dbm_raise_exceptions_t : identifier;
dbm_report_errors_t : identifier;
dbm_begin_work_t   : identifier;
dbm_commit_work_t  : identifier;
dbm_rollback_work_t : identifier;
dbm_rewind_t : identifier;
dbm_fetch_t : identifier;
dbm_end_of_query_t : identifier;
dbm_tuple_t        : identifier;
dbm_tuples_t       : identifier;
dbm_columns_t      : identifier;
dbm_column_name_t  : identifier;
dbm_column_index_t : identifier;
dbm_column_type_t  : identifier;
dbm_is_null_t      : identifier;
dbm_value_t        : identifier;
dbm_engine_of_t    : identifier;
dbm_show_t         : identifier;
dbm_list_t         : identifier;
dbm_schema_t       : identifier;
dbm_users_t        : identifier;
dbm_databases_t    : identifier;
dbm_fetch_values_t : identifier;
dbm_append_for_insert_t : identifier;
dbm_append_for_update_t : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupDBM;
procedure ShutdownDBM;

end parser_dbm;
