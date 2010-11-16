------------------------------------------------------------------------------
-- BUSH Database Package Parser                                             --
--                                                                          --
-- Part of BUSH                                                             --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2005 Ken O. Burtch & FSF                 --
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
-- CVS: $Id: parser_db.ads,v 1.2 2005/02/11 02:59:26 ken Exp $

with ada.strings.unbounded, world;
use  ada.strings.unbounded, world;

package parser_db is

------------------------------------------------------------------------------
-- Database package identifiers
--
-- These will eventually be moved to the Database parser
------------------------------------------------------------------------------

db_column_index_type_t : identifier;
db_tuple_index_type_t : identifier;
db_tuple_count_type_t : identifier;

db_trace_mode_type_t : identifier;
db_trace_none_t      : identifier;
db_trace_db_t        : identifier;
db_trace_apq_t       : identifier;
db_trace_full_t      : identifier;

db_mode_type_t       : identifier;
db_read_t            : identifier;
db_write_t           : identifier;
db_read_write_t      : identifier;

db_fetch_mode_type_t  : identifier;
db_sequential_fetch_t : identifier;
db_random_fetch_t     : identifier;

db_database_type_t     : identifier;
db_engine_postgresql_t : identifier;
db_engine_mysql_t      : identifier;
db_engine_oracle_t     : identifier;
db_engine_sybase_t     : identifier;
db_engine_db2_t        : identifier;

db_connect_t      : identifier;
db_disconnect_t   : identifier;
db_is_connected_t : identifier;
db_reset_t        : identifier;
db_error_message_t: identifier;
db_notice_message_t: identifier;
db_in_abort_state_t: identifier;
db_options_t      : identifier;
db_will_rollback_on_finalize_t : identifier;
db_set_rollback_on_finalize_t : identifier;
db_open_db_trace_t: identifier;
db_close_db_trace_t: identifier;
db_set_trace_t    : identifier;
db_is_trace_t     : identifier;
db_clear_t        : identifier;
db_prepare_t      : identifier;
db_append_t       : identifier;
db_append_line_t  : identifier;
db_append_quoted_t: identifier;
db_execute_t      : identifier;
db_execute_checked_t : identifier;
db_raise_exceptions_t : identifier;
db_report_errors_t : identifier;
db_begin_work_t   : identifier;
db_commit_work_t  : identifier;
db_rollback_work_t : identifier;
db_rewind_t : identifier;
db_fetch_t : identifier;
db_end_of_query_t : identifier;
db_tuple_t        : identifier;
db_tuples_t       : identifier;
db_columns_t      : identifier;
db_column_name_t  : identifier;
db_column_index_t : identifier;
db_column_type_t  : identifier;
db_is_null_t      : identifier;
db_value_t        : identifier;
db_engine_of_t    : identifier;
db_show_t         : identifier;
db_list_t         : identifier;
db_schema_t       : identifier;
db_users_t        : identifier;
db_databases_t    : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupDB;
procedure ShutdownDB;

------------------------------------------------------------------------------
-- PARSE THE DATABASE PACKAGE
------------------------------------------------------------------------------

procedure ParseDBConnect;
procedure ParseDBPrepare;
procedure ParseDBAppend;
procedure ParseDBAppendLine;
procedure ParseDBAppendQuoted;
procedure ParseDBExecute;
procedure ParseDBExecuteChecked;
procedure ParseDBDisconnect;
procedure ParseDBIsConnected( result : out unbounded_string );
procedure ParseDBReset;
procedure ParseDBErrorMessage( result : out unbounded_string );
procedure ParseDBNoticeMessage( result : out unbounded_string );
procedure ParseDBInAbortState( result : out unbounded_string );
procedure ParseDBOptions( result : out unbounded_string );
procedure ParseDBSetRollbackOnFinalize;
procedure ParseDBWillRollbackOnFinalize( result : out unbounded_string );
procedure ParseDBOpenDBTrace;
procedure ParseDBCloseDBTrace;
procedure ParseDBSetTrace;
procedure ParseDBIsTrace( result : out unbounded_string );
procedure ParseDBClear;
procedure ParseDBRaiseExceptions;
procedure ParseDBReportErrors;
procedure ParseDBBeginWork;
procedure ParseDBRollbackWork;
procedure ParseDBCommitWork;
procedure ParseDBRewind;
procedure ParseDBFetch;
procedure ParseDBEndOfQuery( result : out unbounded_string );
procedure ParseDBTuple( result : out unbounded_string );
procedure ParseDBTuples( result : out unbounded_string );
procedure ParseDBColumns( result : out unbounded_string );
procedure ParseDBColumnName( result : out unbounded_string );
procedure ParseDBColumnIndex( result : out unbounded_string );
--procedure ParseDBColumnType( result : out unbounded_string );
procedure ParseDBIsNull( result : out unbounded_string );
procedure ParseDBValue( result : out unbounded_string );
procedure ParseDBEngineOf( result : out unbounded_string );
procedure ParseDBShow;
procedure ParseDBList;
procedure ParseDBSchema;
procedure ParseDBUsers;
procedure ParseDBDatabases;

-- for embedded SQL

procedure DoSQLSelect( sqlcmd : unbounded_string );
procedure DoSQLStatement( sqlcmd : unbounded_string );

--procedure ParseDBIPrepare( result : out unbounded_string );
--procedure ParseDBDo( result : out unbounded_string );
--procedure ParseDBFetchrow( result : out unbounded_string );

end parser_db;
