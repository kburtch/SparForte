------------------------------------------------------------------------------
-- Database Package Parser (MySQL support)                                  --
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

with text_io;use text_io;
with ada.io_exceptions,
     ada.strings.unbounded,
#if MYSQL
     APQ.MySQL.Client,
#end if;
     bush_os.tty,
     world,
     signal_flags,
     string_util,
     user_io,
     scanner,
     parser,
     parser_aux,
     parser_db;
use  ada.io_exceptions,
     ada.strings.unbounded,
#if MYSQL
     APQ,
     APQ.MySQL,
     APQ.MySQL.Client,
#end if;
     bush_os.tty,
     world,
     signal_flags,
     string_util,
     user_io,
     scanner,
     parser,
     parser_aux,
     parser_db;

package body parser_mysql is

#if MYSQL

Q : Query_Type;
C : Connection_Type;
-- for the time being, a single query

procedure ParseMySQLConnect is
  -- Syntax: mysql.connect( dbname [,user ,passwd [,host [,port ] ] ] );
  dbnameExpr : unbounded_string;
  dbnameType : identifier;
  userExpr : unbounded_string;
  userType : identifier;
  hasUser  : boolean := false;
  pswdExpr : unbounded_string;
  pswdType : identifier;
  hostExpr : unbounded_string;
  hostType : identifier;
  hasHost  : boolean := false;
  portExpr : unbounded_string;
  portType : identifier;
  hasPort  : boolean := false;
begin
  -- temporary limitation: only one database open at a time...
  expect( mysql_connect_t );
  if engineOpen then
     err( "only one database connection may be open (a limitation of this verison of bush)" );
     return;
  end if;
  expect( symbol_t, "(" );
  ParseExpression( dbnameExpr, dbnameType );
  if baseTypesOK( string_t, dbnameType ) then
     if token = symbol_t and identifiers( token ).value = "," then
        expect( symbol_t, "," );
        ParseExpression( userExpr, userType );
        if baseTypesOK( string_t, userType ) then
           hasUser := true;
           expect( symbol_t, "," );
           ParseExpression( pswdExpr, pswdType );
           if baseTypesOK( string_t, pswdType ) then
              if token = symbol_t and identifiers( token ).value = "," then
                 expect( symbol_t, "," );
                 ParseExpression( hostExpr, hostType );
                 if baseTypesOK( string_t, hostType ) then
                    hasHost := true;
                    if token = symbol_t and identifiers( token ).value = "," then
                       expect( symbol_t, "," );
                       ParseExpression( portExpr, portType );
                       if baseTypesOK( integer_t, portType ) then
                          hasPort := true;
                       end if;
                    end if;
                 end if;
              end if;
           end if;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     begin
       begin
         Set_DB_Name( C, to_string( dbnameExpr ) );
       exception when others =>
         err( to_string( "Internal error: set_db_name " & dbnameExpr &
              " failed" ) );
       end;
       if hasUser then
          begin
            Set_User_Password( C, to_string( userExpr ), to_string( pswdExpr ) );
          exception when others =>
             err( to_string( "Internal error: set_user_password " & userExpr &
                  "/" & pswdExpr & " failed" ) );
          end;
       end if;
       if hasHost then
          begin
             Set_Host_Name( C, to_string( hostExpr ) );
          exception when others =>
             err( to_string( "Internal error: set_host_name " & hostExpr &
                  " failed" ) );
          end;
       end if;
       if hasPort then
          begin
            Set_Port( C, integer( to_numeric( portExpr ) ) );
          exception when others =>
             err( to_string( "Internal error: set_port " & portExpr &
                  " failed" ) );
          end;
       end if;
       Connect( C );
       engineOpen := true;
       currentEngine := Engine_MySQL;
     exception when not_connected =>
        if hasHost and hasPort then
           err( "database connection failed - User " & User( C ) & ", Password " & Password( C )
           & ", Host " & Host_Name( C ) & "and Port " & integer'image( Port( C ) ) & " " & DB_Name( C ) );
        else
          err( "database connection failed" );
        end if;
     when already_connected =>
        err( "already connected to database" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMySQLConnect;

procedure ParseMySQLEngineOf( result : out unbounded_string ) is
  -- Syntax: b := mysql.engine_of;
  -- Source: APQ.Engine_Of
begin
  expect( mysql_engine_of_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( integer'image( Database_Type'pos( Engine_Of( C ) ) ) );
       if length( result ) > 0 then
          if element( result, 1 ) = ' ' then
             delete( result, 1, 1 );
          end if;
       end if;
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLEngineOf;

procedure ParseMySQLPrepare( result : out unbounded_string ) is
  -- Syntax: mysql.prepare( sqlstmt [,after] );
  sqlExpr   : unbounded_string;
  sqlType   : identifier;
  afterExpr : unbounded_string;
  afterType : identifier;
  hasAfter  : boolean := false;
begin
  expect( mysql_prepare_t );
  expect( symbol_t, "(" );
  ParseExpression( sqlExpr, sqlType );
  if baseTypesOK( string_t, sqlType ) then
     if token = symbol_t and identifiers( token ).value = "," then
        expect( symbol_t, "," );
        ParseExpression( afterExpr, afterType );
        if baseTypesOK( string_t, sqlType ) then
           hasAfter := true;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then  
     result := to_bush_boolean( true );
     begin
       Clear( Q );
       if hasAfter then
          Prepare( Q, to_string( sqlExpr ), to_string( afterExpr ) );
       else
          Prepare( Q, to_string( sqlExpr ) );
       end if;
     exception when others =>
       result := to_bush_boolean( false );
     end;
  end if;
end ParseMySQLPrepare;

procedure ParseMySQLPrepare is
  -- Syntax: mysql.prepare( sqlstmt [,after] );
  sqlExpr   : unbounded_string;
  sqlType   : identifier;
  afterExpr : unbounded_string;
  afterType : identifier;
  hasAfter  : boolean := false;
begin
  expect( mysql_prepare_t );
  expect( symbol_t, "(" );
  ParseExpression( sqlExpr, sqlType );
  if baseTypesOK( string_t, sqlType ) then
     if token = symbol_t and identifiers( token ).value = "," then
        expect( symbol_t, "," );
        ParseExpression( afterExpr, afterType );
        if baseTypesOK( string_t, afterType ) then
           hasAfter := true;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then  
     begin
       if hasAfter then
          Prepare( Q, to_string( sqlExpr ), to_string( afterExpr ) );
       else
          Prepare( Q, to_string( sqlExpr ) );
       end if;
     exception when others =>
       err( "exception raised" );
     end;
  end if;
end ParseMySQLPrepare;

procedure ParseMySQLAppend is
  -- Syntax: mysql.append( sqlstmt [,after] );
  sqlExpr   : unbounded_string;
  sqlType   : identifier;
  afterExpr : unbounded_string;
  afterType : identifier;
  hasAfter  : boolean := false;
begin
  expect( mysql_append_t );
  expect( symbol_t, "(" );
  ParseExpression( sqlExpr, sqlType );
  if baseTypesOK( string_t, sqlType ) then
     if token = symbol_t and identifiers( token ).value = "," then
        expect( symbol_t, "," );
        ParseExpression( afterExpr, afterType );
        if baseTypesOK( string_t, afterType ) then
           hasAfter := true;
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then  
     begin
       if hasAfter then
          Append( Q, to_string( sqlExpr ), to_string( afterExpr ) );
       else
          Append( Q, to_string( sqlExpr ) );
       end if;
     exception when others =>
       err( "exception raised" );
     end;
  end if;
end ParseMySQLAppend;

procedure ParseMySQLAppendLine is
  -- Syntax: mysql.append_line( sqlstmt );
  sqlExpr   : unbounded_string;
  sqlType   : identifier;
begin
  expect( mysql_append_line_t );
  expect( symbol_t, "(" );
  ParseExpression( sqlExpr, sqlType );
  if baseTypesOK( string_t, sqlType ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then  
     begin
       Append_Line( Q, to_string( sqlExpr ) );
     exception when others =>
       err( "exception raised" );
     end;
  end if;
end ParseMySQLAppendLine;

procedure ParseMySQLAppendQuoted is
  -- Syntax: mysql.append_quoted( sqlstmt [,after] );
  sqlExpr   : unbounded_string;
  sqlType   : identifier;
  afterExpr : unbounded_string;
  afterType : identifier;
  hasAfter  : boolean := false;
begin
  expect( mysql_append_quoted_t );
  expect( symbol_t, "(" );
  ParseExpression( sqlExpr, sqlType );
  if baseTypesOK( sqlType, string_t ) then
     if token = symbol_t and identifiers( token ).value = "," then
        expect( symbol_t, "," );
        hasAfter := true;
        ParseExpression( afterExpr, afterType );
        if baseTypesOK( afterType, string_t ) then
           null;
        end if;
     end if;
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then  
     begin
       if hasAfter then
          Append_Quoted( Q, C, to_string( sqlExpr ), to_string( afterExpr ) );
       else
          Append_Quoted( Q, C, to_string( sqlExpr ) );
       end if;
     exception when others =>
       err( "exception raised" );
     end;
  end if;
end ParseMySQLAppendQuoted;

procedure ParseMySQLExecute is
begin
  expect( mysql_execute_t );
  if isExecutingCommand then
     begin
       Execute( Q, C );
     exception when not_connected =>
       err( "not connected" );
     when abort_state =>
       err( "in abort state" );
     when sql_error =>
       err( Error_Message( Q ) );
     when others =>
       err( "exception raised" );
     end;
  end if;
end ParseMySQLExecute;

procedure ParseMySQLExecuteChecked is
  -- Syntax: mysql.execute_checked( [ msg ] );
  msgExpr   : unbounded_string;
  msgType   : identifier;
  hasMsg    : boolean := false;
begin
  expect( mysql_execute_checked_t );
  if token = symbol_t and identifiers( token ).value = "(" then
     expect( symbol_t, "(" );
     ParseExpression( msgExpr, msgType );
     if baseTypesOK( string_t, msgType ) then
        expect( symbol_t, ")" );
     end if;
     hasMsg := true;
  end if;
  if isExecutingCommand then
     begin
       if hasMsg then
          Execute_Checked( Q, C, to_string( msgExpr ) );
       else
          Execute_Checked( Q, C );
       end if;
     exception when not_connected =>
       err( "not connected" );
     when abort_state =>
       err( "in abort state" );
     when sql_error =>
       err( Error_Message( Q ) );
     when others =>
       err( "exception raised" );
     end;
  end if;
end ParseMySQLExecuteChecked;

--procedure ParseDBDo( result : out unbounded_string ) is
--begin
--  expect( db_do_t );
--  result := null_unbounded_string;
--end ParseDBDo;
--
--procedure ParseDBFetchrow( result : out unbounded_string ) is
--begin
--  expect( db_fetchrow_t );
--  result := null_unbounded_string;
--end ParseDBFetchrow;

procedure ParseMySQLDisconnect is
  -- Syntax: mysql.disconnect;
begin
  expect( mysql_disconnect_t );
  if isExecutingCommand then
     begin
        Disconnect( C );
     exception when not_connected =>
        err( "no database connection" );
     when already_connected =>
        err( "already connected to database" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMySQLDisconnect;

procedure ParseMySQLIsConnected( result : out unbounded_string ) is
  -- Syntax: mysql.is_connected
begin
  expect( mysql_is_connected_t );
  if isExecutingCommand then
     begin
       result := to_bush_boolean( is_connected( C ) ); 
     exception when others =>
       result := to_bush_boolean( false );
     end;
  end if;
end ParseMySQLIsConnected;

procedure ParseMySQLReset is
  -- Syntax: mysql.reset
  -- Source: APQ.Reset
begin
  expect( mysql_reset_t );
  if isExecutingCommand then
     begin
       Reset( C );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLReset;

procedure ParseMySQLErrorMessage( result : out unbounded_string ) is
  -- Syntax: mysql.error_message
  -- Source: APQ.Error_Message
begin
  expect( mysql_error_message_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Error_Message( C ) );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLErrorMessage;

-- procedure ParseMySQLNoticeMessage( result : out unbounded_string ) is
--   -- Syntax: mysql.notice_message
--   -- Source: APQ.Notice_Message
-- begin
--   expect( mysql_notice_message_t );
--   if isExecutingCommand then
--      begin
--        result := to_unbounded_string( Notice_Message( C ) );
--      exception when others =>
--        err( "exception was raised" );
--      end;
--   end if;
-- end ParseMySQLNoticeMessage;

procedure ParseMySQLInAbortState( result : out unbounded_string ) is
  -- Syntax: mysql.in_abort_state
  -- Source: APQ.In_Abort_State
begin
  expect( mysql_in_abort_state_t );
  if isExecutingCommand then
     begin
       result := to_bush_boolean( In_Abort_State( C ) );
     exception when not_connected =>
       err( "not connected" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLInAbortState;

procedure ParseMySQLOptions( result : out unbounded_string ) is
  -- Syntax: mysql.options
  -- Source: APQ.Options
begin
  expect( mysql_options_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Options( C ) );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLOptions;

procedure ParseMySQLSetRollbackOnFinalize is
  -- Syntax: mysql.set_rollback_on_finalize( b );
  -- Source: APQ.Set_Rollback_On_Finalize
  rollExpr : unbounded_string;
  rollType : identifier;
begin
  expect( mysql_set_rollback_on_finalize_t );
  expect( symbol_t, "(" );
  ParseExpression( rollExpr, rollType );
  if baseTypesOK( rollType, boolean_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       rollback : boolean := rollExpr = to_unbounded_string( "1" );
     begin
       Set_Rollback_On_Finalize( C, rollback );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLSetRollbackOnFinalize;

procedure ParseMySQLWillRollbackOnFinalize( result : out unbounded_string ) is
  -- Syntax: mysql.will_rollback_on_finalize( b );
  -- Source: APQ.Will_Rollback_On_Finalize
begin
  expect( mysql_will_rollback_on_finalize_t );
  begin
    result := to_bush_boolean( Will_Rollback_On_Finalize( C ) );
  exception when others =>
    err( "exception was raised" );
  end;
end ParseMySQLWillRollbackOnFinalize;

procedure ParseMySQLOpenDBTrace is
  -- Syntax: mysql.open_db_trace( f [,m] );
  -- Source: APQ.Open_DB_Trace
  fnameExpr : unbounded_string;
  fnameType : identifier;
  modeExpr  : unbounded_string;
  modeType  : identifier;
  traceMode : trace_mode_type;
  hasMode  : boolean := false;
begin
  expect( mysql_open_db_trace_t );
  expect( symbol_t, "(" );
  ParseExpression( fnameExpr, fnameType );
  if baseTypesOK( fnameType, string_t ) then
     if token = symbol_t and identifiers( token ).value = "," then
        expect( symbol_t, "," );
        ParseExpression( modeExpr, modeType );
        if baseTypesOK( modeType, db_trace_mode_type_t ) then
           traceMode := Trace_Mode_Type'val( integer'value( ' ' & to_string( modeExpr ) ) );
           hasMode := true;
        end if;
     end if;
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       if hasMode then
          Open_DB_Trace( C, to_string( fnameExpr ), traceMode );
       else
          Open_DB_Trace( C, to_string( fnameExpr ) );
       end if;
     exception when not_connected =>
       err( "not connected" );
     when tracing_state =>
       err( "file already open" );
     when Ada.IO_Exceptions.Name_Error =>
       err( "file not found" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLOpenDBTrace;

procedure ParseMySQLCloseDBTrace is
  -- Syntax: mysql.close_db_trace( f );
  -- Source: APQ.Close_DB_Trace
begin
  expect( mysql_close_db_trace_t );
  if isExecutingCommand then
     begin
       Close_DB_Trace( C );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLCloseDBTrace;

procedure ParseMySQLSetTrace is
  -- Syntax: mysql.set_trace( b );
  -- Source: APQ.Set_Trace
  traceExpr : unbounded_string;
  traceType : identifier;
begin
  expect( mysql_set_trace_t );
  expect( symbol_t, "(" );
  ParseExpression( traceExpr, traceType );
  if baseTypesOK( traceType, boolean_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       traceback : boolean := traceExpr = to_unbounded_string( "1" );
     begin
       Set_Trace( C, traceback );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLSetTrace;

procedure ParseMySQLIsTrace( result : out unbounded_string ) is
  -- Syntax: mysql.is_trace( b );
  -- Source: APQ.Is_Trace
begin
  expect( mysql_is_trace_t );
  begin
    result := to_bush_boolean( Is_Trace( C ) );
  exception when others =>
    err( "exception was raised" );
  end;
end ParseMySQLIsTrace;

procedure ParseMySQLClear is
  -- Syntax: mysql.clear;
  -- Source: APQ.Clear
begin
  expect( mysql_clear_t );
  begin
    Clear( Q );
  exception when others =>
    err( "exception was raised" );
  end;
end ParseMySQLClear;

procedure ParseMySQLRaiseExceptions is
  -- Syntax: mysql.raise_exceptions( [ b ] );
  -- Source: APQ.Raise_Exceptions
  raiseExpr : unbounded_string;
  raiseType : identifier;
begin
  expect( mysql_raise_exceptions_t );
  if token = symbol_t and identifiers( token ).value = "(" then
     expect( symbol_t, "(" );
     ParseExpression( raiseExpr, raiseType );
     if baseTypesOK( raiseType, boolean_t ) then
        expect( symbol_t, ")" );
     end if;
  else
     raiseExpr := to_unbounded_string( "1" );
  end if;
  if isExecutingCommand then
     declare
       raise_them : boolean := raiseExpr = to_unbounded_string( "1" );
     begin
       Raise_Exceptions( Q, raise_them );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLRaiseExceptions;

procedure ParseMySQLReportErrors is
  -- Syntax: mysql.report_errors( [ b ] );
  -- Source: APQ.Report_Errors
  reportExpr : unbounded_string;
  reportType : identifier;
begin
  expect( mysql_report_errors_t );
  if token = symbol_t and identifiers( token ).value = "(" then
     expect( symbol_t, "(" );
     ParseExpression( reportExpr, reportType );
     if baseTypesOK( reportType, boolean_t ) then
        expect( symbol_t, ")" );
     end if;
  else
     reportExpr := to_unbounded_string( "1" );
  end if;
  if isExecutingCommand then
     declare
       report_them : boolean := reportExpr = to_unbounded_string( "1" );
     begin
       Report_Errors( Q, report_them );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLReportErrors;

procedure ParseMySQLBeginWork is
  -- Syntax: mysql.begin_work;
  -- Source: APQ.Begin_Work
begin
  expect( mysql_begin_work_t );
  if isExecutingCommand then
     begin
       Begin_Work( Q, C );
     exception when abort_state =>
       err( "in abort state" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLBeginWork;

procedure ParseMySQLRollbackWork is
  -- Syntax: mysql.rollback_work;
  -- Source: APQ.Rollback_Work
begin
  expect( mysql_rollback_work_t );
  if isExecutingCommand then
     begin
       Rollback_Work( Q, C );
     exception when abort_state =>
       err( "in abort state" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLRollbackWork;

procedure ParseMySQLCommitWork is
  -- Syntax: mysql.commit_work;
  -- Source: APQ.Commit_Work
begin
  expect( mysql_commit_work_t );
  if isExecutingCommand then
     begin
       Commit_Work( Q, C );
     exception when abort_state =>
       err( "in abort state" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLCommitWork;

procedure ParseMySQLRewind is
  -- Syntax: mysql.rewind;
  -- Source: APQ.Rewind
begin
  expect( mysql_rewind_t );
  if isExecutingCommand then
     begin
       Rewind( Q );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLRewind;

procedure ParseMySQLFetch is
  -- Syntax: mysql.fetch;
  -- Source: APQ.Fetch
  expr_val : unbounded_string;
  expr_type : identifier;
  haveIndex : boolean := false;
begin
  expect( mysql_fetch_t );
  if token = symbol_t and identifiers( token ).value = "(" then
     expect( symbol_t, "(" );
     parseExpression( expr_val, expr_type );
     if baseTypesOK( expr_type, mysql_tuple_index_type_t ) then
        expect( symbol_t, ")" );
        haveIndex := true;
     end if;
  end if;
  if isExecutingCommand then
     begin
       if haveIndex then
          Fetch( Q, Tuple_Index_Type( to_numeric( expr_val ) ) );
       else
          Fetch( Q );
       end if;
     exception when no_tuple =>
       err( "no tuple" );
     when no_result =>
       err( "no result" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLFetch;

procedure ParseMySQLEndOfQuery( result : out unbounded_string ) is
  -- Syntax: b := mysql.end_of_query;
  -- Source: APQ.End_Of_Query
begin
  expect( mysql_end_of_query_t );
  if isExecutingCommand then
     begin
       result := to_bush_boolean( End_Of_Query( Q ) );
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLEndOfQuery;

procedure ParseMySQLTuple( result : out unbounded_string ) is
  -- Syntax: t := mysql.tuple;
  -- Source: APQ.Tuple
begin
  expect( mysql_tuple_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Tuple_Index_Type'image( Tuple( Q ) ) );
     exception when no_tuple =>
       err( "no tuple" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLTuple;

procedure ParseMySQLTuples( result : out unbounded_string ) is
  -- Syntax: n := mysql.tuples;
  -- Source: APQ.Tuples
begin
  expect( mysql_tuples_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Tuple_Count_Type'image( Tuples( Q ) ) );
     exception when no_result =>
       err( "no result" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLTuples;

procedure ParseMySQLColumns( result : out unbounded_string ) is
  -- Syntax: n := mysql.columns;
  -- Source: APQ.Columns
begin
  expect( mysql_columns_t );
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Natural'image( Columns( Q ) ) );
     exception when no_result =>
       err( "no result" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLColumns;

procedure ParseMySQLColumnName( result : out unbounded_string ) is
  -- Syntax: n := mysql.column_Name;
  -- Source: APQ.Column_Name;
  exprVal : unbounded_string;
  exprType : identifier;
begin
  expect( mysql_column_name_t );
  expect( symbol_t, "(" );
  ParseExpression( exprVal, exprType );
  if baseTypesOK( exprType, mysql_column_index_type_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := to_unbounded_string(
          Column_Name( Q, Column_Index_Type( to_numeric( exprVal ) ) )
       );
     exception when no_column =>
       err( "no column" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLColumnName;

procedure ParseMySQLColumnIndex( result : out unbounded_string ) is
  -- Syntax: n := mysql.column_index;
  -- Source: APQ.Column_Index
  exprVal : unbounded_string;
  exprType : identifier;
begin
  expect( mysql_column_index_t );
  expect( symbol_t, "(" );
  ParseExpression( exprVal, exprType );
  if baseTypesOK( exprType, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Column_Index_Type'image(
           Column_Index( Q, to_string( exprVal ) )
       ) );
     exception when no_column =>
       err( "no column" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLColumnIndex;

--procedure ParseDBColumnType( result : out unbounded_string ) is
  -- Syntax: n := db.column_type;
  -- Source: APQ.Column_Type;
  --exprVal : unbounded_string;
  --exprType : identifier;
--begin
  --expect( db_column_type_t );
  --expect( symbol_t, "(" );
  --ParseExpression( exprVal, exprType );
  --if baseTypesOK( exprType, db_column_index_type_t ) then
     --expect( symbol_t, ")" );
  --end if;
  --if isExecutingCommand then
     --begin
       --result := to_unbounded_string(
          --Column_Type( Q,
          --Column_Index_Type( to_numeric( exprVal ) ) )
       --);
     --exception when no_column =>
       --err( "no column" );
     --when no_result =>
       --err( "no result" );
     --when others =>
       --err( "exception was raised" );
     --end;
  --end if;
--end ParseDBColumnType;

procedure ParseMySQLIsNull( result : out unbounded_string ) is
  -- Syntax: n := mysql.is_null;
  -- Source: APQ.Is_Null;
  exprVal : unbounded_string;
  exprType : identifier;
begin
  expect( mysql_is_null_t );
  expect( symbol_t, "(" );
  ParseExpression( exprVal, exprType );
  if baseTypesOK( exprType, mysql_column_index_type_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := to_bush_boolean(
          Is_Null( Q,
          Column_Index_Type( to_numeric( exprVal ) ) )
       );
     exception when no_column =>
       err( "no column" );
     when no_result =>
       err( "no result" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLIsNull;

procedure ParseMySQLValue( result : out unbounded_string ) is
  -- Syntax: n := mysql.value;
  -- Source: APQ.Value;
  exprVal  : unbounded_string;
  exprType : identifier;
begin
  expect( mysql_value_t );
  expect( symbol_t, "(" );
  ParseExpression( exprVal, exprType );
  if baseTypesOK( exprType, mysql_column_index_type_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := Value( Q, Column_Index_Type( to_numeric( exprVal ) ) );
     exception when no_tuple =>
       err( "no tuple" );
     when null_value =>
       err( "null value" );
     when no_column =>
       err( "no column" );
     when no_result =>
       err( "no result" );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLValue;

procedure MySQLShowIt is
-- run SQL command and display results in tabular format
  tabularDisplay : boolean := false;
  headingWidth   : integer := 0;
  wasNull        : boolean := false;
  columnWidths   : array( 1..32 ) of integer;
  totalWidth     : integer;
  width          : integer;
begin
  if isExecutingCommand then
     begin
     if is_connected( C ) then
        execute( Q, C );

        -- Initially, the columns widths are the widths of the headings

        for i in 1..columns( Q ) loop
            columnWidths( i ) := column_name( Q, Column_Index_Type( i ) )'length;
            if columnWidths( i ) < 4 then -- room for NULL on tabular display
               columnWidths( i ) := 4;
            end if;
            if headingWidth < columnWidths( i ) then
               headingWidth := columnWidths( i );
            end if;
        end loop;

        -- Check query results and adjust the columns widths for the longest
        -- results.

        while not end_of_query( Q ) loop
              fetch( Q );
              for i in 1..columns( Q ) loop
                 if not is_null( Q, Column_Index_Type( i ) ) then
                    width := length( to_unbounded_string( Value( Q, Column_Index_Type( i ) ) ) );
                    if width > 256 then
                       width := 256;
                    end if;
                    if width > columnWidths( i ) then
                       columnWidths( i ) := width;
                    end if;
                 end if;
              end loop;
        end loop;

        -- Add up all columns for the total width for a tabular display

        totalWidth := 2;                                        -- left/right marg
        for i in 1..columns( Q ) loop
            totalWidth := totalWidth + columnWidths( i );       -- width of column
            if i /= columns(Q) then                             -- not last col?
               totalWidth := totalWidth + 3;                    -- 3 char sep
            end if;
        end loop;

        -- Rewind the clear and prepare to show the results

        Rewind( Q );
        new_line;

        -- Use a tabular display only if will fit in the current display

        tabularDisplay := totalWidth <= integer( displayInfo.col );

        -- Draw the columns

        if tabularDisplay then
           put( " " );
           for i in 1..columns( Q ) loop
               put(
                  to_string(
                      Head(
                          to_unbounded_string(
                              column_name( Q, Column_Index_Type( i ) ) )
                      , columnWidths( i ) )
                  )
               );
               if i /= columns( Q ) then
                  put( " | " );
               end if;
           end loop;
           new_line;
           put( "-" );
           for i in 1..columns( Q ) loop
               put( to_string( columnWidths( i ) * "-" ) );
               if i /= columns( Q ) then
                  put( "-+-" );
               else
                  put( "-" );
               end if;
           end loop;
           new_line;
        end if;

        -- Draw the query results

        while not end_of_query( Q ) loop
            fetch( Q );
            if tabularDisplay then
               put( " " );
            end if;
            for i in 1..columns( Q ) loop
                if tabularDisplay then
                   if is_null( Q, Column_Index_Type( i ) ) then
                      put( inverse( to_string( Head( to_unbounded_string( "NULL" ), columnWidths( i ) ) ) ) );
                      wasNull := true;
                   else
                      put( to_string( Head( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ), columnWidths( i ) ) ) );
                   end if;
                   if i /= columns( Q ) then
                      put( " | " );
                   end if;
                else
                   put( to_string( head( to_unbounded_string( column_name( Q, Column_Index_Type( i ) ) ), headingWidth ) ) );
                   put( ": " );
                   if is_null( Q, Column_Index_Type( i ) ) then
                      put( inverse( "NULL" ) );
                      wasNull := true;
                   else
                      put( bold( to_string( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ) ) ) );
                   end if;
                   new_line;
                end if;
                exit when wasSIGINT;
            end loop;
            new_line;
        end loop;
     end if;

     -- Draw the summary line

     if tuples( Q ) > 1 and columns ( Q ) > 1 then
        if tuples( Q ) = 1 then
           put( " 1 Row" );
        else
           put( Tuple_Index_Type'image( tuples( Q ) ) );
           put( " Rows" );
        end if;
        if wasNull then
           put( " with nulls" );
        end if;
        if columns( Q ) = 1 then
           put( " and 1 Column" );
        else
           put( " and" );
           put( integer'image( columns( Q ) ) );
           put( " Columns" );
        end if;
        new_line;
     end if;
     exception when no_tuple =>
       err( "no tuple" );
     when null_value =>
       err( "null value" );
     when no_column =>
       err( "no column" );
     when no_result =>
       err( "no result" );
     when sql_error =>
       err( Error_Message( Q ) );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end MySQLShowIt;

procedure ParseMySQLShow is
  -- Syntax: mysql.show;
  -- Source: N/A
begin
  expect( mysql_show_t );
  MySQLShowIt;
end ParseMySQLShow;

function pg_kind_to_string( kind : string ) return string is
-- convert the pg_class table's pg_relkind code to a readable string
begin
  if kind = "r" then
     return "table";
  elsif kind = "i" then
     return "index";
  elsif kind = "S" then
     return "sequence";
  elsif kind = "v" then
     return "view";
  elsif kind = "c" then
     return "composite type";
  elsif kind = "s" then
     return "special";
  elsif kind = "t" then
     return "TOAST table";
  end if;
  return "kind code " & kind;
end pg_kind_to_string;

function pg_column_type_to_string( kind, len : string ) return string is
-- convert the pg_class table's pg_relkind code to a readable string
begin
  -- if kind = "bpchar" then              -- blank-padded character array
  --    return "character(" & len & ")";  -- is char(n)
  -- elsif kind = "int4" then             -- 4-byte integer
  --    return "integer";                 -- is integer
  -- elsif kind = "varchar" then          -- varchar has a
  --    return "character varying(" & len & ")";   -- length
  -- elsif kind = "interval" then
  --    return kind;
  -- elsif kind = "timestamp" then
  --    return "timestamp without time zone";
  -- elsif kind = "int8" then
  --    return "bigint";
  -- elsif kind = "serial8" then
  --    return "bigserial";
  -- elsif kind = "bit" then
  --    return kind;
  -- elsif kind = "varbit" then
  --    return "bit varying(" & len & ")";   -- length
  -- elsif kind = "bool" then
  --    return "boolean";
  -- elsif kind = "box" then
  --    return kind;
  -- elsif kind = "bytea" then
  --    return kind;
  -- elsif kind = "cidr" then
  --    return kind;
  -- elsif kind = "circle" then
  --    return kind;
  -- elsif kind = "date" then
  --    return kind;
  -- elsif kind = "float8" then
  --    return "double precision";
  -- elsif kind = "inet" then
  --    return kind;
  -- elsif kind = "line" then
  --    return kind;
  -- elsif kind = "lseg" then
  --    return kind;
  -- elsif kind = "macaddr" then
  --    return kind;
  -- elsif kind = "money" then
  --    return kind;
  -- elsif kind = "decimal" then
  --    return kind;
  -- elsif kind = "path" then
  --    return kind;
  -- elsif kind = "point" then
  --    return kind;
  -- elsif kind = "polygon" then
  --    return kind;
  -- elsif kind = "float4" then
  --    return "real";
  -- elsif kind = "int2" then
  --    return "smallint";
  -- elsif kind = "serial4" then
  --    return "serial";
  -- elsif kind = "text" then
  --    return kind;
  -- elsif kind = "timetz" then
  --    return "time with time zone";
  -- elsif kind = "timestamptz" then
  --    return "timestamp with time zone";
  -- end if;
  return kind;
end pg_column_type_to_string;

function pg_not_null_to_string( val : string ) return string is
-- convert a t/f value to "not null" like psql client
begin
  if val = "t" then
     return "not null";
  end if;
  return "";
end pg_not_null_to_string;

function pg_default_to_string( val : string ) return string is
-- convert a t/f value to "not null" like psql client
begin
  if val = "t" then
     return "default";
  end if;
  return "";
end pg_default_to_string;

function pg_userattributes_to_string( super, create : string ) return string is
-- convert t/f values to "superuser, create database" like psql client
begin
  if super = "t" and create = "t" then
     return "superuser, create database";
  elsif super = "t" then
     return "superuser";
  elsif create = "t" then
     return "create database";
  end if;
  return "";
end pg_userattributes_to_string;

procedure ParseMySQLList is
  -- Syntax: mysql.list
  -- Source: N/A
  tabularDisplay : boolean := false;
  headingWidth   : integer := 0;
  wasNull        : boolean := false;
  columnWidths   : array( 1..32 ) of integer;
  totalWidth     : integer;
  width          : integer;
begin
  expect( mysql_list_t );
  if isExecutingCommand then
     begin
     if is_connected( C ) then
        -- Show tablename and kind, lookup owner from another table.
-- Owner not implemented yet - DEBUG

        prepare( Q, "select table_name, table_type, engine from " &
                    "information_schema.tables order by table_name desc" );

        -- This is the PostgreSQL query...
        -- prepare( Q, "select n.nspname as " & '"' & "Schema" & '"' &
        --   ", c.relname as " & '"' & "Name" & '"' &
        --   ", c.relkind as " & '"' & "Type" & '"' &
        --   ", u.usename as " &  '"' & "Owner" & '"' &
        --   " from pg_class c, pg_user u, pg_namespace n where u.usesysid = c.relowner and n.oid = c.relnamespace and c.relkind <> 't' and c.relkind <> 'i' and u.usesysid <> 1 order by c.relname" );

        -- MySQL 5.0.x has information_schema.  Older versions must fall back
        -- to a show query.

        begin
           execute( Q, C );
        exception when others =>
           prepare( Q, "show tables" );
           begin
              execute( Q, C );
           exception when others =>
              raise;
           end;
        end;

        -- Initially, the columns widths are the widths of the headings

        for i in 1..columns( Q ) loop
            columnWidths( i ) := column_name( Q, Column_Index_Type( i ) )'length;
            if columnWidths( i ) < 4 then -- room for NULL on tabular display
               columnWidths( i ) := 4;
            end if;
            if headingWidth < columnWidths( i ) then
               headingWidth := columnWidths( i );
            end if;
        end loop;

        -- Check query results and adjust the columns widths for the longest
        -- results.

        while not end_of_query( Q ) loop
              fetch( Q );
              for i in 1..columns( Q ) loop
                 if not is_null( Q, Column_Index_Type( i ) ) then
                    if i = 3 then -- column 2 is table type
                       width := length( to_unbounded_string( pg_kind_to_string( Value( Q, Column_Index_Type( i ) ) ) ) );
                    else
                       width := length( to_unbounded_string( Value( Q, Column_Index_Type( i ) ) ) );
                    end if;
                    if width > 256 then
                       width := 256;
                    end if;
                    if width > columnWidths( i ) then
                       columnWidths( i ) := width;
                    end if;
                 end if;
              end loop;
              exit when wasSIGINT;
        end loop;

        -- Add up all columns for the total width for a tabular display

        totalWidth := 2;                                        -- left/right marg
        for i in 1..columns( Q ) loop
            totalWidth := totalWidth + columnWidths( i );       -- width of column
            if i /= columns(Q) then                             -- not last col?
               totalWidth := totalWidth + 3;                    -- 3 char sep
            end if;
        end loop;

        -- Rewind the clear and prepare to show the results

        Rewind( Q );
        new_line;

        -- Use a tabular display only if will fit in the current display

        tabularDisplay := totalWidth <= integer( displayInfo.col );

        -- Draw the columns

        if tabularDisplay then
           put( " " );
           for i in 1..columns( Q ) loop
               put(
                  to_string(
                      Head(
                          to_unbounded_string(
                              column_name( Q, Column_Index_Type( i ) ) )
                      , columnWidths( i ) )
                  )
               );
               if i /= columns( Q ) then
                  put( " | " );
               end if;
           end loop;
           new_line;
           put( "-" );
           for i in 1..columns( Q ) loop
               put( to_string( columnWidths( i ) * "-" ) );
               if i /= columns( Q ) then
                  put( "-+-" );
               else
                  put( "-" );
               end if;
           end loop;
           new_line;
        end if;

        -- Draw the query results

        while not end_of_query( Q ) loop
            fetch( Q );
            if tabularDisplay then
               put( " " );
            end if;
            for i in 1..columns( Q ) loop
                if tabularDisplay then
                   if is_null( Q, Column_Index_Type( i ) ) then
                      put( inverse( to_string( Head( to_unbounded_string( "NULL" ), columnWidths( i ) ) ) ) );
                      wasNull := true;
                   elsif i = 3 then -- column 2 is table type
                      put( to_string( Head( ToEscaped( to_unbounded_string( pg_kind_to_string( value( Q, Column_Index_Type( i ) ) ) ) ), columnWidths( i ) ) ) );
                   else
                      put( to_string( Head( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ), columnWidths( i ) ) ) );
                   end if;
                   if i /= columns( Q ) then
                      put( " | " );
                   end if;
                else
                   put( to_string( head( to_unbounded_string( column_name( Q, Column_Index_Type( i ) ) ), headingWidth ) ) );
                   put( ": " );
                   if is_null( Q, Column_Index_Type( i ) ) then
                      put( inverse( "NULL" ) );
                      wasNull := true;
                   elsif i = 3 then
                      put( bold( to_string( ToEscaped( to_unbounded_string( pg_kind_to_string( value( Q, Column_Index_Type( i ) ) ) ) ) ) ) );
                   else
                      put( bold( to_string( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ) ) ) );
                   end if;
                   new_line;
                end if;
            end loop;
            new_line;
            exit when wasSIGINT;
        end loop;
     end if;

     -- Draw the summary line

     if tuples( Q ) = 1 then
        put( " 1 Row" );
     else
        put( Tuple_Index_Type'image( tuples( Q ) ) );
        put( " Rows" );
     end if;
     if wasNull then
        put( " with nulls" );
     end if;
     if columns( Q ) = 1 then
        put( " and 1 Column" );
     else
        put( " and" );
        put( integer'image( columns( Q ) ) );
        put( " Columns" );
     end if;
     new_line;
     exception when no_tuple =>
       err( "no tuple" );
     when null_value =>
       err( "null value" );
     when no_column =>
       err( "no column" );
     when no_result =>
       err( "no result" );
     when sql_error =>
       err( Error_Message( Q ) );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLList;

procedure ParseMySQLSchema is
  -- Syntax: mysql.schema( "table" );
  -- Source: N/A
  tabularDisplay : boolean := false;
  headingWidth   : integer := 0;
  wasNull        : boolean := false;
  columnWidths   : array( 1..32 ) of integer;
  totalWidth     : integer;
  width          : integer;
  exprType       : identifier;
  exprVal        : unbounded_string;
begin
  expect( mysql_schema_t );
  expect( symbol_t, "(" );
  ParseExpression( exprVal, exprType );
  if baseTypesOK( exprType, string_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
     if is_connected( C ) then
        -- Find column name, type, type length, not null and default flags
        -- Don't show dropped columns, columns with system types (oid, etc).
        -- The PostgreSQL query.
        -- prepare( Q, "select a.attname as " & '"' & "Column" & '"' &
        --            ", t.typname as " & '"' & "Type" & '"' &
        --            ", a.atttypmod-4 as " & '"' & "Length" & '"' &
        --            ", a.attnotnull as " & '"' & "Not Null" & '"' &
        --            ", a.atthasdef as " & '"' & "Default" & '"' &
        --            "from pg_attribute a, pg_class c, pg_type t where a.attrelid = c.oid and t.oid = a.atttypid and a.attisdropped = 'f' and (a.atttypid < 26 or a.atttypid > 29) and c.relname='" &
        --             to_string( exprVal ) & "' order by a.attnum" );

        prepare( Q, "show column_name, data_type, is_nullable, column_default from information_schema.columns where table_name = '" & to_string( exprVal ) & "'" );

        -- MySQL 5.0.x has information_schema.  Older versions must fall back
        -- to a show query.

        begin
           execute( Q, C );
        exception when others =>
           prepare( Q, "show columns from " & to_string( exprVal ) );
           begin
              execute( Q, C );
           exception when others =>
              raise;
           end;
        end;

        -- No result? Then table was not found.

	if end_of_query( Q ) then
	   err( "Did not find any relation named " &
	     optional_bold( to_string( exprVal ) ) );
	   return;
	end if;
	
        -- Initially, the columns widths are the widths of the headings

        for i in 1..columns( Q ) loop
            if i /= 3 then -- column 3 is type length (not shown)
               columnWidths( i ) := column_name( Q, Column_Index_Type( i ) )'length;
               if columnWidths( i ) < 4 then -- room for NULL on tabular display
                  columnWidths( i ) := 4;
               end if;
               if headingWidth < columnWidths( i ) then
                  headingWidth := columnWidths( i );
               end if;
            end if;
        end loop;

        -- Check query results and adjust the columns widths for the longest
        -- results.

        while not end_of_query( Q ) loop
              fetch( Q );
              for i in 1..columns( Q ) loop
                 if i /= 3 then -- column 3 is type length (not shown)
                    if not is_null( Q, Column_Index_Type( i ) ) then
                       if i = 2 then -- column 2 is column type
                          width := length( to_unbounded_string(
                                    pg_column_type_to_string( Value( Q, Column_Index_Type( i ) ),
                                                              Value( Q, Column_Index_Type( i+1 ) ) ) ) );
                       elsif i = 4 then
                          width := length( to_unbounded_string(
                                   pg_not_null_to_string( Value( Q, Column_Index_Type( i ) ) ) ) );
                       elsif i = 5 then
                          width := length( to_unbounded_string(
                                   pg_default_to_string( Value( Q, Column_Index_Type( i ) ) ) ) );
                       else
                          width := length( to_unbounded_string( Value( Q, Column_Index_Type( i ) ) ) );
                       end if;
                       if width > 256 then
                          width := 256;
                       end if;
                       if width > columnWidths( i ) then
                          columnWidths( i ) := width;
                       end if;
                    end if;
                 end if;
              end loop;
        end loop;

        -- Add up all columns for the total width for a tabular display

        totalWidth := 2;                                        -- left/right marg
        for i in 1..columns( Q ) loop
            if i /= 3 then -- column 3 is type length (not shown)
               totalWidth := totalWidth + columnWidths( i );       -- width of column
               if i /= columns(Q) then                             -- not last col?
                  totalWidth := totalWidth + 3;                    -- 3 char sep
               end if;
            end if;
        end loop;

        -- Rewind the clear and prepare to show the results

        Rewind( Q );
        new_line;

        -- Use a tabular display only if will fit in the current display

        tabularDisplay := totalWidth <= integer( displayInfo.col );

        -- Draw the columns

        if tabularDisplay then
           put( " " );
           for i in 1..columns( Q ) loop
               if i /= 3 then -- column 3 is type length (not shown)
                  put(
                     to_string(
                         Head(
                             to_unbounded_string(
                                 column_name( Q, Column_Index_Type( i ) ) )
                         , columnWidths( i ) )
                     )
                  );
                  if i /= columns( Q ) then
                     put( " | " );
                  end if;
               end if;
           end loop;
           new_line;
           put( "-" );
           for i in 1..columns( Q ) loop
               if i /= 3 then -- column 3 is type length (not shown)
                  put( to_string( columnWidths( i ) * "-" ) );
                  if i /= columns( Q ) then
                     put( "-+-" );
                  else
                     put( "-" );
                  end if;
               end if;
           end loop;
           new_line;
        end if;

        -- Draw the query results

        while not end_of_query( Q ) loop
            fetch( Q );
            if tabularDisplay then
               put( " " );
            end if;
            for i in 1..columns( Q ) loop
                if i /= 3 then -- column 3 is type length (not shown)
                   if tabularDisplay then
                      if is_null( Q, Column_Index_Type( i ) ) then
                         put( inverse( to_string( Head( to_unbounded_string( "NULL" ), columnWidths( i ) ) ) ) );
                         wasNull := true;
                      elsif i = 2 then -- column 2 is column type
                         put( to_string( Head( ToEscaped( to_unbounded_string( pg_column_type_to_string( value( Q, Column_Index_Type( i ) ), Value( Q, Column_Index_Type( i+1 ) ) ) ) ), columnWidths( i ) ) ) );
                      elsif i = 4 then
                         put( to_string( Head( ToEscaped( to_unbounded_string( pg_not_null_to_string( value( Q, Column_Index_Type( i ) ) ) ) ), columnWidths( i ) ) ) );
                      elsif i = 5 then
                         put( to_string( Head( ToEscaped( to_unbounded_string( pg_default_to_string( value( Q, Column_Index_Type( i ) ) ) ) ), columnWidths( i ) ) ) );
                      else
                         put( to_string( Head( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ), columnWidths( i ) ) ) );
                      end if;
                      if i /= columns( Q ) then
                         put( " | " );
                      end if;
                   else
                      put( to_string( head( to_unbounded_string( column_name( Q, Column_Index_Type( i ) ) ), headingWidth ) ) );
                      put( ": " );
                      if is_null( Q, Column_Index_Type( i ) ) then
                         put( inverse( "NULL" ) );
                         wasNull := true;
                      elsif i = 2 then
                         put( bold( to_string( ToEscaped( to_unbounded_string( pg_column_type_to_string( value( Q, Column_Index_Type( i ) ), Value( Q, Column_Index_Type( i+1 ) ) ) ) ) ) ) );
                      elsif i = 4 then
                         put( bold( to_string( ToEscaped( to_unbounded_string( pg_not_null_to_string( value( Q, Column_Index_Type( i ) ) ) ) ) ) ) );
                      elsif i = 5 then
                         put( bold( to_string( ToEscaped( to_unbounded_string( pg_default_to_string( value( Q, Column_Index_Type( i ) ) ) ) ) ) ) );
                      else
                         put( bold( to_string( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ) ) ) );
                      end if;
                      new_line;
                   end if;
                end if;
            end loop;
            new_line;
        end loop;
     end if;

     -- Draw the summary line

     if tuples( Q ) = 1 then
        put( " 1 Row" );
     else
        put( Tuple_Index_Type'image( tuples( Q ) ) );
        put( " Rows" );
     end if;
     if wasNull then
        put( " with nulls" );
     end if;
     if columns( Q ) = 1 then
        put( "and 1 Column" );
     else
        put( " and" );
        put( integer'image( columns( Q ) ) );
        put( " Columns" );
     end if;
     new_line;

     -- List of indexes: not available yet through information_schema

     prepare( Q, "show index from " & to_string( exprVal ) );
     begin
        execute( Q, C );
     exception when others =>
        raise;
     end;

     if tuples( Q ) > 0 then
        put_line( "Indexes:" );
        while not end_of_query( Q ) loop
            fetch( Q );
            put( "    " );
            declare
               indexName   : string := Value( Q, 1 );
               primaryKey  : string := Value( Q, 2 );
               uniqueIndex : string := Value( Q, 3 );
               colList     : string := Value( Q, 4 );
            begin
               put( bold( indexName ) );
               put( " " );
               if primaryKey = "t" then
                  put( "primary key " );
               end if;
               if uniqueIndex = "t" then
                  put( "unique " );
               end if;
               put( "on columns " );
               put( bold( colList ) );
            end;
            new_line;
        end loop;
     end if;

     exception when no_tuple =>
       err( "no tuple" );
     when null_value =>
       err( "null value" );
     when no_column =>
       err( "no column" );
     when no_result =>
       err( "no result" );
     when sql_error =>
       err( Error_Message( Q ) );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLSchema;

procedure ParseMySQLUsers is
  -- Syntax: mysql.users
  -- Source: N/A
  tabularDisplay : boolean := false;
  headingWidth   : integer := 0;
  wasNull        : boolean := false;
  columnWidths   : array( 1..32 ) of integer;
  totalWidth     : integer;
  width          : integer;
begin
  expect( mysql_users_t );
  if isExecutingCommand then
     begin
     if is_connected( C ) then

        -- Privileges are unique to a particular database engine

        prepare( Q, "select User as " & '"' & "User Name" & '"' & ", Host" &
                    ", Select_priv, Insert_priv, Update_priv, Delete_priv," &
                    "Create_priv, Drop_priv, Reload_priv, Shutdown_priv," &
                    "Process_priv, File_priv, Grant_priv, References_priv," &
                    "Index_priv, Show_db_priv, Super_priv," &
                    "Create_tmp_table_priv, Lock_tables_priv, Execute_priv," &
                    "Repl_slave_priv, Repl_client_priv from mysql.user" &
                    " order by User;" );
        execute( Q, C );

        -- Initially, the columns widths are the widths of the headings

        for i in 1..columns( Q ) loop
            columnWidths( i ) := column_name( Q, Column_Index_Type( i ) )'length;
            if columnWidths( i ) < 4 then -- room for NULL on tabular display
               columnWidths( i ) := 4;
            end if;
            if headingWidth < columnWidths( i ) then
               headingWidth := columnWidths( i );
            end if;
        end loop;

        -- Check query results and adjust the columns widths for the longest
        -- results.

        while not end_of_query( Q ) loop
              fetch( Q );
              for i in 1..columns( Q ) loop
                 if not is_null( Q, Column_Index_Type( i ) ) then
                    if i = 3 then -- column 3 and 4 are attributes
                       width := length( to_unbounded_string( pg_userattributes_to_string( Value( Q, Column_Index_Type( i ) ), Value( Q, Column_Index_Type( i ) ) ) ) );
                    elsif i = 4 then
                       null;
                    else
                       width := length( to_unbounded_string( Value( Q, Column_Index_Type( i ) ) ) );
                    end if;
                    if width > 256 then
                       width := 256;
                    end if;
                    if width > columnWidths( i ) then
                       columnWidths( i ) := width;
                    end if;
                 end if;
              end loop;
        end loop;

        -- Add up all columns for the total width for a tabular display

        totalWidth := 2;                                        -- left/right marg
        for i in 1..columns( Q ) loop
            totalWidth := totalWidth + columnWidths( i );       -- width of column
            if i /= columns(Q) then                             -- not last col?
               totalWidth := totalWidth + 3;                    -- 3 char sep
            end if;
        end loop;

        -- Rewind the clear and prepare to show the results

        Rewind( Q );
        new_line;

        -- Use a tabular display only if will fit in the current display

        tabularDisplay := totalWidth <= integer( displayInfo.col );

        -- Draw the columns

        if tabularDisplay then
           put( " " );
           for i in 1..columns( Q ) loop
               if i /= 4 then
                  put(
                     to_string(
                         Head(
                             to_unbounded_string(
                                 column_name( Q, Column_Index_Type( i ) ) )
                         , columnWidths( i ) )
                     )
                  );
                  if i /= 3 then
                     put( " | " );
                  end if;
               end if;
           end loop;
           new_line;
           put( "-" );
           for i in 1..columns( Q ) loop
               if i /= 4 then
                  put( to_string( columnWidths( i ) * "-" ) );
                  if i /= 3 then
                     put( "-+-" );
                  else
                     put( "-" );
                  end if;
               end if;
           end loop;
           new_line;
        end if;

        -- Draw the query results

        while not end_of_query( Q ) loop
            fetch( Q );
            if tabularDisplay then
               put( " " );
            end if;
            for i in 1..columns( Q ) loop
                if i /= 4 then
                   if tabularDisplay then
                      if is_null( Q, Column_Index_Type( i ) ) then
                         put( inverse( to_string( Head( to_unbounded_string( "NULL" ), columnWidths( i ) ) ) ) );
                         wasNull := true;
                      elsif i = 2 then -- right-aligned
                         put( to_string( Tail( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ), columnWidths( i ) ) ) );
                      elsif i = 3 then -- column 3 and 4 are attributes
                         put( to_string( Head( ToEscaped( to_unbounded_string( pg_userattributes_to_string( value( Q, Column_Index_Type( i ) ), Value( Q, Column_Index_Type( i ) ) ) ) ), columnWidths( i ) ) ) );
                      else
                         put( to_string( Head( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ), columnWidths( i ) ) ) );
                      end if;
                      if i /= 3 then
                         put( " | " );
                      end if;
                   else
                      put( to_string( head( to_unbounded_string( column_name( Q, Column_Index_Type( i ) ) ), headingWidth ) ) );
                      put( ": " );
                      if is_null( Q, Column_Index_Type( i ) ) then
                         put( inverse( "NULL" ) );
                         wasNull := true;
                      elsif i = 3 then -- column 3 and 4 are attributes
                         put( bold( to_string( ToEscaped( to_unbounded_string( pg_userattributes_to_string( value( Q, Column_Index_Type( i ) ), Value( Q, Column_Index_Type( i ) ) ) ) ) ) ) );
                      else
                         put( bold( to_string( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ) ) ) );
                      end if;
                      new_line;
                   end if;
                end if;
            end loop;
            new_line;
        end loop;
     end if;

     -- Draw the summary line

     if tuples( Q ) = 1 then
        put( " 1 Row" );
     else
        put( Tuple_Index_Type'image( tuples( Q ) ) );
        put( " Rows" );
     end if;
     if wasNull then
        put( " with nulls" );
     end if;
     if columns( Q ) = 1 then
        put( "and 1 Column" );
     else
        put( " and" );
        put( integer'image( columns( Q ) ) );
        put( " Columns" );
     end if;
     new_line;
     exception when no_tuple =>
       err( "no tuple" );
     when null_value =>
       err( "null value" );
     when no_column =>
       err( "no column" );
     when no_result =>
       err( "no result" );
     when sql_error =>
       err( Error_Message( Q ) );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLUsers;

procedure ParseMySQLDatabases is
  -- Syntax: mysql.databases
  -- Source: N/A
  tabularDisplay : boolean := false;
  headingWidth   : integer := 0;
  wasNull        : boolean := false;
  columnWidths   : array( 1..32 ) of integer;
  totalWidth     : integer;
  width          : integer;
begin
  expect( mysql_databases_t );
  if isExecutingCommand then
     begin
     if is_connected( C ) then
        -- Show tablename and kind, lookup owner from another table.
        -- Don't show tables owned by postgres (user 1), TOAST tables or indexes
        -- PostgreSQL query
        -- prepare( Q, "select d.datname as " & '"' & "Name" & '"' &
        --             ", u.usename as " & '"' & "Owner" & '"' &
        --             " from pg_database d, pg_user u where u.usesysid = d.datdba order by d.datname" );

        prepare( Q, "select schema_name as 'Database' from information_schema.schemata" );

        -- MySQL 5.0.x has information_schema.  Older versions must fall back
        -- to a show query.

        begin
           execute( Q, C );
        exception when others =>
           prepare( Q, "show databases" );
           begin
              execute( Q, C );
           exception when others =>
              raise;
           end;
        end;

        -- Initially, the columns widths are the widths of the headings

        for i in 1..columns( Q ) loop
            columnWidths( i ) := column_name( Q, Column_Index_Type( i ) )'length;
            if columnWidths( i ) < 4 then -- room for NULL on tabular display
               columnWidths( i ) := 4;
            end if;
            if headingWidth < columnWidths( i ) then
               headingWidth := columnWidths( i );
            end if;
        end loop;

        -- Check query results and adjust the columns widths for the longest
        -- results.

        while not end_of_query( Q ) loop
              fetch( Q );
              for i in 1..columns( Q ) loop
                 if not is_null( Q, Column_Index_Type( i ) ) then
                    if i = 3 then -- column 2 is table type
                       width := length( to_unbounded_string( pg_kind_to_string( Value( Q, Column_Index_Type( i ) ) ) ) );
                    else
                       width := length( to_unbounded_string( Value( Q, Column_Index_Type( i ) ) ) );
                    end if;
                    if width > 256 then
                       width := 256;
                    end if;
                    if width > columnWidths( i ) then
                       columnWidths( i ) := width;
                    end if;
                 end if;
              end loop;
        end loop;

        -- Add up all columns for the total width for a tabular display

        totalWidth := 2;                                        -- left/right marg
        for i in 1..columns( Q ) loop
            totalWidth := totalWidth + columnWidths( i );       -- width of column
            if i /= columns(Q) then                             -- not last col?
               totalWidth := totalWidth + 3;                    -- 3 char sep
            end if;
        end loop;

        -- Rewind the clear and prepare to show the results

        Rewind( Q );
        new_line;

        -- Use a tabular display only if will fit in the current display

        tabularDisplay := totalWidth <= integer( displayInfo.col );

        -- Draw the columns

        if tabularDisplay then
           put( " " );
           for i in 1..columns( Q ) loop
               put(
                  to_string(
                      Head(
                          to_unbounded_string(
                              column_name( Q, Column_Index_Type( i ) ) )
                      , columnWidths( i ) )
                  )
               );
               if i /= columns( Q ) then
                  put( " | " );
               end if;
           end loop;
           new_line;
           put( "-" );
           for i in 1..columns( Q ) loop
               put( to_string( columnWidths( i ) * "-" ) );
               if i /= columns( Q ) then
                  put( "-+-" );
               else
                  put( "-" );
               end if;
           end loop;
           new_line;
        end if;

        -- Draw the query results

        while not end_of_query( Q ) loop
            fetch( Q );
            if tabularDisplay then
               put( " " );
            end if;
            for i in 1..columns( Q ) loop
                if tabularDisplay then
                   if is_null( Q, Column_Index_Type( i ) ) then
                      put( inverse( to_string( Head( to_unbounded_string( "NULL" ), columnWidths( i ) ) ) ) );
                      wasNull := true;
                   elsif i = 3 then -- column 2 is table type
                      put( to_string( Head( ToEscaped( to_unbounded_string( pg_kind_to_string( value( Q, Column_Index_Type( i ) ) ) ) ), columnWidths( i ) ) ) );
                   else
                      put( to_string( Head( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ), columnWidths( i ) ) ) );
                   end if;
                   if i /= columns( Q ) then
                      put( " | " );
                   end if;
                else
                   put( to_string( head( to_unbounded_string( column_name( Q, Column_Index_Type( i ) ) ), headingWidth ) ) );
                   put( ": " );
                   if is_null( Q, Column_Index_Type( i ) ) then
                      put( inverse( "NULL" ) );
                      wasNull := true;
                   elsif i = 3 then
                      put( bold( to_string( ToEscaped( to_unbounded_string( pg_kind_to_string( value( Q, Column_Index_Type( i ) ) ) ) ) ) ) );
                   else
                      put( bold( to_string( ToEscaped( to_unbounded_string( value( Q, Column_Index_Type( i ) ) ) ) ) ) );
                   end if;
                   new_line;
                end if;
            end loop;
            new_line;
        end loop;
     end if;

     -- Draw the summary line

     if tuples( Q ) = 1 then
        put( " 1 Row" );
     else
        put( Tuple_Index_Type'image( tuples( Q ) ) );
        put( " Rows" );
     end if;
     if wasNull then
        put( " with nulls" );
     end if;
     if columns( Q ) = 1 then
        put( " and 1 Column" );
     else
        put( " and" );
        put( integer'image( columns( Q ) ) );
        put( " Columns" );
     end if;
     new_line;
     exception when no_tuple =>
       err( "no tuple" );
     when null_value =>
       err( "null value" );
     when no_column =>
       err( "no column" );
     when no_result =>
       err( "no result" );
     when sql_error =>
       err( Error_Message( Q ) );
     when others =>
       err( "exception was raised" );
     end;
  end if;
end ParseMySQLDatabases;

procedure DoMySQLSQLSelect( sqlcmd : unbounded_string ) is
begin
  if isExecutingCommand then
     prepare( Q, to_string( sqlcmd ) );
     MySQLShowIt;
  end if;
end DoMySQLSQLSelect;

procedure DoMySQLSQLStatement( sqlcmd : unbounded_string ) is
begin
  if isExecutingCommand then
     prepare( Q, to_string( sqlcmd ) );
     begin
       Execute( Q, C );
     exception when not_connected =>
       err( "not connected" );
     when abort_state =>
       err( "in abort state" );
     when sql_error =>
       err( Error_Message( Q ) );
     when others =>
       err( "exception raised" );
     end;
  end if;
end DoMySQLSQLStatement;

#end if;

procedure StartupMySQL is
begin
  declareIdent( mysql_column_index_type_t, "mysql.column_index_type",
    positive_t, typeClass );

  declareIdent( mysql_tuple_index_type_t, "mysql.tuple_index_type",
    positive_t, typeClass );

  declareIdent( mysql_tuple_count_type_t, "mysql.tuple_count_type",
    mysql_tuple_index_type_t, subClass );

--  declareIdent( mysql_trace_mode_type_t, "mysql.trace_mode_type",
--    root_enumerated_t, typeClass );
--  declareStandardConstant( mysql_trace_none_t, "mysql.trace_none",
--    mysql_trace_mode_type_t, "0" );
--  declareStandardConstant( mysql_trace_db_t, "mysql.trace_db",
--    mysql_trace_mode_type_t, "1" );
--  declareStandardConstant( mysql_trace_apq_t, "mysql.trace_apq",
--    mysql_trace_mode_type_t, "2" );
--  declareStandardConstant( mysql_trace_full_t, "mysql.trace_full",
--    mysql_trace_mode_type_t, "3" );

  declareIdent( mysql_mode_type_t, "mysql.mode_type",
    root_enumerated_t, typeClass );
  declareStandardConstant( mysql_read_t, "mysql.read",
    mysql_mode_type_t, "0" );
  declareStandardConstant( mysql_write_t, "mysql.write",
    mysql_mode_type_t, "1" );
  declareStandardConstant( mysql_read_write_t, "mysql.read_write",
    mysql_mode_type_t, "2" );

--  declareIdent( mysql_fetch_mode_type_t, "mysql.fetch_mode_type",
--    root_enumerated_t, typeClass );
--  declareStandardConstant( mysql_sequential_fetch_t, "mysql.sequential_fetch",
--    mysql_fetch_mode_type_t, "0" );
--  declareStandardConstant( mysql_random_fetch_t, "mysql.random_fetch",
--    mysql_fetch_mode_type_t, "1" );

--  declareIdent( mysql_database_type_t, "mysql.database_type",
--    root_enumerated_t, typeClass );
--  declareStandardConstant( mysql_engine_postgresql_t, "mysql.engine_postgresql",
--    mysql_database_type_t, "0" );
--  declareStandardConstant( mysql_engine_mysql_t, "mysql.engine_mysql",
--    mysql_database_type_t, "1" );
--  declareStandardConstant( mysql_engine_oracle_t, "mysql.engine_oracle",
--    mysql_database_type_t, "2" );
--  declareStandardConstant( mysql_engine_sybase_t, "mysql.engine_sybase",
--    mysql_database_type_t, "3" );
--  declareStandardConstant( mysql_engine_db2_t, "mysql.engine_db2",
--    mysql_database_type_t, "4" );

  declareProcedure( mysql_connect_t, "mysql.connect" );
  declareProcedure( mysql_disconnect_t, "mysql.disconnect" );
  declareFunction( mysql_is_connected_t, "mysql.is_connected" );
  declareProcedure( mysql_reset_t, "mysql.reset" );
  declareFunction( mysql_error_message_t, "mysql.error_message" );
  declareFunction( mysql_notice_message_t, "mysql.notice_message" );
  declareFunction( mysql_in_abort_state_t, "mysql.in_abort_state" );
  declareFunction( mysql_options_t, "mysql.options" );
  declareFunction( mysql_will_rollback_on_finalize_t, "mysql.will_rollback_on_finalize" );
  declareProcedure( mysql_set_rollback_on_finalize_t, "mysql.set_rollback_on_finalize" );
  declareProcedure( mysql_open_db_trace_t, "mysql.open_db_trace" );
  declareProcedure( mysql_close_db_trace_t, "mysql.close_db_trace" );
  declareProcedure( mysql_set_trace_t, "mysql.set_trace" );
  declareFunction( mysql_is_trace_t, "mysql.is_trace" );
  declareProcedure( mysql_clear_t, "mysql.clear" );
  declareProcedure( mysql_prepare_t, "mysql.prepare" );
  declareProcedure( mysql_append_t, "mysql.append" );
  declareProcedure( mysql_append_line_t, "mysql.append_line" );
  declareProcedure( mysql_append_quoted_t, "mysql.append_quoted" );
  declareProcedure( mysql_execute_t, "mysql.execute" );
  declareProcedure( mysql_execute_checked_t, "mysql.execute_checked" );
  declareProcedure( mysql_raise_exceptions_t, "mysql.raise_exceptions" );
  declareProcedure( mysql_report_errors_t, "mysql.report_errors" );
  declareProcedure( mysql_begin_work_t, "mysql.begin_work" );
  declareProcedure( mysql_commit_work_t, "mysql.commit_work" );
  declareProcedure( mysql_rollback_work_t, "mysql.rollback_work" );
  declareProcedure( mysql_rewind_t, "mysql.rewind" );
  declareProcedure( mysql_fetch_t, "mysql.fetch" );
  declareFunction( mysql_end_of_query_t, "mysql.end_of_query" );
  declareFunction( mysql_tuple_t, "mysql.tuple" );
  declareFunction( mysql_tuples_t, "mysql.tuples" );
  declareFunction( mysql_columns_t, "mysql.columns" );
  declareFunction( mysql_column_name_t, "mysql.column_name" );
  declareFunction( mysql_column_index_t, "mysql.column_index" );
  declareFunction( mysql_column_type_t, "mysql.column_type" );
  declareFunction( mysql_is_null_t, "mysql.is_null" );
  declareFunction( mysql_value_t, "mysql.value" );
  declareFunction( mysql_engine_of_t, "mysql.engine_of" );
  declareProcedure( mysql_show_t, "mysql.show" );
  declareProcedure( mysql_list_t, "mysql.list" );
  declareProcedure( mysql_schema_t, "mysql.schema" );
  declareProcedure( mysql_users_t, "mysql.users" );
  declareProcedure( mysql_databases_t, "mysql.databases" );

  --declareFunction( db_do_t, "db.do" );
  --declareFunction( db_fetchrow_t, "dbi.fetchrow" );

  --declareFunction( dbi_prepare_t, "dbi.prepare" );

end StartupMySQL;

procedure ShutdownMySQL is
begin
  null;
end ShutdownMySQL;

end parser_mysql;
