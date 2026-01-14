------------------------------------------------------------------------------
-- Logs Package Parser                                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2025 Free Software Foundation              --
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

with
    gnat.source_info,
    gnat.os_lib,
    interfaces.c,
    ada.exceptions,
    ada.text_io,
    ada.strings.unbounded,
    ada.calendar,
    gnat.lock_files,
    spar_os,
    pegasoft.strings,
    pegasoft.user_io,
    world,
    symbol_table,
    message_strings,
    value_conversion,
    compiler,
    scanner,
    scanner.communications,
    chain_util,
    parser_aux,
    parser_params,
    -- for writeCurrentError
    parser_tio;
use
    gnat.os_lib,
    interfaces.c,
    ada.exceptions,
    ada.text_io,
    ada.strings,
    ada.strings.unbounded,
    ada.calendar,
    gnat.lock_files,
    spar_os,
    pegasoft.user_io,
    pegasoft,
    pegasoft.strings,
    world,
    symbol_table,
    message_strings,
    value_conversion,
    compiler,
    scanner,
    scanner.communications,
    chain_util,
    parser_aux,
    parser_params,
    parser_tio;

package body parser_logs is

defaultWidth : constant positive := 1;

-- stderr_log - output log messages to standard error
-- file_log   - output log messages to a file
-- echo_log   - output log messages to both a file and standard error

type log_modes is ( stderr_log, file_log, echo_log );

log_path        : storage;                          -- path to log file + meta
lock_file_path  : unbounded_string;                       -- path to lock file
level           : natural;                                -- indentation level
width           : positive;                        -- line column to start msg
log_mode        : log_modes;
log_is_open     : boolean;                       -- true if open has been used
log_is_rotating : boolean;                      -- true if rotating_begin used
log_open_time   : time;                       -- timestamp when log was opened

string_header   : unbounded_string;                   -- leading part of entry
string_message  : unbounded_string;                           -- body of entry
indent_required : natural;                       -- > 0  if indent not applied
started_message : boolean;
last_message    : unbounded_string;           -- last entry body for dup check
dup_count       : natural;                            -- number of dup entries

entity          : unbounded_string;  -- enclosing entity
hasEntity       : boolean := false;

-- Simple Metrics

type loggingMetrics is record
  --line_count    : natural := 0;
  ok_count      : natural := 0;
  info_count    : natural := 0;
  warning_count : natural := 0;
  error_count   : natural := 0;
end record;

checkpointMetrics : loggingMetrics;
currentMetrics    : loggingMetrics;

------------------------------------------------------------------------------
-- Logs package identifiers
------------------------------------------------------------------------------

log_level_t         : identifier;

log_modes_t         : identifier;
log_mode_stderr_t   : identifier;
log_mode_file_t     : identifier;
log_mode_echo_t     : identifier;

logs_level_begin_t  : identifier;
logs_level_end_t    : identifier;

logs_ok_t           : identifier;
logs_info_t         : identifier;
logs_warning_t      : identifier;
logs_error_t        : identifier;
logs_open_t         : identifier;
logs_close_t        : identifier;

logs_is_open_t      : identifier;
logs_mode_t         : identifier;
-- logs_level_t       : identifier;
logs_width_t         : identifier;

logs_rotate_begin_t  : identifier;
logs_rotate_end_t    : identifier;
logs_is_rotating_t   : identifier;
logs_metrics_t       : identifier;
logs_checkpoint_t    : identifier;


------------------------------------------------------------------------------
-- Utility subprograms
------------------------------------------------------------------------------


--  GET LINE NO
--
-- Return the current line number.
------------------------------------------------------------------------------

function getLineNo return unbounded_string is
begin
  return trim( to_unbounded_string( natural'image( getLineNo ) ), left );
end getLineNo;


--  WRITE CURRENT ERROR
--
-- Write a message to the current error file (or standard error).  This
-- handles redirection of current error.
--
-- This source code is copied from the SparForte text_io package so we
-- can handle I/O redirection of standard error.  This is primarily
-- from Put_Line.
------------------------------------------------------------------------------

procedure writeCurrentError( expr : unbounded_string ) is
  result    : size_t;
  ch        : character;
  fd        : aFileDescriptor;
  retry     : boolean;
  theCurrentErrorFileRec : storage;
begin
  -- This is an alias a text_io file_type record

  -- theFileRec := identifier( to_numeric( identifiers(current_error_t).store.value ) );
  theCurrentErrorFileRec := identifiers(current_error_t).store.all;

  -- Check to see that the file is open
  if length( theCurrentErrorFileRec.value ) = 0 then
     err( +"log file is not open" );
     return;
  end if;

  -- Get the file descriptor

  fd := aFileDescriptor'value( to_string( stringField( theCurrentErrorFileRec.value, recSep, fd_field ) ) );

  -- If current_error is standard-error, use Ada's Text_IO
  -- TODO: This is probably redundant and should use the same technique as
  -- a regular file.

  --if ref.id = standard_error_t then
  if fd = originalStandardError then
     -- Ada doesn't handle interrupted system calls properly.
     -- maybe a more elegant way to do this...
     loop
        retry := false;
        begin
          put_line_retry( standard_error, to_string( expr ) );
        exception when msg: device_error =>
          if exception_message( msg ) = "interrupted system call" then
             retry := true;
          else
             err( pl( exception_message( msg ) ) );
          end if;
        end;
     exit when not retry;
     end loop;

  -- If redirected to a file, handle that here.

  else
     -- id := aFileDescriptor'value( to_string( stringField( ref, fd_field ) ) );
     for i in 1..length( expr ) loop
         ch := Element( expr, i );
<<logwrite>> writechar( result, fd, ch, 1 );
         if result < 0 then
            if C_errno = EAGAIN or C_errno = EINTR then
               goto logwrite;
            end if;
            err( pl( "unable to write: " & OSerror( C_errno ) ) );
            exit;
         end if;
     end loop;

     -- Add a end-of-line and, if successful, adjust the line count

     for i in eol_characters'range loop
        ch := eol_characters(i);
<<logwrite2>>
        writechar( result, fd, ch, 1 ); -- add a line feed
        if result < 0 then
           if C_errno = EAGAIN or C_errno = EINTR then
              goto logwrite2;
           end if;
           err( pl( "unable to write: " & OSerror( C_errno ) ) );
        end if;
     end loop;
     if not error_found then
        replaceField( identifiers(current_error_t).store.value,
           recSep,
           line_field,
           long_integer'image( long_integer'value(
              to_string( stringField( identifiers(current_error_t).store.value,
              recSep, line_field ) ) ) + 1 ) );
     end if;
  end if;
  exception when others =>
     -- shouldn't normally happen unless the current input/output/error
     -- points to the wrong value.
     err( contextNotes => pl( "At " & gnat.source_info.source_location &
            " while writing to current error using file_type '" ) &
            em_value( theCurrentErrorFileRec.value ) &
            pl( "'" ),
          subjectNotes => subjectInterpreter,
          reason => +"had an internal error because",
          obstructorNotes => em( "an unexpected exception was raised" )
      );
end writeCurrentError;


--  RESET METRICS
--
-- Reset the log counters.
------------------------------------------------------------------------------

procedure resetMetrics is
begin
  checkpointMetrics := currentMetrics;
  currentMetrics.ok_count      := 0;
  currentMetrics.info_count    := 0;
  currentMetrics.warning_count := 0;
  currentMetrics.error_count   := 0;
end resetMetrics;


--  RESET LOG
--
-- Set the logger in its default state, writing to standard error.
------------------------------------------------------------------------------

procedure resetLog is
begin
  level := 0;
  width := defaultWidth;
  log_mode := stderr_log;
  log_path := nullStorage;
  lock_file_path := null_unbounded_string;
  log_is_open := false;
  log_is_rotating := false;
  started_message := true;
  dup_count := 0;
  last_message := null_unbounded_string;
  indent_required := 0;
  resetMetrics;
end resetLog;


--  LOG INDENT MESSAGE
--
-- Add the indent field to a log message.
-----------------------------------------------------------------------------

procedure log_indent_message( msg : in out unbounded_string; clear_indent : boolean ) is
begin
  msg := msg  &  ":";
  while length( string_header & msg ) < width loop
     msg := msg & ' ';
  end loop;
  if indent_required > 0 then
     msg := msg & (indent_required * ' ');
     if clear_indent then
        indent_required := 0;
     end if;
  end if;
  msg := msg  &  ":";
end log_indent_message;


--  LOG CLEAN MESSAGE
--
-- Escape special characters (including colon, used to denote log fields).
-----------------------------------------------------------------------------

procedure log_clean_message( message : in out unbounded_string ) is
  p : natural;
begin
  -- Escape colons
  loop
     p := index( message, ":" );
  exit when p = 0;
     message := delete( message, p, p );
     message := insert( message, p, "[# 58]" );
  end loop;
  -- Escape control characters
  message := toCtrlEscaped( message );
end log_clean_message;


-----------------------------------------------------------------------------
-- Core Loggers
--
-- These routines construct and write the log messages.
-----------------------------------------------------------------------------

--  LOG FIRST PART
--
-- Build the first part of the log message: date, program and location.
-----------------------------------------------------------------------------

--procedure log_first_part( m : unbounded_string; level_tag : string ) is
procedure log_first_part( level_tag : string ) is
  sourceFile : unbounded_string;
begin
  string_header := getDateString( clock ) & ":";
  sourceFile := basename( getSourceFileName );
  string_message := trim( to_unbounded_string( aPID'image( getpid ) ), left ) & ":";
  string_message := string_message & entity & ":";
  string_message := string_message & level_tag & ":";
-- TODO: we don't need a custom first field anymore
  string_message := string_message & to_string( sourceFile ) & ":" & getLineNo;
  --string_message := string_message & m & ":";
  indent_required := level * 2;
  started_message := false;
end log_first_part;


--  LOG MIDDLE PART
--
-- Build the middle part of the log message.  Indent and show the first
-- or next part of the user's message.
-----------------------------------------------------------------------------

procedure log_middle_part( m : unbounded_string ) is
begin
  if not started_message then
     log_indent_message( string_message, true );
     started_message := true;
  end if;
  string_message := string_message & m;
end log_middle_part;


--  LOG LAST PART
--
-- Build the last part of the log message.  Indent (if needed) and show the
-- last of the user's message.
-----------------------------------------------------------------------------

procedure log_last_part( m : unbounded_string ) is
  log_file : file_type;                                        -- log file fd
  repeat_message : unbounded_string;                      -- entry about duptos
  sourceFile : unbounded_string;
begin
  if not started_message then
     log_indent_message( string_message, false );
     started_message := true;
  end if;
  string_message := string_message & m;
  if string_message = last_message then
     dup_count := dup_count + 1;
  else
     -- Open the log file
     -- The lock file prevents two processes from logging on the same line
     if log_mode /= stderr_log then
        lock_file( to_string( lock_file_path ) );
        begin
           open( log_file, append_file, to_string( log_path.value ) );
        exception when others =>
           if rshOpt then
              err( +"creating new logs is not allowed in a " & em( "restricted shell" ) );
              unlock_file( to_string( lock_file_path ) );
              return; -- must abort
           else
              begin
                 create( log_file, append_file, to_string( log_path.value ) );
              exception when others =>
                 err_exception_raised;
                 unlock_file( to_string( lock_file_path ) );
                 return; -- must abort
              end;
           end if;
        end;
     end if;

     -- Handle duplicate messages
     -- If there was one dup, just show it.
     -- if there were multiple dups, show the count
     if dup_count = 1 then
        if log_mode = file_log or log_mode = echo_log then
           put_line_retry( log_file, to_string( string_header & last_message ) );
        end if;
        if log_mode = stderr_log or log_mode = echo_log then
           writeCurrentError( string_header & last_message );
           --put_line( current_error, to_string( string_header & last_message ) );
        end if;
     elsif dup_count > 0 then
        repeat_message := trim( to_unbounded_string( aPID'image( getpid ) ), left ) & ":";
        repeat_message := repeat_message & entity & ":";
        sourceFile := basename( getSourceFileName );
        log_clean_message( sourceFile );
        repeat_message := repeat_message & "INFO:" & sourceFile & ":" & getLineNo & ":";
        log_indent_message( repeat_message, false );
        repeat_message := repeat_message &  "... repeated" & natural'image( dup_count ) & " times";
        if log_mode = file_log or log_mode = echo_log then
           put_line_retry( log_file, to_string( string_header & repeat_message ) );
        end if;
        if log_mode = stderr_log or log_mode = echo_log then
           writeCurrentError( string_header & repeat_message );
           --put_line( current_error, to_string( string_header & repeat_message ) );
        end if;
        dup_count := 0;
     end if;

     -- Log the message
     if log_mode = file_log or log_mode = echo_log then
        put_line_retry( log_file, to_string( string_header & string_message ) );
     end if;
     if log_mode = stderr_log or log_mode = echo_log then
        writeCurrentError( string_header & string_message );
        -- put_line( current_error, to_string( string_header & string_message ) );
     end if;
     last_message := string_message;

      -- Release the lock
     if log_mode /= stderr_log then
        close( log_file );
        unlock_file( to_string( lock_file_path ) );
     end if;
  end if;
  indent_required := 0;
  string_message := null_unbounded_string;
end log_last_part;


--  GET ENTITY
--
-- Get the enclosing entity message like in the source info package.
------------------------------------------------------------------------------

procedure get_entity( entity : out unbounded_string ) is
begin
  if not hasEntity then
     if blocks_top > block'First then
        entity := getBlockName( block'First );
     else
        entity := to_unbounded_string( "script" );
     end if;
     log_clean_message( entity ); -- to be safe
     hasEntity := true;
  end if;
end get_entity;


--  CLOSE LOG
--
-- Flush messages and write the closing message.
-----------------------------------------------------------------------------

procedure closeLog is
  sourceFile : unbounded_string;
  totalTime : duration;
begin
  sourceFile := basename( getSourceFileName );
  log_clean_message( sourceFile );
  level := 0;
  totalTime := clock - log_open_time;
  --log_first_part( sourceFile & ":" & getLineNo, "INFO" );
  log_first_part( "INFO" );
  log_last_part( "Time of " & entity & " logging" & totalTime'img & " seconds" );
  --log_first_part( sourceFile & ":" & getLineNo, "INFO" );
  log_first_part( "INFO" );
  log_last_part( "End " & entity & " logging" );
  log_mode := stderr_log;
  log_is_open := false;
  log_path := nullStorage;
end closeLog;


-- LOG META LABEL OK
--
-- Different files or combinations of files are written depending on
-- the logging mode.  The meta label checks depend on which logging mode is
-- used for the data in expr (if there is one).
-----------------------------------------------------------------------------

function logMetaLabelOk( subprogramId : identifier; msgExpr : storage ) return boolean is
   metaLabelCheckOk : boolean;
begin
   case log_mode is
   when stderr_log =>
      metaLabelCheckOk := metaLabelOk( subprogramId, identifiers( standard_error_t ).store.all, msgExpr );
   when file_log =>
      metaLabelCheckOk := metaLabelOk( subprogramId, log_path, msgExpr );
   when echo_log =>
      metaLabelCheckOk := metaLabelOk( subprogramId, identifiers( standard_error_t ).store.all, log_path, msgExpr );
   when others =>
      metaLabelCheckOk := false;
      err(
         contextNotes => pl( "At " & gnat.source_info.source_location &
             " while checking meta labels " ),
         subject => subprogramId,
         reason => +"had an internal error because",
         obstructorNotes => +"an unknown log mode"
      );
   end case;
  return metaLabelCheckOk;
end logMetaLabelOk;

function logMetaLabelOk( subprogramId : identifier ) return boolean is
   metaLabelCheckOk : boolean;
begin
   case log_mode is
   when stderr_log =>
      metaLabelCheckOk := metaLabelOk( subprogramId, identifiers( standard_error_t ).store.all );
   when file_log =>
      metaLabelCheckOk := metaLabelOk( subprogramId, log_path );
   when echo_log =>
      metaLabelCheckOk := metaLabelOk( subprogramId, identifiers( standard_error_t ).store.all, log_path );
   when others =>
      metaLabelCheckOk := false;
      err(
         contextNotes => pl( "At " & gnat.source_info.source_location &
             " while checking meta labels " ),
         subject => subprogramId,
         reason => +"had an internal error because",
         obstructorNotes => +"an unknown log mode"
      );
   end case;
  return metaLabelCheckOk;
end logMetaLabelOk;

-- RESOLVE LOG META LABEL
--
-- Since log messages can be sent to stderr, a file or both, when a meta
-- label needs to be returned, it depends on the logging destination meta
-- labels.
-----------------------------------------------------------------------------

function resolveLogMetaLabels return metaLabelHashedSet.Set is
   newMetaLabels : metaLabelhashedSet.Set;
begin
   case log_mode is
   when stderr_log =>
      newMetaLabels := identifiers( standard_error_t ).store.policyMetaLabels;
   when file_log =>
      newMetaLabels := log_path.policyMetaLabels;
   when echo_log =>
      newMetaLabels := resolveEffectiveMetaLabels( uni_string_t,
        identifiers( standard_error_t ).store.all,
        log_path );
   when others =>
       -- this should not happen
       newMetaLabels := sparMetaLabels;
   end case;
   return newMetaLabels;
end resolveLogMetaLabels;


------------------------------------------------------------------------------
-- Log package subprogram parsers
--
-- These implement this package's subprograms in SparForte.
------------------------------------------------------------------------------


procedure ParseLevelBegin is
  -- Syntax: level_begin( lvl );
  ref : reference;
begin
  --kind := log_level_t;
  expect( logs_level_begin_t );
  ParseSingleOutParameter( logs_level_begin_t, ref, log_level_t );
  if isExecutingCommand then
     if logMetaLabelOk( logs_level_begin_t ) then
        begin
           AssignParameter(ref, storage'( to_unbounded_string( numericValue( level ) ),
              noMetaLabel, resolveLogMetaLabels ) );
           level := level + 1;
        exception when constraint_error =>
           err( +"constraint_error raised" );
        when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseLevelBegin;

procedure ParseLevelEnd is
  -- Syntax: level_end( lvl );
  logLvlRef : reference;
  loglvl : storage;
begin
  expect( logs_level_end_t );
  ParseSingleInOutParameter( logs_level_end_t, logLvlRef, log_level_t );
  if isExecutingCommand then
     getParameterValue( loglvlRef, loglvl );
     if logMetaLabelOk( logs_level_end_t, loglvl ) then
        begin
           level := natural'value( ' ' & to_string( loglvl.value ) );
        exception when constraint_error =>
           err( +"constraint_error raised" );
        when others =>
           err_exception_raised;
        end;
     end if;
  end if;
end ParseLevelEnd;

procedure ParseOK is
  msgExpr    : storage;
  msgType    : identifier;
  cc         : chain_contexts := none;
begin
  expect( logs_ok_t );
<<repeat_ok>>
  ParseSingleStringParameter( logs_ok_t, msgExpr, msgType, universal_t );
  -- Here we fake a chain context.  We don't create the chain identifiers
  -- but instead read the script and determine the chain position, which
  -- is all we need.
  if token = symbol_t and identifiers( token ).store.value = "@" then
     if cc = none then
        cc := first;
     else
        cc := middle;
     end if;
  else
     if cc /= none then
        cc := last;
     end if;
  end if;
  if isExecutingCommand then
     begin
        get_entity( entity );
        log_clean_message( msgExpr.value );
        if cc = none then
           if logMetaLabelOk( logs_ok_t, msgExpr ) then
              log_first_part( "OK" );
              log_last_part( msgExpr.value );
              begin
                 currentMetrics.ok_count := currentMetrics.ok_count + 1;
              exception when others => null;
              end;
           end if;
        else
           case cc is
           when first =>
              -- only the first item in the chain needs to check the log_path meta tags
              if logMetaLabelOk( logs_ok_t, msgExpr ) then
                 log_first_part( "OK" );
                 log_middle_part( msgExpr.value );
              end if;
           when middle =>
              if logMetaLabelOk( logs_ok_t, msgExpr ) then
                 log_middle_part( msgExpr.value );
              end if;
           when last =>
              if logMetaLabelOk( logs_ok_t, msgExpr ) then
                 log_last_part( msgExpr.value );
                 begin
                    currentMetrics.ok_count := currentMetrics.ok_count + 1;
                 exception when others => null;
                 end;
              end if;
           when others =>
              err( +"internal error: unexpected chain context" );
           end case;
        end if;
     exception when others =>
        err_exception_raised;
     end;
  end if;
  if token = symbol_t and identifiers( token ).store.value = "@" then
     expect( symbol_t );
     goto repeat_ok;
  end if;
end ParseOK;

procedure ParseInfo is
  msgExpr    : storage;
  msgType    : identifier;
  cc         : chain_contexts := none;
begin
  expect( logs_info_t );
<<repeat_info>>
  ParseSingleStringParameter( logs_info_t, msgExpr, msgType, universal_t );
  -- Here we fake a chain context.  We don't create the chain identifiers
  -- but instead read the script and determine the chain position, which
  -- is all we need.
  if token = symbol_t and identifiers( token ).store.value = "@" then
     if cc = none then
        cc := first;
     else
        cc := middle;
     end if;
  else
     if cc /= none then
        cc := last;
     end if;
  end if;
  if isExecutingCommand then
     begin
        get_entity( entity );
        log_clean_message( msgExpr.value );
        if cc = none then
           if logMetaLabelOk( logs_info_t, msgExpr ) then
              log_first_part( "INFO" );
              log_last_part( msgExpr.value );
              begin
                 currentMetrics.info_count := currentMetrics.info_count + 1;
              exception when others => null;
              end;
           end if;
        else
           case cc is
           when first =>
              -- only the first item in the chain needs to check the log_path meta tags
              if logMetaLabelOk( logs_info_t, msgExpr ) then
                 log_first_part( "INFO" );
                 log_middle_part( msgExpr.value );
              end if;
           when middle =>
              if logMetaLabelOk( logs_info_t, msgExpr ) then
                 log_middle_part( msgExpr.value );
              end if;
           when last =>
              if logMetaLabelOk( logs_info_t, msgExpr ) then
                 log_last_part( msgExpr.value );
                 begin
                    currentMetrics.info_count := currentMetrics.info_count + 1;
                 exception when others => null;
                 end;
              end if;
           when others =>
              err( +"internal error: unexpected chain context" );
           end case;
        end if;
     exception when others =>
        err_exception_raised;
     end;
  end if;
  if token = symbol_t and identifiers( token ).store.value = "@" then
     expect( symbol_t );
     goto repeat_info;
  end if;
end ParseInfo;

procedure ParseWarning is
  msgExpr    : storage;
  msgType    : identifier;
  cc         : chain_contexts := none;
begin
  expect( logs_warning_t );
<<repeat_warning>>
  ParseSingleStringParameter( logs_warning_t, msgExpr, msgType, universal_t );
  -- Here we fake a chain context.  We don't create the chain identifiers
  -- but instead read the script and determine the chain position, which
  -- is all we need.
  if token = symbol_t and identifiers( token ).store.value = "@" then
     if cc = none then
        cc := first;
     else
        cc := middle;
     end if;
  else
     if cc /= none then
        cc := last;
     end if;
  end if;
  if isExecutingCommand then
     begin
        get_entity( entity );
        log_clean_message( msgExpr.value );
        if cc = none then
           if logMetaLabelOk( logs_warning_t, msgExpr ) then
              log_first_part( "WARNING" );
              log_last_part( msgExpr.value );
              begin
                 currentMetrics.warning_count := currentMetrics.warning_count + 1;
              exception when others => null;
              end;
           end if;
        else
           case cc is
           when first =>
              -- only the first item in the chain needs to check the log_path meta tags
              if logMetaLabelOk( logs_warning_t, msgExpr ) then
                 log_first_part( "WARNING" );
                 log_middle_part( msgExpr.value );
              end if;
           when middle =>
              if logMetaLabelOk( logs_warning_t, msgExpr ) then
                 log_middle_part( msgExpr.value );
              end if;
           when last =>
              if logMetaLabelOk( logs_warning_t, msgExpr ) then
                 log_last_part( msgExpr.value );
                 begin
                    currentMetrics.warning_count := currentMetrics.warning_count + 1;
                 exception when others => null;
                 end;
              end if;
           when others =>
              err( +"internal error: unexpected chain context" );
           end case;
        end if;
     exception when others =>
        err_exception_raised;
     end;
  end if;
  if token = symbol_t and identifiers( token ).store.value = "@" then
     expect( symbol_t );
     goto repeat_warning;
  end if;
end ParseWarning;

procedure ParseError is
  msgExpr    : storage;
  msgType    : identifier;
  cc         : chain_contexts := none;
begin
  expect( logs_error_t );
<<repeat_error>>
  ParseSingleStringParameter( logs_error_t, msgExpr, msgType, universal_t );
  -- Here we fake a chain context.  We don't create the chain identifiers
  -- but instead read the script and determine the chain position, which
  -- is all we need.
  if token = symbol_t and identifiers( token ).store.value = "@" then
     if cc = none then
        cc := first;
     else
        cc := middle;
     end if;
  else
     if cc /= none then
        cc := last;
     end if;
  end if;
  if isExecutingCommand then
     begin
        get_entity( entity );
        log_clean_message( msgExpr.value );
        if cc = none then
           if logMetaLabelOk( logs_error_t, msgExpr ) then
              log_first_part( "ERROR" );
              log_last_part( msgExpr.value );
              begin
                 currentMetrics.error_count := currentMetrics.error_count + 1;
              exception when others => null;
              end;
           end if;
        else
           case cc is
           when first =>
              -- only the first item in the chain needs to check the log_path meta tags
              if logMetaLabelOk( logs_error_t, msgExpr ) then
                 log_first_part( "ERROR" );
                 log_middle_part( msgExpr.value );
              end if;
           when middle =>
              if logMetaLabelOk( logs_error_t, msgExpr ) then
                 log_middle_part( msgExpr.value );
              end if;
           when last =>
              if logMetaLabelOk( logs_error_t, msgExpr ) then
                 log_last_part( msgExpr.value );
                 begin
                    currentMetrics.error_count := currentMetrics.error_count + 1;
                 exception when others => null;
                 end;
              end if;
           when others =>
              err( +"internal error: unexpected chain context" );
           end case;
        end if;
     exception when others =>
        err_exception_raised;
     end;
  end if;
  if token = symbol_t and identifiers( token ).store.value = "@" then
     expect( symbol_t );
     goto repeat_error;
  end if;
end ParseError;

procedure ParseOpen is
  -- Syntax: logs.open( "program", "path", mode [,width] );
  -- This does not actually open the file.  It sets the paramters for the
  -- log file, which will be open and closed when required.
  -- In a restricted shell, you cannot create logs.
  pathExpr : storage;
  pathType : identifier;
  modeExpr : storage;
  modeType : identifier;
  widthExpr: storage;
  widthType: identifier;
  sourceFile : unbounded_string;
begin
  expect( logs_open_t );
  ParseFirstStringParameter( logs_open_t, pathExpr, pathType, string_t );
  ParseNextEnumParameter( logs_open_t, modeExpr, modeType, log_modes_t );
  if token = symbol_t and identifiers( token ).store.value = ")" then
     widthExpr := storage'( to_unbounded_string( defaultWidth'img ), noMetaLabel, noMetaLabels );
     expect( symbol_t, ")" );
  else
     ParseLastNumericParameter( logs_open_t, widthExpr, widthType, positive_t );
  end if;

  if isExecutingCommand then
     if log_is_open then
        err( +"log is already open" );
     -- Except for stderr, we need a file path
     elsif modeExpr.value /= "0" and length( pathExpr.value ) = 0 then
        err( +"log path is an empty string" );
     elsif modeExpr.value = "0" and length( pathExpr.value ) > 0 then
        err( +"log path should be an empty string" );
     elsif Is_Directory( to_string( pathExpr.value ) & ASCII.NUL ) then
        err( +"log path is a directory" );
     else
        if metaLabelOk( logs_open_t, pathExpr ) then
           get_entity( entity );
           sourceFile := basename( getSourceFileName );
           log_clean_message( sourceFile );
           level := 0;
           width := positive( to_numeric( widthExpr.value ) );
           log_path := pathExpr;
           lock_file_path := log_path.value & ".lck";
           log_mode := log_modes'val( integer( to_numeric( modeExpr.value ) ) );
           log_is_open := true;
           log_open_time := clock;
           --log_first_part( sourceFile & ":" & getLineNo, "INFO" );
           log_first_part( "INFO" );
           log_last_part( "Start " & entity & " logging" );
        end if;
     end if;
  end if;
end ParseOpen;

procedure ParseClose is
-- Syntax: logs.close
begin
  expect( logs_close_t );
  if isExecutingCommand then
     if not log_is_open then
        err( +"log is already closed" );
     elsif metaLabelOk( logs_close_t, log_path ) then
        closeLog;
     end if;
  end if;
  resetLog;
end ParseClose;

procedure ParseIsOpen( result : out storage; kind : out identifier ) is
  -- Syntax: b := logs.is_open
begin
  result := nullStorage;
  kind := boolean_t;
  expect( logs_is_open_t );
  if isExecutingCommand then
     if metaLabelOk( logs_is_open_t, log_path ) then
        result := storage'( to_spar_boolean( log_is_open ), noMetaLabel, log_path.policyMetaLabels );
     end if;
  end if;
end ParseIsOpen;

procedure ParseMode( result : out storage; kind : out identifier ) is
  -- Syntax: m := logs.mode
begin
  result := nullStorage;
  if isExecutingCommand then
     if logMetaLabelOk( logs_mode_t ) then
        result := storage'(
           trim( to_unbounded_string( natural'image( log_modes'pos( log_mode ) ) ), left ),
           noMetaLabel,
           log_path.policyMetaLabels
        );
     end if;
  end if;
  kind := log_modes_t;
  expect( logs_mode_t );
end ParseMode;

-- Note: disabled because logs.level is limited so can't be returned in an
-- expression
--procedure ParseLevel( result : out storage; kind : out identifier ) is
--  -- Syntax: l := logs.level
--begin
--  result :=to_unbounded_string( natural'image( level ) );
--  kind := natural_t;
--  expect( logs_level_t );
--end ParseLevel;

procedure ParseWidth( result : out storage; kind : out identifier ) is
  -- Syntax: w := logs.width
begin
  if isExecutingCommand then
     if logMetaLabelOk( logs_width_t ) then
        result := storage'( to_unbounded_string( positive'image( width ) ),
           noMetaLabel, resolveLogMetaLabels );
     end if;
  end if;
  kind := positive_t;
  expect( logs_width_t );
end ParseWidth;

procedure ParseRotateBegin is
  -- Syntax: logs.rotate_begin;
begin
  expect( logs_rotate_begin_t );
  if isExecutingCommand then
     if logMetaLabelOk( logs_rotate_begin_t ) then
        if log_is_rotating then
           err( +"logs are already rotating" );
        elsif lock_file_path /= "" and log_mode /= stderr_log then
           -- We don't need a lock file when logging to standard error only
           lock_file( to_string( lock_file_path ) );
        end if;
        log_is_rotating := true;
     end if;
  end if;
end ParseRotateBegin;

procedure ParseRotateEnd is
  -- Syntax: logs.rotate_end;
begin
  expect( logs_rotate_end_t );
  if isExecutingCommand then
     if logMetaLabelOk( logs_rotate_end_t ) then
        if not log_is_rotating then
           err( +"logs are not rotating" );
        elsif lock_file_path /= "" and log_mode /= stderr_log then
           -- We don't need a lock file when logging to standard error only
           unlock_file( to_string( lock_file_path ) );
        end if;
        log_is_rotating := false;
     end if;
  end if;
end ParseRotateEnd;

procedure ParseIsRotating( result : out storage; kind : out identifier ) is
  -- Syntax: b := logs.is_rotating
begin
  result := nullStorage;
  kind := boolean_t;
  expect( logs_is_rotating_t );
  if isExecutingCommand then
     if logMetaLabelOk( logs_is_rotating_t ) then
        result := storage'( to_spar_boolean( log_is_rotating ),
           noMetaLabel, resolveLogMetaLabels );
     end if;
  end if;
end ParseIsRotating;

procedure ParseMetrics is
  -- Syntax: logs.metrics
  ok_ref      : reference;
  info_ref    : reference;
  warning_ref : reference;
  error_ref   : reference;
begin
  expect( logs_metrics_t );
  ParseFirstOutParameter( logs_metrics_t, ok_ref, natural_t );
  ParseNextOutParameter(  logs_metrics_t, info_ref, natural_t );
  ParseNextOutParameter(  logs_metrics_t, warning_ref, natural_t );
  ParseLastOutParameter(  logs_metrics_t, error_ref, natural_t );
  if isExecutingCommand then
     if logMetaLabelOk( logs_metrics_t ) then
        assignParameter( ok_ref,
           storage'( to_unbounded_string( numericValue( checkpointMetrics.ok_count ) ), noMetaLabel, resolveLogMetaLabels ) );
        assignParameter( info_ref,
           storage'( to_unbounded_string( numericValue( checkpointMetrics.info_count ) ), noMetaLabel, resolveLogMetaLabels ) );
        assignParameter( warning_ref,
           storage'( to_unbounded_string( numericValue( checkpointMetrics.warning_count ) ), noMetaLabel, resolveLogMetaLabels ) );
        assignParameter( error_ref,
            storage'( to_unbounded_string( numericValue( checkpointMetrics.error_count ) ), noMetaLabel, resolveLogMetaLabels ) );
     end if;
  end if;
end ParseMetrics;

procedure ParseCheckpoint is
  -- Syntax: logs.clear_metrics
begin
  expect( logs_checkpoint_t );
  if isExecutingCommand then
     -- Check to see that the file is open
     if logMetaLabelOk( logs_checkpoint_t ) then
        resetMetrics;
     end if;
  end if;
end ParseCheckpoint;

-----------------------------------------------------------------------------

procedure StartupLogs is
begin
  declareNamespace( "logs" );

  declareIdent( log_level_t, "logs.log_level", natural_t, typeClass );
  identifiers( log_level_t ).usage := limitedUsage;

  declareProcedure( logs_level_begin_t, "logs.level_begin",  ParseLevelBegin'access );
  declareProcedure( logs_level_end_t,   "logs.level_end",    ParseLevelEnd'access );

  declareProcedure( logs_ok_t,          "logs.ok",           ParseOK'access );
  declareProcedure( logs_info_t,        "logs.info",         ParseInfo'access );
  declareProcedure( logs_warning_t,     "logs.warning",      ParseWarning'access );
  declareProcedure( logs_error_t,       "logs.error",        ParseError'access );

  declareProcedure( logs_open_t,        "logs.open",         ParseOpen'access );
  declareProcedure( logs_close_t,       "logs.close",        ParseClose'access );

  declareFunction(  logs_is_open_t,     "logs.is_open",      ParseIsOpen'access );
  declareFunction(  logs_mode_t,        "logs.mode",         ParseMode'access );
  --declareFunction(  logs_level_t,       "logs.level",        ParseLevel'access );
  declareFunction(  logs_width_t,       "logs.width",        ParseWidth'access );

  declareProcedure( logs_rotate_begin_t, "logs.rotate_begin", ParseRotateBegin'access );
  declareProcedure( logs_rotate_end_t,   "logs.rotate_end",   ParseRotateEnd'access );
  declareFunction(  logs_is_rotating_t, "logs.is_rotating",  ParseIsRotating'access );

  declareProcedure( logs_metrics_t,   "logs.metrics",   ParseMetrics'access );
  declareProcedure( logs_checkpoint_t,   "logs.checkpoint",   ParseCheckpoint'access );

  declareIdent( log_modes_t, "logs.log_modes", root_enumerated_t, typeClass );

  declareNamespaceClosed( "logs" );

  declareNamespace( "log_mode" );

  declareStandardEnum( log_mode_stderr_t, "log_mode.stderr", log_modes_t, "0" );
  declareStandardEnum( log_mode_file_t,   "log_mode.file",   log_modes_t, "1" );
  declareStandardEnum( log_mode_echo_t,   "log_mode.echo",   log_modes_t, "2" );

  declareNamespaceClosed( "log_mode" );
  resetLog;
end StartupLogs;

procedure ShutdownLogs is
begin
  if log_is_open then
     closeLog;
  end if;
end ShutdownLogs;

end parser_logs;

