------------------------------------------------------------------------------
-- Logs Package Parser                                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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
    gnat.os_lib,
    interfaces.c,
    ada.exceptions,
    ada.text_io,
    ada.strings.unbounded,
    ada.calendar,
    gnat.lock_files,
    spar_os,
    pegasoft.user_io,
    world,
    pegasoft.strings,
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
    world,
    pegasoft.strings,
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

log_path        : unbounded_string;                        -- path to log file
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

procedure writeCurrentError( expr_val : unbounded_string ) is
  ref       : reference;
  result    : size_t;
  ch        : character;
  fd        : aFileDescriptor;
  retry     : boolean;
begin
  -- This is an alias to the variable with the file
  ref.id := identifier( to_numeric( identifiers(current_error_t).value.all ) );

  -- Check to see that the file is open
  if length( identifiers( ref.id ).value.all ) = 0 then
     err( +"log file is not open" );
     return;
  end if;

  -- If current_error is standard-error, use Ada's Text_IO

  if ref.id = standard_error_t then
     -- Ada doesn't handle interrupted system calls properly.
     -- maybe a more elegant way to do this...
     loop
        retry := false;
        begin
          Put_Line( standard_error, to_string( expr_val ) );
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
     fd := aFileDescriptor'value( to_string( stringField( ref, fd_field ) ) );
     for i in 1..length( expr_val ) loop
         ch := Element( expr_val, i );
<<logwrite>> writechar( result, fd, ch, 1 );
         if result < 0 then
            if C_errno = EAGAIN or C_errno = EINTR then
               goto logwrite;
            end if;
            err( pl( "unable to write: " & OSerror( C_errno ) ) );
            exit;
         end if;
     end loop;
     ch := ASCII.LF;
<<logwrite2>>
     writechar( result, fd, ch, 1 ); -- add a line feed
     if result < 0 then
        if C_errno = EAGAIN or C_errno = EINTR then
            goto logwrite2;
        end if;
        err( pl( "unable to write: " & OSerror( C_errno ) ) );
      else
        replaceField( ref, line_field,
           long_integer'image( long_integer'value(
              to_string( stringField( ref, line_field ) ) ) + 1 ) );
     end if;
  end if;
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
  log_path := null_unbounded_string;
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
  message := toEscaped( message );
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
           open( log_file, append_file, to_string( log_path ) );
        exception when others =>
           if rshOpt then
              err( +"creating new logs is not allowed in a " & em( "restricted shell" ) );
              unlock_file( to_string( lock_file_path ) );
              return; -- must abort
           else
              begin
                 create( log_file, append_file, to_string( log_path ) );
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
           put_line( log_file, to_string( string_header & last_message ) );
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
           put_line( log_file, to_string( string_header & repeat_message ) );
        end if;
        if log_mode = stderr_log or log_mode = echo_log then
           writeCurrentError( string_header & repeat_message );
           --put_line( current_error, to_string( string_header & repeat_message ) );
        end if;
        dup_count := 0;
     end if;

     -- Log the message
     if log_mode = file_log or log_mode = echo_log then
        put_line( log_file, to_string( string_header & string_message ) );
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
end closeLog;


------------------------------------------------------------------------------
-- Log package subprogram parsers
--
-- These implement this package's subprograms in SparForte.
------------------------------------------------------------------------------


procedure ParseLevelBegin is
  -- Syntax: level_being( lvl );
  ref : reference;
begin
  --kind := log_level_t;
  expect( logs_level_begin_t );
  ParseSingleOutParameter( logs_level_begin_t, ref, log_level_t );
  if isExecutingCommand then
     begin
        AssignParameter(ref, to_unbounded_string( long_float( level ) ) );
        level := level + 1;
     exception when constraint_error =>
        err( +"constraint_error raised" );
     when others =>
        err_exception_raised;
     end;
  end if;
end ParseLevelBegin;

procedure ParseLevelEnd is
  -- Syntax: level_end( lvl );
  id : identifier;
begin
  expect( logs_level_end_t );
  ParseSingleInOutParameter( logs_level_end_t, id, log_level_t );
  if isExecutingCommand then
     begin
        level := natural'value( ' ' & to_string( identifiers( id ).value.all ) );
     exception when constraint_error =>
        err( +"constraint_error raised" );
     when others =>
        err_exception_raised;
     end;
  end if;
end ParseLevelEnd;

procedure ParseOK is
  msgExpr    : unbounded_string;
  msgType    : identifier;
  cc         : chain_contexts := none;
begin
  expect( logs_ok_t );
<<repeat_ok>>
  ParseSingleStringParameter( logs_ok_t, msgExpr, msgType, universal_t );
  -- Here we fake a chain context.  We don't create the chain identifiers
  -- but instead read the script and determine the chain position, which
  -- is all we need.
  if token = symbol_t and identifiers( token ).value.all = "@" then
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
        log_clean_message( msgExpr );
        if cc = none then
           --log_first_part( basename( getSourceFileName ) & ":" & getLineNo, "OK" );
           log_first_part( "OK" );
           log_last_part( msgExpr );
           begin
              currentMetrics.ok_count := currentMetrics.ok_count + 1;
           exception when others => null;
           end;
        else
           case cc is
           when first =>
              log_first_part( "OK" );
              log_middle_part( msgExpr );
           when middle =>
              log_middle_part( msgExpr );
           when last =>
              log_last_part( msgExpr );
              begin
                 currentMetrics.ok_count := currentMetrics.ok_count + 1;
              exception when others => null;
              end;
           when others =>
              err( +"internal error: unexpected chain context" );
           end case;
        end if;
     exception when others =>
        err_exception_raised;
     end;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "@" then
     expect( symbol_t );
     goto repeat_ok;
  end if;
end ParseOK;

procedure ParseInfo is
  msgExpr    : unbounded_string;
  msgType    : identifier;
  cc         : chain_contexts := none;
begin
  expect( logs_info_t );
<<repeat_info>>
  ParseSingleStringParameter( logs_info_t, msgExpr, msgType, universal_t );
  -- Here we fake a chain context.  We don't create the chain identifiers
  -- but instead read the script and determine the chain position, which
  -- is all we need.
  if token = symbol_t and identifiers( token ).value.all = "@" then
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
        log_clean_message( msgExpr );
        if cc = none then
           --log_first_part( basename( getSourceFileName ) & ":" & getLineNo, "INFO" );
           log_first_part( "INFO" );
           log_last_part( msgExpr );
           begin
              currentMetrics.info_count := currentMetrics.info_count + 1;
           exception when others => null;
           end;
        else
           case cc is
           when first =>
              -- log_first_part( msgExpr, "INFO" );
              log_first_part( "INFO" );
              log_middle_part( msgExpr );
           when middle =>
              log_middle_part( msgExpr );
           when last =>
              log_last_part( msgExpr );
              begin
                 currentMetrics.info_count := currentMetrics.info_count + 1;
              exception when others => null;
              end;
           when others =>
              err( +"internal error: unexpected chain context" );
           end case;
        end if;
     exception when others =>
        err_exception_raised;
     end;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "@" then
     expect( symbol_t );
     goto repeat_info;
  end if;
end ParseInfo;

procedure ParseWarning is
  msgExpr    : unbounded_string;
  msgType    : identifier;
  cc         : chain_contexts := none;
begin
  expect( logs_warning_t );
<<repeat_warning>>
  ParseSingleStringParameter( logs_warning_t, msgExpr, msgType, universal_t );
  -- Here we fake a chain context.  We don't create the chain identifiers
  -- but instead read the script and determine the chain position, which
  -- is all we need.
  if token = symbol_t and identifiers( token ).value.all = "@" then
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
        log_clean_message( msgExpr );
        if cc = none then
           -- log_first_part( basename( getSourceFileName ) & ":" & getLineNo, "WARNING" );
           log_first_part( "WARNING" );
           log_last_part( msgExpr );
           begin
              currentMetrics.warning_count := currentMetrics.warning_count + 1;
           exception when others => null;
           end;
        else
           case cc is
           when first =>
              -- log_first_part( msgExpr, "WARNING" );
              log_first_part( "WARNING" );
              log_middle_part( msgExpr );
           when middle =>
              log_middle_part( msgExpr );
           when last =>
              log_last_part( msgExpr );
              begin
                 currentMetrics.warning_count := currentMetrics.warning_count + 1;
              exception when others => null;
              end;
           when others =>
              err( +"internal error: unexpected chain context" );
           end case;
        end if;
     exception when others =>
        err_exception_raised;
     end;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "@" then
     expect( symbol_t );
     goto repeat_warning;
  end if;
end ParseWarning;

procedure ParseError is
  msgExpr    : unbounded_string;
  msgType    : identifier;
  cc         : chain_contexts := none;
begin
  expect( logs_error_t );
<<repeat_error>>
  ParseSingleStringParameter( logs_error_t, msgExpr, msgType, universal_t );
  -- Here we fake a chain context.  We don't create the chain identifiers
  -- but instead read the script and determine the chain position, which
  -- is all we need.
  if token = symbol_t and identifiers( token ).value.all = "@" then
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
        log_clean_message( msgExpr );
        if cc = none then
           --log_first_part( basename( getSourceFileName ) & ":" & getLineNo, "ERROR" );
           log_first_part( "ERROR" );
           log_last_part( msgExpr );
           begin
              currentMetrics.error_count := currentMetrics.error_count + 1;
           exception when others => null;
           end;
        else
           case cc is
           when first =>
              --log_first_part( msgExpr, "ERROR" );
              log_first_part( "ERROR" );
              log_middle_part( msgExpr );
           when middle =>
              log_middle_part( msgExpr );
           when last =>
              log_last_part( msgExpr );
              begin
                 currentMetrics.error_count := currentMetrics.error_count + 1;
              exception when others => null;
              end;
           when others =>
              err( +"internal error: unexpected chain context" );
           end case;
        end if;
     exception when others =>
        err_exception_raised;
     end;
  end if;
  if token = symbol_t and identifiers( token ).value.all = "@" then
     expect( symbol_t );
     goto repeat_error;
  end if;
end ParseError;

procedure ParseOpen is
  -- Syntax: logs.open( "program", "path", mode [,width] );
  -- This does not actually open the file.  It sets the paramters for the
  -- log file, which will be open and closed when required.
  -- In a restricted shell, you cannot create logs.
  pathExpr : unbounded_string;
  pathType : identifier;
  modeExpr : unbounded_string;
  modeType : identifier;
  widthExpr: unbounded_string;
  widthType: identifier;
  sourceFile : unbounded_string;
begin
  expect( logs_open_t );
  ParseFirstStringParameter( logs_open_t, pathExpr, pathType, string_t );
  ParseNextEnumParameter( logs_open_t, modeExpr, modeType, log_modes_t );
  if token = symbol_t and identifiers( token ).value.all = ")" then
     widthExpr:= to_unbounded_string( defaultWidth'img );
     expect( symbol_t, ")" );
  else
     ParseLastNumericParameter( logs_open_t, widthExpr, widthType, positive_t );
  end if;

  if isExecutingCommand then
     if log_is_open then
        err( +"log is already open" );
     -- Except for stderr, we need a file path
     elsif modeExpr /= "0" and length( pathExpr ) = 0 then
        err( +"log path is an empty string" );
     elsif modeExpr = "0" and length( pathExpr ) > 0 then
        err( +"log path should be an empty string" );
     elsif Is_Directory( to_string( pathExpr ) & ASCII.NUL ) then
        err( +"log path is a directory" );
     else
        get_entity( entity );
        sourceFile := basename( getSourceFileName );
        log_clean_message( sourceFile );
        level := 0;
        width := positive( to_numeric( widthExpr ) );
        log_path := pathExpr;
        lock_file_path := log_path & ".lck";
        log_mode := log_modes'val( integer( to_numeric( modeExpr ) ) );
        log_is_open := true;
        log_open_time := clock;
        --log_first_part( sourceFile & ":" & getLineNo, "INFO" );
        log_first_part( "INFO" );
        log_last_part( "Start " & entity & " logging" );
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
     else
        closeLog;
     end if;
  end if;
  resetLog;
end ParseClose;

procedure ParseIsOpen( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := logs.is_open
begin
  result := null_unbounded_string;
  kind := boolean_t;
  expect( logs_is_open_t );
  if isExecutingCommand then
     result := to_bush_boolean( log_is_open );
  end if;
end ParseIsOpen;

procedure ParseMode( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: m := logs.mode
begin
  result := trim( to_unbounded_string( natural'image( log_modes'pos( log_mode ) ) ), left );
  kind := log_modes_t;
  expect( logs_mode_t );
end ParseMode;

-- Note: disabled because logs.level is limited so can't be returned in an
-- expression
--procedure ParseLevel( result : out unbounded_string; kind : out identifier ) is
--  -- Syntax: l := logs.level
--begin
--  result :=to_unbounded_string( natural'image( level ) );
--  kind := natural_t;
--  expect( logs_level_t );
--end ParseLevel;

procedure ParseWidth( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: w := logs.width
begin
  result :=to_unbounded_string( positive'image( width ) );
  kind := positive_t;
  expect( logs_width_t );
end ParseWidth;

procedure ParseRotateBegin is
  -- Syntax: logs.rotate_begin;
begin
  expect( logs_rotate_begin_t );
  if isExecutingCommand then
     if log_is_rotating then
        err( +"logs are already rotating" );
     elsif lock_file_path /= "" and log_mode /= stderr_log then
        -- We don't need a lock file when logging to standard error only
        lock_file( to_string( lock_file_path ) );
     end if;
     log_is_rotating := true;
  end if;
end ParseRotateBegin;

procedure ParseRotateEnd is
  -- Syntax: logs.rotate_end;
begin
  expect( logs_rotate_end_t );
  if isExecutingCommand then
     if not log_is_rotating then
        err( +"logs are not rotating" );
     elsif lock_file_path /= "" and log_mode /= stderr_log then
        -- We don't need a lock file when logging to standard error only
        unlock_file( to_string( lock_file_path ) );
     end if;
     log_is_rotating := false;
  end if;
end ParseRotateEnd;

procedure ParseIsRotating( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := logs.is_rotating
begin
  result := null_unbounded_string;
  kind := boolean_t;
  expect( logs_is_rotating_t );
  if isExecutingCommand then
     result := to_bush_boolean( log_is_rotating );
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
     assignParameter( ok_ref,
        to_unbounded_string( long_float( checkpointMetrics.ok_count ) ) );
     assignParameter( info_ref,
        to_unbounded_string( long_float( checkpointMetrics.info_count ) ) );
     assignParameter( warning_ref,
        to_unbounded_string( long_float( checkpointMetrics.warning_count ) ) );
     assignParameter( error_ref,
        to_unbounded_string( long_float( checkpointMetrics.error_count ) ) );
  end if;
end ParseMetrics;

procedure ParseCheckpoint is
  -- Syntax: logs.clear_metrics
begin
  expect( logs_checkpoint_t );
  if isExecutingCommand then
     resetMetrics;
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

