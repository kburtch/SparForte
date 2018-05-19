------------------------------------------------------------------------------
-- Logs Package Parser                                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2018 Free Software Foundation              --
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
    ada.text_io,
    ada.strings.unbounded,
    ada.calendar,
    gnat.lock_files,
    spar_os,
    world,
    string_util,
    compiler,
    scanner,
    chain_util,
    parser_params;
use
    ada.text_io,
    ada.strings,
    ada.strings.unbounded,
    ada.calendar,
    gnat.lock_files,
    spar_os,
    world,
    string_util,
    compiler,
    scanner,
    chain_util,
    parser_params;

package body parser_logs is

type log_modes is ( stderr_log, file_log, echo_log );

program_name    : unbounded_string;
log_path        : unbounded_string;
lock_file_path  : unbounded_string;                   -- path to lock file
level           : natural := 0;
width           : positive := 75;
log_mode        : log_modes;
log_is_open     : boolean := false;
log_is_rotating : boolean := false;

string_header   : unbounded_string;                 -- leading part of entry
string_message  : unbounded_string;                        -- body of entry
indent_required : natural;                   -- true if indent not applied
started_message : boolean := true;                    -- true if new entry
last_message    : unbounded_string := null_unbounded_string;-- last entry body for dup check
dup_count       : natural := 0;                         -- number of dup entries


------------------------------------------------------------------------------
-- Logs package identifiers
------------------------------------------------------------------------------

log_level_t      : identifier;

log_modes_t         : identifier;
log_mode_stderr_t   : identifier;
log_mode_file_t     : identifier;
log_mode_echo_t     : identifier;

logs_level_begin_t : identifier;
logs_level_end_t   : identifier;

logs_ok_t          : identifier;
logs_info_t        : identifier;
logs_warning_t     : identifier;
logs_error_t       : identifier;
logs_open_t        : identifier;
logs_close_t       : identifier;

logs_is_open_t     : identifier;
logs_mode_t        : identifier;
logs_level_t       : identifier;
logs_width_t       : identifier;

logs_rotate_begin_t : identifier;
logs_rotate_end_t   : identifier;
logs_is_rotating_t  : identifier;

------------------------------------------------------------------------------
-- Utility subprograms
------------------------------------------------------------------------------


--  GET ENTITY
--
-- Get the enclosing entity message likes the source info package.
-----------------------------------------------------------------------------

procedure get_entity( entity : out unbounded_string ) is
begin
   if blocks_top > block'First then
      entity := getBlockName( block'First );
   else
      entity := to_unbounded_string( "script" );
   end if;
end get_entity;


--  LOG INDENT MESSAGE
--
-- Add the indent field to a log message.
-----------------------------------------------------------------------------

procedure log_indent_message( msg : in out unbounded_string; clear_indent : boolean ) is
begin
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
-----------------------------------------------------------------------------


--  LOG FIRST PART
--
-- Build the first part of the log message: date, program and location.
-----------------------------------------------------------------------------

procedure log_first_part( m : unbounded_string; level_tag : string ) is
  entity : unbounded_string;
begin
  get_entity( entity );
  string_header := getDateString( clock ) & ":";
  string_message := trim( to_unbounded_string( aPID'image( getpid ) ), left ) & ":";
  string_message := string_message & entity & ":";
  string_message := string_message & level_tag & ":";
  string_message := string_message & m & ":";
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
  entity : unbounded_string;
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
     if lock_file_path /= "" and log_mode /= stderr_log then
        lock_file( to_string( lock_file_path ) );
     end if;
     create( log_file, append_file, to_string( log_path ) );

     -- Handle duplicate messages
     -- If there was one dup, just show it.
     -- if there were multiple dups, show the count
     if dup_count = 1 then
        if log_mode = file_log or log_mode = echo_log then
           put_line( log_file, to_string( string_header & last_message ) );
        end if;
        if log_mode = stderr_log or log_mode = echo_log then
           put_line( standard_error, to_string( string_header & last_message ) );
        end if;
     elsif dup_count > 0 then
        repeat_message := trim( to_unbounded_string( aPID'image( getpid ) ), left ) & ":";
        -- FIXME:
        --repeat_message := repeat_message & source_info.enclosing_entity & ":";
        get_entity( entity );
        repeat_message := repeat_message & entity & ":";
        -- FIXME:
        --repeat_message := repeat_message & "INFO:" & source_info.file & ": 0:";
        repeat_message := repeat_message & "INFO:" & basename( getSourceFileName ) & ": 0:";
        log_indent_message( repeat_message, false );
        repeat_message := repeat_message &  "... repeated" & natural'image( dup_count ) & " times";
        if log_mode = file_log or log_mode = echo_log then
           put_line( log_file, to_string( string_header & repeat_message ) );
        end if;
        if log_mode = stderr_log or log_mode = echo_log then
           put_line( standard_error, to_string( string_header & repeat_message ) );
        end if;
        dup_count := 0;
     end if;

     -- Log the message
     if log_mode = file_log or log_mode = echo_log then
        put_line( log_file, to_string( string_header & string_message ) );
     end if;
     if log_mode = stderr_log or log_mode = echo_log then
        put_line( standard_error, to_string( string_header & string_message ) );
     end if;
     last_message := string_message;

      -- Release the lock
     close( log_file );
     if lock_file_path /= "" and log_mode /= stderr_log then
        unlock_file( to_string( lock_file_path ) );
     end if;
  end if;
  indent_required := 0;
  string_message := null_unbounded_string;
end log_last_part;


------------------------------------------------------------------------------
-- Parser subprograms
------------------------------------------------------------------------------


procedure ParseLevelBegin( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: lvl := level_being;
begin
  kind := log_level_t;
  expect( logs_level_begin_t );
  if isExecutingCommand then
     begin
        result := to_unbounded_string( long_float( level ) );
        level := level + 1;
     exception when constraint_error =>
        err( "constraint_error raised" );
     when others =>
        err_exception_raised;
     end;
  end if;
end ParseLevelBegin;

procedure ParseLevelEnd is
  -- Syntax: level_end( lvl );
  levelExpr    : unbounded_string;
  levelType    : identifier;
begin
  expect( logs_level_end_t );
  ParseSingleNumericParameter( levelExpr, levelType, log_level_t );
  if isExecutingCommand then
     begin
        level := natural'value( ' ' & to_string( levelExpr ) );
     exception when constraint_error =>
        err( "constraint_error raised" );
     when others =>
        err_exception_raised;
     end;
  end if;
end ParseLevelEnd;

procedure ParseOK is
  msgExpr    : unbounded_string;
  msgType    : identifier;
begin
  expect( logs_ok_t );
  ParseSingleStringParameter( msgExpr, msgType, uni_string_t );
  if isExecutingCommand then
     begin
        log_clean_message( msgExpr );
--put_line( "DEBUG: in_chain = " & in_chain'img );
        if in_chain = none then
           log_first_part( basename( getSourceFileName ) & ": 0", "OK" );
           log_last_part( msgExpr );
        else
--put_line( "DEBUG: context = " & chain_context'img );
           case chain_context is
           when first =>
              log_first_part( msgExpr, "OK" );
           when middle =>
              log_middle_part( msgExpr );
           when last =>
              log_last_part( msgExpr );
           when others =>
              err( "internal error: unexpected chain context" );
           end case;
        end if;
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseOK;

procedure ParseInfo is
  msgExpr    : unbounded_string;
  msgType    : identifier;
begin
  expect( logs_info_t );
  ParseSingleStringParameter( msgExpr, msgType, uni_string_t );
  if isExecutingCommand then
     begin
        log_clean_message( msgExpr );
-- TODO: chain context
--
  --case context is
  --when chains.context_first =>
  --   log_first_part( message, "OK" );
  --when chains.context_middle =>
  --   log_middle_part( m );
  --when chains.context_last =>
  --   log_last_part( m );
  --when chains.not_in_chain =>
  --   log_first_part( source_info.file & ": 0", "OK" );
  --   log_last_part( m );
  --when others =>
  --   put_line( standard_error, "unexpect chain context" );
  --end case;
        log_first_part( basename( getSourceFileName ) & ": 0", "INFO" );
        log_last_part( msgExpr );
     exception when others =>
        err_exception_raised;
     end;
  end if;
  null;
end ParseInfo;

procedure ParseWarning is
  msgExpr    : unbounded_string;
  msgType    : identifier;
begin
  expect( logs_warning_t );
  ParseSingleStringParameter( msgExpr, msgType, uni_string_t );
  if isExecutingCommand then
     begin
        log_clean_message( msgExpr );
-- TODO: chain context
--
  --case context is
  --when chains.context_first =>
  --   log_first_part( message, "OK" );
  --when chains.context_middle =>
  --   log_middle_part( m );
  --when chains.context_last =>
  --   log_last_part( m );
  --when chains.not_in_chain =>
  --   log_first_part( source_info.file & ": 0", "OK" );
  --   log_last_part( m );
  --when others =>
  --   put_line( standard_error, "unexpect chain context" );
  --end case;
        log_first_part( basename( getSourceFileName ) & ": 0", "WARNING" );
        log_last_part( msgExpr );
     exception when others =>
        err_exception_raised;
     end;
  end if;
  null;
end ParseWarning;

procedure ParseError is
  msgExpr    : unbounded_string;
  msgType    : identifier;
begin
  expect( logs_error_t );
  ParseSingleStringParameter( msgExpr, msgType, uni_string_t );
  if isExecutingCommand then
     begin
        log_clean_message( msgExpr );
-- TODO: chain context
--
  --case context is
  --when chains.context_first =>
  --   log_first_part( message, "OK" );
  --when chains.context_middle =>
  --   log_middle_part( m );
  --when chains.context_last =>
  --   log_last_part( m );
  --when chains.not_in_chain =>
  --   log_first_part( source_info.file & ": 0", "OK" );
  --   log_last_part( m );
  --when others =>
  --   put_line( standard_error, "unexpect chain context" );
  --end case;
        log_first_part( basename( getSourceFileName ) & ": 0", "ERROR" );
        log_last_part( msgExpr );
     exception when others =>
        err_exception_raised;
     end;
  end if;
  null;
end ParseError;

procedure ParseOpen is
  -- Syntax: logs.open( "program", "path", mode [,width] );
  nameExpr : unbounded_string;
  nameType : identifier;
  pathExpr : unbounded_string;
  pathType : identifier;
  modeExpr : unbounded_string;
  modeType : identifier;
  widthExpr: unbounded_string;
  widthType: identifier;
begin
  expect( logs_open_t );
  ParseFirstStringParameter( nameExpr, nameType, string_t );
  ParseNextStringParameter(  pathExpr, pathType, string_t );
  ParseNextEnumParameter(    modeExpr, modeType, log_modes_t );
  if token = symbol_t and identifiers( token ).value.all = ")" then
     widthExpr:= to_unbounded_string( " 75" );
     expect( symbol_t, ")" );
  else
     ParseLastNumericParameter( widthExpr, widthType, positive_t );
  end if;

  if isExecutingCommand then
     if log_is_open then
        err( "log is already open" );
     else
        level := 0;
        width := positive( to_numeric( widthExpr ) );
        program_name := nameExpr;
        log_path := pathExpr;
        lock_file_path := log_path & ".lck";
        log_mode := log_modes'val( integer( to_numeric( modeExpr ) ) );
        log_is_open := true;
        --log_info( "Start " & log_program_name & " run" )
     end if;
  end if;
end ParseOpen;

procedure ParseClose is
-- Syntax: logs.close
begin
  expect( logs_close_t );
  if isExecutingCommand then
     if not log_is_open then
        err( "log is already closed" );
     else
        level := 0;
        -- log_info( "End " & log_program_name & " run" );
        log_mode := stderr_log;
        log_is_open := false;
     end if;
  end if;
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

procedure ParseLevel( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: l := logs.level
begin
  result :=to_unbounded_string( natural'image( level ) );
  kind := natural_t;
  expect( logs_level_t );
end ParseLevel;

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
        err( "logs are already rotating" );
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
        err( "logs are not rotating" );
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


-----------------------------------------------------------------------------

procedure StartupLogs is
begin
  declareNamespace( "logs" );

  declareIdent( log_level_t, "logs.log_level", natural_t, typeClass );

  declareFunction(  logs_level_begin_t, "logs.level_begin",  ParseLevelBegin'access );
  declareProcedure( logs_level_end_t,   "logs.level_end",    ParseLevelEnd'access );

  declareProcedure( logs_ok_t,          "logs.ok",           ParseOK'access );
  declareProcedure( logs_info_t,        "logs.info",         ParseInfo'access );
  declareProcedure( logs_warning_t,     "logs.warning",      ParseWarning'access );
  declareProcedure( logs_error_t,       "logs.error",        ParseError'access );

  declareProcedure( logs_open_t,        "logs.open",         ParseOpen'access );
  declareProcedure( logs_close_t,       "logs.close",        ParseClose'access );

  declareFunction(  logs_is_open_t,     "logs.is_open",      ParseIsOpen'access );
  declareFunction(  logs_mode_t,        "logs.mode",         ParseMode'access );
  declareFunction(  logs_level_t,       "logs.level",        ParseLevel'access );
  declareFunction(  logs_width_t,       "logs.width",        ParseWidth'access );

  declareProcedure( logs_rotate_begin_t, "logs.rotate_begin", ParseRotateBegin'access );
  declareProcedure( logs_rotate_end_t,   "logs.rotate_end",   ParseRotateEnd'access );
  declareFunction(  logs_is_rotating_t, "logs.is_rotating",  ParseIsRotating'access );

  declareIdent( log_modes_t, "logs.log_modes", root_enumerated_t, typeClass );

  declareNamespaceClosed( "logs" );

  declareNamespace( "log_mode" );

  declareStandardEnum( log_mode_stderr_t, "log_mode.stderr", log_modes_t, "0" );
  declareStandardEnum( log_mode_file_t,   "log_mode.file",   log_modes_t, "1" );
  declareStandardEnum( log_mode_echo_t,   "log_mode.echo",   log_modes_t, "2" );

  declareNamespaceClosed( "log_mode" );
end StartupLogs;

procedure ShutdownLogs is
begin
  null;
end ShutdownLogs;

end parser_logs;

