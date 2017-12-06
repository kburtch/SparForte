------------------------------------------------------------------------------
-- Logs Package Parser                                                      --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2017 Free Software Foundation              --
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
    world,
    scanner,
    parser_params;
use
    ada.text_io,
    ada.strings.unbounded,
    world,
    scanner,
    parser_params;

package body parser_logs is

type log_modes is ( stderr_log, file_log, echo_log );

program_name   : unbounded_string;
log_path       : unbounded_string;
lock_file_path : unbounded_string;
level          : natural := 0;
width          : positive := 75;
log_mode       : log_modes;
log_is_open    : boolean := false;

------------------------------------------------------------------------------
-- Logs package identifiers
------------------------------------------------------------------------------

log_mode_t          : identifier;
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

------------------------------------------------------------------------------
-- Utility subprograms
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Parser subprograms
------------------------------------------------------------------------------

procedure ParseLevelBegin is
begin
  null;
end ParseLevelBegin;

procedure ParseLevelEnd is
begin
  null;
end ParseLevelEnd;

procedure ParseOK is
begin
  null;
end ParseOK;

procedure ParseInfo is
begin
  null;
end ParseInfo;

procedure ParseWarning is
begin
  null;
end ParseWarning;

procedure ParseError is
begin
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
  ParseNextEnumParameter(    modeExpr, modeType, log_mode_t );
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
begin
  result := null_unbounded_string;
  kind := eof_t;
end ParseIsOpen;

procedure ParseMode( result : out unbounded_string; kind : out identifier ) is
begin
  result := null_unbounded_string;
  kind := eof_t;
end ParseMode;

procedure ParseLevel( result : out unbounded_string; kind : out identifier ) is
begin
  result := null_unbounded_string;
  kind := eof_t;
end ParseLevel;

procedure ParseWidth( result : out unbounded_string; kind : out identifier ) is
begin
  result := null_unbounded_string;
  kind := eof_t;
end ParseWidth;

-----------------------------------------------------------------------------

procedure StartupLogs is
begin
  declareNamespace( "logs" );

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
  declareFunction(  logs_level_t,       "logs.level",        ParseLevel'access );
  declareFunction(  logs_width_t,       "logs.width",        ParseWidth'access );

  declareIdent( log_mode_t, "logs.log_mode", root_enumerated_t, typeClass );

  declareNamespaceClosed( "logs" );

  declareNamespace( "log_mode" );

  -- declareStandardConstant( false_t, "false", boolean_t, "0" );
  -- declareStandardEnum( true_t, "true", boolean_t, "1" );

  declareStandardEnum( log_mode_stderr_t, "log_mode.stderr", log_mode_t, "0" );
  declareStandardEnum( log_mode_file_t,   "log_mode.file",   log_mode_t, "1" );
  declareStandardEnum( log_mode_echo_t,   "log_mode.echo",   log_mode_t, "2" );

  declareNamespaceClosed( "log_mode" );
end StartupLogs;

procedure ShutdownLogs is
begin
  null;
end ShutdownLogs;

end parser_logs;

