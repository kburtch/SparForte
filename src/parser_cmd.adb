------------------------------------------------------------------------------
-- Strings Package Parser                                                   --
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
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

with ada.command_line,
    ada.strings.unbounded,
    pegasoft,
    world,
    symbol_table,
    message_strings,
    scanner,
    scanner.communications,
    parser;
use ada.command_line,
    ada.strings,
    ada.strings.unbounded,
    pegasoft,
    world,
    symbol_table,
    message_strings,
    scanner,
    scanner.communications,
    parser;

pragma warnings( off );               -- suppress GNAT interal package warning
with  ada.command_line.environment;
use   ada.command_line.environment;
pragma warnings( on );

package body parser_cmd is

------------------------------------------------------------------------------
-- command_line package identifiers
------------------------------------------------------------------------------

cmd_argument_t    : identifier;
cmd_argcount_t    : identifier;
cmd_commandname_t : identifier;
cmd_setexit_t     : identifier;
cmd_envcnt_t      : identifier;
cmd_envval_t      : identifier;


------------------------------------------------------------------------------
--  ARGUMENT
--
-- Syntax: s := command_line.argument( p )
-- Ada: Command_Line.Argument
------------------------------------------------------------------------------

procedure ParseArgument( result : out storage; kind : out identifier ) is
  expr  : storage;
  expr_type : identifier;
  subprogramId : constant identifier := cmd_argument_t;
begin
  kind := uni_string_t;
  result := nullStorage;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  if baseTypesOk( expr_type, positive_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     begin
       result := storage'( to_unbounded_string( Argument( integer'value(
         to_string( expr.value ) ) + optionOffset ) ), sparMetaLabel );
     exception when constraint_error =>
       err( context => subprogramId,
            subjectNotes => pl( "position " ) & em_value( trim( expr.value, both ) ),
            reason => +"is not in range",
            obstructorNotes => pl( "1 .." ) & pl( integer'image( Argument_Count-optionOffset ) ),
            seeAlso => + "doc/pkg_cmdline.html"
       );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseArgument;


------------------------------------------------------------------------------
--  ARGUMENT COUNT
--
-- Syntax: s := command_line.argument_count
-- Ada: Command_Line.Argument_Cont
------------------------------------------------------------------------------

procedure ParseArgument_Count( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cmd_argcount_t;
begin
  kind := natural_t;
  expect( subprogramId );

  if isExecutingCommand then
     result := storage'( to_unbounded_string( integer'image( Argument_Count-optionOffset )),
        sparMetaLabel );
  end if;
end ParseArgument_Count;


------------------------------------------------------------------------------
--  COMMAND NAME
--
-- Syntax: s := command_line.command_name
-- Ada: Command_Line.Command_Name
------------------------------------------------------------------------------

procedure ParseCommand_Name( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cmd_commandname_t;
begin
  kind := string_t;
  expect( subprogramId );

  if isExecutingCommand then
     result := storage'( to_unbounded_string( Command_Name ), sparMetaLabel );
  end if;
end ParseCommand_Name;


------------------------------------------------------------------------------
--  SET EXIT STATUS
--
-- Syntax: s := command_line.set_exit_status( i )
-- Ada: Command_Line.Set_Exit_Status
------------------------------------------------------------------------------

procedure ParseSetExitStatus is
  statusExpr  : storage;
  statusType : identifier;
  subprogramId : constant identifier := cmd_setexit_t;
begin
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( statusExpr, statusType );
  if baseTypesOk( statusType, short_short_integer_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     begin
       last_status := aStatusCode( to_numeric( statusExpr.value ) );
     exception when constraint_error =>
       err( context => subprogramId,
            subjectNotes => pl( "status code " ) & em_value( trim( statusExpr.value , both ) ),
            reason => +"is not in range",
            obstructorNotes => pl( aStatusCode'first'img ) & pl( " .." ) & pl( aStatusCode'last'img ),
            seeAlso => + "doc/pkg_cmdline.html"
       );
     when others =>
       err_exception_raised;
     end;
  end if;
end ParseSetExitStatus;


------------------------------------------------------------------------------
--  ENVIRONMENT COUNT
--
-- Syntax: s := command_line.environment_count
-- Ada: Command_Line.Environment_Count
------------------------------------------------------------------------------

procedure ParseEnvironment_Count( result : out storage; kind : out identifier ) is
  subprogramId : constant identifier := cmd_envcnt_t;
begin
  kind := natural_t;
  expect( subprogramId );

  if isExecutingCommand then
     result := storage'( to_unbounded_string( integer'image( Environment_Count )),
        sparMetaLabel );
  end if;
end ParseEnvironment_Count;


------------------------------------------------------------------------------
--  ENVIRONMENT VALUE
--
-- Syntax: s := command_line.environment_value( p )
-- Ada: Command_Line.Environment_Value
------------------------------------------------------------------------------

procedure ParseEnvironment_Value( result : out storage; kind : out identifier ) is
  expr  : storage;
  expr_type : identifier;
  subprogramId : constant identifier := cmd_envval_t;
begin
  kind := uni_string_t;
  result := nullStorage;
  expect( subprogramId );
  expect( symbol_t, "(" );
  ParseExpression( expr, expr_type );
  if baseTypesOk( expr_type, positive_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     -- the meta label is not checked since it is a position
     begin
        result := storage'( to_unbounded_string( Environment_Value( integer'value(
           to_string( expr.value ) ) ) ), sparMetaLabel );
     exception when others =>
        err_exception_raised;
     end;
  end if;
end ParseEnvironment_Value;

------------------------------------------------------------------------------
-- Housekeeping
------------------------------------------------------------------------------

procedure StartupCommandLine is
begin
  declareNamespace( "command_line" );
  declareFunction( cmd_argument_t, "command_line.argument", ParseArgument'access );
  declareFunction( cmd_argcount_t, "command_line.argument_count", ParseArgument_Count'access );
  declareFunction( cmd_commandname_t, "command_line.command_name", ParseCommand_Name'access );
  declareProcedure( cmd_setexit_t, "command_line.set_exit_status", ParseSetExitStatus'access );
  declareFunction( cmd_envcnt_t, "command_line.environment.environment_count", ParseEnvironment_Count'access );
  declareFunction( cmd_envval_t, "command_line.environment.environment_value", ParseEnvironment_Value'access );
  declareNamespaceClosed( "command_line" );
end StartupCommandLine;

procedure ShutdownCommandLine is
begin
  null;
end ShutdownCommandLine;

end parser_cmd;
