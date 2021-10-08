------------------------------------------------------------------------------
-- Strings Package Parser                                                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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
    world,
    scanner,
    scanner.communications,
    parser;
use ada.command_line,
    ada.strings.unbounded,
    world,
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

procedure ParseArgument( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := uni_string_t;
  result := null_unbounded_string;
  expect( cmd_argument_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, positive_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Argument( integer'value(
         to_string( expr_val ) ) + optionOffset ) );
     exception when others =>
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

procedure ParseArgument_Count( result : out unbounded_string; kind : out identifier ) is
begin
  kind := natural_t;
  expect( cmd_argcount_t );
  if isExecutingCommand then
     result := to_unbounded_string( integer'image( Argument_Count-optionOffset ));
  end if;
end ParseArgument_Count;


------------------------------------------------------------------------------
--  COMMAND NAME
--
-- Syntax: s := command_line.command_name
-- Ada: Command_Line.Command_Name
------------------------------------------------------------------------------

procedure ParseCommand_Name( result : out unbounded_string; kind : out identifier ) is
begin
  kind := string_t;
  expect( cmd_commandname_t );
  if isExecutingCommand then
     result := to_unbounded_string( Command_Name );
  end if;
end ParseCommand_Name;


------------------------------------------------------------------------------
--  SET EXIT STATUS
--
-- Syntax: s := command_line.set_exit_status( i )
-- Ada: Command_Line.Set_Exit_Status
------------------------------------------------------------------------------

procedure ParseSetExitStatus is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  expect( cmd_setexit_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, short_short_integer_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       last_status := aStatusCode( to_numeric( expr_val ) );
     exception when others =>
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

procedure ParseEnvironment_Count( result : out unbounded_string; kind : out identifier ) is
begin
  kind := natural_t;
  expect( cmd_envcnt_t );
  if isExecutingCommand then
     result := to_unbounded_string( integer'image( Environment_Count ));
  end if;
end ParseEnvironment_Count;


------------------------------------------------------------------------------
--  ENVIRONMENT VALUE
--
-- Syntax: s := command_line.environment_value( p )
-- Ada: Command_Line.Environment_Value
------------------------------------------------------------------------------

procedure ParseEnvironment_Value( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := uni_string_t;
  result := null_unbounded_string;
  expect( cmd_envval_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if baseTypesOk( expr_type, positive_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Environment_Value( integer'value(
         to_string( expr_val ) ) ) );
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
