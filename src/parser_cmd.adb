------------------------------------------------------------------------------
-- Strings Package Parser                                                   --
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

with gnat.regexp,
    gnat.regpat,
    ada.command_line,
    world,
    scanner,
    string_util,
    parser_aux,
    parser,
    bush_os;
use gnat.regexp,
    gnat.regpat,
    ada.command_line,
    world,
    scanner,
    string_util,
    parser_aux,
    parser,
    bush_os;

pragma warnings( off );               -- suppress GNAT interal package warning
with  ada.command_line.environment;
use   ada.command_line.environment;
pragma warnings( on );

package body parser_cmd is


procedure ParseArgument( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := uni_string_t;
  result := null_unbounded_string;
  expect( cmd_argument_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if intTypesOk( expr_type, positive_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Argument( integer'value(
         to_string( expr_val ) ) + optionOffset ) );
     exception when others =>
       err( "exception raised" );
     end;
  end if;
end ParseArgument;

procedure ParseArgument_Count( result : out unbounded_string; kind : out identifier ) is
begin
  kind := natural_t;
  expect( cmd_argcount_t );
  if isExecutingCommand then
     result := to_unbounded_string( integer'image( Argument_Count-optionOffset ));
  end if;
end ParseArgument_Count;

procedure ParseCommand_Name( result : out unbounded_string; kind : out identifier ) is
begin
  kind := string_t;
  expect( cmd_commandname_t );
  if isExecutingCommand then
     result := to_unbounded_string( Command_Name );
  end if;
end ParseCommand_Name;

procedure ParseSetExitStatus is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  expect( cmd_setexit_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if intTypesOk( expr_type, short_short_integer_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       last_status := aStatusCode( to_numeric( expr_val ) );
     exception when others =>
       err( "exception raised" );
     end;
  end if;
end ParseSetExitStatus;

procedure ParseEnvironment_Count( result : out unbounded_string; kind : out identifier ) is
begin
  kind := natural_t;
  expect( cmd_envcnt_t );
  if isExecutingCommand then
     result := to_unbounded_string( integer'image( Environment_Count ));
  end if;
end ParseEnvironment_Count;

procedure ParseEnvironment_Value( result : out unbounded_string; kind : out identifier ) is
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  kind := uni_string_t;
  result := null_unbounded_string;
  expect( cmd_envval_t );
  expect( symbol_t, "(" );
  ParseExpression( expr_val, expr_type );
  if intTypesOk( expr_type, positive_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
       result := to_unbounded_string( Environment_Value( integer'value(
         to_string( expr_val ) ) ) );
     exception when others =>
       err( "exception raised" );
     end;
  end if;
end ParseEnvironment_Value;

procedure StartupCommandLine is
begin
  declareFunction( cmd_argument_t, "command_line.argument", ParseArgument'access );
  declareFunction( cmd_argcount_t, "command_line.argument_count", ParseArgument_Count'access );
  declareFunction( cmd_commandname_t, "command_line.command_name", ParseCommand_Name'access );
  declareProcedure( cmd_setexit_t, "command_line.set_exit_status", ParseSetExitStatus'access );
  declareFunction( cmd_envcnt_t, "command_line.environment.environment_count", ParseEnvironment_Count'access );
  declareFunction( cmd_envval_t, "command_line.environment.environment_value", ParseEnvironment_Value'access );
end StartupCommandLine;

procedure ShutdownCommandLine is
begin
  null;
end ShutdownCommandLine;

end parser_cmd;
