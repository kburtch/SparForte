------------------------------------------------------------------------------
-- SparForte Ada Language API                                               --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2014 Free Software Foundation              --
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


package body spar_interface is


------------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------------


-- RAISING EXCEPTIONS
--
-- True if errors should raise exceptions when errors occur.  If false,
-- the last error message will be saved.
------------------------------------------------------------------------------

procedure Raise_Exceptions( S : spar_session'class; raising : boolean := true ) is
begin
  S.raisingExceptions := raising;
end Raise_Exceptions;


------------------------------------------------------------------------------
-- Queue
--
-- SparForte is not designed to be multi-threaded (it can be run as multiple
-- _processes_).
------------------------------------------------------------------------------


protected type sparforteQueue is
  entry interpretCommands( S : spar_session'class; commandString : string; preserve : boolean := true );
  entry interpretScript( S : spar_session'class; scriptPath : string; preserve : boolean := true );
end sparforteQueue;

protected body spareforteQueue is
  entry interpretCommands( S : spar_session'class; commandString : string; preserve : boolean := true ) is
  begin
    null;
  end interpretCommands;

  entry interpretScript( S : spar_session'class; scriptPath : string; preserve : boolean := true );
  begin
    null;
  end interpretScript;
end sparforteQueue;


------------------------------------------------------------------------------
-- Setup
------------------------------------------------------------------------------


-- SET COMMAND LINE OPTION
--
-- Enables or uses a command line option.  The exist status will not be set.
-- The command line help and version messages are not handled.
------------------------------------------------------------------------------

procedure Set_Command_Line_Option( S : spar_session'class; option_name, option_arg : string ) is
begin

  -- collect command line options

  if option_name = "" then
     S.lastError := to_unbounded_string( "the option name is missing" );
     if S.raisingErrors then
        raise SESSION_ERROR with S.lastError;
     end if;
  elsif option_name = "-h" or option_name = "--help" then
     null;
  elsif option_name = "-V" or option_name = "--version" then
     null;
  elsif option_name = "-L" then
     if options_arg'length = 0 then
        S.lastError := to_unbounded_string( "missing argument for -L" );
        if S.raisingErrors then
           raise SESSION_ERROR with S.lastError;
        end if;

        if length( libraryPath ) > 0 then
           libraryPath := libraryPath & ":";
        end if;
        libraryPath := libraryPath & to_unbounded_string( option_arg );
        libraryPathNext := false;

   -- Interpret Long (--) Arguments

   elsif option_name = "-b" or option_name = "--break" then
      breakoutOpt := true;
   elsif option_name = "-c" or option_name = "--check" then
      syntaxOpt := true;
   elsif option_name = "-d" or option_name = "--debug" then
      debugOpt := true;
   elsif option_name = "-D" or option_name = "--design" then
      designOpt := true;
   elsif option_name = "-e" or option_name = "--exec" then
      execOpt := true;
   elsif option_name = "-g" or option_name = "--gcc-errors" then
      gccOpt := true;
   elsif option_name = "-i" or option_name = "--import-all" then
      importOpt := true;
   elsif option_name = "-l" or option_name = "--login" then
      isLoginShell := true;
   elsif option_name = "-m" or option_name = "--maintenance" then
      maintenanceOpt := true;
   elsif option_name = "-r" or option_name = "--restricted" then
      rshOpt := true;
   elsif option_name = "-t" or option_name = "--test" then
      testOpt := true;
   elsif option_name = "-x" or option_name = "--trace" then
      traceOpt := true;
   elsif option_name = "-v" or option_name = "--verbose" then
      verboseOpt := true;
   elsif option_name = "-V" or option_name = "--version" then
      null;
   elsif option_name = "--" then
      null;
   else
      S.lastError := to_unbounded_string( "unexpected option " & option_name );
      if S.raisingErrors then
         raise SESSION_ERROR with S.lastError;
      end if;
  end if;
end Set_Command_Line_Option;


------------------------------------------------------------------------------
-- running things
------------------------------------------------------------------------------


-- INTERPRET COMMAND
--
------------------------------------------------------------------------------

procedure Interpret_Commands( S : spar_session'class;
  commandString : string; preserve : boolean := true ) is
begin
  null;
end Interpret_Commands;


-- INTERPRET SCRIPT
--
------------------------------------------------------------------------------

procedure Interpret_Script( S : spar_session'class;
  scriptPath : string; preserve : boolean := true ) is
begin
  null;
end Interpret_Script;


------------------------------------------------------------------------------
-- Finish Up
------------------------------------------------------------------------------


function Get_Status return natural is
begin
end Get_Status;

function Get_Value( S : spar_session'class; name : string ) return string is
begin
end Get_Value;


end spar_interface;

