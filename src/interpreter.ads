------------------------------------------------------------------------------
-- AdaScript Language Interpreter                                           --
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
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C,
    ada.strings.unbounded;

use Interfaces.C,
    ada.strings.unbounded;

package interpreter is

------------------------------------------------------------------------------
-- INTERPRETING
--
-- Kicking off an AdaScript session or AdaScript commands
------------------------------------------------------------------------------

procedure interactiveSession;
-- start an interactive session

procedure interpretScript( scriptPath : string );
-- start interpreting a script

procedure interpretCommands( commandString : string );
procedure interpretCommands( commandString : unbounded_string );
-- run the string of commands

procedure interpret;
-- Check the command line options and run a session, script or strings as
-- required.  Also, run any templates.

------------------------------------------------------------------------------
-- For Foreign Languages (eg. C) who want to run SparForte as a library.
------------------------------------------------------------------------------

type C_path is new char_array(0..1024);
type C_cmds is new char_array(0..32768);

procedure SPAR_interpretScript( C_scriptPath : C_path );
pragma export( C, SPAR_interpretScript, "SPAR_interpretScript" );

-- run the indicated script
procedure SPAR_interpretCommands( C_commandString : C_cmds );
pragma export( C, SPAR_interpretCommands, "SPAR_interpretCommands" );


------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------


procedure startInterpreter;
procedure shutdownInterpreter;

end interpreter;
