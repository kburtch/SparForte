------------------------------------------------------------------------------
-- AdaScript Language Parser                                                --
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

with Interfaces.C,
    ada.strings.unbounded,
    bush_os, world, scanner, scanner_arrays, parser_aux;
use Interfaces.C,
    ada.strings.unbounded,
    bush_os, world, scanner, scanner_arrays, parser_aux;

package parser is

---------------------------------------------------------
-- START OF ADASCRIPT PARSER
---------------------------------------------------------

-- This list is not a complete list of parser procedures.
-- It is primarily for child packages and forward declarations.

procedure ParseNewIdentifier( id : out identifier );
procedure ParseIdentifier( id : out identifier );
procedure ParseOutParameter( ref:out reference; defaultType : identifier );
procedure ParseInOutParameter( ref : out reference );

procedure ParseFactor( f : out unbounded_string; kind : out identifier );
procedure ParsePowerTermOperator( op : out unbounded_string );
procedure ParsePowerTerm( term : out unbounded_string; term_type : out identifier );
procedure ParseTermOperator( op : out unbounded_string );
procedure ParseTerm( term : out unbounded_string; term_type : out identifier );
procedure ParseSimpleExpressionOperator( op : out unbounded_string );
procedure ParseSimpleExpression( se : out unbounded_string; expr_type : out identifier );
procedure ParseRelationalOperator( op : out unbounded_string );
procedure ParseRelation( re : out unbounded_string; rel_type : out identifier );
procedure ParseExpressionOperator( op : out identifier );
procedure ParseExpression( ex : out unbounded_string; expr_type : out identifier );
procedure ParseAssignPart( expr_value : out unbounded_string; expr_type : out identifier );
procedure ParseDeclarationPart( id : in out identifier; anon_arrays : boolean );
procedure ParsePragma;
procedure ParseType;
procedure ParseSubtype;
procedure ParseIfBlock;
procedure ParseWhileBlock;
procedure ParseForBlock;

procedure ParseDelay;
procedure ParseTypeset;
procedure ParseDeclarations;
procedure SkipBlock( termid1, termid2 : identifier := keyword_t );
procedure ParseBlock( termid1, termid2 : identifier := keyword_t );
procedure ParseShellCommand;
procedure ParseGeneralStatement;

-- procedure parse;
-- parse is never executed directly: use an interpret procedure

procedure parseNewCommands( scriptState : out aScriptState; commands : unbounded_string; fragment : boolean := true );
-- for user-defined procedures, functions and backquoted literals


------------------------------------------------------------------------------
-- INTERPRETING
--
-- Kicking off an AdaScript session or AdaScript commands
------------------------------------------------------------------------------

procedure interactiveSession;
-- start an interactive session

procedure interpretScript( scriptPath : string );

procedure interpretCommands( commandString : string );
procedure interpretCommands( commandString : unbounded_string );
-- run the string of commands

procedure interpret;
-- Check the command line options and run a session, script or strings as
-- required.  Also, run any templates.

------------------------------------------------------------------------------
-- For Foreign Languages (eg. C) who want to run BUSH as a library.
------------------------------------------------------------------------------

type C_path is new char_array(0..1024);
type C_cmds is new char_array(0..32768);

procedure BUSH_interpretScript( C_scriptPath : C_path );
pragma export( C, BUSH_interpretScript, "BUSH_interpretScript" );

-- run the indicated script
procedure BUSH_interpretCommands( C_commandString : C_cmds );
pragma export( C, BUSH_interpretCommands, "BUSH_interpretCommands" );

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure startParser;
procedure shutdownParser;

end parser;