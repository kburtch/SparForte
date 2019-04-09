------------------------------------------------------------------------------
-- Parser Aux (Parser Support)                                              --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2019 Free Software Foundation              --
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

with ada.strings.unbounded,
     spar_os,
     world,
     scanner,
     parser_params;
use  ada.strings.unbounded,
     spar_os,
     world,
     scanner,
     parser_params;

package parser_aux is

recSep : constant character := ASCII.NUL;  -- record field separator char

procedure makeTempFile( s : out unbounded_string );
-- create a unique temporary filename

--function isEOF( fd : aFileDescriptor ) return boolean;
-- true if the file descriptor is at the end of file

function stringField( i : identifier; f : natural ) return unbounded_string;
-- same as string_util.stringField except users an identifier's value

procedure replaceField( i : identifier; f : natural; field : string );
-- same as string_util.replaceField except users an identifier's value

function stringField( r : reference; f : natural ) return unbounded_string;
-- same as string_util.stringField except users an identifier's value

procedure replaceField( r : reference; f : natural; field : string );
-- same as string_util.replaceField except users an identifier's value

function OSerror( e : integer ) return string;
-- return an OS error message for error number e

function openSocket( serverName : unbounded_string; port : integer ) return ASocketFD;
-- open a TCP/IP socket

-- Parsing Short-cuts

--procedure ParseSingleNumericExpression( expr_val : out unbounded_string;
  --expr_type : out identifier );
-- Parse single universal-numeric actual parameter

--procedure ParseSingleStringExpression( expr_val : out unbounded_string;
  --expr_type : out identifier );
-- Parse single string actual parameter

--procedure ParseSingleUniStringExpression( expr_val : out unbounded_string;
  --expr_type : out identifier );
-- Parse single universal string actual parameter

procedure processTemplate;
-- Read a template and process embedded scripts.  This procedure is expected
-- to be invoked after the main script has run.
-- Exceptions: STATUS_ERROR, NAME_ERROR, MODE_ERROR, etc.

procedure DoQuit;
-- quit the shell

procedure DoReturn;
-- return to execution in breakout mode

-- Parsing Symbols

procedure parseProcedureCallSemicolon;
-- more informative than expect( .. ";" );

procedure parseFunctionCallSemicolon;
-- more informative than expect( .. ";" );

-- Renaming Support

procedure FixRenamedRecordFields( canonicalRef : renamingReference;
  renamingRec : identifier );
-- Given a renamed record, search for and adjust the record fields to
-- refer to the correct variables.

procedure FixRenamedArray( canonicalRef : renamingReference;
  renamingArray : identifier );
-- Given a renamed array set up by ParseRenamingPart, fix the attributes
-- for field type.

end parser_aux;
