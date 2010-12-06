------------------------------------------------------------------------------
-- BUSH Parser Aux (Parser Support)                                         --
--                                                                          --
-- Part of BUSH                                                             --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2005 Ken O. Burtch & FSF                 --
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
-- CVS: $Header: /home/cvsroot/bush/src/parser_aux.ads,v 1.3 2005/06/24 01:01:16 ken Exp $

with ada.strings.unbounded, bush_os, world, scanner, scanner_arrays;
use ada.strings.unbounded, bush_os, world, scanner, scanner_arrays;

package parser_aux is

recSep : constant character := ASCII.NUL;  -- record field separator char

function isExecutingCommand return boolean;
-- True if OK to execute a statement that does work.
-- That is, the parser isn't skipping the line because of
-- an error or exiting a block.
pragma inline( isExecutingCommand );

procedure discardUnusedIdentifier( id : identifier );
-- if an identifier has been not been assigned a type,
-- assume it's unused and discard it.

procedure makeTempFile( s : out unbounded_string );
-- create a unique temporary filename

--function isEOF( fd : aFileDescriptor ) return boolean;
-- true if the file descriptor is at the end of file

-- Parameter references

type reference is record
     id    : identifier;   -- the identifier
     a_id  : arrayID;      -- the array (if an array)
     index : long_integer := 0; -- the array index (if an array)
     kind  : identifier;   -- the type name
end record;

procedure AssignParameter( ref : in reference; value : unbounded_string );
pragma inline( AssignParameter );
-- assign a value to the variable or array indicated by ref

procedure GetParameterValue( ref : in reference; value : out unbounded_string );
pragma inline( GetParameterValue );
-- return the value of the variable or array indicated by ref

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

procedure parseProcedureCallSemicolon;
-- more informative than expect( .. ";" );

procedure parseFunctionCallSemicolon;
-- more informative than expect( .. ";" );


function castToType( val : long_float; kind : identifier ) return unbounded_string;
function castToType( val : unbounded_string; kind : identifier ) return unbounded_string;
-- If a value is an integer type (i.e. positive, natural or integer),
-- round the value.  Otherwise do not round the value.  Return the
-- result as a string value.

end parser_aux;
