------------------------------------------------------------------------------
-- Parser Aux (Parser Support)                                              --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2026 Free Software Foundation              --
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
     symbol_table;
use  ada.strings.unbounded,
     spar_os,
     world,
     symbol_table;

package parser_aux is

recSep : constant character := ASCII.NUL;  -- record field separator char

-- sockets and files identifier fields

ch_field    : constant natural := 1;
fd_field    : constant natural := 2;
line_field  : constant natural := 3;
eol_field   : constant natural := 4;
name_field  : constant natural := 5;
mode_field  : constant natural := 6; -- files only
doget_field : constant natural := 6; -- sockets only
eof_field   : constant natural := 7;

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

function openSocket( serverName : unbounded_string; port : integer ) return ASocketFD;
-- open a TCP/IP socket

procedure DoGet( ref : reference );
-- read a character from a file or socket

procedure DoInitFileVariableFields( file : identifier; fd : aFileDescriptor;
  name : string; mode : identifier  );
-- Create the fields in a new file variable

procedure DoFileOpen( ref : reference;  mode : identifier; create : boolean;
  name : string; fileMetaTags : metaTagHashedSet.Set );
-- Open a file for a variable of type file_type and set the file record's
-- data fields.

procedure DoSocketOpen( file_ref : reference; name : unbounded_string; socketMetaTags : metaTagHashedSet.set );
-- Open a network socket with the name and port specified in name (port in :n
-- format).  The default port is port 80 (usually HTTP).  Update the socket_type
-- variable referenced by ref to reflect the open file.


-- Template Support

procedure processTemplate;
-- Read a template and process embedded scripts.  This procedure is expected
-- to be invoked after the main script has run.
-- Exceptions: STATUS_ERROR, NAME_ERROR, MODE_ERROR, etc.

-- User Exit Support

procedure DoQuit;
-- quit the shell

procedure DoReturn;
-- return to execution in breakout mode

procedure DoStartBreakout( execution_position : out unbounded_string );
-- start a breakout session


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
