------------------------------------------------------------------------------
-- Text_IO Package                                                          --
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

with ada.text_io.editing,
    ada.strings.unbounded,
    bush_os,
    world,
    scanner,
    parser_params;
use ada.text_io.editing,
    ada.strings.unbounded,
    ada.strings.unbounded,
    bush_os,
    world,
    scanner,
    parser_params;

package parser_tio is


------------------------------------------------------------------------------
-- GLOBAL STUFF
------------------------------------------------------------------------------

originalStandardInput  : aFileDescriptor; -- before Set_Input
originalStandardOutput : aFileDescriptor; -- before Set_Output
originalStandardError  : aFileDescriptor; -- before Set_Error

currentStandardInput  : aFileDescriptor; -- as set by Set_Input
currentStandardOutput : aFileDescriptor; -- as set by Set_Output
currentStandardError  : aFileDescriptor; -- as set by Set_Error

-- sockets and files identifier fields

ch_field    : constant natural := 1;
fd_field    : constant natural := 2;
line_field  : constant natural := 3;
eol_field   : constant natural := 4;
name_field  : constant natural := 5;
mode_field  : constant natural := 6; -- files only
doget_field : constant natural := 6; -- sockets only
eof_field   : constant natural := 7;

type decimal_output_type is delta 0.0001 digits 18;
package decimal_io is new Ada.Text_IO.Editing.Decimal_Output( decimal_output_type );
use decimal_io;
-- the decimal output type is arbitrarily chosen

------------------------------------------------------------------------------
-- UTILITIES
------------------------------------------------------------------------------

procedure DoGet( ref : reference );
-- in the ch_field field of the file record.  If there is no next
-- character, set the eof_field to true.  The caller is assumed to
-- check that the file is open.  There is no eof_field check.
--
-- Reasoning: UNIX/Linux has a terrible way to handle end-of-file:
-- you have to read one character too many and check to see if no
-- character was read.  As a result, Text_IO routines must always
-- be "double-buffered": they must read the character into a buffer,
-- and then the application must read the character from the buffer
-- to its final destination.  The end-of-file cannot be checked
-- without a read, and reading will cause characters to be lost if
-- they are not double-buffered.  But I didn't design it, did I?

procedure DoInitFileVariableFields( file : identifier; fd : aFileDescriptor;
  name : string; mode : identifier  );
-- Create the fields in a new file_type limited record variable

procedure DoFileOpen( ref : in out reference;  mode : identifier; create : boolean; name : string );
-- Open a file with the given name, mode and create flag and update the
-- file_type variable referenced by ref to reflect the open file.

procedure DoSocketOpen( file_ref : in out reference; name : unbounded_string );
-- Open a network socket with the name and port specified in name (port in :n
-- format).  The default port is port 80 (usually HTTP).  Update the socket_type
-- variable referenced by ref to reflect the open file.

procedure ParseOpenFile( return_ref : out reference );
procedure ParseOpenSocket( return_ref : out reference );
procedure ParseOpenFileOrSocket( return_ref : out reference; kind : out identifier );
procedure ParseClosedFile( r : out reference );
procedure ParseClosedSocket( f : out identifier );
procedure ParseClosedFileOrSocket( return_ref : out reference; kind : out identifier );

------------------------------------------------------------------------------
-- Text IO package identifiers
------------------------------------------------------------------------------

in_file_t  : identifier;  -- file_mode value 'in_file'
out_file_t : identifier;  -- file_mode value 'out_file'
append_file_t : identifier; -- file_mode value 'append_file'
open_t     : identifier;  -- built-in text_io.open
create_t   : identifier;  -- built-in text_io.create
close_t    : identifier;  -- built-in text_io.close
get_t      : identifier;  -- built-in text_io.get
get_line_t : identifier;  -- built-in text_io.get_line
inkey_t    : identifier;  -- built-in inkey
put_t      : identifier;  -- built-in text_io.put
put_line_t : identifier;  -- built-in text_io.put_line
new_line_t : identifier;  -- built-in text_io.new_line
skip_line_t: identifier;  -- built-in text_io.skip_line
is_open_t  : identifier;  -- built-in text_io.is_open
end_of_file_t : identifier;  -- built-in text_io.end_of_file
end_of_line_t : identifier;  -- built-in text_io.end_of_line
line_t     : identifier;  -- built-in text_io.line
name_t     : identifier;  -- built-in text_io.name
mode_t     : identifier;  -- built-in text_io.mode
set_input_t : identifier;  -- built-in text_io.set_input
set_output_t : identifier;  -- built-in text_io.set_output
reset_t    : identifier;  -- built-in text_io.reset
--delete_t   : identifier;  -- built-in text_io.delete
set_error_t : identifier;  -- built-in text_io.set_error
get_immediate_t   : identifier; -- built-in text_io.get_immediate
look_ahead_t      : identifier; -- built-in text_io.look_ahead
standard_input_t  : identifier; -- built-in text_io.standard_input
standard_output_t : identifier; -- built-in text_io.standard_output
standard_error_t  : identifier; -- built-in text_io.standard_error
current_input_t   : identifier; -- built-in text_io.current_input
current_output_t  : identifier; -- built-in text_io.current_output
current_error_t   : identifier; -- built-in text_io.current_error

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupTextIO;
procedure ShutdownTextIO;

------------------------------------------------------------------------------
-- ADA 95 TEXT_IO
------------------------------------------------------------------------------

procedure ParseIsOpen( b : out identifier );
procedure ParseEndOfFile( result : out unbounded_string; kind : out identifier );
procedure ParseEndOfLine( result : out unbounded_string; kind : out identifier );
procedure ParseLine( result : out unbounded_string; kind : out identifier );
procedure ParseName( result : out unbounded_string; kind : out identifier );
procedure ParseMode( result : out unbounded_string; kind : out identifier );
procedure ParseGetLine( str : out unbounded_string; kind : out identifier );
procedure ParseOpen( create : boolean := false );
procedure ParseReset;
procedure ParseClose;
procedure ParseDelete;
procedure ParseSkipLine;
procedure ParseGet;
procedure ParsePutLine;
procedure ParsePut;
procedure ParseNewLine;
procedure ParseSetInput;
procedure ParseSetOutput;
procedure ParseSetError;

------------------------------------------------------------------------------
-- ADASCRIPT EXTENSIONS
------------------------------------------------------------------------------

procedure ParseQuestion;
procedure ParseInkey( str : out unbounded_string; kind : out identifier );

end parser_tio;

