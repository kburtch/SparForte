------------------------------------------------------------------------------
-- Text_IO Package                                                          --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2023 Free Software Foundation              --
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
    spar_os,
    world,
    symbol_table;
use ada.text_io.editing,
    ada.strings.unbounded,
    spar_os,
    world,
    symbol_table;

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
-- defined in parser_aux

type decimal_output_type is delta 0.0001 digits 18;
package decimal_io is new Ada.Text_IO.Editing.Decimal_Output( decimal_output_type );
use decimal_io;
-- the decimal output type is arbitrarily chosen

------------------------------------------------------------------------------
-- UTILITIES
------------------------------------------------------------------------------

procedure assignRenamedFile( subprogramId : identifier; current_file_id : identifier; canonicalRef :
  reference );
-- make current input, output or error an alias to designated file_type reference

procedure ParseOpenFile( subprogram_id : identifier; return_ref : out reference );
procedure ParseOpenSocket( subprogram_id : identifier; return_ref : out reference );
procedure ParseOpenFileOrSocket( subprogram_id : identifier; return_ref : out reference; kind : out identifier );
procedure ParseClosedFile( r : out reference );
procedure ParseClosedSocket( r : out reference );
procedure ParseClosedFileOrSocket( return_ref : out reference; kind : out identifier );

------------------------------------------------------------------------------
-- Text IO package identifiers
------------------------------------------------------------------------------

-- the file modes are declared in scanner so they are available in
-- parser aux.

-- in_file_t  : identifier;  -- file_mode value 'in_file'
-- out_file_t : identifier;  -- file_mode value 'out_file'
-- append_file_t : identifier; -- file_mode value 'append_file'
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

procedure ParseIsOpen( result : out storage );
procedure ParseEndOfFile( result : out storage; kind : out identifier );
procedure ParseEndOfLine( result : out storage; kind : out identifier );
procedure ParseLine( result : out storage; kind : out identifier );
procedure ParseName( result : out storage; kind : out identifier );
procedure ParseMode( result : out storage; kind : out identifier );
procedure ParseGetLine( result : out storage; kind : out identifier );
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
procedure ParseInkey( result : out storage; kind : out identifier );

end parser_tio;

