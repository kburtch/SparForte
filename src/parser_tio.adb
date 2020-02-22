------------------------------------------------------------------------------
-- Text_IO Package                                                          --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2020 Free Software Foundation              --
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

with interfaces.c,
    ada.exceptions,
    ada.text_io.editing,
    ada.strings.unbounded.text_io,
    gnat.source_info,
    string_util,
    user_io,
    user_io.getline,
    script_io,
    signal_flags,
    scanner.calendar,
    parser_aux,
    parser_cal,
    parser.decl.as,
    parser_params;
use interfaces.c,
    ada.exceptions,
    ada.text_io,
    ada.text_io.editing,
    ada.strings.unbounded,
    ada.strings.unbounded.text_io,
    user_io,
    script_io,
    string_util,
    signal_flags,
    scanner,
    scanner.calendar,
    parser_aux,
    parser_cal,
    parser,
    parser.decl.as,
    parser_params;

package body parser_tio is

procedure ParseIsOpen( b : out identifier ) is
  -- is_open( file )
  -- Ada.Text_IO.Is_Open
  file : identifier;
begin
  b := false_t;
  getNextToken;
  expect( symbol_t, "(" );
  ParseIdentifier( file );
  expect( symbol_t, ")" );
  if file = standard_output_t then
     b := true_t;
  elsif file = standard_error_t then
     b := true_t;
  elsif uniTypesOk( identifiers( file ).kind, file_type_t ) then
     if length( identifiers( file ).value.all ) > 0 then
        b := true_t;
     end if;
  end if;
end ParseIsOpen;

procedure ParseEndOfFile( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: End_of_file( f )
  -- Source: Ada.Text_IO.End_Of_File
  file_ref : reference;
  file_kind : identifier;
begin
  kind := boolean_t;
  result := to_unbounded_string( boolean'image( false ) );
  getNextToken;
  expect( symbol_t, "(" );
  ParseOpenFileOrSocket( file_ref, file_kind );
  if isExecutingCommand then
     if file_kind = file_type_t then
        if identifier'value( to_string( stringField( file_ref, mode_field ) ) ) /= in_file_t then
           err( "end_of_file only applies to " & optional_bold( "in_mode" ) & " files" );
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := stringField( file_ref, eof_field );
  end if;
end ParseEndOfFile;

procedure ParseEndOfLine( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: end_of_line( open-file )
  -- Source: Ada.Text_IO.End_Of_Line
  file_ref : reference;
begin
  kind := boolean_t;
  result := to_unbounded_string( integer'image( 0 ) );
  getNextToken;
  expect( symbol_t, "(" );
  ParseOpenFile( file_ref );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := stringField( file_ref, eol_field );
  end if;
end ParseEndOfLine;

procedure ParseLine( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: line( open-file )
  -- Source: Ada.Text_IO.Line
  file_ref : reference;
begin
  kind := integer_t;   -- TODO: probably should be something more specific
  result := to_unbounded_string( integer'image( 0 ) );
  getNextToken;
  expect( symbol_t, "(" );
  ParseOpenFile( file_ref );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := stringField( file_ref, line_field );
  end if;
end ParseLine;

procedure ParseName( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: name( open-file )
  -- Source: Ada.Text_IO.Name
  file_ref : reference;
begin
  kind := uni_string_t;
  result := null_unbounded_string;
  getNextToken;
  expect( symbol_t, "(" );
  ParseOpenFile( file_ref );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := stringField( file_ref, name_field );
  end if;
end ParseName;

procedure ParseMode( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: mode( open-file )
  -- Source: Ada.Text_IO.Mode
  file_ref : reference;
begin
  kind := file_mode_t;
  result := null_unbounded_string;
  expect( mode_t ); -- getNextToken;
  expect( symbol_t, "(" );
  ParseOpenFile( file_ref );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     result := stringField( file_ref, mode_field );
     if identifier'value( to_string( result ) ) = in_file_t then
        result := identifiers( in_file_t ).value.all;
     elsif identifier'value( to_string( result ) ) = out_file_t then
        result := identifiers( out_file_t ).value.all;
     elsif identifier'value( to_string( result ) ) = append_file_t then
        result := identifiers( append_file_t ).value.all;
     else
        err( Gnat.Source_Info.Source_Location & ": internal error: unable to determine file mode" );
     end if;
  end if;
end ParseMode;

procedure ParseInkey( str : out unbounded_string; kind : out identifier ) is
  -- Syntax: inkey
  -- Source: Ada.Text_IO.Inkey
  ch : character;
begin
  kind := character_t;
  expect( inkey_t );
  if isExecutingCommand then
     getKey( ch );
     --if wasSIGINT then
     --   null;
     --end if;
     str := to_unbounded_string( ch & "" );
  end if;
end ParseInkey;

procedure ParseGetLine( str : out unbounded_string; kind : out identifier ) is
  -- Syntax: get_line [ (open-file) ]
  -- Source: Ada.Text_IO.Get_Line
  -- Note: Gnat get_line can't be used here because it does something
  -- odd with file descriptor 0
  file_ref : reference;
  file_kind : identifier;
  --fd   : aFileDescriptor;
  ch   : character;
  --result : long_integer;
  fileInfo : unbounded_string;
begin
  kind := universal_t;
  file_ref.id := eof_t;
  str := null_unbounded_string;
  getNextToken;
  if token = symbol_t and then identifiers( Token ).value.all = "(" then
      expect( symbol_t, "(" );
      ParseOpenFileOrSocket( file_ref, file_kind );
      expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if file_ref.id /= eof_t then
        if trace then
           put_trace( "Input from file descriptor" & to_string(
              stringField( file_ref, fd_field ) ) );
        end if;
        if stringField(file_ref, eof_field ) = "1" then
           err( "end of file" );
           return;
        end if;
        if file_kind = socket_type_t and then stringField( file_ref, doget_field ) = "1" then
           DoGet( file_ref );
           replaceField( file_ref, doget_field, boolean'image( false ) );
        end if;
        loop
          GetParameterValue( file_ref, fileInfo );
          ch := Element( fileInfo, 1 );
          if stringField(file_ref, eof_field ) = "1" then
             exit;
          end if;
          DoGet( file_ref );
          exit when ch = ASCII.LF or error_found or wasSIGINT;
          str := str & ch;
        end loop;
     else
        user_io.getline.getLine( str );
        if wasSIGINT then
           new_line;  -- user didn't press enter
           -- wasSIGINT will be cleared later
        end if;
     end if;
  end if;
end ParseGetLine;

procedure ParseOpenFile( return_ref : out reference ) is
  -- standard output, standard error or a user file
  -- the file must be closed (ie value of variable is null)
  ref : reference;
begin
  ref.id := eof_t;
  return_ref.id := eof_t; -- assume failure

  -- Special Files: Current_Input, Current_Output and Current_Error
  -- are aliases for a different file variable.  For example, by default,
  -- Current_Output refers to Standard_Output.  These variables must be
  -- "dereferenced" to the actual file they represent so that the file
  -- info (such as eof) are accurately updated in the original file
  -- variable.
  --  Since Standard_Input, Standard_Output and Standard_Error are (now)
  -- represented as true file variables, they need no special treatment.

  if token = current_input_t then
     if isExecutingCommand then
        ref.id := identifier( to_numeric( identifiers(current_input_t).value.all ) );
     end if;
     getNextToken;
  elsif token = current_output_t then
     if isExecutingCommand then
        ref.id := identifier( to_numeric( identifiers(current_output_t).value.all ) );
     end if;
     getNextToken;
  elsif token = current_error_t then
     if isExecutingCommand then
        ref.id := identifier( to_numeric( identifiers(current_error_t).value.all ) );
     end if;
     getNextToken;
  else
     ParseInOutParameter( ref );
     if getUniType( ref.kind ) /= file_type_t then
       err( "file_type expected" );
     end if;
  end if;

  -- Verify that the identifier is a file.  Unless during a syntax check,
  -- verify that the file is also open (files are not opened during a
  -- syntax check).

  if not syntax_check then
     if length( identifiers( ref.id ).value.all ) = 0 then
        err( "file not open" );
     elsif not error_found then
        return_ref := ref;
     end if;
  end if;
end ParseOpenFile;

procedure ParseOpenSocket( return_ref : out reference ) is
  -- standard output, standard error or a user file
  -- the file must be closed (ie value of variable is null)
  ref : reference;
begin
  ref.id := eof_t;
  return_ref.id := eof_t; -- assume failure

  -- Special Files: Current_Input, Current_Output and Current_Error
  -- are aliases for a different file variable.  For example, by default,
  -- Current_Output refers to Standard_Output.  These variables must be
  -- "dereferenced" to the actual file they represent so that the file
  -- info (such as eof) are accurately updated in the original file
  -- variable.
  --  Since Standard_Input, Standard_Output and Standard_Error are (now)
  -- represented as true file variables, they need no special treatment.

  if token = current_input_t then
     if isExecutingCommand then
        ref.id := identifier( to_numeric( identifiers(current_input_t).value.all ) );
     end if;
     getNextToken;
  elsif token = current_output_t then
     if isExecutingCommand then
        ref.id := identifier( to_numeric( identifiers(current_output_t).value.all ) );
     end if;
     getNextToken;
  elsif token = current_error_t then
     if isExecutingCommand then
        ref.id := identifier( to_numeric( identifiers(current_error_t).value.all ) );
     end if;
     getNextToken;
  else
     ParseInOutParameter( ref );
     if getUniType( ref.kind ) /= socket_type_t then
        err( "file_type or socket_type variable expected" );
     end if;
  end if;

  -- Verify that the identifier is a socket.  Unless during a syntax check,
  -- verify that the socket is also open (sockets are not opened during a
  -- syntax check).

  if not syntax_check then
     if length( identifiers( ref.id ).value.all ) = 0 then
        err( "file not open" );
     elsif not error_found then
        return_ref := ref;
     end if;
  end if;
end ParseOpenSocket;

procedure ParseOpenFileOrSocket( return_ref : out reference; kind : out identifier ) is
  -- standard output, standard error or a user file
  -- the file must be closed (ie value of variable is null)
  ref : reference;
begin
  ref.id := eof_t;
  return_ref.id := eof_t; -- assume failure

  -- Special Files: Current_Input, Current_Output and Current_Error
  -- are aliases for a different file variable.  For example, by default,
  -- Current_Output refers to Standard_Output.  These variables must be
  -- "dereferenced" to the actual file they represent so that the file
  -- info (such as eof) are accurately updated in the original file
  -- variable.
  --  Since Standard_Input, Standard_Output and Standard_Error are (now)
  -- represented as true file variables, they need no special treatment.

  if token = current_input_t then
     if isExecutingCommand then
        ref.id := identifier( to_numeric( identifiers(current_input_t).value.all ) );
     end if;
     getNextToken;
  elsif token = current_output_t then
     if isExecutingCommand then
        ref.id := identifier( to_numeric( identifiers(current_output_t).value.all ) );
     end if;
     getNextToken;
  elsif token = current_error_t then
     if isExecutingCommand then
        ref.id := identifier( to_numeric( identifiers(current_error_t).value.all ) );
     end if;
     getNextToken;
  else
     ParseInOutParameter( ref );
     if ref.kind /= file_type_t and ref.kind /= socket_type_t then
        err( "file_type or socket_type variable expected" );
     end if;
  end if;
  kind := ref.kind;

  -- Verify that the identifier is a file or socket.  Unless during a
  -- syntax check, verify that the file/socket is also open (they are
  -- not opened during a syntax check).

  if not syntax_check then
     if length( identifiers( ref.id ).value.all ) = 0 then
        err( "file not open" );
     elsif not error_found then
        return_ref := ref;
     end if;
  end if;
end ParseOpenFileOrSocket;

procedure ParseClosedFile( r : out reference ) is
  -- user file, must be closed (ie. value of variable not null)
  ref : reference;
begin
  ref.id := eof_t; -- assume failure

  -- do not allow "current" or "standard" files to be closed.

  if token = standard_output_t then
     err( "file already open" );
  elsif token = standard_error_t then
     err( "file already open" );
  elsif token = current_input_t then
     err( "file already open" );
  elsif token = current_output_t then
     err( "file already open" );
  elsif token = current_error_t then
     err( "file already open" );
  else
     ParseOutParameter( ref, file_type_t );
     -- GCC Ada 4.7.2 raises STORAGE_ERROR here under certain circumstances
     -- if a put_line doesn't appear.

  -- Verify that the identifier is a file.  Unless during a syntax check,
  -- verify that the file is also open (files are not opened during a
  -- syntax check).

     if getUniType( ref.kind ) /= file_type_t then
        err( "expected file_type variable" );
     elsif not syntax_check then
        if length( identifiers( ref.id ).value.all ) > 0 then
           err( "file already open" );
        elsif not error_found then
           r := ref;
        end if;
     end if;
  end if;
end ParseClosedFile;

procedure ParseClosedSocket( f : out identifier ) is
  -- user file, must be closed (ie. value of variable not null)
  id : identifier;
begin
  f := eof_t; -- assume failure

  -- do not allow "current" or "standard" files to be closed.

  if token = standard_output_t then
     err( "file already open" );
  elsif token = standard_error_t then
     err( "file already open" );
  elsif token = current_input_t then
     err( "file already open" );
  elsif token = current_output_t then
     err( "file already open" );
  elsif token = current_error_t then
     err( "file already open" );
  else
     ParseIdentifier( id );

  -- Verify that the identifier is a socket.  Unless during a syntax check,
  -- verify that the socket is also open (sockets are not opened during a
  -- syntax check).

     if getUniType( identifiers( id ).kind ) /= socket_type_t then
        err( "expected socket_type variable" );
     elsif not syntax_check then
        if length( identifiers( id ).value.all ) > 0 then
           err( "file already open" );
        elsif not error_found then
           f := id;
        end if;
     end if;
  end if;
end ParseClosedSocket;

procedure ParseClosedFileOrSocket( return_ref : out reference; kind : out identifier ) is
  -- user file, must be closed (ie. value of variable not null)
  ref : reference;
begin
  return_ref.id := eof_t; -- assume failure

  -- do not allow "current" or "standard" files to be closed.

  if token = standard_output_t then
     err( "file already open" );
  elsif token = standard_error_t then
     err( "file already open" );
  elsif token = current_input_t then
     err( "file already open" );
  elsif token = current_output_t then
     err( "file already open" );
  elsif token = current_error_t then
     err( "file already open" );
  else
     ParseOutParameter( ref, file_type_t );

  -- Verify that the identifier is a file or socket.  Unless during a
  -- syntax check, verify that the file/socket is also open (they are
  -- not opened during a syntax check).

     kind := ref.kind;
     if kind /= file_type_t and kind /= socket_type_t then
        err( "file_type or socket_type variable expected" );
     elsif not syntax_check then
        if length( identifiers( ref.id ).value.all ) > 0 then
           err( "file already open" );
        elsif not error_found then
           return_ref := ref;
        end if;
     end if;
  end if;
end ParseClosedFileOrSocket;

-- "DO" procedures should be moved to parser_aux.

procedure DoGet( ref : reference ) is
-- Get the next character from a file or socket.  Save the character
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
  fd     : aFileDescriptor;    -- file's file descriptor
  ch     : character := ASCII.NUL; -- a buffer to read the character into
  -- ASCII.NUL to suppress compiler warning
  eof    : boolean := false;   -- true if a character was read
  result : size_t;       -- bytes read by read
  fileInfo : unbounded_string;
begin
   GetParameterValue( ref, fileInfo );
   fd := aFileDescriptor'value( to_string( stringField( fileInfo, recSep, fd_field ) ) );
<<reread>> readchar( result, fd, ch, 1 );
 -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
     if result < 0 or result = size_t'last then
      if C_errno = EAGAIN  or C_errno = EINTR then
         goto reread;                   -- interrupted? try again
      end if;                           -- error? report it
      err( "unable to read file:" & OSerror( C_errno ) );
      return;                           -- and bail out
   elsif result = 0 then                -- nothing read?
      eof := true;                      -- then it's the end of file
   end if;
   if ref.id = current_output_t or      -- SHOULD NEVER BE TRUE BUT...
      ref.id = current_input_t or
      ref.id = current_error_t then
      err( Gnat.Source_Info.Source_Location & ": Internal Error: DoGet was given a file alias not a real file" );
   else
      if eof then                       -- eof? set eof_field
         replaceField( fileInfo, recSep, eof_field, "1" );
         replaceField( fileInfo, recSep, line_field, -- Ada counts EOF as a line!
            long_integer'image( long_integer'value(
            to_string( stringField( fileInfo, recSep, line_field ) ) ) + 1 ) );
      else                              -- else replace the character
         replace_Element( fileInfo, 1, ch ); -- save character in ch_field
         if ch = ASCII.LF then          -- a line? increment line_field
            replaceField( fileInfo, recSep, line_field,
               long_integer'image( long_integer'value(
               to_string( stringField( fileInfo, recSep, line_field ) ) ) + 1 ) );
            replaceField( fileInfo, recSep, eol_field, "1" ); -- and set eol_field
         else
            replaceField( fileInfo, recSep, eol_field, "0" ); -- else not
         end if;                        -- the end of the line
      end if;
   end if;
   AssignParameter( ref, fileInfo );
end DoGet;

procedure DoInitFileVariableFields( file : identifier; fd : aFileDescriptor;
  name : string; mode : identifier  ) is
  -- Create the fields in a new file variable
begin
  -- construct the file variable's value, a series of nul delimited fields
  identifiers( file ).value.all := to_unbounded_string( "." & ASCII.NUL );
  -- 1. character buffer
  identifiers( file ).value.all := identifiers( file ).value.all & to_unbounded_string(      fd'img ) & ASCII.NUL;
  -- 2. file descriptor
  identifiers( file ).value.all := identifiers( file ).value.all & to_unbounded_string(      " 0" ) & ASCII.NUL;
  -- 3. lines
  identifiers( file ).value.all := identifiers( file ).value.all & to_unbounded_string(      "0" ) & ASCII.NUL;
  -- 4. eol flag
  identifiers( file ).value.all := identifiers( file ).value.all & name & ASCII.NUL;
  -- 5. name
  identifiers( file ).value.all := identifiers( file ).value.all & to_unbounded_string(
     mode'img ) & ASCII.NUL;
  -- 6. mode
  identifiers( file ).value.all := identifiers( file ).value.all & to_unbounded_string( "0" ) & ASCII.NUL;
  -- 7. eof
end DoInitFileVariableFields;

procedure DoFileOpen( ref : in out reference;  mode : identifier; create : boolean;
  name : string ) is
  result : aFileDescriptor;
  flags  : anOpenFlag;
  fileOpenRec : unbounded_string;
begin
  if create then
     flags := O_CREAT;
  else
     flags := 0;
  end if;
  if mode = in_file_t then
     result := open( name & ASCII.NUL, flags+O_RDONLY, 8#644# );
  elsif mode = out_file_t then
     result := open( name & ASCII.NUL, flags+O_WRONLY+O_TRUNC, 8#644# );
  elsif mode = append_file_t then
     result := open( name & ASCII.NUL, flags+O_WRONLY+O_APPEND, 8#644# );
  else
     err( Gnat.Source_Info.Source_Location & ": internal error: unexpected file mode" );
  end if;
  if result < 0 then
     err( "Unable to open file: " & OSerror( C_errno ) );
  elsif not error_found then
     -- construct the file variable's value, a series of nul delimited fields
     fileOpenRec := to_unbounded_string( "." & ASCII.NUL );
     -- 1. character buffer
     fileOpenRec := fileOpenRec & to_unbounded_string( result'img ) & ASCII.NUL;
     -- 2. file descriptor
     fileOpenRec := fileOpenRec & to_unbounded_string( " 0" ) & ASCII.NUL;
     -- 3. lines
     --if mode = in_file_t then
     --   identifiers( file ).value.all := identifiers( file ).value.all & to_unbounded_string(
     --      isEOF( result )'img ) & ASCII.NUL;
     --else
        fileOpenRec := fileOpenRec & to_unbounded_string( "0" ) & ASCII.NUL;
     --end if;
     -- 4. eol flag
     fileOpenRec := fileOpenRec & name & ASCII.NUL;
     -- 5. name
     fileOpenRec := fileOpenRec & to_unbounded_string( mode'img ) & ASCII.NUL;
     -- 6. mode
        fileOpenRec := fileOpenRec & to_unbounded_string( "0" ) & ASCII.NUL;
     -- 7. eof
     --end if;
     AssignParameter( ref, fileOpenRec );
     if mode = in_file_t then
        DoGet( ref ); -- buffer first character, set eof if none
     end if;
     if trace then
        put_trace( to_string( identifiers( ref.id ).name ) &
          " is file descriptor" & result'img );
     end if;
  end if;
end DoFileOpen;

procedure DoSocketOpen( file_ref : in out reference; name : unbounded_string ) is
  result : aSocketFD;
  --flags  : anOpenFlag;
  host   : unbounded_string;
  port   : integer;
  pos    : natural;
  fileInfo : unbounded_string;
begin
  pos := index( name, ":" );
  if pos = 0 then
     host := name;
     port := 80;
  else
     begin
       host := to_unbounded_string( slice( name, 1, pos-1 ) );
     exception when others =>
       err( "unable to interpret TCP/IP host" );
     end;
     begin
       port := integer'value( " " & slice( name, pos+1, length( name ) ) );
     exception when others =>
       err( "unable to interpret TCP/IP port" );
     end;
     if port = 19 or port = 25 or port > 32767 then
        err( "access to this TCP/IP port is prohibited" );
     end if;
  end if;
  if error_found then
     return;
  end if;
  result := openSocket( host, port );
  if result < 0 then
     err( "Unable to socket: " & OSerror( C_errno ) );
  elsif not error_found then
     -- construct the file variable's value, a series of nul delimited fields
     fileInfo := to_unbounded_string( " " & ASCII.NUL );
     -- 1. character buffer
     fileInfo := fileInfo & to_unbounded_string( result'img ) & ASCII.NUL;
     -- 2. file descriptor
     fileInfo := fileInfo & to_unbounded_string( " 0" ) & ASCII.NUL;
     -- 3. lines
     --if mode = in_file_t then
     --   identifiers( file ).value.all := identifiers( file ).value.all & to_unbounded_string(
     --      isEOF( result )'img ) & ASCII.NUL;
     --else
        fileInfo := fileInfo & to_unbounded_string( "0" ) & ASCII.NUL;
     --end if;
     -- 4. eol flag
     fileInfo := fileInfo & name & ASCII.NUL;
     -- 5. name
        fileInfo := fileInfo & to_unbounded_string( "1" ) & ASCII.NUL;
     --end if;
     -- 6. doGet flag
     fileInfo := fileInfo & to_unbounded_string( "0" ) & ASCII.NUL;
     --end if;
     -- 7. eof
     AssignParameter( file_ref, fileInfo );

     -- a socket cannot do an initial "DoGet" because we don't know if the user
     -- will be reading or writing first.  DoGet could cause a hang as the server
     -- is waiting for an instruction and the script is waiting for a response
     -- from the server.  As a result, we use a "DoGet" flag.  If DoGet is true,
     -- eof_field and ch_field are not valid until an initial DoGet is done.
     if trace then
        put_trace( to_string( identifiers( file_ref.id ).name ) &
          " is file descriptor" & result'img );
     end if;
  end if;
end DoSocketOpen;

procedure ParseOpen( create : boolean := false ) is
  -- Syntax: open( closed-file, mode, name );
  -- Syntax: create( closed-file [,mode] [,name] );
  -- Source: Ada.Text_IO.Open
  -- Source: Ada.Text_IO.Create
  file_ref : reference;
  mode : identifier;
  name : unbounded_string;
  kind : identifier;
  exprVal  : unbounded_string;
  exprType : identifier;
begin
  if create then
     if rshOpt then
        err( "create not allowed in a " & optional_bold( "restricted shell" ) );
     end if;
     expect( create_t );
     expect( symbol_t, "(" );
     ParseClosedFile( file_ref );
     kind := file_type_t;
     -- the mode is optional, default to out_file
     if token = symbol_t and identifiers( token ).value.all = ")" then
        mode := out_file_t;
     else
        expect( symbol_t, "," );
        ParseIdentifier( mode );
        if baseTypesOk( identifiers( mode ).kind, file_mode_t ) then
           if create and mode = in_file_t then
              err( "cannot create an in_file" );
           end if;
        end if;
     end if;
     -- the name is optional, default to a temp file name
     if token = symbol_t and identifiers( token ).value.all = ")" then
        makeTempFile( name );
     else
        expect( symbol_t, "," );
        ParseExpression( exprVal, exprType );
        if uniTypesOk( exprType, uni_string_t ) then
           name := exprVal;
           if length( name ) = 0 and then not syntax_check then
              err( "pathname should not be null" );
           end if;
        end if;
     end if;
     expect( symbol_t, ")" );
  else
     expect( open_t );
     expect( symbol_t, "(" );
     ParseClosedFileOrSocket( file_ref, kind );
     if kind = file_type_t then
        expect( symbol_t, "," );
        ParseIdentifier( mode );
        if baseTypesOk( identifiers( mode ).kind, file_mode_t ) then
           if boolean(rshOpt) and mode = out_file_t then
              err( "out_file mode not allowed in a " & optional_bold( "restricted shell" ) );
           end if;
        end if;
     end if;
     if not error_found then
        -- not error_found because file must be legit in here
        expect( symbol_t, "," );
        -- At this point, it may be the second or third parameter, depending on
        -- whether or not it is a socket.  The third parameter is an expression
        -- and the expression could contain keywords like $1.
        if kind = socket_type_t then
           -- now host + optional port in name.  host may contain keywords like $1
           if identifiers( token ).kind /= keyword_t then
              if getBaseType( identifiers( token ).kind ) = file_mode_t then
                 err( "sockets don't have a mode" );
               end if;
           end if;
           ParseExpression( exprVal, exprType );
           if uniTypesOk( exprType, uni_string_t ) then
              name := exprVal;
              if length( name ) = 0 and then not syntax_check then
                 err( "hostname should not be null" );
              end if;
              expect( symbol_t, ")" );
           end if;
        else
           ParseExpression( exprVal, exprType );
           if uniTypesOk( exprType, uni_string_t ) then
              name := exprVal;
              if length( name ) = 0 and then not syntax_check then
                 err( "pathname should not be null" );
              end if;
              expect( symbol_t, ")" );
           end if;
        end if;
     end if; -- if mode and file OK
  end if; -- is open
  -- do it
  if isExecutingCommand then -- should use umask for permissions
     if kind = file_type_t then
        DoFileOpen( file_ref, mode, create, to_string( name ) );
     else
        DoSocketOpen( file_ref, name );
     end if;
  end if;
end ParseOpen;

procedure ParseReset is
  -- Syntax: reset( open-file [,mode] )
  -- Source: Ada.Text_IO.Reset
  file_ref: reference;
  mode    : identifier := eof_t;
  name    : unbounded_string;
  modestr : unbounded_string;
  fd      : aFileDescriptor;
  closeResult : int;
begin
  expect( reset_t );
  expect( symbol_t, "(" );
  ParseOpenFile( file_ref );
  if token = symbol_t and identifiers( token ).value.all = "," then
     expect( symbol_t, "," );
     if baseTypesOk( identifiers( token ).kind, file_mode_t ) then
        mode := token;
        getNextToken;
        if boolean(rshOpt) and mode = out_file_t then
           err( "out_file mode not allowed in a " & optional_bold( "restricted shell" ) );
        end if;
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
     name := stringField( file_ref, name_field );
     if mode = eof_t then
        modestr := stringField( file_ref, mode_field );
        if to_string( modestr ) = in_file_t'img then
           mode := in_file_t;
        elsif to_string( modestr ) = out_file_t'img then
           mode := out_file_t;
        elsif to_string( modestr ) = append_file_t'img then
           mode := append_file_t;
        else
           err( Gnat.Source_Info.Source_Location & ": internal error: unable to determine file mode " &
             to_string( modestr ) );
        end if;
     end if;
<<retry>> closeResult := close( fd );
     if closeResult < 0 then
        if C_errno = EINTR then
           goto retry;
        end if;
     end if;
     DoFileOpen( file_ref, mode, false, to_string( name ) );
  end if;
end ParseReset;

procedure ParseClose is
  -- Syntax: close( open-file )
  -- Source: Ada.Text_IO.Close
  file_ref : reference;
  fd   : aFileDescriptor;
  kind : identifier;
  closeResult : int;
begin
  expect( close_t );
  expect( symbol_t, "(" );
  ParseOpenFileOrSocket( file_ref, kind );
  expect( symbol_t, ")" );
  if isExecutingCommand then
     fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
     if fd = currentStandardInput then
        err( "this file is the current input file" );
     elsif fd = currentStandardInput then
        err( "this file is the current output file" );
     elsif fd = currentStandardInput then
        err( "this file is the current error file" );
     else
<<retry>>
        closeResult := close( fd );
        if closeResult < 0 then
           if C_errno = EINTR then
              goto retry;
           end if;
        end if;
        if trace then
           put_trace( "Closed file descriptor" & to_string( stringField( file_ref, fd_field ) ) );
        end if;
        identifiers( file_ref.id ).value.all := null_unbounded_string;
     end if;
  end if;
end ParseClose;

procedure ParseDelete is
  -- Syntax: delete( open-file )
  -- Source: Ada.Text_IO.Delete
  file_ref : reference;
  name : unbounded_string;
  fd   : aFileDescriptor;
  result : integer;
  closeResult : int;
begin
  expect( delete_t );
  expect( symbol_t, "(" );
  ParseOpenFile( file_ref );
  expect( symbol_t, ")" );
  if rshOpt then
     err( "delete is not allowed in a " & optional_bold( "restricted shell" ) );
  end if;
  if isExecutingCommand then
     fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
     if fd = currentStandardInput then
        err( "this file is the current input file" );
     elsif fd = currentStandardInput then
        err( "this file is the current output file" );
     elsif fd = currentStandardInput then
        err( "this file is the current error file" );
     else
        name := stringField( file_ref, name_field );
<<retry>> closeResult := close( fd );
        if closeResult < 0 then
           if C_errno = EINTR then
              goto retry;
           end if;
        end if;
        identifiers( file_ref.id ).value.all := null_unbounded_string;
        result := integer( unlink( to_string( name ) & ASCII.NUL ) );
        if result /= 0 then
           err( "unable to delete file: " & OSerror( C_errno ) );
        end if;
        if trace then
           put_trace( "delete file " & to_string( name ) );
        end if;
     end if;
  end if;
end ParseDelete;

procedure ParseSkipLine is
  -- Syntax: skip_line [ (open-file) ]
  -- Source: Ada.Text_IO.Skip_Line
  file_ref : reference;
  --fd     : aFileDescriptor;
  ch     : character;
  result : size_t;
  kind   : identifier;
  str    : unbounded_string;
begin
  file_ref.id := eof_t;
  expect( skip_line_t );
  --fd := stdin;
  if token = symbol_t and then identifiers( Token ).value.all = "(" then
      getNextToken;
      ParseOpenFileOrSocket( file_ref, kind );
      expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     --if file /= eof_t then
     --   fd := aFileDescriptor'value( to_string( stringField( file, fd_field ) ) );
     --end if;

---
     if trace then
        put_trace( "Input from file descriptor" & to_string( stringField( file_ref, fd_field ) ) );
     end if;
     if file_ref.id /= eof_t then
        if kind = socket_type_t and then stringField( file_ref, doget_field ) = "1" then
           DoGet( file_ref );
           replaceField( file_ref, doget_field, "0" );
        end if;
       loop
         ch := Element( identifiers( file_ref.id ).value.all, 1 );
         if stringField(file_ref, eof_field ) = "1" then
             err( "end of file" );
             exit;
         end if;
         DoGet( file_ref );
         exit when ch = ASCII.LF or error_found;
         str := str & ch;
       end loop;
    else
      -- stdin (I don't like this)
     loop
        readchar( result, stdin, ch, 1 );
 -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
        if result < 0 or result = size_t'last then
           if C_errno /= EAGAIN and C_errno /= EINTR then
              err( "unable to read file:" & OSerror( C_errno ) );
              exit;
           end if;
        elsif result = 0 then
           err( "skipped past the end of the file" );
           exit;
        else
           if ch = ASCII.LF then
              exit;
           end if;
        end if;
        str := str & ch;
      end loop;
    end if;
    if trace then
       Put_Trace( "Skipped '" & to_string( str ) & "'" );
    end if;
  end if;
end ParseSkipLine;

procedure ParseGet is
  -- Syntax: get [ (open-file), ch ]
  -- Source: Ada.Text_IO.Get
  -- Note: Gnat get can't be used here because it does something
  -- odd with file descriptor 0
  file_ref  : reference;
  kind      : identifier;
  fd        : aFileDescriptor;
  ch        : character;
  id_ref    : reference;
  result    : size_t;
  fileInfo  : unbounded_string;
begin
  file_ref.id := eof_t;
  expect( get_t );
  fd := stdin;
  expect( symbol_t, "(" );
  if identifiers( token ).kind /= keyword_t then
     ParseOpenFileOrSocket( file_ref, kind );
     expect( symbol_t, "," );
  else
     file_ref.id := standard_input_t;
  end if;
  ParseOutParameter( id_ref, character_t );
  if baseTypesOk( id_ref.kind, character_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     if trace then
        put_trace( "Using file descriptor " & to_string( stringField( file_ref, fd_field ) ) );
     end if;
     if file_ref.id /= eof_t then
        GetParameterValue( file_ref, fileInfo );
        if kind = socket_type_t and then stringField( fileInfo, recSep, doget_field ) = "1" then
           -- First get must update the 1 char look-ahead in the file_info
           DoGet( file_ref );
           GetParameterValue( file_ref, fileInfo );
           replaceField( fileInfo, recSep, doget_field, boolean'image(false));
        end if;
        if stringField( fileInfo, recSep, eof_field ) = "1" then
           err( "end of file" );
        else
           ch := Element( fileInfo, 1 );
           AssignParameter( id_ref, to_unbounded_string( "" & ch ) );
           AssignParameter( file_ref, fileInfo );
           DoGet( file_ref );
        end if;
     else
        fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
   <<reread>> readchar( result, fd, ch, 1 );
 -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
         if result < 0 or result = size_t'last then
              if C_errno = EAGAIN  or C_errno = EINTR then
                 goto reread;
              end if;
              err( "unable to read file:" & OSerror( C_errno ) );
         elsif result = 0 then
            err( "end of file" );
         else
            AssignParameter( id_ref, to_unbounded_string( "" & ch ) );
         end if;
      end if;
      if ch = ASCII.LF then -- not stdin (or error)?
         replaceField( file_ref, line_field,
            long_integer'image( long_integer'value(
            to_string( stringField( file_ref, line_field ) ) ) + 1 ) );
         replaceField( file_ref, eol_field, "1" );
      else
         replaceField( file_ref, eol_field, "0" );
      end if;
  end if;
end ParseGet;

procedure ParsePutLine is
  -- Syntax: put_line( [file,] expression )
  -- Source: Ada.Text_IO.Put_Line
  target_ref: reference;
  kind      : identifier := file_type_t;
  stderr    : boolean := false;
  expr_val  : unbounded_string;
  expr_type : identifier;
  result    : size_t;
  ch        : character;
  fd        : aFileDescriptor;
  retry     : boolean;
begin
  target_ref.index := 0;
  expect( put_line_t );
  expect( symbol_t, "(" );
  if identifiers( token ).kind /= keyword_t then
     kind := getUniType( token );
     if kind = file_type_t then
        ParseOpenFile( target_ref );
        if isExecutingCommand then
           if to_string( stringField(target_ref, mode_field)) = in_file_t'img then
              err( "This is a in_mode file" );
           end if;
        end if;
        expect( symbol_t, "," );
     elsif kind = socket_type_t then
        ParseOpenSocket( target_ref );
        expect( symbol_t, "," );
     else
        target_ref.id := standard_output_t;
     end if;
  else
     target_ref.id := standard_output_t;
  end if;
  ParseExpression( expr_val, expr_type );
  -- this sould be moved to an image function
  if getUniType( expr_type ) = root_enumerated_t then
     findEnumImage( expr_val, expr_type, expr_val );
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     if target_ref.id = standard_error_t then
        -- Ada doesn't handle interrupted system calls properly.
        -- maybe a more elegant way to do this...
        loop
          retry := false;
          begin
            Put_Line( standard_error, expr_val );
          exception when msg: device_error =>
            if exception_message( msg ) = "interrupted system call" then
               retry := true;
            else
               err( exception_message( msg ) );
            end if;
          end;
        exit when not retry;
        end loop;
     elsif target_ref.id = standard_output_t then
        -- Ada doesn't handle interrupted system calls properly.
        -- maybe a more elegant way to do this...
        loop
          retry := false;
          begin
            Put_Line( expr_val );
          exception when msg: device_error =>
            if exception_message( msg ) = "interrupted system call" then
               retry := true;
            else
               err( exception_message( msg ) );
            end if;
          end;
        exit when not retry;
        end loop;
        last_output := expr_val;
        last_output_type := expr_type;
     else
        fd := aFileDescriptor'value( to_string( stringField( target_ref, fd_field ) ) );
        for i in 1..length( expr_val ) loop
            ch := Element( expr_val, i );
<<rewrite>> writechar( result, fd, ch, 1 );
            if result < 0 then
               if C_errno = EAGAIN or C_errno = EINTR then
                  goto rewrite;
               end if;
               err( "unable to write: " & OSerror( C_errno ) );
               exit;
            end if;
        end loop;
        ch := ASCII.LF;
<<rewrite2>>
        writechar( result, fd, ch, 1 ); -- add a line feed
        if result < 0 then
           if C_errno = EAGAIN or C_errno = EINTR then
              goto rewrite2;
           end if;
           err( "unable to write: " & OSerror( C_errno ) );
         else
           replaceField( target_ref, line_field,
              long_integer'image( long_integer'value(
                 to_string( stringField( target_ref, line_field ) ) ) + 1 ) );
        end if;
     end if;
  end if;
end ParsePutLine;

procedure ParseQuestion is
  -- Syntax: "?" expression
  -- Source: SparForte built-in
  expr_val  : unbounded_string;
  expr_type : identifier;
  uni_type : identifier;
  retry     : boolean;
begin
  expect( symbol_t );
  if onlyAda95 then
      err( optional_bold( "pragma ada_95" ) & " doesn't allow ?" );
      return;
  end if;
  ParseExpression( expr_val, expr_type );
  -- If an error occurred when parsing the expression, the expression and
  -- type may not be valid.  There's nothing to print.
  if error_found then
     null;
  elsif getUniType( expr_type ) = root_enumerated_t then
     -- this will work during the syntax check but we don't need it
     if isExecutingCommand then
        findEnumImage( expr_val, expr_type, expr_val );
     end if;
  -- pretty formating for ? and time values
  elsif getBaseType( expr_type ) = cal_time_t then
     declare
        year    : year_number;
        month   : month_number;
        day     : day_number;
        seconds : day_duration;
        hours   : day_duration;
        minutes : day_duration;

        function get_2_digits( s : string ) return unbounded_string is
           tempStr : unbounded_string;
           dotPos  : natural;
        begin
           tempStr := to_unbounded_string( s );
           delete( tempStr, 1, 1 );
           dotPos := index( tempStr, "." );
           if dotPos > 0 then
              tempStr := head( tempStr, dotPos-1 );
           end if;
           if length( tempStr ) < 2 then
              tempStr := "0" & tempStr;
           end if;
           return tempStr;
        end get_2_digits;

        function get_4_digits( s : string ) return unbounded_string is
           tempStr : unbounded_string;
           dotPos  : natural;
        begin
           tempStr := to_unbounded_string( s );
           delete( tempStr, 1, 1 );
           dotPos := index( tempStr, "." );
           if dotPos > 0 then
              tempStr := head( tempStr, dotPos-1 );
           end if;
           if length( tempStr ) < 4 then
              tempStr := "0" & tempStr;
           end if;
           return tempStr;
        end get_4_digits;

        function drop_leading_space( s : string ) return unbounded_string is
           tempStr : unbounded_string;
        begin
           tempStr := to_unbounded_string( s );
           delete( tempStr, 1, 1 );
           return tempStr;
        end drop_leading_space;

     begin
        if isExecutingCommand then
           Split( time( to_numeric( expr_val ) ), year, month, day, seconds );

           hours := duration( float'truncation( float( seconds / (60 * 60) ) ) );
           seconds := seconds - hours * (60* 60);
           minutes := duration( float'truncation( float( seconds / ( 60 ) ) ) );
           seconds := seconds - (minutes * 60);
           expr_val := get_4_digits( year'img ) &  "/";
           expr_val := expr_val & get_2_digits( month'img ) &  "/";
           expr_val := expr_val & get_2_digits( day'img ) & " ";
           expr_val := expr_val & get_2_digits( hours'img ) & ":";
           expr_val := expr_val & get_2_digits( minutes'img ) & ":";
           expr_val := expr_val & drop_leading_space( seconds'img );
        end if;
     end;
  end if;
  if isExecutingCommand then -- fix this for no output on error!
     -- this sould be moved to an image function
     uni_type := getUniType( expr_type );
     if uni_type = root_enumerated_t then
        findEnumImage( expr_val, expr_type, expr_val );
     elsif uni_type = uni_numeric_t then
        -- For universal numeric, represent it as an integer string if possible
        -- to make it human-readable.
        declare
           val : long_float; -- := to_numeric( expr_val );
        begin
           val := to_numeric( expr_val );
           -- Within the range of a SparForte integer and no decimal (e.g.
           -- casting results in the same value?
           if long_float( val ) >= long_float( integerOutputType'first+0.9 ) and
              long_float( val ) <= maxInteger then
              if val = long_float'floor( val ) then
                 expr_val := to_unbounded_string( val );
              end if;
           end if;
        exception when ada.strings.index_error =>
           -- since this is an expression, this could be something else but
           -- it is almost always this
           err( "numeric variable has no value" );
        when constraint_error =>
           err( "constraint_error in question command - value " &
             to_string( toEscaped( expr_val ) ) &
             " may not be numeric" );
        when others =>
           err_exception_raised;
        end;
     elsif uni_type = root_record_t then
        err( "full records cannot be printed with ?.  Try env");
     end if;
     -- If an just error occurred, don't print anything further.
     if not error_found then
        -- Ada doesn't handle interrupted system calls properly.
        -- maybe a more elegant way to do this...
        loop
           retry := false;
           begin
             Put_Line( expr_val );
           exception when msg: device_error =>
             if exception_message( msg ) = "interrupted system call" then
                retry := true;
             else
                err( exception_message( msg ) );
             end if;
           end;
        exit when not retry;
        end loop;
     end if;
     last_output := expr_val;
     last_output_type := expr_type;
     replaceField( standard_output_t, line_field,
        long_integer'image( long_integer'value(
        to_string( stringField( standard_output_t, line_field ) ) ) + 1 ) );
  end if;
end ParseQuestion;

procedure ParsePut is
  -- Syntax: put( [open-file,] expression [, picture] )
  -- Source: Ada.Text_IO.Editing.Put
  target_ref: reference;
  kind      : identifier;
  stderr    : boolean := false;
  expr_val  : unbounded_string;
  expr_type : identifier;
  result    : size_t;
  ch        : character;
  fd        : aFileDescriptor;
  pic       : Picture;
  pic_val   : unbounded_string;
  pic_type  : identifier;
  retry     : boolean;
  temp      : unbounded_string;
begin
  expect( put_t );
  expect( symbol_t, "(" );
  if identifiers( token ).kind /= keyword_t then
     kind := getUniType( token );
     if kind = file_type_t then
        ParseOpenFile( target_ref );
        if isExecutingCommand then
           if to_string( stringField(target_ref, mode_field)) = in_file_t'img then
              err( "This is a in_mode file" );
           end if;
        end if;
        expect( symbol_t, "," );
     elsif kind = socket_type_t then
        ParseOpenSocket( target_ref );
        expect( symbol_t, "," );
     else
        target_ref.id := standard_output_t;
     end if;
  else
     target_ref.id := standard_output_t;
  end if;
  ParseExpression( expr_val, expr_type );
  if getUniType( expr_type ) = root_enumerated_t then
     -- newer versions of GCC Ada do not like two expr_val params
     findEnumImage( expr_val, expr_type, temp );
     expr_val := temp;
  end if;
  -- apply optional numeric formatting
  if token = symbol_t and identifiers( token ).value.all = "," then
     expect( symbol_t, "," );
     ParseExpression( pic_val, pic_type );
     if getUniType( pic_type ) /= uni_string_t then
        err( "number format picture string expected" );
     elsif not valid( to_string( pic_val ) ) then
        err( "number not a valid format picture" );
     elsif getUniType( expr_type ) /= uni_numeric_t then
        err( "only numeric types can use a format picture" );
     else
        if isExecutingCommand then
           pic := to_picture( to_string( pic_val ) );
           begin
              expr_val := to_unbounded_string( image( decimal_output_type( to_numeric( expr_val ) ), pic ) );
           exception when LAYOUT_ERROR =>
              err( "incorrect image picture " & ASCII.Quotation & to_string( toEscaped( pic_val ) ) & ASCII.Quotation );
           end;
        end if;
        last_output_type := uni_string_t;
     end if;
  else
     last_output_type := expr_type;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     if target_ref.id = standard_error_t then
        -- Ada doesn't handle interrupted system calls properly.
        -- maybe a more elegant way to do this...
        loop
           retry := false;
           begin
             Put( standard_error, expr_val );
           exception when msg: device_error =>
             if exception_message( msg ) = "interrupted system call" then
                retry := true;
             else
                err( exception_message( msg ) );
             end if;
           end;
        exit when not retry;
        end loop;
     elsif target_ref.id = standard_output_t then
        -- Ada doesn't handle interrupted system calls properly.
        -- maybe a more elegant way to do this...
        loop
           retry := false;
           begin
             Put( expr_val );
           exception when msg: device_error =>
             if exception_message( msg ) = "interrupted system call" then
                retry := true;
             else
                err( exception_message( msg ) );
             end if;
           end;
        exit when not retry;
        end loop;
        last_output := expr_val;
     else
        fd := aFileDescriptor'value( to_string( stringField( target_ref, fd_field ) ) );
        for i in 1..length( expr_val ) loop
            ch := Element( expr_val, i );
            writechar( result, fd, ch, 1 );
<<rewrite>> if result < 0 then
               if C_errno = EAGAIN or C_errno = EINTR then
                  goto rewrite;
               end if;
               err( "unable to write: " & OSerror( C_errno ) );
               exit;
            end if;
        end loop;
     end if;
  end if;
end ParsePut;

procedure ParseNewLine is
  -- Syntax: new_line
  -- Source: Ada.Text_IO.New_Line
  target_ref : reference;
  kind   : identifier;
  fd     : aFileDescriptor;         -- Linux file descriptor of output file
  ch     : character;
  result : size_t;
begin
  expect( new_line_t );
  if token = symbol_t and identifiers( token ).value.all = "(" then
     expect( symbol_t, "(" );
     ParseOpenFileOrSocket( target_ref, kind );
     expect( symbol_t, ")" );
  else
     target_ref.id := standard_output_t;
  end if;
  if isExecutingCommand then
     if target_ref.id = standard_error_t then
        New_Line( standard_error );
     elsif target_ref.id = standard_output_t then
        New_Line;
     else
        fd := aFileDescriptor'value( to_string( stringField( target_ref, fd_field ) ) );
        ch := ASCII.LF;
<<rewrite>> writechar( result, fd, ch, 1 );
        if result < 0 then
           if C_errno = EAGAIN or C_errno = EINTR then
              goto rewrite;
           end if;
           err( "unable to write: " & OSerror( C_errno ) );
        end if;
     end if;
  end if;
end ParseNewLine;

procedure ParseSetInput is
  -- Syntax: set_input( open-file )
  -- Source: Ada.Text_IO.Set_Input
  file_ref: reference;              -- open file to assign output to
  fd     : aFileDescriptor;         -- Linux file descriptor of output file
  result : aFileDescriptor := 0;    -- result of dup2
begin
  expect( set_input_t );
  expect( symbol_t, "(" );
  ParseOpenFile( file_ref );
  if file_ref.id = standard_output_t then
     err( optional_bold( "standard_output" ) & " cannot be assigned for " &
          optional_bold( "input" ) );
  elsif file_ref.id = standard_error_t then
     err( optional_bold( "standard_error" ) & " cannot be assigned for " &
          optional_bold( "input" ) );
  elsif not syntax_check then
    if to_string(stringField(file_ref, mode_field)) /= in_file_t'img then
       err( "not an in_file file" );
    end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
     result := dup2( fd, stdin );
     if result < 0 then
        err( "unable to set input: " & OSerror( C_errno ) );
     elsif not error_found then
        currentStandardInput := fd;
        identifiers( current_input_t ).value.all :=
          to_unbounded_string( file_ref.id'img );
        if trace then
           put_trace( "input is currently from file descriptor" &
             currentStandardInput'img );
        end if;
     end if;
  end if;
end ParseSetInput;

procedure ParseSetOutput is
  -- Syntax: set_output( open-file )
  -- Source: Ada.Text_IO.Set_Output
  file_ref : reference;             -- open file to assign output to
  fd       : aFileDescriptor;         -- Linux file descriptor of output file
  result   : aFileDescriptor := 0;    -- result of dup2
begin
  expect( set_output_t );
  expect( symbol_t, "(" );
  ParseOpenFile( file_ref );
  if file_ref.id = standard_input_t then
     err( optional_bold( "standard_input" ) & " cannot be assigned for " &
          optional_bold( "output" ) );
  elsif not syntax_check then
     if to_string(stringField(file_ref.id, mode_field)) = in_file_t'img then
        err( "not an out_file or append_file file" );
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     fd := aFileDescriptor'value( to_string( stringField( file_ref.id, fd_field ) ) );
     result := dup2( fd, stdout );
     if result < 0 then
        err( "unable to set output: " & OSerror( C_errno ) );
     elsif not error_found then
        currentStandardOutput := fd;
        identifiers( current_output_t ).value.all :=
          to_unbounded_string( file_ref.id'img );
        if trace then
           put_trace( "output is currently to file descriptor" &
             currentStandardOutput'img );
        end if;
     end if;
  end if;
end ParseSetOutput;

procedure ParseSetError is
  -- Syntax: set_error( open-file )
  -- Source: Ada.Text_IO.Set_Error
  file_ref:reference;
  result : aFileDescriptor := 0;
  fd     : aFileDescriptor;
begin
  expect( set_error_t );
  expect( symbol_t, "(" );
  ParseOpenFile( file_ref );
  if file_ref.id = standard_input_t then
     err( optional_bold( "standard_input" ) & " cannot be assigned for " &
          optional_bold( " standard error" ) );
  elsif not syntax_check then
     if to_string(stringField(file_ref, mode_field)) = in_file_t'img then
        err( "not an out_file or append_file file" );
     end if;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
     result := dup2( fd, stderr );
     if result < 0 then
        err( "unable to set error: " & OSerror( C_errno ) );
     elsif not error_found then
        currentStandardError := fd;
        identifiers( current_error_t ).value.all :=
          to_unbounded_string( file_ref.id'img );
        if trace then
           put_trace( "error output is currently to file descriptor" &
             currentStandardError'img );
        end if;
     end if;
  end if;
end ParseSetError;

procedure ParseGetImmediate is
  -- Syntax: get_immediate( [file,] ch [, bool] )
  -- Source: Ada.Text_IO.Get_Immediate
  file_ref  : reference;
  kind      : identifier;
  fd        : aFileDescriptor;
  ch        : character;
  id_ref    : reference;
  avail_ref : reference;
  --result    : size_t;
  fileInfo  : unbounded_string;
  hasAvail  : boolean := false;
  baseType  : identifier;
begin
  file_ref.id := eof_t;
  expect( get_immediate_t );
  fd := stdin;
  expect( symbol_t, "(" );
  kind := identifiers( token ).kind;
  if kind /= new_t then
     baseType := getBaseType( kind );
     if baseType = file_type_t or baseType = socket_type_t then
        ParseOpenFileOrSocket( file_ref, kind );
        expect( symbol_t, "," );
     else
        -- default is standard input
        file_ref.id := standard_input_t;
     end if;
  else
     -- default is standard input
     file_ref.id := standard_input_t;
  end if;
  ParseOutParameter( id_ref, character_t );
  if token = symbol_t and identifiers( token ).value.all = "," then
     expect( symbol_t, "," );
     ParseOutParameter( avail_ref, boolean_t );
     hasAvail := true;
  end if;
  expect( symbol_t, ")" );
  if isExecutingCommand then
     -- TODO: integration with text_io functions.  Currently, it's ignoring
     -- TextIO's double-buffer queue.
     if file_ref.id /= eof_t then
        if isatty( fd ) /= 0 then
           if hasAvail then
              -- TODO: this can echo the keypress because echoing is only
              -- turned off when it tries to get a key.  So if a user presses
              -- a key before getKey runs, it will echo because getKey hasn't
              -- run yet.
              getKey( ch, nonblock => true );
              --if wasSIGINT then
              --   null;
              --end if;
              -- when non-blocking, Control-D indicates nothing read
              AssignParameter( avail_ref, to_bush_boolean( ch = ASCII.EOT ) );
           else
              getKey( ch );
              --if wasSIGINT then
              --   null;
              --end if;
           end if;
           AssignParameter( id_ref, to_unbounded_string( "" & ch ) );
        else
           err( "only implemented for a tty/terminal" );
        end if;
     else
        -- TODO: this is only doing the terminal/tty, not files/sockets.
        -- This will probably require a C select() to avoid blocking.
        err( "get_immediate for files are not yet implemented" );
     end if;
  end if;
end ParseGetImmediate;

procedure ParseLookAhead is
  -- Syntax: look_ahead( [file,] ch [, eol] )
  -- Source: Ada.Text_IO.Look_Ahead
  -- This is the equivalent of a Get that does not get the next character.
--   procedure Look_Ahead
--     (Item        : out Character;
--      End_Of_Line : out Boolean);
  file_ref  : reference;
  kind      : identifier;
  fd        : aFileDescriptor;
  ch        : character;
  id_ref    : reference;
  result    : size_t;
  fileInfo  : unbounded_string;
begin
  file_ref.id := eof_t;
  expect( look_ahead_t );
  fd := stdin;
  expect( symbol_t, "(" );
  if identifiers( token ).kind /= keyword_t then
     ParseOpenFileOrSocket( file_ref, kind );
     expect( symbol_t, "," );
  else
     file_ref.id := standard_input_t;
  end if;
  ParseOutParameter( id_ref, character_t );
  if baseTypesOk( id_ref.kind, character_t ) then
     expect( symbol_t, ")" );
  end if;
  -- TODO: second parameter
  if isExecutingCommand then
     if trace then
        put_trace( "Using file descriptor " & to_string( stringField( file_ref, fd_field ) ) );
     end if;
     -- reading from a file?
     if file_ref.id /= eof_t then
        GetParameterValue( file_ref, fileInfo );
        if kind = socket_type_t and then stringField( fileInfo, recSep, doget_field ) = "1" then
           -- First get must update the 1 char look-ahead in the file_info
           DoGet( file_ref );
           GetParameterValue( file_ref, fileInfo );
           replaceField( fileInfo, recSep, doget_field, boolean'image(false));
        end if;
        if stringField( fileInfo, recSep, eof_field ) = "1" then
           err( "end of file" );
        else
           ch := Element( fileInfo, 1 );
           AssignParameter( id_ref, to_unbounded_string( "" & ch ) );
           AssignParameter( file_ref, fileInfo );
           -- DoGet( file_ref );
        end if;
     else
        -- reading from default input
        -- TODO: why no buffering here?
        fd := aFileDescriptor'value( to_string( stringField( file_ref, fd_field ) ) );
   <<reread>> readchar( result, fd, ch, 1 );
 -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
         if result < 0 or result = size_t'last then
              if C_errno = EAGAIN  or C_errno = EINTR then
                 goto reread;
              end if;
              err( "unable to read file:" & OSerror( C_errno ) );
         elsif result = 0 then
            err( "end of file" );
         else
            AssignParameter( id_ref, to_unbounded_string( "" & ch ) );
         end if;
      end if;
      if ch = ASCII.LF then -- not stdin (or error)?
         replaceField( file_ref, line_field,
            long_integer'image( long_integer'value(
            to_string( stringField( file_ref, line_field ) ) ) + 1 ) );
         replaceField( file_ref, eol_field, "1" );
      else
         replaceField( file_ref, eol_field, "0" );
      end if;
  end if;
end ParseLookAhead;

procedure StartupTextIO is
begin
  -- Predefined Text_IO Types

  declareStandardConstant( in_file_t, "in_file", file_mode_t, "0" );
  declareStandardConstant( out_file_t, "out_file", file_mode_t, "1" );
  declareStandardConstant( append_file_t, "append_file", file_mode_t, "2" );

  -- Text I/O Identifiers

  declareProcedure( create_t, "create" );
  declareProcedure( open_t, "open" );
  declareProcedure( close_t, "close", ParseClose'access );
  declareProcedure( get_t, "get", ParseGet'access );
  declareFunction( get_line_t, "get_line", ParseGetLine'access );
  declareFunction( inkey_t, "inkey", ParseInkey'access );
  declareProcedure( put_t, "put", ParsePut'access );
  declareProcedure( put_line_t, "put_line", ParsePutLine'access );
  declareProcedure( new_line_t, "new_line", ParseNewLine'access );
  declareFunction( is_open_t, "is_open" );
  declareFunction( end_of_file_t, "end_of_file", ParseEndOfFile'access );
  declareFunction( end_of_line_t, "end_of_line", ParseEndOfLine'access );
  declareFunction( name_t, "name", ParseName'access );
  declareFunction( mode_t, "mode", ParseMode'access );
  declareProcedure( skip_line_t, "skip_line", ParseSkipLine'access );
  declareFunction( line_t, "line", ParseLine'access );
  declareProcedure( set_input_t, "set_input", ParseSetInput'access );
  declareProcedure( set_output_t, "set_output", ParseSetOutput'access );
  declareProcedure( set_error_t, "set_error", ParseSetError'access );
  declareProcedure( reset_t, "reset", ParseReset'access );
  declareProcedure( get_immediate_t, "get_immediate", ParseGetImmediate'access );
  -- Look ahead doesn't work yet
  -- declareProcedure( look_ahead_t, "look_ahead", ParseLookAhead'access );

  -- declareProcedure( delete_t, "delete" );
  -- delete declared in scanner since it's also a SQL command

  -- Other Text I/O Constants

  declareStandardConstant( standard_input_t, "standard_input", file_type_t,
    "<defined by parser>" );
  declareStandardConstant( standard_output_t, "standard_output", file_type_t,
    "<defined by parser>" );
  declareStandardConstant( standard_error_t, "standard_error", file_type_t,
    "<defined by parser>" );
  -- these should not be constants, but we don't have a function type yet
  declareStandardConstant( current_input_t, "current_input", file_type_t,
    "<defined by parser>" );
  declareStandardConstant( current_output_t, "current_output", file_type_t,
    "<defined by parser>" );
  declareStandardConstant( current_error_t, "current_error", file_type_t,
    "<defined by parser>" );
end StartupTextIO;

procedure ShutdownTextIO is
begin
  null;
end ShutdownTextIO;

end parser_tio;

