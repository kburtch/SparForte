------------------------------------------------------------------------------
-- Parser Aux (Parser Support)                                              --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2025 Free Software Foundation              --
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
-- TODO: break this up into more specific packages

with ada.text_io,
     Interfaces.C,
     gnat.source_info,
     pegasoft.strings,
     pegasoft.script_io,
     pegasoft.user_io,
     world.utf8,
     message_strings,
     compiler,
     scanner,
     scanner.communications,
     parser_params,
     parser.decl.as;
use  ada.text_io,
     Interfaces.C,
     pegasoft,
     pegasoft.strings,
     pegasoft.script_io,
     pegasoft.user_io,
     world.utf8,
     message_strings,
     compiler,
     scanner,
     scanner.communications,
     parser_params,
     parser.decl.as;


package body parser_aux is

use spar_os.HEptrs;


-----------------------------------------------------------------------------
--  MAKE TEMP FILE
--
-- Create a unique temporary filename
-----------------------------------------------------------------------------

procedure makeTempFile( s : out unbounded_string ) is
  LinuxPath : string := "/tmp/sparXXXXXX" & ASCII.NUL;
  result : aFileDescriptor;
  closeResult : int;
begin
  s := null_unbounded_string;
  mkstemp( result, LinuxPath );
  if result < 0 then
     err( contextNotes => +"in /tmp",
          subjectNotes => +"a temporary file",
          reason => +"could not be created because ",
          obstructorNotes => +"syscall mkstemp failed with " & em( OSError( C_errno ) ) );
     s := null_unbounded_string;
  else
<<retry1>> closeResult := close( result ); -- not the most secure
     if closeResult < 0 then
        if C_errno = EINTR then
           goto retry1;
        end if;
     end if;
     for i in aLinuxPath'range loop
         exit when LinuxPath( i ) = ASCII.NUL;
         s := s & LinuxPath( i );
     end loop;
  end if;
end makeTempFile;

--function isEOF( fd : aFileDescriptor ) return boolean is
-- true if the file descriptor is at the end of file
--  ch     : character;
--  result : long_integer;
--  seekResult : long_integer;
--begin
--  read( result, fd, ch, 1 );                 -- read a single character
--  if result > 0 then                         -- read something?
--     seekResult := lseek( fd, -1, WHENCE_SEEK_CUR); -- move back one character
--  end if;
--  return result < 1;                         -- return if read reported EOF
--end isEOF;


-----------------------------------------------------------------------------
--  STRING FIELD
--
-- same as string_util.stringField except uses an identifier's value
-----------------------------------------------------------------------------

function stringField( i : identifier; f : natural ) return unbounded_string is
begin
  return stringField( identifiers( i ).store.value, recSep, f );
end stringField;


-----------------------------------------------------------------------------
--  REPLACE FIELD
--
-- same as string_util.replaceField except users an identifier's value
-----------------------------------------------------------------------------

procedure replaceField( i : identifier; f : natural; field : string ) is
begin
  replaceField( identifiers( i ).store.value, recSep, f, field );
end replaceField;


-----------------------------------------------------------------------------
--  STRING FIELD
--
-- same as string_util.stringField except uses a reference
-----------------------------------------------------------------------------

function stringField( r : reference; f : natural ) return unbounded_string is
   tempStr : storage;
begin
  getParameterValue( r, tempStr );
  return stringField( tempStr.value, recSep, f );
end stringField;


-----------------------------------------------------------------------------
--  REPLACE FIELD
--
-- same as string_util.replaceField except users an identifier's value
-----------------------------------------------------------------------------

procedure replaceField( r : reference; f : natural; field : string ) is
   tempStr : storage;
begin
  getParameterValue( r, tempStr );
  replaceField( tempStr.value, recSep, f, field );
  assignParameter( r, tempStr );
end replaceField;


-----------------------------------------------------------------------------
--  OPEN SOCKET
--
-- Initialize a new TCP/IP socket
-----------------------------------------------------------------------------

function openSocket( serverName : unbounded_string; port : integer ) return aSocketFD is


  mySocket    : aSocketFD;     -- the socket
  myAddress   : aSocketAddr;   -- where it goes
  myServer    : aHEptr;        -- IP number of server
  myServerPtr : HEptrs.Object_Pointer;
  addrList    : addrListPtrs.Object_Pointer;
  Result      : int;

begin

  --Put_Line( "Initializing a TCP/IP socket" );
  --Put_Line( "Socket( " & PF_INET'img & ',' & SOCK_STREAM'img & ", 0 );" );

  mySocket := Socket( PF_INET, SOCK_STREAM, 0 );
  if mySocket = -1 then
     err( subjectNotes => +"a network socket",
          reason => +"could not be created because ",
          obstructorNotes => +"syscall Socket failed with " & em( OSError( C_errno ) ) );
     return -1;
  end if;
  --New_Line;

  -- Lookup the IP number for the server

  --Put_Line( "Looking for information on " & to_string( serverName ) );

  myServer := GetHostByName( to_string( serverName ) & ASCII.NUL );
  myServerPtr := HEptrs.To_Pointer( myServer );
  if myServerPtr = null then
     if C_errno = 0 then
        err( subjectNotes => +"a network socket",
             reason => +"could not be created because ",
             obstructorNotes => pl( "there is no host by the name " ) &
                em_value( serverName ) );
     else
        err( subjectNotes => +"a network socket",
             reason => +"could not be created because ",
             obstructorNotes => +"syscall GetHostByName failed with " & em( OSError( C_errno ) ) );
     end if;
     return -1;
  end if;

  --Put_Line( "IP number is" & myServerPtr.h_length'img & " bytes long" );
  addrList := addrlistPtrs.To_Pointer( myServerPtr.h_addr_list );
  --New_Line;

  -- Create the IP, port and protocol information

  --Put_Line( "Preparing connection destination information" );
  myAddress.family := AF_INET;
  myAddress.port   := htons( Interfaces.C.Unsigned_Short( port ) );
  memcpy( myAddress.ip'address, addrlist.all, myServerPtr.h_length );
  --New_Line;

  -- Open a connection to the server

  --Put_Line( "Connect( Result, Socket, Family/Address rec, F/A rec size )" );

<<retry1>> Connect( Result, mySocket, myAddress, myAddress'size/8 );

  --Put( "Connect( " & Result'img & "," );
  --Put(  myAddress.family'img & "/" );
  --PutIPNum( myAddress.ip );
  --Put(  "," & integer'image( myAddress'size / 8 ) & ")" );
  if Result /= 0 then
     if C_errno = EINTR then
        goto retry1;
     end if;
        err( subjectNotes => +"a network socket",
             reason => +"could not be connected because ",
             obstructorNotes => +"syscall connect failed with " & em( OSError( C_errno ) ) );
<<retry2>> Result := close( aFileDescriptor( mySocket ) );
     if Result < 0 then
        if C_errno = EINTR then
           goto retry2;
        end if;
     end if;
     return -1;
  end if;
  --New_Line;

  return mySocket;

end openSocket;


------------------------------------------------------------------------------
-- DO GET
--
-- Get the next character from a file or socket.  Save the character
-- in the ch_field field of the file record.  If there is no next
-- character, set the eof_field to true.  The caller is assumed to
-- check that the file is open.  There is no eof_field check.
--
--
-- Reasoning: UNIX/Linux has a terrible way to handle end-of-file:
-- you have to read one character too many and check to see if no
-- character was read.  As a result, Text_IO routines must always
-- be "double-buffered": they must read the character into a buffer,
-- and then the application must read the character from the buffer
-- to its final destination.  The end-of-file cannot be checked
-- without a read, and reading will cause characters to be lost if
-- they are not double-buffered.  But I didn't design it, did I?
------------------------------------------------------------------------------

procedure DoGet( ref : reference ) is
  fd     : aFileDescriptor;    -- file's file descriptor
  ch     : character := ASCII.NUL; -- a buffer to read the character into
  -- ASCII.NUL to suppress compiler warning
  eof    : boolean := false;   -- true if a character was read
  result : size_t;       -- bytes read by read
  fileInfo : storage;
begin
   GetParameterValue( ref, fileInfo );
   fd := aFileDescriptor'value( to_string( stringField( fileInfo.value, recSep, fd_field ) ) );
<<reread>> readchar( result, fd, ch, 1 );
 -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
     if result < 0 or result = size_t'last then
      if C_errno = EAGAIN  or C_errno = EINTR then
         goto reread;                   -- interrupted? try again
      end if;                           -- error? report it
      err( pl( "unable to read file:" & OSerror( C_errno ) ) );
      return;                           -- and bail out
   elsif result = 0 then                -- nothing read?
      eof := true;                      -- then it's the end of file
   end if;
   --if ref.id = current_output_t or      -- SHOULD NEVER BE TRUE BUT...
   -- ref.id = current_input_t or
   --   ref.id = current_error_t then
   --   err( pl( Gnat.Source_Info.Source_Location & ": Internal Error: DoGet was given a file alias not a real file" ) );
   --else
      if eof then                       -- eof? set eof_field
         replaceField( fileInfo.value, recSep, eof_field, "1" );
         replaceField( fileInfo.value, recSep, line_field, -- Ada counts EOF as a line!
            long_integer'image( long_integer'value(
            to_string( stringField( fileInfo.value, recSep, line_field ) ) ) + 1 ) );
      else                              -- else replace the character
         replace_Element( fileInfo.value, 1, ch ); -- save character in ch_field
         if ch = ASCII.LF then          -- a line? increment line_field
            replaceField( fileInfo.value, recSep, line_field,
               long_integer'image( long_integer'value(
               to_string( stringField( fileInfo.value, recSep, line_field ) ) ) + 1 ) );
            replaceField( fileInfo.value, recSep, eol_field, "1" ); -- and set eol_field
         else
            replaceField( fileInfo.value, recSep, eol_field, "0" ); -- else not
         end if;                        -- the end of the line
      end if;
   --end if;
   AssignParameter( ref, fileInfo );
end DoGet;


------------------------------------------------------------------------------
-- DO INIT FILE VARIABLE FIELDS
--
-- Create the fields in a new file variable
------------------------------------------------------------------------------

procedure DoInitFileVariableFields( file : identifier; fd : aFileDescriptor;
  name : string; mode : identifier  ) is
begin
  -- construct the file variable's value, a series of nul delimited fields
  identifiers( file ).store.value := to_unbounded_string( "." & ASCII.NUL );
  -- 1. character buffer
  identifiers( file ).store.value := identifiers( file ).store.value & to_unbounded_string(      fd'img ) & ASCII.NUL;
  -- 2. file descriptor
  identifiers( file ).store.value := identifiers( file ).store.value & to_unbounded_string(      " 0" ) & ASCII.NUL;
  -- 3. lines
  identifiers( file ).store.value := identifiers( file ).store.value & to_unbounded_string(      "0" ) & ASCII.NUL;
  -- 4. eol flag
  identifiers( file ).store.value := identifiers( file ).store.value & name & ASCII.NUL;
  -- 5. name
  identifiers( file ).store.value := identifiers( file ).store.value & to_unbounded_string(
     mode'img ) & ASCII.NUL;
  -- 6. mode
  identifiers( file ).store.value := identifiers( file ).store.value & to_unbounded_string( "0" ) & ASCII.NUL;
  -- 7. eof
end DoInitFileVariableFields;


------------------------------------------------------------------------------
-- DO FILE OPEN
--
-- Open a file for a variable of type file_type and set the file record's
-- data fields.
------------------------------------------------------------------------------

procedure DoFileOpen( ref : reference;  mode : identifier; create : boolean;
  name : string; fileMetaLabels : metaLabelHashedSet.Set ) is
  result : aFileDescriptor;
  flags  : anOpenFlag;
  fileOpenRec : storage;
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
     err( pl( Gnat.Source_Info.Source_Location & ": internal error: unexpected file mode" ) );
  end if;
  if result < 0 then
     err( pl( "Unable to open file: " & OSerror( C_errno ) ) );
  elsif not error_found then
     -- construct the file variable's value, a series of nul delimited fields
     fileOpenRec.value := to_unbounded_string( "." & ASCII.NUL );
     -- 1. character buffer
     fileOpenRec.value := fileOpenRec.value & to_unbounded_string( result'img ) & ASCII.NUL;
     -- 2. file descriptor
     fileOpenRec.value := fileOpenRec.value & to_unbounded_string( " 0" ) & ASCII.NUL;
     -- 3. lines
     fileOpenRec.value := fileOpenRec.value & to_unbounded_string( "0" ) & ASCII.NUL;
     -- 4. eol flag
     fileOpenRec.value := fileOpenRec.value & name & ASCII.NUL;
     -- 5. name
     fileOpenRec.value := fileOpenRec.value & to_unbounded_string( mode'img ) & ASCII.NUL;
     -- 6. mode
     fileOpenRec.value := fileOpenRec.value & to_unbounded_string( "0" ) & ASCII.NUL;
     -- 7. eof
     --end if;
     fileOpenRec.unitMetaLabel := noMetaLabel;
     fileOpenRec.policyMetaLabels := fileMetaLabels;
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


------------------------------------------------------------------------------
-- DO SOCKET OPEN
--
-- Open a file for a variable of type file_type and set the file record's
-- data fields.
------------------------------------------------------------------------------

procedure DoSocketOpen( file_ref : reference; name : unbounded_string; socketMetaLabels : metaLabelHashedSet.Set ) is
  result : aSocketFD;
  --flags  : anOpenFlag;
  host   : unbounded_string;
  port   : integer;
  pos    : natural;
  fileInfo : storage;
begin
  pos := index( name, ":" );
  if pos = 0 then
     host := name;
     port := 80;
  else
     begin
       host := to_unbounded_string( slice( name, 1, pos-1 ) );
     exception when others =>
       err( +"unable to interpret TCP/IP host" );
     end;
     begin
       port := integer'value( " " & slice( name, pos+1, length( name ) ) );
     exception when others =>
       err( +"unable to interpret TCP/IP port" );
     end;
     if port = 19 or port = 25 or port > 32767 then
        err( +"access to this TCP/IP port is prohibited" );
     end if;
  end if;
  if error_found then
     return;
  end if;
  result := openSocket( host, port );
  if result < 0 then
     err( pl( "Unable to socket: " & OSerror( C_errno ) ) );
  elsif not error_found then
     -- construct the file variable's value, a series of nul delimited fields
     fileInfo.value := to_unbounded_string( " " & ASCII.NUL );
     -- 1. character buffer
     fileInfo.value := fileInfo.value & to_unbounded_string( result'img ) & ASCII.NUL;
     -- 2. file descriptor
     fileInfo.value := fileInfo.value & to_unbounded_string( " 0" ) & ASCII.NUL;
     -- 3. lines
     --if mode = in_file_t then
     --   identifiers( file ).store.value := identifiers( file ).store.value & to_unbounded_string(
     --      isEOF( result )'img ) & ASCII.NUL;
     --else
        fileInfo.value := fileInfo.value & to_unbounded_string( "0" ) & ASCII.NUL;
     --end if;
     -- 4. eol flag
     fileInfo.value := fileInfo.value & name & ASCII.NUL;
     -- 5. name
        fileInfo.value := fileInfo.value & to_unbounded_string( "1" ) & ASCII.NUL;
     --end if;
     -- 6. doGet flag
     fileInfo.value := fileInfo.value & to_unbounded_string( "0" ) & ASCII.NUL;
     --end if;
     -- 7. eof
     fileInfo.policyMetaLabels := socketMetaLabels;
     fileInfo.unitMetaLabel := noMetaLabel;
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




------------------------------------------------------------------------------
--  PROCESS TEMPLATE
--
-- Read a template and process embedded scripts.  This procedure is expected
-- to be invoked after the main script has run.
-- Exceptions: Caller should handle STATUS_ERROR, NAME_ERROR, MODE_ERROR, etc.
------------------------------------------------------------------------------

procedure processTemplate is
   f : file_type;
   ch : character;
   type aMode is ( noMode,                          -- shouldn't be used
                   gatheringStartTag,               -- looking for a start tag
                   gatheringScript,                 -- gathering script
                   gatheringEndTag,                 -- looking for end tag
                   outputTemplate                   -- writing out template
                  );
   mode : aMode := outputTemplate;                  -- what are we doing now
   bushStartTag   : constant string := "<?spar";    -- the start tag
   bushEndTag     : constant string := "?>";        -- the end tag
   tag            : unbounded_string;               -- possible tag text
   tagCount       : natural := 0;                   -- characters in p. tag
   uncompressedScript : unbounded_string;           -- script being gathered
   -- lineno         : natural := 1;
   startingLineNo : natural := 1;
   -- save_lineno    : aLineNumber;                    -- swap space for line no
   lastLine : Positive_Count := 1;
begin
   -- no longer force GCC-style errors.  That's handled in the err
   -- function now.
   -- gccOpt := true;

   -- Switch to a restricted shell and disable external commands
   -- for normal templates.

   if not unrestrictedTemplate then
      rshOpt := true;
      restriction_no_external_commands := true;
   end if;

   -- Write the HTTP header. If already written, nothing happens.

   if isExecutingCommand then
      putTemplateHeader( templateHeader );
   end if;

   -- Open the template file

   scriptFilePath := templatePath;
   open( f, in_file, to_string( templatePath ) );   -- open the template
   setTemplateName;

   -- Read the template, interpret tags and execute commands

   while not end_of_file( f ) loop                  -- while text to do
      get( f, ch );                                 -- get next character

      ----------------------------------------------------------------------
      -- Text_IO Automatically skips lines.  This should be rewritten.
      ----------------------------------------------------------------------

      if line( f ) - lastLine > 1 then
         for i in 1..line( f ) - lastLine -1 loop
             uncompressedScript := uncompressedScript & ASCII.LF;
         end loop;
      end if;
      lastLine := line( f );

      ----------------------------------------------------------------------
      -- Depending on the mode, interpret the template character
      ----------------------------------------------------------------------

      case mode is                                  -- are we searching for

      ----------------------------------------------------------------------
      -- Examining a possible script start tag?
      ----------------------------------------------------------------------

      when gatheringStartTag =>                     -- possible <?spar ?
-- put_line( "gst: read '" & ch & " at" & lineno'img & "', tag = '" & to_string( tag ) & "', tagCount = " & tagCount'img );
         tag := tag & ch;                           -- record in case not
         tagCount := tagCount + 1;                  -- count characters
         if tagCount < bushStartTag'length then     -- subset of tag?
            if ch /= bushStartTag(tagCount) then    -- match failed?
               mode := outputTemplate;              -- not a tag
               tagCount := 0;                       -- forget it
               if last_status = 0 then              -- we are outputting?
                  put_retry( to_string( tag ) );    -- output template text
               end if;
            end if;
            if end_of_line( f ) then                -- EOL?
              -- lineno := lineno + 1;              -- template line
              if last_status = 0 then               -- we are outputting?
                 new_line_retry;                    -- output a line feed
              end if;
            end if;
         elsif ch = bushStartTag( tagCount ) then   -- but if last matches
            mode := gatheringScript;                -- look for a script
            tagCount := 0;                          -- forget tag
            startingLineNo := integer( line( f ) );
         else
            mode := outputTemplate;                 -- not a tag
            tagCount := 0;                          -- forget it
            if last_status = 0 then                 -- we are outputting?
               put_retry( to_string( tag ) );       -- output template text
            end if;
            if end_of_line( f ) then                -- EOL?
              -- lineno := lineno + 1;              -- template line
              if last_status = 0 then               -- we are outputting?
                 new_line_retry;                    -- output a line feed
              end if;
            end if;
         end if;

      ----------------------------------------------------------------------
      -- Gathering a script?
      ----------------------------------------------------------------------

      when gatheringScript =>                       -- collecting text
--put_line( "read '" & ch & "', tag = '" & to_string( tag ) & "', tagCount = " & tagCount'img );
         if ch = bushEndTag(1) then                 -- possible end tag?
            tag := null_unbounded_string & ch;      -- record char
            tagCount := 1;                          -- first in tag
            mode := gatheringEndTag;                -- looking for tag
         else
            uncompressedScript := uncompressedScript & ch; -- add character
            if end_of_line( f ) then                -- EOL?
               -- lineno := lineno + 1;                -- template line
               uncompressedScript := uncompressedScript & ASCII.LF;
            end if;
         end if;

      ----------------------------------------------------------------------
      -- Examining a possible end tag?
      ----------------------------------------------------------------------

      when gatheringEndTag =>                       -- possible ?> ?
--put_line( "get: read '" & ch & "', tag = '" & to_string( tag ) & "', tagCount = " & tagCount'img );
         tag := tag & ch;                           -- record in case not
         tagCount := tagCount + 1;                  -- count characters
         if tagCount < bushEndTag'length then       -- subset of tag?
            if ch /= bushEndTag(tagCount) then      -- match failed?
               mode := gatheringScript;             -- continue w/script
               tagCount := 0;                       -- forget it
               uncompressedScript := uncompressedScript & tag; -- text is script
            end if;
         elsif ch = bushEndTag( tagCount ) then     -- but if last matches
            mode := outputTemplate;                 -- done, run scripts
            tagCount := 0;                          -- forget tag
         else
            mode := gatheringScript;                -- continue w/script
            tagCount := 0;                          -- forget it
            uncompressedScript := uncompressedScript & tag; -- text is script
         end if;
         if end_of_line( f ) then                   -- EOL?
            -- lineno := lineno + 1;                   -- template line
            uncompressedScript := uncompressedScript & ASCII.LF;
         end if;

      ----------------------------------------------------------------------
      -- We have a script.  Run it.
      ----------------------------------------------------------------------

         if mode = outputTemplate and not error_found then
            -- interpretCommand( uncompressedScript );
            -- there should be a proc for this
            compileTemplate( uncompressedScript, startingLineNo );
            inputMode := fromScriptFile;               -- running a script
            exit_block := false;                       -- not exit-ing a block
            -- The inputMode will be fromScriptFile and no syntax_check was
            -- done.  Therefore, force type checking.
            --type_checks_done := not (inputMode = interactive) and not syntax_check;
            type_checks_done := false;
            cmdpos := firstScriptCommandOffset;        -- start at first char
            token := identifiers'first;                -- dummy, replaced by g_n_t
            -- don't reset line number (for error msgs) lineno := 1;
            getNextToken;                              -- load first token
            -- save_lineno := lineno;                     -- save line
            loop                                       -- run commands
               ParseGeneralStatement;
            exit when done or token = eof_t;           -- continue until done
            end loop;                                  --  or eof hit
            if not done then                           -- not exiting?
               expect( eof_t );                        -- should be nothing else
            end if;
            -- lineno := save_lineno;                     -- restore template line
               --DoRunTimeStatements( identifiers( token ).value, );
            uncompressedScript := null_unbounded_string; -- clear script
         end if;

      ----------------------------------------------------------------------
      -- Examining a template.
      ----------------------------------------------------------------------

      when outputTemplate =>
         if ch = bushStartTag(1) then               -- possible start tag?
            tag := null_unbounded_string & ch;      -- record char
            tagCount := 1;                          -- first in tag
            mode := gatheringStartTag;              -- looking for tag
         elsif end_of_line( f ) then                -- otherwise EOL?
            if last_status = 0 then                 -- we are outputting?
               put_retry( ch );                     -- write character
            end if;
            -- lineno := lineno + 1;                   -- count template line
            if last_status = 0 then                 -- we are outputting?
               new_line_retry;                      -- output a line feed
            end if;
         else                                       -- not EOL?
            if last_status = 0 then                 -- we are outputting?
               put_retry( ch );                     -- write character
            end if;
         end if;

      when others =>                             -- unexpected mode
        err(
            contextNotes => pl( "At " & gnat.source_info.source_location &
               " while reading the template" ),
            subjectNotes => subjectInterpreter,
            reason => +"had an internal error because it entered",
            obstructorNotes => pl( "an unexpected template mode " & mode'img )
        );
      end case;

   end loop;                                     -- continue while text
   close( f );                                   -- close template file
   exception when end_error =>
   -- kludge: if a blank line is at the end of template, an end_error
   -- exception will be thrown.  I should fix the EOL handling...
      close( f );
end processTemplate;


---------------------------------------------------------
--
-- MISC PARSER UTILITIES
--
---------------------------------------------------------


------------------------------------------------------------------------------
--  DO QUIT
--
-- Quit a script
------------------------------------------------------------------------------

procedure DoQuit is
begin
  done := true;                             -- stop parsing
  exit_block := true;                       -- exit any block
  done_sub := false;                        -- only leaving subprogram
  if trace then                             -- tracing? explain
     put_trace( "Terminating" );
  end if;
end DoQuit;


------------------------------------------------------------------------------
--  DO RETURN
--
-- Quit a user-defined subprogram
------------------------------------------------------------------------------

procedure DoReturn is
begin
  done := true;                             -- stop parsing
  exit_block := true;                       -- exit any block
  done_sub := true;                         -- only leaving subprogram
  if trace then                             -- tracing? explain
     put_trace( "Returning" );
  end if;
end DoReturn;


------------------------------------------------------------------------------
--  DO START BREAKOUT
--
-- Start a breakout session
------------------------------------------------------------------------------

procedure DoStartBreakout( execution_position : out unbounded_string ) is
begin
  for i in 1..identifiers_top-1 loop
      if identifiers( i ).inspect then
         Put_Identifier( i );
      end if;
  end loop;
  execution_position :=  get_script_execution_position(
      inv( "Break: return to continue, logout to quit" ),
      utf_wristwatch ); -- show stop posn
  put_line_retry( standard_error, to_string( execution_position ) );
  error_found := true;
end DoStartBreakout;


------------------------------------------------------------------------------
--
-- Renaming Support
--
-- These will be refactored in the future to create cleaner renaming
-- support.
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--  FIX RENAMED RECORD FIELDS
--
-- Given a renamed record, search for and adjust the record fields to
-- refer to the correct variables.
------------------------------------------------------------------------------

procedure FixRenamedRecordFields( canonicalRef : renamingReference;
  renamingRec : identifier ) is
  numFields : natural;
  canonicalField : identifier;
begin
  -- The renaming possibilities
  --
  -- canonicalRec      renamedRec       renamedRenamedRec
  --   canonicalField    renamedField     renamedRenamedField
  --                   renamed2Rec
  --                     renamed2Field
  --
  -- For a record renaming, also create renamings for each field of the
  -- record.  (A reference to the canonical record variable is not good
  -- enough.)  Each field must reference to the canonical record's
  -- fields (or the renaming target, in the cause of a double renaming).
  --
  -- SparForte currently treats record fields as regular variables.
  --
  -- 1. The field_of must point to the associated record, not
  --    the canonical record.
  -- 2. All values must point to the canonical record or field's
  --    value.
  -- 3. The variable referenced, even if a renaming, must have
  --    it's counter incremented.
  -- 4. Renaming of individual fields are treated as a regular
  --    variable renaming.
  --

-- put( "Mapping " & canonicalRef.id'img & ":" );
-- put( to_string( identifiers( canonicalRef.id ).name ) );
-- put_line( " to " & to_string( identifiers( id ).name ) );

  -- TODO: this search should be optimized.
  -- Why not stop when all fields are found?
  -- Skip namespaces.

  -- The canonical record/fields could be, of course, another
  -- renaming.
  -- Always a risk of an exception thrown here
  begin
    numFields := natural( to_numeric( identifiers( identifiers(
        canonicalRef.id ).kind ).store.value ) );
  exception when storage_error =>
    numFields := 0;
    err(
       contextNotes => pl( "At " & gnat.source_info.source_location &
          " while fixing a record" ),
       subjectNotes => subjectInterpreter,
       reason => pl( "had an internal error " &
          "while determining the number of fields because of "),
       obstructorNotes => +"a storage_error exception"
    );
  when constraint_error =>
    numFields := 0;
    err(
       contextNotes => pl( "At " & gnat.source_info.source_location &
          " while fixing a record" ),
       subjectNotes => subjectInterpreter,
       reason => pl( "had an internal error while determining " &
          "while determining the number of fields because of "),
       obstructorNotes => +"a constraint_error exception"
    );
  end;

  canonicalField := canonicalRef.id + 1;

  for fieldNumber in 1..numFields loop

      -- brutal search was...
      -- for canonicalField in reverse keywords_top..identifiers_top-1 loop
      --
      -- As an optimization, the fields are likely located immediately after
      -- the record itself is defined.  Also assumes they are stored
      -- sequentially.  In the future, records will be stored differently.

      while canonicalField < identifiers_top loop
        if identifiers( canonicalField ).field_of = canonicalRef.id then
           exit;
        end if;
        canonicalField := identifier( integer( canonicalField ) + 1 );
      end loop;

     -- no more identifiers means we didn't find it.
     if canonicalField = identifiers_top then
        err(
           contextNotes => pl( "At " & gnat.source_info.source_location &
              " while fixing a record" ),
           subjectNotes => subjectInterpreter,
           reason => pl( "had an internal error while determining " &
              "while determining the number of fields because "),
           obstructorNotes => +"a field was not found"
        );
        exit;
     end if;

     declare
        fieldName     : unbounded_string;
        renamingField : identifier;
        dotPos        : natural;
     begin
        -- construct a new field name using the renaming
        fieldName := identifiers( canonicalField ).name;
        dotPos := length( fieldName );
        while dotPos > 1 loop
           exit when element( fieldName, dotPos ) = '.';
           dotPos := dotPos - 1;
        end loop;
        fieldName := delete( fieldName, 1, dotPos );
        fieldName := identifiers( renamingRec ).name & "." & fieldName;

        -- Locate the field of the renaming record
        -- TODO: this is slow, as fields are likely stored sequentially after
        -- the record type, so there's no reason to do a brute-force lookup.
        findIdent( fieldName, renamingField );
        if renamingField = eof_t then
           err(
              contextNotes => pl( "At " & gnat.source_info.source_location &
                 " while fixing a record" ),
              subjectNotes => subjectInterpreter,
              reason => pl( "had an internal error when it " &
                 "could not find a field in the renaming record"),
              obstructorNotes => pl( "Identifier" & canonicalField'img &
                ": Canonical field " ) &
                name_em( canonicalField ) & pl( "/" &
                "Renaming Field " ) & unb_em( fieldName )
           );
        else
           -- The renaming is created by copying data.  Correct
           -- the fields to be owned
           -- by the renaming, not adding fields to the canonical
           -- record.
           identifiers( renamingField ).field_of := renamingRec;
           -- link the renaming field to the canonical field
           identifiers( canonicalField ).renamed_count :=
              identifiers( canonicalfield ).renamed_count + 1;
           identifiers( renamingField ).store :=
              identifiers( canonicalField ).store;
         end if;
     end;
     canonicalField := identifier( integer( canonicalField ) + 1 );
   end loop;
end FixRenamedRecordFields;


------------------------------------------------------------------------------
--  FIX RENAMED ARRAY
--
-- Given a renamed array set up by ParseRenamingPart, fix the attributes
-- for field type.
------------------------------------------------------------------------------

procedure FixRenamedArray( canonicalRef : renamingReference;
  renamingArray : identifier ) is
begin
  -- For full arrays, the code uses avalue directly.  avalue is a pointer
  -- which is copied when the renaming is created in ParseRenamesPart.  So
  -- nothing else should be done here.
  identifiers( renamingArray ).list := true;
  identifiers( renamingArray ).kind := canonicalRef.kind;
end FixRenamedArray;

end parser_aux;
