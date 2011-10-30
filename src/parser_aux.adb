------------------------------------------------------------------------------
-- Parser Aux (Parser Support)                                              --
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
-- OUT ANY WARRANTY;  without even the  iAsmplied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

with ada.text_io,
     Interfaces.C,
     string_util,
     script_io,
     user_io,
     parser;
use  ada.text_io,
     Interfaces.C,
     string_util,
     script_io,
     user_io,
     parser;


package body parser_aux is

use bush_os.HEptrs;

function isExecutingCommand return boolean is
  -- True if OK to execute a statement that does work.
  -- That is, the parser isn't skipping the line because of
  -- an error or exiting a block.
begin
  return not error_found and not exit_block and not syntax_check;
end isExecutingCommand;
--pragma inline( isExecutingCommand );

procedure discardUnusedIdentifier( id : identifier ) is
  -- if an identifier has been not been assigned a type,
  -- assume it's unused and discard it.
  b : boolean;
begin
  if id /= eof_t then
     if identifiers( id ).kind = new_t or identifiers( id ).kind = eof_t then
        b := deleteIdent( id );
     end if;
  end if;
end discardUnusedIdentifier;

procedure makeTempFile( s : out unbounded_string ) is
  -- create a unique temporary filename
  LinuxPath : string := "/tmp/sparXXXXXX" & ASCII.NUL;
  result : aFileDescriptor;
  closeResult : int;
begin
  s := null_unbounded_string;
  mkstemp( result, LinuxPath );
  if result < 0 then
     err( "makeTempFile: mkstemp failed" & OSError( C_errno ) );
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

procedure AssignParameter( ref : in reference; value : unbounded_string ) is
  -- assign value to an out or in out parameter
begin
   if ref.index = 0 then
      identifiers( ref.id ).value := value;
   else
      assignElement( ref.a_id, ref.index, value );
   end if;
end AssignParameter;
pragma inline( AssignParameter );

procedure GetParameterValue( ref : in reference; value : out unbounded_string )
is
begin
   if ref.index = 0 then
      value := identifiers( ref.id ).value;
   else
      value := arrayElement( ref.a_id, ref.index );
   end if;
end GetParameterValue;
pragma inline( GetParameterValue );

function stringField( i : identifier; f : natural ) return unbounded_string is
-- same as string_util.stringField except uses an identifier's value
begin
  return stringField( identifiers( i ).value, recSep, f );
end stringField;

procedure replaceField( i : identifier; f : natural; field : string ) is
-- same as string_util.replaceField except users an identifier's value
begin
  replaceField( identifiers( i ).value, recSep, f, field );
end replaceField;

function stringField( r : reference; f : natural ) return unbounded_string is
-- same as string_util.stringField except uses a reference
   tempStr : unbounded_string;
begin
  getParameterValue( r, tempStr );
  return stringField( tempStr, recSep, f );
end stringField;

procedure replaceField( r : reference; f : natural; field : string ) is
-- same as string_util.replaceField except users an identifier's value
   tempStr : unbounded_string;
begin
  getParameterValue( r, tempStr );
  replaceField( tempStr, recSep, f, field );
  assignParameter( r, tempStr );
end replaceField;

function OSerror( e : integer ) return string is
-- return an OS error message for error number e
  lastchar : natural := 0;
  ep       : anErrorPtr;
begin
  ep := strerror( e );
  for i in ep.all'range loop
      if ep(i) = ASCII.NUL then
	 lastchar := i-1;
         exit;
      end if;
  end loop;
  return string( ep( 1..lastchar ) );
end OSerror;

function openSocket( serverName : unbounded_string; port : integer ) return aSocketFD is


  mySocket    : aSocketFD;     -- the socket
  myAddress   : aSocketAddr;   -- where it goes
  myServer    : aHEptr;        -- IP number of server
  myServerPtr : HEptrs.Object_Pointer;
  addrList    : addrListPtrs.Object_Pointer;
  Result      : int;

begin

  -- initialize a new TCP/IP socket
  -- 0 for the third param lets the kernel decide

  --Put_Line( "Initializing a TCP/IP socket" );
  --Put_Line( "Socket( " & PF_INET'img & ',' & SOCK_STREAM'img & ", 0 );" );

  mySocket := Socket( PF_INET, SOCK_STREAM, 0 );
  if mySocket = -1 then
     err( "error making socket: " & OSError( C_errno ) );
     return -1;
  end if;
  --New_Line;

  -- Lookup the IP number for the server

  --Put_Line( "Looking for information on " & to_string( serverName ) );

  myServer := GetHostByName( to_string( serverName ) & ASCII.NUL );
  myServerPtr := HEptrs.To_Pointer( myServer );
  if myServerPtr = null then
     if C_errno = 0 then
        err( "there is no server by the name '" & to_string( serverName ) & "'" );
     else
        err( "error looking up host: " & OSError( C_errno ) );
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
     err( "error connecting to server: " & OSerror( C_errno ) );
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

-- Parsing short-cuts

--procedure ParseSingleUniStringExpression( expr_val : out unbounded_string;
  --expr_type : out identifier ) is
--begin
  --expect( symbol_t, "(" );
  --ParseExpression( expr_val, expr_type );
  --if uniTypesOk( expr_type, string_t ) then
     --expect( symbol_t, ")" );
  --end if;
--end ParseSingleUniStringExpression;
                                                                               -- 
--procedure ParseSingleStringExpression( expr_val : out unbounded_string;
  --expr_type : out identifier ) is
--begin
  --expect( symbol_t, "(" );
  --ParseExpression( expr_val, expr_type );
  --if baseTypesOk( expr_type, string_t ) then
     --expect( symbol_t, ")" );
  --end if;
--end ParseSingleStringExpression;
--
--procedure ParseSingleNumericExpression( expr_val : out unbounded_string;
  --expr_type : out identifier ) is
--begin
  --expect( symbol_t, "(" );
  --ParseExpression( expr_val, expr_type );
  --if uniTypesOk( expr_type, uni_numeric_t ) then
     --expect( symbol_t, ")" );
  --end if;
--end ParseSingleNumericExpression;


------------------------------------------------------------------------------
-- PROCESS TEMPLATE
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
   bushStartTag   : string := "<?spar";             -- the start tag
   bushEndTag     : string := "?>";                 -- the end tag
   tag            : unbounded_string;               -- possible tag text
   tagCount       : natural := 0;                   -- characters in p. tag
   uncompressedScript : unbounded_string;           -- script being gathered
   -- lineno         : natural := 1;
   startingLineNo : natural := 1;
   -- save_lineno    : aLineNumber;                    -- swap space for line no
   lastLine : Positive_Count := 1;
begin
   gccOpt := true; -- force gcc errors since these will go into
   -- web servers (text) log so hilighting will be annoying
   if not unrestrictedTemplate then
      rshOpt := true;
      restriction_no_external_commands := true;
   end if;
   scriptFilePath := templatePath;
   open( f, in_file, to_string( templatePath ) );   -- open the template
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
                  put( to_string( tag ) );          -- output template text
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
               put( to_string( tag ) );             -- output template text
            end if;
         end if;
         if end_of_line( f ) then                   -- EOL?
              -- lineno := lineno + 1;                 -- template line
              if last_status = 0 then               -- we are outputting?
                 new_line;                          -- output a line feed
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

         if mode = outputTemplate then
            -- interpretCommand( uncompressedScript );
            -- there should be a proc for this
            compileTemplate( uncompressedScript, startingLineNo );
            inputMode := fromScriptFile;               -- running a script
            error_found := false;                      -- no error found
            exit_block := false;                       -- not exit-ing a block
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
               put( ch );                           -- write character
            end if;
            -- lineno := lineno + 1;                   -- count template line
            if last_status = 0 then                 -- we are outputting?
               new_line;                            -- output a line feed
            end if;
         else                                       -- not EOL?
            if last_status = 0 then                 -- we are outputting?
               put( ch );                           -- write character
            end if;
         end if;

      when others =>                             -- unexpected mode
        err( "internal error: unknown template mode" );
      end case;

   end loop;                                     -- continue while text
   close( f );                                   -- close template file
   exception when end_error =>
   -- kludge: if a blank line is at the end of template, an end_error
   -- exception will be thrown.  I should fix the EOL handling...
      close( f );
end processTemplate;

---------------------------------------------------------
-- PARSER UTILITIES
---------------------------------------------------------
                                                                                
procedure DoQuit is
  -- Quit a script
begin
  done := true;                             -- stop parsing
  exit_block := true;                       -- exit any block
  done_sub := false;                        -- only leaving subprogram
  if trace then                             -- tracing? explain
     put_trace( "Terminating" );
  end if;
end DoQuit;
                                                                                
procedure DoReturn is
  -- Quit a user-defined subprogram
begin
  done := true;                             -- stop parsing
  exit_block := true;                       -- exit any block
  done_sub := true;                         -- only leaving subprogram
  if trace then                             -- tracing? explain
     put_trace( "Returning" );
  end if;
end DoReturn;

procedure parseProcedureCallSemicolon is
begin
  if token = symbol_t then
     if identifiers( token ).value = ";" then
        getNextToken;
     elsif identifiers( token ).value = "|" then
        err( "procedures cannot be used in a pipeline like commands" );
     elsif identifiers( token ).value = ">" then
        err( "procedure output cannot be redirected like commands" );
     elsif identifiers( token ).value = ">>" then
        err( "procedure output cannot be redirected like commands" );
     elsif identifiers( token ).value = "<" then
        err( "procedure input cannot be redirected like commands" );
     elsif identifiers( token ).value = "2>" then
        err( "procedure error output cannot be redirected like commands" );
     elsif identifiers( token ).value = "2>>" then
        err( "procedure error output cannot be redirected like commands" );
     elsif identifiers( token ).value = "&" then
        err( "procedures cannot be run in the background like commands" );
     else 
        expect( symbol_t, ";" );
     end if;
  else
     expect( symbol_t, ";" );
  end if;
end parseProcedureCallSemicolon;

procedure parseFunctionCallSemicolon is
begin
  if token = symbol_t then
     if identifiers( token ).value = ";" then
        getNextToken;
     elsif identifiers( token ).value = "|" then
        err( "functions cannot be used in a pipeline like commands" );
     elsif identifiers( token ).value = ">" then
        err( "function output cannot be redirected like commands" );
     elsif identifiers( token ).value = ">>" then
        err( "function output cannot be redirected like commands" );
     elsif identifiers( token ).value = "<" then
        err( "function input cannot be redirected like commands" );
     elsif identifiers( token ).value = "2>" then
        err( "function error output cannot be redirected like commands" );
     elsif identifiers( token ).value = "2>>" then
        err( "function error output cannot be redirected like commands" );
     elsif identifiers( token ).value = "&" then
        err( "functions cannot be run in the background like commands" );
     else 
        expect( symbol_t, ";" );
     end if;
  else
     expect( symbol_t, ";" );
  end if;
end parseFunctionCallSemicolon;


-- JSON


---> IS JSON WHITESPACE
--
-- True if character is a JSON whitespace character.
-----------------------------------------------------------------------------

function isJSONWhitespace( ch : character ) return boolean is
begin
  return ch = ' ' or ch = ASCII.CR or ch = ASCII.LF or ch = ASCII.HT;
end isJSONWhitespace;
pragma inline( isJSONWhitespace );


---> SKIP JSON WHITESPACE
--
-- Move the start index ahread until it is a position in the string that has
-- a non-whitespace character.
-- --------------------------------------------------------------------------

procedure SkipJSONWhitespace( jsonString : unbounded_string; start : in out positive ) is
  stringEnd : natural := length( jsonString );
  ch        : character;
begin
   while start <= stringEnd loop
     ch := element( jsonString, start );
     exit when not isJSONWhitespace( ch );
     start := start + 1;
   end loop;
end SkipJSONWhitespace;


---> JSON EXPECT
--
-- Expect a character.  Report an error if it's not there.
-----------------------------------------------------------------------------

procedure JSONexpect(jsonString : unbounded_string; start : in out positive;
  expectedChar : character ) is
  ch : character;
begin
  SkipJSONWhitespace( jsonString, start );
  if start <= length( jsonString ) then
     ch := element( jsonString, start );
     if ch = expectedChar then
         start := start + 1;
     else
         err( expectedChar & " expected in JSON string at" & start'img );
     end if;
  else
     err( expectedChar & " expected in JSON string at" & start'img );
  end if;
end JSONexpect;


--->  PARSER JSON ITEM
--
-- Parse a JSON item and return the text.  Start at the character start
-- and if the item has nested elements, recurse and include them in
-- the result.  Does not decode the JSON string.  Nesting is not used, just
-- for debugging.
-----------------------------------------------------------------------------

procedure ParseJSONItem( jsonString : unbounded_string;
  result : in out unbounded_string;
  start  : in out positive;
  nesting : positive ) is

  ch          : character;
  j           : positive;
  inBackslash : boolean := false;
  item        : unbounded_string;
  stringEnd   : natural := length( jsonString );
begin
  -- Beyond end of string?  Just abort.
  if start > stringEnd then
     return;
  end if;

  -- Skip any white space
  j := start;
  skipJSONWhitespace( jsonString, j );
  if j > stringEnd then
     return;
  end if;
  ch := element( jsonString, j );
  item := item & ch;
  j := j + 1;

  -- We have characters to build into a string
--put_line( "Parsing on character " & ch & " nesting" & nesting'img ); -- DEBUG

  -- Deterime what we're doing by examining the first character
  -- Aggregate type will recurse

  if ch = '"' then
     loop
        exit when j > stringEnd-1;
        ch := element( jsonString, j );
        if not inBackslash and ch = '"' then
           item := item & ch;
           j := j + 1;
           exit;
        end if;
        if ch = '\' then
           inBackslash := not inBackslash;
        end if;
        item := item & ch;
        j := j + 1;
     end loop;

  -- JSON object: read the label and recurse.  repeat for all items
  elsif ch = '{' then
     skipJSONWhitespace( jsonString, j );
     while j <= stringEnd loop
        ch := element( jsonString, j );
        if ch = '}' then
           item := item & ch;
           j := j + 1;
           exit;
        end if;
        -- really should be string-only here
        ParseJSONItem( jsonString, item, j, nesting+1 );
        JSONexpect( jsonString, j, ':' );
        item := item & ":";
        ParseJSONItem( jsonString, item, j, nesting+1 );
        if j <= stringEnd then
           ch := element( jsonString, j );
           if ch = ',' then
              item := item & ',';
              j := j + 1;
           end if;
       end if;
     end loop;

  -- JSON array: repeat for all items
  elsif ch = '[' then
     skipJSONWhitespace( jsonString, j );
     while j <= stringEnd loop
        ch := element( jsonString, j );
        if ch = ']' then
           item := item & ch;
           j := j + 1;
           exit;
        end if;
        ParseJSONItem( jsonString, item, j, nesting+1 );
        if j <= stringEnd then
           ch := element( jsonString, j );
           if ch = ',' then
              item := item & ',';
              j := j + 1;
           end if;
        end if;
     end loop;

  -- JSON number/boolean/other: exit on white space or terminating character
  else
     while j <= stringEnd loop
        ch := element( jsonString, j );
        if not inBackslash and (ch = ',' or ch = ']' or ch = '"' or ch = '}'
          or isJSONWhitespace(  ch ) ) then
           exit;
        end if;
        if ch = '\' then
           inBackslash := not inBackslash;
        end if;
        item := item & ch;
        j := j + 1;
     end loop;
   end if;

   start := j;
   result := result & item;

end ParseJSONItem;

-- The wrapper that normally gets called.

procedure ParseJSONItem( jsonString : unbounded_string;
                         item : in out unbounded_string;
                         start : in out positive ) is
begin
  item := null_unbounded_string;
  ParseJSONItem( jsonString, item, start, 1 );
end ParseJSONItem;


---> DO STRING FROM JSON
--
-- Convert a JSON string and return the string  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
--
-----------------------------------------------------------------------------

procedure DoStringFromJson( result : out unbounded_string; expr_val : unbounded_string ) is
  ch : character;
  i  : integer;
begin
  result := null_unbounded_string;
  i := 2;
  -- note : could be rewritten to discard quotes and do json unescaped
  loop
     exit when i > length(expr_val)-1;
     if element( expr_val, i ) = '\' then
        i := i + 1;
        ch := element( expr_val, i );
        -- Note : \u not implemented
        if ch = '"' then
           result := result & '"';
        elsif ch = '\' then
           result := result & '\';
        elsif ch = '/' then
           result := result & '/';
        elsif ch =  'b' then
           result := result & ASCII.BS;
        elsif ch = 'f' then
           result := result & ASCII.FF;
        elsif ch = 'n' then
           result := result & ASCII.LF;
        elsif ch = 'r' then
           result := result & ASCII.CR;
        elsif ch = 't' then
           result := result & ASCII.HT;
        end if;
     else
        result := result & element( expr_val, i );
     end if;
     i := i + 1;
  end loop;
end DoStringFromJson;


---> DO ARRAY TO JSON
--
-- Convert an array to a JSON string.  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
-----------------------------------------------------------------------------

procedure DoArrayToJson( target_ref : reference; source_var_id : identifier ) is
  source_first  : long_integer;
  source_last   : long_integer;
  source_len    : long_integer;
  item          : unbounded_string;
  encoded_item  : unbounded_string;
  sourceArrayId : arrayID;
  kind          : identifier;
  elementKind   : identifier;
  data          : unbounded_string;
  result        : unbounded_string;
begin
  -- look up the array information

     sourceArrayId := arrayID( to_numeric( identifiers( source_var_id ).value ) );
     source_first := firstBound( sourceArrayID );
     source_last  := lastBound( sourceArrayID );
     source_len   := source_last - source_first + 1;
     kind := getUniType( identifiers( source_var_id ).kind );
     elementKind := getBaseType( identifiers( identifiers( source_var_id ).kind ).kind );

     -- In JSON, enumerateds (or, at least, booleans) are by the name,
     -- not the value.
     if elementKind = boolean_t then
        declare
           enum_val : integer;
        begin
           result := to_unbounded_string( "[" );
           for arrayElementPos in source_first..source_last loop
               data := arrayElement( sourceArrayId, arrayElementPos );
               enum_val := integer( to_numeric( data ) );
               if enum_val = 0 then
                  item := to_unbounded_string( "false" );
               elsif enum_val = 1 then
                  item := to_unbounded_string( "true" );
               else
                  err( "internal error: unexpect boolean position" & enum_val'img );
               end if;
               if arrayElementPos /= source_last then
                  result := result & item & to_unbounded_string( "," );
               end if;
           end loop;
           result := result & item & to_unbounded_string( "]" );
        end;
        assignParameter( target_ref, result );

     elsif kind = uni_string_t then
        result := to_unbounded_string( "[" );
        for arrayElementPos in source_first..source_last loop
           --data := arrayElement( sourceArrayId, arrayElementPos+offsetArrayBeingSorted );
           data := arrayElement( sourceArrayId, arrayElementPos );
           if elementKind = json_string_t then
              -- if it's a JSON string, just copy the data
              result := result & data;
           else
              item := to_unbounded_string( """" );
              item := item & toJSONEscaped( data );
              --for i in 1..length( data ) loop
              --    ch := element( data, i );
              --    if ch = '"' then
              --       item := item & "\""";
              --    elsif ch = '\' then
              --       item := item & "\\";
              --    elsif ch = '/' then
              --       item := item & "\/";
              --    elsif ch = ASCII.BS then
              --       item := item & "\b";
              --    elsif ch = ASCII.FF then
              --       item := item & "\f";
              --    elsif ch = ASCII.LF then
              --       item := item & "\n";
              --    elsif ch = ASCII.CR then
              --       item := item & "\r";
              --    elsif ch = ASCII.HT then
              --       item := item & "\t";
              --    else
              --       item := item & ch;
              --    end if;
              --end loop;
              item := item & '"';
           end if;
           if arrayElementPos /= source_last then
              result := result & item & to_unbounded_string( "," );
           end if;
        end loop;
        result := result & item & to_unbounded_string( "]" );
        assignParameter( target_ref, result );
     elsif kind = uni_numeric_t or kind = root_enumerated_t then

        result := to_unbounded_string( "[" );
        for arrayElementPos in source_first..source_last loop
           --data := arrayElement( sourceArrayId, arrayElementPos+offsetArrayBeingSorted );
           data := arrayElement( sourceArrayId, arrayElementPos );
           if element( data, 1 ) = ' ' then
              delete( data, 1, 1 );
           end if;
           if arrayElementPos /= source_last then
              result := result & data & to_unbounded_string( "," );
           end if;
        end loop;
        result := result & data & to_unbounded_string( "]" );
        assignParameter( target_ref, result );
     else
        -- this should not happen
        err( "unsupported array type" );
     end if;
end DoArrayToJson;


---> DO JSON TO ARRAY
--
-- Convert a JSON string and store in an array.  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
-----------------------------------------------------------------------------

procedure DoJsonToArray( target_var_id : identifier; source_val : unbounded_string ) is
  target_first  : long_integer;
  target_last   : long_integer;
  target_len    : long_integer;
  sourceLen     : long_integer;
  item          : unbounded_string;
  decoded_item  : unbounded_string;
  targetArrayId : arrayID;
  arrayElement  : long_integer;
  kind          : identifier;
  elementKind   : identifier;
  ch            : character;
  inBackslash   : boolean;
  inQuotes      : boolean;
begin
  -- look up the array information
     targetArrayId := arrayID( to_numeric( identifiers( target_var_id ).value ) );
     target_first := firstBound( targetArrayID );
     target_last  := lastBound( targetArrayID );
     target_len   := target_last - target_first + 1;
     kind := getUniType( identifiers( target_var_id ).kind );
     elementKind := getBaseType( identifiers( identifiers( target_var_id ).kind ).kind );

     -- Count the number of items in the JSON string, handling escaping as
     -- required.  Source len will be the number of items.

     sourceLen := 0;

     declare
       i : integer := 1;
       discard : unbounded_string;
     begin
       while i <= length( source_val ) loop
         ch := element( source_val, i );
         exit when ch = '[';
         i := i + 1;
       end loop;

       if i <= length( source_val ) then
          i := i + 1; -- skip [
          loop
            ParseJSONItem( source_val, discard, i );
            if i > length( source_val ) then
               exit;
            else
               sourceLen := sourceLen + 1;
               SkipJSONWhitespace( source_val, i );
               if i <= length( source_val ) then
                  ch := element( source_val, i );
                  if ch = ',' then
                     i := i + 1;
                  elsif ch = ']' then
                     exit;
                  end if;
               else
                  err( "JSON parse error on character" & i'img );
                  exit;
               end if;
            end if;
          end loop;
       else
          err( "JSON parse error on character" & i'img );
       end if;
     end;

     --sourceLen := 0;
     inBackslash := false;
     inQuotes := false;

     --if length( source_val ) > 2 then -- length zero for []
     --   for i in 2..length( source_val )-1 loop
     --       ch := element( source_val, i );
     --       if inBackslash then
     --          inBackslash := false;
     --       else
     --          if ch = '\' then
     --             inBackslash := true;
     --          else
     --             if not inBackslash and ch = '"' then
     --                inQuotes := not inQuotes;
     --             elsif not inQuotes and ch = ',' then
     --                sourceLen := sourceLen + 1;
     --             end if;
     --          end if;
     --       end if;
     --   end loop;
     --   sourceLen := sourceLen + 1;
    --end if;

    -- Check to see if the length matches that of the array we are assigning
    -- to.  Null array is a special case.
     if sourceLen = 0 and target_len = 0 then
        null;
     elsif sourceLen /= target_len then
       err( "array has" &
            target_len'img &
            " item(s) but JSON string has" &
            sourceLen'img );
     elsif kind = root_enumerated_t then

        -- In JSON, booleans are stored by the name,
        -- Other enums will be by ordinal position (more or less).

        if elementKind = boolean_t then
           -- for a boolean array, we will have to convert true or false
           -- as well as raise an error on illegal values
           arrayElement := target_first;
           for i in 2..length( source_val ) loop
               ch := element( source_val, i );
               if ch = ',' or ch = ']' then
                  if item = "false" then
                     assignElement( targetArrayId, arrayElement, to_unbounded_string( "0" ) );
                     arrayElement := arrayElement + 1;
                     item := null_unbounded_string;
                  elsif item = "true" then
                     assignElement( targetArrayId, arrayElement, to_unbounded_string( "1" ) );
                     arrayElement := arrayElement + 1;
                     item := null_unbounded_string;
                  else
                     err( optional_bold( to_string( item ) ) & " is neither JSON true nor false" );
                  end if;
               else
                  item := item & ch;
               end if;
           end loop;
        else
           -- for non-boolean array of enumerated types, use the ordinal
           -- position and treat it as an array of integers

           -- i don't actually record the maximum value for an enumerated type
           -- the only way to tell is to search the symbol table for a match.
           -- the enum item closest to the top of the symbol table should be
           -- the greatest ordinal position.

           declare
             maxEnum : integer;
             enumVal : integer;
           begin
              for i in reverse keywords_top..identifiers_top-1  loop
                  if identifiers( i ).kind = elementKind then
                     if identifiers( i ).class = constClass then
                        if identifiers( i ).class = constClass then
                           maxEnum := integer( to_numeric( identifiers( i ).value ) );
                           exit;
                        end if;
                     end if;
                  end if;
              end loop;

              -- Assign the positions to the array, checking for out-of-range
              -- positions.
              arrayElement := target_first;
              for i in 2..length( source_val ) loop
                  ch := element( source_val, i );
                  if ch = ',' or ch = ']' then
                     enumVal := integer'value( ' ' & to_string( item ) );
                     if enumVal < 0 or enumVal > maxEnum then
                        err( "enumerated position " &
                             optional_bold( to_string( item ) ) &
                             " is out of range for " &
                             optional_bold( to_string( identifiers( elementKind ).name ) ) );
                     else
                        assignElement( targetArrayId, arrayElement, ' ' & item );
                        arrayElement := arrayElement + 1;
                        item := null_unbounded_string;
                     end if;
                  else
                     item := item & ch;
                  end if;
              end loop;
             end;
           end if;

     elsif kind = uni_string_t then

        -- some kind of string items

        arrayElement := target_first;
        -- for i in 2..length( source_val ) loop
        declare
          i : integer := 2;
        begin

          skipJSONWhitespace( source_val, i );
          while i <= length( source_val ) loop
            ch := element( source_val, i );

            if elementKind = json_string_t then
            -- if it's JSON string, read the string as-is and assign it to the
            -- array.
               ParseJSONItem( source_val, item, i ); 
               if i <= length( source_val ) then
                   skipJSONWhitespace( source_val, i );
                   ch := element( source_val, i );
                   if ch = ',' or ch = ']' then
                      i := i + 1;
                   else
                      err( "JSON parse error on character" & i'img );
                   end if;
               end if;

               assignElement( targetArrayId, arrayElement, item );
               arrayElement := arrayElement + 1;
               item := null_unbounded_string;
            else
               if i <= length( source_val ) then
                  ch := element( source_val, i );
                  if ch /= '"' then
                     err( "JSON string value expected" );
                  end if;
               end if;

               ParseJSONItem( source_val, item, i ); 

               decoded_item := null_unbounded_string;
               for j in 2..length( item )-1 loop
                   ch := element( item, j );
                   if inBackslash then
                      if ch = '"' then
                         decoded_item := decoded_item & '"';
                      elsif ch = '\' then
                         decoded_item := decoded_item & '\';
                      elsif ch = '/' then
                         decoded_item := decoded_item & '/';
                      elsif ch =  'b' then
                         decoded_item := decoded_item & ASCII.BS;
                      elsif ch = 'f' then
                         decoded_item := decoded_item & ASCII.FF;
                      elsif ch = 'n' then
                         decoded_item := decoded_item & ASCII.LF;
                      elsif ch = 'r' then
                         decoded_item := decoded_item & ASCII.CR;
                      elsif ch = 't' then
                         decoded_item := decoded_item & ASCII.HT;
                      end if;
                      inBackslash := false;
                   elsif ch = '\' then
                      inBackslash := true;
                   else
                      decoded_item := decoded_item & ch;
                   end if;
               end loop;

               if i <= length( source_val ) then
                   skipJSONWhitespace( source_val, i );
                   ch := element( source_val, i );
                   if ch = ',' or ch = ']' then
                      i := i + 1;
                   else
                      err( "JSON parse error on character" & i'img );
                   end if;
               end if;

               assignElement( targetArrayId, arrayElement, decoded_item );
               arrayElement := arrayElement + 1;
               item := null_unbounded_string;
               --else
               --   item := item & ch;
               --end if;
            end if; -- if not json_string
          end loop;
        end;

     elsif kind = uni_numeric_t then

        arrayElement := target_first;
        declare
          i : integer := 1;
        begin
          skipJSONWhitespace( source_val, i );
          if i <= length( source_val ) then
             JSONexpect( source_val, i, '[' );
             while i <= length( source_val ) loop
               skipJSONWhitespace( source_val, i );
               item := null_unbounded_string;
               ParseJSONItem( source_val, item, i ); 
               assignElement( targetArrayId, arrayElement, item );
               arrayElement := arrayElement + 1;
               skipJSONWhitespace( source_val, i );
               if i <= length( source_val ) then
                   ch := element( source_val, i );
                   if ch = ',' or ch = ']' then
                      i := i + 1;
                   else
                      err( "JSON parse error on character" & i'img );
                   end if;
               else
                   err( "JSON parse error on character" & i'img );
                   exit;
               end if;
             end loop;
          else
             err( "JSON parse error on character" & i'img );
          end if;
        end;

--        -- some kind of numeric items
--        arrayElement := target_first;
--        for i in 2..length( source_val ) loop
--            ch := element( source_val, i );
--            if ch = ',' or ch = ']' then
--               -- if first character is a quote, it's a string value not number
--               ch := element( item, 1 );
--               if ch = '"' then
--                  err( "JSON number value expected" );
--               end if;
--               assignElement( targetArrayId, arrayElement, item );
--               arrayElement := arrayElement + 1;
--               item := null_unbounded_string;
--            else
--               item := item & ch;
--            end if;
--        end loop;

     else
        -- this should not happen
        err( "unsupported array type" );
     end if;
end DoJsonToArray;


---> DO RECORD TO JSON
--
-- Convert a record to a JSON string.  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
-----------------------------------------------------------------------------

procedure DoRecordToJson( target_ref : reference; source_var_id : identifier ) is
   jsonString    : unbounded_string;
   firstField    : boolean := true;
   fieldName   : unbounded_string;
   jsonFieldName   : unbounded_string;
   dotPos      : natural;
   field_t     : identifier;
   uniFieldType : identifier;
   item        : unbounded_string;
begin
     jsonString := to_unbounded_string( "{" );
     for i in 1..integer'value( to_string( identifiers( identifiers( source_var_id ).kind ).value ) ) loop
         for j in 1..identifiers_top-1 loop
             if identifiers( j ).field_of = identifiers( source_var_id ).kind then
                if integer'value( to_string( identifiers( j ).value )) = i then
                      fieldName := identifiers( j ).name;
                      dotPos := length( fieldName );
                      while dotPos > 1 loop
                         exit when element( fieldName, dotPos ) = '.';
                         dotPos := dotPos - 1;
                      end loop;
                      jsonFieldName := delete( fieldName, 1, dotPos );
                      fieldName := identifiers( source_var_id ).name & "." & jsonFieldName;
                      findIdent( fieldName, field_t );
                      if field_t = eof_t then
                         err( "unable to find record field " &
                            optional_bold( to_string( fieldName ) ) );
                      else
                         if firstField then
                            firstField := false;
                         else
                            jsonString := jsonString & ',';
                         end if;
                         jsonString := jsonString & '"' & jsonFieldName & '"' & ":";
                         -- json encode primitive types
                         uniFieldType := getUniType( identifiers( field_t ).kind );
                         if getBaseType( identifiers( field_t ).kind ) = boolean_t then
                            if integer( to_numeric( identifiers( field_t ).value ) ) = 0 then
                               jsonString := jsonString & "false";
                            else
                               jsonString := jsonString & "true";
                            end if;
                         elsif uniFieldType = uni_numeric_t then
-- trim?
                            jsonString := jsonString & identifiers( field_t ).value;
                         elsif uniFieldType = root_enumerated_t then
                            jsonString := jsonString & identifiers( field_t ).value;
                         elsif getBaseType( identifiers( field_t ).kind ) = json_string_t then
                            -- if it's a JSON string, just copy the data
                            jsonString := jsonString & identifiers( field_t ).value;
                         else
                            item := to_unbounded_string( """" );
                            item := item & ToJSONEscaped( identifiers( field_t ).value );
                            item := item & '"';
                            jsonString := jsonString & item;
                         end if;
                      end if;
                end if;
             end if;
         end loop;
     end loop;
     jsonString := jsonString & "}";
     assignParameter( target_ref, jsonString );
end DoRecordToJson;


---> DO JSON TO RECORD
--
-- Convert a JSON string and store in a record.  This is placed here so it
-- can be reused elsewhere in the language as required.  Params are not
-- checked.
-----------------------------------------------------------------------------

procedure DoJsonToRecord( target_ref : reference; sourceVal : unbounded_string ) is
  jsonString    : unbounded_string;
  firstField    : boolean := true;
  k             : natural;
  item          : unbounded_string;
  decodedItem  : unbounded_string;
  decodedItemName  : unbounded_string;
  decodedItemValue  : unbounded_string;
  elementKind   : identifier;
  ch            : character;
  sourceLen     : long_integer;
  found         : boolean;
  searchName    : unbounded_string;
  jsonStringType : boolean;
begin

     k := 2; -- skip leading { in JSON
     item := sourceVal;

     -- basic JSon validation.  Important to verify it isn't an array.
     if length( item ) > 0 then
        if element( item, 1 ) = '[' then
           err( "JSON object expected but found array" );
        elsif element( item, 1 ) /= '{' then
           err( "JSON object expected but found string " & optional_bold( to_string( toEscaped( item ) ) ) );
        elsif element( item, length( item ) ) /= '}' then
           err( "expected trailing }" );
        end if;
     end if;

     sourceLen := 0;
     declare
       i : integer := 1;
       discard : unbounded_string;
       stringEnd : natural := length( sourceVal );
     begin
       JSONexpect( sourceVal, i, '{' );
       if i <= length( sourceVal ) then
          loop
            ParseJSONItem( sourceVal, discard, i );
            JSONexpect( sourceVal, i, ':' );
            ParseJSONItem( sourceVal, discard, i );
            if i <= stringEnd then
               ch := element( sourceVal, i );
               sourceLen := sourceLen + 1;
               SkipJSONWhitespace( sourceVal, i );
               if i > length( sourceVal ) then
                  exit;
               else
                  ch := element( sourceVal, i );
                  if ch = ',' then
                     i := i + 1;
                  elsif ch = '}' then
                     exit;
                  end if;
               end if;
            else
               err( "JSON parse error on character" & i'img );
               exit;
            end if;
          end loop;
        else
          err( "JSON parse error on character" & i'img );
       end if;
     end;
--put_line( "new length = " & sourceLen'img ); -- DEBUG
-- If this works, delete the old one below
--
     -- Count the number of items in the JSON string
     --sourceLen := 0;
     --
     --if length( item ) > 2 then -- length zero for {}
     --   for i in 2..length( item )-1 loop
     --       ch := element( item, i );
     --       if inBackslash then
     --          inBackslash := false;
     --       else
     --          if ch = '\' then
     --             inBackslash := true;
     --          else
     --             if not inBackslash and ch = '"' then
     --                inQuotes := not inQuotes;
     --             elsif not inQuotes and ch = ',' then
     --                sourceLen := sourceLen + 1;
     --             end if;
     --          end if;
     --       end if;
     --   end loop;
     --   sourceLen := sourceLen + 1;
    --end if;

    -- The number of items in the JSON string should equal the size of the
    -- record.
    if sourceLen = long_integer'value( to_string( identifiers( identifiers( target_ref.id ).kind ).value ) ) then

       -- for each of the items in the JSON string

       declare
          i : integer := 1;
       begin 
          JSONexpect( sourceVal, i, '{' );
          while i <= length( sourceVal ) loop

          -- there should be a label and a string

          ParseJSONItem( sourceVal, decodedItemName, i );
          JSONexpect( sourceVal, i, ':' );
          ParseJSONItem( sourceVal, decodedItemValue, i );

          -- removed the quotes from around the label

          decodedItemName := ToJSONUnescaped( decodedItemName );
          delete( decodedItemName, 1, 1 );
          delete( decodedItemName, length( decodedItemName ), length( decodedItemName ) );

          -- if the value starts with a quote, it's a string type in JSON

          jsonStringType := false;
          if element( decodedItemValue, 1 ) = '"' then
             jsonStringType := true;
          end if;

          -- we have a label and a value.  the record field is stored in the
          -- symbol table as rec.field.  Prepend the record name and search
          -- the symbol table for the record field.  When found, cast the
          -- value and assign it.  Otherwise, if the field is not found, it
          -- is an error.
          found := false;
          searchName := identifiers( target_ref.id ).name & "." & decodedItemName;
          for j in reverse 1..identifiers_top-1 loop
              if identifiers( j ).name = searchName then
                 found := true;
                 if not error_found then
                    -- for booleans, it's true or false, not value
                    elementKind := getBaseType( identifiers( j ).kind );
                    if elementKind = boolean_t then
                       if decodedItemValue = "true" then
                          identifiers( j ).value := to_unbounded_string( "1" );
                       elsif decodedItemValue = "false" then
                          identifiers( j ).value :=  to_unbounded_string( "0" );
                       else
                          err( optional_bold( to_string( toEscaped( decodedItemName ) ) ) & " has a value of " & optional_bold( to_string( toEscaped( decodedItemValue ) ) ) & " but expected JSON true or false" );
                       end if;

-- range check the valuse for enumerateds
                    elsif getUniType( elementKind ) = root_enumerated_t then

                      -- Enumerateds
                      -- i don't actually record the maximum value for an enumerated type
                      -- the only way to tell is to search the symbol table for a match.
                      -- Enums are numbers shouldn't need to have special characters decoded.

                      declare
                        maxEnum : integer;
                        enumVal : integer;
                      begin
                        for i in reverse keywords_top..identifiers_top-1  loop
                            if identifiers( i ).kind = elementKind then
                               if identifiers( i ).class = constClass then
                                  if identifiers( i ).class = constClass then
                                     maxEnum := integer( to_numeric( identifiers( i ).value ) );
                                     exit;
                                  end if;
                               end if;
                            end if;
                        end loop;
                        enumVal := integer'value( ' ' & to_string( decodedItemValue ) );
                        if enumVal < 0 or enumVal > maxEnum then
                           err( "enumerated position " &
                                optional_bold( to_string( toEscaped( decodedItemValue ) ) ) &
                                " is out of range for " &
                                optional_bold( to_string( identifiers( elementKind ).name ) ) );
                        end if;
                        identifiers( j ).value := decodedItemValue;
                      end;

                    elsif getUniType( elementKind ) = uni_string_t then

                      -- Strings
                      -- JSON string is raw json...could be anything.  Otherwise, the string
                      -- needs to be un-escaped.
                      if elementKind /= json_string_t then
                         if not jsonStringType then
                            err( "JSON string value expected for " & optional_bold( to_string( searchName ) ) );
                         end if;
                         -- strip of quotes and un-escape any characters.
                         delete( decodedItemValue, 1, 1 );
                         delete( decodedItemValue, length( decodedItemValue ), length( decodedItemValue ) );
                         decodedItemValue := ToJSONUnescaped( decodedItemValue );
                      end if;
                      identifiers( j ).value := castToType( decodedItemValue,
                         identifiers( j ).kind );
                    else
                      -- Numbers
                      -- Numbers shouldn't need to have special characters decoded.
                      if jsonStringType then
                         err( "JSON number value expected for " & optional_bold( to_string( searchName ) ) );
                      end if;
                      identifiers( j ).value := castToType( decodedItemValue,
                         identifiers( j ).kind );
                    end if;
                 end if;
                 exit; -- the searchName may occur more than once.  stop when found first match

              end if;
          end loop; -- j
          if not found then
             err( to_string( toEscaped( searchName ) ) & " not declared" );
          end if;

          if i <= length( sourceVal ) then
             ch := element( sourceVal, i );
             if ch = ',' then
                i := i + 1;
             elsif ch = '}' then
                exit;
             end if;
          end if;
       end loop; -- i
       end;
    else
       err( "record has" &
            to_string( identifiers( identifiers( target_ref.id ).kind ).value ) &
            " field(s) but JSON string has" &
            sourceLen'img );
    end if;
end DoJsonToRecord;


---> CAST TO TYPE
--
-- If a value is an integer type (i.e. positive, natural or integer),
-- round the value.  Otherwise do not round the value.  Return the
-- result as a string value.
-----------------------------------------------------------------------------

function castToType( val : long_float; kind : identifier ) return unbounded_string is
  baseType : identifier;
  roundedVal : long_long_integer;
  str : unbounded_string;
begin
  -- what kind is it 
  baseType := getBaseType( kind );
  --put_identifier( baseType ); -- DEBUG
  -- If it's an integer type, just round it
  if baseType = short_short_integer_t or
     baseType = short_integer_t or
     baseType = integer_t or
     baseType = long_integer_t or
     baseType = long_long_integer_t then
     roundedVal := long_long_integer( val );
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's a natural type, round it and check for negative
  elsif baseType = natural_t then
     roundedVal := long_long_integer( val );
     if roundedVal < 0 then
        err( "natural value is less than zero" );
     end if;
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's a positive type, round it and check for negative or zero
  elsif baseType = positive_t then
     roundedVal := long_long_integer( val );
     if roundedVal <= 0 then
        err( "positive value is less than zero" );
     end if;
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's anything else, including universals, don't do anything
  -- except convert to a string
  else
     -- return unchanged
     str := to_unbounded_string( val'img );
  end if;
  return str;
end castToType;

function castToType( val : unbounded_string; kind : identifier ) return unbounded_string is
  baseType : identifier;
  roundedVal : long_long_integer;
  str : unbounded_string;
begin
  -- what kind is it 
  baseType := getBaseType( kind );
  --put_identifier( baseType ); -- DEBUG
  -- If it's an integer type, just round it
  if baseType = short_short_integer_t or
     baseType = short_integer_t or
     baseType = integer_t or
     baseType = long_integer_t or
     baseType = long_long_integer_t then
     begin
       roundedVal := long_long_integer( long_float'value( to_string( val ) ) );
     exception when others =>
       err( "exception raised" );
     end;
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's a natural type, round it and check for negative
  elsif baseType = natural_t then
     begin
       roundedVal := long_long_integer( long_float'value( to_string( val ) ) );
     exception when others =>
       err( "exception raised" );
     end;
     if roundedVal < 0 then
        err( "natural value is less than zero" );
     end if;
     begin
       str := to_unbounded_string( long_long_integer'image( roundedVal ) );
     exception when others =>
       err( "exception raised" );
     end;
  -- If it's a positive type, round it and check for negative or zero
  elsif baseType = positive_t then
     begin
       roundedVal := long_long_integer( long_float'value( to_string( val ) ) );
     exception when others =>
       err( "exception raised" );
     end;
     if roundedVal <= 0 then
        err( "positive value is less than zero" );
     end if;
     str := to_unbounded_string( long_long_integer'image( roundedVal ) );
  -- If it's anything else, including universals, don't do anything
  -- except convert to a string
  elsif baseType = character_t then
     if length( val ) /= 1 then
        err( "character value must be one character long" );
     end if;
     str := val;
  else
     -- return unchanged
     str := val;
  end if;
  return str;
end castToType;

end parser_aux;
