------------------------------------------------------------------------------
-- Lexical Scanner (the thing that reads your source code)                  --
-- Also, the semantic stuff.                                                --
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
pragma ada_2005;

pragma warnings( off ); -- suppress Gnat-specific package warning
with ada.command_line.environment;
pragma warnings( on );

with ada.text_io,
    ada.integer_text_io,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    gnat.source_info,
    spar_os.tty,
    signal_flags,
    user_io,
    script_io,
    string_util,
    scanner_res,
    performance_monitoring;
use ada.text_io,
    ada.integer_text_io,
    ada.command_line,
    ada.command_line.environment,
    ada.strings.unbounded.text_io,
    ada.characters.handling,
    spar_os,
    spar_os.tty,
    signal_flags,
    user_io,
    script_io,
    string_util,
    scanner_res,
    performance_monitoring;

package body compiler is

-- encoded character codes for certain keywords

begin_char     : character;
function_char  : character;
is_char        : character;
procedure_char : character;
subtype_char   : character;
type_char      : character;
-- Cyclomatic complexity
else_char      : character;
elsif_char     : character;
if_char        : character;
loop_char      : character;
when_char      : character;


-----------------------------------------------------------------------------
--  GET SOURCE FILE NAME
--
-- Determine the current source file as stored against the byte code line.
-----------------------------------------------------------------------------

function getSourceFileName return unbounded_string is
  line_firstpos : natural;                           -- start of compiled line
  sourceFile : aSourceFile;
  sourceNumber : sourceFilesList.aListIndex;
begin
  line_firstpos := cmdpos;                           -- start at current pos
  if line_firstpos > 1 then                          -- sane value?
     line_firstpos := line_firstpos - 1;             -- search for previous
     while script( line_firstpos ) /= ASCII.NUL loop -- ASCII.NUL
           line_firstpos := line_firstpos - 1;
     end loop;
  end if;
  line_firstpos := line_firstpos + 1;               -- skip NUL
  sourceNumber := sourceFilesList.aListIndex( character'pos( script( line_firstpos ) ) );
  sourceFilesList.Find( sourceFiles, sourceNumber, sourceFile );
  return sourceFile.Name;
end getSourceFileName;


-----------------------------------------------------------------------------
--  GET LINE NO
--
-- Determine the current line number as stored against the byte code line.
-----------------------------------------------------------------------------

function getLineNo return natural is
  line_firstpos : natural;                           -- start of compiled line
  line_number   : natural;
begin
  line_firstpos := cmdpos;                           -- start at current pos
  if line_firstpos > 1 then                          -- sane value?
     line_firstpos := line_firstpos - 1;             -- search for previous
     while script( line_firstpos ) /= ASCII.NUL loop -- ASCII.NUL
           line_firstpos := line_firstpos - 1;
     end loop;
  end if;
  line_firstpos := line_firstpos + 1;               -- skip NUL
  line_number := ( character'pos( script( line_firstpos + 1 ) ) -1 )
               + ( character'pos( script( line_firstpos + 2 ) ) -1 ) * 256;
  return line_number;
end getLineNo;


-----------------------------------------------------------------------------
--  RESET LINE NO
--
-- Reset the line number back to the lowest value.
-----------------------------------------------------------------------------

procedure resetLineNo is
begin
  SourceLineNoLo := 0;
  SourceLineNoHi := 0;
end resetLineNo;


-----------------------------------------------------------------------------
--  GET COMMAND LINE
--
-- Return current command line, decoded into normal text, but not including
-- the LF separating lines.  token_firstpos and token_lastpos is the location
-- of the current token on the expanded line.
-----------------------------------------------------------------------------

procedure getCommandLine ( cmdline : out unbounded_string;
  token_firstpos, token_lastpos, line_number, file_number : out natural ) is
  line_firstpos : natural;                           -- start of compiled line
  line_lastpos  : natural;                           -- end of compiled line
  indent        : natural;
  len           : natural;
  is_escaping   : boolean;

  i             : natural;
  id            : identifier;
  adv           : integer;
begin

  -- Script unexpectedly null?  Print a message an let an exception be raised
  -- later.

  if script = null then
     put_line( standard_error, Gnat.Source_Info.Source_Location & ": internal_error: getCommandLine: script is null" );
     cmdline        := null_unbounded_string;
     token_firstpos := cmdpos;
     token_lastpos  := cmdpos;
     line_number    := natural'last;
     file_number := natural'last;
     return;
  end if;

  -- cmdpos has an insane value?  Print a message and let an exception be
  -- raised later.

  if cmdpos > script'length then
     put_line( standard_error, Gnat.Source_Info.Source_Location & ": internal_error: getCommandLine: cmdpos " & cmdpos'img & " is greater than length of script " & script'length'img );
     cmdline        := null_unbounded_string;
     token_firstpos := cmdpos;
     token_lastpos  := cmdpos;
     line_number    := natural'last;
     file_number := natural'last;
     return;
  end if;

  -- Prepare to find the start and end of the command line

  line_firstpos := cmdpos;                           -- start at current pos
  line_lastpos := cmdpos;                            -- start at current pos
  is_escaping := false;                              -- not escaping

  -- find beginning and end of command line
  -- (as it appears in the byte code)

  if line_firstpos > 1 then                          -- sane value?
     line_firstpos := line_firstpos - 1;             -- search for previous
     while script( line_firstpos ) /= ASCII.NUL loop -- ASCII.NUL
           line_firstpos := line_firstpos - 1;
     end loop;
  end if;
  if line_lastpos <= script'length then              -- sane value?
     while script( line_lastpos ) /= ASCII.NUL loop  -- look for next
       line_lastpos := line_lastpos + 1;             -- ASCII.NUL
     end loop;                                       -- or this one if
  end if;                                            -- on one
  if line_lastpos - line_firstpos <= 2 then          -- a blank line?
     cmdLine := null_unbounded_string;               -- return null string
     token_firstpos := 1;
     token_lastpos := 1;
     return;
  end if;

  -- skip ASCII.NUL at end of last line and the information at the start
  -- of the current line.  Extract the line number and indent.

  line_firstpos := line_firstpos + 1;               -- skip NUL
  file_number := character'pos( script( line_firstpos ) );
  line_number := ( character'pos( script( line_firstpos + 1 ) ) -1 )
               + ( character'pos( script( line_firstpos + 2 ) ) - 1 ) * 255;
  line_firstpos := line_firstpos + 3;               -- skip line number info
  line_lastpos := line_lastpos - 1;
  indent := natural( integer( character'pos( script( line_firstpos ) ) - 1 ) );
  line_firstpos := line_firstpos+1;

  -- find token in command line

  if firstpos >= line_firstpos then                 -- token on line?
     token_firstpos := firstpos-line_firstpos+1;    -- position in
     token_lastpos := lastpos-line_firstpos+1;      -- returned string
     cmdline := null_unbounded_string;              -- begin decompression
     --for i in line_firstpos..line_lastpos loop      -- for bytes in script
     i := line_firstpos;
     while i <= line_lastpos loop
         if script( i ) > ASCII.DEL then            -- a byte code? expand
            if script( i ) = high_ascii_escape then -- escaping
               if not is_escaping then
                  is_escaping := true;
               else                                 -- escaping itself?
                  cmdline := cmdline & script( i ); -- add it
                  is_escaping := false;             -- and no longer escape
               end if;
               i := i + 1;
            else
               if not is_escaping then              -- not escaping?
                  --cmdline := cmdline & identifiers( character'pos( script(i) )-128 ).name;
                  --len := length( identifiers( character'pos( script(i) ) - 128 ).name );
                  toIdentifier( script(i), script(i+1), id, adv );
                  cmdline := cmdline & identifiers( id ).name;
                  len := length( identifiers( id ).name );
                  if firstpos = lastpos and firstpos = i then -- tokenized keyword?
                     token_lastpos := token_lastpos + len-1; -- adjust end position
                  elsif lastpos > i then                     -- token shifted?
                     token_lastpos := token_lastpos + len-1; -- adjust
                     if firstpos > i then
                     token_firstpos := token_firstpos + len-1;
                     end if;
                  end if;
                  i := i + adv;
               else
                  cmdline := cmdline & script( i );
                  is_escaping := false;
                  i := i + 1;
               end if;
            end if;
         else                                             -- not a code?
            cmdline := cmdline & script( i );             -- just add
            i := i + 1;
         end if;
     end loop;                                            -- for all codes
     token_firstpos := token_firstpos + indent;           -- adj token pos
     token_lastpos := token_lastpos + indent;             -- for ident size
  else                                                    -- not processed yet?
     token_firstpos := 1;                                 -- position at
     token_lastpos := 1;                                  -- first character
     cmdline := null_unbounded_string;                    -- same, without
     --for i in line_firstpos..line_lastpos loop            -- token stuff...
     i := line_firstpos;
     while i <= line_lastpos loop
         if script( i ) = ASCII.HT then                   -- embedded tab?
            while (length( cmdline )) mod 8 /= 0 loop     -- move to a column
               cmdline := cmdline & " ";                  -- of 8
            end loop;
         elsif script( i ) > ASCII.DEL then               -- keyword token?
            if script( i ) = high_ascii_escape then       -- escaping
               if not is_escaping then
                  is_escaping := true;
               else                                       -- escaping itself?
                  cmdline := cmdline & script( i );       -- add it
                  is_escaping := false;                   -- and not escape
               end if;
               i := i + 1;
            else
               if not is_escaping then                    -- not escaping?
                  --cmdline := cmdline &
                  --    identifiers( character'pos( script(i) )-128 ).name;
                  --len := length(
                  --    identifiers( character'pos( script(i) ) - 128 ).name );
                  toIdentifier( script(i), script(i+1), id, adv );
                  cmdline := cmdline & identifiers( id ).name;
                  len := length( identifiers( id ).name );
                  if firstpos = lastpos and firstpos = i then -- token keyword?
                     token_lastpos := token_lastpos + len-1;  -- adj end posn
                  elsif lastpos > i then                      -- token shifted?
                     token_lastpos := token_lastpos + len-1;  -- adjust
                     if firstpos > i then
                        token_firstpos := token_firstpos + len-1;
                     end if;
                  end if;
                  i := i + adv;
               else
                  cmdline := cmdline & script( i );
                  is_escaping := false;
                  i := i + 1;
               end if;
            end if;
         else                                             -- other character?
            cmdline := cmdline & script( i );
            i := i + 1;
         end if;
     end loop;
  end if;
  insert( cmdline, 1, to_string( indent * " " ) );        -- expand indentation
  if token_firstpos > length( cmdline ) then              -- past end of cmd?
     token_firstpos := line_lastpos+1-line_firstpos;      -- treat token as
     token_lastpos := token_firstpos;                     -- one char past end
  end if;
end getCommandLine;

function getCommandLine return unbounded_string is
  -- Return current command line, fully indented, but not including
  -- the LF separating lines.  This function version doesn't compute
  -- the token position on the expanded line.
  firstpos, lastpos : natural;
  cmdline : unbounded_string;
  line_number : natural;
  file_number : natural;
begin
  getCommandLine( cmdline, firstpos, lastpos, line_number, file_number );
  return cmdline;
end getCommandLine;


-----------------------------------------------------------------------------
-- "BYTE CODE" GENERATION
--
-- Spar only runs compressed scripts.  The compression
-- process checks for certain syntax errors and makes
-- the following changes:
--
-- * EOL characters are replaced by ASCII nul's.
-- * leading indentation is a single byte at the start
--   of a line, allowing BUSH to ignore indentation
--   unless the line is being printed to the screen.
--   The actual value is +1 (so that 1 is no
--   indentation, 2 is one space, ...) so that the
--   only zero bytes are the EOL characters.
-- * keywords are tokenized as a single byte with the
--   position in the symbol table with the high bit
--   set, avoiding a slow symbol search on keywords
-- * EOF tokens are added as the beginning and ending
--   "lines" of the script to act as sentinels.
--
-- Each line begins with the 16-bit line number and the- 8-bit file number.
--
-- There could be other features in the future.
--
-- For example:
--    if x > y then
-- becomes
-- file/line/indent/stuff/EOL
-- [ASCII 1][ASCII 1][ACSII 1][ASCII 3][if code] x > y [then code][ASCII 0]
--
-- reducing 17 bytes to 12 bytes, about 2/3rds the
-- number of characters to read through when running
-- a script.
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  SET BYTE CODE LINE
--
-- Set the current byte code line number.
-----------------------------------------------------------------------------

procedure setByteCodeLine( lineno : natural ) is
  -- pragma suppress( RANGE_CHECK ); -- GCC 3.3.3 might falsely say overflow
begin
  SourceLineNoLo := lineno mod 255;
  SourceLineNoHi := lineno / 254;
end setByteCodeLine;


-----------------------------------------------------------------------------
--  NEXT BYTE CODE LINE
--
-- Advance the byte code line counter.
-- Remember: zero is reserved for end of line so count ends at 254 + 1
-----------------------------------------------------------------------------

procedure nextByteCodeLine is
  pragma suppress( RANGE_CHECK ); -- GCC 3.3.3 falsely says overflow
begin
  SourceLineNoLo := SourceLineNoLo + 1;
  if SourceLineNoLo > 254 then
     SourceLineNoLo := 0;
     SourceLineNoHi := SourceLineNoHi + 1;
  end if;
end nextByteCodeLine;


-----------------------------------------------------------------------------
--  GET BYTE CODE LINE NO
--
-- Return the current value of the byte code line counter
-----------------------------------------------------------------------------

function getByteCodeLineNo return natural is
begin
  return (SourceLineNoLo) + (SourceLineNoHi) * 254;
end getByteCodeLineNo;


-----------------------------------------------------------------------------
--  GET BYTE CODE LINE NO
--
-- Return the current value of the byte code line counter
-----------------------------------------------------------------------------

function getByteCodeFileNo return natural is
begin
  return SourceFileNo+1;
end getByteCodeFileNo;


-----------------------------------------------------------------------------
--  ERR TOKENIZE
--
-- If this is the first error encountered, display the message set the token
-- to eof_t to abort the parsing and set the error_found flag to indicate
-- that an error was encountered.  (This is for use when generating internal
-- byte code.)
-----------------------------------------------------------------------------

procedure err_tokenize( msg:string; cmdline:string ) is
  lineStr : unbounded_string;
  sfr     : aSourceFile;
begin
  if error_found then                                         -- not first err?
     return;                                                  -- don't display
  end if;

  if inputMode /= interactive and inputMode /= breakout then  -- a script?
     if gccOpt then                                    -- gcc style?
        lineStr := to_unbounded_string( natural'image( getByteCodeLineNo ) );
                                                       -- remove leading
        if length( lineStr ) > 0 then                  -- space (if any)
           if element( lineStr, 1 ) = ' ' then
              delete( lineStr, 1, 1 );
           end if;
        end if;
        sourceFilesList.Find( sourceFiles, sourceFilesList.aListIndex( getByteCodeFileNo ), sfr );
        put( standard_error, sfr.name );              -- show it
        put( standard_error, ":" );
        put( standard_error, to_string( lineStr ) );
        put( standard_error, ":1:" );
     else
        sourceFilesList.Find( sourceFiles, sourceFilesList.aListIndex( getByteCodeFileNo ), sfr );
        put( standard_error, sfr.name );            -- otherwise
        put( standard_error, ":" );                  -- leave leading
        put( standard_error, getByteCodeLineNo'img );           -- spaces in
        put( standard_error, ":1:" );
     end if;
  else
     if gccOpt then                                    -- gcc style?
        lineStr := to_unbounded_string( natural'image( getByteCodeLineNo ) );
                                                       -- remove leading
        if length( lineStr ) > 0 then                  -- space (if any)
           if element( lineStr, 1 ) = ' ' then
              delete( lineStr, 1, 1 );
           end if;
        end if;
        put( standard_error, to_string( lineStr ) );
        put( standard_error, ":1:" );
     else
        put( standard_error, "In line" );                     -- show line num
        put_line( standard_error, natural'image( getByteCodeLineNo ) );
     end if;
  end if;

  -- Command line that errored (ie the current line)

  if not gccOpt then
     put_line( standard_error, cmdline );                     -- display line

  -- Error Pointer

     -- KB: 07/11/04: guestimated
     for i in 1..length( sfr.name )+length( lineStr )+6 loop                           -- move to token
        put( standard_error, " " );
     end loop;
     for i in 1..firstpos-1 loop                              -- move to token
        put( standard_error, " " );
     end loop;
     put( standard_error, "^" );                              -- underline it
     if lastpos-1 > firstpos then
        for i in 1..lastpos-firstpos-2 loop
           put( standard_error, "-" );
        end loop;
        put( standard_error, "^" );
     end if;
  -- TODO: if we include timestamps for templates, they should be in all error
  -- procedures.
  --else
  --   put( standard_error, "[" & getDateString( ada.calendar.clock ) &
  --     "]" ); -- error time
  end if;

  -- Error Message

  put( standard_error, " " );                                 -- display the
  put_line( standard_error, msg );                            -- error msg
  error_found := true;                                        -- flag error
  token := eof_t;                                             -- stop parser
end err_tokenize;


-----------------------------------------------------------------------------
-- A record containing the compressed script being generated
-- and the parse history (to recognize and deal wtih Bourne
-- shell parameters).

--procedure resetRegisters( ci : in out CompressionInfo ) is
--  -- called when we don't know if the index registers are valid
--  -- anymore (ie. at a 'begin' or 'end;')
--begin
--  ci.nextVMIR := 0;
--end resetRegisters;
--
--procedure freeVMNR( ci : in out compressionInfo; r : out aVMNRNumber ) is
--  -- return a free General Purpose Numeric Register.  If none,
--  -- returns noRegister
--begin
--  r := ci.nextVMNR;
--  if ci.nextVMNR < aVMNRNumber'last then
--     ci.nextVMNR := ci.nextVMNR + 1;
--  end if;
--end freeVMNR;
--
--procedure freeVMSR( ci : in out compressionInfo; r : out aVMSRNumber ) is
--  -- return a free General Purpose String Register.  If none,
--  -- returns noRegister
--begin
--  r := ci.nextVMSR;
--  if ci.nextVMSR < aVMSRNumber'last then
--     ci.nextVMSR := ci.nextVMSR + 1;
--  end if;
--end freeVMSR;
--
--function lookupVMNR( ci : compressionInfo; s : unbounded_string ) return aVMNRNumber is
--   -- find a general purpose string register holding the value of id
--   found : aVMNRNumber := aVMNRNumber( noRegister );
--begin
--   for r in aVMNRNumber'first..ci.nextVMNR-1 loop
--       if ci.VMNRmap( r ) = s then
--          found := r;
--       end if;
--   end loop;
--   return found;
--end lookupVMNR;
--
--function lookupVMSR( ci : compressionInfo; s : unbounded_string ) return aVMSRNumber is
--   -- find a general purpose string register holding the value of id
--   found : aVMSRNumber := aVMSRNumber( noRegister );
--begin
--   for r in aVMSRNumber'first..ci.nextVMSR-1 loop
--       if ci.VMSRmap( r ) = s then
--          found := r;
--       end if;
--   end loop;
--   return found;
--end lookupVMSR;
--
--function lookupVMIR( ci : compressionInfo; id : identifier ) return aVMIRNumber is
--   -- find a general purpose numeric register holding the value of id
--   found : aVMIRNumber := aVMIRNumber( noRegister );
--begin
--   for r in aVMIRNumber'first..ci.nextVMIR-1 loop
--       if ci.VMIRmap( r ) = id then
--          found := r;
--       end if;
--   end loop;
--   return found;
--end lookupVMIR;


-----------------------------------------------------------------------------
-- BYTE CODE PRECOMPILER
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  SKIP WHITE SPACE
--
-- Skip through spaces or horizontal tabs.  Put uncompressed white space into
-- compressed script
-----------------------------------------------------------------------------

procedure skipWhiteSpace( ci : in out compressionInfo;
  command : unbounded_string ) is
begin
  -- Skip white Space Before Token
  --
  -- White space is spaces or horizontal tabs.  If the end of line is reached,
  -- ignore white space and go to the end of line handler.  Otherwise, attach
  -- uncompressed white space to compressed script.

  if Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT then
     while Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT loop
        cmdpos := cmdpos + 1;
        if cmdpos > length( command ) then  -- ignore if at end of line
           return;
        end if;
     end loop;
     lastpos := cmdpos-1;
     ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
     firstpos := cmdpos;
     lastpos  := cmdpos;
  end if;
end skipWhiteSpace;


-----------------------------------------------------------------------------
--  IS BYTE CODE
--
-- Part of line2ByteCode (for isPart)
--
-- "is" may be followed by a "record".  Either one causes a start of statement
-- but "is" must not start a new statement if "record" follows it.
-----------------------------------------------------------------------------

procedure ISByteCode( ci : in out compressionInfo;
  command : unbounded_string ) is
  word : unbounded_string;
  id : identifier;
begin
  firstpos := cmdpos;
  lastpos  := cmdpos;

  -- Skip white Space Before Token
  --
  -- White space is spaces or horizontal tabs.  If the end of line is reached,
  -- ignore white space and go to the end of line handler.  Otherwise, attach
  -- uncompressed white space to compressed script.

  if Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT then
     while Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT loop
        cmdpos := cmdpos + 1;
        if cmdpos > length( command ) then  -- ignore if at end of line
           return;
        end if;
     end loop;
     lastpos := cmdpos-1;
     ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
  end if;

  -- Check for an Ada-style comment
  --
  -- If it's an Ada comment, store it in the byte code and go to next handler
  -- to finish the line and return from this procedure.

  if Element( command, cmdpos ) = '-' and cmdpos < length( command ) then
     if Element( command, cmdpos+1 ) = '-' then
        --ci.compressedScript := ci.compressedScript & slice( command, firstpos, length( command ) );
        -- Characters in comments must be high ASCII escaped or else zero bytes in
        -- foreign characters could randomly break the script.
        for i in firstpos..length(command) loop
            if Element( command, i ) = ASCII.NUL or Element( command, i ) = immediate_word_delimiter then
               err_tokenize( "ASCII character not allowed", to_string( command ) );
            elsif Element( command, i ) > ASCII.DEL then
               ci.compressedScript := ci.compressedScript & toByteCode( char_escape_t );
            end if;
            ci.compressedScript := ci.compressedScript & Element( command, i );
        end loop;
        cmdpos := length( command ) + 1;
        return;
     end if;
  end if;

  -- Leading underscores

  if Element( command, cmdpos ) = '_' or

     -- identifiers with underscores not allowed, but we can't check for
     -- that here since we don't know yet if the identifier is part of a
     -- shell command or an AdaScript command (underscores in a shell
     -- command's parameters are OK).

     -- KB: Note this issues an AdaScript identifier, not a shell word, since
     -- a shell word is not possible right after an "is".

  -- Identifiers

        (Element( command, lastpos ) >= 'a' and Element( command, lastpos ) <='z') or
        (Element( command, lastpos ) >= 'A' and Element( command, lastpos ) <='Z') or
        Element( command, lastpos ) > ASCII.DEL then
     lastpos := cmdpos+1;
     word := null_unbounded_string;
     if Element( command, cmdpos ) > ASCII.DEL then
        --word := word & toHighASCII( char_escape_t );
        word := word & toByteCode( char_escape_t );
     end if;
     word := word & Element( command, cmdpos );
     if lastpos <= length( command ) then
        while is_alphanumeric( Element( command, lastpos ) ) or
              Element( command, lastpos ) = '_' or
              Element( command, lastpos ) = '.' or
              Element( command, lastpos ) > ASCII.DEL loop
              if Element( command, lastpos ) > ASCII.DEL then
                 if Element( command, lastpos ) = ASCII.NUL or Element( command, lastpos ) = immediate_word_delimiter then
                    err_tokenize( "ASCII character not allowed", to_string( command ) );
                 end if;
                 if Element( command, lastpos ) > ASCII.DEL then
                    word := word & toByteCode( char_escape_t );
                 end if;
              elsif Element( command, lastpos ) = '.' and then lastpos < length( command ) then
                 if Element( command, lastpos+1 ) = '.' then -- ".."
                    exit;
                 end if;
              end if;
              word := word & Element( command, lastpos );
            lastpos := lastpos+1;
            exit when lastpos > length( command );
        end loop;
     end if;
     id := eof_t;
     lastpos := lastpos - 1;
     cmdpos := lastpos+1;
     for i in reverse 1..reserved_top-1 loop -- was identifiers top
         if identifiers( i ).name = word and not identifiers( i ).deleted then
            id := i;
            exit;
         end if;
     end loop;
     if ci.context = startOfStatement and id >= keywords_top then
        ci.context := startOfParameters;
     elsif ci.context = startOfStatement then
        ci.context := shellStatement;
     elsif id = record_t then
        ci.context := adaScriptStatement;
     elsif id = begin_t then
       ci.context := startOfStatement; -- shell or adascript
     else
        ci.context := adaScriptStatement;
        --ci.context := startOfStatement;
     end if;
     -- add compressed token to compressed script
     declare
       pragma suppress( RANGE_CHECK );
       -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
       -- exception.  We'll do the range checking manually as a work
       -- around...
     begin
       if id /= eof_t and id < reserved_top then
          ci.compressedScript := ci.compressedScript & toByteCode( id );
       else
          ci.compressedScript := ci.compressedScript & word &
             immediate_word_delimiter;
       end if;
    exception when others =>
       err_tokenize( "interal error: byte code generator: exception thrown", to_string( command) );
       raise;
    end;

     -- anything else is a start of statement
  else
     ci.context := startOfStatement;
  end if;

end ISByteCode;


-----------------------------------------------------------------------------
--  ADA SCRIPT STATEMENT BYTE CODE
--
-- Part of line2ByteCode
--
-- We are continuing a new AdaScript statement.  Convert it to byte code.
-----------------------------------------------------------------------------

procedure adaScriptStatementByteCode( ci : in out compressionInfo;
  command : unbounded_string ) is

  id   : identifier;
  word : unbounded_string;
  decimalCount : natural;
  octathorneCount : natural;
  inBackslash : boolean;
  -- nr : aVMNRNumber;
  -- sr : aVMSRNumber;
  -- ir : aVMIRNumber;

begin
  -- Tokenize keywords in the line.  This is very similar to getNextToken
  -- except tokens are stored in the compressed script instead of in a
  -- variables.  Keywords are stored as their symbol table position with
  -- the high-bit set.

<<next>> if cmdpos > length( command ) then                   -- end of line?
     return;                                                  -- and quit
  end if;


  firstpos := cmdpos;                                         -- prepare to
  lastpos := cmdpos;                                          -- get token

  skipWhiteSpace( ci, command );
  if cmdpos > length( command ) then
     goto next;
  end if;

  -- read through leading white space

  if Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT then
     while Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT loop
        cmdpos := cmdpos + 1;
        if cmdpos > length( command ) then  -- ignore if at end of line
           goto next;
        end if;
     end loop;
     lastpos := cmdpos-1;
     ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
     goto next;
  end if;

  -- Illegal characters: control characters or characters with the high
  -- bit set (since those are used to represent compressed keywords).

  if is_control( Element( command, cmdpos ) ) then
     err_tokenize( "Unexpected character ASCII" & character'pos( Element( command, cmdpos ) )'img, to_string( command ) );
     cmdpos := cmdpos + 1;
     return;

  -- Leading underscores

  elsif Element( command, cmdpos ) = '_' or
     -- identifiers with underscores not allowed, but we can't check for
     -- that here since we don't know yet if the identifier is part of a
     -- shell command or an AdaScript command (underscores in a shell
     -- command's parameters are OK).

  -- Identifiers

        (Element( command, lastpos ) >= 'a' and Element( command, lastpos ) <='z') or
        (Element( command, lastpos ) >= 'A' and Element( command, lastpos ) <='Z') or
        Element( command, lastpos ) > ASCII.DEL then
     lastpos := cmdpos+1;
     word := null_unbounded_string;
     if Element( command, cmdpos ) > ASCII.DEL then
        word := word & toByteCode( char_escape_t );
     end if;
     word := word & Element( command, cmdpos );
     if lastpos <= length( command ) then
        while is_alphanumeric( Element( command, lastpos ) ) or
              Element( command, lastpos ) = '_' or
              Element( command, lastpos ) = '.' or
              Element( command, lastpos ) > ASCII.DEL loop
              if Element( command, lastpos ) > ASCII.DEL then
                 if Element( command, lastpos ) = ASCII.NUL or Element( command, lastpos ) = immediate_word_delimiter then
                    err_tokenize( "ASCII character not allowed", to_string( command ) );
                 end if;
                 if Element( command, lastpos ) > ASCII.DEL then
                    word := word & toByteCode( char_escape_t );
                 end if;
              elsif Element( command, lastpos ) = '.' and then lastpos < length( command ) then
                 if Element( command, lastpos+1 ) = '.' then -- ".."
                    exit;
                 end if;
              end if;
              word := word & Element( command, lastpos );
            lastpos := lastpos+1;
            exit when lastpos > length( command );
        end loop;
     end if;
     id := eof_t;
     lastpos := lastpos - 1;
     cmdpos := lastpos+1;
     for i in reverse 1..reserved_top-1 loop -- was identifiers top
         if identifiers( i ).name = word and not identifiers( i ).deleted then
            id := i;
            exit;
         end if;
     end loop;
     if ci.context = startOfStatement and id >= keywords_top then
        ci.context := startOfParameters;
     elsif ci.context = startOfStatement then
        ci.context := shellStatement;
     elsif id = is_t then
        ci.context := isPart;
     elsif id=then_t or id=loop_t or id=begin_t then
       ci.context := startOfStatement;
     end if;
     -- add compressed token to compressed script
     declare
       pragma suppress( RANGE_CHECK );
       -- GCC 3.3.3 (Red Hat Fedora Core 2) falsely reports a out-of-range
       -- exception.  We'll do the range checking manually as a work
       -- around...
     begin
       if id /= eof_t and id < reserved_top then
          ci.compressedScript := ci.compressedScript & toByteCode( id );
       else
          ci.compressedScript := ci.compressedScript & word &
             immediate_word_delimiter;
       end if;
       goto next;
    exception when others =>
       err_tokenize( "interal error: byte code generator: exception thrown", to_string( command) );
       raise;
    end;

  elsif is_digit( Element( command, cmdpos ) ) then
     -- numeric literal
     lastpos := cmdpos;
     decimalCount := 0;
     octathorneCount := 0;
     while is_digit( Element( command, lastpos)) or
         Element( command, lastpos) = '_' or
         Element( command, lastpos) = '#' or
         Element( command, lastpos) = '.' loop
         if Element( command, lastpos ) = '.' and then lastpos < length( command ) then
            if Element( command, lastpos+1 ) = '.' then -- ".."
               exit;
            end if;
            decimalCount := decimalCount+1;
            if decimalCount > 1 then
               cmdpos := lastpos;  -- move cmdpos or infinite loop!
               err_tokenize( "too many decimal points in floating point literal",  to_string( command ) );
               return;
            end if;
         end if;
         if Element( command, lastpos ) = '#' then
            octathorneCount := octathorneCount + 1;
            if octathorneCount > 1 then
               lastpos := lastpos+1;
               exit;
            end if;
         end if;
         lastpos := lastpos+1;
         exit when lastpos > length( command );
     end loop;
        cmdpos := lastpos;
        lastpos := lastpos-1;
        -- if firstpos /= lastpos then  -- don't bother compressing 1 char numbers
           -- newstr := To_Unbounded_String( Slice( command, firstpos, lastpos ) );
           -- nr := lookupVMNR( ci, newStr );
           -- if nr /= aVMNRNumber( noRegister ) then
           --    ci.compressedScript := ci.compressedScript &
           --        character'val( 128 + integer( load_nr_t ) ) &
           --        character'val( nr+1 );
-- put_line( "Found number at register NR " & sr'img );
           --    cmdpos := lastpos+1; -- skip last "
          --     goto next;
          --  else
          --     freeVMNR( ci, nr );
          --     if nr /= aVMNRNumber( noRegister ) then
          --        VMNR( nr ) := newstr;
-- map is redundant if we're pre-loading all numeric literals
          --        ci.VMNRmap( nr ) := newstr;
          --        ci.compressedScript := ci.compressedScript &
          --            character'val( 128 + integer( load_nr_t ) ) &
          --            character'val( nr+1 );
-- put_line( "Number at new register NR " & nr'img );
          --        cmdpos := lastpos+1; -- skip last "
          --        goto next;
           --    else
-- put_line( "No free numeric registers" );
          --     end if;
          --  end if;
       -- end if;
  elsif Element( command, cmdpos ) = ''' then               -- a char literal?
     cmdpos := cmdpos+1;                                    -- skip single quote
     lastpos := cmdpos;                                     -- first literal ch
     word := null_unbounded_string & ''';                   -- starts with '
     if lastpos <= length( command ) then                   -- not EOL quote?
        -- SPECIAL CASE: ''' (single quoted single quote)
        if Element( command, lastpos ) = ''' then           -- ''?
           if lastpos < length( command ) then              -- not EOL?
              if Element( command, lastpos+1 ) = ''' then   -- '''?
                 lastpos := lastpos+2;                      -- skip literal
                 cmdpos := lastpos;                         -- start here next
                 ci.compressedScript := ci.compressedScript & "'''";
                 goto next;                                 -- add & continue
              end if;                                       -- otherwise
           end if;                                          -- fall through
        end if;
        -- NORMAL CASE
        while Element( command, lastpos ) /= ''' loop       -- not literal end?
            if Element( command, lastpos ) > ASCII.DEL then -- hi ascii?
               word := word & toByteCode( char_escape_t ); -- escape it
            end if;
            word := word & Element( command, lastpos );     -- add letter
            lastpos := lastpos+1;                           -- advance one ch
            exit when lastpos > length( command );          -- EOL? bail
        end loop;
     end if;
     if lastpos > length( command ) then                    -- missing quote?
        err_tokenize( "missing single quote", to_string( command ) );
        return;
     else
        word := word & ''';                                 -- add ' to buffer
        lastpos := lastpos+1;                               -- skip last '
        cmdpos := lastpos;                                  -- start here next
        -- cmdpos := lastpos+1;
     end if;
     if lastpos-firstpos < 3 then                           -- too long?
        err_tokenize( "character literal too short--strings are delimited by double quote characters", to_string( command ) );
     end if;
     if lastpos-firstpos > 3 then                           -- too long?
        err_tokenize( "character literal too long--strings are delimited by double quote characters", to_string( command ) );
     end if;
     ci.compressedScript := ci.compressedScript & word;     -- add literal
     goto next;
  elsif Element( command, cmdpos ) = '"' then
     -- Originally, I sliced the characters from the string literal.  However,
     -- to handle high ASCII, I now have to build a string to return, even
     -- that this is slower.
     cmdpos := cmdpos+1;
     word := null_unbounded_string;
     if cmdpos <= length( command ) then  -- quote as last char on line
        while Element( command, cmdpos ) /= '"' loop
            if Element( command, cmdpos ) > ASCII.DEL then
               word := word & toByteCode( char_escape_t );
            end if;
            word := word & Element( command, cmdpos );
            cmdpos := cmdpos+1;
            exit when cmdpos > length( command );
        end loop;
     end if;
     if cmdpos > length( command ) then
        err_tokenize( "missing double quote", to_string( command ) );
        return;
     else
      --   newstr := To_Unbounded_String( Slice( command, cmdpos, lastpos-1 ) );
      --   sr := lookupVMSR( ci, newStr );
      --   if sr /= aVMSRNumber( noRegister ) then
      --      ci.compressedScript := ci.compressedScript &
      --         character'val( 128 + integer( load_sr_t ) ) &
      --         character'val( sr+1 );
-- put_line( "Found string at register SR " & sr'img );
     --       cmdpos := lastpos+1; -- skip last "
     --       goto next;
     --    else
     --       freeVMSR( ci, sr );
     --       if sr /= aVMSRNumber( noRegister ) then
     --          VMSR( sr ) := newstr;
-- map is redundant if we're pre-loading all string literals
     --          ci.VMSRmap( sr ) := newstr;
     --          ci.compressedScript := ci.compressedScript &
     --              character'val( 128 + integer( load_sr_t ) ) &
     --              character'val( sr+1 );
-- put_line( "String at new register SR " & sr'img );
     --          cmdpos := lastpos+1; -- skip last "
     --          goto next;
     --       else
-- put_line( "No free string registers" );
     --       end if;
     --    end if;
        --cmdpos := lastpos+1; -- skip last "
        cmdpos := cmdpos + 1; -- skip last "
        lastpos := cmdpos;
        ci.compressedScript := ci.compressedScript & '"' & word & '"';     -- add literal
        goto next;
     end if;
  elsif Element( command, cmdpos ) = '`' then
     cmdpos := cmdpos+1;
     lastpos := cmdpos;
     inBackslash := false;
     if lastpos <= length( command ) then  -- quote as last char on line
        while not (Element( command, lastpos ) = '`' and not inBackslash) loop
            -- allow anything to be backslashed. the shell parser will
            -- check if it's legitimate later.
            if inBackslash then
               inBackslash := false;
            elsif Element( command, lastpos ) = '\' then
               inBackslash := true;
            end if;
            lastpos := lastpos+1;
            exit when lastpos > length( command );
        end loop;
     end if;
     if natural( lastpos ) > length( command ) then
        err_tokenize( "missing back quote", to_string( command ) );
        return;
     else
        cmdpos := lastpos+1; -- skip last `
     end if;
  else
     -- other punctuation symbols
     firstpos := cmdpos;
     case Element( command, natural( cmdpos ) ) is
     when ';' =>
          ci.context := startOfStatement;
          ci.compressedScript := ci.compressedScript &
             Element( command, natural( cmdpos ) );
          cmdpos := cmdpos + 1;
          return;
     when '$' =>
          if cmdpos < length( command ) then
             cmdpos := cmdpos + 1;
          end if;
     when '?' =>
          null;
     when '#' =>
           -- shell comments used to be supported but not any more
           err_tokenize( "shell comment not supported", to_string( command ) );
           cmdpos := length( command ); -- for read of next line (or EOF)
     when '-' =>
          if cmdpos < length( command ) then
             if Element( command, cmdpos+1 ) = '-' then
                cmdpos := length( command ); -- for read of next line (or EOF)
             end if;
          end if;
     when '=' =>
          if cmdpos < length( command ) then
             if Element( command, cmdpos+1 ) = '>' then
                -- A big arrow can be the start of a case/when.
                --ci.context := startOfStatement; --DEBUGME
                cmdpos := cmdpos + 1;
             end if;
          end if;
     when ':' =>
          if cmdpos < length( command ) then
             if Element( command, cmdpos+1 ) = '=' then
                cmdpos := cmdpos + 1;
             end if;
          end if;
     when '*' =>
          if cmdpos < length( command ) then
             if Element( command, cmdpos+1 ) = '*' then
                cmdpos := cmdpos + 1;
             end if;
          end if;
     when '>' =>
          if cmdpos < length( command ) then
             if Element( command, cmdpos+1 ) = '=' then
                cmdpos := cmdpos + 1;
             end if;
          end if;
     when '<' =>
          if cmdpos < length( command ) then
             if Element( command, cmdpos+1 ) = '=' then
                cmdpos := cmdpos + 1;
             end if;
          end if;
     when '\' =>
          if cmdpos < length( command ) then
             cmdpos := cmdpos + 1;
          end if;
     when '/' =>
          if cmdpos < length( command ) then
             if Element( command, cmdpos+1 ) = '=' then
                cmdpos := cmdpos + 1;
             end if;
          end if;
     when '.' =>
          if cmdpos < length( command ) then
             if Element( command, cmdpos+1 ) = '.' then
                cmdpos := cmdpos + 1;
             end if;
          end if;
     when others => null; -- just that character
     end case;
     lastpos := cmdpos;
     cmdpos := cmdpos + 1;
  end if;
  ci.compressedScript := ci.compressedScript &
    slice( command, firstpos, lastpos );
  goto next;

end adaScriptStatementByteCode;


-----------------------------------------------------------------------------
--  SHELL STATEMENT BYTE CODE
--
-- Part of line2ByteCode
--
-- Translate a POSIX shell command (with quoted shell words) to byte code
-- (Compressed Tokens). Don't look for Ada or SQL formatting but honour Ada
-- comments.
-----------------------------------------------------------------------------

procedure shellStatementByteCode( ci : in out compressionInfo;
  command : unbounded_string ) is

  word : unbounded_string;
  -- nr : aVMNRNumber;
  -- sr : aVMSRNumber;
  -- ir : aVMIRNumber;
  ch : character;
  -- Various states because this is an iterative algorithm
  inDoubleQuotes : boolean;
  inSingleQuotes : boolean;
  inBackQuotes   : boolean;
  inBackslash    : boolean;
  inRedirect     : boolean;
  isSymbolNotWord : boolean;
  redirectAmpersand : boolean;
  maybeStdErrRedirect : boolean;
  processExpansionLevel : natural;
begin
  -- Tokenize shell words in the line.  This is very similar to getNextToken
  -- except tokens are stored in the compressed script instead of in a
  -- variables.  Keywords are stored as their symbol table position with
  -- the high-bit set.

  -- Check for End of Line

<<next>> if cmdpos > length( command ) then                   -- end of line?
     -- ci.compressedScript := ci.compressedScript & ASCII.NUL;  -- add ASCII 0
     return;                                                  -- and quit
  end if;

  -- First Pos and Last Pos will mark the token text.  Cmdpos is our position
  -- in the line (need a better name!)

  firstpos := cmdpos;                                         -- prepare to
  lastpos := cmdpos;                                          -- get token
  isSymbolNotWord := false;

  skipWhiteSpace( ci, command );
  if cmdpos > length( command ) then
     goto next;
  end if;

  -- special tokens: check up front

  inRedirect        := false;
  redirectAmpersand := false;
  maybeStdErrRedirect := false;

  ch := Element( command, cmdpos );
  if ch = ';' then
     ci.context := startOfStatement;
     ci.compressedScript := ci.compressedScript & ch;
     cmdpos := cmdpos + 1;
     return;
  elsif ch = '|' then
     ci.context := startOfStatement;
     ci.compressedScript := ci.compressedScript & ch;
     cmdpos := cmdpos + 1;
     return;
  elsif ch = '@' then
     ci.context := startOfStatement;
     ci.compressedScript := ci.compressedScript & ch;
     cmdpos := cmdpos + 1;
     return;
  elsif ch = '&' then
     ci.context := startOfStatement;
     ci.compressedScript := ci.compressedScript & ch;
     cmdpos := cmdpos + 1;
     return;
  elsif ch = '>' then
     -- TODO: error redirect
     -- TODO: should probably have a handle redirect subroutine
     inRedirect := true;
     redirectAmpersand := true;
     word := word & ch;
     cmdpos := cmdpos + 1;
  elsif ch = '<' then
     -- TODO: should probably have a handle redirect subroutine
     inRedirect := true;
     word := word & ch;
     cmdpos := cmdpos + 1;
  elsif ch = '2' then
     -- TODO: should probably have a handle redirect subroutine
     maybeStdErrRedirect := true;
     word := word & ch;
     cmdpos := cmdpos + 1;
  end if;

  -- Check for an Ada-style comment
  --
  -- If it's an Ada comment, store it in the byte code and go to next handler
  -- to finish the line and return from this procedure.

  if Element( command, cmdpos ) = '-' and cmdpos < length( command ) then
     if Element( command, cmdpos+1 ) = '-' then
        --ci.compressedScript := ci.compressedScript & slice( command, firstpos, length( command ) );
        -- Characters in comments must be high ASCII escaped or else zero bytes in
        -- foreign characters could randomly break the script.
        for i in firstpos..length(command) loop
            if Element( command, i ) = ASCII.NUL or Element( command, i ) = immediate_word_delimiter then
               err_tokenize( "ASCII character not allowed", to_string( command ) );
            elsif Element( command, i ) > ASCII.DEL then
               ci.compressedScript := ci.compressedScript & toByteCode( char_escape_t );
            end if;
            ci.compressedScript := ci.compressedScript & Element( command, i );
        end loop;
        cmdpos := length( command ) + 1;
        goto next;
     end if;
  end if;

  -- We are now looking at a shell word

  -- Handle Shell Words
  --
  -- After skipping white space,  we are expecting shell words.  A shell word
  -- may contain double quotes, single quotes, back quotes and backslashes.
  -- Beware of Ada comments at the end of line.

  -- We start will all quoting off

  inDoubleQuotes    := false;
  inSingleQuotes    := false;
  inBackQuotes      := false;
  inBackslash       := false;
  processExpansionLevel := 0;
  -- Check for special single-character shell words
  --
  -- A semi-colon is the last word of the shell statement.  Return control
  -- to line2ByteCode in case next command is something other than another
  -- shell statement.
  --
  -- A vertical bar always means another shell statement...

  ch := Element( command, cmdpos );                       -- next character
  if ch = ';' then -- really, an AdaScript statement but we're not ready...
     ci.context := startOfStatement;
     ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
     cmdpos := cmdpos + 1;
     return;
  elsif ch = '|' then
     ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
     cmdpos := cmdpos + 1;
     goto next;
  end if;

  -- Loop through word

  loop

    -- First, check for end of line.
    --
    -- Really, shell words should be extendable to a new line with \ at the
    -- end of line, but I'm not ready to support that tonight.

    if cmdpos > length( command ) then
       if inSingleQuotes then
          err_tokenize( "missing single quote", to_string( command ) );
          return;
       end if;
       if inDoubleQuotes then
          err_tokenize( "missing double quote", to_string( command ) );
          return;
       end if;
       if inBackQuotes then
          err_tokenize( "missing back quote", to_string( command ) );
          return;
       end if;
       if inBackslash then
          err_tokenize( "missing backslashed character", to_string( command ) );
          return;
       end if;
       if redirectAmpersand then
          err_tokenize( "missing end of redirect", to_string( command ) );
          return;
       end if;
       lastpos := cmdpos - 1;
       ci.context := startOfStatement;
       exit;
    end if;

    ch := Element( command, cmdpos );                       -- next character
     --put_line( "ch = " & ch & " at" & cmdpos'img & " quotes = " & inDoubleQuotes'img ); -- DEBUG

    -- Second, check for characters that will interfere with the compressed
    -- tokens.

    if ch > ASCII.DEL then
        word := word & toByteCode( char_escape_t );
    elsif is_control( Element( command, cmdpos ) ) then
       err_tokenize( "Unexpected character ASCII" & character'pos( Element( command, cmdpos ) )'img, to_string( command ) );
       cmdPos := cmdPos + 1;
       return;
    end if;

    -- Third, handle quoting quoting characters

    -- A shell redirect is detected by an unescaped '>'.  It can only
    -- be followed by >, & or 1.  Any other character terminates the
    -- redirection and finishes the shell word.

    if maybeStdErrRedirect then
       if ch = '>' then
          inRedirect := true;
          redirectAmpersand := true;
          maybeStdErrRedirect := false;
       elsif ch /= '2' then  -- should not be
          maybeStdErrRedirect := false;
       end if;
    end if;

    -- General word handling

    -- If we are in a redirect, it's a special case and we're looking to
    -- see when it is complete.  We're not allow $ substitutions as a part
    -- of the same word: a redirect is a word to itself even if more characters
    -- follow without a field separator character.

    if inRedirect and ch /= '>' and ch /= '<' and ch /= '&' and ch /= '1' then
       inRedirect := false;
       redirectAmpersand := false;
       lastpos := cmdpos - 1;
       isSymbolNotWord := true;
       exit;

    -- $(..) process expansion is nest-able
    -- However, $(..) can contain regular (..) also.  A closing parenthesis
    -- could belong to something else, or even be unrelated like "echo ')'.

    elsif ch = '$' and not inSingleQuotes and not inBackslash then
       if cmdpos < length( command ) then
          if element( command, cmdpos + 1 ) = '('  then
             processExpansionLevel := processExpansionLevel + 1;
             --word := word & ch;
            -- cmdpos := cmdpos + 1;
          end if;
       end if;
    elsif ch = ')' and not inSingleQuotes
       and not inBackslash then
       if processExpansionLevel > 0 then
          processExpansionLevel := processExpansionLevel - 1;
          --cmdpos := cmdpos + 1;
       else
          err_tokenize( "extra closing ')' with no '$(' process expansion", to_string( command ) );
       end if;

    -- Redirects

    elsif not inRedirect and (ch = '>' or ch = '<') and not inDoubleQuotes
       and not inSingleQuotes and not inBackslash and processExpansionLevel = 0 then
       lastpos := cmdpos - 1;
       exit;
    elsif ch = '"' and not inSingleQuotes and not inBackslash then
       inDoubleQuotes := not inDoubleQuotes;
    elsif ch = ''' and not inDoubleQuotes and not inBackslash then
       inSingleQuotes := not inSingleQuotes;
    elsif ch = '`' and not inSingleQuotes and not inBackslash then
       inBackQuotes := not inBackQuotes;
    elsif ch = '\' and not inSingleQuotes and not inBackslash then
       inBackslash := true;
    else

    -- Fourth, look for word terminators
    -- These only terminate if they're not the first character, which is
    -- checked up front.

       if not (inSingleQuotes or inDoubleQuotes or inBackQuotes or inBackslash
          or processExpansionLevel > 0) then
          if ch = ' ' then
             lastpos := cmdpos - 1;
             exit;
          elsif ch = ASCII.HT then
             lastpos := cmdpos - 1;
             exit;
          elsif ch = ';' then
             lastpos := cmdpos - 1;
             exit;
          elsif ch = '|' then
             lastpos := cmdpos - 1;
             exit;
          elsif ch = '&' and not redirectAmpersand then
             lastpos := cmdpos - 1;
             exit;
          end if;
       end if; -- escaped

       -- Got here? Character is good.  Backslashing?  Turn it off.

       inBackslash := false;

    end if; -- no quoting characters
    word := word & ch;
    cmdpos := cmdpos + 1;
  end loop;

  --put_line( "word = " & to_string(word) ); -- DEBUG

  -- for bareword &, redirections, these are encoded as symbols, not words

  if isSymbolNotWord then
     ci.compressedScript := ci.compressedScript & toByteCode( imm_symbol_delim_t ) &
        word & toByteCode( imm_symbol_delim_t );
  else
     ci.compressedScript := ci.compressedScript & toByteCode( imm_delim_t ) &
        word & toByteCode( imm_delim_t );
  end if;
  --ci.compressedScript := ci.compressedScript & toByteCode( imm_delim_t ) &
  --   slice( command, firstpos, lastpos ) & toByteCode( imm_delim_t );
  word := null_unbounded_string;
  goto next;

end shellStatementByteCode;


-----------------------------------------------------------------------------
--  SQL STATEMENT BYTE CODE
--
-- Part of line2ByteCode
--
-- We are beginning a new SQL statement.  Convert it to byte code.  This is
-- similar the shell words but it's one sentence.
-----------------------------------------------------------------------------

procedure SQLStatementByteCode( ci : in out compressionInfo;
  command : unbounded_string ) is

  ch : character;
  inDoubleQuotes : boolean;
  inSingleQuotes : boolean;
  inBackQuotes   : boolean;
  inBackslash    : boolean;
begin
  -- Tokenize shell words in the line.  This is very similar to getNextToken
  -- except tokens are stored in the compressed script instead of in a
  -- variables.  Keywords are stored as their symbol table position with
  -- the high-bit set.

  -- Check for End of Line

<<next>> if cmdpos > length( command ) then                   -- end of line?
     -- ci.compressedScript := ci.compressedScript & ASCII.NUL;  -- add ASCII 0
     return;                                                  -- and quit
  end if;

  -- Trailing shell params on the end

  if ci.context /= SQLstatement then
     return;
  end if;

  -- First Pos and Last Pos will mark the token text.  Cmdpos is our position
  -- in the line (need a better name!)

  firstpos := cmdpos;                                         -- prepare to
  lastpos := cmdpos;                                          -- get token

  skipWhiteSpace( ci, command );
  if cmdpos > length( command ) then
     goto next;
  end if;

  -- Check for an Ada-style comment
  --
  -- If it's an Ada comment, store it in the byte code and go to next handler
  -- to finish the line and return from this procedure.

  if Element( command, cmdpos ) = '-' and cmdpos < length( command ) then
     if Element( command, cmdpos+1 ) = '-' then
        -- ci.compressedScript := ci.compressedScript & slice( command, firstpos, length( command ) );
        -- Characters in comments must be high ASCII escaped or else zero bytes in
        -- foreign characters could randomly break the script.
        for i in firstpos..length(command) loop
            if Element( command, i ) = ASCII.NUL or Element( command, i ) = immediate_word_delimiter then
               err_tokenize( "ASCII character not allowed", to_string( command ) );
            elsif Element( command, i ) > ASCII.DEL then
               ci.compressedScript := ci.compressedScript & toByteCode( char_escape_t );
            end if;
            ci.compressedScript := ci.compressedScript & Element( command, i );
        end loop;
        cmdpos := length( command ) + 1;
        goto next;
     end if;
  end if;

  -- We are now looking at a shell word

  -- Handle Shell Words
  --
  -- After skipping white space,  we are expecting shell words.  A shell word
  -- may contain double quotes, single quotes, back quotes and backslashes.
  -- Beware of Ada comments at the end of line.

  -- We start will all quoting off

  inDoubleQuotes := false;
  inSingleQuotes := false;
  inBackQuotes   := false;
  inBackslash    := false;

  -- Check for special single-character shell words
  --
  -- A semi-colon is the last word of the shell statement.  Return control
  -- to line2ByteCode in case next command is something other than another
  -- shell statement.
  --
  -- A vertical bar always means another shell statement...

  ch := Element( command, cmdpos );                       -- next character
  if ch = ';' then -- really, an AdaScript statement but we're not ready...
     ci.context := startOfStatement;
     ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
     cmdpos := cmdpos + 1;
     return;
  end if;

  -- Loop through word

  loop

    -- First, check for end of line.

    if cmdpos > length( command ) then
       if inSingleQuotes then
          err_tokenize( "missing single quote", to_string( command ) );
          return;
       end if;
       if inDoubleQuotes then
          err_tokenize( "missing double quote", to_string( command ) );
          return;
       end if;
       if inBackQuotes then
          err_tokenize( "missing back quote", to_string( command ) );
          return;
       end if;
       if inBackslash then
          err_tokenize( "missing backslashed character", to_string( command ) );
          return;
       end if;
       lastpos := cmdpos - 1;
       ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
       goto next;
    end if;

    ch := Element( command, cmdpos );                       -- next character

    -- Check for charactes that will interfere with the compressed tokens.

    --if is_control( Element( command, cmdpos ) ) or Element( command, cmdpos ) > '~' then
    if is_control( Element( command, cmdpos ) ) then
       err_tokenize( "Unexpected character ASCII" & character'pos( Element( command, cmdpos ) )'img, to_string( command ) );
       cmdPos := cmdPos + 1;
       return;
    end if;

    -- Second, handle quoting quoting characters

    if ch = '"' and not inSingleQuotes and not inBackslash then
       inDoubleQuotes := not inDoubleQuotes;
    elsif ch = ''' and not inDoubleQuotes and not inBackslash then
       inSingleQuotes := not inSingleQuotes;
    elsif ch = '\' and not inSingleQuotes and not inBackslash then
       inBackslash := true;
    else

    -- Third, look for word terminators

       if not (inSingleQuotes or inDoubleQuotes or inBackQuotes or inBackslash) then
          if ch = ';' then
             ci.context := startOfStatement;
             lastpos := cmdpos - 1;
             exit;
          elsif ch = '|' then
             ci.context := shellStatement;
             lastpos := cmdpos - 1;
             exit;
          elsif ch = '>' then
             ci.context := shellStatement;
             lastpos := cmdpos - 1;
             exit;
          end if;
       end if; -- escaped

       -- Got here? Character is good.  Backslashing?  Turn it off.

       inBackslash := false;

    end if; -- no quoting characters
    cmdpos := cmdpos + 1;
  end loop;

  ci.compressedScript := ci.compressedScript & toByteCode( imm_sql_delim_t ) &
     slice( command, firstpos, lastpos ) & toByteCode( imm_sql_delim_t );
  goto next;

end SQLStatementByteCode;


-----------------------------------------------------------------------------
--  START OF PARAMETERS BYTE CODE
--
-- Part of line2ByteCode
--
-- Check for ( (AdaScript parameters), := (AdaScript statement), ...
-- Besides skipping whitespace does nothing.
-----------------------------------------------------------------------------

procedure startOfParametersByteCode( ci : in out compressionInfo;
  command : unbounded_string ) is
  ch : character;
begin
  firstpos := cmdpos;
  lastpos  := cmdpos;

  -- Skip white Space Before Token
  --
  -- White space is spaces or horizontal tabs.  If the end of line is reached,
  -- ignore white space and go to the end of line handler.  Otherwise, attach
  -- uncompressed white space to compressed script.

  if Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT then
     while Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT loop
        cmdpos := cmdpos + 1;
        if cmdpos > length( command ) then  -- ignore if at end of line
           return;
        end if;
     end loop;
     lastpos := cmdpos-1;
     ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
  end if;

  -- Check for an Ada-style comment
  --
  -- If it's an Ada comment, store it in the byte code and go to next handler
  -- to finish the line and return from this procedure.

  if Element( command, cmdpos ) = '-' and cmdpos < length( command ) then
     if Element( command, cmdpos+1 ) = '-' then
        -- Characters in comments must be high ASCII escaped or else zero bytes in
        -- foreign characters could randomly break the script.
        for i in firstpos..length(command) loop
            if Element( command, i ) = ASCII.NUL or Element( command, i ) = immediate_word_delimiter then
               err_tokenize( "ASCII character not allowed", to_string( command ) );
            elsif Element( command, i ) > ASCII.DEL then
               ci.compressedScript := ci.compressedScript & toByteCode( char_escape_t );
            end if;
            ci.compressedScript := ci.compressedScript & Element( command, i );
        end loop;
        cmdpos := length( command ) + 1;
        return;
     end if;
  end if;

  ch := Element( command, cmdpos );

  case ch is
  when '(' =>
       ci.context := adaScriptStatement;
  when ':' =>
       ci.context := adaScriptStatement;
  when ',' =>
       ci.context := adaScriptStatement;
  when others =>
       ci.context := shellStatement;
  end case;

end startOfParametersByteCode;


-----------------------------------------------------------------------------
--  START OF DELETE PARAMETERS BYTE CODE
--
-- Part of line2ByteCode
--
-- The delete command has two formats: SQL and AdaScript, not Shell and
-- AdaScript...  This is similar to parametersByteCode.
-----------------------------------------------------------------------------

procedure startOfDeleteParametersByteCode( ci : in out compressionInfo;
  command : unbounded_string ) is
  ch : character;
begin
  firstpos := cmdpos;
  lastpos  := cmdpos;

  -- Skip white Space Before Token
  --
  -- White space is spaces or horizontal tabs.  If the end of line is reached,
  -- ignore white space and go to the end of line handler.  Otherwise, attach
  -- uncompressed white space to compressed script.

  if Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT then
     while Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT loop
        cmdpos := cmdpos + 1;
        if cmdpos > length( command ) then  -- ignore if at end of line
           return;
        end if;
     end loop;
     lastpos := cmdpos-1;
     ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
  end if;

  -- Check for an Ada-style comment
  --
  -- If it's an Ada comment, store it in the byte code and go to next handler
  -- to finish the line and return from this procedure.

  if Element( command, cmdpos ) = '-' and cmdpos < length( command ) then
     if Element( command, cmdpos+1 ) = '-' then
        -- Characters in comments must be high ASCII escaped or else zero bytes in
        -- foreign characters could randomly break the script.
        for i in firstpos..length(command) loop
            if Element( command, i ) = ASCII.NUL or Element( command, i ) = immediate_word_delimiter then
               err_tokenize( "ASCII character not allowed", to_string( command ) );
            elsif Element( command, i ) > ASCII.DEL then
               ci.compressedScript := ci.compressedScript & toByteCode( char_escape_t );
            end if;
            ci.compressedScript := ci.compressedScript & Element( command, i );
        end loop;
        cmdpos := length( command ) + 1;
        return;
     end if;
  end if;

  ch := Element( command, cmdpos );

  case ch is
  when '(' =>
       ci.context := adaScriptStatement;
  when ':' =>
       ci.context := adaScriptStatement;
  when others =>
       ci.context := SQLStatement;
  end case;

end startOfDeleteParametersByteCode;


-----------------------------------------------------------------------------
--  START OF STATEMENT BYTE CODE
--
-- Part of line2ByteCode
--
-- We are beginning a new statement of unknown type (ie. after a ;).  Handle
-- the first word and compress it.  Determine what context is next and return
-- it.
--
-- We don't know what the first word is.  It must be treated as an Ada
-- variable (or shell word in double quotes) so it can be declared in the
-- symbol table.
--
-- Results can be:
--   Start of Parameters ( word := | ( | shell-words )
--   SQL Statement       ( select | ... )
--   Shell Statement     ( env | ... )
--   Ada Statement       ( ... )
-----------------------------------------------------------------------------

procedure startOfStatementByteCode( ci : in out compressionInfo;
  command : unbounded_string ) is

  word : unbounded_string;
  -- nr : aVMNRNumber;
  -- sr : aVMSRNumber;
  -- ir : aVMIRNumber;
  ch : character;
  id             : identifier;
  -- backupPos      : natural;
  shell_word : boolean := false; -- KB: 17/12/2
  inDoubleQuotes : boolean;
  inSingleQuotes : boolean;
  inBackQuotes   : boolean;
  inBackslash    : boolean;
  inRedirect     : boolean;
  redirectAmpersand : boolean;
begin
  -- Tokenize shell words in the line.  This is very similar to getNextToken
  -- except tokens are stored in the compressed script instead of in a
  -- variables.  Keywords are stored as their symbol table position with
  -- the high-bit set.

  -- Check for End of Line

  if cmdpos > length( command ) then                          -- end of line?
     ci.compressedScript := ci.compressedScript & ASCII.NUL;  -- add ASCII 0
     return;                                                  -- and quit
  end if;

  -- First Pos and Last Pos will mark the token text.  Cmdpos is our position
  -- in the line (need a better name!)

  firstpos := cmdpos;                                         -- prepare to
  lastpos := cmdpos;                                          -- get token

  -- Skip white Space Before Token
  --
  -- White space is spaces or horizontal tabs.  If the end of line is reached,
  -- ignore white space and go to the end of line handler.  Otherwise, attach
  -- uncompressed white space to compressed script.

  if Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT then
     while Element( command, cmdpos ) = ' ' or Element( command, cmdpos ) = ASCII.HT loop
        cmdpos := cmdpos + 1;
        if cmdpos > length( command ) then  -- ignore if at end of line
           return;
        end if;
     end loop;
     lastpos := cmdpos-1;
     ci.compressedScript := ci.compressedScript & slice( command, firstpos, lastpos );
     -- goto next;
  end if;

  -- Check for an Ada-style comment
  --
  -- If it's an Ada comment, store it in the byte code and go to next handler
  -- to finish the line and return from this procedure.

  if Element( command, cmdpos ) = '-' and cmdpos < length( command ) then
     if Element( command, cmdpos+1 ) = '-' then
        -- ci.compressedScript := ci.compressedScript & slice( command, firstpos, length( command ) );
        -- Characters in comments must be high ASCII escaped or else zero bytes in
        -- foreign characters could randomly break the script.
        for i in firstpos..length(command) loop
            if Element( command, i ) = ASCII.NUL or Element( command, i ) = immediate_word_delimiter then
               err_tokenize( "ASCII character not allowed", to_string( command ) );
            elsif Element( command, i ) > ASCII.DEL then
               ci.compressedScript := ci.compressedScript & toByteCode( char_escape_t );
            end if;
            ci.compressedScript := ci.compressedScript & Element( command, i );
        end loop;
        cmdpos := length( command ) + 1;
        return;
     end if;
  end if;

  -- backupPos := cmdpos;

  -- First, try an AdaScript word.  If that fails, treat it as a new identifier
  -- and declare it.  Don't use quoted shell words since parser assumes new
  -- ident will be declared in the symbol table.

  id := eof_t;
  ch := Element( command, cmdpos );
  if ch = '?' then
     ci.context := adaScriptStatement;
     return;

  elsif ch = '@' then
     ci.context := adaScriptStatement;
     return;

  -- Get the first token on the line.
  --
  -- Determine if this could be an Adascript identifier.  If it's not, then
  -- it must be a Bourne shell word.

  elsif (ch >= 'a' and ch <='z') or (ch >= 'A' and ch <='Z') or
        ch = '.' or ch = directory_delimiter or -- KB: 17/12/22
        ch = '"' or ch = ''' or ch = '`' or ch = '\' or
        ch > ASCII.DEL then
     -- get ready to scan the word/identifier
     lastpos := cmdpos+1;
     word    := null_unbounded_string;
     inDoubleQuotes := false;
     inSingleQuotes := false;
     inBackQuotes   := false;
     inBackslash    := false;
     inRedirect     := false;
     redirectAmpersand := false;

     -- read the first character, escaping high ascii if needed
     if ch > ASCII.DEL then
        word := word & toByteCode( char_escape_t );
     end if;
     word := word & ch;

     if ch = '"' then
        inDoubleQuotes := true;
        shell_word := true;
     elsif ch = ''' then
        inSingleQuotes := true;
        shell_word := true;
     elsif ch = '`' then
        inBackQuotes := true;
        shell_word := true;
     elsif ch = '\' then
        inBackslash := true;
        shell_word := true;
     elsif ch = '>' then
        inRedirect := true;
        shell_word := true;
     else
        -- it's a shell word if the first character is a path character
        shell_word := shell_word or (ch = '.' or ch = directory_delimiter); -- KB: 17/12/2
     end if;

     if lastpos <= length( command ) then
        -- This is a shell word or an AdaScript identifier
        -- as a shell word, we don't support quoting yet.  Note that
        -- space is graphical.
        loop
           -- reusing ch variable
<<next_word_char>>
           ch := Element( command, lastpos );
           -- Follow the quoting
           if ch = '"' and not inSingleQuotes and not inBackslash then
              inDoubleQuotes := not inDoubleQuotes;
           elsif ch = ''' and not inDoubleQuotes and not inBackslash then
              inSingleQuotes := not inSingleQuotes;
           elsif ch = '`' and not inSingleQuotes and not inBackslash then
              inBackQuotes := not inBackQuotes;
           elsif ch = '\' and not inSingleQuotes and not inBackslash then
              -- flag we saw it, add it, then get the next character
              inBackslash := true;
              word := word & ch;
              lastpos := lastpos+1;
              exit when lastpos > length( command );
              goto next_word_char;

          -- A shell redirect is detected by an unescaped '>'.  It can only
          -- be followed by >, & or 1.  Any other character terminates the
          -- redirection and finishes the shell word.

            elsif inRedirect and ch /= '>' and ch /= '&' and ch /= '1' then
               lastpos := cmdpos - 1;
               exit;
            elsif ch = '>' and not inSingleQuotes and not inBackslash then
              inRedirect := true;
              redirectAmpersand := true;
              inRedirect := true;
              word := word & ch;
              lastpos := lastpos+1;
              exit when lastpos > length( command );
              goto next_word_char;
           end if;

           -- various stop characters only have meaning if not escaped
           if not inSingleQuotes and not inDoubleQuotes and not inBackQuotes
              and not inBackslash then
              exit when
                 -- quotes will affect these
                 not is_graphic( ch ) or
                 ch = ' ' or
                 ch = ASCII.HT or
                 ch = ',' or
                 ch = ';' or
                 ch = ':' or
                 ch = '=' or
                 ch = '(' ;
              exit when
                 ch = '&' and not redirectAmpersand;
           end if;

           -- It must be a shell word if not an identifier...if it's not
           -- alphabetical, an underscore or escaped Latin-1.
           shell_word := shell_word or not
               ( is_alphanumeric( ch ) or
                 ch = '_' or
                 ch = '.' or
                 ch > ASCII.DEL ); -- KB: 17/12/30
           -- ch = '.' or ch = directory_delimiter); -- KB: 17/12/2
           -- escape high ASCII characters
           if ch > ASCII.DEL then
              if ch = ASCII.NUL or ch = immediate_word_delimiter then
                 err_tokenize( "ASCII character not allowed", to_string( command ) );
              end if;
              word := word & toByteCode( char_escape_t );
           -- I can't remember why I did this.  was breaking ../../x
           --elsif ch = '.' and then lastpos < length( command ) then
           --   if Element( command, lastpos+1 )  = '.' then -- ".."
           --      exit;
           --   end if;
           end if;
           -- add the character to the word/identifier.  The backslash, if
           -- any, is consumed.
           word := word & ch;
           lastpos := lastpos+1;
           inBackslash := false;
           inRedirect := false;
           -- exit when there are no more characters in the command line
           exit when lastpos > length( command );
        end loop; -- while
     end if;
--put_line( "SOS: word = " & to_string( word ) ); -- DEBUG
     id := eof_t;
     lastpos := lastpos - 1;
     cmdpos := lastpos+1;
     for i in reverse 1..reserved_top-1 loop -- SHOULD "keyword_top" BE ALL?
         if identifiers( i ).name = word and not identifiers( i ).deleted then
            id := i;
            exit;
         end if;
     end loop;

  -- If a keyword was found compress it and change the context

     if id /= eof_t then                                   -- found it?
       if id < reserved_top then                           -- reserved word?
          ci.compressedScript := ci.compressedScript &     -- ASCII 128 +
             toByteCode( id );                            -- sym table pos
       else                                                -- should not occur
          ci.compressedScript := ci.compressedScript &     -- store store as an
             word & immediate_word_delimiter;              -- immediate word
       end if;

       if id = alter_t then                                -- first word alter?
          ci.context := SQLStatement;                      -- treat as SQL
       elsif id = insert_t then                            -- insert?
          ci.context := SQLStatement;                      -- treat as SQL
       elsif id = select_t then                            -- select?
          ci.context := SQLStatement;                      -- treat as SQL
       elsif id = update_t then                            -- update?
          ci.context := SQLStatement;                      -- treat as SQL
       elsif id = delete_t then                            -- delete?
          ci.context := StartOfDeleteParameters;           -- SQL or Ada
       elsif id = env_t then                               -- env?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = typeset_t then                           -- typeset?
          ci.context := adaScriptStatement;                -- treat as Ada
       elsif id = unset_t then                             -- unset?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = trace_t then                             -- trace?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = help_t then                              -- help?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = clear_t then                             -- clear?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = jobs_t then                              -- job?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = logout_t then                            -- logout?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = pwd_t then                               -- pwd?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = cd_t then                                -- cd?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = history_t then                           -- history?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = wait_t then                              -- wait?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = step_t then                              -- step?
          ci.context := StartOfParameters;                 -- Shell or Ada
       elsif id = begin_t then                             -- begin?
          ci.context := startOfStatement;                  -- don't know
       elsif id = is_t then                                -- is?
          ci.context := isPart;
       elsif id = else_t then                              -- else?
          ci.context := startOfStatement;                  -- don't know
       elsif id = loop_t then                              -- loop?
          ci.context := startOfStatement;                  -- don't know
       elsif id = record_t then                            -- record?
          ci.context := startOfStatement;                  -- don't know
       elsif id = then_t then                              -- then?
          ci.context := startOfStatement;                  -- don't know
       else                                                -- otherwise
          ci.context := adaScriptStatement;                -- assume Ada
       end if;

     else                                                  -- not reserved?
       -- it's either an identifier or a shell word (if it contains
       -- path characaters).  Shell words start/end with a delim
       -- while undeclared idents just end with a delim.
       if shell_word then
          ci.compressedScript := ci.compressedScript &        -- store store as an
             immediate_word_delimiter & -- KB: 17/12/22
              word & immediate_word_delimiter;                -- immediate word
       else
          ci.compressedScript := ci.compressedScript &        -- store store as an
              word & immediate_word_delimiter;                -- immediate word
       end if;
       ci.context := StartOfParameters;                    -- Shell or Ada

     end if;
  else
    ci.context := StartOfParameters;
  end if;
end startOfStatementByteCode;


-----------------------------------------------------------------------------
--  LINE 2 BYTE CODE
--
-- Receive a new line of a script and start/continue compiling the script
-- into byte code (compressed tokens).  ci is the context the text occurs in
-- (e.g. were we working on SQL, Shell or an AdaScript statement, etc.)
-----------------------------------------------------------------------------

procedure line2ByteCode( ci : in out compressionInfo;
  command : unbounded_string ) is
  tabAdjust : natural := 0;
begin

  -- Next Line
  --
  -- Remember: zero is reserved for end of line so count ends at 254+1

  nextByteCodeLine;

  -- SOURCE IDENTIFICATION
  --
  -- Start of line has file number (8-bit) and line number (16-bit).  Each
  -- number is plus one to avoid ASCII 0 (reserved for end of lines).

  ci.compressedScript := ci.compressedScript & character'val( SourceFileNo+1 );
  ci.compressedScript := ci.compressedScript & character'val( SourceLineNoLo+1 );
  ci.compressedScript := ci.compressedScript & character'val( SourceLineNoHi+1 );

  -- BLANK LINES
  --
  -- A blank line ASCII 1 byte (no indentation) and an ASCII 0 byte (end of
  -- line).

  if length( command ) = 0 then
     ci.compressedScript := ci.compressedScript & ASCII.SOH & ASCII.NUL;
     return;
  end if;

  -- INDENT COMPRESSION
  --
  -- The third character of the line is the indentation white space byte (plus
  -- one to avoid ASCII 0).  (Tab stops are treated as tabSize (default 8) spaces.)

  cmdpos := 1;
  if Element( command, cmdpos ) = ' ' or Element( command,
    cmdpos ) = ASCII.HT then
    while Element( command, cmdpos ) = ' ' or Element(
       command, cmdpos ) = ASCII.HT loop
       if Element( command, cmdpos ) = ASCII.HT then
          tabAdjust := tabAdjust + tabSize - ((cmdpos+tabAdjust) mod tabSize);
       end if;
       cmdpos := cmdpos + 1;
       exit when cmdpos > length( command );  -- ignore if at end of line
    end loop;
    lastpos := cmdpos-1;            -- actually, first non-whitespace char
    if lastpos+tabAdjust > 80 then
       err_tokenize( "style issue: large identation can hide text beyond right display margin", to_string( command ) );
    end if;
    if lastpos+tabAdjust > 254 then -- hopefully, never, but harmless
       lastpos := 254-tabAdjust;    -- to truncate leading indentation
    end if;
    ci.compressedScript := ci.compressedScript &
      character'val( lastpos+1+tabAdjust );
    firstpos := cmdpos;                                       -- first non-
    lastpos := cmdpos;                                        -- white char
  else                                                        -- no indent?
    ci.compressedScript := ci.compressedScript & ASCII.SOH;   -- then ASCII 1
  end if;

  -- Empty Command Line

  if cmdpos > length( command ) then
     ci.compressedScript := ci.compressedScript & ASCII.NUL;
     return;
  end if;

  -- BYTE CODE GENERATION
  --
  -- Use the appropriate compiler for the context.

<<next>> if wasSIGINT then
      wasSIGINT := false;
      done := true;                             -- stop parsing
      exit_block := true;                       -- exit any block
      done_sub := false;                        -- only leaving subprogram
      if trace then                             -- tracing? explain
         put_trace( "Terminating" );
      end if;
      return;
  end if;
  case ci.context is
  when startOfStatement =>
       startOfStatementByteCode( ci, command );
  when startOfParameters =>
       startOfParametersByteCode( ci, command );
  when startOfDeleteParameters =>
       startOfDeleteParametersByteCode( ci, command );
  when adaScriptStatement =>
       adaScriptStatementByteCode( ci, command );
  when shellStatement =>
       shellStatementByteCode( ci, command );
  when SQLStatement =>
       SQLStatementByteCode( ci, command );
  when ISPart =>
       ISByteCode( ci, command );
  when others =>
       err_tokenize( "don't know how to handle compressionInfo context", to_string( command ) );
  end case;

  -- Line not finished?

  if cmdpos <= length( command ) then
     goto next;
  end if;

  -- END OF LINE
  --
  -- ASCII zero is the end of line character

  ci.compressedScript := ci.compressedScript & ASCII.NUL;  -- add ASCII 0

-- put_line( "new_line2ByteCode final: " & toescaped( ci.compressedScript ) );
end line2ByteCode;


-----------------------------------------------------------------------------
--  BEGIN BYTE CODE
--
-- Part of Byte Code Generator
-- Write the EOF header line to complete an AdaScript script.
-----------------------------------------------------------------------------

procedure beginByteCode( ci : in out compressionInfo ) is
  pragma suppress( RANGE_CHECK );
  -- GCC 3.3.3 (Fedora Core 2) falsely says overflow
begin

  ci.compressedScript := null_unbounded_string;

  -- Script Header: 2 bytes

  ci.compressedScript := ci.compressedScript & ASCII.STX; -- version 2
  ci.compressedScript := ci.compressedScript & ASCII.NUL; -- reserved

  -- EOF Leader Line: 6 bytes

  ci.compressedScript := ci.compressedScript & ASCII.SOH; -- file number
  ci.compressedScript := ci.compressedScript & ASCII.SOH; -- line number low
  ci.compressedScript := ci.compressedScript & ASCII.SOH; -- line number high
  ci.compressedScript := ci.compressedScript & ASCII.SOH; -- indent
  ci.compressedScript := ci.compressedScript & toByteCode( eof_t );
  ci.compressedScript := ci.compressedScript & ASCII.NUL; -- EOL marker

end beginByteCode;


-----------------------------------------------------------------------------
--  END BYTE CODE
--
-- Part of Byte Code Generator
-- Write the EOF trailer line to complete an AdaScript script.
-----------------------------------------------------------------------------

procedure endByteCode( ci : in out compressionInfo ) is
  pragma suppress( RANGE_CHECK );
  -- GCC 3.3.3 (Fedora Core 2) falsely says overflow
begin
  -- EOF Trailer Line: 6 bytes
  ci.compressedScript := ci.compressedScript & ASCII.SOH;    -- trailing sent.
  ci.compressedScript := ci.compressedScript & character'val( SourceLineNoLo+1 );
  ci.compressedScript := ci.compressedScript & character'val( SourceLineNoHi+1 );
  ci.compressedScript := ci.compressedScript & ASCII.SOH;
  ci.compressedScript := ci.compressedScript & toByteCode( eof_t );
  ci.compressedScript := ci.compressedScript & ASCII.NUL;
end endByteCode;


-----------------------------------------------------------------------------
--  DUMP BYTE CODE
--
-- Display the compressed script to standard output in a readable form.
-----------------------------------------------------------------------------

procedure dumpByteCode( ci : compressionInfo ) is
   line : integer := 0;
begin
   put_line( "--- Byte Code dump ---------------------------------------------------" );
   put( "  H:   1:" );
   put( ToEscaped( to_unbounded_string( "" & Element( ci.compressedScript, 1 ) ) ) );
   put( ToEscaped( to_unbounded_string( "" & Element( ci.compressedScript, 2 ) ) ) );
   put_line( " Byte Code Version: " & character'pos( Element( ci.compressedScript, 1 ) )'img );
   put( "  0:   3:" );
   for i in 3..length( ci.compressedScript ) loop
       put( ToEscaped( to_unbounded_string( "" & Element( ci.compressedScript, i ) ) ) );
       if Element( ci.compressedScript, i ) = ASCII.NUL then
          line := line + 1;
          if i /= length( ci.compressedScript ) then
             new_line;
             if wasSIGINT then
                wasSIGINT := false;
                done := true;                          -- stop parsing
                exit_block := true;                    -- exit any block
                done_sub := false;                     -- only leaving subprogram
                if trace then                          -- tracing? explain
                   put_trace( "Terminating" );
                end if;
                exit;
             end if;
             put( line, width => 3 );
             put( ":" );
             put( i+1, width => 4 );
             put( ":" );
          end if;
       end if;
   end loop;
   new_line;
   put_line( "Byte Code Size =" & length( ci.compressedScript )'img );
end dumpByteCode;


-----------------------------------------------------------------------------
--  COMPILE INCLUDE
--
-- Compile into byte code a command typed interactively at the command prompt
-- or backquotes or templates.
-----------------------------------------------------------------------------

procedure compileInclude( command : unbounded_string ) is
  ci : compressionInfo;
  linePos : integer;
  firstLinePos : integer;
  lastLinePos : integer;
  line2compile : unbounded_string;
begin
  if script /= null then                                      -- discard script
     free( script );
  end if;

  cmdpos := firstScriptCommandOffset; -- Reset cmdpos to beginning of script

  ci.compressedScript := null_unbounded_string;
  SourceLineNoLo := 0;
  SourceLineNoHi := 0;

  -- Find lines and compress each in turn

  linePos := 1;
  firstLinePos := linePos;
  -- Null command?
  if linepos > length( command ) then
    line2ByteCode( ci, null_unbounded_string );
  end if;
  -- Multiple lines
  while linepos <= length( command ) and not error_found loop -- anything left?
    loop
      exit when element( command, linePos ) = ASCII.LF; -- UNIX/Linux EOL
      exit when element( command, linePos ) = ASCII.CR; -- DOS/Apple EOL
      linePos := linePos + 1;                            -- next character
      exit when error_found;                             -- quit on err
      exit when linePos > length( command );             -- if not beyond EOF
    end loop;
    lastLinePos := linePos - 1;                          -- back up one
    line2compile := to_unbounded_string( slice( command, firstLinePos, lastLinePos ) );
    line2ByteCode( ci, line2compile );                   -- compress that slice
    if element( command, lastLinePos ) = ' ' or element( command, lastLinePos ) = ASCII.HT then
       if not maintenanceOpt then
          err_tokenize( "trailing whitespace at end of line", to_string( line2compile ) );
       end if;
    end if;
    -- DOS text files have CR+LF
    if  linePos < length( command ) then
        if element( command, linePos ) = ASCII.CR then
            if element( command, linePos+1 ) = ASCII.LF then   -- skip extra LF
               linePos := linePos + 1;
            end if;
       end if;
    end if;
    linePos := linePos+1;                                -- skip term char
    firstLinePos := linePos;
  end loop;

  nextByteCodeLine;

  -- Verbose? Show the byte code

  if verboseOpt then
     dumpByteCode( ci );
  end if;

  script := new string( 1..length( ci.compressedScript ) );   -- alloc script
  script.all := to_string( ci.compressedScript );             -- and copy
  identifiers( source_info_script_size_t ).value.all := delete( to_unbounded_string( script.all'length'img), 1, 1 );
end compileInclude;


-----------------------------------------------------------------------------
--  COMPILE COMMAND OR TEMPLATE
--
-- Compile into byte code a command typed interactively at the command prompt
-- or backquotes or templates.
-----------------------------------------------------------------------------

procedure compileCommandOrTemplate( command : unbounded_string ) is
  ci : compressionInfo;
  linePos : integer;
  firstLinePos : integer;
  lastLinePos : integer;
  line2compile : unbounded_string;
begin
  if script /= null then                                      -- discard script
     free( script );
  end if;

  cmdpos := firstScriptCommandOffset; -- Reset cmdpos to beginning of script

  beginByteCode( ci );

  -- Find lines and compress each in turn

  linePos := 1;
  firstLinePos := linePos;
  -- Null command?
  if linepos > length( command ) then
    line2ByteCode( ci, null_unbounded_string );
  end if;
  -- Multiple lines
  while linepos <= length( command ) and not error_found loop -- anything left?
    loop
      exit when element( command, linePos ) = ASCII.LF; -- UNIX/Linux EOL
      exit when element( command, linePos ) = ASCII.CR; -- DOS/Apple EOL
      linePos := linePos + 1;                            -- next character
      exit when error_found;                             -- quit on err
      exit when linePos > length( command );             -- if not beyond EOF
    end loop;
    lastLinePos := linePos - 1;                          -- back up one
    line2compile := to_unbounded_string( slice( command, firstLinePos, lastLinePos ) );
    line2ByteCode( ci, line2compile );                   -- compress that slice
    -- DOS text files have CR+LF
    if  linePos < length( command ) then
        if element( command, linePos ) = ASCII.CR then
            if element( command, linePos+1 ) = ASCII.LF then   -- skip extra LF
               linePos := linePos + 1;
            end if;
       end if;
    end if;
    linePos := linePos+1;                                -- skip term char
    firstLinePos := linePos;
  end loop;

  nextByteCodeLine;
  endByteCode( ci );

  -- Verbose? Show the byte code

  if verboseOpt then
     dumpByteCode( ci );
  end if;

  script := new string( 1..length( ci.compressedScript ) );   -- alloc script
  script.all := to_string( ci.compressedScript );             -- and copy
  identifiers( source_info_script_size_t ).value.all := delete( to_unbounded_string( script.all'length'img), 1, 1 );
end compileCommandOrTemplate;


-----------------------------------------------------------------------------
--  COMPILE TEMPLATE
--
-- Compile into byte code a command from a template.  Set the line number
-- to reflect the template file line number.
-----------------------------------------------------------------------------

procedure compileTemplate( command : unbounded_string; lineno : natural ) is
begin
  setByteCodeLine( lineno );
  compileCommandOrTemplate( command );
end compileTemplate;


-----------------------------------------------------------------------------
--  COMPILE COMMAND
--
-- Compile into byte code a command typed interactively at the command prompt
-- or backquotes or templates.
-----------------------------------------------------------------------------

procedure compileCommand( command : unbounded_string; firstLineNo : natural := 1 ) is
begin
  setByteCodeLine( firstLineNo-1 );
  --SourceLineNoLo := 0;
  --SourceLineNoHi := 0;
  compileCommandOrTemplate( command );
end compileCommand;


-----------------------------------------------------------------------------
--  COMPILE SCRIPT
--
-- Compile into byte code a command loaded from a script file.
-----------------------------------------------------------------------------

procedure compileScript( firstLine : unbounded_string ) is
  ci : compressionInfo;
  command : aliased unbounded_string := firstLine;
  compileDone : boolean := false;
  lastLineNumber : natural := 0;
begin
  SourceLineNoLo := 0;
  SourceLineNoHi := 0;
  if script /= null then                                      -- discard script
     free( script );
  end if;

  cmdpos := firstScriptCommandOffset; -- Reset cmdpos to beginning of script
  SourceLineNoLo := 0; -- Reset line number
  SourceLineNoHi := 0;

  beginByteCode( ci );

  -- parser loads first line...check for "#!" signature line and ignore it
  if length( command ) > 0 then
     if element( command, 1 ) = '#' then
        compileDone := not LineRead( command'access );        -- quit when done
        SourceLineNoLo := 1;                                  -- skip 1 line
     end if;
  end if;
  if verboseOpt then
     put_line( standard_error, "=> (Line 1 ...)" );
  end if;

  -- compile the script into byte code
  while not compileDone loop                                  -- for all lines
     if verboseOpt then
        if getByteCodeLineNo >= lastLineNumber + 500 then
           lastLineNumber := getByteCodeLineNo;
           put_line( standard_error, to_string( term( up ) & "=> (Line" & lastLineNumber'img & " ...)" ) );
        end if;
     end if;
     line2ByteCode( ci, command );                            -- compress line
     -- check for white space at end-of-line.  Do line2ByteCode first because
     -- it increments the line number so line number is accurate.
     declare
       i  : integer := length( command );
       ch : character;
     begin
       while i > 0 loop
         ch := element( command, i );
         if ch = ' ' or ch = ASCII.HT then
            err_tokenize( "trailing whitespace at end-of-line", to_string( command ) );
         elsif ch /= ASCII.CR and ch /= ASCII.LF then
            exit;
         end if;
         i := i - 1;
       end loop;
     end;
     exit when error_found;                                   -- quit on err
     compileDone := not LineRead( command'access );           -- quit when done
  end loop;
  nextByteCodeLine;
  endByteCode( ci );

  -- Verbose? Show the byte code

  if verboseOpt then
     dumpByteCode( ci );
  end if;
  script := new string( 1..length( ci.compressedScript ) );  -- alloc script
  script.all := to_string( ci.compressedScript );            -- and copy
  identifiers( source_info_script_size_t ).value.all := delete( to_unbounded_string( script.all'length'img), 1, 1 );
end compileScript;


-----------------------------------------------------------------------------
--  COPY BYTE CODE LINES
--
-- Copy the byte code lines containing point1 through point2.
-----------------------------------------------------------------------------

function copyByteCodeLines( point1, point2 : natural ) return string is
  line_firstpos : natural;                           -- start of compiled lines
  line_lastpos  : natural;                           -- end of compiled lines
begin

  -- Script unexpectedly null?  Print a message an let an exception be raised
  -- later.

  if script = null then
     put_line( standard_error, Gnat.Source_Info.Source_Location & ": internal_error: copyByteCodeLines: script is null" );
     return "";
  end if;

  -- Invalid range test

  if point1 > point2 then
     put_line( standard_error, Gnat.Source_Info.Source_Location & ": internal error: copyByteCodeLines: point1 " & point1'img & " is greater than point2 " & point2'img );
     return "";
  end if;

  -- point2 has an insane value?  Print a message and let an exception be
  -- raised later.

  if point2 > script'length then
     put_line( standard_error, Gnat.Source_Info.Source_Location & ": internal_error: copyByteCodeLines: cmdpos " & cmdpos'img & " is greater than length of script " & script'length'img );
     return "";
  end if;

  -- Prepare to find the start and end of the command line

  line_firstpos := point1;
  line_lastpos := point2;

  -- find beginning and end of command line
  -- (as it appears in the byte code)

  if line_firstpos > 1 then                          -- sane value?
     line_firstpos := line_firstpos - 1;             -- search for previous
     while script( line_firstpos ) /= ASCII.NUL loop -- ASCII.NUL
           line_firstpos := line_firstpos - 1;
     end loop;
  end if;
  if line_lastpos <= script'length then              -- sane value?
     while script( line_lastpos ) /= ASCII.NUL loop  -- look for next
       line_lastpos := line_lastpos + 1;             -- ASCII.NUL
     end loop;                                       -- or this one if
  end if;                                            -- on one
  line_firstpos := line_firstpos + 1;

  -- cut the lines

  return script( line_firstpos..line_lastpos );

end copyByteCodeLines;


-----------------------------------------------------------------------------
--  STATIC BYTE CODE ANALYSIS
--
-- Determine LOC for the script (including any include files)
--
-- After syntax check, all include files will be loaded into the
-- script.
--
-- This will loop through to the end of the script, counting the
-- end of line markers (ASCII.NUL), etc.
-----------------------------------------------------------------------------

procedure staticByteCodeAnalysis is
   pos : natural := firstScriptCommandOffset;
   -- TODO: these depend on the byte code
   saw_procedure : boolean := false;
   saw_function  : boolean := false;
   saw_one_minus : boolean := false;
   edge_count    : natural := 0;
   loop_count    : natural := 0;
   ch : character;
begin
   while pos < script'last loop
      ch := script( pos );
      if saw_one_minus and ch /= '-' then
         saw_one_minus := false;
      end if;
      if ch = ASCII.NUL then
         perfStats.loc := perfStats.loc + 1;
         -- skip start of line bytes
         pos := pos + 4;
      elsif ch = '-' then
         if saw_one_minus then
            perfStats.numComments := perfStats.numComments + 1;
            saw_one_minus := false;
         else
            saw_one_minus := true;
         end if;
         pos := pos + 1;
      elsif ch = procedure_char then
         saw_one_minus := false;
         -- to deal with forward specifications, note that we saw
         -- the keyword but don't take effect until an "is" is seen.
         -- seeing a new procedure will cancel any previous procedure
         -- or function.
         if saw_procedure then
            saw_procedure := true;
            saw_function := false;
         else
            saw_procedure := true;
         end if;
         pos := pos + 1;
      elsif ch = function_char then
         -- to deal with forward specifications, note that we saw
         -- the keyword but don't take effect until an "is" is seen.
         -- seeing a new procedure will cancel any previous procedure
         -- or function.
         if saw_function then
            saw_procedure := false;
            saw_function := true;
         else
            saw_function := true;
         end if;
         pos := pos + 1;
      elsif ch = begin_char then
         perfStats.numBlocks := perfStats.numBlocks + 1;
         pos := pos + 1;
      elsif ch = type_char then
         -- an "is" occurs here so we cannot count it as a subprogram is
         saw_procedure := false;
         saw_function := false;
         pos := pos + 1;
      elsif ch = subtype_char then
         -- an "is" occurs here so we cannot count it as a subprogram is
         saw_procedure := false;
         saw_function := false;
         pos := pos + 1;
      elsif ch = loop_char then
         -- while and for will be counted here
         loop_count := loop_count + 1;
         pos := pos + 1;
      elsif ch = else_char or
            ch = elsif_char or
            ch = if_char or
            ch = when_char then
         edge_count := edge_count + 1;
         pos := pos + 1;
      elsif script( pos ) = is_char then
         if saw_procedure then
            saw_procedure := false;
            perfStats.numProcs := perfStats.numProcs + 1;
            pos := pos + 1;
         elsif saw_function then
            saw_function := false;
            perfStats.numFuncs := perfStats.numFuncs + 1;
            pos := pos + 1;
         end if;
         pos := pos + 1;
      else
         pos := pos + 1;
      end if;
   end loop;
   -- Ignore ASCII.NUL in sentinal record of the byte code
   perfStats.loc := perfStats.loc + 1;
   -- loop_count -> discount the end loop's.
   edge_count := edge_count + loop_count/2;
   perfStats.numBranches := 1 + edge_count;
end staticByteCodeAnalysis;


-----------------------------------------------------------------------------
-- Housekeeping
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  START COMPILER
--
-- Initialize the compiler.
-----------------------------------------------------------------------------

procedure startCompiler is
    discard_char : character;
begin

  -- SYMBOL TABLE
  --
  -- Initialize the start of the symbol table.
  -- The compiler needs to identify and compress keywords so they must be
  -- declared here, not in the scanner.
  -- All keywords are assumed to compress to one byte by the scanner so
  -- there is an upper limit to how many we can have.

  -- KEYWORD TYPE
  --
  -- The first symbol is the keyword "type", the type of
  -- all keywords

  declareKeyword( keyword_t, "_keyword" );

  -- END OF FILE
  --
  -- The end of file token (must be declared early, used in declarations)

  -- eof_character := toHighASCII( identifiers_top );
  declareKeyword( eof_t, "End of File" );
  toByteCode( eof_t, eof_character, discard_char );
  if discard_char /= ASCII.NUL then
     put_line( standard_error,
       gnat.source_info.source_location &
        ": internal error: eof_t is declared too late" );
  end if;

  -- Global Namespace

  lastNamespaceId     := identifiers'first;
  currentNamespaceId  := lastNamespaceId;
  declareGlobalNamespace;

  -- KEYWORDS
  --
  -- Ada 2012 keywords (that we use)

  declareKeyword( abstract_t, "abstract" );
  declareKeyword( abort_t, "abort" );
  declareKeyword( abs_t, "abs" );
  -- abstract
  declareKeyword( accept_t, "accept" );
  -- access
  -- aliases
  declareKeyword( all_t, "all" );
  declareKeyword( and_t, "and" );
  declareKeyword( array_t, "array" );
  declareKeyword( at_t, "at" );
  declareKeyword( begin_t, "begin" );
  begin_char := character'val( identifiers_top + 126 );
  declareKeyword( body_t, "body" );
  declareKeyword( case_t, "case" );
  declareKeyword( constant_t, "constant" );
  declareKeyword( declare_t, "declare" );
  declareKeyword( delay_t, "delay" );
  -- delta
  -- digits
  declareKeyword( do_t, "do" );
  declareKeyword( else_t, "else" );
  else_char := character'val( identifiers_top + 126 );
  declareKeyword( elsif_t, "elsif" );
  elsif_char := character'val( identifiers_top + 126 );
  declareKeyword( end_t, "end" );
  -- entry
  declareKeyword( exception_t, "exception" );
  declareKeyword( exit_t, "exit" );
  declareKeyword( for_t, "for" );
  declareKeyword( function_t, "function" );
  function_char := character'val( identifiers_top + 126 );
  -- generic
  declareKeyword( goto_t, "goto" );
  declareKeyword( if_t, "if" );
  if_char := character'val( identifiers_top + 126 );
  declareKeyword( in_t, "in" );
  -- interface
  declareKeyword( is_t, "is" );
  is_char := character'val( identifiers_top + 126 );
  declareKeyword( limited_t, "limited" );
  declareKeyword( loop_t, "loop" );
  loop_char := character'val( identifiers_top + 126 );
  declareKeyword( mod_t, "mod" );
  declareKeyword( new_t, "new" );
  declareKeyword( not_t, "not" );
  declareKeyword( null_t, "null" );
  declareKeyword( of_t, "of" );
  declareKeyword( or_t, "or" );
  declareKeyword( others_t, "others" );
  declareKeyword( out_t, "out" );
  -- overriding
  declareKeyword( package_t, "package" );
  declareKeyword( pragma_t, "pragma" );
  declareKeyword( private_t, "private" );
  declareKeyword( procedure_t, "procedure" );
  procedure_char := character'val( identifiers_top + 126 );
  -- protected
  declareKeyword( raise_t, "raise" );
  declareKeyword( range_t, "range" );
  declareKeyword( record_t, "record" );
  declareKeyword( rem_t, "rem" );
  declareKeyword( renames_t, "renames" );
  -- requeue
  declareKeyword( return_t, "return" );
  declareKeyword( reverse_t, "reverse" );
  -- select
  declareKeyword( separate_t, "separate" );
  -- some
  declareKeyword( subtype_t, "subtype" );
  subtype_char := character'val( identifiers_top + 126 );
  -- synchronized
  -- tagged
  declareKeyword( task_t, "task" );
  -- terminate
  declareKeyword( then_t, "then" );
  declareKeyword( type_t, "type" );
  type_char := character'val( identifiers_top + 126 );
  -- until
  declareKeyword( use_t, "use" );
  declareKeyword( when_t, "when" );
  when_char := character'val( identifiers_top + 126 );
  declareKeyword( while_t, "while" );
  declareKeyword( with_t, "with" );
  declareKeyword( xor_t, "xor" );

  declareKeyword( affirm_t, "affirm" );
  declareKeyword( copies_t, "copies" );
  declareKeyword( configuration_t, "configuration" );
  declareKeyword( policy_t, "policy" );

  -- This variable is for limiting searches of the symbol table.  Only
  -- keywords below keyword_top, but there may be more keywords above it.

  keywords_top := identifiers_top;

  -- A punctuation symbol

  declareKeyword( symbol_t, "Punctuation Symbol" );

  -- Universal Types
  --
  -- These must be declared here for use by the literals.  Other
  -- standard types are declared later.

  declareIdent( variable_t, "root variable type", keyword_t );
  declareIdent( uni_numeric_t, "universal_numeric", variable_t, typeClass );
  declareIdent( uni_string_t, "universal_string", variable_t, typeClass );

  -- Literals
  --
  -- These must be declared after the universal types

  declareIdent( backlit_t, "Backquote Literal", uni_string_t );
  declareIdent( strlit_t, "String Literal", uni_string_t );
  declareIdent( charlit_t, "Character Literal", uni_string_t );
  declareIdent( number_t, "Numeric Literal", uni_numeric_t );

  -- Virtual Machine Special Codes
  --
  -- These entries in the symbol table will never be used.
  -- Some of these are expected to compress to one-character codes.

  declareIdent( imm_delim_t, "", symbol_t );
  toByteCode( imm_delim_t, immediate_word_delimiter, discard_char );
  if discard_char /= ASCII.NUL then
     put_line( standard_error, gnat.source_info.source_location & ": internal error: imm_delim_t is declared too late" );
  end if;

  declareIdent( imm_sql_delim_t, "", symbol_t );
  toByteCode( imm_sql_delim_t, immediate_sql_word_delimiter, discard_char );
  if discard_char /= ASCII.NUL then
     put_line( standard_error, gnat.source_info.source_location &": internal error: imm_sql_delim_t is declared too late" );
  end if;

  declareIdent( imm_symbol_delim_t, "", symbol_t );
  toByteCode( imm_symbol_delim_t, immediate_symbol_delimiter, discard_char );
  if discard_char /= ASCII.NUL then
     put_line( standard_error, gnat.source_info.source_location & ": internal error: imm_symbol_t is declared too late" );
  end if;

  declareIdent( char_escape_t, "", symbol_t );
  toByteCode( char_escape_t, high_ascii_escape, discard_char );
  if discard_char /= ASCII.NUL then
     put_line( standard_error, gnat.source_info.source_location & ": internal error: high_ascii_escape is declared too late" );
  end if;

  declareIdent( word_t, "Shell Word", uni_string_t );
  declareIdent( sql_word_t, "SQL Word", uni_string_t );
  declareIdent( shell_symbol_t, "Shell Symbol", uni_string_t );

  -- declareKeyword( load_nr_t, "[Load Numeric Register]" );
  -- declareKeyword( load_sr_t, "[Load String Register]" );
  -- declareKeyword( load_ir_t, "[Load Index Register]" );
  -- declareKeyword( fetch_nr_t, "[Load Numeric Register]" );
  -- declareKeyword( fetch_sr_t, "[Load String Register]" );
  -- declareKeyword( fetch_ir_t, "[Load Index Register]" );

  -- BOURNE SHELL
  --
  -- Built-in Bourne shell-type commands
  -- Note: keep built-ins and SQL together.  They are some statements that
  -- check for built-ins using >= and <=.

  declareProcedure( env_t, "env" );
  declareProcedure( typeset_t, "typeset" );
  declareProcedure( unset_t, "unset" );
  declareProcedure( trace_t, "trace" );
  declareProcedure( help_t, "help" );
  declareProcedure( clear_t, "clear" );
  declareProcedure( jobs_t, "jobs" );
  declareProcedure( logout_t, "logout" );
  declareProcedure( pwd_t, "pwd" );
  declareProcedure( cd_t, "cd" );
  declareProcedure( history_t, "history" );
  declareProcedure( wait_t, "wait" );
  declareProcedure( step_t, "step" );
  -- declareKeyword( template_t, "template" );

  -- SQL
  --
  -- SQL commands that can be used at the SparForte command prompt
  -- These can be declared late as they (mostly) do not appear in scripts
  -- (they can compress to two-byte codes).

  declareKeyword( alter_t, "alter" );
  declareKeyword( insert_t, "insert" );
  declareKeyword( select_t, "select" ); -- doubles with Ada 95 select
  declareKeyword( update_t, "update" );
  declareKeyword( delete_t, "delete" ); -- doubles with text_io.delete

  -- RESERVED
  --
  -- Additional keywords not currently used by SparForte but are in Ada.  We
  -- reserve them.  However, we can declare them later than the other
  -- keywords for performance purposes (they can compress to two-byte
  -- codes).

  declareKeyword( access_t, "access" );
  declareKeyword( aliased_t, "aliased" ); -- Ada 95
  declareKeyword( delta_t, "delta" );
  declareKeyword( digits_t, "digits" );
  declareKeyword( entry_t, "entry" );
  declareKeyword( generic_t, "generic" );
  declareKeyword( interface_t, "interface" );
  declareKeyword( overriding_t, "overriding" );
  declareKeyword( protected_t, "protected" ); -- Ada 95
  declareKeyword( requeue_t, "requeue" ); -- Ada 95
  declareKeyword( synchronized_t, "synchronized" );
  declareKeyword( some_t, "some" );
  declareKeyword( tagged_t, "tagged" );
  declareKeyword( terminate_t, "terminate" );
  declareKeyword( until_t, "until" ); -- Ada 95

  -- remember stack top for last keyword

  reserved_top := identifiers_top;

  -- There is a limit to the maximum number of reserved words.  It used to be
  -- 128 but now is 8191 so we shouldn't be anywhere close to the limit.

  if reserved_top > 8191 then
     raise SPARFORTE_ERROR with Gnat.Source_Info.Source_Location &
       ": internal error: too many reserved words (limit 8191)";
  end if;

  -- Collect reserved words for testing new identifier names

  reserved_words := null_unbounded_string;
  for i in 1..reserved_top-1 loop
      reserved_words := reserved_words & " " & identifiers( i ).name & " ";
  end loop;
end startCompiler;


-----------------------------------------------------------------------------
--  RESET COMPILER
--
-- Do a warm restart of the compiler.
-----------------------------------------------------------------------------

procedure resetCompiler is
begin
  null;
end resetCompiler;


-----------------------------------------------------------------------------
--  SHUTDOWN COMPILER
--
-- Shut down the compiler.
-----------------------------------------------------------------------------

procedure shutdownCompiler is
begin
  null;
end shutdownCompiler;

end compiler;
