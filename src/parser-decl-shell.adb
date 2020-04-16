------------------------------------------------------------------------------
-- AdaScript Language Parser (Bourne Shell)                                 --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with gen_list,
     ada.command_line,
     gnat.directory_operations,
     gnat.regexp,
     gnat.source_info,
     spar_os,
     string_util,
     user_io,
     world,
     scanner,
     parser.decl.as;

use  world,
     ada.command_line,
     gnat.directory_operations,
     gnat.regexp,
     spar_os,
     string_util,
     user_io,
     scanner,
     parser.decl.as;

package body parser.decl.shell is

-- package shellWordLists is new gen_list( shellWord, ">","=" );

-- shellWordList : shellWordLists.List;


-----------------------------------------------------------------------------
--
--  Expansions and Globbing
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  SCAN TILDE
-----------------------------------------------------------------------------

procedure scanTilde( rawWordValue : aShellWord ) is
begin
   null;
end scanTilde;

-----------------------------------------------------------------------------
--  SCAN DOLLAR EXPANSION
-----------------------------------------------------------------------------

procedure scanDollarExpansion( rawWordValue : aShellWord ) is
begin
   null;
end scanDollarExpansion;

-----------------------------------------------------------------------------
--  SCAN GLOB PATTERN
-----------------------------------------------------------------------------

procedure scanGlobPattern( rawWordValue : aShellWord ) is
begin
   null;
end scanGlobPattern;


-----------------------------------------------------------------------------
--
--  Quote handling
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  SCAN BARE QUOTED SHELL WORD
-----------------------------------------------------------------------------

procedure scanBareQuotedShellWord( rawWordValue : aShellWord ) is
begin
   null;
end scanBareQuotedShellWord;

-----------------------------------------------------------------------------
--  SCAN SINGLE QUOTED SHELL WORD
-----------------------------------------------------------------------------

procedure scanSingleQuotedShellWord( rawWordValue : aShellWord ) is
begin
   null;
end scanSingleQuotedShellWord;

-----------------------------------------------------------------------------
--  SCAN DOUBLE QUOTED SHELL WORD
-----------------------------------------------------------------------------

procedure scanDoubleQuotedShellWord( rawWordValue : aShellWord ) is
begin
   null;
end scanDoubleQuotedShellWord;

-----------------------------------------------------------------------------
--  SCAN SHELL WORD
--
-- Treat the token as a shell word.
-- Perform any substitutions, expansions and word splitting.
-----------------------------------------------------------------------------

procedure scanShellWord( rawWordValue : aShellWord ) is
   -- rawWordValue : constant shellWord := unbounded_string(
   --   identifiers( token ).value.all );
begin
   null;
end scanShellWord;


-----------------------------------------------------------------------------
--
--  Public Subprograms
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE NEXT SHELL WORD
--
-- return the next shell word
-----------------------------------------------------------------------------

procedure getNextShellWord is
begin
  --if not shellWordLists.Empty( shellWordList ) then
  --   shellWordLists.Pull( shellWordList, shellWord );
  --else
  --   getNextToken;
  --   scanShellWord( shellWord );
  --end if;
null;
end getNextShellWord;



-----------------------------------------------------------------------------
--
--  Legacy Subprograms
--
-----------------------------------------------------------------------------



-----------------------------------------------------------------------------
--  PARSE SHELL WORD
--
-- Parse and expand a shell word argument.  Return a shellWordList containing
-- the original pattern, the expanded words and their types.  If the first is
-- true, the word should be the first word.  If there is already shell words
-- in the list, any new words will be appended to the list.  The caller is
-- responsible for clearing (deallocating) the list.
--   Expansion is the process of performing substitutions on a shell word.
--
-- Bourne shell expansions include:
--   TYPE                  PATTERN        EX. WORDS        STATUS
--   Brace expansion       a{.txt,.dat}   a.txt a.dat      not implemented
--   Tilde expansion       ~/a.txt        /home/ken/a.txt  OK
--   Variable expansion    $HOME/a.txt    /home/ken/a.txt  no special $
--   Command substituion   `echo a.txt`   a.txt            no $(...)
--   Arithmetic expansion  -              -                not implemented
--   Word splitting        a\ word        "a word"         OK
--   Pathname expansion    *.txt          a.txt b.txt      OK
--
-- Since Spar has to interpret the shell words as part of the byte code
-- compilation, word splitting before pathname expansion.  This means that
-- certain rare expansions will have different results in Spar than in a
-- standard Bourne shell.  (Some might call this an improvement over the
-- standard.)  Otherwise, Spar conforms to the Bourne shell standard.
--
-- UsedEscape is true if the shell word was escaped
--
-- The wordType is used to differentiate between words like "|" (a string)
-- and | (the pipe operator) which look the same once quotes are removed.
-----------------------------------------------------------------------------

procedure ParseShellWord( wordList : in out shellWordList.List; First : boolean := false ) is
-- TODO: first is not implemented

-- these should be global
semicolon_string : constant unbounded_string := to_unbounded_string( ";" );
--   semi-colon, as an unbounded string

verticalbar_string : constant unbounded_string := to_unbounded_string( "|" );
--   vertical bar, as an unbounded string

ampersand_string : constant unbounded_string := to_unbounded_string( "&" );
--   ampersand, as an unbounded string

redirectIn_string : constant unbounded_string := to_unbounded_string( "<" );
--   less than, as an unbounded string

redirectOut_string : constant unbounded_string := to_unbounded_string( ">" );
--   greater than, as an unbounded string

redirectAppend_string : constant unbounded_string := to_unbounded_string( ">>" );
--   double greater than, as an unbounded string

redirectErrOut_string : constant unbounded_string := to_unbounded_string( "2>" );
--   '2' + greater than, as an unbounded string

redirectErrAppend_string : constant unbounded_string := to_unbounded_string( "2>>" );
--   '2' + double greater than, as an unbounded string

redirectErr2Out_string : constant unbounded_string := to_unbounded_string( "2>&1" );
--   '2' + greater than + ampersand and '1', as an unbounded string

itself_string : constant unbounded_string := to_unbounded_string( "@" );
--   itself, as an unbounded string

  ch          : character;
  inSQuote    : boolean := false;                      -- in single quoted part
  inDQuote    : boolean := false;                      -- in double quoted part
  inBQuote    : boolean := false;                      -- in double quoted part
  inBackslash : boolean := false;                      -- in backquoted part
  inDollar    : boolean := false;                      -- $ expansion
  inDollarBraces : boolean := false;                   -- ${ expansion
  inDollarParen : boolean := false;                    -- $( expansion
  wasSQuote   : boolean := false;                      -- is $ expan in sin qu
  wasDQuote   : boolean := false;                      -- is $ expan in dbl qu
  wasBQuote   : boolean := false;                      -- is $ expan in bck qu
  expansionVar: unbounded_string;                      -- the $ name
  escapeGlobs : boolean := false;                      -- escaping glob chars
  ignoreTerminatingWhitespace : boolean := false;                 -- SQL word has whitespace in it (do not use whitespace as a word terminator)
  expandInSingleQuotes : boolean := false;  -- SQL words allow $ expansion for single quotes (for PostgreSQL)
  stripQuoteMarks : boolean := true; -- SQL words require quotes words left in the word
  startOfBQuote : integer;

  addExpansionSQuote : boolean := false;               -- SQL, add ' after exp
  addExpansionDQuote : boolean := false;               -- SQL, add " after exp
  temp_id     : identifier;                            -- for ~ processing
  shell_word  : unbounded_string;

  word        : unbounded_string;
  wordLen     : integer;
  pattern     : unbounded_string;
  wordType    : aShellWordType;

    --  SIMPLE COMMAND SUBSTITUTION
    --
    -- to run this backquoted shell word, we need to save the current
    -- script, compile the command into byte code, and run the commands
    -- while capturing the output.  Substitute the results into the
    -- shell word and restore the original script.
    -- tempStr is the command to run in the back quotes.
    --------------------------------------------------------------------------

    function simpleCommandSubstitution( originalCommands : unbounded_string ) return unbounded_string is
       commands : unbounded_string := originalCommands;
       commandsOutput : unbounded_string;
    begin
       -- put_line( "commands = " & to_string( commands ) );
       if commands = "" or commands = " " then
          err( "no commands to run" );
       else

          -- If the backquoted commands don't end with a semi-colon, add one.
          -- There is a chance that the semi-colon could be hidden by a
          -- comment symbol (--).

          if tail( commands, 1 ) /= ";" then
             commands := commands & ";";
          end if;

          -- Run the command and attach the output to the word we are
          -- assembling.
          CompileRunAndCaptureOutput( commands, commandsOutput );
          -- put_line( "PSW: res  = " & to_string(commandsOutput) );
      end if;
      return commandsOutput;
    end simpleCommandSubstitution;

  --  BACKQUOTE SUBSTITUTION
  --
  ----------------------------------------------------------------------------

  procedure backquoteSubstitution is
     tempStr : unbounded_string := unbounded_slice( word, startOfBQuote+1, length( word ) );
  begin
     -- remove the command from the end of the word assembled so far

     delete( word, startOfBQuote+1, length( word ) );
     -- put_line( "PSW: word (2) = '" & word & "'" );

     word := word & simpleCommandSubstitution( tempStr );
     -- put_line( "backquoteSubstitution: word is """ & word & """" ); -- DEBUG
  end backquoteSubstitution;

  --  DOLLAR EXPANSION (parseShellWord)
  --
  -- perform a dollar expansion by appending a variable's value to the
  -- shell word.  Also handles dollar brace expansions.
  ---------------------------------------------------------------------------

  procedure dollarExpansion is
     subword : unbounded_string;
     ch      : character;

     --  SIMPLE DOLLAR EXPANSION
     --
     -- A dollar expansion given a specific variable name.  Also handle $0..$9,
     -- $#, $?, $$, $!.
     -------------------------------------------------------------------------

     procedure simpleDollarExpansion( expansionVar : unbounded_string ) is
       id      : identifier;
     begin
       if expansionVar = "#" then
          if isExecutingCommand then
             subword := to_unbounded_string( integer'image( Argument_Count-optionOffset) );
             delete( subword, 1, 1 );
          end if;
       elsif expansionVar = "?" then
          if isExecutingCommand then
             subword := to_unbounded_string( last_status'img );
             delete( subword, 1, 1 );
          end if;
       elsif expansionVar = "$" then
          if isExecutingCommand then
             subword := to_unbounded_string( aPID'image( getpid ) );
             delete( subword, 1, 1 );
          end if;
       elsif expansionVar = "!" then
          if isExecutingCommand then
             subword := to_unbounded_string( aPID'image( lastChild ) );
             delete( subword, 1, 1 );
          end if;
       elsif expansionVar = "0" then
          if isExecutingCommand then
             subword := to_unbounded_string( Ada.Command_Line.Command_Name );
          end if;
       elsif length( expansionVar ) = 1 and (expansionVar >= "1" and expansionVar <= "9" ) then
          if syntax_check and then not suppress_word_quoting and then not inDQuote then
             err( "style issue: expected double quoted word parameters in shell or SQL command to stop word splitting" );
          end if;
          if isExecutingCommand then
             begin
                subword := to_unbounded_string(
                    Argument(
                      integer'value(
                        to_string( " " & expansionVar ) )+optionOffset ) );
             exception when program_error =>
                err( "program_error exception raised" );
             when others =>
                err( "script argument " & to_string(expansionVar) & " not found " &
                     "in arguments 0 .." &
                     integer'image( Argument_Count-optionOffset) );
             end;
          end if;
       else
          findIdent( expansionVar, id );
          if id = eof_t then
             -- TODO: this check takes place after the token is read, so token
             -- following the one in question is highlighted
             err( optional_bold( to_string( expansionVar ) ) & " not declared" );
          else
             if syntax_check then
                identifiers( id ).wasReferenced := true;
                --identifiers( id ).referencedByThread := getThreadName;
                subword := to_unbounded_string( "undefined" );
             else
                subword := identifiers( id ).value.all;     -- word to substit.
                if not inDQuote then                        -- strip spaces
                   subword := Ada.Strings.Unbounded.Trim(   -- unless double
                      subword, Ada.Strings.Both );          -- quotes;
                elsif getUniType( id ) = uni_numeric_t then -- a number?
                   if length( subword ) > 0 then            -- something there?
                      if element( subword, 1 ) = ' ' then   -- leading space
                         delete( subword, 1, 1 );           -- we don't want it
                      end if;
                   end if;
                end if;
             end if;
          end if;
       end if;
    end simpleDollarExpansion;

    procedure simpleLengthExpansion( expansionVar : unbounded_string ) is
       id      : identifier;
    begin
       findIdent( expansionVar, id );
       if id = eof_t then
          -- TODO: this check takes place after the token is read, so token
          -- following the one in question is highlighted
          err( optional_bold( to_string( expansionVar ) ) & " not declared" );
       else
          if syntax_check then
             identifiers( id ).wasReferenced := true;
             --identifiers( id ).referencedByThread := getThreadName;
             subword := to_unbounded_string( "1" );
          else
             subword := identifiers( id ).value.all;     -- word to substit.
             subword := to_unbounded_string( natural'image( length( subword ) ) );
             if not inDQuote then                        -- strip spaces
                subword := Ada.Strings.Unbounded.Trim(   -- unless double
                   subword, Ada.Strings.Both );          -- quotes;
             elsif element( subword, 1 ) = ' ' then   -- leading space
                   delete( subword, 1, 1 );           -- we don't want it
             end if;
          end if;
       end if;
    end simpleLengthExpansion;

  begin
    --put_line( "dollarExpansion for var """ & expansionVar & """" ); -- DEBUG
    if expansionVar = "" or expansionVar = " " then
       err( "dollar expansion expects a variable name (or escape the $ if not an expansion)" );
    elsif element(expansionVar,1) = '{' then
       if element(expansionVar, length(expansionVar)) /= '}' then
          err( "expected closing } in " & to_string(toEscaped(expansionVar) ) );
       elsif element(expansionVar,2) = '#' then
          -- string length
          simpleLengthExpansion( unbounded_slice( expansionVar, 3, length(expansionVar)-1 ) );
       else
          simpleDollarExpansion( unbounded_slice( expansionVar, 2, length(expansionVar)-1 ) );
       end if;
    elsif element(expansionVar,1) = '(' then
       if element(expansionVar, length(expansionVar)) /= ')' then
          err( "expected closing ) in " & to_string(toEscaped(expansionVar) ) );
       else
          subword := simpleCommandSubstitution( unbounded_slice( expansionVar, 2, length(expansionVar)-1 ) );
          --put_line( "dollarExpansion: subword is """ & subword & """" ); -- DEBUG
       end if;
    else
       if syntax_check and then not suppress_word_quoting and then not inDQuote then
          err( "style issue: expected double quoted word parameters in shell or SQL command to prevent word splitting" );
       end if;
       simpleDollarExpansion( expansionVar );
    end if;
    -- escapeGlobs affects the variable substitution
    -- shell word will be "undefined" during syntax check.  It only has
    -- a meaningful value at run-time.
    for i in 1..length( subword ) loop                  -- each letter
        ch := element( subword, i );                    -- get it
        if escapeGlobs and not inBackslash then         -- esc glob chars?
           case ch is                                   -- is a glob char?
           when '*' => pattern := pattern & "\";        -- escape *
           when '[' => pattern := pattern & "\";        -- escape [
           when '\' => pattern := pattern & "\";        -- escape \
           when '?' => pattern := pattern & "\";        -- escape *
           when others => null;                         -- others? no esc
           end case;
        end if;
        pattern := pattern & ch;                        -- add the letter
        word := word & ch;                              -- add the letter
    end loop;
    --put_line( "dollarExpansion: word is """ & word & """" ); -- DEBUG
    inDollar := false;       -- TODO: recursion not handled
    inDollarBraces := false;
    inDollarParen := false;
  -- SQL words require the quote marks to be left intact in the word.
  -- Unfortunately, this has to be checked after the quote character has
  -- been processed.  This checks for the flag variables to attach a quote
  -- mark retroactively.
    if addExpansionSQuote then
        word := word & "'";
        addExpansionSQuote := false;
    end if;
    if addExpansionDQuote then
        word := word & ASCII.Quotation;
        addExpansionDQuote := false;
    end if;
  end dollarExpansion;

  --  PATHNAME EXPANSION (parseShellWord)
  --
  -- Perform shell pathname expansion by using the shell word as a glob
  -- pattern and searching the current directory.  Return a list of shell
  -- words created by the expansion.
  --
  -- Note: file name length is limited to 256 characters.
  ---------------------------------------------------------------------------

  procedure pathnameExpansion( word, pattern : unbounded_string; list : in out shellWordList.List ) is
    globCriteria : regexp;
    currentDir   : Dir_Type;
    fileName     : string(1..256);
    fileNameLen  : natural;
    found        : boolean := false;
    noPWD        : boolean := false;
    dirpath      : string := to_string( dirname( word ) );
    globexpr     : constant string := to_string( basename( pattern ) );
    noDir        : boolean;
    isOpen       : boolean := false;
  begin
    -- put_line( "pathnameExpansion for original pattern """ & pattern & """" ); -- DEBUG
    -- put_line( "pathnameExpansion for expanded word """ & word & """" ); -- DEBUG
    -- put_line( "wasDQuote: " & wasDQuote'img ); -- DEBUG
    -- In the case of a syntax check, return the word as-is as a place holder.
    -- Don't try to glob it.
    if syntax_check then
       shellWordList.Queue( wordList, aShellWord'( normalWord, pattern, shell_word ) );
       return;
    end if;
    -- word is an empty string? it still counts: param is a null string
    if length( pattern ) = 0 or length( word ) = 0 then
       shellWordList.Queue( list, aShellWord'( normalWord, pattern, null_unbounded_string ) );
       return;
    end if;
    -- otherwise, prepare to glob the current directory
    noDir := globexpr = pattern;
    globCriteria := Compile( globexpr, Glob => true, Case_Sensitive => true );
    begin
      open( currentDir, dirpath );
      isOpen := true;
    exception when others =>
      noPWD := true;
    end;
    -- is the current directory invalid? then param is just the word
    if noPWD then
       shellWordList.Queue( list, aShellWord'( normalWord, pattern, word ) );
       return;
    end if;
    -- Linux/UNIX: skip "." and ".." directory entries
    --read( currentDir, fileName, fileNameLen );
    --read( currentDir, fileName, fileNameLen );
    -- search the directory, adding files that match the glob pattern
    loop
      read( currentDir, fileName, fileNameLen );
      -- KB: 12/02/18 - no longer returns "." and "..".  Commented out reads
      -- but as a safety precaution check the filename here.
      exit when fileNameLen = 0;
      if filename( 1..fileNameLen ) = "." then
         null;
      elsif filename( 1..fileNameLen ) = ".." then
         null;
      elsif Match( fileName(1..fileNameLen ) , globCriteria ) then
         if noDir then
            shellWordList.Queue( list, aShellWord'(
               normalWord,
               pattern,
               to_unbounded_string( fileName( 1..fileNameLen ) ) ) );
         else
            -- root directory?  no need to add directory delimiter
            if dirpath'length = 1 and dirpath(1) = directory_delimiter then
               shellWordList.Queue( list, aShellWord'(
                  normalWord,
                  pattern,
                  to_unbounded_string( dirpath & fileName( 1..fileNameLen ) ) ) );
            else
               shellWordList.Queue( list, aShellWord'(
                  normalWord,
                  pattern,
                  to_unbounded_string( dirpath & directory_delimiter & fileName( 1..fileNameLen ) ) ) );
            end if;
         end if;
         found := true;
      end if;
    end loop;
    -- there are no matches? word still counts: the param is just the word
    if not found then
       shellWordList.Queue( list, aShellWord'( normalWord, pattern, word ) );
    end if;
    if isOpen then
       close( currentDir );
    end if;
  exception when ERROR_IN_REGEXP =>
    -- The globbing expression may be bad.  For example,
    -- '/</{ :loop s/<[^>]*>//g /</{ N b loop } }'
    -- will be split into ... / globExpr = { N b loop } }
    -- which will fail with this exception.  There's no way to know if the
    -- expression produced by basename will be glob-able.  So this is not
    -- an error...there's just nothing to glob.
    --
    -- Was:
    -- err( "error in globbing expression """ & globExpr & """" );
    --
    -- Now, queue the shell word as the word just counts as-is.
    shellWordList.Queue( list, aShellWord'( normalWord, pattern, word ) );
    if isOpen then
       close( currentDir );
    end if;
  when DIRECTORY_ERROR =>
    err( "directory error on directory " & toSecureData(dirPath));
  end pathnameExpansion;

  --  PATHNAME EXPANSION WITH IFS (parseShellWord)
  --
  -- Breakup barewords into subwords and do pathname expansion (that is, apply
  -- globbing pattern and get applicable files).
  -- Quoted words pathname expanded as-is
  -- TODO: is this too high?  Probably goes lower in the logic. What about the pattern?
  ---------------------------------------------------------------------------

  procedure pathnameExpansionWithIFS( word, pattern : unbounded_string; list : in out shellWordList.List ) is
    subword    : unbounded_string;
    subpattern : unbounded_string;
    ch         : character;
    word_pos   : natural := 1;
    pattern_pos: natural := 1;
    isBackslash: boolean;
  begin
    -- put_line( "pathnameExpansionWithIFS for original pattern """ & pattern & """" ); -- DEBUG
    -- put_line( "pathnameExpansionWithIFS for expanded word """ & word & """" ); -- DEBUG
    -- put_line( "wasBQuote: " & wasBQuote'img ); -- DEBUG
    -- if in double quotes, then no IFS handling
    if wasDQuote then
       pathnameExpansion( word, pattern, list );
    elsif wasSQuote then
       shellWordList.Queue( list, aShellWord'( normalWord, pattern, word ) );
    elsif wasBQuote then
       shellWordList.Queue( list, aShellWord'( normalWord, pattern, word ) );
    elsif length( pattern ) = 0 or length( word ) = 0 then
       pathnameExpansion( word, pattern, list );
    else
       -- If this is a bareword, break up each piece separated by whitespace
       -- into separate worders to be handled individually.
       while word_pos <= length( word ) loop
          -- skip leading whitespace
          while word_pos <= length( word ) loop
             ch := element( word, word_pos );
             exit when ch /= ASCII.HT and ch /= ' ';
             word_pos := word_pos + 1;
          end loop;
          -- handle word and backslash characters
          while word_pos <= length( word ) loop
             ch := element( word, word_pos );
             exit when ch = ASCII.HT or ch = ' ';
             subword := subword & ch;
             word_pos := word_pos + 1;
          end loop;
          -- skip leading whitespace in pattern
          while pattern_pos <= length( pattern ) loop
             ch := element( pattern, pattern_pos );
             exit when ch /= ASCII.HT and ch /= ' ';
             pattern_pos := pattern_pos + 1;
          end loop;
          -- break up the pattern but honour backslashes
          isBackslash := false;
          while pattern_pos <= length( pattern ) loop
             ch := element( pattern, pattern_pos );
             if ch = '\' then
                isBackslash := true;
             elsif isBackslash then
               isBackslash := false;
             else
               exit when ch = ASCII.HT or ch = ' ';
             end if;
             subpattern := subpattern & ch;
             pattern_pos := pattern_pos + 1;
          end loop;
          -- expand the whitespace delinated subword
          pathnameExpansion( subword, subpattern, list );
          subword := null_unbounded_string;
          subpattern := null_unbounded_string;
          word_pos := word_pos + 1;
          pattern_pos := pattern_pos + 1;
       end loop;
    end if;
  end pathnameExpansionWithIFS;

begin

  word := null_unbounded_string;
  pattern := null_unbounded_string;
  wordType := normalWord;

  -- Get the next unexpanded word.  A SQL command is a special case: never
  -- expand it.  We don't want the * in "select *" to be replaced with a list
  -- of files.

  -- ignoreTerminatingWhitespace is a workaround and should be redone.  We
  -- need one word but expand the items in the word (for SQL).

  if token = sql_word_t then
     shell_word := identifiers( token ).value.all;
     ignoreTerminatingWhitespace := true;                -- word contains space
     expandInSingleQuotes := true;
     stripQuoteMarks := false;
  else
     -- Otherwise, get a non-SQL shell word
     ParseBasicShellWord( shell_word );
  end if;

  wordLen := length( shell_word ); -- we refer a lot to the length

  -- Null string shell word?  Then nothing to do.

  if wordLen = 0 then
     word := null_unbounded_string;
     pattern := null_unbounded_string;
     wordType := normalWord;
     shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
     getNextToken;
     return;
  end if;

  -- Special Cases
  --
  -- The special shell words are always unescaped and never expand.  We handle
  -- them as special cases before beginning the expansion process.

  ch := Element( shell_word, 1 );                      -- next character

  if ch = ';' then                                     -- semicolon?
     word := semicolon_string;                         -- type
     pattern := semicolon_string;
     wordType := semicolonWord;
     shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
     getNextToken;
     return;

  elsif ch = '|' then                                  -- vertical bar?
     word := verticalbar_string;
     pattern := verticalbar_string;
     wordType := pipeWord;
     shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
     getNextToken;
     return;

  elsif ch = '&' then                                  -- ampersand?
     word := ampersand_string;                         -- type
     pattern := ampersand_string;
     wordType := ampersandWord;
     shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
     getNextToken;
     return;

  elsif ch = '<' then                                  -- less than?
     word := redirectIn_string;                        -- type
     pattern := redirectIn_string;
     wordType := redirectInWord;
     shellWordList.Queue( wordList, aShellWord'( wordtype, pattern, word ) );
     if wordLen > length( word ) then
        err( "unexpected characters after redirection " & to_string(word) );
     end if;
     getNextToken;
     return;

  elsif ch = '>' then                                  -- greater than?

     if wordLen > 1 and then Element(shell_word, 2 ) = '>' then -- double greater than?
        word := redirectAppend_string;                 -- type
        pattern := redirectAppend_string;
        wordType := redirectAppendWord;
        shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
        if wordLen > length( word ) then
           err( "unexpected characters after redirection " & to_string(word) );
        end if;
        getNextToken;
        return;
     end if;
     word := redirectOut_string;                       -- it's redirectOut
     pattern := redirectOut_string;
     wordType := redirectOutWord;
     shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
     if wordLen > length( word ) then
        err( "unexpected characters after redirection " & to_string(word) );
     end if;
     getNextToken;
     return;

  elsif ch = '2' and then wordLen > 1 and then Element(shell_word, 2 ) = '>' then -- 2+greater than?
     if wordLen > 2 and then Element( shell_word, 3  ) = '&' then            -- fold error into out?
        if wordLen > 3 and then Element( shell_word, 4 ) = '1' then
           word := redirectErr2Out_string;              -- type
           pattern := redirectErr2Out_string;
           wordType := redirectErr2OutWord;
           shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
           if wordLen > length( word ) then
              err( "unexpected characters after redirection " & to_string(word) );
           end if;
           getNextToken;
           return;
        end if;
     elsif wordLen > 2 and then Element( shell_word, 3 ) = '>' then -- double greater than?
        word := redirectErrAppend_string;               -- it's redirectErrApp
        pattern := redirectErrAppend_string;
        wordType := redirectErrAppendWord;
        shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
        if wordLen > length( word ) then
           err( "unexpected characters after redirection " & to_string(word) );
        end if;
        getNextToken;
        return;
     end if;
     word := redirectErrOut_string;                     -- it's redirectErrOut
     pattern := redirectErrOut_string;
     wordType := redirectErrOutWord;
     shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
     if wordLen > length( word ) then
        err( "unexpected characters after redirection " & to_string(word) );
     end if;
     getNextToken;
     return;

  elsif ch = '@' then                                   -- itself?
     word := itself_string;                             -- it's an itself type
     pattern := itself_string;
     wordType := itselfWord;
     shellWordList.Queue( wordList, aShellWord'( wordType, pattern, word ) );
     getNextToken;
     return;

  end if;

  -- There are times when we don't want to expand the word.  In the case of
  -- a syntax check, return the word as-is as a place holder (not sure if
  -- it's necessary but "> $path" becomes ">" with no path otherwise...at
  -- least, it's easier for the programmer to debug.
  --  However, to trace whether variables are referenced.

  if error_found then                                 -- error:
     getNextToken;
     return;                                          -- no expansions
  --elsif syntax_check then                             -- chk?
  --   shellWordList.Queue( wordList, aShellWord'( normalWord, pattern, shell_word ) );
  --   return;                                          -- just the word
  end if;                                             -- as a place holder

  ---------------------------------------------------------------------------
  -- We have a word.  Perform the expansion: process quotes and other escape
  -- characters, possibly creating multiple words from one original pattern.
  ---------------------------------------------------------------------------

  temp_id := eof_t; -- for tilde expansion

  -- Expand any quotes quotes and handle shell variable substitutions

  for i in 1..length( shell_word ) loop
    ch := Element( shell_word, i );                          -- next character

    -- tilde expansion must only occur in an unescaped word and not within a
    -- dollar expansion.  The tilde expansion is only valid for the first
    -- character in the word

    if ch = '~' and i = 1 and not inSQuote and not inDQuote and not inBackslash then
       if temp_id = eof_t then
          findIdent( to_unbounded_string( "HOME" ), temp_id ); -- find HOME var
       end if;
       word := word & to_string( identifiers( temp_id ).value.all );  -- replace w/HOME
       pattern := pattern & ch;  -- TODO: verify this should be ch


       -- Double Quote?  If not escaped by a backslash or single quote,
       -- we're in a new double quote escape.  If we were in a dollar expansion,
       -- perform the expansion.

    --elsif ch = '"' and not inSQuote and not inBackslash then    -- unescaped "?
    elsif ch = '"' and not inSQuote and not inBackslash then    -- unescaped "?
       if inDollar then                                      -- was doing $?
          dollarExpansion;                                    -- complete it
       end if;
       wasDQuote := inDQuote;                                -- remember
       inDQuote := not inDQuote;                             -- toggle " flag
       if not stripQuoteMarks then                           -- SQL word?
          if inDollar then                                   -- in an exp?
             addExpansionDQuote := true;                     -- add after exp
          else                                               -- else
             word := word & """";                            -- add quote now
          end if;
       end if;
       escapeGlobs := inDQuote;                              -- inside? do esc

       -- Single Quote?  If not escaped by a backslash or double quote,
       -- we're in a new single quote escape.  If we were in a dollar expansion,
       -- perform the expansion.

    elsif ch = ''' and not inDQuote and not inBackslash then -- unescaped '?
       if inDollar then                                      -- was doing $?
          dollarExpansion;                                   -- complete it
       end if;
       wasSQuote := inSQuote;                                -- remember
       inSQuote := not inSQuote;                             -- toggle ' flag
       if not stripQuoteMarks then                           -- SQL word?
          if inDollar then                                   -- in an exp?
             addExpansionSQuote := true;                     -- add after exp
          else                                               -- else
             word := word & "'";                             -- add quote now
          end if;
       end if;
       escapeGlobs := inSQuote;                              -- inside? do esc

       -- Back Quote?  If not escaped by a backslash or single quote,
       -- we're in a new back quote escape.  If we were in a dollar expansion,
       -- perform the expansion before executing the back quote.

    elsif ch = '`' and not inSQuote and not inBackslash then -- unescaped `?
       wasBQuote := inBQuote;                                -- remember
       inBQuote := not inBQuote;                             -- toggle ` flag
       if inBQuote and inDollar then                         -- doing $ ere `?
          dollarExpansion;                                   -- complete it
       end if;
       if inBQuote then                                      -- starting?
          startOfBQuote := length( word );                   -- offset to start
       else                                                  -- ending?
          if inDollar then                                   -- in a $?
             dollarExpansion;                                -- finish it
          end if;
          backquoteSubstitution;
       end if;
       escapeGlobs := inBQuote;                            -- inside? do esc

       -- Backslash?  If not escaped by another backslash or single quote,
       -- we're in a new backslash escape.  If we were in a dollar expansion,
       -- perform the expansion.  Keep the backslashes for pathname expansion
       -- but not for SQL words.

    elsif ch = '\' and not inSQuote and not inBackslash then -- unescaped \?
       inBackslash := true;                                -- \ escape
       if inDollar then                                    -- in a $?
          dollarExpansion;                                 -- complete it
       end if;

       pattern := pattern & "\";                           -- an escaping \

       -- Dollar Brace handling

    elsif ch = '{' and inDollar and not inDollarBraces then
       inDollarBraces := true;
       expansionVar := expansionVar & ch;
    elsif inDollarBraces and ch = '}' then
       expansionVar := expansionVar & ch;
       if length( expansionVar ) > 0 then  -- TODO: needed?
          dollarExpansion;
          expansionVar := null_unbounded_string;
       end if;

       -- Dollar Parenthesis handling

    elsif ch = '(' and inDollar and not inDollarParen then
       inDollarParen := true;
       expansionVar := expansionVar & ch;
    elsif inDollarParen and ch = ')' then
       expansionVar := expansionVar & ch;
       if length( expansionVar ) > 0 then  -- TODO: needed?
          dollarExpansion;
          expansionVar := null_unbounded_string;
       end if;

       -- Dollar sign?  Then begin collecting the letters to the substitution
       -- variable.

    elsif ch = '$' and not (inSQuote and not expandInSingleQuotes) and
       not inBackslash and not inDollarBraces and not inDollarParen then
       if inDollar then                                    -- in a $?
          if length( expansionVar ) = 0 then               -- $$ is special
             expansionVar := expansionVar & ch;            -- var is $
             dollarExpansion;                              -- expand it
          else                                             -- otherwise
             dollarExpansion;                              -- complete it
             inDollar := true;                             -- start new one
          end if;
       else                                                -- not in one?
          inDollar := true;                                -- start new one
       end if;
       expansionVar := null_unbounded_string;

    else

       -- End of quote handling...now we have a character, handle it

       -- if name is greater than 1 char, dollarExpansion ends when
       -- non-alpha/digit/underscore is read.  Pass through to allow the
       -- character to otherwise be treated normally.

       if inDollar and not inDollarBraces and not inDollarParen then
          if ch /= '_' and
             ch not in 'A'..'Z' and
             ch not in 'a'..'z' and
             ch not in '0'..'9' then
             if length( expansionVar ) > 0 then  -- TODO: needed?
                dollarExpansion;
             end if;
          end if;
       end if;

       -- Terminating characters (whitespace or semi-colon)

       -- exit when (ch = ' ' or ch = ASCII.HT or ch = ';' or ch = '|' )
       --   and not inDQuote and not inSQuote and not inBQuote and not inBackslash and not ignoreTerminatingWhitespace;
       if (ch = ' ' or ch = ASCII.HT or ch = ';' or ch = '|' )
          and not inDQuote and not inSQuote and not inBQuote and not inBackslash
          and not ignoreTerminatingWhitespace then
          exit;
       end if;

       -- Looking at a $ expansion?  Then collect the letters of the variable
       -- to substitute but don't add them to the shell word.  Apply dollar
       -- expansions to both word and pattern.

       if inDollar then                                    -- in a $?
          expansionVar := expansionVar & ch;               -- collect $ name
       else                                                -- not in $?
          -- When escaping characters that affect globbing, this is only done
          -- for the pattern to be used for globbing.  Do not escape the
          -- characters in the word...this will be the fallback word used if
          -- globbing fails to match any files.
          -- backslash => user already escaped it
          if escapeGlobs and not inBackslash then          -- esc glob chars?
             case ch is                                    -- is a glob char?
             when '*' => pattern := pattern & "\";         -- escape *
             when '[' => pattern := pattern & "\";         -- escape [
             when ']' => pattern := pattern & "\";         -- escape ]
             when '\' => pattern := pattern & "\";         -- escape \
             when '?' => pattern := pattern & "\";         -- escape *
             when others => null;                          -- others? no esc
             end case;
          end if;
          pattern := pattern & ch;                         -- add the char
          word := word & ch;                               -- original word
          if inBackslash then                              -- \ escaping?
             inBackslash := false;                         -- not anymore
          end if;
       end if;
    end if;
  end loop;                                                -- expansions done

  if inDollar then                                      -- last $ not done ?
     dollarExpansion;                                      -- finish it
  end if;

  -- These should never occur because of the tokenizing process, but
  -- to be safe there should be no open quotes.

  if inSQuote then
     err( gnat.source_info.source_location & ": Internal error: missing single quote mark" );
  elsif inDQuote then
     err( gnat.source_info.source_location & ": Internal error: missing double quote mark" );
  end if;

  -- process special characters

  --for i in 1..length( word ) loop
      --if Element( word, i ) = '~' then                      -- leading tilda?
  if length( word ) > 0 then
     if Element( word, 1 ) = '~' then                      -- leading tilda?
        findIdent( to_unbounded_string( "HOME" ), temp_id ); -- find HOME var
        pattern := identifiers( temp_id ).value.all;       -- replace w/HOME
     end if;
  end if;

  --end loop;

  -- Perform pathname expansion (file globbing).  Since the expansion can create
  -- multiple words, this also queues the words in the word list.  If a syntax
  -- check, we don't want to actually scan the disk and expand paths--instead,
  -- a dummy word will be queued and no other action is taken.
  --
  -- If this is a bareword dollar substitution, a second round of IFS (space/
  -- tab processing) must be performed.  Otherwise, the IFS processing in this
  -- procedure is sufficient (doing it twice will lose the escaping of IFS
  -- characters).
  --
  -- Here, I am assuming if the first element is a bareword dollar substitution.
  -- However, they could occur anywhere and this needs to be refactored.  The
  -- issue is noted in the Todo file.

  if Element( shell_word, 1 ) = '$' then
     pathnameExpansionWithIFS( word, pattern, wordList );
  else
     pathnameExpansion( word, pattern, wordList );
  end if;

  if isExecutingCommand then

     if trace then
        declare
          theWord : aShellWord;
        begin
          if ignoreTerminatingWhitespace then
             put_trace( "SQL word '" & to_string( toEscaped( pattern ) ) &
                "' expands to:" );
          else
             put_trace( "shell word '" & to_string( toEscaped( pattern ) ) &
                "' expands to:" );
          end if;
          for i in 1..shellWordList.length( wordList ) loop
              shellWordList.Find( wordList, i, theWord );
              put_trace( to_string( toEscaped( theWord.word ) ) );
          end loop;
        end;
     end if;
  end if;
  getNextToken;

end ParseShellWord;


-----------------------------------------------------------------------------
--  PARSE ONE SHELL WORD
--
-- Parse and expand one shell word arguments.  Return the resulting pattern,
-- the expanded word, and the type of word.  First should be true if the word
-- can be a command.  An error occurs if the word can expand into more than
-- one word.
-----------------------------------------------------------------------------

procedure ParseOneShellWord( wordType : out aShellWordType;
   pattern, word : in out unbounded_string; First : boolean := false ) is
   wordList : shellWordList.List;
   theWord  : aShellWord;
   listLen  : shellWordList.aListIndex;
begin
   ParseShellWord( wordList, First );
   listLen := shellWordList.Length( wordList );
   if listLen > 1 then
      err( "one shell word expected but it expanded to multiple words.  (SparForte requires commands that expand to one shell word.)" );
   elsif listLen = 0 then
      err( "internal error: one shell word expected but it expanded to none" );
   else
      shellWordList.Find( wordList, 1, theWord );
      wordType := theWord.wordType;
      pattern  := theWord.pattern;
      word     := theWord.word;
   end if;
   shellWordList.Clear( wordList );
end ParseOneShellWord;


-----------------------------------------------------------------------------
--  PARSE SHELL WORDS
--
-- Parse and expand zero or more shell word arguments.  Return the results
-- as a shellWordList.  First should be true if the first word is a command.
--
-- A list of shell words ends with either a semi-colon (the end of a general
-- statement) or when a pipe or @ is read in as a parameter.  Do not include
-- a semi-colon in the parameters.
-----------------------------------------------------------------------------

procedure ParseShellWords( wordList : in out shellWordList.List; First : boolean := false ) is
   theWord  : aShellWord;
   theFirst : boolean := First;
   listLen  : shellWordList.aListIndex;
begin
   loop
     exit when token = symbol_t and identifiers( token ).value.all = ";";
     ParseShellWord( wordList, theFirst );
     listLen := shellWordList.Length( wordList );
     if listLen = 0 then
        err( "internal error: one shell word expected but it expanded to none" );
     else
        theFirst := false;
        shellWordList.Find( wordList, shellWordList.Length( wordList ), theWord );
        exit when theWord.wordType = pipeWord;       -- pipe always ends a command
        exit when theWord.wordType = itselfWord;     -- itself always ends a command
        exit when error_found;
     end if;
   end loop;
end ParseShellWords;



-----------------------------------------------------------------------------
--
--  Housekeeping
--
-----------------------------------------------------------------------------


procedure startShellScanner is
begin
  null;
end startShellScanner;

procedure stopShellScanner is
begin
  null;
end stopShellScanner;

end parser.decl.shell;
