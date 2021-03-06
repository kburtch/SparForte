------------------------------------------------------------------------------
-- AdaScript Language Parser (Bourne Shell)                                 --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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
with ada.text_io; use ada.text_io;

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

type whitespaceOptions is ( keep, trim );
-- during an expansion, should leading/trailing whitespace be kept or not

type globOptions is ( normalGlob, sqlDoubleNoGlob, sqlSingleNoGlob );
-- For SQL words,there's no globbing and special escaping

type defaultModes is (none, minus, plus, question, var_slice, var_replace );
-- ${..} modes for default values


-----------------------------------------------------------------------------
--
--  Globbing Expansions
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE TILDE
--
-- Syntax: ~ or ~username
-- Replace tilde with the home directory path.
-----------------------------------------------------------------------------

procedure parseTilde( globPattern : in out aGlobShellWord ) is
   home_id : identifier;
begin
   -- For ~username, simply replace ~ with "/home/".  Otherwise, substitute the
   -- value of $HOME, the user's home directory.

   if length( globPattern ) > 1 and then element( globPattern, 2 ) /= directory_delimiter then
      globPattern := replace_slice( globPattern, 1, 1, "/home/" );
   else
      findIdent( to_unbounded_string( "HOME" ), home_id );
      globPattern := replace_slice( globPattern, 1, 1, to_string( identifiers( home_id ).value.all ) );
   end if;
end parseTilde;


-----------------------------------------------------------------------------
--  DO PATHNAME EXPANSION
--
-- Perform shell pathname expansion by using the shell word as a glob
-- pattern.  Given a globbing expression path (e.g. /a/b*/c*), do a depth-
-- first traversal and find all matching files.  Return a list of shell
-- words created by the expansion in the shell word list.
--
-- Note: file name length is limited to 256 characters.
-----------------------------------------------------------------------------

procedure doPathnameExpansion(
  originalGlobPattern : aGlobShellWord;
  bourneShellWordList : in out bourneShellWordLists.List
) is

  originalGlobPatternLen : natural := length( originalGlobPattern );

  ---------------------------------------------------------------------------
  --  GLOB PATTERN WITHOUT ESCAPSES
  --
  -- Occasionally, such as when a glob matches nothing, we want to show the
  -- globbing pattern minus the backslash escapes we inserted.  In Bash,
  -- raw word "*"*.txt will be shown as **.txt not \**.txt.
  ---------------------------------------------------------------------------

  function globPatternWithoutEscapes return anExpandedShellWord is
    expandedWord : anExpandedShellWord;
    globPos : natural := 1;
    globLen : constant natural := length( originalGlobPattern );
    ch      : character;
  begin
    while globPos <= globLen loop
      ch := element( originalGlobPattern, globPos );
      if ch = '\' then
         globPos := globPos + 1;
         ch := element( originalGlobPattern, globPos );
      end if;
      expandedWord := expandedWord & ch;
      globPos := globPos + 1;
    end loop;
    return expandedWord;
  exception when others =>
    err( "internal error: cannot remove escapes from globbing pattern " & toProtectedValue( unbounded_string( originalGlobPattern ) ) );
    return anExpandedShellWord( originalGlobPattern );
  end globPatternWithoutEscapes;

  ---------------------------------------------------------------------------
  --  SPLIT NEXT GLOB SEGMENT
  --
  -- Given a glob path like /a/b*/c, from position start, extract the next
  -- segment between the slashes.  A leading slash is returned as its own
  -- segment.  Backslash escapes are honoured.
  ---------------------------------------------------------------------------

  procedure splitNextGlobSegment(
      nextSegment : out aGlobShellWord;
      start       : positive;
      finish      : out natural ) is
    pos : positive;
    ch  : character;
    inBackslash : boolean := false;
  begin
<<retry_next_segment>>
    pos := start;
    nextSegment := nullGlobShellWord;

    -- if we're at the start of the glob pattern, then the root directory
    -- is treated as the first segment, if there is one.

    if pos = 1 then
       if pos <= originalGlobPatternLen then
          if element( originalGlobPattern, pos ) = directory_delimiter then
             -- this is just a /
             nextSegment := nextSegment & directory_delimiter;
             finish := 1;
             return;
          end if;
       end if;
    end if;

    -- otherwise, extract the next globbing segment

    while pos <= originalGlobPatternLen loop
       ch := element( originalGlobPattern, pos );
       if not inBackslash then
          if ch = directory_delimiter then
             -- empty slash in path like /x//y? skip it and try the following
             -- character
             if pos = start then
                splitNextGlobSegment( nextSegment, start+1, finish );
                return;
             end if;
             exit;
          elsif ch = '\' then
             inBackslash := true;
          end if;
       else
          inBackslash := false;
       end if;
       nextSegment := nextSegment & ch;
       pos := pos + 1;
    end loop;

    if pos > originalGlobPatternLen then
       pos := pos - 1;
    end if;
    finish := pos;
  end splitNextGlobSegment;

  ---------------------------------------------------------------------------
  --  WALK SEGMENT
  --
  -- Given a parent directory, extract the next segment of the glob path.
  -- Read the parent directory and compare entries against the segment.
  -- If it matches, recursively walk that segment.
  --
  -- If we run out of segments in the glob path, add the result to the
  -- word list.
  ---------------------------------------------------------------------------

  procedure walkSegment( candidateParentPath : anExpandedShellWord; start : natural ) is
       finish : natural;
       nextSegment : aGlobShellWord;
       newCandidatePath : anExpandedShellWord;
     globCriteria    : regexp;
     parentDir       : Dir_Type;
     fileName        : string(1..256);
     fileNameLen     : natural;
     found           : boolean := false;
     noPWD           : boolean := false;
     isOpen          : boolean := false;
     segmentFound    : boolean := false;
  begin
  --put_line( "Walking '" & to_string( candidateParentPath ) & "'"); -- DEBUG
     if start > originalGlobPatternLen then
        bourneShellWordLists.Queue( bourneShellWordList, candidateParentPath );
        return;
     end if;

     -- handling multiple ///foo///bar in a path

     splitNextGlobSegment( nextSegment, start, finish );
      if trace then -- DEBUG
         put_trace( "next path segment is '" & toSecureData( to_string( nextSegment ) ) & "'" );
      end if;

     -- A leading slash is an absolute path.  There's no need to check
     -- the slash.  Just make it the parent and go continue walking.

     if candidateParentPath = nullExpandedShellWord then
        if nextSegment = "" & directory_delimiter then
           newCandidatePath := newCandidatePath & directory_delimiter;
           walkSegment( newCandidatePath, finish+1 );
           return;
        end if;
     end if;

     -- otherwise, prepare to glob the current directory

     globCriteria := Compile( to_string( nextSegment ), Glob => true, Case_Sensitive => true );

     -- open the parent directory
     --
     -- if there's no parent, it's the current directory

     declare
       path : unbounded_string;
     begin
       if candidateParentPath = nullExpandedShellWord then
          open( parentDir, "." );
       else
          open( parentDir, to_string( candidateParentPath ) );
       end if;
       isOpen := true;
     exception when others =>
       noPWD := true;
     end;

     -- is the current directory invalid? then param is just the word

     if noPWD then
        return;
     end if;

     if trace then
        if verboseOpt then
           put_trace( "walking path '" & toSecureData( to_string( candidateParentPath ) ) & "'" );
        end if;
     end if;

     -- search the directory, adding files that match the glob pattern

     loop
        read( parentDir, fileName, fileNameLen );
        -- KB: 12/02/18 - no longer returns "." and "..".  Commented out reads
        -- but as a safety precaution check the filename here.
--put_line( filename(1..fileNameLen) ); -- DEBUG
        exit when fileNameLen = 0;
        segmentFound := false;
        if filename( 1..fileNameLen ) = "." and nextSegment = "." then
           segmentFound := true;
        elsif filename( 1..fileNameLen ) = ".." and nextSegment = ".." then
           segmentFound := true;
        elsif filename( 1..fileNameLen ) = "." then
           null;
        elsif filename( 1..fileNameLen ) = ".." then
           null;
        elsif Match( fileName(1..fileNameLen ) , globCriteria ) then
           segmentFound := true;
        end if;

        if segmentFound then

           -- if no parent directory, it's the current directory.  The filename
           -- is a potential directory...just copy.  Otherwise, attach the
           -- next segment to the parent with the delimiter.
           --
           -- The root directory does not need a delimiter because it already
           -- is one.

           if candidateParentPath = nullExpandedShellWord then
              newCandidatePath := to_unbounded_string( filename( 1..fileNameLen ) );
           elsif candidateParentPath = "" & directory_delimiter then
              newCandidatePath := candidateParentPath &
                 to_unbounded_string( filename( 1..fileNameLen ) );
           else
              newCandidatePath := candidateParentPath & directory_delimiter &
                 to_unbounded_string( filename( 1..fileNameLen ) );
           end if;

           walkSegment( newCandidatePath, finish+1 );

           found := true;
        end if;
     end loop;

     -- there are no matches? word still counts: the param is just the word

     if not found then
        if trace then
           if verboseOpt then
              put_trace( "no matches" );
           end if;
        end if;
     end if;

     if isOpen then
        close( parentDir );
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

    if trace then
       if verboseOpt then
          put_trace( "error in globbing expression" );
       end if;
    end if;
    if isOpen then
       close( parentDir );
    end if;
  when DIRECTORY_ERROR =>
    err( "directory error on directory " & toSecureData(to_string( candidateParentPath)));
  when program_error =>
    err( "program_error exception raised" );
  when others =>
    err_exception_raised;
  end walkSegment;

  ---------------------------------------------------------------------------
  --  WALK FIRST SEGMENT
  --
  -- Initialize the walk of the globbing path.
  ---------------------------------------------------------------------------

  procedure walkFirstSegment is
     numShellWords : long_integer;
  begin
     numShellWords := bourneShellWordLists.Length( bourneShellWordList );
     walkSegment( nullExpandedShellWord, 1 );
     -- If globbing produced nothing, use the original pattern
     if numShellWords = bourneShellWordLists.Length( bourneShellWordList ) then
        bourneShellWordLists.Queue( bourneShellWordList, globPatternWithoutEscapes );
     end if;
  end walkFirstSegment;

begin

  -- In the case of a syntax check, return the word as-is as a place holder.
  -- Don't try to glob it.

  if syntax_check then
     bourneShellWordLists.Queue( bourneShellWordList, anExpandedShellWord( originalGlobPattern ) );
     return;
  end if;

  -- word is an empty string? it still counts: param is a null string

  if length( originalGlobPattern ) = 0 then
     bourneShellWordLists.Queue( bourneShellWordList, nullExpandedShellWord );
     return;
  end if;

  walkFirstSegment;
end doPathnameExpansion;


-----------------------------------------------------------------------------
--  DO GLOB PATTERN
--
-- The globbing does not happen on the word token but on the expanded word
-- after quote handling and substitution.
--
-- Nothing special is done for globbing the first word.  In Bash, this
-- matches files in the current directory like any other glob.
-----------------------------------------------------------------------------

procedure doGlobPattern(
   originalGlobPattern: aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List ) is
   globLen : constant natural := length( originalGlobPattern );
   globPos : natural := 1;
   globPattern : aGlobShellWord := aGlobShellWord( originalGlobPattern );
begin
   if trace then
      put_trace( "globbing with pattern '" & to_string( originalGlobPattern ) & "'" );
   end if;

   -- Tilde handling
   --
   -- Expand a leading tilde
   -- If a leading "\~", remove the backslash leaving the tilde.

   if globLen > 0 then
      if element( globPattern, globPos ) = '~' then
         parseTilde( globPattern );
      elsif element( globPattern, globPos ) = '\' then
         if globPos < globLen then
            if element( globPattern, globPos+1 ) = '~' then
               delete( globPattern, globPos, globPos );
            end if;
         end if;
      end if;
   end if;

   -- Pathname Expansion

   doPathnameExpansion( globPattern, bourneShellWordList );
end doGlobPattern;


-----------------------------------------------------------------------------
--
--  Substitutions
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE SHELL EXPANSION NAME
--
-- Syntax: # | ? | $ | ! | 0-9 | var
-- Read an identifier name from within a shell word.  Also handle special
-- expansion names like $?, $#, etc.
-----------------------------------------------------------------------------

procedure parseShellExpansionName(
      rawWordValue : aRawShellWord;
      wordLen  : natural;
      wordPos  : in out natural ) is
   ch       : character;
   firstPos : constant natural := wordPos;
begin
   -- put_line( "parseShellExpansionName" ); -- DEBUG

   -- if no characters left or an error occurred, do not proceed

   if not endOfShellWord and not error_found then

     -- Bourne shell special substitutions
     -- $# - number of parameters
     -- $? - status of last command
     -- $$ - process id (pid)
     -- $! - child process id
     -- $* - all parameters
     -- $@ - all parameters, escape spaces

      ch := Element( rawWordValue, wordPos );
      if ch = '#' then
         getNextChar( rawWordValue, wordLen, wordPos );
      elsif ch = '?' then
         getNextChar( rawWordValue, wordLen, wordPos );
      elsif ch = '$' then
         getNextChar( rawWordValue, wordLen, wordPos );
      elsif ch = '!' then
         getNextChar( rawWordValue, wordLen, wordPos );
      elsif ch = '*' then
         getNextChar( rawWordValue, wordLen, wordPos );
      elsif ch = '@' then
         getNextChar( rawWordValue, wordLen, wordPos );
      else

         -- Environment variables can contain any character except equals, but
         -- shells restrict to alphanumeric, underscore and no leading number.
         -- AdaScript does not allow leading underscore either.

         -- side-stepping unicode issue for now by checking length instead of
         -- using an end of word character like ASCII.NUL.

         while not endOfShellWord and not error_found loop
            -- I know this is done twice...
            ch := Element( rawWordValue, wordPos );
            -- to support arrays, this will have to change
            exit when ch /= '_' and ch not in 'A'..'Z' and ch not in 'a'..'z'
               and ch not in '0'..'9';
            getNextChar( rawWordValue, wordLen, wordPos );
         end loop;

         -- Check for valid leading character

         -- Determine the length of the variable name and the position of the
         -- first character.

         if syntax_check then
            declare
              varNameLen    : natural := 0;
              firstNameChar : natural := 0;
            begin
               for i in 1..wordLen loop
                   ch := Element( rawWordValue, i );
                   if ch = ':' or ch = '}' then
                      exit;
                   elsif ch /= '$' and ch /= '#' and ch /= '{' and ch /= '"' then
                      varNameLen := varNameLen + 1;
                      if firstNameChar = 0 then
                         firstNameChar := i;
                      end if;
                   end if;
               end loop;

               --
               -- $0 - command name
               -- $1 to $9 - first nine parameters
               -- also "${0}" vs ${0}

               if varNameLen > 1 then -- if more than one char
                  ch := Element( rawWordValue, firstNameChar );
                  if ch = '_' then
                     err_shell( "leading underscores should not be in identifier names", wordPos );
                  elsif ch in '0'..'9' then --and firstPos /= wordPos then
                     err_shell( "identifier names should not start with numerals", wordPos );
                  end if;
               end if;
            end;
         end if;

      end if;
  end if;
end parseShellExpansionName;


-----------------------------------------------------------------------------
--  ADD SUBWORD WITH WORD SPLITTING
--
-- Perform word splitting on the string, adding words to the list.
-----------------------------------------------------------------------------

procedure addSubwordWithWordSplitting(
      globPattern : in out aGlobShellWord;
      bourneShellWordList : in out bourneShellWordLists.List;
      subword : aGlobShellWord )  is
   pos : natural :=1;
   last_pos : natural := pos;
   ch : character;
begin
   while pos < length( subword ) loop
      ch := element( subword, pos );
      if ch = ' ' or ch = ASCII.HT then
         globPattern := globPattern & aGlobShellWord( unbounded_slice( subword, last_pos, pos-1 ) );
         --put_line( "addSubwordWithWordSplitting: globPattern = " & to_string( globPattern ) );
         doGlobPattern( globPattern, bourneShellWordList );
         -- bourneShellWordLists.Queue( bourneShellWordList, expandedWord );
         globPattern := nullGlobShellWord;
         last_pos := pos + 1;
       end if;
       pos := pos + 1;
   end loop;
   globPattern := globPattern & aGlobShellWord( unbounded_slice( subword, last_pos, length( subword ) ) );
   -- put_line( "addSubwordWithWordSplitting: final globPattern = " & to_string( globPattern ) ); -- DEBUG
end addSubwordWithWordSplitting;


-----------------------------------------------------------------------------
--  GET EXPANSION VALUE
--
-- Lookup the value for the expansion.
-----------------------------------------------------------------------------

procedure getExpansionValue(
      expansionVar : unbounded_string;
      subword : out unbounded_string;
      whitespaceOption : whitespaceOptions;
      wordPos : natural ) is
   var_id       : identifier;
   temp         : unbounded_string;
begin
-- put_line("getExpansionValue: " & to_string( expansionVar )); -- DEBUG
   if expansionVar = "" then
      err_shell( "missing variable name", wordPos );
   elsif expansionVar = "#" then
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
   elsif expansionVar = "*" then
      if isExecutingCommand then
         for i in optionOffset+1..Argument_Count loop
             subword := subword & to_unbounded_string( Argument( i ) );
             if i /= Argument_Count then
                subword := subword & ' ';
             end if;
         end loop;
      end if;
   elsif length( expansionVar ) = 1 and (expansionVar >= "1" and expansionVar <= "9" ) then
      if syntax_check and then not suppress_word_quoting and then whitespaceOption = keep then
         err( "style issue: expected double quoted word parameters in shell or SQL command to stop word splitting" );
      end if;
      if isExecutingCommand then
         begin
            subword := to_unbounded_string(
                Argument(
                   integer'value(
                   to_string( " " & expansionVar ) )+optionOffset ) );
         exception when others =>
            err_shell( "script argument " & to_string(expansionVar) & " not found " &
                 "in arguments 0 .." &
                 integer'image( Argument_Count-optionOffset), wordPos );
         end;
      end if;
   else
      findIdent( expansionVar, var_id );

      if var_id = eof_t then
         err_shell( "identifier " & optional_yellow( to_string( toEscaped( expansionVar ) ) ) & " not declared", wordPos );

      -- For an enumerated item, the value is the item name

      elsif identifiers( var_id ).class = enumClass then
         if syntax_check then
            identifiers( var_id ).wasReferenced := true;
         end if;
         subword := expansionVar;

      -- For a variable, the value is the variable's value

      elsif identifiers( var_id ).class = varClass then

         -- For an array or a record, there's no value
         -- record fields are not checked because the name will end with the period,
         -- and will be the record name.

         if identifiers( var_id ).list then
            err_shell( optional_yellow( to_string( expansionVar ) ) & " is an array", wordPos);
         elsif getUniType( identifiers( var_id ).kind ) = root_record_t then
            err_shell( optional_yellow( to_string( expansionVar ) ) & " is a record", wordPos);
         elsif syntax_check then
            identifiers( var_id ).wasReferenced := true;
            --identifiers( id ).referencedByThread := getThreadName;
            subword := to_unbounded_string( "undefined" );
         else
            if getUniType( identifiers( var_id ).kind ) = root_enumerated_t then
               -- an enumerated is just the name of the value
               subword := identifiers( var_id ).name;
               findEnumImage( identifiers( var_id ).value.all , identifiers( var_id ).kind, temp );
               subword := temp;
            else
               subword := identifiers( var_id ).value.all;              -- word to substit.
               if whitespaceOption = trim then
                  subword := Ada.Strings.Unbounded.Trim(                   -- unless double
                     unbounded_string( subword ), Ada.Strings.Both );            -- quotes;
               elsif getUniType( var_id ) = uni_numeric_t then -- a number?
                  if length( subword ) > 0 then            -- something there?
                     if element( subword, 1 ) = ' ' then   -- leading space
                        delete( subword, 1, 1 );           -- we don't want it
                     end if;
                  end if;
               end if;
            end if;
         end if;
      else
         err_shell( optional_yellow( to_string( expansionVar ) ) & " is not a variable", wordPos );
      end if;
   end if;
end getExpansionValue;


-----------------------------------------------------------------------------
--  GLOB BACKSLASH ESCAPE
--
-- Convert an unbounded string to a glob word by escaping the special
-- characters that affect globbing.
--
-- The tilde is not a glob character: but I have to handle it and not expand
-- the tilde.
-----------------------------------------------------------------------------

function globBackslashEscape( unescapedWord : unbounded_string;
      whitespaceOption : whitespaceOptions ) return aGlobShellWord is
   escapedWord : aGlobShellWord;
   ch : character;
   p  : natural := 1;
   l  : constant natural := length( unescapedWord );
begin
   while p <= l loop
     ch := element( unescapedWord, p );
     case ch is
     when '*' =>
        -- in a bareword, * is not escaped.  In double quotes, it is.
        if whitespaceOption = keep then
           escapedWord := escapedWord & '\' & ch;
        else
           escapedWord := escapedWord & ch;
        end if;
     when '[' =>
        escapedWord := escapedWord & '\' & ch;
     when '?' =>
        escapedWord := escapedWord & '\' & ch;
     when '~' =>
        escapedWord := escapedWord & '\' & ch;
     when '\' =>
        escapedWord := escapedWord & '\' & ch;
     when others =>
        escapedWord := escapedWord & ch;
     end case;
     p := p + 1;
   end loop;
   return escapedWord;
end globBackslashEscape;


-----------------------------------------------------------------------------
--  SQL DOUBLE BACKSLASH ESCAPE
--
-- Convert an unbounded string to a SQL string word by escaping the special
-- characters.
-----------------------------------------------------------------------------

function sqlDoubleBackslashEscape( unescapedWord : unbounded_string ) return aGlobShellWord is
   escapedWord : aGlobShellWord;
   ch : character;
   p  : natural := 1;
   l  : constant natural := length( unescapedWord );
begin
   while p <= l loop
     ch := element( unescapedWord, p );
     case ch is
     -- when '*' =>
     --   -- in a bareword, * is not escaped.  In double quotes, it is.
     --   if whitespaceOption = keep then
     --      escapedWord := escapedWord & '\' & ch;
     --   else
     --      escapedWord := escapedWord & ch;
     --   end if;
     when '\' =>
        escapedWord := escapedWord & '\' & ch;
     when '"' =>
        escapedWord := escapedWord & '\' & ch;
     when others =>
        escapedWord := escapedWord & ch;
     end case;
     p := p + 1;
   end loop;
   return escapedWord;
end sqlDoubleBackslashEscape;


-----------------------------------------------------------------------------
--  SQL SINGLE BACKSLASH ESCAPE
--
-- Convert an unbounded string to a SQL string word by escaping the special
-- characters.
-----------------------------------------------------------------------------

function sqlSingleBackslashEscape( unescapedWord : unbounded_string ) return aGlobShellWord is
   escapedWord : aGlobShellWord;
   ch : character;
   p  : natural := 1;
   l  : constant natural := length( unescapedWord );
begin
   while p <= l loop
     ch := element( unescapedWord, p );
     case ch is
     when '\' =>
        escapedWord := escapedWord & '\' & ch;
     when ''' =>
        escapedWord := escapedWord & '\' & ch;
     when others =>
        escapedWord := escapedWord & ch;
     end case;
     p := p + 1;
   end loop;
   return escapedWord;
end sqlSingleBackslashEscape;


-----------------------------------------------------------------------------
--  DO VARIABLE EXPANSION
--
-- Perform a dollar variable expansion.
-----------------------------------------------------------------------------

procedure doVariableExpansion(
      rawWordValue : aRawShellWord;
      expansionVar : unbounded_string;
      defaultValue : unbounded_string;
      defaultMode  : defaultModes;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      bourneShellWordList : in out bourneShellWordLists.List;
      whitespaceOption : whitespaceOptions;
      globOption : globOptions ) is


   --  DO VAR SLICE EXPANSION (DO VARIABLE EXPANSION)
   --
   -- ${s:a} or ${s:a:b} string slice
   --------------------------------------------------------------------------

   procedure doVarSliceExpansion( subword : in out unbounded_string ) is
      colonPos : natural;
      slicePos : natural;
      sliceEnd : integer;
      tmp : unbounded_string;
      tmp2: unbounded_string;
   begin
      sliceEnd := length( subword ); -- To avoid exception undefined val
      -- currently no spaces allowed but maybe one day will be...
      tmp := Ada.Strings.Unbounded.trim(
          defaultValue, Ada.Strings.Both );
      -- a slice may have an optional length value
      -- in ada, slice needs a last position not a length
      colonPos := index( tmp, ":" );
      if length(defaultValue) > 1 and colonPos = length(defaultValue) then
          err_shell( "slice length is missing", wordPos );
      elsif colonPos = 1 then
          err_shell( "slice position is missing", wordPos );
      elsif colonPos > 0 then
         -- a negative position is not possible, it ends up :-
         begin
            tmp2 := unbounded_slice( tmp, 1, colonPos-1);
            slicePos := natural'value( " " & to_string( tmp2 ) ) + 1;
         exception when others =>
            err_shell( "slice position is not a natural", wordPos );
         end;
         begin
            tmp2 := unbounded_slice( tmp, colonPos+1, length(tmp) );
            if element( tmp2, 1 ) = '-' then
               sliceEnd := integer'value( to_string( tmp2 ) );
               sliceEnd := length( subword ) + sliceEnd;
               if sliceEnd < 1 then
                  sliceEnd := length( subword );
                  err_shell( "negative slice length is less that start position", wordPos );
               end if;
            else
              sliceEnd := integer'value( " " & to_string( tmp2 ) );
              sliceEnd := slicePos + sliceEnd - 1;
            end if;
         exception when others =>
            err_shell( "slice length is not a integer", wordPos );
         end;
         -- Ada will error of end is out-of-range but shell doesn't care
         if sliceEnd > length( subword ) then
            sliceEnd := length( subword );
         end if;
      else
         begin
            slicePos := natural'value( " " & to_string( tmp ) ) + 1;
            sliceEnd := length( subword );
         exception when others =>
            err_shell( "slice position is not a natural", wordPos );
         end;
      end if;
      begin
         if not error_found then
            subword := unbounded_slice( subword, slicePos, sliceEnd );
         end if;
      exception when Ada.Strings.INDEX_ERROR =>
        -- In Bourne shell, this is not actually an error.
         subword := null_unbounded_string;
      when CONSTRAINT_ERROR =>
         -- should not happen
         err_shell( "slice position or length out of range", wordPos );
      end;
   end doVarSliceExpansion;


   --  DO VAR REPLACE EXPANSION (DO VARIABLE EXPANSION)
   --
   -- ${s/a/b} string replacement
   --------------------------------------------------------------------------

   procedure doVarReplaceExpansion( subword : in out unbounded_string ) is
      slashPos   : natural;
      searchStr  : unbounded_string;
      replaceStr : unbounded_string;
      searchPos  : natural;
   begin
      slashPos := ada.strings.unbounded.index( defaultValue, "/" );
      if slashPos <= 1 then
          err_shell( "search string is missing", wordPos );
      else
          searchStr := unbounded_slice( defaultValue, 1, slashPos-1);
          replaceStr := unbounded_slice( defaultValue, slashPos+1, length( defaultValue ) );
          searchPos := ada.strings.unbounded.index( subword, to_string( searchStr ) );
          if searchPos > 0 then
             if length( replaceStr ) > 0 then
                Replace_Slice( subword,
                   searchPos,
                   searchPos + length( searchStr ) - 1,
                   to_string( replaceStr ) );
             else
                Delete( subword,
                   searchPos,
                   searchPos + length( searchStr ) - 1 );
             end if;
          end if;
      end if;
   end doVarReplaceExpansion;

   subword      : unbounded_string;
   globSubword  : aGlobShellWord;
   expandedWord : anExpandedShellWord;
begin
   -- put_line( "doVariableExpansion" ); -- DEBUG

   -- Perform the expansion

   getExpansionValue( expansionVar, subword, whitespaceOption, wordPos );

   -- Apply default if subword is an empty string

   case defaultMode is
   when minus =>
      if subword = "" then
         subword := defaultValue ;
      end if;
   when plus =>
      if subword /= "" then
         subword := defaultValue;
      end if;
   when question =>
      if subword = "" then
         if defaultValue = "" then
            err_shell( toProtectedValue( expansionVar ) & " is an empty string", wordPos );
         else
            err_shell( toProtectedValue( expansionVar ) & " " & to_string( defaultValue ), wordPos );
         end if;
      end if;
   when var_slice =>
      doVarSliceExpansion( subword );
   when var_replace =>
      doVarReplaceExpansion( subword );
   when others =>
      null;
   end case;

   -- The string returned may have special characters that need to be escaped
   -- for a glob pattern (such as a \).  However, the escaping is different
   -- for SQL than for Bourne shell globbing.

   if globOption = sqlDoubleNoGlob then
      globPattern := globPattern & sqlDoubleBackslashEscape( subword );
   elsif globOption = sqlSingleNoGlob then
      globPattern := globPattern & sqlSingleBackslashEscape( subword );
   else
      globSubword := globBackslashEscape( subword, whitespaceOption );
   end if;

   -- if trimming, then it's a bareword.  Even a bareword escapes a leading
   -- tilde.

   if whitespaceOption = trim then
      if length( subword ) > 0 then
         if element( subword, 1 ) = '~' then
            subword := '\' & subword;
         end if;
      end if;
      addSubwordWithWordSplitting(
         globPattern,
         bourneShellWordList,
         globSubword );
   else
      globPattern := globPattern & globSubword;
      --put_line( "doVariableExpansion: expandedWord = " & to_string( globPattern ) ); -- DEBUG
   end if;
end doVariableExpansion;


-----------------------------------------------------------------------------
--  DO VARIABLE LENGTH EXPANSION
--
-- Perform a dollar variable length expansion.
-----------------------------------------------------------------------------

procedure doVariableLengthExpansion(
      expansionVar : unbounded_string;
      globPattern : in out aGlobShellWord;
      whitespaceOption : whitespaceOptions;
      wordPos : natural ) is
   subword      : unbounded_string;
begin
   -- Perform the expansion

   getExpansionValue( expansionVar, subword, whitespaceOption, wordPos );

   globPattern := globPattern & aGlobShellWord( Ada.Strings.Unbounded.trim(
      to_unbounded_string( length( subword )'img ), Ada.Strings.Both ) );
end doVariableLengthExpansion;


-----------------------------------------------------------------------------
--  DO COMMAND SUBSTITUTION
--
-- Perform a command substitution.  Run the specified commands and return
-- the standard output results.
-----------------------------------------------------------------------------

function doCommandSubstitution( originalCommands : unbounded_string ) return unbounded_string is
   commands : unbounded_string := originalCommands;
   commandsOutput : unbounded_string;
begin
   -- put_line( "commands = " & to_string( commands ) );
   if commands = "" or commands = " " then
      -- In Bourne shell, nothing happens.  It's not an error.  However, we
      -- enforce an error by default.
      if not suppress_no_empty_command_subs then
         err( "empty command substitution" );
      end if;
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
      if trace then
         declare
            fragment : unbounded_string;
         begin
           if length( commandsOutput ) > 70 then
              fragment := head( commandsOutput, 70 );
              put_trace( "output '" & to_string( toEscaped( fragment ) ) & "'..." );
           else
              put_trace( "output '" & to_string( toEscaped( commandsOutput ) ) & "'" );
           end if;
         end;
      end if;
   end if;
   return commandsOutput;
end doCommandSubstitution;


-----------------------------------------------------------------------------


procedure parseDoubleQuotedShellSubword(
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural;
   globPattern : in out aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List );

procedure parseSingleQuotedShellSubword(
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural;
   globPattern : in out aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List );

procedure parseBackQuotedShellSubword(
      rawWordValue : aRawShellWord;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      whitespaceOption : whitespaceOptions;
      bourneShellWordList : in out bourneShellWordLists.List );

procedure parseBackslash(
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural;
   globPattern : in out aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List );


-----------------------------------------------------------------------------
--  PARSE DOLLAR BRACE EXPANSION
--
-- Syntax: ${...}
-- Handle a dollar brace variable expansion.
-----------------------------------------------------------------------------

procedure parseDollarBraceExpansion(
      rawWordValue : aRawShellWord;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      bourneShellWordList : in out bourneShellWordLists.List;
      whitespaceOption : whitespaceOptions;
      globOption : globOptions ) is
   firstPos     : natural;
   expansionVar : unbounded_string;
   defaultValue : unbounded_string;
   isLengthExpansion : boolean := false;
   defaultMode  : defaultModes := none;

   --  GET BRACE OPERATOR DEFAULT VALUE
   --
   -- The default value in a brace operation can be quoted and
   -- contain other substitutions.
   --------------------------------------------------------------------------

   procedure getBraceOperatorDefaultValue is
     globPattern: aGlobShellWord;
     ch : character;
   begin
     while not endOfShellWord loop
       ch := element( rawWordValue, wordPos );
       exit when ch = '}';
       case ch is
       when '}' =>
          exit;
       when ''' =>
          parseSingleQuotedShellSubword(
             rawWordValue,
             wordLen,
             wordPos,
             globPattern,
             bourneShellWordList );
             defaultValue := defaultValue & ada.strings.unbounded.unbounded_string( globPattern );
       when '"' =>
          parseDoubleQuotedShellSubword(
             rawWordValue,
             wordLen,
             wordPos,
             globPattern,
             bourneShellWordList );
             defaultValue := defaultValue & ada.strings.unbounded.unbounded_string( globPattern );
       exit when ch = '}';
       when '`' =>
          parseBackQuotedShellSubword(
             rawWordValue,
             wordLen,
             wordPos,
             globPattern,
             trim,
             bourneShellWordList );
             defaultValue := defaultValue & ada.strings.unbounded.unbounded_string( globPattern );
       when '\' =>
          parseBackslash(
             rawWordValue,
             wordLen,
             wordPos,
             globPattern,
             bourneShellWordList );
             defaultValue := defaultValue & ada.strings.unbounded.unbounded_string( globPattern );
       when others =>
          defaultValue := defaultValue & ch;
          getNextChar( rawWordValue, wordLen, wordPos );
       end case;
     end loop;
-- put_line( "getBraceOperatorDefaultValue: " & to_string( defaultvalue ) ); -- DEBUG
  end getBraceOperatorDefaultValue;

begin
   --put_line( "parseDollarBraceExpansion" ); -- DEBUG
   expectChar( '{', rawWordValue, wordLen, wordPos );

   -- Determine if it is a length expansion.  Also, get the variable name..

   firstPos := wordPos;
   if not endOfShellWord and then element( rawWordValue, wordPos) = '#' then
      getNextChar( rawWordValue, wordLen, wordPos );
      -- Edge-case ${#} is not a length expansion
      if not endOfShellWord and then element( rawWordValue, wordPos ) /= '}' then
         isLengthExpansion := true;
         firstPos := wordPos;
         parseShellExpansionName( rawWordValue, wordLen, wordPos );
      end if;
   else
     parseShellExpansionName( rawWordValue, wordLen, wordPos );
   end if;

   if endOfShellWord then
      -- forced error: otherwise, we'll get a no expansion name error.
      expectChar( '}', rawWordValue, wordLen, wordPos  );
   else

      -- Extract the variable name.
      -- wordPos is always one position ahead.

      if firstPos < wordPos then
         expansionVar := unbounded_slice( unbounded_string( rawWordValue ), firstPos, wordPos-1 );
      else
         -- this may not happen
         expansionVar := null_unbounded_string;
      end if;
      -- put_line( "parseDollarBraceExpansion: expansionVar = " & to_string( expansionVar ) ); -- DEBUG

      if element( rawWordValue, wordPos ) = ':' then
         expectChar( ':', rawWordValue, wordLen, wordPos  );
         if endOfShellWord then
            err_shell( "expected -, + or ? after colon", wordPos );
         elsif isLengthExpansion then
            err_shell( "# cannot be used with colon", wordPos );
         elsif element( rawWordValue, wordPos ) = '-' then
            defaultMode := minus;
            expectChar( '-', rawWordValue, wordLen, wordPos  );
            getBraceOperatorDefaultValue;
            --while not endOfShellWord loop
            --  exit when element( rawWordValue, wordPos ) = '}';
            --  defaultValue := defaultValue & element( rawWordValue, wordPos );
            --  getNextChar( rawWordValue, wordLen, wordPos );
            --end loop;
         elsif element( rawWordValue, wordPos ) = '+' then
            defaultMode := plus;
            expectChar( '+', rawWordValue, wordLen, wordPos  );
            getBraceOperatorDefaultValue;
            --while not endOfShellWord loop
            --  exit when element( rawWordValue, wordPos ) = '}';
            --  defaultValue := defaultValue & element( rawWordValue, wordPos );
            --  getNextChar( rawWordValue, wordLen, wordPos );
            --end loop;
         elsif element( rawWordValue, wordPos ) = '?' then
            defaultMode := question;
            expectChar( '?', rawWordValue, wordLen, wordPos  );
            getBraceOperatorDefaultValue;
            --while not endOfShellWord loop
            --  exit when element( rawWordValue, wordPos ) = '}';
            --  defaultValue := defaultValue & element( rawWordValue, wordPos );
            --  getNextChar( rawWordValue, wordLen, wordPos );
            --end loop;
         elsif element( rawWordValue, wordPos ) = '=' then
            err_shell( "assignment in curly braces not supported", wordPos );
         else
            -- substring slice
            defaultMode := var_slice;
            getBraceOperatorDefaultValue;
         end if;
      elsif element( rawWordValue, wordPos ) = '/' then
         -- substring slice
         defaultMode := var_replace;
         expectChar( '/', rawWordValue, wordLen, wordPos  );
         getBraceOperatorDefaultValue;
      elsif element( rawWordValue, wordPos ) = '-' or
            element( rawWordValue, wordPos ) = '+' or
            element( rawWordValue, wordPos ) = '?' then
         expectChar( ':', rawWordValue, wordLen, wordPos  );
      end if;
   end if;

   -- ${#VAR} length expansion

   if isLengthExpansion then
      doVariableLengthExpansion(
         expansionVar, globPattern, whitespaceOption, wordPos );
   else
      doVariableExpansion( rawWordValue, expansionVar, defaultValue, defaultMode,
         wordLen, wordPos, globPattern, bourneShellWordList, whitespaceOption,
         globOption );
   end if;

   expectChar( '}', rawWordValue, wordLen, wordPos );

end parseDollarBraceExpansion;


-----------------------------------------------------------------------------


procedure parseDollarExpansion(
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural;
   globPattern : in out aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List;
   whitespaceOption : whitespaceOptions );

-----------------------------------------------------------------------------
--  PARSE DOLLAR PROCESS EXPANSION
--
-- Syntax: $(...)
-- Handle a dollar round bracket process output expansion.
--
-- Where SparForte differs from BASH:
--
--   X := "echo `date`";
--   echo $($X) - prints the date when echo is run
--   X is assigned a string
--
--  But in Bash:
--
--   X="echo `date`"
--   echo $($X) - prints the date when X was assigned
--   X is assigned the result of running the command
-----------------------------------------------------------------------------

procedure parseDollarProcessExpansion(
      rawWordValue : aRawShellWord;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      bourneShellWordList : in out bourneShellWordLists.List;
      whitespaceOption : whitespaceOptions;
      globOption : globOptions ) is
   commands     : unbounded_string;
   expansionResult : aGlobShellWord;
   ch           : character;
begin
   --put_line( "parseDollarProcessExpansion" ); -- DEBUG

   -- Extract the variable name
   -- wordPos is always one position ahead.

    expectChar( '(', rawWordValue, wordLen, wordPos );

      while wordPos <= wordLen loop
        ch := element( rawWordValue, wordPos );
        exit when ch = ')';
        if ch = '$' then
           expansionResult := nullGlobShellWord;
           parseDollarExpansion( rawWordValue, wordLen, wordPos,
              expansionResult, bourneShellWordList, keep );
           commands := commands & unbounded_string( expansionResult );
        else
          commands := commands & ch;
          exit when wordPos = wordLen;
          getNextChar( rawWordValue, wordLen, wordPos );
        end if;
     end loop;

   expectChar( ')', rawWordValue, wordLen, wordPos );

   -- The string returned may have special characters that need to be escaped
   -- for a glob pattern (such as a \).

   globPattern := globPattern & globBackslashEscape( doCommandSubstitution( commands ), whitespaceOption );

end parseDollarProcessExpansion;


-----------------------------------------------------------------------------
--  PARSE DOLLAR EXPANSION
--
-- Syntax: $...
-- Redirects to the appropriate type of expansion.
-----------------------------------------------------------------------------

procedure parseDollarExpansion(
      rawWordValue : aRawShellWord;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      bourneShellWordList : in out bourneShellWordLists.List;
      whitespaceOption : whitespaceOptions ) is
   firstPos     : natural;
   expansionVar : unbounded_string;
   ch : character;
begin
   --put_line( "parseDollarExpansion" ); -- DEBUG
   expectChar( '$', rawWordValue, wordLen, wordPos );

   ch := ASCII.NUL;
   if length( rawWordvalue ) >= wordPos then
      ch := element( rawWordvalue, wordPos );
   end if;

   if ch = '{' then
      parseDollarBraceExpansion( rawWordValue, wordLen, wordPos,
         globPattern, bourneShellWordList, whitespaceOption,
         normalGlob );
   elsif ch = '(' then
      parseDollarProcessExpansion( rawWordValue, wordLen, wordPos,
         globPattern, bourneShellWordList, whitespaceOption, normalGlob );
   else

      -- Simple Dollar Expansion: it's just $ and the name

      firstPos := wordPos;
      parseShellExpansionName( rawWordValue, wordLen, wordPos );

      -- Extract the variable name
      -- wordPos is always one position ahead.

      if endOfShellWord then
         expansionVar := unbounded_slice( unbounded_string( rawWordValue ), firstPos, wordPos );
      elsif firstPos < wordPos then
         expansionVar := unbounded_slice( unbounded_string( rawWordValue ), firstPos, wordPos-1 );
      else
         expansionVar := null_unbounded_string;
      end if;
      --put_line( "expansionVar = " & to_string( expansionVar ) ); -- DEBUG

      doVariableExpansion( rawWordValue, expansionVar, null_unbounded_string, none,
         wordLen, wordPos, globPattern, bourneShellWordList, whitespaceOption,
         normalGlob );
   end if;
end parseDollarExpansion;


-----------------------------------------------------------------------------
--  PARSE SQL DOLLAR EXPANSION
--
-- Syntax: $...
-- Redirects to the appropriate type of expansion.
-----------------------------------------------------------------------------

procedure parseSQLDollarExpansion(
      rawWordValue : aRawShellWord;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      bourneShellWordList : in out bourneShellWordLists.List;
      whitespaceOption : whitespaceOptions;
      globOption : globOptions ) is
   firstPos     : natural;
   expansionVar : unbounded_string;
   ch : character;
begin
   --put_line( "parseSQLDollarExpansion" ); -- DEBUG
   expectChar( '$', rawWordValue, wordLen, wordPos );

   ch := ASCII.NUL;
   if length( rawWordvalue ) >= wordPos then
      ch := element( rawWordvalue, wordPos );
   end if;

   if ch = '{' then
      parseDollarBraceExpansion( rawWordValue, wordLen, wordPos,
         globPattern, bourneShellWordList, whitespaceOption, globOption );
   elsif ch = '(' then
      parseDollarProcessExpansion( rawWordValue, wordLen, wordPos,
         globPattern, bourneShellWordList, whitespaceOption, globOption );
   else

      -- Simple Dollar Expansion: it's just $ and the name

      firstPos := wordPos;
      parseShellExpansionName( rawWordValue, wordLen, wordPos );

      -- Extract the variable name
      -- wordPos is always one position ahead.

      if endOfShellWord then
         expansionVar := unbounded_slice( unbounded_string( rawWordValue ), firstPos, wordPos );
      elsif firstPos < wordPos then
         expansionVar := unbounded_slice( unbounded_string( rawWordValue ), firstPos, wordPos-1 );
      else
         expansionVar := null_unbounded_string;
      end if;
      --put_line( "expansionVar = " & to_string( expansionVar ) ); -- DEBUG

      doVariableExpansion( rawWordValue, expansionVar, null_unbounded_string, none,
         wordLen, wordPos, globPattern, bourneShellWordList, whitespaceOption, globOption );
   end if;
end parseSQLDollarExpansion;


-----------------------------------------------------------------------------
--
--  Quote handling
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE BACKSLASH
-----------------------------------------------------------------------------

procedure parseBackslash(
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural;
   globPattern : in out aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List ) is
begin
   --put_line( "parseBackslash" ); -- DEBUG
   expectChar( '\', rawWordValue, wordLen, wordPos );
   -- TODO: implicit backslash on command line may fool this by adding
   -- a semi-colon after the backslash.
   if endOfShellWord then
      err_shell( "missing character after backslash", wordPos );
   else
     globPattern := globPattern & "\" & element( rawWordValue, wordPos );
     getNextChar( rawWordValue, wordLen, wordPos );
   end if;
end parseBackslash;


-----------------------------------------------------------------------------
--  PARSE SINGLE QUOTED BACKSLASH
--
-- In a single quoted subword, a backslash escapes a single quote or a
-- backslash itself.
-----------------------------------------------------------------------------

--procedure parseSingleQuotedBackslash(
--   rawWordValue : aRawShellWord;
--   wordLen : natural;
--   wordPos : in out natural;
--   globPattern : in out aGlobShellWord;
--   bourneShellWordList : in out bourneShellWordLists.List ) is
--   ch : character;
--begin
--   expectChar( '\', rawWordValue, wordLen, wordPos );
--   ch := element( rawWordValue, wordPos );
--   if endOfShellWord then
--      err_shell( "missing character after backslash", wordPos );
--   elsif ch = ''' then
--     globPattern := globPattern & "\" & ch;
--     getNextChar( rawWordValue, wordLen, wordPos );
--   else
--     globPattern := globPattern & "\\";
--   end if;
--end parseSingleQuotedBackslash;


-----------------------------------------------------------------------------
--  PARSE SINGLE QUOTED SHELL SUBWORD
--
-- Syntax: 'word'
-- Handle a single quoted section of a shell word
-----------------------------------------------------------------------------

procedure parseSingleQuotedShellSubword(
      rawWordValue : aRawShellWord;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      bourneShellWordList : in out bourneShellWordLists.List ) is
   ch : character;
begin
   --put_line( "parseSingleQuotedShellSubword" ); -- DEBUG
   expectChar( ''', rawWordValue, wordLen, wordPos );

   while wordPos <= wordLen and not error_found and not endOfShellWord loop
      ch := element( rawWordValue, wordPos );

      -- However, we have to escape characters for the globbing pattern.
      -- there are no backslash escapes in single quotes

      case ch is
      when ''' =>
         exit;
      --when '\' =>                                     -- a backslash?
      --   parseSingleQuotedBackslash(
      --      rawWordValue, wordLen, wordPos, globPattern, bourneShellWordList );
                                                      -- is a glob char?
      when '~' => globPattern := globPattern & "\~";  -- escape \
         getNextChar( rawWordValue, wordLen, wordPos );
      when '\' => globPattern := globPattern & "\\";  -- escape \
         getNextChar( rawWordValue, wordLen, wordPos );
      when '*' => globPattern := globPattern & "\*";  -- escape *
         getNextChar( rawWordValue, wordLen, wordPos );
      when '[' => globPattern := globPattern & "\[";  -- escape [
         getNextChar( rawWordValue, wordLen, wordPos );
      when '?' => globPattern := globPattern & "\?";  -- escape ?
         getNextChar( rawWordValue, wordLen, wordPos );
      when others => globPattern := globPattern & ch; -- others? no esc
         getNextChar( rawWordValue, wordLen, wordPos );
      end case;
   end loop;

   expectChar( ''', rawWordValue, wordLen, wordPos );
end parseSingleQuotedShellSubword;


-----------------------------------------------------------------------------
--  PARSE DOUBLE QUOTED BACKSLASH
--
-- In a double quoted subword, a backslash escapes a double quote, dollar
-- sign, a backquote or a backslash itself.
-----------------------------------------------------------------------------

procedure parseDoubleQuotedBackslash(
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural;
   globPattern : in out aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List ) is
   ch : character;
begin
   expectChar( '\', rawWordValue, wordLen, wordPos );
   ch := element( rawWordValue, wordPos );
   if endOfShellWord then
      err_shell( "missing character after backslash", wordPos );
   elsif ch = '"' then
     globPattern := globPattern & "\" & ch;
     getNextChar( rawWordValue, wordLen, wordPos );
   elsif ch = '$' then
     globPattern := globPattern & "\" & ch;
     getNextChar( rawWordValue, wordLen, wordPos );
   elsif ch = '`' then
     globPattern := globPattern & "\" & ch;
     getNextChar( rawWordValue, wordLen, wordPos );
   elsif ch = '\' then
     globPattern := globPattern & "\" & ch;
     getNextChar( rawWordValue, wordLen, wordPos );
   else
     globPattern := globPattern & "\\";
   end if;
end parseDoubleQuotedBackslash;


-----------------------------------------------------------------------------
--  PARSE DOUBLE QUOTED SHELL WORD
--
-- Syntax: "word"
-- Handle a double quoted section of a shell word
-----------------------------------------------------------------------------

procedure parseDoubleQuotedShellSubword(
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural;
   globPattern : in out aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List ) is
   ch : character;
begin
   expectChar( '"', rawWordValue, wordLen, wordPos );

   while wordPos <= wordLen and not error_found and not endOfShellWord loop
      ch := element( rawWordValue, wordPos );

      -- However, we have to escape characters for the globbing pattern.

      case ch is
      when '"' =>
         exit;
      when '\' =>                                     -- a backslash?
         parseDoubleQuotedBackslash(
            rawWordValue, wordLen, wordPos, globPattern, bourneShellWordList );
      when '$' =>                                     -- an expansion
         parseDollarExpansion( rawWordValue, wordLen, wordPos, globPattern, bourneShellWordList, keep );
      when '`' =>
         -- This is permitted in double quotes in a Bourne shell
         parseBackQuotedShellSubword( rawWordValue, wordLen, wordPos, globPattern,
            keep, bourneShellWordList );
                                                      -- is a glob char?
      when '~' => globPattern := globPattern & "\~";  -- escape \
         getNextChar( rawWordValue, wordLen, wordPos );
      when '*' => globPattern := globPattern & "\*";  -- escape *
         getNextChar( rawWordValue, wordLen, wordPos );
      when '[' => globPattern := globPattern & "\[";  -- escape [
         getNextChar( rawWordValue, wordLen, wordPos );
      when '?' => globPattern := globPattern & "\?";  -- escape ?
         getNextChar( rawWordValue, wordLen, wordPos );
      when others => globPattern := globPattern & ch; -- others? no esc
         getNextChar( rawWordValue, wordLen, wordPos );
      end case;
   end loop;

   expectChar( '"', rawWordValue, wordLen, wordPos );
end parseDoubleQuotedShellSubword;


-----------------------------------------------------------------------------
--  PARSE DOUBLE QUOTED SQL WORD
--
-- Syntax: "word"
-- Handle a double quoted section of a sql word
-----------------------------------------------------------------------------

procedure parseDoubleQuotedSQLSubword(
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural;
   globPattern : in out aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List ) is
   ch : character;
begin
   expectChar( '"', rawWordValue, wordLen, wordPos );
   globPattern := globPattern & '"';

   while wordPos <= wordLen and not error_found and not endOfShellWord loop
      ch := element( rawWordValue, wordPos );

      -- However, we have to escape characters for the globbing pattern.

      case ch is
      when '"' =>
         exit;
      when '\' =>                                     -- a backslash?
         parseDoubleQuotedBackslash(
            rawWordValue, wordLen, wordPos, globPattern, bourneShellWordList );
      when '$' =>                                     -- an expansion
         parseSQLDollarExpansion( rawWordValue, wordLen, wordPos, globPattern,
            bourneShellWordList, keep, sqlDoubleNoGlob );
         globPattern := globPattern;
      -- when '`' =>
      --    -- This is permitted in double quotes in a Bourne shell
      --    parseBackQuotedShellSubword( rawWordValue, wordLen, wordPos, globPattern,
      --       keep, bourneShellWordList );
      when others => globPattern := globPattern & ch; -- others? no esc
         getNextChar( rawWordValue, wordLen, wordPos );
      end case;
   end loop;

   expectChar( '"', rawWordValue, wordLen, wordPos );
   globPattern := globPattern & '"';
end parseDoubleQuotedSQLSubword;


-----------------------------------------------------------------------------
--  PARSE SINGLE QUOTED SQL WORD
--
-- Syntax: "word"
-- Handle a single quoted section of a sql word.  The behaviour is the same
-- as a double quoted sql word.
-----------------------------------------------------------------------------

procedure parseSingleQuotedSQLSubword(
   rawWordValue : aRawShellWord;
   wordLen : natural;
   wordPos : in out natural;
   globPattern : in out aGlobShellWord;
   bourneShellWordList : in out bourneShellWordLists.List ) is
   ch : character;
begin
   expectChar( ''', rawWordValue, wordLen, wordPos );
   globPattern := globPattern & ''';

   while wordPos <= wordLen and not error_found and not endOfShellWord loop
      ch := element( rawWordValue, wordPos );

      -- However, we have to escape characters for the globbing pattern.

      case ch is
      when ''' =>
         exit;
      when '\' =>                                     -- a backslash?
         parseDoubleQuotedBackslash(
            rawWordValue, wordLen, wordPos, globPattern, bourneShellWordList );
      when '$' =>                                     -- an expansion
         parseSQLDollarExpansion( rawWordValue, wordLen, wordPos, globPattern,
            bourneShellWordList, keep, sqlSingleNoGlob );
         globPattern := globPattern;
      -- when '`' =>
      --    -- This is permitted in double quotes in a Bourne shell
      --    parseBackQuotedShellSubword( rawWordValue, wordLen, wordPos, globPattern,
      --       keep, bourneShellWordList );
      when others => globPattern := globPattern & ch; -- others? no esc
         getNextChar( rawWordValue, wordLen, wordPos );
      end case;
   end loop;

   expectChar( ''', rawWordValue, wordLen, wordPos );
   globPattern := globPattern & ''';
end parseSingleQuotedSQLSubword;



procedure parseBareShellSubword(
      rawWordValue : aRawShellWord;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      bourneShellWordList : in out bourneShellWordLists.List );


-----------------------------------------------------------------------------
--  PARSE BACK QUOTED SHELL SUBWORD
--
-- Syntax: `word`
-- Handle a back quoted section of a shell word.  A backquote subword can
-- occur as a bareword or within double quotes so it has a whitespace option.
-- Note substitutions happen when the back quotes execute:
--   export HELLO="a" ; echo `export HELLO="b" ; echo "$HELLO"`
--     displays "b"
--   echo `'`'`
--     is an error in bash - single or double quotes cannot escape a
--     backquote within backquoted commands
-----------------------------------------------------------------------------

procedure parseBackQuotedShellSubword(
      rawWordValue : aRawShellWord;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      whitespaceOption : whitespaceOptions;
      bourneShellWordList : in out bourneShellWordLists.List ) is
   ch : character;
   commands     : unbounded_string;
begin
   --put_line( "parseBackQuotedShellSubword" ); -- DEBUG
   expectChar( '`', rawWordValue, wordLen, wordPos );

   while wordPos <= wordLen and not error_found and not endOfShellWord loop
      ch := element( rawWordValue, wordPos );
      exit when ch = '`';
      if ch = '\' then
         parseBackslash( rawWordValue, wordLen, wordPos, aGlobShellWord( commands ), bourneShellWordList );
      else
         commands := commands & ch;
         getNextChar( rawWordValue, wordLen, wordPos );
      end if;
   end loop;

   -- put_line( "commands = " & to_string( commands ) ); -- DEBUG
   expectChar( '`', rawWordValue, wordLen, wordPos );

   -- The string returned may have special characters that need to be escaped
   -- for a glob pattern (such as a \).

   globPattern := globPattern & globBackslashEscape( doCommandSubstitution( commands ), whitespaceOption ) ;
   -- put_line( "end of back quotes, expanded word is = " & to_string( globPattern ) );
end parseBackQuotedShellSubword;


-----------------------------------------------------------------------------
--  PARSE BARE SHELL WORD
--
-- Syntax: bare-word
-- Handle an unquoted section of a shell word.
-----------------------------------------------------------------------------

procedure parseBareShellSubword(
      rawWordValue : aRawShellWord;
      wordLen : natural;
      wordPos : in out natural;
      globPattern : in out aGlobShellWord;
      bourneShellWordList : in out bourneShellWordLists.List ) is
   ch : character;
begin
   -- handle word and backslash characters

   while wordPos <= wordLen and not error_found and not endOfShellWord loop
      ch := element( rawWordValue, wordPos );
      exit when ch = ASCII.HT or ch = ' ' or ch = '"' or ch = ''';
      if ch = '\' then
         parseBackslash( rawWordValue, wordLen, wordPos, globPattern, bourneShellWordList );
      elsif ch = '$' then
         parseDollarExpansion( rawWordValue, wordLen, wordPos, globPattern, bourneShellWordList, trim );
      else
         globPattern := globPattern & ch;
         getNextChar( rawWordValue, wordLen, wordPos );
      end if;
   end loop;
end parseBareShellSubword;


-----------------------------------------------------------------------------
--  PARSE DOLLAR AT SIGN
--
-- $@ is a special case because we have to handle word splitting at the
-- parameter level.  The parameters are passed as-is into the word queue.
-----------------------------------------------------------------------------

procedure parseDollarAtSign(bourneShellWordList : in out bourneShellWordLists.List) is
begin
   if isExecutingCommand then
      for i in optionOffset+1..Argument_Count loop
           bourneShellWordLists.Queue( bourneShellWordList, to_unbounded_string(
             Argument( i ) ) );
      end loop;
   end if;
end parseDollarAtSign;


-----------------------------------------------------------------------------
--  PARSE SHELL WORD
--
-- Treat the token as a shell word.
-- Perform any substitutions, expansions and word splitting.
-----------------------------------------------------------------------------

procedure parseShellWord(
   bourneShellWordList : in out bourneShellWordLists.List ) is
   len       : natural;
   first_ch  : character;
   wordPos   : natural := 1;
   shellWord : aRawShellWord;
   globPattern : aGlobShellWord;
begin
  --put_line( "parseShellWord" ); -- DEBUG
--put_token; -- DEBUG
  resetShellScanner;

   -- A shell word consists of a sequence of subwords, whether bare words or
   -- quoted.

   if token = word_t then
      shellWord := aRawShellWord( identifiers( token ).value.all );
      len := length( shellWord );
      if len > 0 then
         -- $@ is a special case because the word splitting is not done
         -- the normal way.  We don't allow it to be combined with other
         -- subwords.
         if shellWord = "$@" then
            err_shell("$@ should be in double quotes", wordPos);
         elsif shellWord = ASCII.Quotation & "$@" & ASCII.Quotation then
            parseDollarAtSign( bourneShellWordList );
         else
            while wordPos <= len and not error_found and not endOfShellWord  loop
               first_ch := element( shellWord, wordPos );
               if first_ch = '"' then
                  parseDoubleQuotedShellSubword( shellWord, len, wordPos, globPattern, bourneShellWordList );
               elsif first_ch = ''' then
                  parseSingleQuotedShellSubword( shellWord, len, wordPos, globPattern, bourneShellWordList );
               elsif first_ch = '`' then
                  parseBackQuotedShellSubword( shellWord, len, wordPos, globPattern, trim, bourneShellWordList );
               else
                  parseBareShellSubword( shellWord, len, wordPos, globPattern, bourneShellWordList );
               end if;
            end loop;
            -- Globbing happens after substitutions and quotes are handled
            -- i.e. HELLO="*.txt" ls $HELLO will show text files
            doGlobPattern( globPattern, bourneShellWordList );
         end if;
      end if;
   elsif token = sql_word_t then
      -- An SQL word is a shell word with spaces in it containing the
      -- SQL command except for the first keywrod.
      shellWord := aRawShellWord( identifiers( token ).value.all );
      len := length( shellWord );
      if len > 0 then
         while wordPos <= len and not error_found and not endOfShellWord loop
            first_ch := element( shellWord, wordPos );
            if first_ch = '"' then
               parseDoubleQuotedSQLSubword( shellWord, len, wordPos, globPattern, bourneShellWordList );
            elsif first_ch = ''' then
               parseSingleQuotedSQLSubword( shellWord, len, wordPos, globPattern, bourneShellWordList );
            elsif first_ch = '`' then
               parseBackQuotedShellSubword( shellWord, len, wordPos, globPattern, trim, bourneShellWordList );
            elsif first_ch = ' ' or first_ch = ASCII.HT then
               -- unlike shell words, sql words have whitespace in them
               globPattern := globPattern & first_ch;
               getNextChar( shellWord, len, wordPos );
            else
               parseBareShellSubword( shellWord, len, wordPos, globPattern, bourneShellWordList );
            end if;
         end loop;
         -- SQL word is not globbed
         if length( globPattern ) > 0 then
            bourneShellWordLists.Queue( bourneShellWordList, anExpandedShellWord( globPattern ) );
         end if;
      end if;
   elsif token = symbol_t then
      -- This is primarily for "&"
      err_shell( "internal error: unexpected symbol", wordPos );
   elsif identifiers( token ).kind = new_t then
      -- If there's any kind of quotation, it will be a word not an identifier.
      -- This will always be a bareword.
      shellWord := aRawShellWord( identifiers( token ).name );
      len := length( shellWord );
      discardUnusedIdentifier( token );
      parseBareShellSubword( shellWord, len, wordPos, globPattern, bourneShellWordList );
      if length( globPattern ) > 0 then
         bourneShellWordLists.Queue( bourneShellWordList, anExpandedShellWord( globPattern ) );
      end if;
   -- The following may never happen...but are included here to catch the unexpected.
   -- These follow the same pattern as expected identifiers.
   elsif token = number_t then
     err( optional_yellow( "shell word") & " expected, not a " &
          optional_yellow( "number" ) );
   elsif token = strlit_t then
     err( optional_yellow( "shell word" ) & " expected, not a " &
          optional_yellow( "string literal" ) );
   elsif token = backlit_t then
     err( optional_yellow( "shell word" ) & " expected, not a " &
          optional_yellow( "backquoted literal" ) );
   elsif token = charlit_t then
     err( optional_yellow( "shell word" ) & " expected, not a " &
          optional_yellow( "character literal" ) );
   elsif is_keyword( token ) and token /= eof_t then
      err( optional_yellow( "shell word" ) & " expected, not a " &
           optional_yellow( "keyword" ) );
   else -- including EOF
      err( optional_yellow( "shell word" ) & " expected" );
   end if;

   if trace then
      declare
        theWord : anExpandedShellWord;
      begin
        if identifiers( token ).kind /= new_t then
           put_trace( "shell word '" & to_string( toEscaped( identifiers( token ).value.all ) ) &
              "' expands to:" );
        else
           put_trace( "shell word '" & to_string( toEscaped( identifiers( token ).name ) ) &
              "' expands to:" );
        end if;
        for i in 1..bourneShellWordLists.length( bourneShellWordList ) loop
            bourneShellWordLists.Find( bourneShellWordList, i, theWord );
            put_trace( to_string( toEscaped( unbounded_string( theWord ) ) ) );
        end loop;
      end;
   end if;

   getNextToken;

--put_line("end parseShellWord" ); -- DEBUG
end parseShellWord;


-----------------------------------------------------------------------------
--
--  Public Subprograms
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PARSE UNIQUE SHELL WORD
--
-- Parse a shell word.  The shell word should expand to a single word.
-- Otherwise an error will occur.
--
-- This is used for special words like a command name or a target for a
-- redirect.
-----------------------------------------------------------------------------

procedure parseUniqueShellWord( shellWord : in out anExpandedShellWord ) is
  wordList : bourneShellWordLists.List;
begin
  parseShellWord( wordList );
  if bourneShellWordLists.Length( wordList ) > 1 then
     err( "one shell word expected but it expanded to multiple words.  (SparForte requires commands that expand to one shell word.)" );
  elsif bourneShellWordLists.Length( wordList ) = 0 then
     err( "internal error: one shell word expected but it expanded to none" );
  else
     bourneShellWordLists.Pull(wordList, shellWord);
  end if;
end parseUniqueShellWord;


-----------------------------------------------------------------------------
--  ADD ADASCRIPT VALUE
--
-- Add an AdaScript value to the shell word list as if it was a final shell
-- word.  When using AdaScript values in shell commands, we put the values
-- as if they were expanded words into the word list.
-----------------------------------------------------------------------------

procedure addAdaScriptValue( wordList : in out bourneShellWordLists.List;
  unbounded_val : unbounded_string ) is
begin
  bourneShellWordLists.Queue( wordList, anExpandedShellWord( unbounded_val ) );
end addAdaScriptValue;


-----------------------------------------------------------------------------
--
--  Housekeeping
--
-----------------------------------------------------------------------------


-- procedure startShellScanner is
-- begin
--   null;
-- end startShellScanner;

-- procedure stopShellScanner is
-- begin
--   null;
-- end stopShellScanner;

end parser.decl.shell;
