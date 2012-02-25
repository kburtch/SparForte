
------------------------------------------------------------------------------
-- Reading the keyboard, writing to the terminal/console                    --
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

with ada.text_io,
    ada.strings.unbounded.text_io,
    --bush_os.spawn,
    ada.calendar,
    bush_os.tty,
    gnat.regexp,
    gnat.os_lib,
    gnat.directory_operations,
    world,
    signal_flags,
    string_util;
use ada.text_io,
    ada.strings.unbounded.text_io,
    bush_os,
    --bush_os.spawn,
    ada.calendar,
    bush_os.tty,
    gnat.regexp,
    gnat.os_lib,
    gnat.directory_operations,
    signal_flags,
    world,
    string_util;

package body user_io is


procedure beep is
  -- beep the terminal
begin
  simpleBeep;
end beep;

procedure getKey( ch : out character ) is
-- read a single keypress and don't show the character to user
begin
  simpleGetKey( ch );
  -- constrain to lower ASCII
  if ch > ASCII.DEL then
     ch := character'val( character'pos( ch ) - 128 );
  end if;
end getKey;

function getPromptIndent return natural is
  -- determine how far the command prompt indents from the
  -- left hand side of the screen, taking into account
  -- control characters and carriage returns / line feeds
  len : natural := 0;
begin
  for i in 1..length( prompt ) loop
      if element( prompt, i ) = ASCII.CR then      -- carriage return?
         len := 0;                                 -- back to left side
      elsif element( prompt, i ) = ASCII.LF then   -- line feed?
         len := 0;                                 -- back to left side
      elsif element( prompt, i ) >= ' ' then       -- not a control char?
         len := len + 1;                           -- count it
      end if;
  end loop;
  return len;
end getPromptIndent;

function getPromptExtraLines return natural is
  -- determine how far the command prompt indents from the
  -- left hand side of the screen, taking into account
  -- control characters and carriage returns / line feeds
  last : character := ASCII.NUL;
  extra : natural := 0;
begin
  for i in 1..length( prompt ) loop
      if element( prompt, i ) = ASCII.CR then      -- carriage return?
         extra := extra + 1;                       -- back to left side
      elsif element( prompt, i ) = ASCII.LF then   -- line feed?
         if last /= ASCII.CR then                  -- for MS-DOS
            extra := extra + 1;                    -- back to left side
         end if;
      end if;
      last := element( prompt, i );
  end loop;
  return extra;
end getPromptExtraLines;

procedure getLine( line : out unbounded_string; keepHistory : boolean
  := false ) is
-- read a line from the keyboard
-- Note: the keymap is hard-coded, but really should be taken from
-- current terminal settings.

  procedure redrawLine( oldPos : natural; newPos : natural := 0 ) is
  -- redraw the command line
    oldLines  : integer; -- number of lines of old text
    oldChars  : integer; -- number of characters from margin
  begin
    -- these two formulas are not quite right
    -- oldChars and oldLines are the coordinates of the cursor
    if keepHistory then
       -- if keepHistory, then we're at the command line
       --oldLines := ( getPromptIndent+oldPos ) /
       --   integer( displayInfo.col ) + getPromptExtraLines;
       oldLines := ( getPromptIndent+oldPos-2 ) /
          integer( displayInfo.col ) + getPromptExtraLines+1;
       oldChars  := (getPromptIndent + oldPos) -
          ( ( oldLines-1-getPromptExtraLines) * integer( displayInfo.col ) );
    else
       -- otherwise in the get_line function or someplace w/out a prompt
       oldLines := (oldPos-1) / integer( displayInfo.col )+1;
       oldChars  := oldPos - ( (oldLines-1) * integer( displayInfo.col ) );
    end if;
    -- position at start of old line
    for i in 1..oldChars-1 loop
        put( ASCII.BS );
    end loop;
    for i in 1..oldLines-1 loop
        put( term( cleol ) );
        put( term( up ) );
    end loop;
    -- draw new line
    if keepHistory then
       put_bold( prompt );
    end if;
    put( line );
    -- the final character on the bottom-right of the screen has special
    -- properties (ie. it triggers the scrolling).  We need to compensate
    -- or the final character won't appear.  If we don't scroll the screen
    -- by writing another character, the final character of the line won't
    -- be displayed!
    if getPromptIndent + length( line ) = 80 then
       put( ' ' );
       put( ASCII.BS );
    end if;
    put( term( cleol ) );
    -- reposition cursor, if desired
    if newPos > 0 then
       for i in newPos..length( line ) loop   -- reposition cursor
           put( ASCII.BS );                   -- (not very efficient)
       end loop;
    end if;
    --put( " - " );
    --put( oldchars'img );
    --put( displayInfo.col'img );
  end redrawLine;

---> IS DIRECTORY
--
-- A quick and simple function to check to see if string dir refers
-- to a directory.
-----------------------------------------------------------------------------

function isDirectory( dir : unbounded_string ) return boolean is
   f : dir_type;
   result : boolean := true;
begin
  begin
  GNAT.Directory_Operations.Open( f, to_string( dir ) );
  GNAT.Directory_Operations.Close( f );
  -- TODO: should do a stat() probably more efficient
  exception when others => result := false;
  end;
  return result;
end isDirectory;

---> SLASHIFY PATH
--
-- Add a trailing character for a path.  If a directory, add a trailing /.
-- If a file, add a trailing space.  If both, add nothing.
-----------------------------------------------------------------------------

function slashifyPath( dir, path : unbounded_string ) return string is
  f  : file_type;
  ch : character := ASCII.NUL;
begin
  begin
    Ada.Text_IO.Open( f, in_file, to_string( dir & Dir_Separator & path ) );
    Ada.Text_IO.Close( f );
    if isDirectory( dir & Dir_Separator & path ) then
       ch := Dir_Separator;
    else
       ch := ' ';
    end if;
  exception when others => null;
  end;
  if ch /= ASCII.NUL then
     return "" & ch;
  end if;
  return "";
end slashifyPath;

 procedure completePathname( dir : unbounded_string;
    s : unbounded_string;
    result : out unbounded_string;
    list : boolean := false ) is
    globCriteria : regexp;
    count        : natural := 0;
    dir2test     : Dir_Type;
    fileName     : string(1..256);
    fileNameLen  : natural;
    noPWD        : boolean := false;
    bestCommon   : unbounded_string; -- longest common part
    listCount    : natural := 0;
    isListing    : boolean := list;
    expandedDir  : unbounded_string;
    home_id      : identifier;
  begin
    -- Show a list of possible matches? Move down a line.
    if isListing then
       New_Line;
       put( term( cleop ) );
    end if;
    -- A tilde?  Then substitute in the home directory.
    if length( dir ) > 0 and then element( dir, 1) = '~' then
       findIdent( to_unbounded_string( "HOME" ), home_id );
       expandedDir := identifiers( home_id ).value & slice( dir, 2, length(dir));
    else
       expandedDir := dir;
    end if;
--put_line( "EXPANDED DIR = '" & dir & "' => " & expandedDir & "'+'" & s & "'" ); -- DEBUG
    bestCommon   := null_unbounded_string;
    globCriteria := Compile( to_string( s ), Glob => true,
       Case_Sensitive => true );
    begin
       open( dir2test, to_string( expandedDir ) );
    exception when others =>
      noPWD := true;
    end;
    if noPWD then
       beep;
       result := null_unbounded_string;
       return;
    end if;
    -- KB: 12/02/18 - gcc dir ops changes, no longer returns "." but we'll
    -- check below to be safe.
    --read( dir2test, fileName, fileNameLen ); -- skip "."
    --read( dir2test, fileName, fileNameLen ); -- skip ".."
    loop
      read( dir2test, fileName, fileNameLen );
      exit when fileNameLen = 0;
      if fileName( 1..fileNameLen ) = "." then
         null;
      elsif fileName( 1..fileNameLen ) = ".." then
         null;
      elsif Match( fileName(1..fileNameLen ) , globCriteria ) then
         count := count + 1;
         if isListing then
            put_line( fileName(1..fileNameLen) & slashifyPath( dir, to_unbounded_string( fileName(1..fileNameLen) ) ) );
            listCount := listCount + 1;
            isListing := listCount /= 10; -- no more than 10
            if not isListing then
               put_line( optional_bold( "[Maybe More]" ) );
            end if;
         end if;
         if count = 1 then
            result := to_unbounded_string( fileName( 1..fileNameLen ) );
            bestCommon := result;
         else
            -- more than 1 hit? trim bestCommon start to fit the
            -- result.  If bestCommon is null, no common starting
            -- characters.
            for i in 1..fileNameLen loop
                exit when i > length( bestCommon );
                if i = fileNameLen then
                   bestCommon := head( bestCommon, i-1 );
                elsif element( bestcommon, i ) /= fileName(i) then
                   if i = 1 then
                      bestCommon := null_unbounded_string;
                   else
                      bestCommon := head( bestCommon, i-1 );
                   end if;
                end if;
            end loop;
         end if;
      end if;
    end loop;
    close( dir2test );
    if count > 1 then
       beep;
       result := bestCommon;
    elsif count = 0 then
       beep;
       result := null_unbounded_string;
    end if;
  end completePathname;

  procedure lastWord( s : unbounded_string; lw : out unbounded_string;
    lp : out natural ) is
    -- remove the last "word" delinated by a space from the string
    -- return the ending character of the rest of the string
  begin
    lp := 0;
    for i in reverse 1..length( s )-1 loop
        if element( s, i ) = ' ' then
           lp := i;
           exit;
        end if;
    end loop;
    lw := to_unbounded_string( slice( s,  lp+1, length( s ) ) );
  end lastWord;

  vi_escape    : boolean := false; -- true if expecting vi-mode command
  arrow_escape : boolean := false; -- true if expecting VT-100 arrow
  last_was_esc : boolean := false; -- true if last char was ESC
  ch         : character; -- last character read
  pos        : natural;   -- position to insert next character
  old_pos    : natural;   -- for redrawing lines
  histpos    : integer;   -- position in history
  crit_pos   : natural;   -- for searching history
  criteria   : unbounded_string;
  justCompleted : boolean := false;
  pwd_id     : identifier;
begin
  line     := null_unbounded_string;
  criteria := null_unbounded_string;
  pos      := 1;
  histpos  := historyNext;

  loop
<<retry>> getKey( ch );
    if wasSIGINT then                   -- if SIGINT raised
       return;                          -- let caller determine what to do
    end if;

    -- VI ESCAPE MODE HANDLING
    --
    -- process sequence and convert to its emacs mode equivalent

    if ch = ASCII.ESC then
       if last_was_esc then             -- two escapes?
          ch := ASCII.HT;               -- cmdline completion
          last_was_esc := false;        -- cancel vi mode
          vi_escape := false;           -- exit vi escape
       else                             -- first escape?
          vi_escape := not vi_escape;   -- enter/exit vi escape mode
          if vi_escape then
             last_was_esc := true;         -- remember in case double esc
          end if;
          goto retry;                   -- and go get next key
       end if;
    elsif arrow_escape then                   -- ESC + [ + ...?
       last_was_esc := false;
       arrow_escape := false;                 -- got our arrow code
       if ch = 'A' then                       -- A = up
          ch := ASCII.DLE;
       elsif ch = 'B' then                    -- B = down
          ch := ASCII.SO;
       elsif ch = 'C' then                    -- C = right
          ch := ASCII.ACK;
       elsif ch = 'D' then                    -- D = left
          ch := ASCII.STX;
       else
          beep;                               -- otherwise unknown
       end if;
    elsif vi_escape then
       last_was_esc := false;
       if ch ='[' then                        -- same as control-n
          arrow_escape := true;
          vi_escape := false;                 -- cancel vi mode
          goto retry;
       elsif ch ='j' then                     -- same as control-n
          ch := ASCII.SO;
       elsif ch = 'k' then                    -- same as control-p
          ch := ASCII.DLE;
       elsif ch = 'h' then                    -- same as control-b
          ch := ASCII.STX;
       elsif ch = 'l' then                    -- same as control-f
          ch := ASCII.ACK;
       elsif ch = '^' then                    -- same as control-a
          ch := ASCII.SOH;
       elsif ch = '$' then                    -- same as control-e
          ch := ASCII.ENQ;
       else                                   -- else not supported
          vi_escape := false;                 -- drop out of vi escape mode
          beep;                               -- beep at illegal character
          goto retry;                         -- and get next key
       end if;
    elsif ch = ASCII.EOT then                 -- control-d / end of file?
       if length( line ) = 0 and keepHistory then  -- nothing at prompt?
          line := to_unbounded_string( "return" ); -- treat as "return"
          pos := 5;
          put( line );
       end if;                                -- otherwise
       ch := ASCII.CR;                        -- treat as end of input
    end if;

    if ch /= ASCII.HT then
       justCompleted := false;
    end if;

    -- EMACS MODE HANDLING

    case ch is
    when ASCII.HT =>                         -- TAB = pathname completion
       declare
          path : unbounded_string;
          dir  : unbounded_string;
          file : unbounded_string;
          lp   : natural;
          listFiles : boolean := false;
       begin
          if justCompleted then
             listFiles := true;
          else
             justCompleted := true;
          end if;
          lastWord( line, path, lp );
          if length( path ) = 0 then
             beep;
             goto retry;
          end if;
          -- as a special case, if the path is the name of a directory,
          -- just add a slash and redraw the line (bash works this way)
          -- I set justCompleted to false to be in line with bash's
          -- behaviour. ('ls .' , you have to hit tab 3 times to get list)
          if element( path, length( path ) ) /= Dir_Separator then
             if isDirectory( path ) then
                line := line & Dir_Separator;
                justCompleted := false;
                goto redraw;
             end if;
          end if;
          -- for path x/y, dir = x and y = file
          -- for path x/, dir = x and basename file = . (i.e. itself)
          dir  := dirname( path );
          -- we don't want .* in this case, just * unless the user
          -- specifically adds a /.
          if element( path, length( path ) ) /= Dir_Separator then
             file := basename( path );
          else
             file := null_unbounded_string;
          end if;
          file := file & "*";
--put_line( "BREAKDOWN = '" & path & "' => " & dir & "'+'" & file & "'" ); -- DEBUG
          if listFiles then
             completePathname( dir, file, path, list => true );
          else
             completePathname( dir, file, path );
          end if;
          if length( path ) > 0 then
             if to_string( dir ) = "." then
                line := slice( line, 1, lp ) & path;
             else
                line := slice( line, 1, lp ) & dir & Dir_Separator & path;
             end if;
             declare
                -- if a file, add a ' '.
                f : file_type;
             begin
                Ada.Text_IO.Open( f, in_file, to_string( dir & Dir_Separator & path ) );
                Ada.Text_IO.Close( f );
                -- should do a stat()
                declare
                   -- if a dir, add a '/'
                   f : dir_type;
                begin
                   GNAT.Directory_Operations.Open( f, to_string( dir & Dir_Separator & path ) );
                   GNAT.Directory_Operations.Close( f );
                   -- TODO: should do a stat()
                   line := line & Dir_Separator;
                exception when others =>
                   line := line & " ";
                end;
             exception when others => null;
             end;
          end if;
<<redraw>>old_pos := pos;                     -- remember old position
          pos := length( line ) + 1;          -- new position at end
          redrawLine( old_pos, pos );
       end;
    when ASCII.CR | ASCII.LF =>
       new_line;
       if length( line ) > 0 and keepHistory then -- don't save empty
          history( historyNext ).line := line;           -- lines
          findIdent( to_unbounded_string( "PWD" ), pwd_id ); -- TODO: SLOW!
          if pwd_t /= eof_t then
             history( historyNext ).pwd:= identifiers( pwd_id ).value;
          end if;
          history( historyNext ).time := Ada.Calendar.Clock; -- time
          historyNext := historyNext + 1;
          if historyNext > historyArray'last then
             historyNext := 1;
          end if;
       end if;
       exit;
    when ASCII.BS | ASCII.DEL =>              -- control-h or delete key
       if pos > 1 then
          delete( line, pos-1, pos-1 );       -- delete char to left of cursor
          old_pos := pos;                     -- remember old position
          pos := pos - 1;                     -- new position is one to left
          redrawLine( old_pos, pos );         -- redraw line, cursor at end
       else                                   -- nothing to delete?
          beep;                               -- beep
       end if;
    when ASCII.STX =>                         -- control-b
       if pos > 1 then
          old_pos := pos;                     -- remember old position
          pos := pos - 1;                     -- new position is one to left
          put( ASCII.BS );                    -- move cursor
       else                                   -- no place to go?
          beep;                               -- beep
       end if;
    when ASCII.ACK =>                         -- control-f
          if pos <= length( line ) then
             old_pos := pos;                  -- remember old position
             pos := pos + 1;                  -- new position is one to right
             put( term( right ) );            -- move cursor
          else                                -- no place to go?
             beep;                            -- beep
          end if;
    when ASCII.SO =>                          -- control-n
      if histpos = historyNext or not keepHistory then
         beep;
      else
         histpos := histpos + 1;
         if histpos > historyArray'last then
            histpos := 1;
         end if;
         old_pos := pos;                     -- remember old position
         line := history( histpos ).line;
         pos := length( line ) + 1;          -- new position at end of string
         redrawLine( old_pos, pos );         -- redraw line, cursor at end
      end if;
    when ASCII.DLE =>                         -- control-p
      if not keepHistory then
         beep;
      else
         old_pos := histpos;
         histpos := histpos - 1;
         if histpos < 1 then
            histpos := historyArray'last;
         end if;
         if histpos = historyNext then
            histpos := old_pos;
            beep;
         else
            old_pos := pos;                     -- remember old position
            line := history( histpos ).line;
            pos := length( line ) + 1;          -- new position at end of string
            redrawLine( old_pos, pos );         -- redraw line, cursor at end
         end if;
      end if;
    when ASCII.CAN =>                     -- control-x/c
      old_pos := pos;                     -- remember old position
      pos := 1;                           -- empty string now
      line := null_unbounded_string;
      redrawLine( old_pos );              -- redraw line
    when ASCII.SOH =>                     -- control-a
      redrawLine( pos, 1 );
      pos := 1;                           -- fix position
    when ASCII.ENQ =>                     -- control-e
      old_pos := pos;                     -- remember old position
      pos := length( line ) + 1;          -- new position at end of string
      redrawLine( old_pos, pos );         -- redraw line, cursor at end
    when ASCII.DC2 =>                         -- control-r
      if pos = 1 then
         beep;
         goto retry;
      elsif pos > length( line ) then
         criteria := line;
      else
         criteria := Delete( line, pos, length( line ) );
      end if;
      old_pos := histpos;
      crit_pos := pos;
      loop
        histpos := histpos - 1;
        if histpos < 1 then
           histpos := historyArray'last;
        end if;
        if histpos = historyNext then         -- out of history?
           line := criteria;                  -- restore old line
           histpos := old_pos;                -- that's all folks
           beep;
           exit;
        end if;
        line := history( histpos ).line;      -- next history line
        if Index( line, To_String( Criteria ) ) = 1 then -- matches criteria?
           pos := length( line ) + 1;         -- new position at end of string
           redrawLine( crit_pos, pos );       -- redraw line, cursor at end
           exit;
        end if;
      end loop;
    when ASCII.GS =>                          -- control-] (forward char search)
      old_pos := pos;
      getKey( ch );
      for i in pos+1..length( line ) loop
          if Element( line, i ) = ch then
             pos := i;
             exit;
          end if;
      end loop;
      if pos = old_pos then
         beep;
      else
         redrawLine( old_pos, pos );
      end if;
    when others =>
       if pos > length( line ) then           -- adding at end of line?
          line := line & ch;                  -- append character
          pos := pos + 1;                     -- new position is one to right
          put( ch );                          -- display character
       else
          insert( line, pos, ch & "" );       -- insert the character
          old_pos := pos;                     -- remember old position
          pos := pos + 1;                     -- new position is one to right
          redrawLine( old_pos, pos );         -- redraw line, cursor at end
       end if;
    end case;
  end loop;
end getLine;

function bold( s : string ) return string is
begin
  return to_string( term( bold ) ) & s & to_string( term( normal ) );
end bold;

function optional_bold( s : string ) return string is
begin
  if gccOpt then
     return s;
  end if;
  return to_string( term( bold ) ) & s & to_string( term( normal ) );
end optional_bold;

procedure put_bold( s : string ) is
begin
   put( bold( s ) );
end put_bold;

procedure put_bold( s : unbounded_string ) is
begin
  put( term( bold ) & s & term( normal ) );
end put_bold;

function inverse( s : string ) return string is
begin
  return to_string( term( inverse ) ) & s & to_string( term( normal ) );
end inverse;

function optional_inverse( s : string ) return string is
begin
  if gccOpt then
     return s;
  end if;
  return to_string( term( inverse ) ) & s & to_string( term( normal ) );
end optional_inverse;

procedure put_inverse( s : string ) is
begin
   put( bold( s ) );
end put_inverse;

procedure put_inverse( s : unbounded_string ) is
begin
  put( term( bold ) & s & term( normal ) );
end put_inverse;

procedure checkDisplay( ttype : unbounded_string ) is
begin
  updateTtyAttributes( ttype );
  updateDisplayInfo;
end checkDisplay;

procedure terminalReset( ttype : unbounded_string ) is
begin
  if isatty( stdout ) = 1 then
     put( term( reset ) );
     checkDisplay( ttype );
  end if;
end terminalReset;

procedure terminalClear( ttype : unbounded_string ) is
begin
  if isatty( stdout ) = 1 then
     put( term( reset ) );
     put( term( clear ) );
     checkDisplay( ttype );
  end if;
end terminalClear;

procedure clearHistory is
  -- Clear the command line history
begin
  historyNext := 1;
  for i in historyArray'range loop
      history(i).line := null_unbounded_string;
  end loop;
end clearHistory;

procedure displayVersionSplash is
  -- display --version message.  This is located here because it
  -- uses term attributes.
begin
  if isatty( stdout ) = 1 then
     Put( "SparForte (Business Shell, BUSH) version " );
     Put_Line( version );
     Put_Line( "Copyright (c)2001-2011 Free Software Foundation");
     Put_Line( "This is free software; see the source for copying conditions." );
     Put_Line( "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE." );
  end if;
end displayVersionSplash;

procedure displayCopyrightSplash is
  -- display copyright message.  This is located here because it
  -- uses term attributes.  Suppress the message on a login shell
  -- or if there is no tty.
begin
  if isatty( stdout ) = 1 and not isLoginShell then
     -- Put( "BUSH " );
     -- Put( version );
     -- Put_Line( " Copyright (c)2001-2011 Free Software Foundation");
     -- Put_Line( "AdaCGI 1.6 Copyright (c)2000 David A. Wheeler & Free Software Foundation");
     -- Put_Line( "APQ 2.1 Copyright (c)2002-2003 Warren W. Gay & Free Software Foundation");
     -- Put_Line( "MD5 Copyright RSA Data Security, Inc. -- Message-Digest Algorithm" );
     Put_Line( "Type ""help"" for help" );
     -- New_Line;
  end if;
end displayCopyrightSplash;

end user_io;

