------------------------------------------------------------------------------
-- Reading the keyboard, writing to the terminal/console                    --
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

with ada.text_io,
    ada.strings.unbounded.text_io,
    ada.calendar,
    spar_os,
    spar_os.tty,
    world,
    string_util;
use ada.text_io,
    ada.strings.unbounded.text_io,
    ada.calendar,
    spar_os,
    spar_os.tty,
    world,
    string_util;

package body user_io is


procedure beep is
  -- beep the terminal
begin
  simpleBeep;
end beep;

procedure getKey( ch : out character; nonblock : boolean := false ) is
-- read a single keypress and don't show the character to user
begin
  simpleGetKey( ch, nonblock );
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

-----------------------------------------------------------------------------
-- PUT TRACE
--
-----------------------------------------------------------------------------

procedure put_trace( msg : string ) is
-- Display an escaped message to standard error in the format used when
-- "trace true" is used.  This does not check the tracing flag.
begin
  put_line( standard_error, "=> (" & toEscaped( to_unbounded_string( msg ) ) & ")" );
end put_trace;

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
     Put( "SparForte version " );
     if released then
        Put_Line( version );
     else
        Put_Line( version & " (Build ID " & buildDate & ')' );
     end if;
     Put_Line( copyright );
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

