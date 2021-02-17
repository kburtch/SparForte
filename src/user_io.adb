------------------------------------------------------------------------------
-- Reading the keyboard, writing to the terminal/console                    --
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

-----------------------------------------------------------------------------
--  BEEP
--
-- Ring the terminal bell.
-----------------------------------------------------------------------------

procedure beep is
begin
  simpleBeep;
end beep;


-----------------------------------------------------------------------------
--  GET KEY
--
-- Read a single keypress and don't show the character to user
-----------------------------------------------------------------------------

procedure getKey( ch : out character; nonblock : boolean := false ) is
begin
  simpleGetKey( ch, nonblock );
  -- constrain to lower ASCII
  if ch > ASCII.DEL then
     ch := character'val( character'pos( ch ) - 128 );
  end if;
end getKey;


-----------------------------------------------------------------------------
--  GET PROMPT INDENT
--
-- determine how far the command prompt indents from the
-- left hand side of the screen, taking into account
-- control characters and carriage returns / line feeds
-----------------------------------------------------------------------------

function getPromptIndent return natural is
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


-----------------------------------------------------------------------------
--  GET PROMPT EXTRA LINES
--
-- determine how far the command prompt indents from the
-- left hand side of the screen, taking into account
-- control characters and carriage returns / line feeds
-----------------------------------------------------------------------------

function getPromptExtraLines return natural is
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


-----------------------------------------------------------------------------
--  BOLD
--
-- Add to the string the character codes to draw the string in bold in the
-- terminal.
-----------------------------------------------------------------------------

function bold( s : string ) return string is
begin
  return to_string( term( bold ) ) & s & to_string( term( normal ) );
end bold;


-----------------------------------------------------------------------------
--  OPTIONAL BOLD
--
-- Add to the string the character codes to draw the string in bold in the
-- terminal, but only if not for GCC format errors.
-----------------------------------------------------------------------------

function optional_bold( s : string ) return string is
begin
  if gccOpt then
     return s;
  end if;
  return to_string( term( bold ) ) & s & to_string( term( normal ) );
end optional_bold;

-----------------------------------------------------------------------------
--  OPTIONAL RED
--
-- Return a string with terminal codes to draw the string
-- in red characters --colour is used, else inverse.
-- Return the string as-is if -g is used
-----------------------------------------------------------------------------

function optional_red( s : string ) return string is
begin
  if gccOpt then
     return s;
  elsif not colourOpt then
     return optional_inverse( s );
  end if;
  return to_string( term( red ) ) & s & to_string( term( white ) );
end optional_red;

-----------------------------------------------------------------------------
--  ADORN RED
--
-- Return a string with terminal codes to draw the string
-- in red characters --colour is used, else normal.
-----------------------------------------------------------------------------

function adorn_red( s : string ) return string is
begin
  if not colourOpt then
     return s;
  end if;
  return to_string( term( red ) ) & s & to_string( term( white ) );
end adorn_red;

-----------------------------------------------------------------------------
--  OPTIONAL YELLOW
--
-- return a string with terminal codes to draw the string
-- in yellow characters if -g not used
-----------------------------------------------------------------------------

function optional_yellow( s : string ) return string is
begin
  if gccOpt then
     return s;
  elsif not colourOpt then
     return optional_bold( s );
  end if;
  return to_string( term( yellow ) ) & s & to_string( term( white ) );
end optional_yellow;

-----------------------------------------------------------------------------
--  OPTIONAL GREEN
--
-- return a string with terminal codes to draw the string
-- in green characters if -g not used
-----------------------------------------------------------------------------

function optional_green( s : string ) return string is
begin
  if gccOpt then
     return s;
  elsif not colourOpt then
     return optional_bold( s );
  end if;
  return to_string( term( green ) ) & s & to_string( term( white ) );
end optional_green;

-----------------------------------------------------------------------------
--  ADORN GREEN
--
-- Return a string with terminal codes to draw the string
-- in green characters --colour is used, else normal.
-----------------------------------------------------------------------------

function adorn_green( s : string ) return string is
begin
  if not colourOpt then
     return s;
  end if;
  return to_string( term( green ) ) & s & to_string( term( white ) );
end adorn_green;


-----------------------------------------------------------------------------
--  INVERSE
--
-- Add to the string the character codes to draw the string in inverse in the
-- terminal.
-----------------------------------------------------------------------------

function inverse( s : string ) return string is
begin
  return to_string( term( inverse ) ) & s & to_string( term( normal ) );
end inverse;


-----------------------------------------------------------------------------
--  OPTIONAL INVERSE
--
-- Add to the string the character codes to draw the string in inverse in the
-- terminal, but only if not for GCC format errors.
-----------------------------------------------------------------------------

function optional_inverse( s : string ) return string is
begin
  if gccOpt then
     return s;
  end if;
  return to_string( term( inverse ) ) & s & to_string( term( normal ) );
end optional_inverse;


-----------------------------------------------------------------------------
--  TO PROTECTED VALUE
--
-- combines optional bold, secure data and escaped.  A null value will
-- return double single quotes.
-----------------------------------------------------------------------------

function toProtectedValue( s : unbounded_string ) return string is
begin
  if s = "" then
     return "''";
  else
     return optional_bold( toSecureData( to_string( toEscaped( s ) ) ) );
  end if;
end toProtectedValue;


-----------------------------------------------------------------------------
--  PUT TRACE
--
-- Display an escaped message to standard error in the format used when
-- "trace true" is used.  This does not check the tracing flag.
-----------------------------------------------------------------------------

procedure put_trace( msg : string; icon : string := "" ) is
begin
  if icon /= "" and boolean(colourOpt) then
     put_line( standard_error, adorn_green( to_string( "=> (" & icon & " " & toEscaped( to_unbounded_string( msg ) ) ) & ")" ) );
  else
     put_line( standard_error, adorn_green( to_string( "=> (" & toEscaped( to_unbounded_string( msg ) ) ) & ")" ) );
  end if;
end put_trace;

-----------------------------------------------------------------------------
--  PUT TRACE ERROR
--
-- Display an escaped message to standard error in the format used when
-- "trace true" is used.  This does not check the tracing flag.
-----------------------------------------------------------------------------

procedure put_trace_error( msg : string; icon : string := "" ) is
begin
  if icon /= "" and boolean(colourOpt) then
     put_line( standard_error, optional_red( to_string( "=> (" & icon & " " & toEscaped( to_unbounded_string( msg ) ) ) & ")" ) );
  else
     put_line( standard_error, optional_red( to_string( "=> (" & toEscaped( to_unbounded_string( msg ) ) ) & ")" ) );
  end if;
end put_trace_error;


-----------------------------------------------------------------------------
--  CHECK DISPLAY
--
-- if the terminal display changed, update for the changes. ttype is the TERM
-- environment variable value.
-----------------------------------------------------------------------------

procedure checkDisplay( ttype : unbounded_string ) is
begin
  updateTtyAttributes( ttype );
  updateDisplayInfo;
end checkDisplay;


-----------------------------------------------------------------------------
--  TERMINAL RESET
--
-- reset the terminal.  ttype is the TERM environment variable value.
-----------------------------------------------------------------------------

procedure terminalReset( ttype : unbounded_string ) is
begin
  if isatty( stdout ) = 1 then
     put( term( reset ) );
     checkDisplay( ttype );
  end if;
end terminalReset;


-----------------------------------------------------------------------------
--  TERMINAL CLEAR
--
-- clear the terminal.  ttype is the TERM environment variable value.
-----------------------------------------------------------------------------

procedure terminalClear( ttype : unbounded_string ) is
begin
  if isatty( stdout ) = 1 then
     put( term( reset ) );
     put( term( clear ) );
     checkDisplay( ttype );
  end if;
end terminalClear;


-----------------------------------------------------------------------------
--  CLEAR HISTORY
--
-- Clear the command line history
-----------------------------------------------------------------------------

procedure clearHistory is
begin
  historyNext := 1;
  for i in historyArray'range loop
      history(i).line := null_unbounded_string;
  end loop;
end clearHistory;


-----------------------------------------------------------------------------
--  DISPLAY VERSION SPLASH
--
-- display --version message.  This is located here because it
-- uses term attributes.
-----------------------------------------------------------------------------

procedure displayVersionSplash is
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


-----------------------------------------------------------------------------
--  DISPLAY COPYRIGHT SPLASH
--
-- display copyright message.  This is located here because it
-- uses term attributes.  Suppress the message on a login shell
-- or if there is no tty.
-- Some of this is now defind in the world.ads file.
-----------------------------------------------------------------------------

procedure displayCopyrightSplash is
begin
  if isatty( stdout ) = 1 and not isLoginShell then
     Put_Line( "Type ""help"" for help" );
  end if;
end displayCopyrightSplash;


-----------------------------------------------------------------------------
--  UTF-8 icons
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  UTF BALLOT
--
-- Output the bytes for a UTF-8 "x" ballot symbol.
-----------------------------------------------------------------------------

function utf_ballot return string is
begin
  if colourOpt then
     -- if colourOpt then
     --    return character'val( 226 ) & character'val( 157 ) & character'val( 140 );
     -- else
     return character'val( 226 ) & character'val( 156 ) & character'val( 151 );
     -- end if;
  end if;
  return "";
end utf_ballot;


-----------------------------------------------------------------------------
--  UTF CHECKMARK
--
-- Output the bytes for a UTF-8 checkmark symbol.
-----------------------------------------------------------------------------
function utf_checkmark return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 156 ) & character'val( 147 );
  end if;
  return "";
end utf_checkmark;


-----------------------------------------------------------------------------
--  UTF WARNING SIGN
--
-- Output the bytes for a UTF-8 warning sign symbol.
-----------------------------------------------------------------------------

function utf_warning_sign return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 154 ) & character'val( 160 );
  end if;
  return "";
end utf_warning_sign;


-----------------------------------------------------------------------------
--  UTF WRISTWATCH
--
-- Output the bytes for a UTF-8 watch symbol.
-----------------------------------------------------------------------------

function utf_wristwatch return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 140 ) & character'val( 154 );
  end if;
  return "";
end utf_wristwatch;


-----------------------------------------------------------------------------
--  UTF LEFT
--
-- Output the bytes for a UTF-8 watch symbol.
-----------------------------------------------------------------------------

function utf_left return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 148 ) & character'val( 148 );
  end if;
  return "";
end utf_left;


-----------------------------------------------------------------------------
--  UTF RIGHT
--
-- Output the bytes for a UTF-8 watch symbol.
-----------------------------------------------------------------------------

function utf_right return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 148 ) & character'val( 152 );
  end if;
  return "";
end utf_right;


-----------------------------------------------------------------------------
--  UTF TRIANGLE
--
-- Output the bytes for a UTF-8 triangle.
-----------------------------------------------------------------------------

function utf_triangle return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 150 ) & character'val( 179 );
  end if;
  return "";
end utf_triangle;


-----------------------------------------------------------------------------
--  UTF HORIZONTAL LINE
--
-- Output the bytes for a UTF-8 watch symbol.
-----------------------------------------------------------------------------

function utf_horizontalLine return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 148 ) & character'val( 128 );
  end if;
  return "";
end utf_horizontalLine;

-- Return the bytes for a UTF-8 bullet, otherwise asterisk

function utf_bullet return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 128 ) & character'val( 162 );
  end if;
  return "*";
end utf_bullet;

-- Return the bytes for a UTF-8 diamond, otherwise a minus sign

function utf_diamond return string is
begin
  if colourOpt then
     return character'val( 226 ) & character'val( 151 ) & character'val( 134 );
  end if;
  return "-";
end utf_diamond;


end user_io;

