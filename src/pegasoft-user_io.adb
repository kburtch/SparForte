------------------------------------------------------------------------------
-- Reading the keyboard, writing to the terminal/console                    --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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

with ada.strings.unbounded.text_io,
    ada.calendar,
    spar_os,
    spar_os.tty,
    pegasoft.numerics,
    pegasoft.strings;
use ada.strings.unbounded.text_io,
    ada.calendar,
    spar_os,
    spar_os.tty,
    pegasoft.numerics,
    pegasoft.strings;

package body pegasoft.user_io is


-----------------------------------------------------------------------------
--
-- SUPPORT FUNCTIONS FOR MESSAGES
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  PUT LINE RETRY
--
-- On some operating systems, put_line can raise a device_error when an
-- interrupted system call occurs.
-- TODO: this is a workaround.  A better solution is needed
-- TODO: should probably use these functions throughout
-----------------------------------------------------------------------------

procedure put_line_retry( output_file : file_type; s : string ) is
  retryCnt : natural := 0;
begin
  loop
     begin
        put_line( output_file, s );
        retryCnt := 2;
     exception when device_error =>
        retryCnt := retryCnt + 1;
     end;
     exit when retryCnt >= 2;
  end loop;
end put_line_retry;

procedure put_line_retry( s : string ) is
begin
  put_line_retry( standard_output, s );
end put_line_retry;

procedure put_line_retry( output_file : file_type; us : unbounded_string ) is
begin
  put_line_retry( output_file, to_string( us ) );
end put_line_retry;

procedure put_line_retry( us : unbounded_string ) is
begin
  put_line_retry( standard_output, to_string( us ) );
end put_line_retry;

procedure new_line_retry( output_file : file_type ) is
  retryCnt : natural := 0;
begin
  loop
     begin
        new_line( output_file );
        retryCnt := 2;
     exception when device_error =>
        retryCnt := retryCnt + 1;
     end;
     exit when retryCnt >= 2;
  end loop;
end new_line_retry;

procedure new_line_retry is
begin
  new_line( standard_output );
end new_line_retry;


-----------------------------------------------------------------------------
--  PUT RETRY
--
-- On some operating systems, put_line can raise a device_error when an
-- interrupted system call occurs.
-- TODO: this is a workaround.  A better solution is needed
-- TODO: should probably use these functions throughout
-----------------------------------------------------------------------------

procedure put_retry( output_file : file_type; s : string ) is
  retryCnt : natural := 0;
begin
  loop
     begin
        put( output_file, s );
        retryCnt := 2;
     exception when device_error =>
        retryCnt := retryCnt + 1;
     end;
     exit when retryCnt >= 2;
  end loop;
end put_retry;

procedure put_retry( s : string ) is
begin
  put_retry( standard_output, s );
end put_retry;

procedure put_retry( output_file : file_type; us : unbounded_string ) is
begin
  put_retry( output_file, to_string( us ) );
end put_retry;

procedure put_retry( us : unbounded_string ) is
begin
  put_retry( standard_output, to_string( us ) );
end put_retry;

procedure put_retry( ch : character ) is
  retryCnt : natural := 0;
begin
  loop
     begin
        put( ch );
        retryCnt := 2;
     exception when device_error =>
        retryCnt := retryCnt + 1;
     end;
     exit when retryCnt >= 2;
  end loop;
end put_retry;


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

--function optional_bold( s : string; no_bold : boolean ) return string is
--begin
--  if no_bold then
--     return s;
--  end if;
--  return to_string( term( bold ) ) & s & to_string( term( normal ) );
--end optional_bold;

-----------------------------------------------------------------------------
--  OPTIONAL RED
--
-- Return a string with terminal codes to draw the string
-- in red characters --colour is used, else inverse.
-- Return the string as-is if -g is used
-----------------------------------------------------------------------------

function optional_red( s : string; as_plain, in_colour : boolean ) return string is
begin
  if as_plain then
     return s;
  elsif not in_colour then
     return inverse( s );
  end if;
  return to_string( term( red ) ) & s & to_string( term( white ) );
end optional_red;

-----------------------------------------------------------------------------
--  ADORN RED
--
-- Return a string with terminal codes to draw the string
-- in red characters --colour is used, else normal.
-----------------------------------------------------------------------------

function adorn_red( s : string; in_colour : boolean ) return string is
begin
  if not in_colour then
     return s;
  end if;
  return to_string( term( red ) ) & s & to_string( term( white ) );
end adorn_red;

-----------------------------------------------------------------------------
--  RED
--
-- Return a string with terminal codes to draw the string
-- in red characters
-----------------------------------------------------------------------------

function red( s : string ) return string is
begin
  return to_string( term( red ) ) & s & to_string( term( white ) );
end red;

-----------------------------------------------------------------------------
--  OPTIONAL YELLOW
--
-- return a string with terminal codes to draw the string
-- in yellow characters if -g not used
-----------------------------------------------------------------------------

function optional_yellow( s : string; as_plain, in_colour : boolean  ) return string is
begin
  if as_plain then
     return s;
  elsif not in_colour then
     return bold( s );
  end if;
  return to_string( term( yellow ) ) & s & to_string( term( white ) );
end optional_yellow;

-----------------------------------------------------------------------------
--  YELLOW
--
-- return a string with terminal codes to draw the string
-- in yellow characters
-----------------------------------------------------------------------------

function yellow( s : string  ) return string is
begin
  return to_string( term( yellow ) ) & s & to_string( term( white ) );
end yellow;

-----------------------------------------------------------------------------
--  OPTIONAL GREEN
--
-- return a string with terminal codes to draw the string
-- in green characters if -g not used
-----------------------------------------------------------------------------

function optional_green( s : string; as_plain, in_colour : boolean ) return string is
begin
  if as_plain then
     return s;
  elsif not in_colour then
     return bold( s );
  end if;
  return to_string( term( green ) ) & s & to_string( term( white ) );
end optional_green;

-----------------------------------------------------------------------------
--  GREEN
--
-- return a string with terminal codes to draw the string
-- in green characters
-----------------------------------------------------------------------------

function green( s : string ) return string is
begin
  return to_string( term( green ) ) & s & to_string( term( white ) );
end green;

-----------------------------------------------------------------------------
--  ADORN GREEN
--
-- Return a string with terminal codes to draw the string
-- in green characters --colour is used, else normal.
-----------------------------------------------------------------------------

function adorn_green( s : string; in_colour : boolean ) return string is
begin
  if not in_colour then
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

function optional_inverse( s : string; as_plain : boolean ) return string is
begin
  if as_plain then
     return s;
  end if;
  return to_string( term( inverse ) ) & s & to_string( term( normal ) );
end optional_inverse;


-----------------------------------------------------------------------------
--  PUT SCRAMBLED
--
-- Display a animated scrambled message and started a new line
-- In the style of the "Inside Man" movie end-credits.
-----------------------------------------------------------------------------

procedure put_scrambled( msg : string ) is
  message : unbounded_string;
begin
  for rand in msg'range loop
      message := null_unbounded_string;
      for i in msg'range loop
          if i <= rand then
             message := message & msg(i);
          else
             message := message & character'val( 64 + rnd(32) );
          end if;
      end loop;
      delay 0.02;
      put_line( message );
      put( term( up ) );
  end loop;
  New_Line;
  delay 0.5;
end put_scrambled;


-----------------------------------------------------------------------------
--  PUT TRACE ERROR
--
-- Display an escaped message to standard error in the format used when
-- "trace true" is used.  This does not check the tracing flag.
-----------------------------------------------------------------------------

procedure put_trace_error( msg : string; as_plain, in_colour : boolean; icon : string := "" ) is
begin
  if icon /= "" and in_colour then
     put_line( standard_error, optional_red( to_string( "=> (" & icon & " " & toEscaped( to_unbounded_string( msg ) ) ) & ")", as_plain, in_colour ) );
  else
     put_line( standard_error, optional_red( to_string( "=> (" & toEscaped( to_unbounded_string( msg ) ) ) & ")", as_plain, in_colour ) );
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

end pegasoft.user_io;

