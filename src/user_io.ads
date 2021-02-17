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

with ada.calendar,
     ada.strings.unbounded;
use  ada.strings.unbounded;


package user_io is

------------------------------------------------------
-- Prompt Info
--
-- The user's command line prompt info
------------------------------------------------------

defaultPrompt  : constant unbounded_string := to_unbounded_string("=> ");
-- default input prompt
promptScript   : unbounded_string := null_unbounded_string;
-- script created by pragma prompt_script
prompt         : unbounded_string := defaultPrompt;
-- the last prompt displayed (set by parser)


------------------------------------------------------
-- Command History
--
-- Command history is a circular queue
------------------------------------------------------

type historyRecord is record
     time : ada.calendar.time;
     pwd  : unbounded_string;
     line : unbounded_string;
end record;

type historyArray is array( 1..100 ) of historyRecord;

history     : historyArray;     -- the previous commands
historyNext : integer;          -- next to be overwritten

procedure clearHistory;
-- Clear the command line history


------------------------------------------------------
-- User Input
--
-- Special capabilities not in standard Ada packages
------------------------------------------------------

procedure getKey( ch : out character; nonblock : boolean := false );
-- read a single keypress and don't show the character to user
-- when non-blocking, ASCII.EOT is returned if no key

------------------------------------------------------
-- User Output
--
-- Special terminal control
------------------------------------------------------

function optional_red( s : string ) return string;
-- Return a string with terminal codes to draw the string
-- in red characters --colour is used, else inverse.
-- Return the string as-is if -g is used

function adorn_red( s : string ) return string;
-- Return a string with terminal codes to draw the string
-- in red characters --colour is used, else normal.

function optional_yellow( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in yellow characters if -g not used

function optional_green( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in green characters if -g not used

function adorn_green( s : string ) return string;
-- Return a string with terminal codes to draw the string
-- in green characters --colour is used, else normal.

function optional_bold( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in bold characters if -g not used

function bold( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in bold characters

function toProtectedValue( s : unbounded_string ) return string;
-- combines optional bold, secure data and escaped.  A null value will
-- return double single quotes.

function optional_inverse( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in inverse characters if -g not used

function inverse( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in inverse characters

procedure put_trace( msg : string; icon : string := "" );
-- display a trace message to standard error

procedure put_trace_error( msg : string; icon : string := "" );
-- display a trace error message to standard error

procedure displayVersionSplash;
-- show --version version message

procedure displayCopyrightSplash;
-- show bootup copyright message


------------------------------------------------------
-- Terminal Control
--
------------------------------------------------------

procedure checkDisplay( ttype : unbounded_string );
-- if the terminal display changed, update for the
-- changes

procedure terminalReset( ttype : unbounded_string );
-- reset the terminal.  ttype is the TERM variable value.

procedure terminalClear( ttype : unbounded_string );
-- clear the screen. ttype is the TERM variable value.


-----------------------------------------------------------------------------
--  UTF-8 icons
--
-----------------------------------------------------------------------------

function utf_ballot return string;
-- Return the bytes for a UTF-8 "x" ballot symbol.

function utf_checkmark return string;
-- Return the bytes for a UTF-8 checkmark symbol.

function utf_warning_sign return string;
-- Return the bytes for a UTF-8 warning sign symbol.

function utf_wristwatch return string;
-- Return the bytes for a UTF-8 watch symbol.

function utf_left return string;
-- Return the bytes for a UTF-8 left-end of underline

function utf_right return string;
-- Return the bytes for a UTF-8 right-end of underline

function utf_triangle return string;
-- Return the bytes for a UTF-8 triangle

function utf_horizontalLine return string;
-- Return the bytes for a UTF-8 horizontal line

function utf_bullet return string;
-- Return the bytes for a UTF-8 bullet, otherwise asterisk

function utf_diamond return string;
-- Return the bytes for a UTF-8 diamond, otherwise a minus sign

end user_io;
