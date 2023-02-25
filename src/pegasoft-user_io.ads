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

with ada.calendar,
     ada.strings.unbounded;
     --world;
use  ada.strings.unbounded;
     --world;


package pegasoft.user_io is

------------------------------------------------------
-- Prompt Info
--
-- The user's command line prompt info
------------------------------------------------------

defaultPrompt  : constant unbounded_string := to_unbounded_string("=> ");
-- default input prompt
promptScript   : unbounded_string := null_unbounded_string;
-- script created by pragma prompt_script
promptIdleScript   : unbounded_string := null_unbounded_string;
-- script created by pragma prompt_idle_script
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

function optional_red( s : string; as_plain, in_colour : boolean ) return string;
-- Return a string with terminal codes to draw the string
-- in red characters --colour is used, else inverse.
-- Return the string as-is if -g is used

function red( s : string ) return string;
-- Return a string with terminal codes to draw the string
-- in red characters

function adorn_red( s : string; in_colour : boolean ) return string;
-- Return a string with terminal codes to draw the string
-- in red characters --colour is used, else normal.

function optional_yellow( s : string; as_plain, in_colour : boolean ) return string;
-- return a string with terminal codes to draw the string
-- in yellow characters if -g not used

function yellow( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in yellow characters

function optional_green( s : string; as_plain, in_colour : boolean ) return string;
-- return a string with terminal codes to draw the string
-- in green characters if -g not used

function green( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in green characters

function adorn_green( s : string; in_colour : boolean ) return string;
-- Return a string with terminal codes to draw the string
-- in green characters --colour is used, else normal.

function bold( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in bold characters

function inverse( s : string ) return string;
-- return a string with terminal codes to draw the string
-- in inverse characters

procedure put_scrambled( msg : string );
-- Display an animated scrambled message and started a new line
-- In the style of the "Inside Man" movie end-credits.

procedure put_trace_error( msg : string; as_plain, in_colour : boolean; icon : string := "" );
-- display a trace error message to standard error


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

end pegasoft.user_io;
