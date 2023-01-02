------------------------------------------------------------------------------
-- Spar OS.Readline - The GNU Readline library Command                      --
-- This version is for UNIX/Linux Commands                                  --
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

with interfaces.C.strings;
use  interfaces.C,
     interfaces.C.strings;

package spar_os.readline is

-----------------------------------------------------------------------------
--  C_init_readline
--
-- Setup the GNU readline library and the C helper functions for readline a
-- line of text.  Sets the Readline configuration section to "SparForte",
-- for example.
-- C defintion: void init_readline()
-----------------------------------------------------------------------------
procedure C_init_readline;
pragma import( C, C_init_readline, "C_init_readline" );

-----------------------------------------------------------------------------
--  C_readline
--
-- Setup the GNU readline library and the C helper functions.
-- C defintion: void C_readline( char *term, char **ada_buffer)
-----------------------------------------------------------------------------
procedure C_readline( term : char_array; prompt : char_array; buffer : in out chars_ptr; keep_history : int );
pragma import( C, C_readline, "C_readline" );

-----------------------------------------------------------------------------
--  C_free_readline
--
-- Finish the GNU readline library session, freeing any memory used.
-- C defintion: void C_free_readline(char *ada_buffer)
-----------------------------------------------------------------------------
procedure C_free_readline( buffer : chars_ptr );
pragma import( C, C_free_readline, "C_free_readline" );

-----------------------------------------------------------------------------
--  C_strdup
--
-- Wrapper around the strdup function.
-- C defintion: char *C_strdup( const char *s ) {
-----------------------------------------------------------------------------
function C_strdup( s : chars_ptr ) return chars_ptr;
pragma import( C,C_strdup, "C_strdup" );

-----------------------------------------------------------------------------
--  C ADD HISTORY
--
-- Add a string to GNU readline's history.  Used when reading SparForte's
-- history file.  String ust be NUL terminated.
-----------------------------------------------------------------------------
procedure add_history( s : string );
pragma import( C, add_history, "add_history" );

-----------------------------------------------------------------------------
--  RL SET KEYBOARD INPUT TIMEOUT
--
-----------------------------------------------------------------------------
function rl_set_keyboard_input_timeout( new_timeout : int ) return int;
pragma import( C, rl_set_keyboard_input_timeout, "rl_set_keyboard_input_timeout" );

end spar_os.readline;

