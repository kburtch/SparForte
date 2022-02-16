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

with interfaces.C.strings,
     ada.strings.unbounded;
use  interfaces.C,
     interfaces.C.strings,
     ada.strings.unbounded;

package pegasoft.user_io.getline is

--READLINE_EOF : exception;
COMPLETION_MEMORY_LEAK : exception;

procedure getLine( line : out unbounded_string;
                   prompt : unbounded_string := null_unbounded_string;
                   keepHistory : boolean := false );
-- read a line from the keyboard
-- This uses GNU readline or the older Ada-only version depending on
-- the configuration options.  For the older Ada-only version,
-- the keymap is hard-coded, not taken from current terminal settings.


function has_readline return boolean;
-- return true if using GNU readline / history for getLine

-- these are hard-coded: C preprocessor constants.  These mark
-- non-display characters in the prompot for GNU readline.
RL_PROMPT_START_IGNORE : constant character := ASCII.SOH; -- Ctrl-A
RL_PROMPT_END_IGNORE   : constant character := ASCII.STX; -- Ctrl-B


-----------------------------------------------------------------------------
--
-- HOUSEKEEPING
--
-----------------------------------------------------------------------------


procedure startupGetLine(optionOffset : natural);

procedure shutdownGetLine;

private

-- These are callbacks for c_rl.c, for GNU readline completion.

function Ada_ipset_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_ipset_word_generator, "Ada_ipset_word_generator" );

function Ada_git_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_git_word_generator, "Ada_git_word_generator" );

function Ada_svn_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_svn_word_generator, "Ada_svn_word_generator" );

function Ada_command_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_command_word_generator, "Ada_command_word_generator" );

function Ada_executable_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_executable_word_generator, "Ada_executable_word_generator" );

function Ada_assignment_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_assignment_word_generator, "Ada_assignment_word_generator" );

function Ada_variable_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_variable_word_generator, "Ada_variable_word_generator" );

function Ada_parameter_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_parameter_word_generator, "Ada_parameter_word_generator" );

function Ada_yum_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_yum_word_generator, "Ada_yum_word_generator" );

function Ada_apt_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_apt_word_generator, "Ada_apt_word_generator" );

function Ada_docker_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_docker_word_generator, "Ada_docker_word_generator" );

function Ada_k8s_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_k8s_word_generator, "Ada_k8s_word_generator" );

function Ada_npm_word_generator(text : chars_ptr; state : int ) return chars_ptr;
pragma export( C, Ada_npm_word_generator, "Ada_npm_word_generator" );

end pegasoft.user_io.getline;
