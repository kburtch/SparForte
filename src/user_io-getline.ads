
------------------------------------------------------------------------------
-- Reading the keyboard, writing to the terminal/console                    --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2016 Free Software Foundation              --
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

with system, ada.calendar, ada.strings.unbounded, bush_os;
use ada.strings.unbounded, bush_os;

package user_io.getline is

procedure getLine( line : out unbounded_string; prompt : unbounded_string := null_unbounded_string; keepHistory : boolean := false );
-- read a line from the keyboard
-- This uses GNU readline or the older Ada-only version depending on
-- the configuration options.  For the older Ada-only version,
-- the keymap is hard-coded, not taken from current terminal settings.

function has_readline return boolean;
-- return true if using GNU readline / history for getLine

end user_io.getline;
