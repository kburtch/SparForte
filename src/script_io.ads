
------------------------------------------------------------------------------
-- Opening and Reading script files.                                        --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2017 Free Software Foundation              --
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

with system, ada.strings.unbounded, spar_os;
use ada.strings.unbounded, spar_os;

package script_io is


------------------------------------------------------
-- Script File Scanning
--
------------------------------------------------------

scriptFile : aFileDescriptor := 0; -- the file we're processing
scriptFilePath : unbounded_string; -- and its path
scriptLinestart : long_integer;    -- start of current line


------------------------------------------------------
-- Subprograms
--
------------------------------------------------------

function LineRead( lineptr : access unbounded_string ) return boolean;
-- read a line from the current script file.  return false on eof

end script_io;
