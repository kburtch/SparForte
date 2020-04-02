------------------------------------------------------------------------------
-- AdaScript Language Scanner (Shell)                                       --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with ada.strings.unbounded;

use  ada.strings.unbounded;

package scanner_shell is


-----------------------------------------------------------------------------
--
--  Shell Words
--
-----------------------------------------------------------------------------


type aShellWord is new unbounded_string;

shellWord : aShellWord;


-----------------------------------------------------------------------------
--
--  Public Subprograms
--
-----------------------------------------------------------------------------


procedure getNextShellWord;
-- return the next shell word


-----------------------------------------------------------------------------
--
--  Housekeeping
--
-----------------------------------------------------------------------------


procedure startShellScanner;

procedure stopShellScanner;

end scanner_shell;
