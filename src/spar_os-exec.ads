------------------------------------------------------------------------------
-- Spar OS.Exec - Fork off an Operating System Command                      --
-- This version is for UNIX/Linux Commands                                  --
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

with system,
     unchecked_deallocation,
     ada.strings.unbounded;
use  ada.strings.unbounded;

package spar_os.exec is


-- Arguments for the Program to Run

type argumentPtr is access all String;
type argumentList is array( positive range <> ) of argumentPtr;
type argumentListPtr is access all argumentList;

procedure Free is new Unchecked_Deallocation( Object => String,
   Name => argumentPtr );

procedure free( ap : in out argumentListPtr );
-- free an entire argument list


-- The Spawn Procedure

procedure spawn( fullPath : unbounded_string;
                 ap       : argumentListPtr;
                 status   : out integer;
                 noReturn : boolean := false );

-- Spawn runs another program.  The program at the full pathname fullPath
-- is executed with the arguments in ap returning to the original program
-- if noReturn is false.  Any error is returned in the errno variable.
--
-- Spawn is a utility function for the BUSH run functions. It not (normally)
-- called directly since it provides no job control, etc.
--
-- GNAT.OS_Lib Spawn is too limited for what we're trying to do.  Under
-- UNIX/Linux, running a program requires several operating system
-- functions.  This is a convenience function bundling these functions
-- together under one command.  This function also takes care of translating
-- the parameters into C null-termianted strings.
--
-- If noReturn is false, run the other program without returning to
-- BUSH.  If noReturn is true, return control to BUSH when the program
-- is finished.  (That is, under UNIX/Linux, a new process is fork()'ed
-- and the program wait()'s for the command to finish.)

end spar_os.exec;

