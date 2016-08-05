------------------------------------------------------------------------------
--                                                                          --
--                           AREADLINE COMPONENTS                           --
--                                                                          --
--                             R E A D L I N E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2006-2010 Samuel Tardieu <sam@rfc1149.net>        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--   The main repository for this software is located at:                   --
--       http://git.rfc1149.net/areadline.git                               --
--                                                                          --
------------------------------------------------------------------------------

pragma License (Modified_GPL);

package Readline is

   pragma Preelaborate;

   function Read_Line (Prompt : String := "") return String;
   --  May raise Ada.IO_Exceptions.End_Error if end-of-file is encountered

end Readline;
