------------------------------------------------------------------------------
--                                                                          --
--                           AREADLINE COMPONENTS                           --
--                                                                          --
--                             R E A D L I N E                              --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.IO_Exceptions;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;

package body Readline is

   ---------------
   -- Read_Line --
   ---------------

   function Read_Line (Prompt : String := "") return String is

      function Readline (Prompt : char_array) return chars_ptr;
      pragma Import (C, Readline, "readline");

      procedure Add_History (Line : chars_ptr);
      pragma Import (C, Add_History, "add_history");

      C_Line   : chars_ptr;

   begin
      C_Line := Readline (To_C (Prompt));

      if C_Line = Null_Ptr then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      declare
         Result : constant String := Value (C_Line);
      begin
         Add_History (C_Line);
         Free (C_Line);
         return Result;
      end;
   end Read_Line;

end Readline;
