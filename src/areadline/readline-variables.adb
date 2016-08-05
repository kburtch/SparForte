------------------------------------------------------------------------------
--                                                                          --
--                           AREADLINE COMPONENTS                           --
--                                                                          --
--                   R E A D L I N E . V A R I A B L E S                    --
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

with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Readline.Variables is

   -------------------
   -- Variable_Bind --
   -------------------

   procedure Variable_Bind (Name : String; Value : String) is

      procedure rl_variable_bind (variable : char_array;
                                  value    : char_array);
      pragma Import (C, rl_variable_bind, "rl_variable_bind");

   begin
      rl_variable_bind (To_C (Name), To_C (Value));
   end Variable_Bind;

   --------------------
   -- Variable_Value --
   --------------------

   function Variable_Value (Name : String) return String is

      function rl_variable_value (variable : char_array) return chars_ptr;
      pragma Import (C, rl_variable_value, "rl_variable_value");

   begin
      return Value (rl_variable_value (To_C (Name)));
   end Variable_Value;

end Readline.Variables;
