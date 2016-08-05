------------------------------------------------------------------------------
--                                                                          --
--                           AREADLINE COMPONENTS                           --
--                                                                          --
--                  R E A D L I N E . C O M P L E T I O N                   --
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

with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;       use Interfaces.C, Interfaces.C.Strings;
with Readline.Variables;         use Readline.Variables;

package body Readline.Completion is

   type rl_compentry_func_t is
      access function (Text : chars_ptr; State : int) return chars_ptr;
   pragma Convention (C, rl_compentry_func_t);

   rl_completion_entry_function : rl_compentry_func_t;
   pragma Import (C, rl_completion_entry_function,
      "rl_completion_entry_function");

   type String_Access is access String;
   package String_Arrays is
      new Ada.Containers.Vectors (Positive, String_Access);
   use String_Arrays;

   List        : Vector;
   List_Cursor : Cursor;

   Case_Sensitive : Boolean := True;

   function Completer (Text : chars_ptr; State : int)
         return chars_ptr;
   pragma Convention (C, Completer);

   --------------
   -- Add_Word --
   --------------

   procedure Add_Word (Word : String) is
   begin
      Append (List, new String'(Word));
   end Add_Word;

   ---------------------
   -- Clear_All_Words --
   ---------------------

   procedure Clear_All_Words is
      procedure Delete is
         new Ada.Unchecked_Deallocation (String, String_Access);
   begin
      for I in First_Index (List) .. Last_Index (List) loop
         declare
            Current : String_Access := Element (List, I);
         begin
            Delete (Current);
         end;
      end loop;
      Clear (List);
   end Clear_All_Words;

   ---------------
   -- Completer --
   ---------------

   function Completer (Text : chars_ptr; State : int)
         return chars_ptr is

      function Starts_With (Text : String; Start : String;
                            Case_Sensitive : Boolean) return Boolean;

      function To_Lower (C : Character) return Character renames
            Ada.Characters.Handling.To_Lower;

      function To_Lower (Str : String) return String;

      T : constant String := Value (Text);

      --  To_Lower - translates all alphabetic, uppercase characters
      --  in Str to lowercase
      function To_Lower (Str : String) return String is
         Result : String (Str'Range);
      begin
         for C in  Str'Range loop
            Result (C) := To_Lower (Str (C));
         end loop;
         return Result;
      end To_Lower;

      --  Starts_With - returns true if Text starts with Start
      function Starts_With (Text : String; Start : String;
                            Case_Sensitive : Boolean)
            return Boolean is
      begin
         if Text'Length < Start'Length then
            return False;
         end if;
         if Case_Sensitive then
            return Text (Text'First .. Text'First + Start'Length - 1) = Start;
         else
            declare
               LText : constant String := To_Lower (Text);
            begin
               return LText (LText'First .. LText'First + Start'Length - 1)
                  = To_Lower (Start);
            end;
         end if;
      end Starts_With;

   begin
      if State = 0 then
         List_Cursor := First (List);
         Case_Sensitive := Variable_Value ("completion-ignore-case") = "off";
      end if;

      while List_Cursor /= No_Element loop
         declare
            Current : String renames Element (List_Cursor).all;
         begin
            Next (List_Cursor);
            if Starts_With (Current, T, Case_Sensitive) then
               return New_String (Current);
            end if;
         end;
      end loop;

      return Null_Ptr;
   end Completer;

begin
   rl_completion_entry_function := Completer'Access;
end Readline.Completion;
