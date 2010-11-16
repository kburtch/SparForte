
-- Test_Get is a short program that tests package Ustrings.

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Text_IO;                use Text_IO;
with Ustrings;               use Ustrings;

procedure Test_Get is

   The_Line : Unbounded_String;

begin
   Put_Line("Type in test text and press return.");
   Put_Line("Enter a blank line when finished.");
   loop
      Get_Line (The_Line);
      exit when Length(The_Line) = 0;
      Put (Integer'Image(Length(The_Line)) & ": ");
      Put_Line (The_Line);
   end loop;
end Test_Get;


