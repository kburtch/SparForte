
-- Test_Encode is a short program to permit interactively trying
-- out the encodings.  Setup the CGI input data, then type.

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Text_IO;                use Text_IO;
with Ustrings;               use Ustrings;
with CGI;                    use CGI;

procedure Test_Encode is

   The_Line : Unbounded_String;

begin
   Put_Line("Type in test text and press return.");
   Put_Line("Enter a blank line when finished.");
   loop
      Get_Line (The_Line);
      exit when Length(The_Line) = 0;
      Put (Integer'Image(Length(The_Line)) & ": ");
      Put_Line (The_Line);
      Put("HTML Encoding:");
      Put_Line(HTML_Encode(The_Line));
      Put("URL Encoding:");
      Put_Line(URL_Encode(The_Line));
      Put("URL Decoding:");
      Put_Line(URL_Decode(The_Line));
   end loop;
end Test_Encode;


