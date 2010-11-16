-- Test sending a cookie.

with Text_IO; use Text_IO;
with CGI; use CGI;

procedure Test_Send is
begin
   Set_Cookie("test_key", "test_value");
   Put_CGI_Header;
   Put_HTML_Head(Title=>"test send cookie");
   Put_Line("<p>This is just a test");
   Put_HTML_Tail;
end Test_Send;

