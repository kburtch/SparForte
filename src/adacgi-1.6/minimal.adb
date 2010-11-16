
with CGI, Text_IO; use CGI, Text_IO;

procedure Minimal is
-- Demonstrate CGI interface.

-- To run this program directly (without an HTTP server), set the
-- environment variable REQUEST_METHOD to "GET" and the variable
-- QUERY_STRING to either "" or "x=a&y=b".

begin
  Put_CGI_Header;   -- We will reply with a generated HTML document.
  Put_HTML_Head("Minimal Form Demonstration"); -- Start generating HTML.
  if CGI.Input_Received then  -- Check if input was received.
    Put_Variables;  -- Input received; show all variable values.
  else
    -- No input received; reply with a simple HTML form.
    Put_Line("<form method=""POST"">What's your Name?" &
             "<input name=""username""><input type=""submit""></form>");
  end if;
  Put_HTML_Tail;  -- End the HTML document, sending </body></html>
end Minimal;
