procedure minimal_cgi is
  pragma annotate( summary, "minimal_cgi" );
  pragma annotate( description, "Demonstrate SparFortes CGI interface" );
  pragma annotate( description, "based on AdaCGI's minimal.adb example" );
  pragma annotate( description, "To run this script directly (without a HTTP server), set the" );
  pragma annotate( description, "environment variable REQUEST_METHOD to GET and the variable" );
  pragma annotate( description, "QUERY_STRING to either '' or 'x=a&y=b'." );
  pragma license( unrestricted );

  pragma ada_95;
  pragma restriction( no_external_commands );

begin
  -- cgi.put_cgi_header defaults to "content-type" but should do "Content-type"
  cgi.put_cgi_header( "Content-type: text/html" );
  cgi.put_html_head( "Minimal Form Demonstration" );
  if cgi.input_received then
     cgi.put_variables;
  else
     put_line( "<form method=" & ASCII.Quotation & "POST" & ASCII.Quotation &
       ">What's your name?<input name=" & ASCII.Quotation & "username" &
       ASCII.Quotation & "><input type=" & ASCII.Quotation & "submit" &
       ASCII.Quotation & "></form>" );
  end if;
  cgi.put_html_tail;
end minimal_cgi;

-- VIM editor formatting instructions
-- vim: ft=spar

