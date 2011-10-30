procedure json_health_check is

  pragma annotate( summary, "json_example.sp" );
  pragma annotate( description, "An example of a JSON API web service call to" );
  pragma annotate( description, "perform a health check: return the string 'OK'" );
  pragma annotate( author, "Ken O. Burtch" );

  pragma license( public_domain );

  pragma restriction( no_external_commands );

  -- A standard response
  --
  -- make it a record because it might contain more fields in the future

  type standard_response is record
     message : string;
  end record;

  -- HTTP headers for success and failure

  http_success_header : constant string :=
    "HTTP/1.0 200 OK" & ASCII.LF & ASCII.CR &
    "Content-Type: application/json" & ASCII.CR & ASCII.LF;

  http_fail_header : constant string :=
    "HTTP/1.0 404 Not Found" & ASCII.LF & ASCII.CR &
    "Content-Type: application/json" & ASCII.CR & ASCII.LF;

  -- this is our response

  response : standard_response;

  -- true if call is successful

  success : boolean := false;

begin
  -- Standard error normally goes to the web server logs
  put_line( standard_error, source_info.source_location & ": health check" );

  
  response.message := "OK";
  success := true;

  -- Package up the JSON response.  Calculate the length of the content.
  -- Send the response back to the caller.

  declare
     json_response : json_string;
     content_length : natural;
  begin
     records.to_json( json_response, response );
     content_length := strings.length( json_response );
     if success then
        cgi.put_cgi_header( http_success_header &
          "Content-Length:" & strings.image( content_length ) );
     else
        cgi.put_cgi_header( http_fail_header &
          "Content-Length:" & strings.image( content_length ) );
     end if;
     put( json_response );
  end;
end json_health_check;
-- vim: ft=spar
