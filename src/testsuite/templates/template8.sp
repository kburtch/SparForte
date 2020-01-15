   pragma unrestricted_template( html );
   message : constant string := "OK";

declare
  b : boolean;
begin
  templates.set_http_status( 404 );
  templates.set_http_location( "redirect.html" );
  b := templates.has_put_template_header;
  pragma assert( b = false );
  b := false;
  begin
    templates.put_template_header;
    b := true;
  exception when others =>
    null;
  end;
  pragma assert( b = true );
end;

