#!/usr/local/bin/spar

pragma annotate( summary, "Run good tests of cgi package" )
              @( description, "Runs a series of typical operations on the" )
              @( description, "cgi package to ensure these operations" )
              @( description, "are not broken." )
              @( created, "March 24, 2017" )
              @( author, "Ken O. Burtch" );

pragma annotate( todo, "some coverage is done by other regression tests so not " &
   "all functions are tested here" );

procedure cgi_good_test is
  key  : string;
  val  : string;
  abool : boolean;
  hdr  : string;
  url  : string;
  nat  : natural;
  html : string;
begin

  -- General CGI

  url := cgi.my_url;
  pragma assert( url = "http://example.com/example/cgi_test.html" );

  abool := cgi.parsing_errors;
  pragma assert( not abool );

  abool := cgi.input_received;
  pragma assert( abool );

  abool := cgi.is_index;
  pragma assert( not abool );

  -- FORM variables

  abool := cgi.key_exists( "first_key", 1 );
  pragma assert( abool );

  key := cgi.key( 1 );
  pragma assert( key = "first_key" );

  abool := cgi.key_value_exists( "first_key", "first value" );
  pragma assert( abool );

  val := cgi.key_value( 1 );
  pragma assert( val = "first value" );

  -- Output functions

  hdr := `cgi.put_cgi_header;`;
  pragma assert( strings.length( hdr ) = 24 );

  hdr := `cgi.put_cgi_header( "test" );`;
  pragma assert( strings.length( hdr ) = 5 );

  hdr := `cgi.put_html_head( "title", "foo@example.com" );`;
  pragma assert( strings.length( hdr ) = 94 );

  hdr := `cgi.put_html_heading( "example", 1 );`;
  pragma assert( strings.head( hdr, 4 ) = "<h1>" );

  hdr := `cgi.put_html_heading( "example", 6 );`;
  pragma assert( strings.head( hdr, 4 ) = "<h6>" );

  hdr := `cgi.put_html_tail;`;
  pragma assert( strings.length( hdr ) = 14 );

  hdr := `cgi.put_variables;`;
  pragma assert( strings.length( hdr ) > 0 );

  hdr := `cgi.put_error_message( "error text" );`;
  pragma assert( strings.head( hdr, 6 ) = "<html>" );

  -- CR/LF handling

  nat := cgi.line_count( "a" & ASCII.CR & ASCII.LF & "b" );
  pragma assert( nat = 2 );

  nat := cgi.line_count_of_value( "first_key" );
  pragma assert( nat = 1 );

  hdr := cgi.line( "a" & ASCII.CR & ASCII.LF & "b", 1 );
  pragma assert( hdr = "a" );

  -- TODO: not a thorough test
  hdr := cgi.value_of_line( "first_key", 1 );
  pragma assert( hdr = "first value" );

  -- Encoding/Decoding

  url := cgi.url_encode( "a test" );
  pragma assert( url = "a%20test" );

  url := cgi.url_decode( "a%20test" );
  pragma assert( url = "a test" );

  url := cgi.url_encode( "a test", true );
  pragma assert( url = "a+test" );

  url := cgi.url_decode( "a+test" );
  pragma assert( url = "a test" );

  url := cgi.url_encode( "a+test", false );
  pragma assert( url = "a%2Btest" );

  url := cgi.url_decode( "a+test", false );
  pragma assert( url = "a+test" );

  html := cgi.html_encode( "Encode < and &" );
  pragma assert( html = "Encode &lt; and &amp;" );

  -- There is no html_decode

  -- Cookies

  -- No incoming cookies

  nat := cgi.cookie_count;
  pragma assert( nat = 0 );

  hdr := cgi.cookie_value( "fake_cookie" );
  pragma assert( strings.length( hdr ) = 0 );

  hdr := cgi.cookie_value( "fake_cookie", 2 );
  pragma assert( strings.length( hdr ) = 0 );

  cgi.set_cookie( "fake_cookie", "fake_cookie_value" );
  cgi.set_cookie( "fake_cookie2", "fake_cookie_value2", "fake date", "fake path", "fake server", false );

end cgi_good_test;

-- VIM editor formatting instructions
-- vim: ft=spar

