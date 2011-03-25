-- telnet localhost 11215

with ada.text_io,
     ada.strings.unbounded,
     pegasock.tinyserve,
     pegasock.tinyserve.http;
use  ada.text_io,
     ada.strings.unbounded,
     pegasock,
     pegasock.tinyserve,
     pegasock.tinyserve.http;

procedure tinyservetester is
  done   : boolean := false;
  res    : aClientID;
  server : aSocketServer;
  sd     : aBufferedSocket;
  s      : unbounded_string;
begin
  put_line( "Creating Server on Port locahost:11215" );
  startupTinyServe( server,
     "localhost",
     11215,
     0, 0, 0, 0, 0, 0 );

  -- This only handles one connection at a time (for testing
  -- with telnet) as there is only one socket descriptor.
 
  while not done loop
    manageConnections( server, res );
    if not isTimeout( res ) then
       establish( sd, res );
       setEOL( sd, CRLF ); -- for telnet
       new_line( sd );
       put_line( sd, "TinyServe Test" );
       put_line( sd, "Type exit to disconnect or quit to stop server" );
       new_line( sd );
       loop
         get( sd, s );
         put_line( sd, "you typed '" & to_string( s ) & "'" );
         put_line( sd, "number of characters is" & integer'image( length( s ) ) );
         if to_string( s ) = "exit" then
             put_line( sd, "closing connection but server still runs..." );
             close( server, sd );
	     exit;
         elsif to_string( s ) = "quit" then
             put_line( sd, "stopping server..." );
             close( server, sd );
	     done := true;
             exit;
         end if;
      end loop;
    end if;
  end loop;

  shutdownTinyServe( server );

  put_line( "Creating HTTP Server on Port locahost:11215" );
  declare
    hs : aHTTPServer;
    id : aHTTPClientID;
  begin
    startupWebServer( hs,
       "localhost",
       11216,
       0, 0, 0, 0, 0, 0 );
    -- normally, this would loop
    getWebRequest( hs, id );
    putWebResponse( hs, id, to_unbounded_string( "<html><head><title>TinyServe HTML Test</title></head><body><p>The HTTP Request was:</p><pre>" & to_string( getHeader( hs, id ) & "</pre></body></html>" ) ) );
    --putNotFound( hs, id );
    shutdownWebServer( hs );
  end;

end tinyservetester;

