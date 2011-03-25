-- A Simple, non-threading, insecure Web Server built on Tinyserve.
-- Ken O. Burtch, May 2010
-----------------------------------------------------------------------------
with ada.text_io;
use ada.text_io;

package body pegasock.tinyserve.http is


-- HOUSEKEEPING


--  START UP WEB SERVER
--
-- Start up the server.  Use "0" for options to get defaults.
-----------------------------------------------------------------------------

procedure startupWebServer( server : out aHTTPServer;
   host : string;
   port : integer;
   min_recv_buffer_size : integer;
   min_send_buffer_size : integer;
   socket_queue_length : integer;
   socket_linger_seconds : integer;
   timeout_secs : integer;
   timeout_usecs : integer ) is
begin
  startupTinyServe( server.server,
    host,
    port,
    min_recv_buffer_size,
    min_send_buffer_size,
    socket_queue_length,
    socket_linger_seconds,
    timeout_secs,
    timeout_usecs );
end startupWebServer;


--  SHUTDOWN WEB SERVER
--
-- Shutdown the server and free memory
-----------------------------------------------------------------------------

procedure shutdownWebServer( server : aHTTPServer ) is
begin
  shutdownTinyServe( server.server );
end shutdownWebServer;


-----------------------------------------------------------------------------


--  GET WEB REQUEST
--
-- Check for outstanding requests and return them to the calling application
-----------------------------------------------------------------------------

procedure getWebRequest( server : in out aHTTPServer; client : out aHttpClientID ) is
  foundFreeClient : boolean := false;
  s : unbounded_string;
begin
  loop
    getNextClient( server.server, server.serverClient );
    if server.serverClient < 0 then
       manageConnections( server.server, server.serverClient );
    end if;
    if not isTimeout( server.serverClient ) then
       -- this loop is not necessary if the id's are fd's.
       for i in aHttpClientID'range loop
         if not isOpen( server.client(i).sd ) then
            client := i;
         end if;
         exit;
       end loop;
       -- assumes we don't run out of id's
       establish( server.client(client).sd, server.serverClient );
       setEOL( server.client(client).sd, CRLF );
       server.client(client).header := null_unbounded_string;
       loop
          get( server.client(client).sd, s );
          if length( s ) = 0 then
             if slice( server.client(client).header, 1, 4 ) = "HEAD" then
                server.client(client).requestType := head;
             elsif slice( server.client(client).header, 1, 4 ) = "GET " then
                server.client(client).requestType := get;
             elsif slice( server.client(client).header, 1, 4 ) = "POST" then
                server.client(client).requestType := post;
             else
                server.client(client).requestType := unknown;
             end if;
             --put_line( "returning = " & to_string( server.client(client).header ) );
             return;
          end if;
          server.client(client).header := server.client(client).header & s & ASCII.LF;
       end loop;
    end if;
  end loop;
end getWebRequest;


--  PUT WEB RESPONSE
--
-- Send a document back to the web client and close the connection.
-----------------------------------------------------------------------------

procedure putWebResponse( server : in out aHTTPServer; client : aHttpClientID; response : unbounded_string; responseType : aHttpResponseType := html ) is
begin
  put_line( server.client(client).sd, to_unbounded_string( "HTTP/1.0 200 OK" ) );
  put_line( server.client(client).sd, to_unbounded_string( "Server: TinyServe" ) );
  case responseType is
  when bmp =>
     put_line( server.client(client).sd, to_unbounded_string( "Content-type: image/bmp" ) );
  when css =>
     put_line( server.client(client).sd, to_unbounded_string( "Content-type: text/css" ) );
  when gif =>
     put_line( server.client(client).sd, to_unbounded_string( "Content-type: image/gif" ) );
  when html =>
     put_line( server.client(client).sd, to_unbounded_string( "Content-type: text/html" ) );
  when jpeg =>
     put_line( server.client(client).sd, to_unbounded_string( "Content-type: image/jpeg" ) );
  when png =>
     put_line( server.client(client).sd, to_unbounded_string( "Content-type: image/png" ) );
  when xml =>
     put_line( server.client(client).sd, to_unbounded_string( "Content-type: application/xml" ) );
  when others => 
     put_line( server.client(client).sd, to_unbounded_string( "Content-type: text/plain" ) );
  end case;
  put_line( server.client(client).sd, "Content-length:" & integer'image( length( response ) ) );
  new_line( server.client(client).sd );
  if server.client(client).requestType /= head then
     put( server.client(client).sd, response );
  end if;
  close( server.server, server.client(client).sd );
end putWebResponse;


--  PUT NOT FOUND
--
-- Return a document not found message to the web client and close the
-- connection.
-----------------------------------------------------------------------------

procedure putNotFound( server : in out aHTTPServer; client : aHttpClientID ) is
begin
  put_line( server.client(client).sd, to_unbounded_string( "HTTP/1.0 404 Not Found" ) );
  put_line( server.client(client).sd, to_unbounded_string( "Server: TinyServe" ) );
end putNotFound;


-- GET HEADER
--
-- Return the HTTP header of the client's request.
-----------------------------------------------------------------------------

function getHeader( server : aHTTPServer; client : aHttpClientID ) return unbounded_string is
begin
  return server.client(client).header;
end getHeader;


-- GET CONTENT
--
-- Return the content/document of the last GET.
-----------------------------------------------------------------------------

--function getContent( server : aHTTPServer; client : aHttpClientID ) return unbounded_string is
--begin
--  return server.client(client).content;
--end getContent;

end pegasock.tinyserve.http;
