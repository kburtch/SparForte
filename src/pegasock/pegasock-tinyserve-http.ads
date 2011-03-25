-- A Simple, non-threading, insecure Web Server built on Tinyserve.
-- Ken O. Burtch, May 2010
-----------------------------------------------------------------------------
pragma ada_2005;

package pegasock.tinyserve.http is

type aHttpClientID is new integer range 1..integer(getFDSetSize);

type aHttpRequestType is ( unknown, get, head, post );

type aHttpResponseType is ( bmp, css, gif, html, jpeg, png, text, xml );

type aHttpServer is private;


-- HOUSEKEEPING


procedure startupWebServer( server : out aHTTPServer;
   host : string;
   port : integer;
   min_recv_buffer_size : integer;
   min_send_buffer_size : integer;
   socket_queue_length : integer;
   socket_linger_seconds : integer;
   timeout_secs : integer;
   timeout_usecs : integer );
-- Start up the server.  Use "0" for options to get defaults.

procedure shutdownWebServer( server : aHTTPServer );
-- Shutdown the server and free memory


-- I/O


procedure getWebRequest( server : in out aHTTPServer; client : out aHttpClientID );

procedure putWebResponse( server : in out aHTTPServer; client : aHttpClientID; response : unbounded_string; responseType : aHttpResponseType := html );

procedure putNotFound( server : in out aHTTPServer; client : aHttpClientID );

-- GET HEADER
--
-- Return the HTTP header of the last GET or HEAD.  Lines are delineated
-- by line feeds.
-----------------------------------------------------------------------------

function getHeader( server : aHTTPServer; client : aHttpClientID ) return unbounded_string;


-- GET CONTENT
--
-- Return the content/document of the last GET.
-----------------------------------------------------------------------------

--function getContent( server : aHTTPServer; client : aHttpClientID ) return unbounded_string;

-----------------------------------------------------------------------------
private
-----------------------------------------------------------------------------
--
type aHttpRequest is record
     sd          : aBufferedSocket; 
     header      : unbounded_string;
--     content     : unbounded_string;
     requestType : aHttpRequestType;
end record;

type aClientArray is array(aHttpClientID'range) of aHttpRequest;

type aHttpServer is record
    server : aSocketServer;
    client : aClientArray;
    serverClient : aClientID := -1;
end record;

end pegasock.tinyserve.http;

