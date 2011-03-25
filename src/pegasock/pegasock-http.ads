-- Basic support for the Hypertext Transport Protocol (HTTP)
-- Ken O. Burtch, April 2010
-----------------------------------------------------------------------------

package pegasock.http is

-- This should probably be made to look more like the SMTP routines (that is,
-- have an establish/close.

type aHttpResult is private;

-- HTTP GET
--
-- Retrieve a document with a GET command.
-----------------------------------------------------------------------------

procedure httpGet( hr : out aHttpResult;
                  fd : in out aBufferedSocket;
                  item : unbounded_string := to_unbounded_string( "/index.html" );
                  acceptTypes : unbounded_string := to_unbounded_string( "text/html" ) );

-- HTTP HEAD
--
-- Retrieve the status of a document (but not the document itself) with a
-- HEAD command.
-----------------------------------------------------------------------------

procedure httpHead( hr : out aHttpResult;
                  fd : in out aBufferedSocket;
                  item : unbounded_string := to_unbounded_string( "/index.html" );
                  acceptTypes : unbounded_string := to_unbounded_string( "text/html" ) );

-- HTTP POST
--
-- Send a simple form (that is, send <INPUT> variabvles in POST format) with
-- a POST command.  The variables are set with other procedures before
-- calling this one.
-----------------------------------------------------------------------------

procedure httpPost( hr : out aHttpResult;
                    fd : in out aBufferedSocket;
                    item : unbounded_string := to_unbounded_string( "/index.html" ) );


-- CLEAR FORM VARIABLES
--
-- Erase all form variables added to this aHttpResult record.
-----------------------------------------------------------------------------

procedure clearFormVariables( hr : in out aHttpResult );

-- ADD FORM VARIABLE
--
-- Define a formm <INPUT> variable and its value in this aHttpResult record.
-- The data will be HTTP encoded.
-----------------------------------------------------------------------------

procedure addFormVariable( hr : in out aHttpResult; name : unbounded_string;
  val : unbounded_string );

-- GET RESPONSE CODE
--
-- Return the last HTTP response code from the web server.
-----------------------------------------------------------------------------

function getResponseCode( hr : aHttpResult ) return integer;

-- GET HEADER
--
-- Return the HTTP header of the last GET or HEAD.
-----------------------------------------------------------------------------

function getHeader( hr : aHttpResult ) return unbounded_string;

-- GET CONTENT
--
-- Return the content/document of the last GET.
-----------------------------------------------------------------------------

function getContent( hr : aHttpResult ) return unbounded_string;

-----------------------------------------------------------------------------
private
-----------------------------------------------------------------------------

type aHttpResult is record
     responseCode : integer;
     header       : unbounded_string;
     content      : unbounded_string;
     postVars     : unbounded_string;
end record;

end pegasock.http;

