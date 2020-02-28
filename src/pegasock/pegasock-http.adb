with Ada.Characters,
    Ada.Strings.Maps.Constants;
use  Ada.Strings.Maps,
     Ada.Strings.Maps.Constants;

--with ada.text_io;
--use  ada.text_io;

package body pegasock.http is

Unescaped_URL : constant Character_Set      := Alphanumeric_Set or
                                               To_Set("-_.!~*'()");

-- from David A. Wheeler's Ada CGI Library
function To_Hex_Char(Number : Natural ) return Character is
begin
  -- Assumes ASCII (or at least continuity in 0..9 and A..F).
  if Number < 10 then
     return Character'Val(Number + Character'Pos('0'));
  else
     return Character'Val(Number + Character'Pos('A') - 10);
  end if;
end To_Hex_Char;

-- from David A. Wheeler's Ada CGI Library
procedure URL_Encode(Data : in out Unbounded_String;
          Translate_Plus : Boolean := False) is
          I : Positive := 1;
          Current_Character : Character;
          Replacer : String := "%xx"; -- The String we replace with.
          Last_Position : Natural := Length(Data);
begin
  -- For URL/URI encoding, see IETF RFC 2396.
   while I <= Last_Position loop
     Current_Character := Element(Data, I);
     if Translate_Plus and Current_Character = '+' then
        Replace_Element(Data, I, ' ');
     elsif not Is_In(Current_Character, Unescaped_URL) then
        -- Character I isn't safe, replace it:
        Replacer(2) := To_Hex_Char(Character'Pos(Current_Character) / 16);
        Replacer(3) := To_Hex_Char(Character'Pos(Current_Character) mod 16);
        Replace_Slice(Data, I, I, Replacer);
        Last_Position := Last_Position + 2;
     end if;
     I := I + 1;
  end loop;
end URL_Encode;


--  HTTP GET
--
-- Issue a basic HTTP HEAD command to the web server connected on fd.  This may
-- close the socket if the web server closes the socket.
-----------------------------------------------------------------------------

procedure httpHead( hr : out aHttpResult;
                  fd : in out aBufferedSocket;
                  item : unbounded_string := to_unbounded_string( "/index.html" );
                  acceptTypes : unbounded_string := to_unbounded_string( "text/html" ) ) is
  p  : positive;
  s  : unbounded_string;
  mustClose : boolean := true;
begin
  hr.header := null_unbounded_string;
  hr.content := null_unbounded_string;
  setEOL( fd, CRLF );
  put_line( fd, "HEAD " & item & " HTTP/1.0" );
  put_line( fd, "Accept: " & acceptTypes );
  -- not officially supported
  -- put_line( fd, "Connection: Keep-Alive" );
  put_line( fd, "User-Agent: fileutils/1.0" );
  new_line( fd );
  loop
    get( fd, s );
    hr.header := hr.header & s & Ada.Characters.Latin_1.LF;
    -- should be container??
    exit when length( s ) = 0;
    if slice( s, 1, 5 ) = "HTTP/" then
       p := 6;
       while element( s, p ) /= ' ' loop
          p := p + 1;
       end loop;
       hr.responseCode := integer'value( slice( s, p, p+4 ) );
    elsif slice( s, 1, 17 ) = "Connection: close" then
       mustClose := true;
    end if;
  end loop;

  if mustClose then
     close ( fd );
  end if;
end httpHead;


--  HTTP GET
--
-- Issue a basic HTTP GET command to the web server connected on fd.  This may
-- close the socket if the web server closes the socket.
-----------------------------------------------------------------------------

procedure httpGet( hr : out aHttpResult;
                  fd : in out aBufferedSocket;
                  item : unbounded_string := to_unbounded_string( "/index.html" );
                  acceptTypes : unbounded_string := to_unbounded_string( "text/html" ) ) is
  p  : positive;
  s  : unbounded_string;
  contentLength : integer;
  mustClose : boolean := true;
begin
  hr.header := null_unbounded_string;
  hr.content := null_unbounded_string;
  setEOL( fd, CRLF );
  put_line( fd, "GET " & item & " HTTP/1.0" );
  put_line( fd, "Accept: " & acceptTypes );
  -- not officially supported
  -- put_line( fd, "Connection: Keep-Alive" );
  put_line( fd, "User-Agent: fileutils/1.0" );
  new_line( fd );
  contentLength := 0;
  loop
    get( fd, s );
    hr.header := hr.header & s & Ada.Characters.Latin_1.LF;
    -- should be container??
    exit when length( s ) = 0;
    if slice( s, 1, 15 ) = "Content-Length:" then
       declare
         temp : constant unbounded_string := tail( s, length( s ) - 15 );
       begin
         contentLength := integer'value( to_string( temp ) );
       end;
    elsif slice( s, 1, 5 ) = "HTTP/" then
       p := 6;
       while element( s, p ) /= ' ' loop
          p := p + 1;
       end loop;
       hr.responseCode := integer'value( slice( s, p, p+4 ) );
    elsif slice( s, 1, 17 ) = "Connection: close" then
       mustClose := true;
    end if;
  end loop;

  if contentLength > 0 then
    get( fd, contentLength, hr.content );
  end if;

  if mustClose then
     close ( fd );
  end if;
end httpGet;


--  HTTP POST
--
-- Issue a basic HTTP POST command, sending form variables to the web server
-- listening on fd.  This may close the socket if the web server closes the
-- socket.
-----------------------------------------------------------------------------

procedure httpPost( hr : out aHttpResult;
                    fd : in out aBufferedSocket;
                    item : unbounded_string := to_unbounded_string( "/index.html" ) ) is
  p  : positive;
  s  : unbounded_string;
  contentLength : integer;
  mustClose : boolean := true;
begin
  hr.header := null_unbounded_string;
  hr.content := null_unbounded_string;
  setEOL( fd, CRLF );
  put_line( fd, "POST " & item & " HTTP/1.0" );
  put_line( fd, "Content-Type: application/x-www-form-urlencoded" );
  put_line( fd, "Content-Length:" & integer'image( length( hr.content ) ) );
  -- not officially supported
  -- put_line( fd, "Connection: Keep-Alive" );
  put_line( fd, "User-Agent: fileutils/1.0" );
  new_line( fd );

  put_line( fd, to_string( hr.postVars ) );

  contentLength := 0;
  loop
    get( fd, s );
    hr.header := hr.header & s & Ada.Characters.Latin_1.LF;
    -- should be container??
    exit when length( s ) = 0;
    if slice( s, 1, 15 ) = "Content-Length:" then
       declare
         temp : constant unbounded_string := tail( s, length( s ) - 15 );
       begin
         contentLength := integer'value( to_string( temp ) );
       end;
    elsif slice( s, 1, 5 ) = "HTTP/" then
       p := 6;
       while element( s, p ) /= ' ' loop
          p := p + 1;
       end loop;
       hr.responseCode := integer'value( slice( s, p, p+4 ) );
    elsif slice( s, 1, 17 ) = "Connection: close" then
       mustClose := true;
    end if;
  end loop;

  if contentLength > 0 then
    get( fd, contentLength, hr.content );
  end if;

  if mustClose then
     close ( fd );
  end if;
end httpPost;


--  CLEAR FORM VARIABLES
--
--  Discard all form variables saved so far
-----------------------------------------------------------------------------

procedure clearFormVariables( hr : in out aHttpResult ) is
begin
  hr.postVars := null_unbounded_string;
end clearFormVariables;


--  ADD FORM VARIABLE
--
-- Add a form variable for an HTTP POST.
-----------------------------------------------------------------------------

procedure addFormVariable( hr : in out aHttpResult; name : unbounded_string;
	val : unbounded_string ) is
  temp : unbounded_string;
begin
  if length( hr.postVars ) > 0 then
    hr.postVars := hr.postVars & "&";
  end if;
  temp := name;
  url_encode( temp );
  url_encode( temp, Translate_Plus => true );
  hr.postVars := hr.postVars & temp;
  hr.postVars := hr.postVars & "=";
  temp := val;
  url_encode( temp, Translate_Plus => true );
  hr.postVars := hr.postVars & temp;
end addFormVariable;


--  GET RESPONSE CODE
--
-- Return the response code from the last HTTP operation.
-----------------------------------------------------------------------------

function getResponseCode( hr : aHttpResult ) return integer is
begin
  return hr.responseCode;
end getResponseCode;


--  GET HEADER
--
-- Return the header from the last HTTP operation.
-----------------------------------------------------------------------------

function getHeader( hr : aHttpResult ) return unbounded_string is
begin
  return hr.header;
end getHeader;


--  GET CONTENT
--
-- Return the content from the last HTTP operation.
-----------------------------------------------------------------------------
--
function getContent( hr : aHttpResult ) return unbounded_string is
begin
  return hr.content;
end getContent;

end pegasock.http;

