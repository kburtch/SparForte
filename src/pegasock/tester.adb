-- Basic functionality tests.  These assume memached and a web server are
-- running on localhost on default ports.  a second memcached server should
-- be running on port 11212.  smtp is not tested but is loaded
-- to check for syntax errors.
--
-- To start memcached, something like memcached -vv to watch activity
--
-- Ken O. Burtch, April 2010
-----------------------------------------------------------------------------

with ada.text_io,
     ada.strings.unbounded,
     Ada.IO_Exceptions,
     pegasock.http,
     --pegasock.smtp,
     pegasock.memcache;
     --pegasock.tinyserve;

use  ada.text_io,
     ada.strings.unbounded,
     pegasock,
     pegasock.http,
     --pegasock.smtp,
     pegasock.memcache;
     --pegasock.memcache.highread,
     --pegasock.tinyserve;

procedure tester is
  myfile : aBufferedFile;
  mysock : aBufferedSocket;
  s : unbounded_string;
  contentLength : integer;
  c : character;
  flag : boolean;

  hr : aHttpResult;

  mc : aMemcacheCluster;

begin

  -- Basic file tests

  put_line( "open bad file test:" );
  flag := false;
  begin
    open( myfile, "fake.txt" );
  exception when others =>
    flag := true;
  end;
  pragma assert( flag, "exception not thrown" );
  pragma assert( not isOpen( myfile ) );

  put_line( "put string / overwrite test:" );
  overwrite( myfile, "test.txt" );
  pragma assert( isOpen( myfile ) );
  put( myfile, "test line" );
  close( myfile );

  put_line( "put character / append test:" );
  append( myfile, "test.txt" );
  pragma assert( isOpen( myfile ) );
  put( myfile, ASCII.LF );
  flush( myfile );
  close( myfile );

  put_line( "get string test:" );
  open( myfile, "test.txt" );
  pragma assert( isOpen( myfile ) );
  get( myfile, s );
  put_line( to_string( s ) );
  close( myfile );

  put_line( "EOF test:" );
  open( myfile, "test.txt" );
  pragma assert( isOpen( myfile ) );
  while not is_eof( myfile ) loop
    get( myfile, s );
    put_line( to_string( s ) );
  end loop;
  close( myfile );

  put_line( "read character test:" );
  open( myfile, "test.txt" );
  get( myfile, c );
  put_line( "read '" & c & "'" );
  pragma assert( c = 't', "get did not return first character of 't'" );
  close( myfile );

  put_line( "read bytes test:" );
  open( myfile, "test.txt" );
  get( myfile, 10, s );
  put_line( '"' & to_string( s ) & '"' );
  pragma assert( to_string(s) = "test line" & ASCII.LF, "get did not return expected first line" );
  close( myfile );

  put_line( "The following tests assume Apache is running on port 80 on local host" );
  put_line( "If Apache is not running, they will fail with an exception" );

  put_line( "open bad socket port test:" );
  flag := false;
  begin
    establish( mysock, to_unbounded_string( "localhost" ), 1234 );
  exception when others =>
    flag := true;
  end;
  pragma assert( flag, "exception not thrown" );
  pragma assert( not isOpen( mysock ) );

  put_line( "HTTP request test:" );
  establish( mysock, to_unbounded_string( "localhost" ), 80 );
  setEOL( mysock, CRLF );
  put_line( mysock, "GET /index.html HTTP/1.0" );
  put_line( mysock, "Accept: text/html" );
  new_line( mysock );
  contentLength := 0;

  put_line( "HTTP header: " );
  loop
    get( mysock, s );
    put_line( to_string( s ) );
    exit when length( s ) = 0;
    if slice( s, 1, 15 ) = "Content-Length:" then
       declare
         temp : constant unbounded_string := tail( s, length( s ) - 15 );
       begin
         contentLength := integer'value( to_string( temp ) );
       end;
    end if;
  end loop;

  if contentLength > 0 then
    put_line( "HTTP content: " );
    get( mysock, contentLength, s );
    put_line( to_string( s ) );
  end if;

  close ( mysock );

  put_line( "Non-blocking socket test:" );

  establish( mysock, to_unbounded_string( "localhost" ), 80, mode => nonblocking );
  flag := false;
  begin
    get( mysock, c );
  exception when fileutils_wouldblock =>
    put_line( "reading would block...good" );
    flag := true;
  end;
  pragma assert( flag, "didn't block on reading from apache connection" );
  close(mysock);

  put_line( "HttpGet test" );

  establish( mysock, to_unbounded_string( "localhost" ), 80 );
  httpGet( hr, mysock, to_unbounded_string( "/abcde.html" ) );
  if isOpen( mysock ) then
     close( mysock );
  end if;

  put_line( "Response Code is:" );
  put_line( integer'image( getResponseCode( hr ) ) );
  pragma assert( getResponseCode( hr ) = 404, "page was expected to be not there" );
  put_line( "Header is:" );
  put_line( to_string( getHeader( hr ) ) );
  put_line( "Content is:" );
  put_line( to_string( getContent( hr ) ) );

  put_line( "HttpHead test" );

  establish( mysock, to_unbounded_string( "localhost" ), 80 );
  httpHead( hr, mysock, to_unbounded_string( "/abcde.html" ) );
  if isOpen( mysock ) then
     close( mysock );
  end if;

  put_line( "HttpPost test" );

  establish( mysock, to_unbounded_string( "localhost" ), 80 );
  clearFormVariables( hr ); -- THING
  addFormVariable( hr, to_unbounded_string( "foo" ), to_unbounded_string( "bar" ) );
  httpPost( hr, mysock, to_unbounded_string( "/abcde.html" ) );
  if isOpen( mysock ) then
     close( mysock );
  end if;
  put_line( "Response Code is:" );
  put_line( integer'image( getResponseCode( hr ) ) );
  pragma assert( getResponseCode( hr ) = 404, "page was expected to be not there" );
  put_line( "Header is:" );
  put_line( to_string( getHeader( hr ) ) );
  put_line( "Content is:" );
  put_line( to_string( getContent( hr ) ) );


-- Memcache tests


  new_line;
  put_line( "Zero Server test (for error checking)" );
  new_line;

  flag := false;
  ClearServers( mc );
  begin
    Set( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bars" ) );
  exception when constraint_error =>
    flag := true;
  end;
  pragma assert( flag, "constraint error not raised on no servers" );

  flag := false;
  begin
    Get( mc, to_unbounded_string( "foo" ), s );
  exception when constraint_error =>
    flag := true;
  end;
  pragma assert( flag, "constraint error not raised on no servers" );

  new_line;
  put_line( "Single Server Memcache test" );
  new_line;

  ClearServers( mc );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11211 );

  Set( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bars" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bars" );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bars" );
  Add( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bar" ) );
  Replace( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bar" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bar" );
  Append( mc, to_unbounded_string( "foo" ), to_unbounded_string( "barz" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "barbarz" );
  Prepend( mc, to_unbounded_string( "foo" ), to_unbounded_string( "abar" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "abarbarbarz" );
  put_line( "get = " & to_string( s ) );
 
  Delete( mc, to_unbounded_string( "foo" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "" );

  Set( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bars" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bars" );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bars" );
  Add( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bar" ) );
  Replace( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bar" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bar" );
  Append( mc, to_unbounded_string( "foo" ), to_unbounded_string( "barz" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "barbarz" );
  Prepend( mc, to_unbounded_string( "foo" ), to_unbounded_string( "abar" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "abarbarbarz" );
  put_line( "get = " & to_string( s ) ); 
  Version( mc, s );
  put_line( "version = " & to_string( s ) );
  Stats( mc, s );
  put_line( "Stats = " & to_string( s ) );
  Flush( mc );

  new_line;
  put_line( "Multiple Memcache test (2 servers)" );
  new_line;

  flag := false;
  begin
    ClearServers( mc );
    RegisterServer( mc, to_unbounded_string( "localhost" ), 11211 );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11211 );
  exception when Ada.IO_Exceptions.name_error =>
    flag := true;
  end;
  pragma assert( flag, "duplicate servers didn't throw an exception" );

  ClearServers( mc );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11211 );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11212 );
  Set( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bar" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bar" );
  put_line( "foo = " & to_string( s ) );
 
  new_line;
  put_line( "Multiple Memcache test (2 servers, one failed)" );
  new_line;

  ClearServers( mc );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11211 );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11213 );
  Set( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bar" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bar" );
  put_line( "foo = " & to_string( s ) );

  new_line;
  put_line( "Multiple Memcache test (3 servers, one failed)" );
  new_line;

  ClearServers( mc );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11211 );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11212 );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11213 );
  -- We'll do two get's because get alternates between the primary and
  -- secondary servers.
  -- this hashes to server 2 and 3 (3 doesn't exist)
  Set( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bar" ) );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bar" );
  Get( mc, to_unbounded_string( "foo" ), s );
  pragma assert( to_string( s ) = "bar" );
  put_line( "foo = " & to_string( s ) );
  -- this hashes to server 1 and 3 (3 doesn't exist)
  Set( mc, to_unbounded_string( "foo2" ), to_unbounded_string( "bar2" ) );
  Get( mc, to_unbounded_string( "foo2" ), s );
  pragma assert( to_string( s ) = "bar2" );
  Get( mc, to_unbounded_string( "foo2" ), s );
  pragma assert( to_string( s ) = "bar2" );
  -- this hashes to server 3 and 1 (3 doesn't exist)
  Set( mc, to_unbounded_string( "foo4" ), to_unbounded_string( "bar4" ) );
  Get( mc, to_unbounded_string( "foo4" ), s );
  pragma assert( to_string( s ) = "bar4" );
  -- this hashes to server 1 and 2 (both exist)
  Set( mc, to_unbounded_string( "foo5" ), to_unbounded_string( "bar5" ) );
  Get( mc, to_unbounded_string( "foo5" ), s );
  pragma assert( to_string( s ) = "bar5" );
  Get( mc, to_unbounded_string( "foo5" ), s );
  pragma assert( to_string( s ) = "bar5" );
  new_line;
  put_line( "tests complete" );

end tester;

