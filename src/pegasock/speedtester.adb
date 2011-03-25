-- Basic functionality tests.  These assume memached and a web server are
-- running on localhost on default ports.  a second memcached server should
-- be running on port 11212.  smtp is not tested but is loaded
-- to check for syntax errors.
--
-- To start memcached, run without -vv
--
-- Ken O. Burtch, April 2010
-----------------------------------------------------------------------------

with ada.text_io,
     ada.strings.unbounded,
     Ada.IO_Exceptions,
     pegasock.memcache;

use  ada.text_io,
     ada.strings.unbounded,
     pegasock,
     pegasock.memcache;

procedure speedtester is
  iterations : integer := 100_000;
  s : unbounded_string;
  mc : aMemcacheCluster;
begin

-- Memcache tests

  new_line;
  put_line( "Single Server Memcache test" );
  put_line( "Iterations:" & iterations'img );
  new_line;

  ClearServers( mc );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11211 );

  declare
    key : unbounded_string := to_unbounded_string( "foo" );
  begin
    -- pick typical values (ie. 16 character key and 24 character value)
    Set( mc, to_unbounded_string( "key16_1234567890" ), to_unbounded_string( "value_24_123456789012345" ) );
    for i in 1..iterations loop
      Get( mc, key, s );
    end loop;
  end;

end speedtester;

