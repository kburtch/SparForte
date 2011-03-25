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
     fileutils.memcache;

use  ada.text_io,
     ada.strings.unbounded,
     fileutils,
     fileutils.memcache;

procedure backofftester is
  iterations : integer := 100_000;
  s : unbounded_string;
  mc : aMemcacheCluster;
begin

-- Memcache tests

  new_line;
  put_line( "Single Server Memcache Backoff test" );
  put_line( "Iterations:" & iterations'img );
  new_line;

  ClearServers( mc );
  RegisterServer( mc, to_unbounded_string( "localhost" ), 11213 );

  declare
    key : unbounded_string := to_unbounded_string( "foo" );
  begin
    Set( mc, to_unbounded_string( "foo" ), to_unbounded_string( "bars" ) );
    for i in 1..iterations loop
      Get( mc, key, s );
    end loop;
  end;

end backofftester;

