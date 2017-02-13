-- Basic support for the Memcached (distributed in-memory cache)
-- Ken O. Burtch, April 2010
-----------------------------------------------------------------------------
pragma ada_2005;

with Gnat.Source_Info,
     Ada.Text_IO,
     Ada.Characters.Handling,
     Ada.Strings.Unbounded,
     Ada.Calendar;
use  Ada.Text_IO,
     Ada.Characters,
     Ada.Characters.Handling,
     Ada.Strings.Unbounded,
     Ada.Calendar;

package body pegasock.memcache is

type hash_integer is mod 2**32;

-- Basic Memache result messages

memcacheResult_OK           : constant unbounded_string := to_unbounded_string( "OK" );
memcacheResult_END          : constant unbounded_string := to_unbounded_string( "END" );
memcacheResult_ERROR        : constant unbounded_string := to_unbounded_string( "ERROR" );
memcacheResult_CLIENT_ERROR : constant unbounded_string := to_unbounded_string( "CLIENT_ERROR" );
memcacheResult_SERVER_ERROR : constant unbounded_string := to_unbounded_string( "SERVER_ERROR" );
memcacheResult_STORED       : constant unbounded_string := to_unbounded_string( "STORED" );
memcacheResult_NOT_STORED   : constant unbounded_string := to_unbounded_string( "NOT_STORED" );
memcacheResult_EXISTS       : constant unbounded_string := to_unbounded_string( "EXISTS" );
memcacheResult_NOT_FOUND    : constant unbounded_string := to_unbounded_string( "NOT_FOUND" );
memcacheResult_VALUE        : constant unbounded_string := to_unbounded_string( "VALUE" );
memcacheResult_DELETED      : constant unbounded_string := to_unbounded_string( "DELETED" );
memcacheResult_STAT         : constant unbounded_string := to_unbounded_string( "STAT" );
memcacheResult_VERSION      : constant unbounded_string := to_unbounded_string( "VERSION" );


--  FNV HASH OF
--
-- FNV-1a hash, same string hash used in DNS servers.  Murmur would have been
-- overkill.  This is still probabably overkill.
-----------------------------------------------------------------------------

function fnv_hash_of( s : string; limit : hash_integer ) return long_integer is
  pragma suppress( range_check );
  pragma suppress( length_check );
  pragma suppress( index_check );
  pragma suppress( division_check );
  hash   : hash_integer := 16#811c9dc5#;
  k      : hash_integer;
begin
  for data in 1..s'length-3 loop
      k := character'pos( s(data) ) +
           character'pos( s(data+1) ) * 256 +     -- 8
           character'pos( s(data+2) ) * 65536 +   -- 16
           character'pos( s(data+3) ) * 16777216; -- 24
           hash := hash xor k;
           hash := hash * 16#01000193#;
 end loop;

 hash := (hash mod limit) + 1;
 return long_integer( hash );
end fnv_hash_of;


--  BACKOFF SERVER
--
-- Increase the backoff level and start/resume backing off.
-----------------------------------------------------------------------------

procedure backoffServer( mcdp : access aMemcacheDescriptor ) is
begin
  if isOpen( mcdp.fd ) then
     close( mcdp.fd );
  end if;
  if mcdp.backoffLevel < backoffMax then
     mcdp.backoffLevel := mcdp.backoffLevel + 1;
  end if;
  mcdp.backoffCount := backoffFactor**mcdp.backoffLevel;
  -- error message could be pre
  put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": server " & to_string( mcdp.host) & mcdp.port'img & " error - backing off for" &
    mcdp.backoffCount'img & " tries" );
end backoffServer;

procedure backoffServer( mc : in out aMemcacheCluster; s : aServerNumber ) is
begin
  backoffServer( mc.server( s )'unchecked_access );
end backoffServer;


--  HANDLE BACKOFF
--
-- When a communication error occurs, begin exponentially backing off
-- accessing a server.  When the backoff counts down, try to access the
-- server again.  This is to avoid massive connection attempts to down
-- server resulting in a cascading failure.
-----------------------------------------------------------------------------

procedure handleBackoff( mcdp : access aMemcacheDescriptor ) is
begin
  mcdp.backoffCount := mcdp.backoffCount - 1;
  if mcdp.backoffCount = 0 then
     begin
       if mcdp.backoffLevel > 0 then
          put_line( standard_error, Gnat.Source_Info.Source_Location &
            ": server" & to_string( mcdp.host) & mcdp.port'img & " backoff complete - attempting to reestablish connection" );
       else
          -- a first attempt
          pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
            ": server" & to_string( mcdp.host) & mcdp.port'img & " trying to establish connection" ) );
          null;
       end if;
       establish( mcdp.fd, mcdp.host, mcdp.port );
       setEOL( mcdp.fd, CRLF );
       mcdp.backoffLevel := 0;
     exception when others =>
-- catch the exception message and write it out to standard error
       backoffServer( mcdp );
     end;
  end if;
end handleBackoff;

procedure handleBackoff( mc : in out aMemcacheCluster; s : aServerNumber ) is
begin
  handleBackoff( mc.server( s )'unchecked_access );
end handleBackoff;

--  RECONCILE RESULTS
--
-- Given memcache result message from a primary and secondary server, compare
-- the results and return a single result that best describes the operation.
-----------------------------------------------------------------------------

procedure reconcileResults( result : out unbounded_string; primaryResult, secondaryResult : unbounded_string ) is
begin
  if length( primaryResult ) = 0 then
     result := secondaryResult;
  elsif length( secondaryResult ) = 0 then
     result := primaryResult;
  elsif primaryResult = memcacheResult_STORED then
     result := memcacheResult_STORED;
  elsif secondaryResult = memcacheResult_STORED then
     result := memcacheResult_STORED;
  elsif primaryResult = memcacheResult_DELETED then
     result := memcacheResult_DELETED;
  elsif secondaryResult = memcacheResult_DELETED then
     result := memcacheResult_DELETED;
  elsif primaryResult = secondaryResult then
     result := primaryResult;
  else
     result := primaryResult;
  end if;
end reconcileResults;


--  GET SERVERS
--
-- Returns primary and secondary servers in cluster for given key name.
-----------------------------------------------------------------------------

procedure getServers( mc : aMemcacheCluster; name : unbounded_string; primaryServer, secondaryServer : out aServerNumber ) is
  pragma suppress( range_check );
  pragma suppress( length_check );
  pragma suppress( index_check );
  pragma suppress( division_check );
begin
  if mc.serverCount = 0 then
     -- this should force a constraint error
     primaryServer := 0;
     secondaryServer := 0;
     return;
  end if;
  primaryServer := aServerNumber(
    fnv_hash_of( to_string(name), hash_integer(mc.serverCount) ) );
  -- KISS: Keep it simple, stupid.
  secondaryServer := primaryServer + 1;
  if secondaryServer > mc.serverCount then
     secondaryServer := 1;
  end if;
end getServers;


--  GET RANDOM SERVERS
--
-- Returns two random serers from the memcache cluster.
-----------------------------------------------------------------------------

procedure getRandomServers( mc : aMemcacheCluster; primaryServer, secondaryServer : out aServerNumber ) is
  pragma suppress( range_check );
  pragma suppress( length_check );
  pragma suppress( index_check );
  pragma suppress( division_check );
begin
  primaryServer := aServerNumber(
    fnv_hash_of( seconds( clock )'img, hash_integer(mc.serverCount) ) );
  secondaryServer := primaryServer + 1;
  if secondaryServer > mc.serverCount then
     secondaryServer := 1;
  end if;
end getRandomServers;


--  IS VALID MEMCACHE KEY
--
-- Returns true if the memcache key is valid.
-----------------------------------------------------------------------------

function isValidMemcacheKey( key : unbounded_string ) return boolean is
  ch : character;
begin
  if length( key ) > 250 then
     return false;
  elsif length( key ) = 0 then
     return false;
  else
     for i in 1..length( key ) loop
         ch := element( key, i );
         if is_control( ch ) then
            return false;
         elsif ch = ' ' then
            return false;
         end if;
     end loop;
  end if;
  return true;
end isValidMemcacheKey;


--  REGISTER SERVER
--
-- Add a new server to a memcache cluster.  Does not open a connection to the
-- server.  Name_Error is raised if the server is duplicated in list or if
-- there are too many servers.
-----------------------------------------------------------------------------

procedure RegisterServer( mc : in out aMemcacheCluster; host : unbounded_string; port : integer ) is
  duplicateServer : boolean := false;
begin
  if mc.serverCount = aServerNumber'last then
     raise NAME_ERROR with  "Error: Too many servers - limit is" & aServerNumber'last'img;
  end if;
  for i in 1..mc.serverCount loop
      if mc.server( i ).host = host and then mc.server( i ).port = port then
         duplicateServer := true;
         exit;
      end if;
  end loop;
  if duplicateServer then
     raise NAME_ERROR with "Error: Server exists - " & to_string( host ) & port'img;
  end if;
  mc.serverCount := mc.serverCount + 1;
  mc.server( mc.serverCount ).host := host;
  mc.server( mc.serverCount ).port := port;
  mc.server( mc.serverCount ).backoffCount := 1;
  mc.server( mc.serverCount ).backoffLevel := 0;
  -- setEOL( mc.server( mc.serverCount ).fd, CRLF );
  -- cannot be done here because fd will be overwritten in establish
end RegisterServer;


--  CLEAR SERVERS
--
-- Close all open file descriptors and discard all registered servers.
-----------------------------------------------------------------------------

procedure ClearServers( mc : in out aMemcacheCluster ) is
begin
  for i in 1..mc.serverCount loop
    if is_open( mc.server(i).fd ) then
       close( mc.server(i).fd );
    end if;
  end loop;
  mc.serverCount := 0;
end ClearServers;


--  SET CLUSTER TYPE
--
--
-----------------------------------------------------------------------------

procedure SetClusterName( mc : in out aMemcacheCluster; name : unbounded_string ) is
begin
  mc.name := name;
end SetClusterName;


--  SET CLUSTER TYPE
--
--
-----------------------------------------------------------------------------

procedure SetClusterType( mc : in out aMemcacheCluster; mct : aMemcacheClusterType ) is
begin
  mc.clusterType := mct;
end SetClusterType;


--  SET
--
-- Store a value for the named key, ovewriting if necessary.
-- constraint_error is raised if no servers are defined.
-----------------------------------------------------------------------------

procedure Set( mc : in out aMemcacheCluster; name, value : unbounded_string ) is
  memcache_cmd    : unbounded_string;
  primaryServer   : aServerNumber;
  secondaryServer : aServerNumber;
  result          : unbounded_string;
  primaryResult   : unbounded_string;
  secondaryResult : unbounded_string;
  primaryPtr      : aMemcacheDescriptorPtr;
  secondaryPtr    : aMemcacheDescriptorPtr;
  pragma suppress( access_check ); -- disable when debugging!
begin

  getServers( mc, name, primaryServer, secondaryServer );

  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": set '" & to_string( name ) & "' on primary server" &
       primaryServer'img & " and secondary" & secondaryServer'img ) );

  memcache_cmd := "set " & name & " 0 0" & integer'image( length( value ) );
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  -- this pointer is used to improve performance. not sure why unchecked
  -- is necessary for a local pointer to a global identifier
  primaryPtr := mc.server( primaryServer )'unchecked_access;

  if primaryPtr.backoffCount > 0 then
     handleBackoff( primaryPtr );
  end if;
  if primaryPtr.backoffCount = 0 then
     begin
       put_line( primaryPtr.fd, memcache_cmd );
       put_line( primaryPtr.fd, value );
       get( primaryPtr.fd, primaryResult );
     exception when others =>
       backoffServer( primaryPtr );
     end;
  end if;

  if primaryServer = secondaryServer then
     result := primaryResult;
  else
     secondaryPtr := mc.server( secondaryServer )'unchecked_access;
     if secondaryPtr.backoffCount > 0 then
        handleBackoff( mc, secondaryServer );
     end if;
     if secondaryPtr.backoffCount = 0 then
        begin
          put_line( secondaryPtr.fd, memcache_cmd );
          put_line( secondaryPtr.fd, value );
          get( secondaryPtr.fd, secondaryResult );
       exception when others =>
          backoffServer( secondaryPtr );
       end;
     end if;
     reconcileResults( result, primaryResult, secondaryResult );
  end if;

  if head( result, length( memcacheResult_STORED ) ) /= memcacheResult_STORED then
     put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": set failed on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" );
  else
     null;
     pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": set succeeded on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" ) );

  end if;
end Set;


--  ADD
--
-- Store a value for the named key, only if it doesn't exist.
-- constraint_error is raised if no servers are defined.
-----------------------------------------------------------------------------

procedure Add( mc : in out aMemcacheCluster; name, value : unbounded_string ) is
  memcache_cmd    : unbounded_string;
  primaryServer   : aServerNumber;
  secondaryServer : aServerNumber;
  result          : unbounded_string;
  primaryResult   : unbounded_string;
  secondaryResult : unbounded_string;
  primaryPtr      : aMemcacheDescriptorPtr;
  secondaryPtr    : aMemcacheDescriptorPtr;
  pragma suppress( access_check ); -- disable when debugging!
begin
  getServers( mc, name, primaryServer, secondaryServer );

  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": add '" & to_string( name ) & "' on primary server" &
       primaryServer'img & " and secondary" & secondaryServer'img ) );

  memcache_cmd := "add " & name & " 0 0" & integer'image( length( value ) );
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  -- this pointer is used to improve performance. not sure why unchecked
  -- is necessary for a local pointer to a global identifier
  primaryPtr := mc.server( primaryServer )'unchecked_access;

  if primaryPtr.backoffCount > 0 then
     handleBackoff( primaryPtr );
  end if;
  if primaryPtr.backoffCount = 0 then
     begin
       put_line( primaryPtr.fd, memcache_cmd );
       put_line( primaryPtr.fd, value );
       get( primaryPtr.fd, primaryResult );
     exception when others =>
       backoffServer( primaryPtr );
     end;
  end if;

  if primaryServer = secondaryServer then
     result := primaryResult;
  else
     secondaryPtr := mc.server( secondaryServer )'unchecked_access;
     if secondaryPtr.backoffCount > 0 then
        handleBackoff( secondaryPtr );
     end if;
     if secondaryPtr.backoffCount = 0 then
        begin
          put_line( secondaryPtr.fd, memcache_cmd );
          put_line( secondaryPtr.fd, value );
          get( secondaryPtr.fd, secondaryResult );
       exception when others =>
          backoffServer( secondaryPtr );
       end;
     end if;
     reconcileResults( result, primaryResult, secondaryResult );
  end if;

  if head( result, length( memcacheResult_STORED ) ) /= memcacheResult_STORED then
     put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": add failed on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" );
  else
     null;
     pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": add succeeded on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" ) );
  end if;
end Add;


--  REPLACE
--
-- Store a value for the named key, only if it already exists.
-- constraint_error is raised if no servers are defined.
-----------------------------------------------------------------------------

procedure Replace( mc : in out aMemcacheCluster; name, value : unbounded_string ) is
  memcache_cmd    : unbounded_string;
  primaryServer   : aServerNumber;
  secondaryServer : aServerNumber;
  result          : unbounded_string;
  primaryResult   : unbounded_string;
  secondaryResult : unbounded_string;
  primaryPtr      : aMemcacheDescriptorPtr;
  secondaryPtr    : aMemcacheDescriptorPtr;
  pragma suppress( access_check ); -- disable when debugging!
begin
  getServers( mc, name, primaryServer, secondaryServer );

  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": replace '" & to_string( name ) & "' on primary server" &
       primaryServer'img & " and secondary" & secondaryServer'img ) );

  memcache_cmd := "replace " & name & " 0 0" & integer'image( length( value ) );
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  -- this pointer is used to improve performance. not sure why unchecked
  -- is necessary for a local pointer to a global identifier
  primaryPtr := mc.server( primaryServer )'unchecked_access;

  if primaryPtr.backoffCount > 0 then
     handleBackoff( primaryPtr );
  end if;
  if primaryPtr.backoffCount = 0 then
     begin
       put_line( primaryPtr.fd, memcache_cmd );
       put_line( primaryPtr.fd, value );
       get( primaryPtr.fd, primaryResult );
     exception when others =>
       backoffServer( primaryPtr );
     end;
  end if;

  if primaryServer = secondaryServer then
     result := primaryResult;
  else
     secondaryPtr := mc.server( secondaryServer )'unchecked_access;
     if secondaryPtr.backoffCount > 0 then
        handleBackoff( secondaryPtr );
     end if;
     if secondaryPtr.backoffCount = 0 then
        begin
          put_line( secondaryPtr.fd, memcache_cmd );
          put_line( secondaryPtr.fd, value );
          get( secondaryPtr.fd, secondaryResult );
       exception when others =>
          backoffServer( secondaryPtr );
       end;
     end if;
     reconcileResults( result, primaryResult, secondaryResult );
  end if;

  if head( result, length( memcacheResult_STORED ) ) /= memcacheResult_STORED then
     put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": replace failed on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" );
  else
     null;
     pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": replace succeeded on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" ) );
  end if;
end Replace;


--  APPEND
--
-- Add string to the end of the value for the named key.
-- constraint_error is raised if no servers are defined.
-----------------------------------------------------------------------------

procedure Append( mc : in out aMemcacheCluster; name, value : unbounded_string ) is
  memcache_cmd    : unbounded_string;
  primaryServer   : aServerNumber;
  secondaryServer : aServerNumber;
  result          : unbounded_string;
  primaryResult   : unbounded_string;
  secondaryResult : unbounded_string;
  primaryPtr      : aMemcacheDescriptorPtr;
  secondaryPtr    : aMemcacheDescriptorPtr;
  pragma suppress( access_check ); -- disable when debugging!
begin
  getServers( mc, name, primaryServer, secondaryServer );

  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": append '" & to_string( name ) & "' on primary server" &
       primaryServer'img & " and secondary" & secondaryServer'img ) );

  memcache_cmd := "append " & name & " 0 0" & integer'image( length( value ) );
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  -- this pointer is used to improve performance. not sure why unchecked
  -- is necessary for a local pointer to a global identifier
  primaryPtr := mc.server( primaryServer )'unchecked_access;

  if primaryPtr.backoffCount > 0 then
     handleBackoff( primaryPtr );
  end if;
  if primaryPtr.backoffCount = 0 then
     begin
       put_line( primaryPtr.fd, memcache_cmd );
       put_line( primaryPtr.fd, value );
       get( primaryPtr.fd, primaryResult );
     exception when others =>
       backoffServer( primaryPtr );
     end;
  end if;

  if primaryServer = secondaryServer then
     result := primaryResult;
  else
     secondaryPtr := mc.server( secondaryServer )'unchecked_access;
     if secondaryPtr.backoffCount > 0 then
        handleBackoff( secondaryPtr );
     end if;
     if secondaryPtr.backoffCount = 0 then
        begin
          put_line( secondaryPtr.fd, memcache_cmd );
          put_line( secondaryPtr.fd, value );
          get( secondaryPtr.fd, secondaryResult );
       exception when others =>
          backoffServer( secondaryPtr );
       end;
     end if;
     reconcileResults( result, primaryResult, secondaryResult );
  end if;

  if head( result, length( memcacheResult_STORED ) ) /= memcacheResult_STORED then
     put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": append failed on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" );
  else
     null;
     pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": append succeeded on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" ) );
  end if;
end Append;


--  PREPEND
--
-- Add string to the beginning to the value for the named key.
-- constraint_error is raised if no servers are defined.
-----------------------------------------------------------------------------

procedure Prepend( mc : in out aMemcacheCluster; name, value : unbounded_string ) is
  memcache_cmd    : unbounded_string;
  primaryServer   : aServerNumber;
  secondaryServer : aServerNumber;
  result          : unbounded_string;
  primaryResult   : unbounded_string;
  secondaryResult : unbounded_string;
  primaryPtr      : aMemcacheDescriptorPtr;
  secondaryPtr    : aMemcacheDescriptorPtr;
  pragma suppress( access_check ); -- disable when debugging!
begin
  getServers( mc, name, primaryServer, secondaryServer );

  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": prepend '" & to_string( name ) & "' on primary server" &
       primaryServer'img & " and secondary" & secondaryServer'img ) );

  memcache_cmd := "prepend " & name & " 0 0" & integer'image( length( value ) );
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  -- this pointer is used to improve performance. not sure why unchecked
  -- is necessary for a local pointer to a global identifier
  primaryPtr := mc.server( primaryServer )'unchecked_access;

  if primaryPtr.backoffCount > 0 then
     handleBackoff( primaryPtr );
  end if;
  if primaryPtr.backoffCount = 0 then
     begin
       put_line( primaryPtr.fd, memcache_cmd );
       put_line( primaryPtr.fd, value );
       get( primaryPtr.fd, primaryResult );
     exception when others =>
       backoffServer( primaryPtr );
     end;
  end if;

  if primaryServer = secondaryServer then
     result := primaryResult;
  else
     secondaryPtr := mc.server( secondaryServer )'unchecked_access;
     if secondaryPtr.backoffCount > 0 then
        handleBackoff( secondaryPtr );
     end if;
     if secondaryPtr.backoffCount = 0 then
        begin
          put_line( secondaryPtr.fd, memcache_cmd );
          put_line( secondaryPtr.fd, value );
          get( secondaryPtr.fd, secondaryResult );
       exception when others =>
          backoffServer( secondaryPtr );
       end;
     end if;
     reconcileResults( result, primaryResult, secondaryResult );
  end if;

  if head( result, length( memcacheResult_STORED ) ) /= memcacheResult_STORED then
     put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": prepend failed on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" );
  else
     null;
     pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": prepend succeeded on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" ) );
  end if;
end Prepend;


--  DELETE
--
-- Delete a key and its value.
-- constraint_error is raised if no servers are defined.
-----------------------------------------------------------------------------

procedure Delete( mc : in out aMemcacheCluster; name : unbounded_string ) is
  memcache_cmd    : unbounded_string;
  primaryServer   : aServerNumber;
  secondaryServer : aServerNumber;
  result          : unbounded_string;
  primaryResult   : unbounded_string;
  secondaryResult : unbounded_string;
  primaryPtr      : aMemcacheDescriptorPtr;
  secondaryPtr    : aMemcacheDescriptorPtr;
  pragma suppress( access_check ); -- disable when debugging!
begin
  getServers( mc, name, primaryServer, secondaryServer );

  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": delete '" & to_string( name ) & "' on primary server" &
       primaryServer'img & " and secondary" & secondaryServer'img ) );

  memcache_cmd := "delete " & name;
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  -- this pointer is used to improve performance. not sure why unchecked
  -- is necessary for a local pointer to a global identifier
  primaryPtr := mc.server( primaryServer )'unchecked_access;

  if primaryPtr.backoffCount > 0 then
     handleBackoff( primaryPtr );
  end if;
  if primaryPtr.backoffCount = 0 then
     begin
       put_line( primaryPtr.fd, memcache_cmd );
       get( primaryPtr.fd, primaryResult );
     exception when others =>
       backoffServer( primaryPtr );
     end;
  end if;

  if primaryServer = secondaryServer then
     result := primaryResult;
  else
     secondaryPtr := mc.server( secondaryServer )'unchecked_access;
     if secondaryPtr.backoffCount > 0 then
        handleBackoff( secondaryPtr );
     end if;
     if secondaryPtr.backoffCount = 0 then
        begin
          put_line( secondaryPtr.fd, memcache_cmd );
          get( secondaryPtr.fd, secondaryResult );
       exception when others =>
          backoffServer( secondaryPtr );
       end;
     end if;
     reconcileResults( result, primaryResult, secondaryResult );
  end if;

  if head( result, length( memcacheResult_DELETED ) ) /= memcacheResult_DELETED then
     put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": delete failed on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" );
  else
     null;
     pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": delete succeeded on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" ) );
  end if;
end Delete;


--  GET
--
-- Return the value of the given key.  Return an empty string if the key is
-- not found or if there is a major server failure.
-- constraint_error is raised if no servers are defined.
-----------------------------------------------------------------------------

procedure Get( mc : in out aMemcacheCluster; name : unbounded_string; value : out unbounded_string ) is
  memcache_cmd    : unbounded_string;
  primaryServer   : aServerNumber;
  secondaryServer : aServerNumber;
  temp            : aServerNumber;
  result          : unbounded_string;
  primaryResult   : unbounded_string;
  secondaryResult : unbounded_string;
  p               : natural;
  sizeToRead      : integer;
  primaryPtr      : aMemcacheDescriptorPtr;
  secondaryPtr    : aMemcacheDescriptorPtr;
  pragma suppress( access_check ); -- disable when debugging!
begin
  getServers( mc, name, primaryServer, secondaryServer );
  flipPrimary := not flipPrimary;
  if flipPrimary then
    temp := primaryServer;
    primaryServer := secondaryServer;
    secondaryServer := temp;
  end if;

  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": get '" & to_string( name ) & "'" ) );

  memcache_cmd := "get " & name;
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  value := null_unbounded_string;

  primaryPtr := mc.server( primaryServer )'unchecked_access;

  if primaryPtr.backoffCount > 0 then
     handleBackoff( primaryPtr );
  end if;
  if primaryPtr.backoffCount = 0 then
     begin
       put_line( primaryPtr.fd, memcache_cmd );
       get( primaryPtr.fd, primaryResult );
       if head( primaryResult, length( memcacheResult_VALUE ) ) = memcacheResult_VALUE then
          p := length( primaryResult )-1;
          while element( primaryResult, p ) /= ' ' loop
            p := p - 1;
          end loop;
          sizeToRead := integer'value( slice( primaryResult, p, length( primaryResult ) ) );
          if sizeToRead > 0 then
             get( primaryPtr.fd, sizeToRead, value );
          end if;
          get( primaryPtr.fd, 2, primaryResult );  -- read EOL
          get( primaryPtr.fd, primaryResult );
          result := primaryResult;
       end if;
     exception when others =>
       backoffServer( primaryPtr );
     end;
  end if;

  if length( value ) = 0 then
     if primaryServer /= secondaryServer then
        secondaryPtr := mc.server( secondaryServer )'unchecked_access;
       if secondaryPtr.backoffCount > 0 then
          handleBackoff( secondaryPtr );
       end if;
       if secondaryPtr.backoffCount = 0 then
          begin
            put_line( secondaryPtr.fd, memcache_cmd );
            get( secondaryPtr.fd, secondaryResult );
            if head( secondaryResult, length( memcacheResult_VALUE ) ) = memcacheResult_VALUE then
               p := length( secondaryResult )-1;
               while element( secondaryResult, p ) /= ' ' loop
                 p := p - 1;
               end loop;
               sizeToRead := integer'value( slice( secondaryResult, p, length( secondaryResult ) ) );
               if sizeToRead > 0 then
                  get( secondaryPtr.fd, sizeToRead, value );
               end if;
               get( secondaryPtr.fd, 2, secondaryResult );  -- read EOL
               get( secondaryPtr.fd, secondaryResult );
            end if;
         exception when others =>
           backoffServer( secondaryPtr );
         end;
       end if;
       result := secondaryResult;
     end if;
  end if;

  if head( result, length( memcacheResult_END ) ) /= memcacheResult_END then
     -- a server failure will result in a null string response.  If we report
     -- every failed access attempt (which also occurs even after backing off)
     -- we'll bombard the logs.  so suppress these error messages.  the backoff
     -- warnings will still be logged anyway
     if length( result ) > 0 then
        put_line( standard_error, Gnat.Source_Info.Source_Location &
          ": get failed on key '" & to_string( name ) & "' with result '" &
          to_string( result ) & "'" );
     end if;
  else
     null;
     pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": get succeeded on key '" & to_string( name ) & "' with result '" &
       to_string( result ) & "'" ) );
  end if;
end Get;


--  STATS
--
-- Return memcache stats information from all servers in the cluster.  Also
-- returns host and port. Each line consists of server number, a space, and
-- that stat line.  Each item in the string is separated by a line feed.
-- empty string returned if no servers are defined.
-----------------------------------------------------------------------------

procedure Stats( mc : in out aMemcacheCluster; value : out unbounded_string ) is
  memcache_cmd    : unbounded_string;
  result          : unbounded_string;
begin
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": stats on all servers" ) );

  memcache_cmd := to_unbounded_string( "stats" );
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  value := null_unbounded_string;
  for s in 1..mc.serverCount loop
     if mc.server( s ).backoffCount > 0 then
        handleBackoff( mc, s );
     end if;
     result := null_unbounded_string;
     if mc.server( s ).backoffCount = 0 then
       begin
         put_line( mc.server(s).fd, memcache_cmd );
         value := value & s'img & " host " & mc.server(s).host & Latin_1.LF;
         value := value & s'img & " port" & mc.server(s).port'img & Latin_1.LF;
         loop
           get( mc.server( s ).fd, result );
         exit when head( result, length( memcacheResult_STAT ) ) /= memcacheResult_STAT;
 -- hack: slice
           value := value & s'img & ' ' & unbounded_slice( result, length( memcacheResult_STAT ) + 2, length( result ) ) & ASCII.LF;
         end loop;
       exception when others =>
         backoffServer( mc, s );
       end;
     end if;
     if head( result, length( memcacheResult_END ) ) /= memcacheResult_END then
        put_line( standard_error, Gnat.Source_Info.Source_Location &
          ": stats failed on server" & s'img & " with result '" &
          to_string( result ) & "'" );
     else
        null;
        pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
          ": stats succeeded on server" & s'img & " with result '" &
          to_string( result ) & "'" ) );
     end if;
  end loop;
end Stats;


--  VERSION
--
-- Return the version of a random server in the cluster.  (For all servers,
-- use stat.)
-- constraint_error is raised if no servers are defined.
-----------------------------------------------------------------------------

procedure Version( mc : in out aMemcacheCluster; value : out unbounded_string ) is
  memcache_cmd    : unbounded_string;
  primaryServer   : aServerNumber;
  secondaryServer : aServerNumber;
  result          : unbounded_string;
  primaryResult   : unbounded_string;
  secondaryResult : unbounded_string;
  primaryPtr      : aMemcacheDescriptorPtr;
  secondaryPtr    : aMemcacheDescriptorPtr;
begin
  getRandomServers( mc, primaryServer, secondaryServer );

  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": version on primary server" &
       primaryServer'img & " and secondary" & secondaryServer'img ) );

  memcache_cmd := to_unbounded_string( "version" );
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  primaryPtr := mc.server( primaryServer )'unchecked_access;

  if primaryPtr.backoffCount > 0 then
     handleBackoff( primaryPtr );
  end if;
  if primaryPtr.backoffCount = 0 then
     begin
       put_line( primaryPtr.fd, memcache_cmd );
       get( primaryPtr.fd, primaryResult );
     exception when others =>
       backoffServer( primaryPtr );
     end;
  end if;

  if head( result, length( memcacheResult_VERSION ) ) /= memcacheResult_VERSION then
     if primaryServer = secondaryServer then
       result := primaryResult;
    else
       secondaryPtr := mc.server( secondaryServer )'unchecked_access;
       if secondaryPtr.backoffCount > 0 then
          handleBackoff( secondaryPtr );
       end if;
       if secondaryPtr.backoffCount = 0 then
          begin
            put_line( secondaryPtr.fd, memcache_cmd );
            put_line( secondaryPtr.fd, value );
            get( secondaryPtr.fd, secondaryResult );
         exception when others =>
           backoffServer( secondaryPtr );
         end;
       end if;
       reconcileResults( result, primaryResult, secondaryResult );
    end if;
  end if;

  if head( result, length( memcacheResult_VERSION ) ) /= memcacheResult_VERSION then
     put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": version failed with result '" &
       to_string( result ) & "'" );
  else
     value := unbounded_slice( result, length( memcacheResult_VERSION ) + 2, length( result ) );
     pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": version succeeded with result '" &
       to_string( result ) & "'" ) );
  end if;
end Version;


--  FLUSH
--
-- Invalidate all caches in the cluster.  This is different from a file
-- descriptor flush.
-----------------------------------------------------------------------------

procedure Flush( mc : in out aMemcacheCluster ) is
  memcache_cmd    : unbounded_string;
  result          : unbounded_string;
begin
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
       ": flush on all servers" ) );

  memcache_cmd := to_unbounded_string( "flush_all" );
  pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": cmd = '" & to_string( memcache_cmd ) & "'" ) );

  for s in 1..mc.serverCount loop
     if mc.server( s ).backoffCount > 0 then
        handleBackoff( mc, s );
     end if;
     result := null_unbounded_string;
     if mc.server( s ).backoffCount = 0 then
       begin
         put_line( mc.server(s).fd, memcache_cmd );
         get( mc.server( s ).fd, result );
       exception when others =>
         backoffServer( mc, s );
       end;
     end if;
     if head( result, length( memcacheResult_OK ) ) /= memcacheResult_OK then
        put_line( standard_error, Gnat.Source_Info.Source_Location &
          ": flush failed on server" & s'img & " with result '" &
          to_string( result ) & "'" );
     else
        null;
        pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
          ": flush succeeded on server" & s'img & " with result '" &
          to_string( result ) & "'" ) );
     end if;
  end loop;
end Flush;

end pegasock.memcache;

