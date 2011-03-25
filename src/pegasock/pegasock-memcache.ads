-- Basic support for the Memcached (distributed in-memory cache)
-- Ken O. Burtch, April 2010
-----------------------------------------------------------------------------

package pegasock.memcache is

type aMemcacheDescriptor is private;

type aMemcacheCluster is tagged private;
-- tagged

type aMemcacheClusterType is ( normal );
-- future expanson

function isValidMemcacheKey( key : unbounded_string ) return boolean;

procedure RegisterServer( mc : in out aMemcacheCluster; host : unbounded_string; port : integer );
procedure ClearServers( mc : in out aMemcacheCluster );
procedure SetClusterName( mc : in out aMemcacheCluster; name : unbounded_string );
procedure SetClusterType( mc : in out aMemcacheCluster; mct : aMemcacheClusterType );

procedure Set( mc : in out aMemcacheCluster; name, value : unbounded_string );
procedure Add( mc : in out aMemcacheCluster; name, value : unbounded_string );
procedure Replace( mc : in out aMemcacheCluster; name, value : unbounded_string );
procedure Append( mc : in out aMemcacheCluster; name, value : unbounded_string );
procedure Prepend( mc : in out aMemcacheCluster; name, value : unbounded_string );
procedure Get( mc : in out aMemcacheCluster; name : unbounded_string; value : out unbounded_string );
procedure Delete( mc : in out aMemcacheCluster; name : unbounded_string );
procedure Stats( mc : in out aMemcacheCluster; value : out unbounded_string );
procedure Version( mc : in out aMemcacheCluster; value : out unbounded_string );
procedure Flush( mc : in out aMemcacheCluster );
--procedure Quit( mc : in out aMemcacheCluster );

-----------------------------------------------------------------------------
private
-----------------------------------------------------------------------------

type aMemcacheDescriptor is record
     fd           : aBufferedSocket;
     host         : unbounded_string;
     port         : integer;
     backoffCount : integer := 0;
     backoffLevel : integer := 1;
end record;
type aMemcacheDescriptorPtr is access all aMemcacheDescriptor;

backoffMax : constant integer := 3;
backoffFactor : constant integer := 8;
-- exponential backoff: for 3 and 8, retry after 8**1 = 8, 8**2 = 64, 8**3 = 512

type aServerNumber is new integer range 0..32;

type aMemcacheArray is array ( 1..aServerNumber'last ) of aliased aMemcacheDescriptor;

type aMemcacheCluster is tagged record
  name        : unbounded_string;
  clusterType : aMemcacheClusterType := normal;
  server      : aMemcacheArray;
  serverCount : aServerNumber := 0;
end record;

flipPrimary : boolean := true;
-- global

end pegasock.memcache;

