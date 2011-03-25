-- Support for a dual Memcached cluster (distributed in-memory cache),
-- two basic sub-clusters working in tandem to store data items on for
-- different 4 servers for reading (instead of two).
--
-- Ken O. Burtch, April 2010
-----------------------------------------------------------------------------

package pegasock.memcache.highread is

type aMemcacheDualCluster is tagged private;

procedure RegisterAlphaServer( mc : in out aMemcacheDualCluster; host : unbounded_string; port : integer );
procedure RegisterBetaServer( mc : in out aMemcacheDualCluster; host : unbounded_string; port : integer );
procedure ClearServers( mc : in out aMemcacheDualCluster );
procedure SetClusterName( mc : in out aMemcacheDualCluster; name : unbounded_string );
procedure SetClusterType( mc : in out aMemcacheDualCluster; mct : aMemcacheClusterType );

procedure Set( mc : in out aMemcacheDualCluster; name, value : unbounded_string );
procedure Add( mc : in out aMemcacheDualCluster; name, value : unbounded_string );
procedure Replace( mc : in out aMemcacheDualCluster; name, value : unbounded_string );
procedure Append( mc : in out aMemcacheDualCluster; name, value : unbounded_string );
procedure Prepend( mc : in out aMemcacheDualCluster; name, value : unbounded_string );
procedure Get( mc : in out aMemcacheDualCluster; name : unbounded_string; value : out unbounded_string );
procedure Delete( mc : in out aMemcacheDualCluster; name : unbounded_string );
procedure Stats( mc : in out aMemcacheDualCluster; value : out unbounded_string );
procedure Version( mc : in out aMemcacheDualCluster; value : out unbounded_string );
procedure Flush( mc : in out aMemcacheDualCluster );

-----------------------------------------------------------------------------
private
-----------------------------------------------------------------------------

type aMemcacheDualCluster is tagged record
  alphaCluster : aMemcacheCluster;
  betaCluster  : aMemcacheCluster;
  flipCluster  : boolean := true;
end record;

end pegasock.memcache.highread;

