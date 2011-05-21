-- Support for a dual Memcached cluster (distributed in-memory cache),
-- two basic clusters working in tandem to create 4 readable servers for
-- a data item.
--
-- Ken O. Burtch, April 2010
-----------------------------------------------------------------------------

package body pegasock.memcache.highread is

-- These procedures are identical to the basic memache procedures except they
-- operate on the two basic memeache sub-clusters called alpha and beta.

-- Register should really check all hosts/ports in each cluster for duplications
-- but doesn't

procedure RegisterAlphaServer( mc : in out aMemcacheDualCluster; host : unbounded_string; port : integer ) is
begin
  RegisterServer( mc.alphaCluster, host, port );
end RegisterAlphaServer;

procedure RegisterBetaServer( mc : in out aMemcacheDualCluster; host : unbounded_string; port : integer ) is
begin
  RegisterServer( mc.betaCluster, host, port );
end RegisterBetaServer;

procedure ClearServers( mc : in out aMemcacheDualCluster ) is
begin
  ClearServers( mc.alphaCluster );
  ClearServers( mc.betaCluster );
end ClearServers;

procedure SetClusterName( mc : in out aMemcacheDualCluster; name : unbounded_string ) is
begin
  SetClusterName( mc.alphaCluster, name );
  SetClusterName( mc.betaCluster, name );
end SetClusterName;

procedure SetClusterType( mc : in out aMemcacheDualCluster; mct : aMemcacheClusterType ) is
begin
  SetClusterType( mc.alphaCluster, mct );
  SetClusterType( mc.betaCluster, mct );
end SetClusterType;

procedure Set( mc : in out aMemcacheDualCluster; name, value : unbounded_string ) is
begin
  Set( mc.alphaCluster, name, value );
  Set( mc.betaCluster, name, value );
end Set;

procedure Add( mc : in out aMemcacheDualCluster; name, value : unbounded_string ) is
begin
  Add( mc.alphaCluster, name, value );
  Add( mc.betaCluster, name, value );
end Add;

procedure Replace( mc : in out aMemcacheDualCluster; name, value : unbounded_string ) is
begin
  Replace( mc.alphaCluster, name, value );
  Replace( mc.betaCluster, name, value );
end Replace;

procedure Append( mc : in out aMemcacheDualCluster; name, value : unbounded_string ) is
begin
  Append( mc.alphaCluster, name, value );
  Append( mc.betaCluster, name, value );
end Append;

procedure Prepend( mc : in out aMemcacheDualCluster; name, value : unbounded_string ) is
begin
  Prepend( mc.alphaCluster, name, value );
  Prepend( mc.betaCluster, name, value );
end Prepend;

procedure Get( mc : in out aMemcacheDualCluster; name : unbounded_string; value : out unbounded_string ) is
begin
  mc.flipCluster := not mc.flipCluster;
  if not mc.flipCluster then
     Get( mc.alphaCluster, name, value );
     if length( value ) = 0 then
        Get( mc.betaCluster, name, value );
     end if;
  else
     Get( mc.betaCluster, name, value );
     if length( value ) = 0 then
        Get( mc.alphaCluster, name, value );
     end if;
  end if;
end Get;

procedure Delete( mc : in out aMemcacheDualCluster; name : unbounded_string ) is
begin
  Delete( mc.alphaCluster, name );
  Delete( mc.betaCluster, name );
end Delete;

procedure Stats( mc : in out aMemcacheDualCluster; value : out unbounded_string ) is
  tempStats : unbounded_string;
begin
  Stats( mc.alphaCluster, tempStats );
  tempStats := "alpha" & tempStats;
  for i in 1..length( tempStats )-1 loop
      if element( tempStats, i ) = ASCII.LF then
         value := value & ASCII.LF & "alpha";
      else
         value := value & element( tempStats, i );
      end if;
  end loop;
  Stats( mc.betaCluster, tempStats );
  tempStats := "beta" & tempStats;
  for i in 1..length( tempStats )-1 loop
      if element( tempStats, i ) = ASCII.LF then
         value := value & ASCII.LF & "beta";
      else
         value := value & element( tempStats, i );
      end if;
  end loop;
end Stats;

procedure Version( mc : in out aMemcacheDualCluster; value : out unbounded_string ) is
begin
  mc.flipCluster := not mc.flipCluster;
  if not mc.flipCluster then
     Version( mc.alphaCluster, value );
  else
     Version( mc.betaCluster, value );
  end if;
end Version;

procedure Flush( mc : in out aMemcacheDualCluster ) is
begin
  Flush( mc.alphaCluster );
  Flush( mc.betaCluster );
end Flush;

end pegasock.memcache.highread;

