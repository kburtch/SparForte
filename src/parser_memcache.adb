------------------------------------------------------------------------------
-- Memcache Parckage Parser                                                 --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2011 Free Software Foundation              --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  This is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

with ada.text_io;
use ada.text_io;
with gen_list,
    ada.strings.unbounded,
    user_io,
    world,
    scanner,
    parser_aux,
    parser,
    parser_params,
    pegasock.memcache.highread;
use ada.strings,
    ada.strings.unbounded,
    user_io,
    world,
    scanner,
    parser_aux,
    parser,
    parser_params,
    pegasock,
    pegasock.memcache,
    pegasock.memcache.highread;

package body parser_memcache is


-----------------------------------------------------------------------------
-- UTILITIES: MEMCACHE CLUSTER
-- since HTTP result is private, we'll track them in a linked list
-----------------------------------------------------------------------------


type aMemcacheClusterID is new natural;

type aMemcacheClusterEntry is record
  id      : aMemcacheClusterID := 0;
  cluster : aMemcacheCluster;
end record;

function ">="( left, right : aMemcacheClusterEntry ) return boolean is
begin
   return left.id >= right.id;
end ">=";

function "="( left, right : aMemcacheClusterEntry ) return boolean is
begin
   return left.id >= right.id;
end "=";

package memcacheClusterList is new gen_list( aMemcacheClusterEntry, "=", ">=" );

memcacheCluster : memcacheClusterList.List;
memcacheClusterIdTop : aMemcacheClusterID := 1;                   -- id counter


--  GET CLUSTER
--
-- Lookup the cluster record.  If not found, set cluster_id to zero.
-----------------------------------------------------------------------------

procedure GetCluster( cluster_id : in out aMemcacheClusterID;
   theMemcacheClusterEntry : out aMemcacheClusterEntry;
   clusterIndex : out memcacheClusterList.aListIndex ) is
begin
  clusterIndex := 0; --to suppress compiler warning
  theMemcacheClusterEntry.id := cluster_id;
  memcacheClusterList.Find( memcacheCluster, theMemcacheClusterEntry, 1, clusterIndex );
  if clusterIndex = 0 then
     put_line( standard_error, "no such cluster id -" & cluster_id'img );
  else
     memcacheClusterList.Find( memcacheCluster, clusterIndex, theMemcacheClusterEntry );
  end if;
end GetCluster;


-----------------------------------------------------------------------------
-- UTILITIES: MEMCACHE HIGHREAD CLUSTER
-- since HTTP result is private, we'll track them in a linked list
-----------------------------------------------------------------------------


type aMemcacheDualClusterID is new natural;

type aMemcacheDualClusterEntry is record
  id      : aMemcacheDualClusterID := 0;
  cluster : aMemcacheDualCluster;
end record;

function ">="( left, right : aMemcacheDualClusterEntry ) return boolean is
begin
   return left.id >= right.id;
end ">=";

function "="( left, right : aMemcacheDualClusterEntry ) return boolean is
begin
   return left.id >= right.id;
end "=";

package memcacheDualClusterList is new gen_list( aMemcacheDualClusterEntry, "=", ">=" );

memcacheDualCluster : memcacheDualClusterList.List;
memcacheDualClusterIdTop : aMemcacheDualClusterID := 1;          -- id counter


--  GET CLUSTER
--
-- Lookup the cluster record.  If not found, set cluster_id to zero.
-----------------------------------------------------------------------------

procedure GetCluster( cluster_id : in out aMemcacheDualClusterID;
   theMemcacheClusterEntry : out aMemcacheDualClusterEntry;
   clusterIndex : out memcacheDualClusterList.aListIndex ) is
begin
  clusterIndex := 0; --to suppress compiler warning
  theMemcacheClusterEntry.id := cluster_id;
  memcacheDualClusterList.Find( memcacheDualCluster, theMemcacheClusterEntry, 1, clusterIndex );
  if clusterIndex = 0 then
     put_line( standard_error, "no such cluster id -" & cluster_id'img );
  else
     memcacheDualClusterList.Find( memcacheDualCluster, clusterIndex, theMemcacheClusterEntry );
  end if;
end GetCluster;

procedure checkMemcacheRestriction is
begin
  if restriction_no_memcache then
     err( "not allowed with " & bold( "pragma restriction( no_memcache )" ) );
  end if;
end checkMemcacheRestriction;

procedure checkRestrictedShell is
begin
  if rshOpt then
     err( "not allowed in a " & optional_bold( "restricted shell" ) );
  end if;
end checkRestrictedShell;

----------------------------------------------------------------------------
-- PARSE THE MEMCACHE PACKAGE
----------------------------------------------------------------------------


procedure ParseMemcacheIsValidMemcacheKey( result : out unbounded_string ) is
  -- Syntax: pegasock.is_valid_memcache_key
  -- Source: PegaSock.Memcache.isValidMemcacheKey
  expr_val : unbounded_string;
  expr_type : identifier;
begin
  expect( memcache_is_valid_memcache_key_t );
  ParseSingleStringParameter( expr_val, expr_type );
  if isExecutingCommand then
     begin
       checkMemcacheRestriction;
       result := to_bush_boolean( isValidMemcacheKey( expr_val ) );
     exception when others =>
       err( "exception raised" );
     end;
  end if;
end ParseMemcacheIsValidMemcacheKey;

procedure ParseMemcacheNewCluster( result : out unbounded_string ) is
-- Syntax: cluster := new_cluster;
-- Source: n/a
  cluster_entry : aMemcacheClusterEntry;
  cluster_id_value : aMemcacheClusterID;
begin
  checkRestrictedShell;
  expect( memcache_new_cluster_t );
  if isExecutingCommand then
     begin
       checkMemcacheRestriction;
        cluster_id_value := memcacheClusterIdTop;
        memcacheClusterIdTop := memcacheClusterIdTop + 1;
        cluster_entry.id := cluster_id_value;
        memcacheClusterList.Queue( memcacheCluster, cluster_entry );
        result := to_unbounded_string( long_float( cluster_id_value ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheNewCluster;

procedure ParseMemcacheRegisterServer is
-- Syntax: register_server( cluster, host, port )
-- Source: pegasock.memcache.register_server
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_register_server_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastNumericParameter( expr_val2, expr_type2, natural_t );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        port : natural := natural( to_numeric( expr_val2 ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           RegisterServer( cluster_entry.cluster, expr_val, port );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheRegisterServer;

procedure ParseMemcacheClearServers is
-- Syntax: clear_servers( cluster )
-- Source: pegasock.memcache.clear_servers
  cluster_entry : aMemcacheClusterEntry;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_clear_servers_t );
  ParseSingleInOutParameter( cluster_id, memcache_cluster_t  );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           ClearServers( cluster_entry.cluster );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheClearServers;

procedure ParseMemcacheSetClusterName is
-- Syntax: set_cluster_name( cluster, name )
-- Source: pegasock.memcache.set_cluster_name
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_set_cluster_name_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseLastStringParameter( expr_val, expr_type, string_t );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           SetClusterName( cluster_entry.cluster, expr_val );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheSetClusterName;

procedure ParseMemcacheSetClusterType is
-- Syntax: set_cluster_type( cluster, name )
-- Source: pegasock.memcache.set_cluster_type
-- Example:memcache.set_cluster_type( c, memcache.memcache_cluster_type.normal )
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  cluster_id : identifier;
  mct : aMemcacheClusterType;
begin
  checkRestrictedShell;
  expect( memcache_set_cluster_type_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseLastEnumParameter( expr_val, expr_type, memcache_cluster_type_t );
  if isExecutingCommand then
     begin
        mct := aMemcacheClusterType'val( natural( to_numeric( expr_val ) ) );
     exception when constraint_error =>
        err( "constraint error" );
     when others =>
        err( "exception raised" );
     end;
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           SetClusterType( cluster_entry.cluster, mct );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheSetClusterType;

procedure ParseMemcacheSet is
-- Syntax: set( cluster, key, value )
-- Source: pegasock.memcache.set
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_set_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Set( cluster_entry.cluster, expr_val, expr_val2 );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheSet;

procedure ParseMemcacheAdd is
-- Syntax: add( cluster, key, value )
-- Source: pegasock.memcache.add
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_add_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Add( cluster_entry.cluster, expr_val, expr_val2 );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheAdd;

procedure ParseMemcacheReplace is
-- Syntax: replace( cluster, key, value )
-- Source: pegasock.memcache.replace
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_replace_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Replace( cluster_entry.cluster, expr_val, expr_val2 );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheReplace;

procedure ParseMemcacheAppend is
-- Syntax: append( cluster, key, value )
-- Source: pegasock.memcache.append
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_append_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Append( cluster_entry.cluster,expr_val, expr_val2 );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheAppend;

procedure ParseMemcachePrepend is
-- Syntax: prepend( cluster, key, value )
-- Source: pegasock.memcache.prepend
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_prepend_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Prepend( cluster_entry.cluster, expr_val, expr_val2 );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcachePrepend;

procedure ParseMemcacheGet( result : out unbounded_string ) is
-- Syntax: value := get( cluster, key )
-- Source: pegasock.memcache.get
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_get_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseLastStringParameter( expr_val, expr_type );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Get( cluster_entry.cluster, expr_val, result );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheGet;

procedure ParseMemcacheDelete is
-- Syntax: delete( cluster, key )
-- Source: pegasock.memcache.delete
  cluster_entry : aMemcacheClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_delete_t );
  ParseFirstInOutParameter( cluster_id, memcache_cluster_t  );
  ParseLastStringParameter( expr_val, expr_type );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Delete( cluster_entry.cluster, expr_val );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheDelete;

procedure ParseMemcacheStats( result : out unbounded_string ) is
-- Syntax: stats( cluster )
-- Source: pegasock.memcache.stats
  cluster_entry : aMemcacheClusterEntry;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_stats_t );
  ParseSingleInOutParameter( cluster_id, memcache_cluster_t  );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Stats( cluster_entry.cluster, result );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheStats;

procedure ParseMemcacheVersion( result : out unbounded_string ) is
-- Syntax: version( cluster )
-- Source: pegasock.memcache.version
  cluster_entry : aMemcacheClusterEntry;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_version_t );
  ParseSingleInOutParameter( cluster_id, memcache_cluster_t  );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           pegasock.memcache.Version( cluster_entry.cluster, result );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheVersion;

procedure ParseMemcacheFlush is
-- Syntax: flush( cluster )
-- Source: pegasock.memcache.flush
  cluster_entry : aMemcacheClusterEntry;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( memcache_flush_t );
  ParseSingleInOutParameter( cluster_id, memcache_cluster_t  );
  if isExecutingCommand then
     declare
        cluster : aMemcacheClusterID := aMemcacheClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
        checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Flush( cluster_entry.cluster );
           memcacheClusterList.Replace( memcacheCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseMemcacheFlush;


----------------------------------------------------------------------------
-- PARSE THE MEMCACHE HIGHREAD PACKAGE
----------------------------------------------------------------------------


procedure ParseHighreadNewCluster( result : out unbounded_string ) is
-- Syntax: cluster := new_cluster;
-- Source: n/a
  cluster_entry : aMemcacheDualClusterEntry;
  cluster_id_value : aMemcacheDualClusterID;
begin
  checkRestrictedShell;
  expect( highread_new_cluster_t );
  if isExecutingCommand then
     begin
       checkMemcacheRestriction;
        cluster_id_value := memcacheDualClusterIdTop;
        memcacheDualClusterIdTop := memcacheDualClusterIdTop + 1;
        cluster_entry.id := cluster_id_value;
        memcacheDualClusterList.Queue( memcacheDualCluster, cluster_entry );
        result := to_unbounded_string( long_float( cluster_id_value ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadNewCluster;

procedure ParseHighreadRegisterAlphaServer is
-- Syntax: register_alpha_server( cluster, host, port )
-- Source: pegasock.memcache.highread.registerAlphaServer
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_register_alpha_server_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastNumericParameter( expr_val2, expr_type2, natural_t );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        port : natural := natural( to_numeric( expr_val2 ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           RegisterAlphaServer( cluster_entry.cluster, expr_val, port );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadRegisterAlphaServer;

procedure ParseHighreadRegisterBetaServer is
-- Syntax: register_beta_server( cluster, host, port )
-- Source: pegasock.memcache.highread.register_beta.server
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_register_beta_server_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastNumericParameter( expr_val2, expr_type2, natural_t );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        port : natural := natural( to_numeric( expr_val2 ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           RegisterBetaServer( cluster_entry.cluster, expr_val, port );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadRegisterBetaServer;

procedure ParseHighreadClearServers is
-- Syntax: clear_servers( cluster )
-- Source: pegasock.memcache.highread.clear_servers
  cluster_entry : aMemcacheDualClusterEntry;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_clear_servers_t );
  ParseSingleInOutParameter( cluster_id, highread_cluster_t  );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           ClearServers( cluster_entry.cluster );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadClearServers;

procedure ParseHighreadSetClusterName is
-- Syntax: set_cluster_name( cluster, name )
-- Source: pegasock.memcache.highread.set_cluster_name
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_set_cluster_name_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseLastStringParameter( expr_val, expr_type, string_t );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           SetClusterName( cluster_entry.cluster, expr_val );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadSetClusterName;

procedure ParseHighreadSetClusterType is
-- Syntax: set_cluster_type( cluster, name )
-- Source: pegasock.memcache.set_cluster_type
-- Example:memcache.highread.set_cluster_type( c, memcache.memcache_cluster_type.normal )
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  cluster_id : identifier;
  mct : aMemcacheClusterType;
begin
  checkRestrictedShell;
  expect( highread_set_cluster_type_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseLastEnumParameter( expr_val, expr_type, memcache_cluster_type_t );
  if isExecutingCommand then
     begin
        mct := aMemcacheClusterType'val( natural( to_numeric( expr_val ) ) );
     exception when constraint_error =>
        err( "constraint error" );
     when others =>
        err( "exception raised" );
     end;
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           SetClusterType( cluster_entry.cluster, mct );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadSetClusterType;

procedure ParseHighreadSet is
-- Syntax: set( cluster, key, value )
-- Source: pegasock.memcache.highread.set
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_set_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Set( cluster_entry.cluster, expr_val, expr_val2 );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadSet;

procedure ParseHighreadAdd is
-- Syntax: add( cluster, key, value )
-- Source: pegasock.memcache.highread.add
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_add_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Add( cluster_entry.cluster, expr_val, expr_val2 );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadAdd;

procedure ParseHighreadReplace is
-- Syntax: replace( cluster, key, value )
-- Source: pegasock.memcache.highread.replace
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_replace_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Replace( cluster_entry.cluster, expr_val, expr_val2 );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadReplace;

procedure ParseHighreadAppend is
-- Syntax: append( cluster, key, value )
-- Source: pegasock.memcache.highread.append
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_append_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Append( cluster_entry.cluster,expr_val, expr_val2 );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadAppend;

procedure ParseHighreadPrepend is
-- Syntax: prepend( cluster, key, value )
-- Source: pegasock.memcache.highread.prepend
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  expr_val2 : unbounded_string;
  expr_type2 : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_prepend_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseNextStringParameter( expr_val, expr_type );
  ParseLastStringParameter( expr_val2, expr_type2 );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Prepend( cluster_entry.cluster, expr_val, expr_val2 );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadPrepend;

procedure ParseHighreadGet( result : out unbounded_string ) is
-- Syntax: value := get( cluster, key )
-- Source: pegasock.memcache.highread.get
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_get_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseLastStringParameter( expr_val, expr_type );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Get( cluster_entry.cluster, expr_val, result );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadGet;

procedure ParseHighreadDelete is
-- Syntax: delete( cluster, key )
-- Source: pegasock.memcache.highread.delete
  cluster_entry : aMemcacheDualClusterEntry;
  expr_val : unbounded_string;
  expr_type : identifier;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_delete_t );
  ParseFirstInOutParameter( cluster_id, highread_cluster_t  );
  ParseLastStringParameter( expr_val, expr_type );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Delete( cluster_entry.cluster, expr_val );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadDelete;

procedure ParseHighreadStats( result : out unbounded_string ) is
-- Syntax: stats( cluster )
-- Source: pegasock.memcache.highread.stats
  cluster_entry : aMemcacheDualClusterEntry;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_stats_t );
  ParseSingleInOutParameter( cluster_id, highread_cluster_t  );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Stats( cluster_entry.cluster, result );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadStats;

procedure ParseHighreadVersion( result : out unbounded_string ) is
-- Syntax: version( cluster )
-- Source: pegasock.memcache.highread.version
  cluster_entry : aMemcacheDualClusterEntry;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_version_t );
  ParseSingleInOutParameter( cluster_id, highread_cluster_t  );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           pegasock.memcache.highread.Version( cluster_entry.cluster, result );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when constraint_error =>
        err( "no memcache servers registered" );
     when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadVersion;

procedure ParseHighreadFlush is
-- Syntax: flush( cluster )
-- Source: pegasock.memcache.highread.flush
  cluster_entry : aMemcacheDualClusterEntry;
  cluster_id : identifier;
begin
  checkRestrictedShell;
  expect( highread_flush_t );
  ParseSingleInOutParameter( cluster_id, highread_cluster_t  );
  if isExecutingCommand then
     declare
        cluster : aMemcacheDualClusterID := aMemcacheDualClusterID( to_numeric( identifiers( cluster_id ).value ) );
        clusterIndex : memcacheDualClusterList.aListIndex;
     begin
       checkMemcacheRestriction;
        GetCluster( cluster, cluster_entry, clusterIndex );
        if clusterIndex /= 0 then
           Flush( cluster_entry.cluster );
           memcacheDualClusterList.Replace( memcacheDualCluster, clusterIndex, cluster_entry );
        end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParseHighreadFlush;


-----------------------------------------------------------------------------
-- HOUSEKEEPING
-----------------------------------------------------------------------------


procedure StartupMemcache is
begin

  -- Memcache Package identifiers

  declareIdent( memcache_cluster_t, "memcache.memcache_cluster", long_integer_t, typeClass );
  declareIdent( memcache_cluster_type_t, "memcache.memcache_cluster_type", root_enumerated_t, typeClass );
  declareStandardConstant( memcache_cluster_type_normal_t, "memcache.memcache_cluster_type.normal", memcache_cluster_type_t, "0" );

  declareFunction(  memcache_is_valid_memcache_key_t, "memcache.is_valid_memcache_key" );
  declareProcedure( memcache_new_cluster_t, "memcache.new_cluster" );
  declareProcedure( memcache_register_server_t, "memcache.register_server" );
  declareProcedure( memcache_clear_servers_t, "memcache.clear_servers" );
  declareProcedure( memcache_set_cluster_name_t, "memcache.set_cluster_name" );
  declareProcedure( memcache_set_cluster_type_t, "memcache.set_cluster_type" );
  declareProcedure( memcache_set_t, "memcache.set" );
  declareProcedure( memcache_add_t, "memcache.add" );
  declareProcedure( memcache_replace_t, "memcache.replace" );
  declareProcedure( memcache_append_t, "memcache.append" );
  declareProcedure( memcache_prepend_t, "memcache.prepend" );
  declareProcedure( memcache_get_t, "memcache.get" );
  declareProcedure( memcache_delete_t, "memcache.delete" );
  declareProcedure( memcache_stats_t, "memcache.stats" );
  declareProcedure( memcache_version_t, "memcache.version" );
  declareProcedure( memcache_flush_t, "memcache.flush" );

  declareIdent( highread_cluster_t, "memcache.highread.memcache_dual_cluster", long_integer_t, typeClass );

  declareProcedure( highread_new_cluster_t, "memcache.highread.new_cluster" );
  declareProcedure( highread_register_alpha_server_t, "memcache.highread.register_alpha_server" );
  declareProcedure( highread_register_beta_server_t, "memcache.highread.register_beta_server" );
  declareProcedure( highread_clear_servers_t, "memcache.highread.clear_servers" );
  declareProcedure( highread_set_cluster_name_t, "memcache.highread.set_cluster_name" );
  declareProcedure( highread_set_cluster_type_t, "memcache.highread.set_cluster_type" );
  declareProcedure( highread_set_t, "memcache.highread.set" );
  declareProcedure( highread_add_t, "memcache.highread.add" );
  declareProcedure( highread_replace_t, "memcache.highread.replace" );
  declareProcedure( highread_append_t, "memcache.highread.append" );
  declareProcedure( highread_prepend_t, "memcache.highread.prepend" );
  declareProcedure( highread_get_t, "memcache.highread.get" );
  declareProcedure( highread_delete_t, "memcache.highread.delete" );
  declareProcedure( highread_stats_t, "memcache.highread.stats" );
  declareProcedure( highread_version_t, "memcache.highread.version" );
  declareProcedure( highread_flush_t, "memcache.highread.flush" );

end StartupMemcache;

procedure ShutdownMemcache is
begin
  memcacheClusterList.clear( memcacheCluster );
  memcacheDualClusterList.clear( memcacheDualCluster );
end ShutdownMemcache;

end parser_memcache;
