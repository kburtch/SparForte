------------------------------------------------------------------------------
-- Http Package Parser                                                      --
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
    pegasock.http;
use ada.strings,
    ada.strings.unbounded,
    user_io,
    world,
    scanner,
    parser_aux,
    parser,
    parser_params,
    pegasock,
    pegasock.http;

package body parser_http is

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupHttp;
procedure ShutdownHttp;

------------------------------------------------------------------------------
-- PARSE THE HTTP PACKAGE
------------------------------------------------------------------------------

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
-- PARSE THE HTTP PACKAGE
----------------------------------------------------------------------------

procedure ParseHttpNewSocket( result : out unbounded_string ) is
begin
end ParseHttpNewSocket;

procedure ParseHttpGet is
begin
end ParseHttpGet;

procedure ParseHttpHead is
begin
end ParseHttpHead;

procedure ParseHttpPost is
begin
end ParseHttpPost;

procedure ParseHttpClearFormVariables is
begin
end ParseHttpClearFormVariables;

procedure ParseHttpAddFormVariables is
begin
end ParseHttpAddFormVariables;

procedure ParseHttpGetResponseCode( result : out unbounded_string ) is
begin
end ParseHttpGetResponseCode;

procedure ParseHttpGetHeader( result : out unbounded_string ) is
begin
end ParseHttpGetHeader;

procedure ParseHttpGetContent( result : out unbounded_string ) is
begin
end ParseHttpGetContent;

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
        err_exception_raised;
     end;
  end if;
end ParseMemcacheSetClusterName;


-----------------------------------------------------------------------------
-- HOUSEKEEPING
-----------------------------------------------------------------------------


procedure StartupHttp is
begin
  declareNamespace( "http" );
  -- Http Package identifiers

  declareIdent( http_result_id_t, "http.result_id", long_integer_t, typeClass );
  declareIdent( http_socket_id_t, "http.socket_id", long_integer_t, typeClass );

  declareFunction(  http_new_socket_t, "http.new_socket" );
  declareProcedure( http_get_t, "http.get" );
  declareProcedure( http_head_t, "http.head" );
  declareProcedure( http_post_t, "http.post" );
  declareProcedure( http_clear_form_variables_t, "http.clear_form_variables" );
  declareProcedure( http_add_form_variable_t, "http.add_form_variable" );
  declareFunction(  http_get_response_code_t, "http.get_response_code" );
  declareFunction(  http_get_header_t, "http.get_header" );
  declareFunction(  http_get_content_t, "http.get_content" );
  declareNamespaceClosed( "http" );
end StartupHttp;

procedure ShutdownHttp is
begin
  httpSocketList.clear( httpSockets );
  httpResultList.clear( httpResults );
end ShutdownHttp;

end parser_http;

