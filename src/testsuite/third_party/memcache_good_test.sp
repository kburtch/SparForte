#!/usr/local/bin/spar

pragma annotate( summary, "Run good tests of the memcacheb library" )
              @( description, "Runs a series of typical operations on the" )
              @( description, "memcache library to ensure these operations" )
              @( description, "are not broken." )
              @( created, "February 13, 2017" )
              @( author, "Ken O. Burtch" );

procedure memcache_good_test is
pragma annotate( todo, "should really test each of arrays, strings, records" );
pragma annotate( todo, "should loop to test for memory leaks" );

mc : memcache.memcache_cluster;

s : string;
b : boolean;

begin

-- cluster setup

mc := memcache.new_cluster;
memcache.register_server( mc, "localhost", 11211 );
memcache.set_cluster_name( mc, "test cluster" );
memcache.set_cluster_type( mc, memcache.memcache_cluster_type.normal );

-- basic operations

s := memcache.version( mc );
pragma assert( s /= "" );

b := memcache.is_valid_memcache_key( "foo" );
pragma assert( b );

b := memcache.is_valid_memcache_key( "foo bar" );
pragma assert( not b );

-- Basic delete, get and set

memcache.set( mc, "foo", "bar" );
s := memcache.get( mc, "foo" );
pragma assert( s = "bar" );

memcache.set( mc, "foo", "" );
s := memcache.get( mc, "foo" );
pragma assert( s = "" );

memcache.set( mc, "foo", "bar" );
memcache.delete( mc, "foo" );
s := memcache.get( mc, "foo" );
pragma assert( s = "" );

-- flush

memcache.set( mc, "foo", "bar" );
memcache.flush( mc );
s := memcache.get( mc, "foo" );
pragma assert( s = "" );

-- advanced sets

memcache.add( mc, "foo", "baz" );
s := memcache.get( mc, "foo" );
pragma assert( s = "baz" );

memcache.replace( mc, "foo", "bar" );
s := memcache.get( mc, "foo" );
pragma assert( s = "bar" );

memcache.append( mc, "foo", "baz" );
s := memcache.get( mc, "foo" );
pragma assert( s = "barbaz" );

memcache.prepend( mc, "foo", "boo" );
s := memcache.get( mc, "foo" );
pragma assert( s = "boobarbaz" );

memcache.append( mc, "foo", "" );
s := memcache.get( mc, "foo" );
pragma assert( s = "boobarbaz" );

memcache.prepend( mc, "foo", "" );
s := memcache.get( mc, "foo" );
pragma assert( s = "boobarbaz" );

-- stats

s := memcache.stats( mc );
pragma assert( s /= "" );

-- cleanup

memcache.flush( mc );
memcache.clear_servers( mc );

command_line.set_exit_status( 0 );
end memcache_good_test;

-- VIM editor formatting instructions
-- vim: ft=spar

