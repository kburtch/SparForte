#!/usr/local/bin/spar

pragma annotate( summary, "Run good tests of hash_io library" )
              @( description, "Runs a series of typical operations on the" )
              @( description, "hash_io library to ensure these operations" )
              @( description, "are not broken." )
              @( created, "January 14, 2017" )
              @( author, "Ken O. Burtch" );

procedure hash_good_test is
pragma annotate( todo, "arrays not fully implemented" );
pragma annotate( todo, "should loop to test for memory leaks" );

type person_type is record
   first_name : string;
   age  : natural;
end record;

type arr_type is array(1..2) of natural;

person : person_type;

f : hash_io.file( person_type );
i : integer;
b : boolean;
s : string;
a : arr_type;
e : bdb.db_error;

fi : hash_io.file( integer );
fs : hash_io.file( string );
fa : hash_io.file( arr_type );

begin

-- create the test directory if it doesn't exist.  Otherwise, clear it.
-- reuse btree directory

if not files.is_directory( "btree_test" ) then
   mkdir btree_test ;
else
   rm -f btree_test/* ;
end if;

pragma assert( not hash_io.is_open( f ) );

hash_io.create( f, "btree_test/person.hash", 80, 80 );
pragma assert( hash_io.is_open( f ) );
s := hash_io.name( f );
pragma assert( s = "btree_test/person.hash" );

b := hash_io.will_raise( f );
pragma assert( b = true );
hash_io.raise_exceptions( f, true );
b := hash_io.will_raise( f );
pragma assert( b = true );

e := hash_io.last_error( f );
pragma assert( e = 0 );
person.first_name := "John";
person.age  := 18;
hash_io.set( f, person.first_name, person );

b := hash_io.has_element( f, "John" );
pragma assert( b = true );

hash_io.get( f, "John", person );
pragma assert( person.first_name = "John" );
pragma assert( person.age = 18 );

person.first_name := "Sue";
person.age  := 21;
hash_io.add( f, person.first_name, person );

person.first_name := "Al";
person.age  := 42;
hash_io.add( f, person.first_name, person );

person.first_name := "Foo";
person.age  := 41;
hash_io.add( f, person.first_name, person );
person.first_name := "Foo";
person.age  := 42;
hash_io.replace( f, person.first_name, person );
hash_io.get( f, "Foo", person );
pragma assert( person.first_name = "Foo" );
pragma assert( person.age = 42 );
hash_io.remove( f, "Foo" );
b := hash_io.has_element( f, "Foo" );
pragma assert( b = false );

hash_io.flush( f );

declare
  c : hash_io.cursor( person_type );
  key : string;
begin
  hash_io.open_cursor( f, c );
  hash_io.get_last( f, c, key, person );
  pragma assert( key = "Sue" );

  hash_io.get_first( f, c, key, person );
  pragma assert( key = "Al" );

  i := 1;
  loop
    hash_io.get_next( f, c, key, person );
    i := @ + 1;
  end loop;
exception when others =>
  hash_io.close_cursor( f, c );
end;
pragma assert( i = 3 );

-- replace by cursor
declare
  c : hash_io.cursor( person_type );
  key : string;
  new_person : person_type;
begin
  hash_io.open_cursor( f, c );
  hash_io.get_first( f, c, key, person );
  hash_io.get_next( f, c, key, person );
  hash_io.get_previous( f, c, key, person );
  pragma assert( key = "Al" );
  new_person.first_name := "Abc";
  new_person.age  := 44;
  hash_io.replace( f, c, new_person );
  hash_io.get_first( f, c, key, person );
  hash_io.close_cursor( f, c );
  pragma assert( person.first_name = "Abc" );
  pragma assert( person.age = 44 );
exception when others =>
  hash_io.close_cursor( f, c );
end;

hash_io.close( f );
pragma assert( not hash_io.is_open( f ) );

hash_io.open( f, "btree_test/person.hash", 80, 80 );
pragma assert( hash_io.is_open( f ) );
hash_io.truncate( f );
hash_io.close( f );

hash_io.delete( f );
pragma assert( not hash_io.is_open( f ) );

hash_io.create( f, "btree_test/person.hash", 80, 80 );
hash_io.delete( f );
pragma assert( not hash_io.is_open( f ) );

-- Clear test

hash_io.recover(f, "btree_test/person.hash" );
hash_io.create( f, "btree_test/person.hash", 80, 80 );
hash_io.clear( f );
hash_io.open( f, "btree_test/person.hash", 80, 80 );
hash_io.delete( f );

-- Integers

hash_io.create( fi, "btree_test/integer.hash", 80, 80 );
hash_io.set( fi, "foobar", 1 );
hash_io.increment( fi, "foobar" );
hash_io.get( fi, "foobar", i );
pragma assert( i = 2 );
hash_io.increment( fi, "foobar", 2 );
hash_io.get( fi, "foobar", i );
pragma assert( i = 4 );
hash_io.decrement( fi, "foobar" );
hash_io.get( fi, "foobar", i );
pragma assert( i = 3 );
hash_io.decrement( fi, "foobar", 2 );
hash_io.get( fi, "foobar", i );
pragma assert( i = 1 );
hash_io.replace( fi, "foobar", 3 );
hash_io.get( fi, "foobar", i );
pragma assert( i = 3 );
hash_io.add( fi, "foobar2", 905 );
hash_io.get( fi, "foobar2", i );
pragma assert( i = 905 );
hash_io.close( fi );
hash_io.delete( fi );

-- integer cursor

hash_io.create( fi, "btree_test/integer.hash", 80, 80 );
hash_io.set( fi, "bar", 1 );
hash_io.set( fi, "foo", 2 );
declare
  c : hash_io.cursor( integer );
  key : string;
  new_int : limited integer;
begin
  hash_io.open_cursor( fi, c );
  hash_io.get_last( fi, c, key, new_int );
  -- order not guarnteed
  pragma assert( key = "foo" or key = "bar" );

  hash_io.get_first( fi, c, key, new_int );
  -- order not guarnteed
  pragma assert( key = "foo" or key = "bar" );

  i := 1;
  loop
    hash_io.get_next( fi, c, key, new_int );
    i := @+1;
  end loop;
exception when others =>
  hash_io.close_cursor( fi, c );
end;
pragma assert( i = 2 );

-- integer replace by cursor

declare
  c : hash_io.cursor( integer );
  key : string;
  new_int : integer;
begin
  hash_io.open_cursor( fi, c );
  hash_io.get_first( fi, c, key, new_int );
  hash_io.get_next( fi, c, key, new_int );
  hash_io.get_previous( fi, c, key, new_int );
  pragma assert( key = "foo" );
  hash_io.replace( fi, c, 3 );
  hash_io.get_first( fi, c, key, new_int );
  pragma assert( new_int = 3 );
  hash_io.remove( fi, c );
  hash_io.get_first( fi, c, key, new_int );
  pragma assert( new_int = 2 );
  hash_io.close_cursor( fi, c );
exception when others =>
  hash_io.close_cursor( fi, c );
end;
hash_io.close( fi );
hash_io.delete( fi );

-- Strings

hash_io.create( fs, "btree_test/string.hash", 80, 80 );
hash_io.set( fs, "foobar", "pool" );
hash_io.set( fs, "foobar", "pool" );
hash_io.get( fs, "foobar", s );
pragma assert( s = "pool" );
hash_io.append( fs, "foobar", "s" );
hash_io.get( fs, "foobar", s );
pragma assert( s = "pools" );
hash_io.prepend( fs, "foobar", "s" );
hash_io.get( fs, "foobar", s );
pragma assert( s = "spools" );
hash_io.replace( fs, "foobar", "pond" );
hash_io.get( fs, "foobar", s );
pragma assert( s = "pond" );
hash_io.add( fs, "foobar2", "lake" );
hash_io.get( fs, "foobar2", s );
pragma assert( s = "lake" );
hash_io.close( fs );
hash_io.delete( fs );

-- string cursor

hash_io.create( fs, "btree_test/string.hash", 80, 80 );
hash_io.set( fs, "foo", "blue" );
hash_io.set( fs, "bar", "green" );
declare
  c : hash_io.cursor( string );
  key : string;
  new_s : limited string;
begin
  hash_io.open_cursor( fs, c );
  hash_io.get_last( fs, c, key, new_s );
  -- order no guarnateed
  pragma assert( key = "bar" or key = "foo" );

  hash_io.get_first( fs, c, key, new_s );
  -- order no guarnateed
  pragma assert( key = "bar" or key = "foo" );

  i := 1;
  loop
    hash_io.get_next( fs, c, key, new_s );
    i := @+1;
  end loop;
exception when others =>
  hash_io.close_cursor( fs, c );
end;
pragma assert( i = 2 );

-- string replace by cursor

declare
  c : hash_io.cursor( string );
  key : string;
  new_s : string;
begin
  hash_io.open_cursor( fs, c );
  hash_io.get_first( fs, c, key, new_s );
  hash_io.get_next( fs, c, key, new_s );
  hash_io.get_previous( fs, c, key, new_s );
  -- order no guarnateed
  pragma assert( key = "bar" or key = "foo" );
  hash_io.replace( fs, c, "red" );
  hash_io.get_first( fs, c, key, new_s );
  pragma assert( new_s = "red" );
  hash_io.remove( fs, c );
  hash_io.get_first( fs, c, key, new_s );
  pragma assert( new_s = "blue" );
  hash_io.close_cursor( fs, c );
exception when others =>
  hash_io.close_cursor( fs, c );
end;
hash_io.close( fs );
hash_io.delete( fs );

-- Arrays

hash_io.create( fa, "btree_test/array.hash", 80, 80 );
a(1) := 3;
a(2) := 4;
hash_io.set( fa, "foobar", a );
a(1) := 0;
a(2) := 0;
hash_io.get( fa, "foobar", a );
pragma assert( a(1) = 3 );
pragma assert( a(2) = 4 );
a(1) := 5;
a(2) := 6;
hash_io.add( fa, "foobar2", a );
a(1) := 0;
a(2) := 0;
hash_io.get( fa, "foobar2", a );
pragma assert( a(1) = 5 );
pragma assert( a(2) = 6 );
a(1) := 5;
a(2) := 6;
hash_io.replace( fa, "foobar", a );
a(1) := 0;
a(2) := 0;
hash_io.get( fa, "foobar", a );
pragma assert( a(1) = 5 );
pragma assert( a(2) = 6 );
hash_io.close( fa );
hash_io.delete( fa );

-- Array cursor

hash_io.create( fa, "btree_test/array.hash", 80, 80 );
a(1) := 1;
a(2) := 2;
hash_io.set( fa, "foo", a );
a(1) := 3;
a(2) := 4;
hash_io.set( fa, "bar", a );
declare
  c : hash_io.cursor( arr_type );
  key : string;
  new_a : limited arr_type;
begin
  hash_io.open_cursor( fa, c );
  hash_io.get_last( fa, c, key, new_a );
  -- order no guarnateed
  pragma assert( key = "bar" or key = "foo" );

  hash_io.get_first( fa, c, key, new_a );
  -- order no guarnateed
  pragma assert( key = "bar" or key = "foo" );

  i := 1;
  loop
    hash_io.get_next( fa, c, key, new_a );
    i := @+1;
  end loop;
exception when others =>
  hash_io.close_cursor( fa, c );
end;
pragma assert( i = 2 );

-- array replace by cursor

declare
  c : hash_io.cursor( arr_type );
  key : string;
  new_a : limited arr_type;
begin
  hash_io.open_cursor( fa, c );
  hash_io.get_first( fa, c, key, new_a );
  hash_io.get_next( fa, c, key, new_a );
  hash_io.get_previous( fa, c, key, new_a );
  -- order no guarnateed
  pragma assert( key = "bar" or key = "foo" );
  hash_io.replace( fa, c, a );
  hash_io.get_first( fa, c, key, new_a );
  -- order no guarnateed
  pragma assert( key = "bar" or key = "foo" );
  hash_io.remove( fa, c );
  hash_io.get_first( fa, c, key, new_a );
  -- order no guarnateed
  pragma assert( key = "bar" or key = "foo" );
  hash_io.close_cursor( fa, c );
exception when others =>
  hash_io.close_cursor( fa, c );
  raise;
end;
hash_io.close( fa );
hash_io.delete( fa );

command_line.set_exit_status( 0 );
end hash_good_test;

-- VIM editor formatting instructions
-- vim: ft=spar

