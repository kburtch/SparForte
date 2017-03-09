#!/usr/local/bin/spar

pragma annotate( summary, "Run good tests of btree_io library" )
              @( description, "Runs a series of typical operations on the" )
              @( description, "btree_io library to ensure these operations" )
              @( description, "are not broken." )
              @( created, "January 14, 2017" )
              @( author, "Ken O. Burtch" );

procedure btree_good_test is
pragma annotate( todo, "arrays not fully implemented" );
pragma annotate( todo, "should loop to test for memory leaks" );

type person_type is record
   first_name : string;
   age  : natural;
end record;

type arr_type is array(1..2) of natural;

person : person_type;

f : btree_io.file( person_type );
i : integer;
b : boolean;
s : string;
a : arr_type;
e : bdb.db_error;

fi : btree_io.file( integer );
fs : btree_io.file( string );
fa : btree_io.file( arr_type );

begin

-- create the test directory if it doesn't exist.  Otherwise, clear it.

if not files.is_directory( "btree_test" ) then
   mkdir btree_test ;
else
   rm -f btree_test/* ;
end if;

pragma assert( not btree_io.is_open( f ) );

btree_io.create( f, "btree_test/person.btree", 80, 80 );
pragma assert( btree_io.is_open( f ) );
s := btree_io.name( f );
pragma assert( s = "btree_test/person.btree" );

b := btree_io.will_raise( f );
pragma assert( b = true );
btree_io.raise_exceptions( f, true );
b := btree_io.will_raise( f );
pragma assert( b = true );

e := btree_io.last_error( f );
pragma assert( e = 0 );
person.first_name := "John";
person.age  := 18;
btree_io.set( f, person.first_name, person );

b := btree_io.has_element( f, "John" );
pragma assert( b = true );

btree_io.get( f, "John", person );
pragma assert( person.first_name = "John" );
pragma assert( person.age = 18 );

person.first_name := "Sue";
person.age  := 21;
btree_io.add( f, person.first_name, person );

person.first_name := "Al";
person.age  := 42;
btree_io.add( f, person.first_name, person );

person.first_name := "Foo";
person.age  := 41;
btree_io.add( f, person.first_name, person );
person.first_name := "Foo";
person.age  := 42;
btree_io.replace( f, person.first_name, person );
btree_io.get( f, "Foo", person );
pragma assert( person.first_name = "Foo" );
pragma assert( person.age = 42 );
btree_io.remove( f, "Foo" );
b := btree_io.has_element( f, "Foo" );
pragma assert( b = false );

btree_io.flush( f );

declare
  c : btree_io.cursor( person_type );
  key : string;
begin
  btree_io.open_cursor( f, c );
  btree_io.get_last( f, c, key, person );
  pragma assert( key = "Sue" );

  btree_io.get_first( f, c, key, person );
  pragma assert( key = "Al" );

  i := 1;
  loop
    btree_io.get_next( f, c, key, person );
    i := @ + 1;
  end loop;
exception when others =>
  btree_io.close_cursor( f, c );
end;
pragma assert( i = 3 );

-- replace by cursor
declare
  c : btree_io.cursor( person_type );
  key : string;
  new_person : person_type;
begin
  btree_io.open_cursor( f, c );
  btree_io.get_first( f, c, key, person );
  btree_io.get_next( f, c, key, person );
  btree_io.get_previous( f, c, key, person );
  pragma assert( key = "Al" );
  new_person.first_name := "Abc";
  new_person.age  := 44;
  btree_io.replace( f, c, new_person );
  btree_io.get_first( f, c, key, person );
  btree_io.close_cursor( f, c );
  pragma assert( person.first_name = "Abc" );
  pragma assert( person.age = 44 );
exception when others =>
  btree_io.close_cursor( f, c );
end;

btree_io.close( f );
pragma assert( not btree_io.is_open( f ) );

btree_io.open( f, "btree_test/person.btree", 80, 80 );
pragma assert( btree_io.is_open( f ) );
btree_io.truncate( f );
btree_io.close( f );

btree_io.delete( f );
pragma assert( not btree_io.is_open( f ) );

btree_io.create( f, "btree_test/person.btree", 80, 80 );
btree_io.delete( f );
pragma assert( not btree_io.is_open( f ) );

-- Clear test

btree_io.create( f, "btree_test/person.btree", 80, 80 );
btree_io.clear( f );
btree_io.open( f, "btree_test/person.btree", 80, 80 );
btree_io.delete( f );

-- Integers

btree_io.create( fi, "btree_test/integer.btree", 80, 80 );
btree_io.set( fi, "foobar", 1 );
btree_io.increment( fi, "foobar" );
btree_io.get( fi, "foobar", i );
pragma assert( i = 2 );
btree_io.increment( fi, "foobar", 2 );
btree_io.get( fi, "foobar", i );
pragma assert( i = 4 );
btree_io.decrement( fi, "foobar" );
btree_io.get( fi, "foobar", i );
pragma assert( i = 3 );
btree_io.decrement( fi, "foobar", 2 );
btree_io.get( fi, "foobar", i );
pragma assert( i = 1 );
btree_io.replace( fi, "foobar", 3 );
btree_io.get( fi, "foobar", i );
pragma assert( i = 3 );
btree_io.add( fi, "foobar2", 905 );
btree_io.get( fi, "foobar2", i );
pragma assert( i = 905 );
btree_io.close( fi );
btree_io.delete( fi );

-- integer cursor

btree_io.create( fi, "btree_test/integer.btree", 80, 80 );
btree_io.set( fi, "bar", 1 );
btree_io.set( fi, "foo", 2 );
declare
  c : btree_io.cursor( integer );
  key : string;
  new_int : integer;
begin
  btree_io.open_cursor( fi, c );
  btree_io.get_last( fi, c, key, new_int );
  pragma assert( key = "foo" );

  btree_io.get_first( fi, c, key, new_int );
  pragma assert( key = "bar" );

  i := 1;
  loop
    btree_io.get_next( fi, c, key, new_int );
    i := @+1;
  end loop;
exception when others =>
  btree_io.close_cursor( fi, c );
end;
pragma assert( i = 2 );

-- integer replace by cursor

declare
  c : btree_io.cursor( integer );
  key : string;
  new_int : integer;
begin
  btree_io.open_cursor( fi, c );
  btree_io.get_first( fi, c, key, new_int );
  btree_io.get_next( fi, c, key, new_int );
  btree_io.get_previous( fi, c, key, new_int );
  pragma assert( key = "foo" );
  btree_io.replace( fi, c, 3 );
  btree_io.get_first( fi, c, key, new_int );
  pragma assert( new_int = 3 );
  btree_io.remove( fi, c );
  btree_io.get_first( fi, c, key, new_int );
  pragma assert( new_int = 2 );
  btree_io.close_cursor( fi, c );
exception when others =>
  btree_io.close_cursor( fi, c );
end;
btree_io.close( fi );
btree_io.delete( fi );

-- Strings

btree_io.create( fs, "btree_test/string.btree", 80, 80 );
btree_io.set( fs, "foobar", "pool" );
btree_io.set( fs, "foobar", "pool" );
btree_io.get( fs, "foobar", s );
pragma assert( s = "pool" );
btree_io.append( fs, "foobar", "s" );
btree_io.get( fs, "foobar", s );
pragma assert( s = "pools" );
btree_io.prepend( fs, "foobar", "s" );
btree_io.get( fs, "foobar", s );
pragma assert( s = "spools" );
btree_io.replace( fs, "foobar", "pond" );
btree_io.get( fs, "foobar", s );
pragma assert( s = "pond" );
btree_io.add( fs, "foobar2", "lake" );
btree_io.get( fs, "foobar2", s );
pragma assert( s = "lake" );
btree_io.close( fs );
btree_io.delete( fs );

-- string cursor

btree_io.create( fs, "btree_test/string.btree", 80, 80 );
btree_io.set( fs, "foo", "blue" );
btree_io.set( fs, "bar", "green" );
declare
  c : btree_io.cursor( string );
  key : string;
  new_s : string;
begin
  btree_io.open_cursor( fs, c );
  btree_io.get_last( fs, c, key, new_s );
  pragma assert( key = "bar" );

  btree_io.get_first( fs, c, key, new_s );
  pragma assert( key = "foo" );

  i := 1;
  loop
    btree_io.get_next( fs, c, key, new_s );
    i := @+1;
  end loop;
exception when others =>
  btree_io.close_cursor( fs, c );
end;
pragma assert( i = 2 );

-- string replace by cursor

declare
  c : btree_io.cursor( string );
  key : string;
  new_s : string;
begin
  btree_io.open_cursor( fs, c );
  btree_io.get_first( fs, c, key, new_s );
  btree_io.get_next( fs, c, key, new_s );
  btree_io.get_previous( fs, c, key, new_s );
  pragma assert( key = "foo" );
  btree_io.replace( fs, c, "red" );
  btree_io.get_first( fs, c, key, new_s );
  pragma assert( new_s = "red" );
  btree_io.remove( fs, c );
  btree_io.get_first( fs, c, key, new_s );
  pragma assert( new_s = "blue" );
  btree_io.close_cursor( fs, c );
exception when others =>
  btree_io.close_cursor( fs, c );
end;
btree_io.close( fs );
btree_io.delete( fs );

-- Arrays

btree_io.create( fa, "btree_test/array.btree", 80, 80 );
a(1) := 3;
a(2) := 4;
btree_io.set( fa, "foobar", a );
a(1) := 0;
a(2) := 0;
btree_io.get( fa, "foobar", a );
pragma assert( a(1) = 3 );
pragma assert( a(2) = 4 );
a(1) := 5;
a(2) := 6;
btree_io.add( fa, "foobar2", a );
a(1) := 0;
a(2) := 0;
btree_io.get( fa, "foobar2", a );
pragma assert( a(1) = 5 );
pragma assert( a(2) = 6 );
a(1) := 5;
a(2) := 6;
btree_io.replace( fa, "foobar", a );
a(1) := 0;
a(2) := 0;
btree_io.get( fa, "foobar", a );
pragma assert( a(1) = 5 );
pragma assert( a(2) = 6 );
btree_io.close( fa );
btree_io.delete( fa );

-- Array cursor

btree_io.create( fa, "btree_test/array.btree", 80, 80 );
a(1) := 1;
a(2) := 2;
btree_io.set( fa, "foo", a );
a(1) := 3;
a(2) := 4;
btree_io.set( fa, "bar", a );
declare
  c : btree_io.cursor( arr_type );
  key : string;
  new_a : arr_type;
begin
  btree_io.open_cursor( fa, c );
  btree_io.get_last( fa, c, key, new_a );
  pragma assert( key = "bar" );
  pragma assert( new_a(1) = 3 );
  pragma assert( new_a(2) = 4 );

  btree_io.get_first( fa, c, key, new_a );
  pragma assert( key = "foo" );
  pragma assert( new_a(1) = 1 );
  pragma assert( new_a(2) = 2 );

  i := 1;
  loop
    btree_io.get_next( fa, c, key, new_a );
    i := @+1;
  end loop;
exception when others =>
  btree_io.close_cursor( fa, c );
end;
pragma assert( i = 2 );

-- array replace by cursor

declare
  c : btree_io.cursor( arr_type );
  key : string;
  new_a : arr_type;
begin
  btree_io.open_cursor( fa, c );
  btree_io.get_first( fa, c, key, new_a );
  btree_io.get_next( fa, c, key, new_a );
  btree_io.get_previous( fa, c, key, new_a );
  pragma assert( key = "bar" );
  a(1) := 5;
  a(2) := 6;
  btree_io.replace( fa, c, a );
  btree_io.get_first( fa, c, key, new_a );
  pragma assert( new_a(1) = 5 );
  pragma assert( new_a(2) = 6 );
  btree_io.remove( fa, c );
  btree_io.get_first( fa, c, key, new_a );
  pragma assert( new_a(1) = 1 );
  pragma assert( new_a(2) = 2 );
  btree_io.close_cursor( fa, c );
exception when others =>
  btree_io.close_cursor( fa, c );
  raise;
end;
btree_io.close( fa );
btree_io.delete( fa );

command_line.set_exit_status( 0 );
end btree_good_test;

-- VIM editor formatting instructions
-- vim: ft=spar

