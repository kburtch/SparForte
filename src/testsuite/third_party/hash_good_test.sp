#!/usr/local/bin/spar

pragma annotate( summary, "Run good tests of hash_io library" )
              @( description, "Runs a series of typical operations on the" )
              @( description, "hash_io library to ensure these operations" )
              @( description, "are not broken." )
              @( created, "February 17, 2017" )
              @( author, "Ken O. Burtch" );

procedure hash_good_test is
pragma annotate( todo, "should really test each of arrays, strings, records" );
pragma annotate( todo, "should loop to test for memory leaks" );

type person_type is record
   first_name : string;
   age  : natural;
end record;

person : person_type;

f : hash_io.file( person_type );
i : integer;
b : boolean;
e : bdb.db_error;

begin

-- create the test directory if it doesn't exist.  Otherwise, clear it.

if not files.is_directory( "btree_test" ) then
   mkdir btree_test ;
else
   rm -f btree_test/* ;
end if;

pragma assert( not hash_io.is_open( f ) );

hash_io.create( f, "btree_test/person.hash", 80, 80 );
pragma assert( hash_io.is_open( f ) );

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

hash_io.delete( f );
pragma assert( not hash_io.is_open( f ) );

hash_io.create( f, "btree_test/person.hash", 80, 80 );
hash_io.delete( f );
pragma assert( not hash_io.is_open( f ) );

command_line.set_exit_status( 0 );
end hash_good_test;

-- VIM editor formatting instructions
-- vim: ft=spar

