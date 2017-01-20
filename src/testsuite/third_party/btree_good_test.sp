#!/usr/local/bin/spar

pragma annotate( summary, "Run good tests of btree_io library" )
              @( description, "Runs a series of typical operations on the" )
              @( description, "btree_io library to ensure these operations" )
              @( description, "are not broken." )
              @( created, "January 14, 2017" )
              @( author, "Ken O. Burtch" );

procedure btree_good_test is

type person_type is record
   first_name : string;
   age  : natural;
end record;

person : person_type;

f : btree_io.file;
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

btree_io.new_file( f, person_type );
pragma assert( not btree_io.is_open( f ) );

btree_io.create( f, "btree_test/person.btree", 80, 80 );
pragma assert( btree_io.is_open( f ) );

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
  c : btree_io.cursor;
  key : string;
begin
  btree_io.new_cursor( c, person_type );
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

btree_io.close( f );
pragma assert( not btree_io.is_open( f ) );

btree_io.delete( f );
pragma assert( not btree_io.is_open( f ) );

btree_io.create( f, "btree_test/person.btree", 80, 80 );
btree_io.delete( f );
pragma assert( not btree_io.is_open( f ) );

command_line.set_exit_status( 0 );
end btree_good_test;

-- VIM editor formatting instructions
-- vim: ft=spar

