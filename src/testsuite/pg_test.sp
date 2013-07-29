procedure pg_test is

  user : string;
  pass : string;
  dbname : string;

  -- sanity check of db (postgresql) package

  --C : dbm.connection;
  --Q : dbm.query;
  b : boolean;
  e : db.database_type;
  s : string;
  --t : dbm.tuple_count_type;
  --i : dbm.tuple_index_type;
  --c : dbm.column_index_type;
  t1: db.tuple_count_type;
  i1: db.tuple_index_type;
  c1: db.column_index_type;
  n : natural;

begin
  put_line( "Good tests for MySQL" );
  new_line;
  put_line( "This requires an empty database" );
  put_line( "Login and database info:" );
  new_line;
  put( "Db username? " );
  user := get_line;
  put( "Db password? " );
  pass := get_line;
  put( "Db name? " );
  dbname := get_line;

--  dbm.new_query( Q );
--  dbm.new_connection( C );
--
--  -- TODO: drop no db on restricted shell?  db has its own security
--
--  dbm.connect( C, dbname, user, pass );
--  dbm.disconnect( C );
--  b := dbm.is_connected( C );
--
--  dbm.connect( C, dbname, user, pass );
--  dbm.disconnect( C );
--  b := dbm.is_connected( C );
--
--  e := dbm.engine_of( C );
--
--  dbm.connect( C, dbname, user, pass );
--  b := dbm.is_connected( C );
--  dbm.disconnect( C );
--
--  dbm.connect( C, dbname, user, pass );
--  dbm.prepare( Q, "select 1" );
--  dbm.clear( Q );
--  dbm.append( Q, "select 1" );
--  dbm.clear( Q );
--  dbm.append_line( Q, "select " );
--  dbm.append_quoted( Q, C, "foobar" );
--  dbm.clear( Q );
--  dbm.append_line( Q, "select " );
--  dbm.append_quoted( Q, C, "foobar", "dummy" );
--  dbm.clear( Q );
--  dbm.append( Q, "select 1" );
--  dbm.execute( Q, C );
--  dbm.execute_checked( Q, C, "a message" );
--
--  s := dbm.error_message( C ); -- s should be blank
--  dbm.reset( C );
--  b := dbm.in_abort_state( C );
--  s := dbm.options( C ); -- s should be blank
--  dbm.set_rollback_on_finalize( C, true );
--  b := dbm.will_rollback_on_finalize( C );
--  dbm.disconnect( C );
--
--  dbm.connect( C, dbname, user, pass );
--  dbm.open_db_trace( C, "/tmp/trace.txt" );
--  dbm.set_trace( C, true );
--  b := dbm.is_trace( C );
--  dbm.close_db_trace( C );
--  dbm.raise_exceptions( Q, true );
--  dbm.report_errors( Q, true );
--  dbm.disconnect( C );
--
--  dbm.connect( C, dbname, user, pass );
--  dbm.begin_work( Q, C );
--  dbm.rollback_work( Q, C );
--  dbm.begin_work( Q, C );
--  dbm.commit_work( Q, C );
--  dbm.prepare( Q, "select 1+1 as total" );
--  dbm.execute( Q, C );
--  dbm.rewind( Q );
--  dbm.fetch( Q );
--  t := dbm.tuples( Q );
--  pragma assert( t = 1 );
--  b := dbm.end_of_query( Q );
--  pragma assert( b = true );
--  i := dbm.tuple( Q );
--  pragma assert( i = 1 );
--  n := dbm.columns( Q );
--  pragma assert( n = 1 );
--  s := dbm.column_name( Q, 1 );
--  pragma assert( s = "total" );
--  c := dbm.column_index( Q, "total" );
--  pragma assert( c = 1 );
--  b := dbm.is_null( Q, 1 );
--  pragma assert( b = false );
--  n := dbm.value( Q, 1 );
--  pragma assert( n = 2 );
--
--  -- user commands
--  dbm.databases( C );
--
--  -- must add a table in order to see the schema
--  dbm.prepare( Q, "create table foobar ( i integer not null )" );
--  dbm.execute( Q, C );
--  dbm.schema( C, "foobar" );
--
--  dbm.list( C );
--
--  dbm.prepare( Q, "drop table foobar" );
--  dbm.execute( Q, C );
--
--  dbm.prepare( Q, "select 1+1 as total" );
--  dbm.execute( Q, C );
--  dbm.show( Q, C ); -- exception thrown
--
--  dbm.prepare( Q, "create table foobar ( b integer not null, i integer not null, s varchar(256) not null )" );
--  dbm.execute( Q, C );
--
--  dbm.prepare( Q, "insert into foobar (b, i, s) values (0, 15, " & ASCII.Quotation & "hello" & ASCII.Quotation & ")" );
--  dbm.execute( Q, C );
--
--  declare
--     type foobar_row is record
--         b : boolean;
--         i : integer;
--         s : string;
--     end record;
--     r : foobar_row;
--  begin
--     dbm.prepare( Q, "select b, i, s from foobar" );
--     dbm.execute( Q, C );
--     dbm.fetch_values( Q, C, r );
--
--     pragma assert( r.b = false );
--     pragma assert( r.i = 15 );
--     pragma assert( r.s = "hello" );
--  end;
--  dbm.disconnect( C );

  -- REGULAR POSTGRESQL

  db.connect( dbname, user, pass );
  db.disconnect;
  b := db.is_connected;

  db.connect( dbname, user, pass );
  db.disconnect;
  b := db.is_connected;

  e := db.engine_of;

  db.connect( dbname, user, pass );
  b := db.is_connected;
  db.disconnect;

  db.connect( dbname, user, pass );
  db.prepare( "select 1" );
  db.clear;
  db.append( "select 1" );
  db.clear;
  db.append_line( "select " );
  db.append_quoted( "foobar" );
  db.clear;
  db.append_line( "select " );
  db.append_quoted( "foobar", "dummy" );
  db.clear;
  db.append( "select 1" );
  db.execute;
  db.execute_checked( "a message" );

  s := db.error_message; -- s should be blank
  -- db.reset;
  -- semantics of reset a bit wonky.  shouldn't use
  -- TODO: db.reset breaks db connection
  b := db.in_abort_state;
  s := db.options; -- s should be blank
  db.set_rollback_on_finalize( true );
  b := db.will_rollback_on_finalize;
  db.disconnect;

  db.connect( dbname, user, pass );
  db.open_db_trace( "/tmp/trace.txt" );
  db.set_trace( true );
  b := db.is_trace;
  db.close_db_trace;
  db.raise_exceptions( true );
  db.report_errors( true );
  db.disconnect;

  db.connect( dbname, user, pass );
  --db.set_trace( false ); -- DEBUG
  db.begin_work; -- throws an exception
  db.rollback_work;
  db.begin_work;
  db.commit_work;
  db.prepare( "select 1+1 as total" );
  db.execute;
  db.rewind;
  db.fetch;
  t1 := db.tuples;
  pragma assert( t1 = 1 );
  b := db.end_of_query;
  pragma assert( b = true );
  i1 := db.tuple;
  pragma assert( i1 = 1 );
  n := db.columns;
  pragma assert( n = 1 );
  s := db.column_name( 1 );
  pragma assert( s = "total" );
  c1 := db.column_index( "total" );
  pragma assert( c1 = 1 );
  b := db.is_null( 1 );
  pragma assert( b = false );
  n := db.value( 1 );
  pragma assert( n = 2 );

  -- user commands
  db.databases;

  --db.prepare( "drop table foobar" );
  --db.execute;

  -- must add a table in order to see the schema
  db.prepare( "create table foobar ( i integer not null )" );
  db.execute;
  db.schema( "foobar" );

  db.list;

  db.prepare( "drop table foobar" );
  db.execute;

  db.prepare( "select 1+1 as total" );
  db.execute;
  db.show; -- exception thrown

end pg_test;

