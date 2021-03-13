procedure mysql_test is

  user : string;
  pass : string;
  dbname : string;

  -- sanity check of mysql package

  C : mysqlm.connection;
  Q : mysqlm.query;
  b : boolean;
  e : db.database_type;
  s : string;
  t : mysqlm.tuple_count_type;
  i : mysqlm.tuple_index_type;
  c : mysqlm.column_index_type;
  t1: mysql.tuple_count_type;
  i1: mysql.tuple_index_type;
  c1: mysql.column_index_type;
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

  mysqlm.new_query( Q );
  mysqlm.new_connection( C );

  -- TODO: drop no mysql on restricted shell?  mysql has its own security

  mysqlm.connect( C, dbname, user, pass, "localhost" );
  mysqlm.disconnect( C );
  b := mysqlm.is_connected( C );

  mysqlm.connect( C, dbname, user, pass, "localhost" );
  mysqlm.disconnect( C );
  b := mysqlm.is_connected( C );

  e := mysqlm.engine_of( C );

  mysqlm.connect( C, dbname, user, pass, "localhost" );
  b := mysqlm.is_connected( C );
  mysqlm.disconnect( C );

  mysqlm.connect( C, dbname, user, pass, "localhost" );
  mysqlm.prepare( Q, "select 1" );
  mysqlm.clear( Q );
  mysqlm.append( Q, "select 1" );
  mysqlm.clear( Q );
  mysqlm.append_line( Q, "select " );
  mysqlm.append_quoted( Q, C, "foobar" );
  mysqlm.clear( Q );
  mysqlm.append_line( Q, "select " );
  mysqlm.append_quoted( Q, C, "foobar", "dummy" );
  mysqlm.clear( Q );
  mysqlm.append( Q, "select 1" );
  mysqlm.execute( Q, C );
  mysqlm.execute_checked( Q, C, "a message" );

  s := mysqlm.error_message( C ); -- s should be blank
  -- reset has weird semantics.  use disconnect instead.
  -- mysqlm.reset( C );
  b := mysqlm.in_abort_state( C );
  s := mysqlm.options( C ); -- s should be blank
  mysqlm.set_rollback_on_finalize( C, true );
  b := mysqlm.will_rollback_on_finalize( C );
  mysqlm.disconnect( C );

  mysqlm.connect( C, dbname, user, pass, "localhost" );
  mysqlm.open_db_trace( C, "/tmp/trace.txt" );
  mysqlm.set_trace( C, true );
  b := mysqlm.is_trace( C );
  mysqlm.close_db_trace( C );
  mysqlm.raise_exceptions( Q, true );
  mysqlm.report_errors( Q, true );
  mysqlm.disconnect( C );

  mysqlm.connect( C, dbname, user, pass, "localhost" );
  mysqlm.begin_work( Q, C );
  mysqlm.rollback_work( Q, C );
  mysqlm.begin_work( Q, C );
  mysqlm.commit_work( Q, C );
  mysqlm.prepare( Q, "select 1+1 as total" );
  mysqlm.execute( Q, C );
  mysqlm.rewind( Q );
  mysqlm.fetch( Q );
  t := mysqlm.tuples( Q );
  pragma assert( t = 1 );
  b := mysqlm.end_of_query( Q );
  pragma assert( b = true );
  i := mysqlm.tuple( Q );
  pragma assert( i = 1 );
  n := mysqlm.columns( Q );
  pragma assert( n = 1 );
  s := mysqlm.column_name( Q, 1 );
  pragma assert( s = "total" );
  c := mysqlm.column_index( Q, "total" );
  pragma assert( c = 1 );
  b := mysqlm.is_null( Q, 1 );
  pragma assert( b = false );
  n := mysqlm.value( Q, 1 );
  pragma assert( n = 2 );

  -- user commands
  mysqlm.databases( C );

  -- must add a table in order to see the schema
  mysqlm.prepare( Q, "create table foobar ( i integer not null )" );
  mysqlm.execute( Q, C );
  mysqlm.schema( C, "foobar" );

  mysqlm.list( C );

  mysqlm.prepare( Q, "drop table foobar" );
  mysqlm.execute( Q, C );

  mysqlm.prepare( Q, "select 1+1 as total" );
  mysqlm.execute( Q, C );
  mysqlm.show( Q, C ); -- exception thrown

  mysqlm.prepare( Q, "create table foobar ( b integer not null, i integer not null, s varchar(256) not null )" );
  mysqlm.execute( Q, C );

  mysqlm.prepare( Q, "insert into foobar (b, i, s) values (0, 15, " & ASCII.Quotation & "hello" & ASCII.Quotation & ")" );
  mysqlm.execute( Q, C );

  declare
     type foobar_row is record
         b : boolean;
         i : integer;
         s : string;
     end record;
     r : foobar_row;
  begin
     mysqlm.prepare( Q, "select b, i, s from foobar" );
     mysqlm.execute( Q, C );
     mysqlm.fetch_values( Q, C, r );

     pragma assert( r.b = false );
     pragma assert( r.i = 15 );
     pragma assert( r.s = "hello" );

     r.i := 7;
     r.s := "cart";
     r.b := true;

     mysqlm.prepare( Q, "UPDATE foobar" );
     mysqlm.append_for_update( Q, C, r );
     mysqlm.append( Q, "WHERE i = 15" );
     mysqlm.execute( Q, C );

     mysqlm.prepare( Q, "select b, i, s from foobar" );
     mysqlm.execute( Q, C );
     mysqlm.fetch_values( Q, C, r );

     pragma assert( r.b = true );
     pragma assert( r.i = 7 );
     pragma assert( r.s = "cart" );

     r.i := 2;
     r.s := "horse";
     r.b := true;

     mysqlm.prepare( Q, "INSERT INTO foobar" );
     mysqlm.append_for_insert( Q, C, r );
     mysqlm.execute( Q, C );

     mysqlm.prepare( Q, "select b, i, s from foobar order by i" );
     mysqlm.execute( Q, C );
     mysqlm.fetch_values( Q, C, r );

     pragma assert( r.b = true );
     pragma assert( r.i = 2 );
     pragma assert( r.s = "horse" );

  end;
  mysqlm.disconnect( C );

  -- REGULAR MYSQL

  mysql.connect( dbname, user, pass, "localhost", 3306 );
  mysql.disconnect;
  b := mysql.is_connected;

  mysql.connect( dbname, user, pass, "localhost", 3306 );
  mysql.disconnect;
  b := mysql.is_connected;

  e := mysql.engine_of;

  mysql.connect( dbname, user, pass, "localhost", 3306 );
  b := mysql.is_connected;
  mysql.disconnect;

  mysql.connect( dbname, user, pass, "localhost" );
  mysql.prepare( "select 1" );
  mysql.clear;
  mysql.append( "select 1" );
  mysql.clear;
  mysql.append_line( "select " );
  mysql.append_quoted( "foobar" );
  mysql.clear;
  mysql.append_line( "select " );
  mysql.append_quoted( "foobar", "dummy" );
  mysql.clear;
  mysql.append( "select 1" );
  mysql.execute;
  mysql.execute_checked( "a message" );

  s := mysql.error_message; -- s should be blank
  mysql.reset;
  b := mysql.in_abort_state;
  s := mysql.options; -- s should be blank
  mysql.set_rollback_on_finalize( true );
  b := mysql.will_rollback_on_finalize;
  mysql.disconnect;

  mysql.connect( dbname, user, pass, "localhost" );
  mysql.open_db_trace( "/tmp/trace.txt" );
  mysql.set_trace( true );
  b := mysql.is_trace;
  mysql.close_db_trace;
  mysql.raise_exceptions( true );
  mysql.report_errors( true );
  mysql.disconnect;

  mysql.connect( dbname, user, pass, "localhost" );
  --mysql.set_trace( false ); -- DEBUG
  mysql.begin_work; -- throws an exception
  mysql.rollback_work;
  mysql.begin_work;
  mysql.commit_work;
  mysql.prepare( "select 1+1 as total" );
  mysql.execute;
  mysql.rewind;
  mysql.fetch;
  t1 := mysql.tuples;
  pragma assert( t1 = 1 );
  b := mysql.end_of_query;
  pragma assert( b = true );
  i1 := mysql.tuple;
  pragma assert( i1 = 1 );
  n := mysql.columns;
  pragma assert( n = 1 );
  s := mysql.column_name( 1 );
  pragma assert( s = "total" );
  c1 := mysql.column_index( "total" );
  pragma assert( c1 = 1 );
  b := mysql.is_null( 1 );
  pragma assert( b = false );
  n := mysql.value( 1 );
  pragma assert( n = 2 );

  -- user commands
  mysql.databases;

  mysql.prepare( "drop table foobar" );
  mysql.execute;

  -- must add a table in order to see the schema
  mysql.prepare( "create table foobar ( i integer not null )" );
  mysql.execute;
  mysql.schema( "foobar" );

  mysql.list;

  mysql.prepare( "drop table foobar" );
  mysql.execute;

  mysql.prepare( "select 1+1 as total" );
  mysql.execute;
  mysql.show; -- exception thrown

end mysql_test;

