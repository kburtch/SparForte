-- Test program to test bdb library

with ada.text_io,
     ada.strings.unbounded,
     bdb,
     bdb_constants;
use  ada.text_io,
     ada.strings.unbounded,
     bdb,
     bdb_constants;

procedure db is

  session : berkeley_session;
  cursor  : berkeley_cursor;
  cursor2 : berkeley_cursor;
  env     : berkeley_environment;
  s : unbounded_string;
  s2 : unbounded_string;
  rec_cnt : natural;

  less, equal, greater : long_float := 99.0;

begin
  new_berkeley_session( session, standalone_environment, 10, 10 );
  raise_exceptions( session );
  --set_create_dir( session, "/tmp" );
  --set_pagesize( session, 512 );
  --set_cachesize( session, 0, 512, 0 );
  --set_bt_minkey( session, 2 );

  -- B-Tree

  put_line( "B-Tree Test" );
  create( session, "nbs", "", DB_BTREE, 0, 0 ); -- TODO move type
  close( session );
  open( session, "nbs", "", DB_BTREE, 0, 0 );
  put( session, "foo", "bar" );
  key_range( session, "key", less, equal, greater, 0 );
  sync( session );
  exists( session, "foo" );
  get( session, "foo", s );
  put_line( "foo = " & to_string( s ) );
  delete( session, "foo" );
  truncate( session, rec_cnt );
put_line( "truncate count =" & rec_cnt'img );
  begin
    get( session, "foo", s );  
  exception when berkeley_error =>
    null;
  end;
  close( session );
  rename( session, "foobar" );
  remove( session );

  -- In-memory B-Tree and Cursors test

  put_line( "In-memory B-Tree Test" );
  create( session, "", "test", DB_BTREE, 0, 0 ); -- TODO move type
  put( session, "foo", "bar" );
  exists( session, "foo" );
  get( session, "foo", s );
  put_line( "foo = " & to_string( s ) );

  new_berkeley_cursor( session, cursor, 0 );
  dup( session, cursor, cursor2 );
  close( session, cursor2 );
  get( session, cursor, s, s2, DB_C_GET_FIRST );
  put_line( "cursor foo / bar = " & to_string( s ) & '/' & to_string( s2 ) );
  close( session, cursor );

  new_berkeley_cursor( session, cursor, 0 );
  get( session, cursor, s, s2, DB_C_GET_FIRST );
  put( session, cursor, "foo", "not bar" );
  get( session, cursor, s, s2, DB_C_GET_FIRST );
  put_line( "cursor foo / not bar = " & to_string( s ) & '/' & to_string( s2 ) );
  bdb.count( session, cursor, rec_cnt );
  put_line( "cursor foo count / 1 =" & rec_cnt'img );
  put( session, "foo2", "bar2");
  delete( session, cursor );
  get( session, cursor, s, s2, DB_C_GET_FIRST );
  put_line( "cursor foo / not bar = " & to_string( s ) & '/' & to_string( s2 ) );
  close( session, cursor );

-- procedure dbc_c_get( session : in out berkeley_session; cursor : in out berkeley_cursor; 
--  key, data : out unbounded_string; flags : c_get_flags := 0 );
  close( session );

  -- Hash table test

  put_line( "Hash Test" );
  create( session, "nbs", "", DB_HASH, 0, 0 ); -- TODO move type
  close( session );
  open( session, "nbs", "", DB_HASH, 0, 0 );
  put( session, "apple", "red" );
  put( session, "blueberry", "blue" );
  put( session, "banana", "yellow" );
  exists( session, "banana" );
  close( session );
  remove( session );

  -- Recno test

  put_line( "Recno Test" );
  Init( session );
  -- if we want fixed-length arrays of character, padded like Ada fixed
  -- strings, use set_re_len / set_re_pad
  --set_re_len( session, 10 );
  --set_re_source( session, "fake.txt" );
  create( session, "nbs", "", DB_RECNO, 0, 0 );
  close( session );
  open( session, "nbs", "", DB_RECNO, 0, 0 );
  put( session, 1, "red", 0 );
  put( session, 2, "blue", 0 );
  put( session, 3, "yellow", 0 );
  get( session, 2, s );
  put_line( "get = '" & to_string( s ) & "'");

  close( session );
  remove( session );

  -- Queue test

  put_line( "Queue Test" );
  Init( session );
  set_re_len( session, 10 );
  create( session, "nbs", "", DB_QUEUE, 0, 0 ); -- TODO move type
  close( session );
  open( session, "nbs", "", DB_QUEUE, 0, 0 );
  put( session, 1, "red", 0 );
  put( session, 2, "blue", 0 );
  put( session, 3, "yellow", 0 );
  get( session, 1, s );
  put_line( "get = " & to_string( s ) );

  close( session );
  remove( session );

  -- Environment Test

put_line( "commit test" );
  Init( session );
  Init( env );
  set_data_dir( env, "/tmp" );
  set_tmp_dir( env, "/tmp" );
  create( env, "/tmp", 
   DB_E_OPEN_INIT_LOCK OR
   DB_E_OPEN_INIT_LOG OR
   DB_E_OPEN_INIT_MPOOL OR
   DB_E_OPEN_INIT_TXN,
   0
  );
put_line("a1" );
  create( session, "nbs", "", DB_HASH, 0, 0 );
put_line("a2" );
--  simple_txn_begin( session, env );
--  simple_txn_commit( session, env );
--  simple_txn_begin( session, env );
--  simple_txn_abort( session, env );
  close( session );
put_line("a3" );
  dbremove( env, "nbs", "", 0 );
put_line( "done" );
  close( env );
put_line( "d1" );
  init( env );
put_line( "d2" );
  remove( env, "/home/ken/ada/berkeley" );
put_line( "d3" );

--return;

  Init( session );
  Init( env, 0 );
  open( env, "/home/ken/ada/berkeley", DB_E_OPEN_CREATE, 0 );
  close( env );

  --Init( session );
  --Init( env, 0 );
  --remove( env, "/tmp", 0 );

  Init( session );
  Init( env );
  create( env, "/home/ken/ada/berkeley", 0, 0 );
  create( session, "nbs", "", DB_HASH, 0, 0 );
  close( session );
  dbremove( env, "nbs", "", 0 );
  close( env );

  Init( session );
  Init( env );
  set_data_dir( env, "/tmp" );
  set_encrypt( env, "foobar" );
  set_flags( env, DB_E_SET_AUTO_COMMIT );
  set_shm_key( env, 9999 );
  set_timeout( env, 100, DB_E_SET_LOCK_TIMEOUT );
  set_tmp_dir( env, "/tmp" );
  set_verbose( env, DB_VERB_DEADLOCK );
  set_lk_max_lockers( env, 100 );
  set_lk_max_locks( env, 100 );
  set_lk_max_objects( env, 100 );
  set_lk_detect( env, DB_LOCK_DEFAULT );
  create( env, "/home/ken/ada/berkeley", 0, 0 );
  create( session, "nbs", "", DB_HASH, 0, 0 );
  close( session );
  dbrename( env, "nbs", "", "nbs2", 0 );
  dbremove( env, "nbs2", "", 0 );
  close( env );
  Init( env );
  remove( env, "/home/ken/ada/berkeley" );

  put_line( to_string( version ) );

end db;
