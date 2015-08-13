with system.address_to_access_conversions,
     unchecked_deallocation,
     interfaces.c,
     gnat.source_info,
     ada.text_io,
     ada.strings.unbounded;
use  interfaces.c,
     ada.text_io,
     ada.strings.unbounded;

package body bdb is

------------------------------------------------------------------------------
--
-- Char Array
--
------------------------------------------------------------------------------

-- TO UNBOUNDED STRING
--
-- Like Interfaces.C.To_Ada except doesn't use ASCII.NUL as end of string
------------------------------------------------------------------------------

function to_unbounded_string( buffer : char_array; buffer_len : size_t ) return unbounded_string is
  i : interfaces.C.size_t := 0;
  s : unbounded_string;
begin
    while i < buffer_len loop
      s := s & Interfaces.C.To_Ada( buffer(i) );
      i := i + 1;
    end loop;
  return s;
end to_unbounded_string;

-- TO CHAR ARRAY
--
-- Like Interfaces.C.To_C except doesn't use ASCII.NUL as end of string
------------------------------------------------------------------------------

procedure to_char_array( s : string; buffer : in out char_array; buffer_len : out size_t ) is
  i : interfaces.C.size_t := 0;
begin
  buffer_len := interfaces.C.size_t( s'length );
  while i < buffer_len loop
    buffer( i ) := Interfaces.C.To_C( s( integer(i)+1 ) );
    i := i + 1;
  end loop;
end to_char_array;

------------------------------------------------------------------------------
--
-- Berkeley DB Interface
--
------------------------------------------------------------------------------

  procedure show_err( err : db_error ) is
  begin
     if err /= DB_OK then
        put_line( "Error: " & err'img );
     end if;
  end show_err;

------------------------------------------------------------------------------
-- C interface
------------------------------------------------------------------------------

  procedure C_db_create( err : in out db_error; dbh : in out db_handle; dbe : db_environment );
  pragma import( C, C_db_create, "C_db_create" );

  procedure C_db_open( err : in out db_error; dbh : db_handle; txn : db_transaction;
     fname : string; dbname : string;
     kind : db_type; flags : open_flags; mode : interfaces.C.int  );
  pragma import( C, C_db_open, "C_db_open" );

  procedure C_db_close( err : in out db_error; dbh : db_handle );
  pragma import( C, C_db_close, "C_db_close" );

  procedure C_db_remove( err : in out db_error; dbh : db_handle; fname : string; dbname : string; unused : flags );
  pragma import( C, C_db_remove, "C_db_remove" );

  procedure C_db_rename( err : in out db_error; dbh : db_handle; fname : string; dbname : string; new_dbname : string; unused : flags );
  pragma import( C, C_db_rename, "C_db_rename" );

  procedure C_db_put( err : in out db_error;
                      dbh : db_handle;
                      txn : db_transaction;
                      key : system.address;
                      key_length : interfaces.C.size_t;
                      data : system.address;
                      data_length : interfaces.C.size_t;
                      flags : put_flags );
  pragma import( C, C_db_put, "C_db_put" );

  procedure C_db_put_recno( err : in out db_error;
                      dbh : db_handle;
                      txn : db_transaction;
                      recno : db_recno_t;
                      data : system.address;
                      data_length : interfaces.C.size_t;
                      flags : put_flags );
  pragma import( C, C_db_put_recno, "C_db_put_recno" );

  procedure C_db_get( err : in out db_error;
                      dbh    : db_handle;
                      txn    : db_transaction;
                      key    : system.address;
                      key_length : interfaces.C.size_t;
                      data   : system.address;
                      data_length : in out interfaces.C.size_t;
                      unused : get_flags );
  pragma import( C, C_db_get, "C_db_get" );

  procedure C_db_get_recno( err : in out db_error;
                      dbh    : db_handle;
                      txn    : db_transaction;
                      recno  : db_recno_t;
                      data   : system.address;
                      data_length : in out interfaces.C.size_t;
                      unused : get_flags );
  pragma import( C, C_db_get_recno, "C_db_get_recno" );

  procedure C_db_del( err : in out db_error;
                      dbh : db_handle;
                      txn : db_transaction;
                      key : system.address;
                      key_length : interfaces.C.size_t;
                      unused : delete_flags );
  pragma import( C, C_db_del, "C_db_del" );

  procedure C_db_exists( err : in out db_error;
                      dbh : db_handle;
                      txn : db_transaction;
                      key : system.address;
                      key_length : interfaces.C.size_t;
                      unused : exists_flags );
  pragma import( C, C_db_exists, "C_db_exists" );

  procedure C_db_strerror( error_number : db_error; buffer : system.address; buffer_length : interfaces.C.size_t );
  pragma import( C, C_db_strerror, "C_db_strerror");

  procedure C_set_pagesize( err : in out db_error; dbh : db_handle; pagesize : aPageSize );
  pragma import( C, C_set_pagesize, "C_set_pagesize" );

  procedure C_db_truncate( err : in out db_error;
    dbh : db_handle;
    txn : db_transaction;
    countp : in out u_int32_t;
    flags  : u_int32_t );
  pragma import( C, C_db_truncate, "C_db_truncate" );

  procedure C_db_sync( err : in out db_error; dbh : db_handle; flags : sync_flags );
  pragma import( C, C_db_sync, "C_db_sync" );

  procedure C_db_set_re_len( db_err : in out db_error; dbh : db_handle; len : u_int32_t );
  pragma import( C, C_db_set_re_len, "C_db_set_re_len" );

  procedure C_db_set_re_pad( db_err : in out db_error; dbh : db_handle; len : int );
  pragma import( C, C_db_set_re_pad, "C_db_set_re_pad" );

  procedure C_db_set_re_delim( db_err : in out db_error; dbh : db_handle; delim : int );
  pragma import( C, C_db_set_re_delim, "C_db_set_re_delim" );

  procedure C_db_set_re_source( db_err : in out db_error; dbh : db_handle; source : system.address );
  pragma import( C, C_db_set_re_source, "C_db_set_re_source" );

  procedure C_db_set_flags( db_err : in out db_error; dbh : db_handle; f : config_flags );
  pragma import( C, C_db_set_flags, "C_db_set_flags" );

  procedure C_db_set_create_dir( db_err : in out db_error; dbh : db_handle; dirpath : system.address );
  pragma import( C, C_db_set_create_dir, "C_db_set_create_dir" );

  procedure C_db_set_cachesize( db_err : in out db_error; dbh : db_handle;
    gbytes, bytes : u_int32_t; ncache : Interfaces.C.int );
  pragma import( C, C_db_set_cachesize, "C_db_set_cachesize" );

  procedure C_db_bt_minkey( db_err : in out db_error; dbh : db_handle;
     bt_minkey : u_int32_t );
  pragma import( C, C_db_bt_minkey, "C_db_bt_minkey"  );

  procedure C_key_range( err : in out db_error;
    dbh : db_handle;
    txn : db_transaction;
    key : system.address;
    key_length : interfaces.C.size_t;
    less, equal, greater : system.address;
    flags : key_range_flags );
  pragma import( C, C_key_range, "C_key_range" );

procedure C_db_cursor( db_err : in out db_error; dbh : db_handle;
  txn : db_transaction; cursor : in out db_cursor; flags : cursor_flags );
pragma import( C, C_db_cursor, "C_db_cursor" );

  procedure C_dbc_c_get( err : in out db_error;
                      dbc    : db_cursor;
                      key    : system.address;
                      key_length : in out interfaces.C.size_t;
                      data   : system.address;
                      data_length : in out interfaces.C.size_t;
                      flags : c_get_flags );
  pragma import( C, C_dbc_c_get, "C_dbc_c_get" );

  procedure C_dbc_c_close( err : in out db_error; cursor : db_cursor );
  pragma import( C, C_dbc_c_close, "C_dbc_c_close" );

  procedure C_dbc_c_put( err : in out db_error;
                      dbc    : db_cursor;
                      key    : system.address;
                      key_length : interfaces.C.size_t;
                      data   : system.address;
                      data_length : interfaces.C.size_t;
                      flags : c_put_flags );
  pragma import( C, C_dbc_c_put, "C_dbc_c_put" );

  procedure C_dbc_c_count( err : in out db_error;
     cursor : db_cursor;
     countp : in out db_recno_t;
     unused : flags := 0 );
  pragma import( C, C_dbc_c_count, "C_dbc_c_count" );

  procedure C_dbc_c_del( err : in out db_error;
     cursor : db_cursor;
     unused : flags := 0 );
  pragma import( C, C_dbc_c_del, "C_dbc_c_del" );

  procedure C_dbc_c_dup( err : in out db_error;
     cursor  : db_cursor;
     cursor2 : in out db_cursor;
     flags  : c_dup_flags := 0 );
  pragma import( C, C_dbc_c_dup, "C_dbc_c_dup" );

  procedure C_db_env_create( err : in out db_error;
   env : in out db_environment;
   flags : env_flags := 0 );
  pragma import( C, C_db_env_create, "C_db_env_create" );

  procedure C_dbe_open( err : in out db_error;
   env : db_environment;
   db_home : string;
   flags : e_open_flags := 0;
   mode : interfaces.C.int );
  pragma import( C, C_dbe_open, "C_dbe_open" );

  procedure C_dbe_close( err : in out db_error;
   env : db_environment;
   unused : flags := 0 );
  pragma import( C, C_dbe_close, "C_dbe_close" );

  procedure C_dbe_remove( err : in out db_error;
   env : db_environment;
   dbhome : string;
   flags : env_flags := 0 );
  pragma import( C, C_dbe_remove, "C_dbe_remove" );

  procedure C_dbe_dbremove( err : in out db_error;
   dbe : db_environment;
   txnid : db_transaction;
   fname : string;
   dbname : string;
   flags : e_dbremove_flags := 0 );
  pragma import( C, C_dbe_dbremove, "C_dbe_dbremove" );

  procedure C_dbe_dbrename( err : in out db_error;
   dbe : db_environment;
   txnid : db_transaction;
   fname : string;
   dbname : string;
   fname2 : string;
   flags : e_dbrename_flags := 0 );
  pragma import( C, C_dbe_dbrename, "C_dbe_dbrename" );

  procedure C_db_version( buffer : system.address; buffer_length : interfaces.C.size_t );
  pragma import( C, C_db_version, "C_db_version" );

  procedure C_dbe_set_data_dir( err : in out db_error;
   dbe : db_environment;
   dir : string );
  pragma import( C, C_dbe_set_data_dir, "C_dbe_set_data_dir" );

  procedure C_dbe_set_encrypt( err : in out db_error;
   dbe : db_environment;
   passwd : string;
   flags : e_encrypt_flags := 0 );
  pragma import( C, C_dbe_set_encrypt, "C_dbe_set_encrypt" );

  procedure C_dbe_set_flags( err : in out db_error;
   dbe : db_environment;
   flags : e_set_flags := 0;
   onoff : interfaces.C.int := 0 );
  pragma import( C, C_dbe_set_flags, "C_dbe_set_flags" );

  procedure C_dbe_set_shm_key( err : in out db_error;
   dbe : db_environment;
   shm_key : Interfaces.C.long );
  pragma import( C, C_dbe_set_shm_key, "C_dbe_set_shm_key" );

  --procedure C_dbe_set_tas_spins( err : in out db_error;
  -- dbe : db_environment;
  -- tas_spins : u_int32_t );
  --pragma import( C, C_dbe_set_tas_spins, "C_dbe_set_tas_spins" );

  procedure C_dbe_set_timeout( err : in out db_error;
   dbe : db_environment;
   timeout : Interfaces.C.long;
   flags : e_set_timeout_flags := 0 );
 pragma import( C, C_dbe_set_timeout, "C_dbe_set_timeout" );

  procedure C_dbe_set_tmp_dir( err : in out db_error;
   dbe : db_environment;
   dir : string );
  pragma import( C, C_dbe_set_tmp_dir, "C_dbe_set_tmp_dir" );

  procedure C_dbe_set_verbose( err : in out db_error;
   dbe : db_environment;
   flags : e_verbose_flags := 0;
   onoff : interfaces.C.int := 0 );
  pragma import( C, C_dbe_set_verbose, "C_dbe_set_verbose" );

-- Locking

  procedure C_dbe_set_lk_max_lockers( err : in out db_error;
   dbe : db_environment;
   max : u_int32_t := 0 );
  pragma import( C, C_dbe_set_lk_max_lockers, "C_dbe_set_lk_max_lockers" );

  procedure C_dbe_set_lk_max_locks( err : in out db_error;
   dbe : db_environment;
   max : u_int32_t := 0 );
  pragma import( C, C_dbe_set_lk_max_locks, "C_dbe_set_lk_max_locks" );

  procedure C_dbe_set_lk_max_objects( err : in out db_error;
   dbe : db_environment;
   max : u_int32_t := 0 );
  pragma import( C, C_dbe_set_lk_max_objects, "C_dbe_set_lk_max_objects" );

  procedure C_dbe_set_lk_detect( err : in out db_error;
   dbe : db_environment;
   detect : deadlock_detection_modes := 0 );
  pragma import( C, C_dbe_set_lk_detect, "C_dbe_set_lk_detect" );

  procedure C_dbe_txn_begin( err : in out db_error;
   dbe : db_environment;
   parent : db_transaction;
   tid : in out db_transaction;
   flags : e_begin_flags );
  pragma import( C, C_dbe_txn_begin, "C_dbe_txn_begin" );

  procedure C_txn_abort( err : in out db_error;
    tid : db_transaction );
  pragma import( C, C_txn_abort, "C_txn_abort" );

  procedure C_txn_commit( err : in out db_error;
    tid : db_transaction;
   flags : e_commit_flags );
  pragma import( C, C_txn_commit, "C_txn_commit" );


------------------------------------------------------------------------------
--
-- The Interface
--
------------------------------------------------------------------------------


-- RAISE ERROR
--
-- Raise the last error as an exception.  Include the location on that option
-- is enabled in the session.
------------------------------------------------------------------------------

procedure raise_error( location : string; session : berkeley_session ) is
   buffer : char_array_ptr;
   msg : unbounded_string;
begin
   if session.error_location then
      msg := msg & location & ": ";
   end if;
   buffer := new char_array(0..255);
   C_db_strerror( session.err, char_array_ptr_Conv.to_address( buffer ), size_t( buffer'length ) );
   for i in buffer'range loop
        exit when To_Ada( buffer(i) ) = ASCII.NUL;
        msg := msg & To_Ada( buffer(i) );
    end loop;
    free( buffer );
    msg := msg & " (error" & session.err'img & ")";
    raise berkeley_error with to_string( msg );
end raise_error;

procedure raise_error( location : string; env : berkeley_environment ) is
   buffer : char_array_ptr;
   msg : unbounded_string;
begin
   if env.error_location then
      msg := msg & location & ": ";
   end if;
   buffer := new char_array(0..255);
   C_db_strerror( env.err, char_array_ptr_Conv.to_address( buffer ), size_t( buffer'length ) );
   for i in buffer'range loop
        exit when To_Ada( buffer(i) ) = ASCII.NUL;
        msg := msg & To_Ada( buffer(i) );
    end loop;
    free( buffer );
    msg := msg & " (error" & env.err'img & ")";
    raise berkeley_error with to_string( msg );
end raise_error;


-- RAISE EXCEPTIONS
--
-- Set the options related to exceptions.
------------------------------------------------------------------------------

procedure raise_exceptions( session : in out berkeley_session; will_raise : boolean := true; show_location : boolean := true ) is
begin
  session.will_raise := will_raise;
  session.error_location := show_location;
end raise_exceptions;

function will_raise( session : berkeley_session ) return boolean is
begin
  return session.will_raise;
end will_raise;

function last_error( session : berkeley_session ) return db_error is
begin
  return session.err;
end last_error;


-- INIT
--
-- Initialize the BDB handle using db_create().
------------------------------------------------------------------------------

procedure init( session : in out berkeley_session; env : berkeley_environment := standalone_environment ) is
begin
  C_db_create( session.err, session.dbh, env.env );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
  session.must_init := false;
end init;


-- NEW BERKELEY SESSION
--
-- This initialize the Ada session record.  If the file name is an empty
-- string, create will create an in-memory database.
-- (Null strings will be treated as NULL pointers in C.)
------------------------------------------------------------------------------

procedure new_berkeley_session( session : out berkeley_session; env : berkeley_environment; max_key_length, max_data_length : size_t ) is
begin
  session.key_buffer := new char_array( 0 .. max_key_length-1 );  -- size_t starts at zero
  session.data_buffer := new char_array( 0 .. max_data_length-1 ); -- but make it explicit for readability
  init( session, env );
end new_berkeley_session;


-- FREE BERKELEY SESSION
--
-- Releases memory allocated by new berkely session.
------------------------------------------------------------------------------

procedure free_berkeley_session( session : out berkeley_session ) is
  use char_array_ptr_Conv;
begin
  if session.key_buffer /= null then
     free( session.key_buffer );
  end if;
  if session.data_buffer /= null then
     free( session.data_buffer );
  end if;
end free_berkeley_session;


-- OPEN
--
-- Open a database.
------------------------------------------------------------------------------

procedure open( session : in out berkeley_session;
   fname, dbname : string;
   kind : db_type; flags : open_flags; mode : interfaces.C.int ) is
begin
  -- TODO: this creates a standalone session.  Can it do an environment?
  if session.must_init then
     init( session );
  end if;
  session.fname := to_unbounded_string( fname );
  session.dbname := to_unbounded_string( dbname );
  C_db_open( session.err, session.dbh, session.txn, to_string( session.fname ) & ASCII.NUL,
     to_string( session.dbname ) & ASCII.NUL, kind, flags, mode );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end open;


-- CREATE
--
-- Create a database.  This is the same as open with the create flag.
------------------------------------------------------------------------------

procedure create( session : in out berkeley_session;
   fname, dbname : string;
   kind : db_type; flags : open_flags; mode : interfaces.C.int  ) is
begin
  -- TODO: this creates a standalone session.  Can it do an environment?
  if session.must_init then
     init( session );
  end if;
  session.fname := to_unbounded_string( fname );
  session.dbname := to_unbounded_string( dbname );
  C_db_open( session.err, session.dbh, session.txn, to_string( session.fname ) & ASCII.NUL,
    to_string( session.dbname ) & ASCII.NUL, kind, flags or DB_OPEN_CREATE, mode );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end create;


-- CLOSE
--
-- Close an open database.
------------------------------------------------------------------------------

procedure close( session : in out berkeley_session ) is
begin
  C_db_close( session.err, session.dbh );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
  session.must_init := true;
end close;


-- RENAME
--
-- Rename a database that is not open.
------------------------------------------------------------------------------

procedure rename( session : in out berkeley_session; new_fname : string ) is
begin
  -- must be closed
  -- TODO: this creates a standalone session.  Can it do an environment?
  if session.must_init then
     init( session );
  end if;
  C_db_rename( session.err, session.dbh, to_string( session.fname ) & ASCII.NUL,
    to_string( session.dbname ) & ASCII.NUL, new_fname & ASCII.NUL, 0 );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
  session.fname := to_unbounded_string( new_fname );
  session.must_init := true;
end rename;


-- REMOVE
--
-- Drop a database that is not open.
------------------------------------------------------------------------------

procedure remove( session : in out berkeley_session ) is
begin
  -- must be closed
  -- TODO: this creates a standalone session.  Can it do an environment?
  if session.must_init then
     init( session );
  end if;
  C_db_remove( session.err, session.dbh, to_string( session.fname ) & ASCII.NUL,
    to_string( session.dbname ) & ASCII.NUL, 0 );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
  session.must_init := true;
end remove;


-- PUT
--
-- Store a key-value pair in the database.
------------------------------------------------------------------------------

procedure put( session : in out berkeley_session; key : string; data : string; flags : put_flags := 0 ) is
begin
  to_char_array( key, session.key_buffer.all, session.key_length );
  to_char_array( data, session.data_buffer.all, session.data_length );
  C_db_put( session.err, session.dbh, session.txn,
    char_array_ptr_Conv.to_address( session.key_buffer ),
    session.key_length,
    char_array_ptr_Conv.to_address( session.data_buffer ),
    session.data_length,
    flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end put;

procedure put( session : in out berkeley_session; key, data : unbounded_string; flags : put_flags := 0 ) is
begin
  put( session, to_string( key ), to_string( data ), flags );
end put;

procedure put( session : in out berkeley_session; recno : positive; data : string; flags : put_flags := 0 ) is
begin
  to_char_array( data, session.data_buffer.all, session.data_length );
  C_db_put_recno( session.err, session.dbh, session.txn,
    db_recno_t( recno ),
    char_array_ptr_Conv.to_address( session.data_buffer ),
    session.data_length,
    flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end put;
-- TODO: not tested

-- GET
--
-- Fetch a key-value pair from the database.  An error occurs if it doesn't
-- exist.
------------------------------------------------------------------------------

procedure get( session : in out berkeley_session; key : string; data : out unbounded_string; flags : get_flags := 0 ) is
begin
  to_char_array( key, session.key_buffer.all, session.key_length );
  C_db_get( session.err, session.dbh, session.txn,
    char_array_ptr_Conv.to_address( session.key_buffer ),
    session.key_length,
    char_array_ptr_Conv.to_address( session.data_buffer ),
    session.data_length,
    flags );
  data := to_unbounded_string( session.data_buffer.all, session.data_length );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end get;

-- GET
--
-- Fetch a value by record number from the database.  An error occurs if it doesn't
-- exist.
------------------------------------------------------------------------------

procedure get( session : in out berkeley_session; recno : positive; data : out unbounded_string; flags : get_flags := 0 ) is
begin
  C_db_get_recno( session.err, session.dbh, session.txn,
    db_recno_t( recno ),
    char_array_ptr_Conv.to_address( session.data_buffer ),
    session.data_length,
    flags );
  data := to_unbounded_string( session.data_buffer.all, session.data_length );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end get;

-- TODO: pget

-- DELETE
--
-- Delete a key-value pair from the database.  An error occurs if it doesn't
-- exist.
------------------------------------------------------------------------------

procedure delete( session : in out berkeley_session; key : string; flags : delete_flags := 0 ) is
begin
  to_char_array( key, session.key_buffer.all, session.key_length );
  C_db_del( session.err, session.dbh, session.txn,
    char_array_ptr_Conv.to_address( session.key_buffer ),
    session.key_length,
    flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end delete;

procedure set_pagesize( session : in out berkeley_session; pagesize : aPageSize ) is
begin
  if session.must_init then
     init( session );
  end if;
  C_set_pagesize( session.err, session.dbh, pagesize );
end set_pagesize;

-- returns err DB_NOTFOUND if key doesn't exist

procedure exists( session : in out berkeley_session; key : string; flags : exists_flags := 0 ) is
begin
  to_char_array( key, session.key_buffer.all, session.key_length );
  C_db_exists( session.err, session.dbh, session.txn,
    char_array_ptr_Conv.to_address( session.key_buffer ),
    session.key_length,
    flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end exists;

procedure truncate( session : in out berkeley_session; count : out natural ) is
  count32 : u_int32_t := 0;
begin
  C_db_truncate( session.err, session.dbh, session.txn, count32, 0 );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  else
     count := natural( count32 );
  end if;
end truncate;

procedure sync( session : in out berkeley_session; flags : sync_flags := 0 ) is
begin
  C_db_sync( session.err, session.dbh, flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end sync;

-- SET RE LEN
--
-- With queues or recno tables, store data in fixed-length record of len size
-- This must be specified before the database is created.  This is ignored when
-- opening an existing database.

procedure set_re_len( session : in out berkeley_session; len : natural ) is
begin
  if interfaces.C.size_t( len ) > session.data_buffer.all'length then
     raise berkeley_error with "record length" & len'img & " is bigger than data buffer length" &
        session.data_buffer.all'length'img;
  end if;
  C_db_set_re_len( session.err, session.dbh, u_int32_t( len ) );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end set_re_len;

-- SET RE PAD
--
-- With queues or recno tables, pad is the padding byte for fixed length
-- records.  The default is a space.  Use this in conjunction with set_re_len.
-- This must be specified before the database is created.  This is ignored when
-- opening an existing database.

procedure set_re_pad( session : in out berkeley_session; pad : character ) is
begin
  C_db_set_re_pad( session.err, session.dbh, int( character'pos( pad ) ) );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end set_re_pad;

-- SET RE DELIM
--
-- With recno tables using text files as a backing store, this is the end of
-- record character for the text files (default is a newline/line feed).  This
-- is used in conjunction with set_re_source.
-- This must be specified before the database is created.  This is ignored when
-- opening an existing database.

procedure set_re_delim( session : in out berkeley_session; delim : character ) is
begin
  C_db_set_re_delim( session.err, session.dbh, int( character'pos( delim ) ) );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end set_re_delim;

-- SET RE SOURCE
--
-- Treat the specified text file as a variable length record database.  The
-- file is only updated on close or sync.  The file must exist.  This is used
-- in conjunction with set_re_delim.
-- This must be specified before the database is created.  This is ignored when
-- opening an existing database.
-- This assumes source_str may be a pointer to a temporary value.

procedure set_re_source( session : in out berkeley_session; source : string ) is
  source_str : string := source & ASCII.NUL;
begin
  C_db_set_re_source( session.err, session.dbh, source_str'address );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end set_re_source;

-- nowait non-zero means do not wait

--procedure set_lk_exclusive( session : in out berkeley_session; nowait : integer ) is
--begin
--  C_db_set_lk_exclusive( session.err, session.dbh, int( nowait ) );
--  if session.will_raise and then session.err /= DB_OK then
--     raise_error( gnat.source_info.source_location, session );
--  end if;
--end set_lk_exclusive;

procedure set_flags( session : in out berkeley_session; f : config_flags ) is
begin
  C_db_set_flags( session.err, session.dbh, f );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end set_flags;

-- directory must be in environment list

procedure set_create_dir( session : in out berkeley_session; dirpath : string ) is
  temp : char_array := interfaces.C.To_C( dirpath );
begin
  C_db_set_create_dir( session.err, session.dbh, temp'address );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end set_create_dir;

procedure set_cachesize( session : in out berkeley_session; gbytes, bytes, ncache : integer ) is
begin
  C_db_set_cachesize( session.err, session.dbh,
    u_int32_t( gbytes ), u_int32_t( bytes ), Interfaces.C.int ( ncache ) );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end set_cachesize;

procedure set_bt_minkey( session : in out berkeley_session; bt_minkey : natural ) is
begin
  C_db_bt_minkey( session.err, session.dbh, u_int32_t( bt_minkey ) );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end set_bt_minkey;

procedure key_range( session : in out berkeley_session; key : string; less, equal, greater : out long_float; flags : key_range_flags ) is
   c_less, c_equal, c_greater : interfaces.C.double;
begin
  to_char_array( key, session.key_buffer.all, session.key_length );
  C_key_range( session.err, session.dbh, session.txn,
    char_array_ptr_Conv.to_address( session.key_buffer ),
    session.key_length,
    c_less'address, c_equal'address, c_greater'address, flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
  less    := long_float( c_less );
  equal   := long_float( c_equal );
  greater := long_float( c_greater );
end key_range;


------------------------------------------------------------------------------
--
-- Cursors
--
------------------------------------------------------------------------------


-- NEW BEREKELEY CURSOR
--
-- Allocate a new Berkeley cursor record

procedure new_berkeley_cursor( session : in out berkeley_session; cursor : in out berkeley_cursor; flags : cursor_flags ) is
begin
  C_db_cursor( session.err, session.dbh, session.txn, cursor.cursor, flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end new_berkeley_cursor;

-- GET (CURSOR)
--
-- Get a key/pair value from the current cursor position (or the position
-- specified in the flags).

procedure get( session : in out berkeley_session; cursor : in out berkeley_cursor;
  key, data : out unbounded_string; flags : c_get_flags := DB_C_GET_CURRENT ) is
begin
  C_dbc_c_get( session.err,
               cursor.cursor,
               char_array_ptr_Conv.to_address( session.key_buffer ),
               session.key_length,
               char_array_ptr_Conv.to_address( session.data_buffer ),
               session.data_length,
               flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
  key  := to_unbounded_string( session.key_buffer.all,  session.key_length );
  data := to_unbounded_string( session.data_buffer.all, session.data_length );
end get;

-- CLOSE (CURSOR)
--
-- Destroy a cursor.

procedure close( session : in out berkeley_session; cursor : berkeley_cursor ) is
begin
  C_dbc_c_close( session.err, cursor.cursor );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end close;

-- PUT (CURSOR)
--
-- Write a key/data pair to the current cursor position (or where specified in
-- the flags).  Putting before or after only applies to identical keys.

procedure put( session : in out berkeley_session; cursor : berkeley_cursor;
  key, data : string; flags : c_put_flags := DB_C_PUT_CURRENT ) is
begin
  to_char_array( key, session.key_buffer.all, session.key_length );
  to_char_array( data, session.data_buffer.all, session.data_length );
  C_dbc_c_put( session.err,
               cursor.cursor,
               char_array_ptr_Conv.to_address( session.key_buffer ),
               session.key_length,
               char_array_ptr_Conv.to_address( session.data_buffer ),
               session.data_length,
               flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end put;

procedure put( session : in out berkeley_session; cursor : berkeley_cursor;
  key, data : unbounded_string; flags : c_put_flags := DB_C_PUT_CURRENT ) is
begin
  put( session, cursor, to_string( key ), to_string( data ), flags );
end put;

-- COUNT (CURSOR)
--
-- Return the number of duplicate records stored at the current cursor
-- position.  flags is unused.

procedure count( session : in out berkeley_session; cursor : berkeley_cursor;
   countp : out natural; unused : flags := 0 ) is
   cnt : db_recno_t := db_recno_t'first;
begin
  C_dbc_c_count( session.err, cursor.cursor, cnt, unused );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
  countp := natural( cnt );
end count;

-- DELETE (CURSOR)
--
-- Remove a key/data pair from the cursor position.  Cursor does not move.
-- Flags are unused.

procedure delete( session : in out berkeley_session; cursor : berkeley_cursor;
   unused : flags := 0 ) is
begin
  C_dbc_c_del( session.err, cursor.cursor, unused );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end delete;

-- DUP (CURSOR)
--
-- Create a second cursor with the same transaction and locking properties
-- as the original.

procedure dup( session : in out berkeley_session; cursor : berkeley_cursor;
   cursor2 : out berkeley_cursor;  flags : c_dup_flags := 0 ) is
begin
  C_dbc_c_dup( session.err, cursor.cursor, cursor2.cursor, flags );
  if session.will_raise and then session.err /= DB_OK then
     raise_error( gnat.source_info.source_location, session );
  end if;
end dup;


------------------------------------------------------------------------------
--
-- Environments
--
------------------------------------------------------------------------------


-- INIT (environment)
--
-- Allocate an environment handle.  It is destroyed by close/remove.
-- TODO: create specific flag type

procedure init( env : in out berkeley_environment;  flags : env_flags := 0 ) is
begin
  C_db_env_create( env.err, env.env, flags );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  elsif env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is unexpectedly null";
  end if;
end init;


-- OPEN (environment)
--
-- Define an environment for BDB (required for transactions/logging, caching,
-- etc.).  dbhome is the working directory.
-- TODO: create specific flag type

procedure open( env : in out berkeley_environment;
  dbhome : string;
  flags  : e_open_flags := 0;
  mode   : interfaces.C.int ) is
begin
  C_dbe_open( env.err, env.env, dbhome & ASCII.NUL, flags, mode );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end open;


-- CREATE (environment)
--
-- Same as open with create flag set.
-- TODO: create specific flag type

procedure create( env : in out berkeley_environment;
  dbhome : string;
  flags  : e_open_flags := 0;
  mode   : interfaces.C.int ) is
begin
  C_dbe_open( env.err, env.env, dbhome & ASCII.NUL, flags or DB_E_OPEN_CREATE, mode );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end create;


-- CLOSE (environment)
--
--Free all resources created by open.  Does not close databases.

procedure close( env : in out berkeley_environment ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_close( env.err, env.env, 0 );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
  env.env := no_environment;
end close;


-- REMOVE (environment)
--
-- Deallocate an environment handle created by db_env_create.  This is normally
-- automatic on application termination but it can be done explicitly to free
-- memory using this call.    Handle must not be open.
-- TODO: I get EBUSY during tests.
-- TODO: I don't auto-init the env if none because flags can be non-zero

procedure remove( env : in out berkeley_environment;
  dbhome : string; flags : env_flags := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_remove( env.err, env.env, dbhome & ASCII.NUL, flags );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
  env.env := no_environment;
end remove;


-- DB REMOVE (environment)
--
-- Delete a database.  The database must not be open.

procedure dbremove( env : in out berkeley_environment;
  file, database : string; flags : e_dbremove_flags := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_dbremove( env.err, env.env, no_transaction, file & ASCII.NUL,
     database & ASCII.NUL, flags );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end dbremove;
-- TODO: session.txn


-- DB REMOVE (environment)
--
-- Delete a database.  The database must not be open.

procedure dbrename( env : in out berkeley_environment;
  file, database, new_fname : string; flags : e_dbrename_flags := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_dbrename( env.err, env.env, no_transaction, file & ASCII.NUL,
     database & ASCII.NUL, new_fname & ASCII.NUL, flags );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end dbrename;
-- TODO: session.txn


-- VERSION
--
-- Return the version of Berkeley DB as a string

function version return unbounded_string is
   buffer : char_array_ptr;
   msg : unbounded_string;
begin
   buffer := new char_array(0..255);
   C_db_version( char_array_ptr_Conv.to_address( buffer ), size_t( buffer'length ) );
   for i in buffer'range loop
   exit when To_Ada( buffer(i) ) = ASCII.NUL;
        msg := msg & To_Ada( buffer(i) );
   end loop;
   free( buffer );
   return msg;
end version;


-- SET DATA DIR (environment)
--
-- Set the path of the database file directory.  Each call adds a path.  This
-- must be called before the environment is opened.

procedure set_data_dir( env : in out berkeley_environment;
  dir : string; flags : e_dbrename_flags := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_data_dir( env.err, env.env, dir & ASCII.NUL );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_data_dir;


-- SET ENCRYPT (environment)
--
-- Set the password for encryption.  This must be called before the environment
-- is opened.

procedure set_encrypt( env : in out berkeley_environment;
  passwd : string; flags : e_encrypt_flags := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_encrypt( env.err, env.env, passwd & ASCII.NUL, flags );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_encrypt;


-- SET FLAGS (environment)
--
-- Change/set envrionment flags.

procedure set_flags( env : in out berkeley_environment;
  flags : e_set_flags; onoff : integer := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_flags( env.err, env.env, flags, Interfaces.C.int( onoff ) );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_flags;


-- SET SHM KEY (environment)
--
-- Change the root shared memory key when you have multiple environments.

procedure set_shm_key( env : in out berkeley_environment;
  shm_key : long_integer ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_shm_key( env.err, env.env, Interfaces.C.long( shm_key ) );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_shm_key;


-- SET TAS SPINS (environment)
--
-- Set the number of mutex spins before blocking occurs.

--procedure set_tas_spins( env : in out berkeley_environment;
--  tas_spins : integer ) is
--begin
--  if env.env = no_environment then
--     raise berkeley_error with gnat.source_info.source_location &
--        ": environment is not initialized";
--  end if;
--  C_dbe_set_tas_spins( env.err, env.env, u_int32_t( tas_spins ) );
--  if env.will_raise and then env.err /= DB_OK then
--     raise_error( gnat.source_info.source_location, env );
--  end if;
--end set_tas_spins;


-- SET TIMEOUT (environment)
--
-- Set the password for encryption.  This must be called before the environment
-- is opened.

procedure set_timeout( env : in out berkeley_environment;
  timeout : long_integer; flags : e_set_timeout_flags ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_timeout( env.err, env.env, interfaces.C.long( timeout ), flags );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_timeout;


-- SET TEMP DIR (environment)
--
-- Set the temp directory, overriding any environment variables.

procedure set_tmp_dir( env : in out berkeley_environment;
  dir : string ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_tmp_dir( env.err, env.env, dir & ASCII.NUL );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_tmp_dir;


-- SET VERBOSE (environment)
--
-- Enable additional error logging.

procedure set_verbose( env : in out berkeley_environment;
  flags : e_verbose_flags; onoff : integer := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_verbose( env.err, env.env, flags, Interfaces.C.int( onoff ) );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_verbose;


-- SET LK MAX LOCKERS (environment)
--
-- Set the maximum number of locking entities (default 1000).

procedure set_lk_max_lockers( env : in out berkeley_environment;
  max : natural := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_lk_max_lockers( env.err, env.env, u_int32_t( max ) );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_lk_max_lockers;


-- SET LK MAX LOCKS (environment)
--
-- Set the maximum number of locks (default 1000).

procedure set_lk_max_locks( env : in out berkeley_environment;
  max : natural := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_lk_max_locks( env.err, env.env, u_int32_t( max ) );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_lk_max_locks;


-- SET LK MAX OBJECTS (environment)
--
-- Set the maximum number of simultaneous locks (default 1000).

procedure set_lk_max_objects( env : in out berkeley_environment;
  max : natural := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_lk_max_objects( env.err, env.env, u_int32_t( max ) );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_lk_max_objects;


-- SET LK DETECT (environment)
--
-- Configure deadlock detection mode (policy).

procedure set_lk_detect( env : in out berkeley_environment;
  mode : deadlock_detection_modes := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_set_lk_detect( env.err, env.env, mode );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end set_lk_detect;

-- Transactions

-- SIMPLE TXN BEGIN
--
-- Start a non-nested transaction.

procedure simple_txn_begin( session : in out berkeley_session;
  env : in out berkeley_environment; flags : e_begin_flags := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_dbe_txn_begin(
    env.err,
    env.env,
    no_transaction,
    session.txn,
    flags );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end simple_txn_begin;

-- SIMPLE TXN ABORT
--
-- Rollback a non-nested transaction.

procedure simple_txn_abort( session : in out berkeley_session;
  env : in out berkeley_environment ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_txn_abort( env.err, session.txn );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end simple_txn_abort;

-- SIMPLE TXN COMMIT
--
-- Commit a non-nested transaction.

procedure simple_txn_commit( session : in out berkeley_session;
  env : in out berkeley_environment; flags : e_commit_flags := 0 ) is
begin
  if env.env = no_environment then
     raise berkeley_error with gnat.source_info.source_location &
        ": environment is not initialized";
  end if;
  C_txn_commit( env.err, session.txn, flags );
  if env.will_raise and then env.err /= DB_OK then
     raise_error( gnat.source_info.source_location, env );
  end if;
end simple_txn_commit;

end bdb;
