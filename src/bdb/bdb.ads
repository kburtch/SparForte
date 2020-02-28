------------------------------------------------------------------------------
--
-- Berkeley DB Interface
--
------------------------------------------------------------------------------

with system.address_to_access_conversions,
     unchecked_deallocation,
     interfaces.c,
     ada.strings.unbounded,
     bdb_constants;
use  interfaces.c,
     ada.strings.unbounded,
     bdb_constants;

package bdb is

 berkeley_error : exception;

 -- The following errors and values come from the Berkeley DB include file
 -- /usr/include/db.h.  These are hard-coded.

------------------------------------------------------------------------------
--
-- The Interface
--
------------------------------------------------------------------------------

-- A Berkeley session holds most of the variables for opening a database,
-- including the I/O buffers.

type berkeley_session is private;

-- A Berkeley cursor holds a database cursor

type berkeley_cursor is private;

-- A Berkeley environment holds common settings and coordinates multiple
-- open databases.
-- standalone_environment means you will not be using an environment (that
-- is, a null pointer).

type berkeley_environment is private;

standalone_environment : constant berkeley_environment; --private

-- berkeley_lock holds a lock object
--
-- Lock functions not yet implemented.

type berkeley_lock is private;


-- RAISE ERROR
--
-- Raise the last error as an exception.  Include the location on that option
-- is enabled in the session.
------------------------------------------------------------------------------

procedure raise_error( location : string; session : berkeley_session );

-- RAISE EXCEPTIONS
--
-- Set the options related to exceptions.
------------------------------------------------------------------------------

procedure raise_exceptions( session : in out berkeley_session; will_raise : boolean := true; show_location : boolean := true );

function will_raise( session : berkeley_session ) return boolean;

function last_error( session : berkeley_session ) return db_error;

procedure show_err( err : db_error );

-- INIT
--
-- Initialize the BDB handle using db_create().
------------------------------------------------------------------------------

procedure init( session : in out berkeley_session; env : berkeley_environment := standalone_environment );


-- NEW BERKELEY SESSION
--
-- This initialize the Ada session record.  If the file name is an empty
-- string, create will create an in-memory database.
-- (Null strings will be treated as NULL pointers in C.)
------------------------------------------------------------------------------

procedure new_berkeley_session( session : out berkeley_session; env : berkeley_environment; max_key_length, max_data_length : size_t );


-- FREE BERKELEY SESSION
--
-- Releases memory allocated by new berkeley session.
------------------------------------------------------------------------------

procedure free_berkeley_session( session : out berkeley_session );


-- OPEN
--
-- Open a database.
------------------------------------------------------------------------------

procedure open( session : in out berkeley_session;
   fname, dbname : string;
   kind : db_type; flags : open_flags; mode : interfaces.C.int );

-- CREATE
--
-- Create a database.  This is the same as open with the create flag.
------------------------------------------------------------------------------

procedure create( session : in out berkeley_session;
   fname, dbname : string;
   kind : db_type; flags : open_flags; mode : interfaces.C.int  );

-- CLOSE
--
-- Close an open database.
------------------------------------------------------------------------------

procedure close( session : in out berkeley_session );


-- RENAME
--
-- Rename a database that is not open.
------------------------------------------------------------------------------

procedure rename( session : in out berkeley_session; new_fname : string );


-- REMOVE
--
-- Drop a database that is not open.
------------------------------------------------------------------------------

procedure remove( session : in out berkeley_session );


-- PUT
--
-- Store a key-value pair in the database.
------------------------------------------------------------------------------

procedure put( session : in out berkeley_session; key : string; data : string; flags : put_flags := 0 );

procedure put( session : in out berkeley_session; key, data : unbounded_string; flags : put_flags := 0 );

procedure put( session : in out berkeley_session; recno : positive; data : string; flags : put_flags := 0 );

-- GET
--
-- Fetch a key-value pair from the database.  An error occurs if it doesn't
-- exist.
------------------------------------------------------------------------------

procedure get( session : in out berkeley_session; key : string; data : out unbounded_string; flags : get_flags := 0 );

procedure get( session : in out berkeley_session; recno : positive; data : out unbounded_string; flags : get_flags := 0 );

-- DELETE
--
-- Delete a key-value pair from the database.  An error occurs if it doesn't
-- exist.
------------------------------------------------------------------------------

procedure delete( session : in out berkeley_session; key : string; flags : delete_flags := 0 );

procedure set_pagesize( session : in out berkeley_session; pagesize : aPageSize );

-- returns err DB_NOTFOUND if key doesn't exist

procedure exists( session : in out berkeley_session; key : string; flags : exists_flags := 0 );

procedure truncate( session : in out berkeley_session; count : out natural );

procedure sync( session : in out berkeley_session; flags : sync_flags := 0 );

procedure set_re_len( session : in out berkeley_session; len : natural );

procedure set_re_pad( session : in out berkeley_session; pad : character );

procedure set_re_delim( session : in out berkeley_session; delim : character );

procedure set_re_source( session : in out berkeley_session; source : string );

procedure set_flags( session : in out berkeley_session; f : config_flags );

-- directory must be in environment list? DB_ENV->add_data_dir

procedure set_create_dir( session : in out berkeley_session; dirpath : string );

procedure set_cachesize( session : in out berkeley_session; gbytes, bytes, ncache : integer );

procedure set_bt_minkey( session : in out berkeley_session; bt_minkey : natural );

procedure key_range( session : in out berkeley_session; key : string; less, equal, greater : out long_float; flags : key_range_flags );


------------------------------------------------------------------------------
--
-- Cursors
--
------------------------------------------------------------------------------

procedure new_berkeley_cursor( session : in out berkeley_session; cursor : in out berkeley_cursor; flags : cursor_flags := 0 );

procedure get( session : in out berkeley_session; cursor : in out berkeley_cursor;
  key, data : out unbounded_string; flags : c_get_flags := DB_C_GET_CURRENT );

procedure close( session : in out berkeley_session; cursor : berkeley_cursor );

procedure put( session : in out berkeley_session; cursor : berkeley_cursor;
  key, data : string; flags : c_put_flags := DB_C_PUT_CURRENT );

procedure put( session : in out berkeley_session; cursor : berkeley_cursor;
  key, data : unbounded_string; flags : c_put_flags := DB_C_PUT_CURRENT );

procedure count( session : in out berkeley_session; cursor : berkeley_cursor;
   countp : out natural; unused : flags := 0 );

procedure delete( session : in out berkeley_session; cursor : berkeley_cursor;
   unused : flags := 0 );

procedure dup( session : in out berkeley_session; cursor : berkeley_cursor;
   cursor2 : out berkeley_cursor;  flags : c_dup_flags := 0 );


------------------------------------------------------------------------------
--
-- Environments
--
------------------------------------------------------------------------------

procedure init( env : in out berkeley_environment;  flags : env_flags := 0 );

procedure create( env : in out berkeley_environment;
  dbhome : string;
  flags  : e_open_flags := 0;
  mode   : interfaces.C.int );

procedure open( env : in out berkeley_environment;
  dbhome : string;
  flags  : e_open_flags := 0;
  mode   : interfaces.C.int );

procedure close( env : in out berkeley_environment );

procedure remove( env : in out berkeley_environment;
  dbhome : string; flags : env_flags := 0 );

procedure dbremove( env : in out berkeley_environment;
  file, database : string; flags : e_dbremove_flags := 0 );

procedure dbrename( env : in out berkeley_environment;
  file, database, new_fname : string; flags : e_dbrename_flags := 0 );

function version return unbounded_string;

procedure set_data_dir( env : in out berkeley_environment;
  dir : string );

procedure set_encrypt( env : in out berkeley_environment;
  passwd : string; flags : e_encrypt_flags := 0 );

procedure set_flags( env : in out berkeley_environment;
  flags : e_set_flags; onoff : integer := 0 );

procedure set_shm_key( env : in out berkeley_environment;
  shm_key : long_integer );

--procedure set_tas_spins( env : in out berkeley_environment;
--  tas_spins : integer );

procedure set_timeout( env : in out berkeley_environment;
  timeout : long_integer; flags : e_set_timeout_flags );

procedure set_tmp_dir( env : in out berkeley_environment;
  dir : string );

procedure set_lg_dir( env : in out berkeley_environment;
  dir : string );

procedure set_verbose( env : in out berkeley_environment;
  flags : e_verbose_flags; onoff : integer := 0 );

-- Locking Subsystem

procedure set_lk_max_lockers( env : in out berkeley_environment;
  max : natural := 0 );

procedure set_lk_max_locks( env : in out berkeley_environment;
  max : natural := 0 );

procedure set_lk_max_objects( env : in out berkeley_environment;
  max : natural := 0 );

procedure set_lk_detect( env : in out berkeley_environment;
  mode : deadlock_detection_modes := 0 );

-- Transactions

procedure simple_txn_begin( session : in out berkeley_session;
  env : in out berkeley_environment; flags : e_begin_flags := 0 );

procedure simple_txn_abort( session : in out berkeley_session;
  env : in out berkeley_environment );

procedure simple_txn_commit( session : in out berkeley_session;
  env : in out berkeley_environment; flags : e_commit_flags := 0 );

-- TODO: associate, pget, pget (cursor)
-- TODO: complex transactions
-- TODO: get with not found not an exception but a boolean

------------------------------------------------------------------------------

private

-- Berkeley database handles


  type db_handle is new system.address;

-- Data

  -- C-style array of characters.
  -- Warnings about converting from C back to Ada will lose bounds are
  -- suppressed.

  pragma warnings( off );
  package char_array_ptr_Conv is new
    system.address_to_access_conversions( char_array );
  pragma warnings( on );
  subtype char_array_ptr is char_array_ptr_Conv.Object_Pointer;
  type char_array_address is new system.address;
  procedure Free is new Unchecked_Deallocation(
      Object => char_array,
      Name   => char_array_ptr );
  null_array : constant char_array_address := char_array_address( char_array_ptr_Conv.To_Address( null ) );

-- Black box C structures
--
-- The following are structures allocated by C.  Ada is only used to track
-- the pointers to the structures and provide type-specifc null pointers.
-- dummy is a data object that will never be accessed in Ada.

 type dummy is new integer;

 -- Environments

 type db_environment_dummy is new dummy;
 package db_environment_ptr_Conv is new
    system.address_to_access_conversions( db_environment_dummy );
 type db_environment is new system.address;
 no_environment : constant db_environment := db_environment( db_environment_ptr_conv.To_Address( null ) );

-- Transactions

  type db_transaction_dummy is new dummy;
  package db_transaction_ptr_Conv is new
     system.address_to_access_conversions( db_transaction_dummy );
  type db_transaction is new system.address;
  no_transaction : constant db_transaction := db_transaction( db_transaction_ptr_conv.To_Address( null ) );

-- Cursors

  type db_cursor_dummy is new dummy;
  package db_cursor_ptr_Conv is new
     system.address_to_access_conversions( db_cursor_dummy );
  type db_cursor is new system.address;
  no_cursor : constant db_cursor := db_cursor( db_cursor_ptr_conv.To_Address( null ) );

-- Cursors

  type db_lock_dummy is new dummy;
  package db_lock_ptr_Conv is new
     system.address_to_access_conversions( db_lock_dummy );
  type db_lock is new system.address;
  no_lock : constant db_lock := db_lock( db_lock_ptr_conv.To_Address( null ) );

-- Ada Sessions

type berkeley_session is record
  dbh         : db_handle;
  err         : db_error := DB_OK;
  key_buffer  : char_array_ptr;
  key_length  : interfaces.C.size_t := 0;
  has_key     : boolean := true;
  data_buffer : char_array_ptr;
  data_length : interfaces.C.size_t := 0;
  has_data    : boolean := true;
  will_raise  : boolean := true;
  fname       : unbounded_string;
  dbname      : unbounded_string;
  -- TODO: this limits transactions to a single open transaction per session
  txn         : db_transaction := no_transaction;
  must_init   : boolean := true;
  error_location : boolean := true;
end record;

-- Ada Cursors

type berkeley_cursor is record
  cursor      : db_cursor := no_cursor;
end record;

-- Ada Environments

type berkeley_environment is record
  env         : db_environment := no_environment;
  err         : db_error := DB_OK;
  will_raise  : boolean := true;
  error_location : boolean := true;
end record;

standalone_environment : constant berkeley_environment := berkeley_environment'(no_environment,
  DB_OK, true, true);

-- Ada locks

type berkeley_lock is record
  lock : db_lock := no_lock;
end record;

end bdb;

