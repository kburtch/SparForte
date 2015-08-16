#ifdef BDB4
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <db.h>

// C_db_create
//
// Initialize a database handle

void C_db_create( int *err, DB **dbh, DB_ENV *dbenv ) {

  *err = db_create( dbh, dbenv, 0 );
}

// C_db_open
//
// Open or create a database.  Null strings translated to NULL's.

void C_db_open(
               int *db_err,
               DB *dbh,
               DB_TXN *txnid,
               const char *fname,
               const char *dbname,
               int type,
               u_int32_t flags,
               int mode ) {
	if ( fname[0] == '\0' ) {
		if ( dbname[0] == '\0' ) {
			*db_err = dbh->open( dbh, txnid, NULL, NULL, type, flags, mode );
		} else {
			*db_err = dbh->open( dbh, txnid, NULL, dbname, type, flags, mode );
		}
	} else {
		if ( dbname[0] == '\0' ) {
			*db_err = dbh->open( dbh, txnid, fname, NULL, type, flags, mode );
		} else {
			*db_err = dbh->open( dbh, txnid, fname, dbname, type, flags, mode );
		}
	}
}

// C_db_close
//
// Close the database.  Database handle become invalid.

void C_db_close( int *db_err, DB *dbh ) {
	*db_err = dbh->close( dbh, 0 );
}

// C_db_remove
//
// Remove an unopened database.  Null strings translated to NULL's.
// Handle must be initialized before use.

void C_db_remove(
       int *db_err,
       DB *dbh,
       const char *fname,
       const char *dbname,
       u_int32_t unused ) {

	if ( fname[0] == '\0' ) {
		if ( dbname[0] == '\0' ) {
			*db_err = dbh->remove( dbh, NULL, NULL, unused );
		} else {
			*db_err = dbh->remove( dbh, NULL, dbname, unused );
		}
	} else {
		if ( dbname[0] == '\0' ) {
			*db_err = dbh->remove( dbh, fname, NULL, unused );
		} else {
			*db_err = dbh->remove( dbh, fname, dbname, unused );
		}
	}
}

// C_db_rename
//
// Rename an unopened database.  Null strings translated to NULL's.
// Handle must be initialized before use.
// TODO: handle new name null

void C_db_rename(
       int *db_err,
       DB *dbh,
       const char *fname,
       const char *dbname,
       const char *new_dbname,
       u_int32_t unused ) {

	if ( fname[0] == '\0' ) {
		if ( dbname[0] == '\0' ) {
			*db_err = dbh->rename( dbh, NULL, NULL, new_dbname, unused );
		} else {
			*db_err = dbh->rename( dbh, NULL, dbname, new_dbname, unused );
		}

	} else {
		if ( dbname[0] == '\0' ) {
			*db_err = dbh->rename( dbh, fname, NULL, new_dbname, unused );
		} else {
			*db_err = dbh->rename( dbh, fname, dbname, new_dbname, unused );
		}
	}
}

// C_db_put
//
// Store data in the database.

void C_db_put(
	int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    char *key,
    const int  key_length,
    char *data,
    const int  data_length,
    u_int32_t flags ) {

    DBT key_dbt, data_dbt;

    memset(&key_dbt, 0, sizeof(DBT));
    memset(&data_dbt, 0, sizeof(DBT));

    key_dbt.data = key;
    key_dbt.size = key_length;

    data_dbt.data = data;
    data_dbt.size = data_length;

    *db_err = dbh->put( dbh, txnid, &key_dbt, &data_dbt, flags );
}

// C_db_put_recno
//
// Store data in the database.

void C_db_put_recno(
	int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    db_recno_t recno,
    char *data,
    const int  data_length,
    u_int32_t flags ) {

    DBT key_dbt, data_dbt;

    memset(&key_dbt, 0, sizeof(DBT));
    memset(&data_dbt, 0, sizeof(DBT));

    key_dbt.data = &recno;
    key_dbt.size = sizeof(recno);

    data_dbt.data = data;
    data_dbt.size = data_length;
    *db_err = dbh->put( dbh, txnid, &key_dbt, &data_dbt, flags );
}

/*
void C_db_put_heap(
    int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    char *data,
    const int  data_length,
    long *page_no,
    long *page_indx,
    u_int32_t flags ) {

    DBT key_dbt, data_dbt;
    DB_HEAP_RID *recno;

    memset(&key_dbt, 0, sizeof(DBT));
    memset(&data_dbt, 0, sizeof(DBT));

    data_dbt.data = data;
    data_dbt.size = data_length;

    *db_err = dbh->put( dbh, txnid, &key_dbt, &data_dbt, flags );

    // Get the record number
    recno = (DB_HEAP_RID *)&key_dbt;
    page_no = recno->pgno;
    page_indx = recno->indx;

}
*/

// C_db_get
//
// Fetch data in the database.
// because key must be empty for root node insert.

void C_db_get(
	int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    char *key,
    const int  key_length,
    char *data,
    int  *data_length,
    u_int32_t flags ) {

	DBT key_dbt, data_dbt;

	memset(&key_dbt, 0, sizeof(DBT));
	memset(&data_dbt, 0, sizeof(DBT));

	key_dbt.data = key;
	key_dbt.size = key_length;

	*db_err = dbh->get( dbh, txnid, &key_dbt, &data_dbt, flags );

	memcpy( data, data_dbt.data, data_dbt.size );
	*data_length = data_dbt.size;

}

// C_db_get_recno
//
// Fetch data in the database.
// because key must be empty for root node insert.

void C_db_get_recno(
	int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    db_recno_t recno,
    char *data,
    int  *data_length,
    u_int32_t flags ) {

	DBT key_dbt, data_dbt;

	memset(&key_dbt, 0, sizeof(DBT));
	memset(&data_dbt, 0, sizeof(DBT));

	key_dbt.data = &recno;
	key_dbt.size = sizeof(recno);

	*db_err = dbh->get( dbh, txnid, &key_dbt, &data_dbt, flags );

	memcpy( data, data_dbt.data, data_dbt.size );
	*data_length = data_dbt.size;

}

// C_db_del
//
// Delete a record in the database.

void C_db_del(
	int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    char *key,
    const int  key_length,
    u_int32_t flags ) {

	DBT key_dbt;

	memset(&key_dbt, 0, sizeof(DBT));

	key_dbt.data = key;
	key_dbt.size = key_length;

	*db_err = dbh->del( dbh, txnid, &key_dbt, flags );

}

// C_db_strerror
//
// Return an error message

void C_db_strerror( int error_number, char *buffer, int buffer_len ) {
    char *cp;
	int len;

	cp = db_strerror( error_number );

    // length is message plus final zero, else buffer width
	len = strlen( cp ) + 1;
	if ( len > buffer_len ) {
		len = buffer_len;
	}

	memcpy( buffer, cp, len );
	// sanity: ensure last byte of buffer is always a zero
	buffer[buffer_len-1] = '\0';
}

// C_db_version
//
// Return the version string

void C_db_version( char *buffer, int buffer_len ) {
    char *cp;
	int len;

	cp = db_version( NULL, NULL, NULL );

    // length is message plus final zero, else buffer width
	len = strlen( cp ) + 1;
	if ( len > buffer_len ) {
		len = buffer_len;
	}

	memcpy( buffer, cp, len );
	// sanity: ensure last byte of buffer is always a zero
	buffer[buffer_len-1] = '\0';
}

void C_set_pagesize( int *db_err, DB *dbh, u_int32_t pagesize ) {
  *db_err = dbh->set_pagesize( dbh, pagesize );
}

// C_db_exists
//
// Check to see if a record is in the database.

void C_db_exists(
	int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    char *key,
    const int  key_length,
    u_int32_t flags ) {

	DBT key_dbt;

	memset(&key_dbt, 0, sizeof(DBT));

	key_dbt.data = key;
	key_dbt.size = key_length;

	*db_err = dbh->exists( dbh, txnid, &key_dbt, flags );

}

// C_db_truncate
//
// Delete all records in the database.

void C_db_truncate(
    int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    u_int32_t *countp,
    u_int32_t flags ) {

    *db_err = dbh->truncate( dbh, txnid, countp, flags );

}

// TODO: this could be done straight from Ada with a valued proc

void C_db_sync( int *db_err,
    DB *dbh,
    u_int32_t flags ) {

    *db_err = dbh->sync( dbh, flags );
}

void C_db_set_re_len( int *db_err,
    DB *dbh,
    u_int32_t len ) {

    *db_err = dbh->set_re_len( dbh, len );
}

void C_db_set_re_pad( int *db_err,
    DB *dbh,
    int pad ) {

    *db_err = dbh->set_re_pad( dbh, pad );
}

void C_db_set_re_delim( int *db_err,
    DB *dbh,
    int delim ) {

    *db_err = dbh->set_re_delim( dbh, delim );
}

void C_db_set_re_source( int *db_err,
    DB *dbh,
    char *source) {

    *db_err = dbh->set_re_source( dbh, source );
}

/*
void C_db_set_lk_exclusive( int *db_err,
    DB *dbh,
    int nowait ) {

    *db_err = dbh->set_lk_exclusive( dbh, nowait );
}
*/

void C_db_set_flags( int *db_err,
    DB *dbh,
    u_int32_t flags ) {

    *db_err = dbh->set_flags( dbh, flags );
}

void C_db_set_create_dir ( int *db_err,
    DB *dbh,
    const char * dir ) {

    *db_err = dbh->set_create_dir( dbh, dir );
}

void C_db_set_cachesize ( int *db_err,
    DB *dbh,
    u_int32_t gbytes,
    u_int32_t bytes,
    int ncache ) {

    *db_err = dbh->set_cachesize( dbh, gbytes, bytes, ncache );
}

void C_db_bt_minkey ( int *db_err,
    DB *dbh,
    u_int32_t bt_minkey) {

    *db_err = dbh->set_bt_minkey( dbh, bt_minkey );
}

void C_key_range(
    int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    char *key,
    const int  key_length,
    double *less,
    double *equal,
    double *greater,
    u_int32_t flags ) {

    DB_KEY_RANGE kr;
    DBT key_dbt;

    memset(&key_dbt, 0, sizeof(DBT));

    key_dbt.data = key;
    key_dbt.size = key_length;

    *db_err = dbh->key_range( dbh, txnid, &key_dbt, &kr, flags );
    *less = kr.less;
    *equal = kr.equal;
    *greater = kr.greater;

}

// C_db_cursor
//
// Initialize a database cursor

void C_db_cursor( int *err, DB *dbh, DB_TXN *txnid, DBC **cursor, u_int32_t flags ) {

  *err = dbh->cursor( dbh, txnid, cursor, flags );

}

// C_dbc_c_get
//
// Get next key/data pair via the cursor

void C_dbc_c_get( int *err,
  DBC *cursh,
  char *key,
  int  *key_length,
  char *data,
  int  *data_length,
  u_int32_t flags ) {

  DBT key_dbt, data_dbt;

  memset(&key_dbt, 0, sizeof(DBT));
  memset(&data_dbt, 0, sizeof(DBT));

  *err = cursh->c_get( cursh, &key_dbt, &data_dbt, flags );

  memcpy( key, key_dbt.data, key_dbt.size );
  *key_length = key_dbt.size;
  
  memcpy( data, data_dbt.data, data_dbt.size );
  *data_length = data_dbt.size;

}

// C_dbc_c_close
//
// Destory the cursor

void C_dbc_c_close( int *err, DBC *cursh ) {
   *err = cursh->c_close( cursh );
}

// C_dbc_c_put
//
// Store key/data pair via the cursor

void C_dbc_c_put( int *err,
  DBC *cursh,
  char *key,
  int  key_length,
  char *data,
  int  data_length,
  u_int32_t flags ) {

  DBT key_dbt, data_dbt;

  memset(&key_dbt, 0, sizeof(DBT));
  memset(&data_dbt, 0, sizeof(DBT));

  key_dbt.data = key;
  key_dbt.size = key_length;

  data_dbt.data = data;
  data_dbt.size = data_length;

  *err = cursh->c_put( cursh, &key_dbt, &data_dbt, flags );

}

// C_dbc_c_count
//
// Count the duplicates at the cursor position

void C_dbc_c_count( int *err,
  DBC *cursh,
  db_recno_t *countp,
  u_int32_t flags ) {

  *err = cursh->c_count( cursh, countp, flags );

}

// C_dbc_c_del
//
// Delete the key/data pair at the cursor.  The cursor does not move after
// the delete.

void C_dbc_c_del( int *err,
  DBC *cursh,
  u_int32_t flags ) {

  *err = cursh->c_del( cursh, flags );

}

// C_dbc_c_dup
//
// Create a second cursor with the same locking and transaction properties
// of the original.

void C_dbc_c_dup( int *err,
  DBC *cursh,
  DBC **cursh2,
  u_int32_t flags ) {

  *err = cursh->c_dup( cursh, cursh2, flags );

}


// ---------------------------------------------------------------------------
//
// Environments
//
// ---------------------------------------------------------------------------


// C_db_env_create
//
// Allocate an environment handle.  It is destroyed by close/remove.

void C_db_env_create( int *err,
   DB_ENV **dbe,
   u_int32_t flags) {
   
   *err = db_env_create( dbe, flags );

}

// C_dbe_open
//
// Define an environment for BDB (required for transactions/logging, caching,
// etc.).  db_home i the working directory.

void C_dbe_open( int *err,
   DB_ENV *dbe,
   char *db_home,
   u_int32_t flags,
   int mode ) {
   *err = dbe->open( dbe, db_home, flags, mode );
}

// C_dbe_close
//
// Free all resources created by open.  Does not close databases.

void C_dbe_close( int *err,
   DB_ENV *dbe,
   u_int32_t flags ) {
   *err = dbe->close( dbe, flags );
}

// C_dbe_remove
//
// Deallocate an environment handle created by db_env_create.  This is normally
// automatic on application termination but it can be done explicitly to free
// memory using this call.  Handle must not be open.

void C_dbe_remove( int *err,
   DB_ENV *dbe,
   char *db_home,
   u_int32_t flags) {

   *err = dbe->remove( dbe, db_home, flags );
}

// C_dbe_dbremove
//
// Delete a database.  Null strings translated to NULL's.  Database should be
// closed.

void C_dbe_dbremove( int *err,
   DB_ENV *dbe,
   DB_TXN *txnid,
   char *fname,
   char *dbname,
   u_int32_t flags) {

	if ( fname[0] == '\0' ) {
		if ( dbname[0] == '\0' ) {
			*err = dbe->dbremove( dbe, txnid, NULL, NULL, flags );
		} else {
			*err = dbe->dbremove( dbe, txnid, NULL, dbname, flags );
		}

	} else {
		if ( dbname[0] == '\0' ) {
			*err = dbe->dbremove( dbe, txnid, fname, NULL, flags );
		} else {
			*err = dbe->dbremove( dbe, txnid, fname, dbname, flags );
		}
	}

}

// C_dbe_dbrename
//
// Rename a database.  Null strings translated to NULL's.  Database should be
// closed.
// TODO: handle new name null

void C_dbe_dbrename( int *err,
   DB_ENV *dbe,
   DB_TXN *txnid,
   char *fname,
   char *dbname,
   char *fname2,
   u_int32_t flags) {

	if ( fname[0] == '\0' ) {
		if ( dbname[0] == '\0' ) {
			*err = dbe->dbrename( dbe, txnid, NULL, NULL, fname2, flags );
		} else {
			*err = dbe->dbrename( dbe, txnid, NULL, dbname, fname2, flags );
		}

	} else {
		if ( dbname[0] == '\0' ) {
			*err = dbe->dbrename( dbe, txnid, fname, NULL, fname2, flags );
		} else {
			*err = dbe->dbrename( dbe, txnid, fname, dbname, fname2, flags );
		}
	}

}


// C_dbe_set_data_dir
//
// Set the path of the database file directory.  Each call adds a path.  This
// must be called before the environment is opened.

void C_dbe_set_data_dir( int *err,
   DB_ENV *dbe,
   char *dir ) {
   
   *err = dbe->set_data_dir( dbe, dir );
}


// C_dbe_set_encrypt
//
// Set the password for encryption.  This must be called before the environment
// is opened.

void C_dbe_set_encrypt( int *err,
   DB_ENV *dbe,
   char *passwd,
   u_int32_t flags) {

   *err = dbe->set_encrypt( dbe, passwd, flags );
}


// C_dbe_set_flags
//
// Change/set envrionment flags

void C_dbe_set_flags( int *err,
   DB_ENV *dbe,
   u_int32_t flags,
   int onoff ) {

   dbe->set_flags( dbe, flags, onoff );
}


// C_dbe_set_shm_key
//
// Change the root shared memory key when you have multiple environments.

void C_dbe_set_shm_key( int *err,
   DB_ENV *dbe,
   long shm_key ) {

   *err = dbe->set_shm_key( dbe, shm_key );
}


// C_dbe_set_tas_spins
//
// Set the number of mutex spins before blocking occurs.
// NOTE: this function is not defined on my system.

//void C_dbe_set_tas_spins( int *err,
//   DB_ENV *dbe,
//   u_int32_t tas_spins ) {
//
//   *err = dbe->set_tas_spins( dbe, tas_spins );
//}


// C_dbe_set_timeout
//
// Set the timeout on locks/transactions in microseconds.

void C_dbe_set_timeout( int *err,
   DB_ENV *dbe,
   long timeout,
   u_int32_t flags) {

   *err = dbe->set_timeout( dbe, timeout, flags );
}


// C_dbe_set_tmp_dir
//
// Set the password for encryption.  This must be called before the environment
// is opened.

void C_dbe_set_tmp_dir( int *err,
   DB_ENV *dbe,
   const char *dir ) {

   *err = dbe->set_tmp_dir( dbe, dir );
}


// C_dbe_set_lg_dir
//
// Set the password for encryption.  This must be called before the environment
// is opened.

void C_dbe_set_lg_dir( int *err,
   DB_ENV *dbe,
   const char *dir ) {

   *err = dbe->set_lg_dir( dbe, dir );
}


// C_dbe_set_verbose
//
// Enable additional error logging.

void C_dbe_set_verbose( int *err,
   DB_ENV *dbe,
   u_int32_t flags,
   int onoff ) {

   *err = dbe->set_verbose( dbe, flags, onoff );
}


// C_dbe_set_lk_max_lockers
//
// Set the maximum number of locking entities (default 1000).

void C_dbe_set_lk_max_lockers( int *err,
   DB_ENV *dbe,
   u_int32_t max ) {

   *err = dbe->set_lk_max_lockers( dbe, max );
}


// C_dbe_set_lk_max_locks
//
// Set the maximum number of locks (default 1000).

void C_dbe_set_lk_max_locks( int *err,
   DB_ENV *dbe,
   u_int32_t max ) {

   *err = dbe->set_lk_max_locks( dbe, max );
}


// C_dbe_set_lk_max_objects
//
// Set the maximum number of simultaenous locks (default 1000).

void C_dbe_set_lk_max_objects( int *err,
   DB_ENV *dbe,
   u_int32_t max ) {

   *err = dbe->set_lk_max_objects( dbe, max );
}


// C_dbe_set_lk_detect
//
// Configure deadlock detection mode (policy).

void C_dbe_set_lk_detect( int *err,
   DB_ENV *dbe,
   u_int32_t detect ) {

   *err = dbe->set_lk_detect( dbe, detect );
}


// C_dbe_txn_begin
//
// Start a transaction.

void C_dbe_txn_begin( int *err,
   DB_ENV *dbe,
   DB_TXN *parent,
   DB_TXN **tid,
   u_int32_t flags ) {

   *err = dbe->txn_begin( dbe, parent, tid, flags );
}


// C_txn_abort
//
// Rollback a transaction.

void C_txn_abort( int *err,
   DB_TXN *tid ) {

   *err = tid->abort( tid );
}


// C_txn_commit
//
// Commit a transaction.

void C_txn_commit( int *err,
   DB_TXN *tid,
   u_int32_t flags ) {

   *err = tid->commit( tid, flags );
}


/*
void C_db_compact(
    int *db_err,
    DB *dbh,
    DB_TXN *txnid,
    char *start_key,
    const int  start_key_length,
    char *stop_key,
    const int  stop_key_length,
    char *data,
    int  *data_length,
    u_int32_t flags ) {

 *db_err = int
DB->compact(DB *db, DB_TXN *txnid,
DBT *start, DBT *stop, DB_COMPACT *c_data, u_int32_t flags, DBT *end);
}
*/

#endif
