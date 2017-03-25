#!/usr/local/bin/spar

pragma annotate( summary, "ramcache" )
       @( description, "Use a ramdisk as a key-value lookup table with " )
       @( description, "values that expire." )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

procedure ramcache is
  ramdisk_dir : constant string := "/dev/shm";
  -- absolute path to the ram disk directory (typically /dev/shm on Linux)

  cache_dir   : constant string := "cache";
  -- diretory name of our cache on the ramdisk

  cache_path  : constant string := ramdisk_dir & "/" & cache_dir;
  -- absolute path of our cache

  ramcache_error : exception;

  ----------------------------------------------------------------------------
  -- Support Subprograms
  ----------------------------------------------------------------------------

  type a_cache_entry is record
       saved_time_year  : calendar.year_number;
       saved_time_month : calendar.month_number;
       saved_time_day   : calendar.day_number;
       saved_time_day_duration : calendar.day_duration;
       key_value        : universal_typeless;
  end record;
  -- the value and the time it was stored


  ----------------------------------------------------------------------------
  -- Cache Primitives
  --
  -- These are primitive operations, to be used as a foundation for other
  -- main operation.
  ----------------------------------------------------------------------------


  -- WRITE CACHE
  --
  -- Store a key-value pair in the cache, along with the time it was stored.
  ----------------------------------------------------------------------------

  procedure write_cache( key : string; key_value : universal_typeless ) is
    md5_sig : string;
    subdir  : string;
    f : file_type;
    cache_entry : a_cache_entry;
    json : json_string;
  begin
    md5_sig := numerics.md5( key );
    subdir := strings.head( md5_sig, 2 );
    -- Create the subdirectory, if none exists
    if not files.is_directory( cache_path & "/" & subdir ) then
       mkdir( "-m", "700", cache_path & "/" & subdir );
    end if;
    calendar.split(
       calendar.clock,
       cache_entry.saved_time_year,
       cache_entry.saved_time_month,
       cache_entry.saved_time_day,
       cache_entry.saved_time_day_duration
    );
    cache_entry.key_value := key_value;
    records.to_json( json, cache_entry );
    create( f, out_file, cache_path & "/" & subdir & "/" & md5_sig );
    put_line( f, json );
    close( f );
  end write_cache;


  -- READ CACHE
  --
  -- Read a key-value pair in the cache, also returning the time of storage.
  -- An empty string is returned if the value is not found.
  ----------------------------------------------------------------------------

  procedure read_cache( key : string; key_value : out universal_typeless; saved_time : out calendar.time ) is
    md5_sig : string;
    subdir  : string;
    f : file_type;
    cache_entry : a_cache_entry;
    json : json_string;
  begin
    md5_sig := numerics.md5( key );
    subdir := strings.head( md5_sig, 2 );
    open( f, in_file, cache_path & "/" & subdir & "/" & md5_sig );
    json := get_line( f );
    close( f );
    records.to_record( cache_entry, json );
    key_value := cache_entry.key_value;
    saved_time := calendar.time_of(
      cache_entry.saved_time_year,
      cache_entry.saved_time_month,
      cache_entry.saved_time_day,
      cache_entry.saved_time_day_duration
    );
  exception when others =>
    key_value := "";
    saved_time := calendar.clock;
  end read_cache;


  -- REMOvE CACHE
  --
  -- Remove an entry from the cache.  Does not remove the parent
  -- subdirectory.
  ----------------------------------------------------------------------------

  procedure remove_cache( key : string ) is
    md5_sig : string;
    subdir  : string;
    f : file_type;
  begin
    md5_sig := numerics.md5( key );
    subdir := strings.head( md5_sig, 2 );
    open( f, in_file, cache_path & "/" & subdir & "/" & md5_sig );
    delete( f );
  end remove_cache;


  ----------------------------------------------------------------------------
  -- Cache Interface Subprograms
  ----------------------------------------------------------------------------


  -- INIT CACHE
  --
  -- Create a cache directory or ensure he directory is writable.
  ----------------------------------------------------------------------------

  procedure init_cache is
  begin
     if not files.is_directory( cache_path ) then
        mkdir( "-m", "700", cache_path );
     elsif not files.is_writable( cache_path ) then
        raise ramcache_error with "cache directory is not writable";
     end if;
  end init_cache;


  -- PUT CACHE
  --
  -- Put a key-value pair in the cache.  The key and value must not be empty
  -- strings.
  ----------------------------------------------------------------------------

  procedure put_cache( key : string; key_value : universal_typeless ) is
  begin
    if key /= "" then
       if key_value /= "" then
          write_cache( key, key_value );
       end if;
    end if;
  end put_cache;


  -- GET CACHE
  --
  -- Get a value from the cache.  If it is too old or if it doesn't exist,
  -- return an empty string.  If key is empty, return an empty string.
  ----------------------------------------------------------------------------

  function get_cache( key : string; expires : duration ) return universal_typeless is
    key_value  : universal_typeless;
    saved_time : calendar.time;
  begin
    if key /= "" then
       read_cache( key, key_value, saved_time );
    end if;
  --  if calendar.clock > saved_time + expires then
  --     key_value := "";
  --  end if;
    return key_value;
  end get_cache;


  -- DELETE CACHE
  --
  -- Delete a value from the cache.  If key is empty, does nothing.
  ----------------------------------------------------------------------------

  procedure delete_cache ( key : string ) is
  begin
    if key /= "" then
       remove_cache( key );
    end if;
  end delete_cache;

begin
  -- create or verify the cache

  init_cache;

  -- add some values to the cache

  ? "Putting foo = bar, cnt = 15 in the cache";
  put_cache( "foo", "bar" );
  put_cache( "cnt", 15 );

  -- get values from the cache (good for 100 seconds)

  ? "Getting the value from the cache";
  ? get_cache( "foo", 100 );
  ? get_cache( "cnt", 100 );

  -- deleting the value

  ? "Deleting foo and cnt";
  delete_cache( "foo" );
  delete_cache( "cnt" );

end ramcache;

-- VIM editor formatting instructions
-- vim: ft=spar

