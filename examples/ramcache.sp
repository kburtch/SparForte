procedure ramcache is
  ramdisk_dir : constant string := "/dev/shm";
  cache_dir   : constant string := "cache";
  cache_path  : constant string := ramdisk_dir & "/" & cache_dir;

  ramcache_error : exception;

  ----------------------------------------------------------------------------
  -- Support Subprograms
  ----------------------------------------------------------------------------

  type a_cache_entry is record
       saved_time : calendar.time;
       key_value : string;
  end record;

  -- WRITE CACHE
  --
  -- Store a key-value pair in the cache, along with the time of saving.
  ----------------------------------------------------------------------------

  procedure write_cache( key : string; key_value : string ) is
    md5_sig : string;
    f : file_type;
    cache_entry : a_cache_entry;
    json : json_string;
  begin
    md5_sig := numerics.md5( key );
    cache_entry.saved_time := calendar.clock;
    cache_entry.key_value := key_value;
    records.to_json( json, cache_entry );
    create( f, out_file, cache_path & "/" & md5_sig );
    put_line( f, json );
    close( f );
  end write_cache;


  -- READ CACHE
  --
  -- Store a key-value pair in the cache, along with the time of saving.
  ----------------------------------------------------------------------------

  procedure read_cache( key : string; key_value : out string; saved_time : out calendar.time ) is
    md5_sig : string;
    f : file_type;
    cache_entry : a_cache_entry;
    json : json_string;
  begin
    md5_sig := numerics.md5( key );
    open( f, in_file, cache_path & "/" & md5_sig );
    json := get_line( f );
    close( f );
? json;
-- TODO: time is numeric but incorrectly saved as a string, possibly because
-- calendar.time is a private time.
-- {"saved_time":"-4191177771725300000","key_value":"bar"}
    records.to_record( cache_entry, json );
    key_value := cache_entry.key_value;
    saved_time := cache_entry.saved_time;
  exception when others =>
    key_value := "";
    saved_time := calendar.clock;
raise;
  end read_cache;

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
  -- Put a key-value pair in the cache.  The value must not be an empty
  -- string.
  ----------------------------------------------------------------------------

  procedure put_cache( key : string; key_value : string ) is
  begin
    if key_value /= "" then
       write_cache( key, key_value );
    end if;
  end put_cache;


  -- GET CACHE
  --
  -- Get a value from the cache.  If it is too old or if it doesn't exist,
  -- return an empty string.
  ----------------------------------------------------------------------------

  function get_cache( key : string; expires : duration ) return string is
    key_value : string;
    saved_time : calendar.time;
  begin
    read_cache( key, key_value, saved_time );
    if calendar.clock > saved_time + expires then
       key_value := "";
    end if;
    return key_value;
  end get_cache;

begin
  init_cache;
  put_cache( "foo", "bar" );
  ? get_cache( "foo", 1000000 );
end ramcache;

-- VIM editor formatting instructions
-- vim: ft=spar

