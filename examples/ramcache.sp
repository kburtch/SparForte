#!/usr/local/bin/spar

-- TODO raising index strings error during syntax check?

pragma annotate( summary, "ramcache" )
       @( description, "Use a ramdisk as a key-value lookup table with " )
       @( description, "values that expire." )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

procedure ramcache is
  ramdisk_dir : constant string := "/dev/shm";
  -- path to the ram disk directory

  cache_dir   : constant string := "cache";
  -- diretory name of this cache

  cache_path  : constant string := ramdisk_dir & "/" & cache_dir;
  -- location of the cache

  ramcache_error : exception;

  ----------------------------------------------------------------------------
  -- Support Subprograms
  ----------------------------------------------------------------------------

  type a_cache_entry is record
       saved_time_year  : calendar.year_number;
       saved_time_month : calendar.month_number;
       saved_time_day   : calendar.day_number;
       saved_time_day_duration : calendar.day_duration;
       key_value : string;
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

  procedure write_cache( key : string; key_value : string ) is
    md5_sig : string;
    f : file_type;
    cache_entry : a_cache_entry;
    json : json_string;
  begin
    md5_sig := numerics.md5( key );
    calendar.split(
       calendar.clock,
       cache_entry.saved_time_year,
       cache_entry.saved_time_month,
       cache_entry.saved_time_day,
       cache_entry.saved_time_day_duration
    );
    cache_entry.key_value := key_value;
    records.to_json( json, cache_entry );
    create( f, out_file, cache_path & "/" & md5_sig );
    put_line( f, json );
    close( f );
  end write_cache;


  -- READ CACHE
  --
  -- Read a key-value pair in the cache, also returning the time of storage.
  -- An empty string is returned if the value is not found.
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
  -- create or verify the cache

  init_cache;

  -- add a value to the cache

  ? "Putting foo = bar in the cache";
  put_cache( "foo", "bar" );

  -- get a value from the cache (good for 100 seconds)

  ? "Getting foo's value from the cache";
  ? get_cache( "foo", 100 );
end ramcache;

-- VIM editor formatting instructions
-- vim: ft=spar

