#!/usr/local/bin/spar

pragma annotate( summary, "pack [-v] source-file" );
pragma annotate( description, "Try compressing a file using different compression programs" );
pragma annotate( description, "and keep the best result." );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

procedure pack is

  procedure usage is
    -- show help
  begin
    put( "usage: " ) @ ( source_info.file );
    put_line( " [-v] source-file" );
    new_line;
    put_line( "Try compressing a file using different compression programs" );
    put_line( "and keep the best result." );
    new_line;
  end usage;

  verbose : boolean := false;

begin

  -- There should be two parameters.  Any other number of parameters, or
  -- -h or --help, should show script usage

command_line.set_exit_status( 0 );                            -- status OK

if $# /= 1 and $# /= 2 then                                    -- not 2 params?
   usage;                                                     -- show usage
   return;                                                    -- and quit
elsif $1 = "-h" or $1 = "--help" then                         -- help request?
   usage;                                                     -- show usage
   return;                                                    -- and quit
elsif $1 = "-v" or $1 = "--verbose" then                      -- verbose?
   verbose;
end if;

  -- main block that does the work

declare
  source_path  : string;

  type compress_programs is ( c_none, c_compress, c_zip, c_zoo, c_gzip,
       c_bzip );

  compress : constant string := "/usr/bin/compress";
  zip      : constant string := "/usr/bin/zip";
  zoo      : constant string := "/bin/zoo";
  gzip     : constant string := "/bin/gzip";
  bzip     : constant string := "/usr/bin/bzip2";

  this_size : long_integer;
  best_size : long_integer;
  best_method : compress_programs := c_none;

begin

  if $# = 1 then
     source_path := directory_operations.format_pathname( $1 );
  else
     source_path := directory_operations.format_pathname( $2 );
  end if;
  -- first param - convert path to operating system format

  -- Verify that the source file exists

  if not files.is_waiting_file( source_path ) then
     put( standard_error, source_info.file )
         @( standard_error, ": source file is not readable, doesn't exist or is empty" );
     command_line.set_exit_status( 192 );
     return;
  end if;

  -- Try...

  best_size   := numerics.value( `stat -c '%s' "$source_path";` );
  best_method := c_none; 

  if files.is_executable( compress ) then
     $compress "-c" "$source_path" > "/tmp/temp.out";
     this_size   := numerics.value( `stat -c '%s' /tmp/temp.out;` );
     if verbose then
        put_line( source_info.file & ": compress size = " & strings.image( this_size ) );
     end if;
     if this_size < best_size then
        best_size := this_size;
        best_method := c_compress;
     end if;
  else
     if verbose then
        put_line( source_info.file & ": no compress" );
     end if;
  end if;

  if files.is_executable( zip ) then
     $zip "-q" "-" "$source_path" > "/tmp/temp.out";
     this_size   := numerics.value( `stat -c '%s' /tmp/temp.out;` );
     if verbose then
        put_line( source_info.file & ": zip size = " & strings.image( this_size ) );
     end if;
     if this_size < best_size then
        best_size := this_size;
        best_method := c_zip;
     end if;
  else
     if verbose then
        put_line( source_info.file & ": no zip" );
     end if;
  end if;

  --if files.is_executable( zoo ) then
  --   zoo( source_path );
  --end if;

  if files.is_executable( gzip ) then
     $gzip "-c" "$source_path" > "/tmp/temp.out";
     this_size   := numerics.value( `stat -c '%s' /tmp/temp.out;` );
     if verbose then
        put_line( source_info.file & ": gzip size = " & strings.image( this_size ) );
     end if;
     if this_size < best_size then
        best_size := this_size;
        best_method := c_gzip;
     end if;
  else
     if verbose then
        put_line( source_info.file & ": no gzip" );
     end if;
  end if;

  if files.is_executable( bzip ) then
     $bzip "-c" "$source_path" > "/tmp/temp.out";
     this_size   := numerics.value( `stat -c '%s' /tmp/temp.out;` );
     if verbose then
        put_line( source_info.file & ": bzip2 size = " & strings.image( this_size ) );
     end if;
     if this_size < best_size then
        best_size := this_size;
        best_method := c_bzip;
     end if;
  else
     if verbose then
        put_line( source_info.file & ": no bzip2" );
     end if;
  end if;

  case best_method is
  when c_none =>
       put_line( source_info.file & ": no compression" );
       mv "$source_path" "$source_path"".none";
  when c_compress =>
       put_line( source_info.file & ": using compress" );
       $compress "$source_path";
  when c_zip =>
       put_line( source_info.file & ": using zip" );
       $zip "$source_path"".zip" "$source_path";
       rm  "$source_path";
  when c_zoo =>
       null;
  when c_gzip =>
       put_line( source_info.file & ": using gzip" );
       $gzip "$source_path";
  when c_bzip =>
       put_line( source_info.file & ": using bzip" );
       $bzip2 "$source_path";
  when others =>
     put( standard_error, source_info.file )
         @( standard_error, ": unknown compression method" );
     command_line.set_exit_status( 192 );
  end case;

end;
rm /tmp/temp.out;

command_line.set_exit_status( 0 );

end pack;

-- VIM editor formatting instructions
-- vim: ft=spar

