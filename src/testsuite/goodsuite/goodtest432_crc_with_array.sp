procedure t is
    c32_list : array(1..1) of gnat.crc32.crc32;
    c32 : gnat.crc32.crc32;
    checksum1 : integer;
    checksum2 : integer;
begin
  gnat.crc32.initialize( c32 );
  gnat.crc32.update( c32, "Deign, Sovereign Mistress! to accept a lay," );
  gnat.crc32.update( c32, "No Laureate offering of elaborate art;" );
  checksum1:= gnat.crc32.get_value( c32 );

  -- this is an out parameter.  Out parameters take a reference.
  -- but doesn not use assign reference.

  gnat.crc32.initialize( c32_list( 1 ) );
  gnat.crc32.update( c32_list( 1 ), "Deign, Sovereign Mistress! to accept a lay," );
  gnat.crc32.update( c32_list( 1 ), "No Laureate offering of elaborate art;" );
  checksum2 := gnat.crc32.get_value( c32_list( 1 ) );
  pragma assert( checksum1 = checksum2 );
end t;

