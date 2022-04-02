ok_count : natural;
info_count : natural;
warn_count : natural;
err_count : natural;

logs.metrics( ok_count, info_count, warn_count, err_count );
pragma assert( ok_count = 0 );
pragma assert( info_count = 0 );
pragma assert( warn_count = 0 );
pragma assert( err_count = 0 );

