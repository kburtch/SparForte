ok_count : natural;
info_count : natural;
warn_count : natural;
err_count : natural;

logs.metrics( ok_count, info_count, warn_count, err_count );

logs.ok( "test" );
logs.info( "test" );
logs.info( "test" );
logs.warning( "test" );
logs.warning( "test" );
logs.warning( "test" );
logs.error( "test" );
logs.error( "test" );
logs.error( "test" );
logs.error( "test" );

pragma assert( ok_count = 0 );
pragma assert( info_count = 0 );
pragma assert( warn_count = 0 );
pragma assert( err_count = 0 );

logs.checkpoint;
logs.metrics( ok_count, info_count, warn_count, err_count );

pragma assert( ok_count = 1 );
pragma assert( info_count = 2 );
pragma assert( warn_count = 3 );
pragma assert( err_count = 4 );

