ok_count : natural;
info_count : natural;
warn_count : natural;
err_count : natural;

logs.metrics( ok_count, info_count, warn_count, err_count );

logs.ok( "this is" ) @ ( "test" );
logs.ok( "this is" ) @ ( "test" );
logs.ok( "this is" ) @ ( "test" );
logs.ok( "this is" ) @ ( "test" );
logs.info( "this is" ) @ ( "test" );
logs.info( "this is" ) @ ( "test" );
logs.info( "this is" ) @ ( "test" );
logs.warning( "this is" ) @ ( "test" );
logs.warning( "this is" ) @ ( "test" );
logs.error( "this is" ) @ ( "test" );

pragma assert( ok_count = 0 );
pragma assert( info_count = 0 );
pragma assert( warn_count = 0 );
pragma assert( err_count = 0 );

logs.checkpoint;
logs.metrics( ok_count, info_count, warn_count, err_count );

pragma assert( ok_count = 4 );
pragma assert( info_count = 3 );
pragma assert( warn_count = 2 );
pragma assert( err_count = 1 );

