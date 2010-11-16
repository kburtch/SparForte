#!/usr/local/bin/bush -i

-- You can include command line options in #!.  In this case, import the
-- entire environment.

pragma annotate( "env.cgi" );
pragma annotate( "" );
pragma annotate( "List the environment variables on the server" );
pragma annotate( "by Ken O. Burtch" );

-- run env and get the results

env_result : string := `env;`;

-- display the results using this template

pragma template( html, "env.tmpl" );

