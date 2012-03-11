#!/usr/local/bin/spar -i

-- You can include command line options in #!.  In this case, import the
-- entire environment.

pragma annotate( summary, "env.cgi" );
pragma annotate( description, "List the environment variables on the server" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

-- run env and get the results

env_result : string := `env;`;

-- display the results using this template

pragma template( html, "env.tmpl" );

-- VIM editor formatting instructions
-- vim: ft=spar

