#!/usr/local/bin/spar

pragma annotate( summary, "random.cgi" );
pragma annotate( description, "Display a random number when the page is loaded" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

random_number : positive := numerics.rnd(100);

-- display the results using this template

pragma template( html, "random.tmpl" );

