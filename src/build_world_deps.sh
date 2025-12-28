#!/bin/sh

# Generates a custom world-deps.ads file based on the SparForte
# executable file.

DF="world-deps.ads"
BIN="spar"

if [ ! -f "$BIN" ] ; then
   echo "$0: cannot create dependencies without the executable"
   exit 192
fi

if [ -f "$DF" ] ; then
   chmod 644 "$DF"
   rm "$DF"
fi

cat > "$DF" <<HERE
with ada.strings.unbounded;
use  ada.strings.unbounded;

package world.deps is
-- This is a machine generated package containing a list of SparForte
-- Ada and C dependencies for displaying a Software Bill-of-Materials
-- (SBOM).  Do not modify this package file directly.

type aDependency is record
     names   : unbounded_string;
     version : unbounded_string;
     kind    : unbounded_string;
     files   : unbounded_string;
     license : unbounded_string;
end record;

sparBuildDependencies : array(1..3) of aDependency := (
  aDependency'(
     names   => to_unbounded_string( "AdaCGI" ),
     version => to_unbounded_string( "1.6" ),
     kind    => to_unbounded_string( "Ada Source Code" ),
     files   => to_unbounded_string( "N/A" ),
     license => to_unbounded_string( "LGPL 2.1" )
  ),
  aDependency'(
     names   => to_unbounded_string( "APQ" ),
     version => to_unbounded_string( "2.1 (SparForte patched)" ),
     kind    => to_unbounded_string( "Ada Source Code" ),
     files   => to_unbounded_string( "N/A" ),
     license => to_unbounded_string( "ACL or GPL2" )
  ),
  aDependency'(
     names   => to_unbounded_string( "MD5" ),
     version => to_unbounded_string( "1.1" ),
     kind    => to_unbounded_string( "Ada Source Code" ),
     files   => to_unbounded_string( "N/A" ),
     license => to_unbounded_string( "RSA" )
  )
HERE

cat >> "$DF" <<HERE
);

end world.deps;
HERE
chmod 444 "$DF"
exit
