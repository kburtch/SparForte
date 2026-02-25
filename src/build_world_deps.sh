#!/bin/sh

# Generates a custom world-deps.ads file based on the SparForte
# executable file.

DF="world-deps.ads"
BIN="spar"

DPKG_CMD="/usr/bin/dpkg"
ZYPPER_CMD="/usr/bin/zypper"
PACMAN_CMD="/usr/bin/pacman"
PKG_CMD="/usr/sbin/pkg"

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

sparBuildDependencies : array( Positive range <> ) of aDependency := (
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

DPKG_OUTPUT=""
ZYPPER_OUTPUT=""
PACMAN_OUTPUT=""
PKG_OUTPUT=""

# C Dependencies

LDD_OUTPUT=`ldd spar | sed 's/\t//g'`
echo "$LDD_OUTPUT" | ( while read LDD_LINE ; do
  LDD_FILE=`echo "$LDD_LINE" | cut -d' ' -f 1`
  # This file is always included and is not a real file.  It's a
  # virtual file added for the Linux kernel.
  # Dash shell does not support substring substitutions so use case
  case "$LDD_FILE" in
  "linux-vdso.so"* ) continue
  ;;
  esac
  #if [ "${LDD_FILE:0:13}" = "linux-vdso.so" ] ; then
  #   continue
  #fi
  LDD_PATH=`echo "$LDD_LINE" | cut -d' ' -f 3`
  # Determine the package providing the file
  if [ -x "$DPKG_CMD" ] ; then
     DPKG_OUTPUT=`"$DPKG_CMD" -S "$LDD_FILE"`
  elif [ -x "$ZYPPER_CMD" ] ; then
     ZYPPER_OUTPUT=`"$ZYPPER_CMD" -n -t --no-refresh search --provides --match-exact "$LDD_FILE" | tail -1 | sed 's/\ //g' | cut -d'|' -f2`
  elif [ -x "$PACMAN_CMD" ] ; then
     PACMAN_OUTPUT=`"$PACMAN_CMD" --query --owns "$LDD_PATH"`
  elif [ -x "$PKG_CMD" ] ; then
     # PKG_OUTPUT=`"$PKG_CMD" shlib "$LDD_PATH" | grep -Fv "provided by"`
     PKG_OUTPUT=`"$PKG_CMD" which "$LDD_PATH"`
     PKG_OUTPUT=`echo "${PKG_OUTPUT##*\ }"`  # last string
     if [ "${PKG_OUTPUT:0:12}" = "No packages " ] ; then
        PKG_OUTPUT=""
     fi
  fi
  if [ -n "$DPKG_OUTPUT" ] ; then
     FIRST=1
     echo "$DPKG_OUTPUT" | (while read DPKG_LINE ; do
        # The dpkg path does not necessarily match the ldd path
        DPKG_PKGNAME=`echo "$DPKG_LINE" | cut -d':' -f1`
        DPKG_PATH=`echo "$DPKG_LINE" | cut -d':' -f3`

        # Case eliminates duplicates
        case "$DPKG_PKGS" in
        *"$DPKG_PKGNAME"* )
          ;;
        *)
           if [ -n "$FIRST" ] ; then
              FIRST=
              DPKG_PKGS="$DPKG_PKGNAME"
           else
              DPKG_PKGS="$DPKG_PKGS"" ""$DPKG_PKGNAME"
           fi
           # Older apt will show a warning
           APT_SHOW_OUTPUT=`apt show "$DPKG_PKGNAME" 2>/dev/null`
           APT_VERSION=`echo "$APT_SHOW_OUTPUT" | grep -F "Version:" | cut -d' ' -f2`
           APT_LICENSE=`echo "$APT_SHOW_OUTPUT" | grep -F "License:" | cut -d' ' -f2`
           if [ -z "$APT_VERSION" ] ; then
              APT_VERSION="unknown"
           fi
           if [ -z "$APT_LICENSE" ] ; then
              APT_LICENSE="unknown"
           fi
           ;;
        esac
     done # DPKG_LINE
 cat >> "$DF" <<HERE
  , aDependency'(
     names   => to_unbounded_string( "$DPKG_PKGS" ),
     version => to_unbounded_string( "$APT_VERSION" ),
     kind    => to_unbounded_string( "C Library" ),
     files   => to_unbounded_string( "$LDD_PATH" ),
     license => to_unbounded_string( "N/A" )
  )
HERE
  )
  elif [ -n "$ZYPPER_OUTPUT" ] ; then
     # Zypper seems to pick the first matching package even if it is not
     # installed.
     ZYPPER_PKG="$ZYPPER_OUTPUT"
     ZYPPER_OUTPUT=`zypper -n -t --no-refresh product-info "package:""$ZYPPER_PKG"`
     # Berkeley DB has a "version" in its description so we use head -1
     ZYPPER_VERSION=`echo "$ZYPPER_OUTPUT" | fgrep Version | head -1 | cut -d: -f2-`
 cat >> "$DF" <<HERE
  , aDependency'(
     names   => to_unbounded_string( "$ZYPPER_PKG" ),
     version => to_unbounded_string( "$ZYPPER_VERSION" ),
     kind    => to_unbounded_string( "C Library" ),
     files   => to_unbounded_string( "$LDD_PATH" ),
     license => to_unbounded_string( "N/A" )
  )
HERE
  elif [ -n "$PACMAN_OUTPUT" ] ; then
     PACMAN_PKG=`echo "$PACMAN_OUTPUT" | cut -d' ' -f 5 | cut -d. -f1-2`
     PACMAN_OUTPUT=`"$PACMAN_CMD" --query --info "$PACMAN_PKG"`
      With version, strip any leading space
     PACMAN_VERSION=`echo "$PACMAN_OUTPUT" | grep -F "Version" | cut -d: -f2-`
     if [ "${PACMAN_VERSION:0:1}" = ' ' ] ; then
        PACMAN_VERSION="${PACMAN_VERSION:1}"
     fi
     # With license, strip any leading space
     PACMAN_LICENSE=`echo "$PACMAN_OUTPUT" | grep -F "Licenses" | cut -d: -f2-`
     if [ "${PACMAN_LICENSE:0:1}" = ' ' ] ; then
        PACMAN_LICENSE="${PACMAN_LICENSE:1}"
     fi
     if [ -z "$PACMAN_LICENSE" ] ; then
        PACMAN_LICENSE="N/A"
     fi
 cat >> "$DF" <<HERE
  , aDependency'(
     names   => to_unbounded_string( "$PACMAN_PKG" ),
     version => to_unbounded_string( "$PACMAN_VERSION" ),
     kind    => to_unbounded_string( "C Library" ),
     files   => to_unbounded_string( "$LDD_PATH" ),
     license => to_unbounded_string( "$PACMAN_LICENSE" )
  )
HERE
  elif [ -n "$PKG_OUTPUT" ] ; then
    if [ "$PKG_OUTPUT" = "database" ] ; then
       continue
    fi
    PKG_PKGS="$PKG_OUTPUT"
    PKG_OUTPUT=`pkg info "$PKG_PKGS"`
    PKG_VERSION=`echo "$PKG_OUTPUT" | grep -F "Version" | cut -d: -f2-`
    if [ "${PKG_VERSION:0:1}" = ' ' ] ; then
       PKG_VERSION="${PKG_VERSION:1}"
    fi
    PKG_LICENSE=`echo "$PKG_OUTPUT" | grep -F "Licenses" | cut -d: -f2-`
    # Remove leading space
    if [ "${PKG_LICENSE:0:1}" = ' ' ] ; then
       PKG_LICENSE="${PKG_LICENSE:1}"
    fi
    if [ -z "$PKG_LICENSE" ] ; then
       PKG_LICENSE="N/A"
    fi
 cat >> "$DF" <<HERE
  , aDependency'(
     names   => to_unbounded_string( "$PKG_PKGS" ),
     version => to_unbounded_string( "$PKG_VERSION" ),
     kind    => to_unbounded_string( "C Library" ),
     files   => to_unbounded_string( "$LDD_PATH" ),
     license => to_unbounded_string( "$PKG_LICENSE" )
  )
HERE
  else
cat >> "$DF"  <<HERE
  , aDependency'(
     names   => to_unbounded_string( "$LDD_FILE" ),
     version => to_unbounded_string( "unknown" ),
     kind    => to_unbounded_string( "C Library" ),
     files   => to_unbounded_string( "$LDD_PATH" ),
     license => to_unbounded_string( "unknown" )
  )
HERE
  fi
done ) # LDD_LINE


# Finish

cat >> "$DF" <<HERE
);

end world.deps;
HERE
chmod 444 "$DF"
exit
