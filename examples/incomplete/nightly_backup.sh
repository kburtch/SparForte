#!/bin/bash
#
# CVS: $Id$
# Created by: Jeremy Stockmans
# Revised by: Ken Burtch
# ---------------------------------------------------------------------------
shopt -s -o nounset

declare -r SCRIPT=${0##*/}
declare -r TODAY=`date +%Y-%m-%d`
declare  DOW=`date +%w`
declare -r HOST=`hostname --short`
declare -r BDIR="/var/backup/$HOST"
declare -r TFILE="db_$TODAY.tar.bz2"
declare -r LFILE="$BDIR/$SCRIPT""_log.txt"
declare -r LOCK="/var/lock/$SCRIPT.lck"
#declare -r EMAIL="jstockmans"
declare -r EMAIL="kburtch"
declare -r TEST=
# TEST is 1 or blank

# Sanity Checks

if [ ! -x /usr/bin/mysqldump ] ; then
   echo "$SCRIPT: cannot find/run mysqldump" >&2
   exit 192
fi
if [ ! -x /usr/bin/pg_dump ] ; then
   echo "$SCRIPT: cannot find/run pg_dump" >&2
   exit 192
fi
if [ ! -x /bin/tar ] ; then
   echo "$SCRIPT: cannot find/run tar" >&2
   exit 192
fi
if [ ! -x /bin/rm ] ; then
   echo "$SCRIPT: cannot find/run rm" >&2
   exit 192
fi
if [ ! -x /usr/bin/find ] ; then
   echo "$SCRIPT: cannot find/run /usr/bin/find" >&2
   exit 192
fi
if [ ! -d "/home/$EMAIL" ] ; then
   echo "$SCRIPT: email account $EMAIL may not exist" >&2
   exit 192
fi
if [ ! -d "/$BDIR" ] ; then
   echo "$SCRIPT: backup directory $BDIR may not exist" >&2
   exit 192
fi
if [ "$LOGNAME" != "root" ] ; then
   echo "$SCRIPT: you must be root to do backups" >&2
   exit 192
fi
if [ ! -d "/var/lock" ] ; then
   echo "$SCRIPT: lock directory /var/lock may not exist" >&2
   exit 192
fi

# Create a lock

if [ -f "$LOCK" ] ; then
   echo "$SCRIPT: another copy of this script is already running" >&2
   echo "$SCRIPT: if not, remove $LOCK" >&2
   exit 192
fi
date > "$LFILE"
echo "Process $$" >> "$LFILE"

# Show summary....

date > "$LFILE"
echo "--- Summary --------------------------------------------------" >> "$LFILE"

ls -lh "$BDIR" >> "$LFILE"
#du -h "$BLOC" >> "$LFILE"

date >> "$LFILE"
echo "--- Details --------------------------------------------------" >> "$LFILE"

# Do backups

if [ -z "$TEST" ] ; then
   test -f "$LFILE" && rm "$LFILE"
   date > "$LFILE"
   cd /
   if [ "$DOW" -ne 0 ] ; then
      # Incrememntal
      test -f "$BDIR/cvs_$TODAY""_inc.tar.bz2" && rm "$BDIR/cvs_$TODAY""_inc.tar.bz2"
      find var/cvsroot -type f -mtime -1 -print | ( while read FILE; do
        if test -f "$BDIR/cvs_$TODAY.tar" ; then
           nice /bin/tar ufv "$BDIR/cvs_$TODAY""_inc.tar" "$FILE" >> "$LFILE" 2>&1
        else
           nice /bin/tar cfv "$BDIR/cvs_$TODAY""_inc.tar" "$FILE" >> "$LFILE" 2>&1
        fi
      done )
      bzip2 "$BDIR/cvs_$TODAY""_inc.tar" >> "$LFILE" 2>&1
   else
      # Full
      nice /bin/tar cfvj "$BDIR/cvs_$TODAY"".tar.bz2" var/cvsroot >> "$LFILE" 2>&1
   fi
   nice /bin/tar cfvj "$BDIR/named_$TODAY.tar.bz2" var/lib/named >> "$LFILE" 2>&1
   nice /bin/tar cfvj "$BDIR/etc_$TODAY.tar.bz2" etc >> "$LFILE" 2>&1
   cd - > /dev/null
   #nice /usr/bin/mysqldump -u jeremy -pgmC410 mantis > $BDIR/mantis_$TODAY.sql 2>> "$LFILE"
   #nice /usr/bin/mysqldump -u jeremy -pgmC410 netoffice > $BDIR/netoffice_$TODAY.sql 2>> "$LFILE"
   #nice /usr/local/pgsql/bin/pg_dump -sxO ca_new > $BDIR/pgs_ca.sql 2>> "$LFILE"
   #nice /usr/local/pgsql/bin/pg_dump -Fp -C -a -D -x -O ca_new > $BDIR/pgd_ca.sql 2>> "$LFILE"
   #nice /usr/local/pgsql/bin/pg_dump -sxO townsend > $BDIR/pgs_tl.sql 2>> "$LFILE"
   #nice /usr/local/pgsql/bin/pg_dump -Fp -C -a -D -x -O townsend > $BDIR/pgd_tl.sql 2>> "$LFILE"

   cd "$BDIR"
   #nice /bin/tar -cjf "$TFILE" *.sql 2>> "$LFILE"
   #/bin/rm -f *.sql 2>> "$LFILE"

   # Expire Archive Entries after 7 days

   /usr/bin/find . -type f -name '*.gz' -mtime +7 -exec rm {} \; 2>> "$LFILE"
   /usr/bin/find . -type f -name '*.bz2' -mtime +7 -exec rm {} \; 2>> "$LFILE"

   date >> "$LFILE"
fi


# Handle results

if [ -s "$LFILE" ] ; then
   if [ ! -z "$TEST" ] ; then
      cat "$LFILE"
   else
      mail "$EMAIL" -s `hostname`" $SCRIPT Problems" < "$LFILE"
   fi
fi

rm -f "$LOCK"

exit 0

