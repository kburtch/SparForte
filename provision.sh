#!/bin/sh
# A script to install SparForte build dependences.
# by Ken O. Burtch
#
# This Bourne shell script was written on Bash but should support most
# Bourne shell variants, including ksh.  FreeBSD will fail using the
# default tcsh (which is based on csh and is not Bourne shell compatible).
# ----------------------------------------------------------------------------
#set -eu

DISTRO=""
TMP=""

# Show help

if [ $# -ne 0 ] ; then
   echo "A simple script to attempt to identify your operating system and"
   echo "install all SparForte dependences needed to enable all packages."
   echo "It is especially useful for deploying virtualized or cloud instances."
   echo "You may be prompted for the administrator password."
   exit 1
fi


# Detect the Linux distribution
# ----------------------------------------------------------------------------

# CentOS/Red hat

if test -f "/etc/redhat-release" ; then
   DISTRO="redhat"
fi

# Debian

if test -f "/etc/debian_version" ; then
   echo "This script uses sudo.  You may need to add your login to the"
   echo "sudoers configuration file.  Press return to continue."
   read TMP
   DISTRO="debian"
fi

# SuSE

TMP=`fgrep SUSE /etc/issue`
if [ "$TMP" != "" ] ; then
   DISTRO="suse"
fi

# Mint/Ubuntu

TMP=`fgrep Ubuntu /etc/issue`
if [ "$TMP" != "" ] ; then
   DISTRO="ubuntu"
fi
TMP=`fgrep Mint /etc/issue`
if [ "$TMP" != "" ] ; then
   DISTRO="ubuntu"
fi


# Install software dependences
# ----------------------------------------------------------------------------

case "$DISTRO" in
redhat )
   sudo -u root yum list installed | fgrep epel-release >/dev/null
   if [ $? -eq 1 ] ; then
      set -e
      sudo -u root rpm -Uvh http://download.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-9.noarch.rpm
      sudo -u root yum install -q -y bzip2
      sudo -u root yum install -q -y gcc-gnat
      sudo -u root yum install -q -y git
      sudo -u root yum install -q -y gstreamer1
      sudo -u root yum install -q -y gstreamer1-devel
      sudo -u root yum install -q -y libdb-devel
      sudo -u root yum install -q -y mariadb
      sudo -u root yum install -q -y mariadb-devel
      sudo -u root yum install -q -y mariadb-server
      sudo -u root yum install -q -y mlocate
      sudo -u root yum install -q -y postgresql
      sudo -u root yum install -q -y postgresql-devel
      sudo -u root yum install -q -y postgresql-server
      sudo -u root yum install -q -y SDL
      sudo -u root yum install -q -y SDL-devel
      sudo -u root yum install -q -y SDL_image
      sudo -u root yum install -q -y SDL_image-devel
      sudo -u root yum install -q -y bc
      sudo -u root yum install -q -y memcached
      set +e
   fi
   ;;
suse)
   set -e
   sudo -u root zypper "--non-interactive" install mlocate
   sudo -u root zypper "--non-interactive" install gcc-ada
   sudo -u root zypper "--non-interactive" install git
   sudo -u root zypper "--non-interactive" install gstreamer-0_10-devel
   sudo -u root zypper "--non-interactive" install libopenssl-devel
   sudo -u root zypper "--non-interactive" install libSDL-devel
   sudo -u root zypper "--non-interactive" install libSDL_image-devel
   sudo -u root zypper "--non-interactive" install libmysqlclient-devel
   sudo -u root zypper "--non-interactive" install postgresql
   sudo -u root zypper "--non-interactive" install postgresql-devel
   sudo -u root zypper "--non-interactive" install rpmlint
   sudo -u root zypper "--non-interactive" install memcached
   set +e
   ;;
ubuntu )
   set -e
   sudo -u root apt-get -q -y install libselinux-dev
   sudo -u root apt-get -q -y install bzip2
   sudo -u root apt-get -q -y install gnat
   sudo -u root apt-get -q -y install git
   sudo -u root apt-get -q -y install libdb-dev
   sudo -u root apt-get -q -y install libmysqlclient-dev
   sudo -u root apt-get -q -y install mysql-server
   sudo -u root apt-get -q -y install locate
   sudo -u root apt-get -q -y install postgresql-client
   sudo -u root apt-get -q -y install postgresql-server-dev-all
   sudo -u root apt-get -q -y install libgstreamer0.10-dev
   sudo -u root apt-get -q -y install libsdl1.2-dev
   sudo -u root apt-get -q -y install libsdl-image1.2-dev
   sudo -u root apt-get -q -y install wget
   sudo -u root apt-get -q -y install bc
   sudo -u root apt-get -q -y install memcached
   set +e
   ;;
debian )
   set -e
   sudo -u root apt-get -q -y install libselinux-dev
   sudo -u root apt-get -q -y install bzip2
   sudo -u root apt-get -q -y install gnat
   sudo -u root apt-get -q -y install git
   sudo -u root apt-get -q -y install libdb-dev
   sudo -u root apt-get -q -y install libmariadbclient-dev
   sudo -u root apt-get -q -y install mariadb-server
   sudo -u root apt-get -q -y install locate
   sudo -u root apt-get -q -y install postgresql-client
   sudo -u root apt-get -q -y install postgresql-server-dev-all
   sudo -u root apt-get -q -y install libgstreamer1.0-dev
   sudo -u root apt-get -q -y install libsdl1.2-dev
   sudo -u root apt-get -q -y install libsdl-image1.2-dev
   sudo -u root apt-get -q -y install wget
   sudo -u root apt-get -q -y install bc
   sudo -u root apt-get -q -y install memcached
   sudo -u root apt-get -q -y install libssl1.1
   set +e
   ;;
*)
   echo "I don't know how to provision this distro"
   exit 192
esac
echo "Dependencies installed"


# Update locate database
# ----------------------------------------------------------------------------

echo "Updating locate database..."
sudo -u root updatedb


# Done
# ----------------------------------------------------------------------------

echo "OK"

