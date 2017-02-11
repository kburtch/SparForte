#!/bin/bash
# A script to install SparForte build dependences.
# by Ken O. Burtch
#
# This Bourne shell script was written on Bash but should support most
# Bourne shell variants, including ksh.  FreeBSD will fail using the
# default tcsh (which is based on csh and is not Bourne shell compatible).
#set -eu

DISTRO=""
TMP=""

# Detect the Linux distribution

TMP=`fgrep Ubuntu /etc/issue`
if [ "$TMP" != "" ] ; then
   DISTRO="ubuntu"
fi
TMP=`fgrep Mint /etc/issue`
if [ "$TMP" != "" ] ; then
   DISTRO="ubuntu"
fi
TMP=`fgrep SUSE /etc/issue`
if [ "$TMP" != "" ] ; then
   DISTRO="suse"
fi
if test -f "/etc/redhat-release" ; then
   DISTRO="redhat"
fi

# Install software dependences

case "$DISTRO" in
redhat )
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
   sudo -u root yum install -q -y php
   sudo -u root yum install -q -y postgresql
   sudo -u root yum install -q -y postgresql-devel
   sudo -u root yum install -q -y postgresql-server
   sudo -u root yum install -q -y SDL
   sudo -u root yum install -q -y SDL-devel
   sudo -u root yum install -q -y SDL_image
   sudo -u root yum install -q -y SDL_image-devel
   sudo -u root yum install -q -y bc
   set +e
   ;;
suse)
   set -e
   sudo -u root yast -i mlocate
   sudo -u root yast -i gcc-ada
   sudo -u root yast -i git
   sudo -u root yast -i postgresql
   sudo -u root yast -i postgresl-client
   sudo -u root yast -i libopenssl-devel
   sudo -u root yast -i libSDL-devel
   sudo -u root yast -i libSDL_image-devel
   sudo -u root yast -i libSDL_image
   sudo -u root yast -i libmysqlclient-devel
   sudo -u root yast -i postgresql-devel
   set +e
   ;;
ubuntu )
   set -e
   sudo -u root apt-get -q -y install alpine
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
   sudo -u root apt-get -q -y install apache2
   set +e
   ;;
*)
   echo "I don't know how to provision this distro"
   exit 192
esac
echo "Dependencies installed"
echo "Updating locate database..."
sudo -u root updatedb
echo "OK"

