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


# Runners
# ----------------------------------------------------------------------------
#
# Depending on the environment, sudo may or may not be required to install
# software.  For example, a container will not have sudo because there is
# only one user.

# Test for the presence of sudo

HAS_SUDO=
(exec sudo -h >/dev/null 2>&1)
if [ $? -eq 0 ] ; then
   HAS_SUDO=1
fi

# Runners functions that use or not use sudo

yum_install () {
  if [ -n "$HAS_SUDO" ] ; then
     sudo -u root yum install -q -y $@
  else
     yum install -q -y $@
  fi
}

apt_install () {
  if [ -n "$HAS_SUDO" ] ; then
     sudo -u root apt-get -q -y install $@
  else
     DEBIAN_FRONTEND=noninteractive apt-get -q -y install $@
  fi
}

zypper_install () {
  if [ -n "$HAS_SUDO" ] ; then
     sudo -u root zypper "--quiet" "--non-interactive" install $@
  else
     zypper "--quiet" "--non-interactive" install $@
  fi
}


# Detect the Linux distribution
# ----------------------------------------------------------------------------

# CentOS/Red hat

if test -f "/etc/redhat-release" ; then
   DISTRO="redhat"
fi

if test -f "/etc/SUSE-brand" ; then
   DISTRO="suse"
fi

if test -f "/etc/freebsd-update.conf" ; then
   DISTRO="freebsd"
fi

# Mint/Ubuntu

if [ -f "/etc/issue" ] ; then
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
fi

# Debian

if [ -z "$DISTRO" ] ; then
   if test -f "/etc/debian_version" ; then
      echo "Debian users"
      echo
      echo "This script uses sudo.  You may need to add your login to the"
      echo "sudoers configuration file.  Press return to continue."
      read TMP
      DISTRO="debian"
   fi
fi


# Install software dependences
# ----------------------------------------------------------------------------

case "$DISTRO" in
redhat )
   if [ -n "$HAS_SUDO" ] ; then
      sudo -u root yum list installed | fgrep epel-release >/dev/null
   else
      yum list installed | fgrep epel-release >/dev/null
   fi
   if [ $? -eq 1 ] ; then
      set -e
      if [ -n "$HAS_SUDO" ] ; then
         sudo -u root rpm -Uvh http://download.fedoraproject.org/pub/epel/7/x86_64/Packages/e/epel-release-7-14.noarch.rpm
      else
         rpm -Uvh http://download.fedoraproject.org/pub/epel/7/x86_64/Packages/e/epel-release-7-14.noarch.rpm
      fi
      # Containres may not have these
      yum_install make
      #
      yum_install bzip2
      yum_install gcc-gnat
      yum_install git
      yum_install gstreamer1
      yum_install gstreamer1-devel
      yum_install libdb-devel
      yum_install mariadb
      yum_install mariadb-devel
      yum_install mariadb-server
      yum_install mlocate
      yum_install postgresql
      yum_install postgresql-devel
      yum_install postgresql-server
      yum_install SDL
      yum_install SDL-devel
      yum_install SDL_image
      yum_install SDL_image-devel
      yum_install bc
      yum_install memcached
      yum_install readline-devel
      set +e
   fi
   ;;
suse)
   set -e
   zypper_install make
   zypper_install mlocate
   zypper_install gcc-ada
   zypper_install git
   zypper_install gstreamer-devel
   zypper_install libopenssl-devel
   zypper_install libSDL-devel
   zypper_install libSDL_image-devel
   zypper_install libmariadb-devel 
   zypper_install postgresql
   zypper_install postgresql-devel
   zypper_install postgresql-server-devel  # for pg_config
   zypper_install rpmlint
   zypper_install memcached
   zypper_install libdb-4_8-devel
   set +e
   ;;
ubuntu )
   set -e
   # Container
   apt_install apt-utils
   apt_install ncurses-bin
   #
   apt_install libselinux-dev
   apt_install bzip2
   apt_install gnat
   apt_install git
   apt_install libdb-dev
   apt_install libmysqlclient-dev
   apt_install mysql-server
   apt_install locate
   apt_install postgresql-client
   apt_install postgresql-server-dev-all
   #sudo -u root apt-get -q -y install libgstreamer0.10-dev
   apt_install libgstreamer1.0-dev
   apt_install libsdl1.2-dev
   apt_install libsdl-image1.2-dev
   apt_install wget
   apt_install bc
   apt_install memcached
   apt_install libreadline-dev
   set +e
   ;;
debian )
   set -e
   apt_install libselinux-dev
   apt_install bzip2
   apt_install gnat
   apt_install git
   apt_install libdb-dev
   apt_install libmariadbclient-dev
   apt_install libmariadb-dev-compat # R Pi 4
   apt_install mariadb-server
   apt_install locate
   apt_install postgresql-client
   apt_install postgresql-server-dev-all
   apt_install libgstreamer1.0-dev
   apt_install libsdl1.2-dev
   apt_install libsdl-image1.2-dev
   apt_install wget
   apt_install bc
   apt_install memcached
   apt_install libssl1.1
   apt_install libreadline-dev
   set +e
   ;;
freebsd )
   set -e
   echo "y" | pkg install bash
   echo "y" | pkg install mysql57-client
   echo "y" | pkg install bzip2
   echo "y" | pkg install gcc6-aux
   echo "y" | pkg install xmlada
   echo "y" | pkg install git
   # echo "y" | pkg install libdb-dev
   # echo "y" | pkg install libmysqlclient-dev
   echo "y" | pkg install mysql57-server
   echo "y" | pkg install postgresql12-client
   echo "y" | pkg install postgresql12-server
   # echo "y" | pkg install postgresql-server-dev-all
   echo "y" | pkg install gstreamer
   echo "y" | pkg install sdl # libsdl1.2-dev
   echo "y" | pkg install sdl_image # libsdl-image1.2-dev
   echo "y" | pkg install wget
   echo "y" | pkg install memcached
   echo "y" | pkg install readline
   echo "y" | pkg install db18
   set +x
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
if [ "$DISTRO" = "freebsd" ];then
   if [ "$LOGNAME" != "root" ] ; then
      echo "run /usr/libexec/locate.updatedb if you have not already"
   else
      /usr/libexec/locate.updatedb
   fi
elif [ "$HAS_SUDO" ] ; then
   sudo -u root updatedb
else
   updatedb
fi

# Done
# ----------------------------------------------------------------------------

echo "OK"

