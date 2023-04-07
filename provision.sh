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

while [ $# -gt 0 ] ; do
  TMP=`echo "$1" | cut -d= -f1`
  if [ -z "$TMP" ] ; then
     $TMP="$1"
  fi
  case "$TMP" in
  -h|--help)
      echo "SparForte provision script"
      echo
      echo "A simple script to attempt to identify your operating system and"
      echo "install SparForte software dependences needed to build the language."
      echo "It is especially useful for deploying virtualized or cloud instances."
      echo "You may be prompted for the administrator password."
      echo
      echo "Additional options:"
      #echo "  --prefix=PREFIX       root installation directory"
      echo "  --without-bdb         do not Berkeley DB"
      echo "  --without-l10n        do not install GNU localization"
      echo "  --without-memcached   do not install memcached"
      echo "  --without-mysql       do not install MySQL/MariaDB"
      echo "  --without-opengl      do not install OpenGL"
      echo "  --without-pcre        do not install PCRE library"
      echo "  --without-postgres    do not install PostgreSQL"
      echo "  --without-readline    do not install GNU readline"
      echo "  --without-sdl         do not install SDL and SDLImage"
      echo "  --without-sound       do not install GStreamer"
      echo
      echo "Read INSTALL for further information on these options"
      exit 0
      ;;
  #--prefix)
  #    PREFIX=`echo "$1" | cut -d= -f2`
  #    ;;
  --without-bdb)
      NO_BDB=1
      ;;
  --without-l10n)
      NO_L10N=1
      ;;
  --without-memcached)
      NO_MEMCACHED=1
      ;;
  --without-mysql)
      NO_MYSQL=1
      ;;
  --without-opengl)
      NO_OPENGL=1
      ;;
  --without-pcre)
      NO_PCRE=1  # does not do anything as pcre is usually installed by default
      ;;
  --without-postgres)
      NO_POSTGRES=1
      ;;
  --without-readline)
      NO_READLINE=1
      ;;
  --without-sdl)
      NO_SDL=1
      ;;
  --without-sound)
      NO_SOUND=1
      ;;
  *)
      echo "Unrecognized option $TMP"
      exit 192
  esac
  shift
done


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
   if [ "$LOGNAME" != "root" ] ; then
      echo "you may need to run this script as root"
   fi
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
      # Containers may not have these
      yum_install make
      #
      yum_install bzip2
      yum_install gcc-gnat
      yum_install git
      yum_install bc
      yum_install mlocate
      if [ -z "$NO_SOUND" ] ; then
         yum_install gstreamer1
         yum_install gstreamer1-devel
      fi
      if [ -z "$NO_BDB" ] ; then
         yum_install libdb-devel
      fi
      if [ -z "$NO_MYSQL" ] ; then
         yum_install mariadb
         yum_install mariadb-devel
         yum_install mariadb-server
      fi
      if [ -z "$NO_POSTGRES" ] ; then
         yum_install postgresql
         yum_install postgresql-devel
         yum_install postgresql-server
      fi
      if [ -z "$NO_SDL" ] ; then
         yum_install SDL
         yum_install SDL-devel
         yum_install SDL_image
         yum_install SDL_image-devel
      fi
      if [ -z "$NO_MEMCACHED" ] ; then
         yum_install memcached
      fi
      if [ -z "$NO_READLINE" ] ; then
         yum_install readline-devel
      fi
      set +e
   fi
   ;;
suse)
   set -e
   zypper_install make
   zypper_install gcc-ada
   zypper_install git
   zypper_install libopenssl-devel
   zypper_install mlocate
   # zypper_install rpmlint
   if [ -z "$NO_SOUND" ] ; then
      zypper_install gstreamer-devel
   fi
   if [ -z "$NO_SDL" ] ; then
      zypper_install libSDL-devel
      zypper_install libSDL_image-devel
   fi
   if [ -z "$NO_MYSQL" ] ; then
      zypper_install libmariadb-devel
   fi
   if [ -z "$NO_POSTGRES" ] ; then
      zypper_install postgresql
      zypper_install postgresql-devel
      zypper_install postgresql-server-devel  # for pg_config
   fi
   if [ -z "$NO_MEMCACHED" ] ; then
      zypper_install memcached
   fi
   if [ -z "$NO_BDB" ] ; then
      zypper_install libdb-4_8-devel
   fi
   set +e
   ;;
ubuntu )
   set -e
   # Containers may not have these
   apt_install apt-utils
   apt_install ncurses-bin
   if [ -z "$NO_PCRE" ] ; then
      apt_install libpcre3-dev
   fi
   #
   apt_install libselinux-dev
   apt_install bzip2
   apt_install gnat
   apt_install git
   apt_install wget
   apt_install bc
   apt_install locate
   if [ -z "$NO_BDB" ] ; then
      apt_install libdb-dev
   fi
   if [ -z "$NO_MYSQL" ] ; then
      apt_install libmysqlclient-dev
      apt_install mysql-server
   fi
   if [ -z "$NO_POSTGRES" ] ; then
      apt_install postgresql-client
      apt_install postgresql-server-dev-all
   fi
   if [ -z "$NO_SOUND" ] ; then
      #sudo -u root apt-get -q -y install libgstreamer0.10-dev
      apt_install libgstreamer1.0-dev
   fi
   if [ -z "$NO_SDL" ] ; then
      apt_install libsdl1.2-dev
      apt_install libsdl-image1.2-dev
   fi
   if [ -z "$NO_MEMCACHED" ] ; then
      apt_install memcached
   fi
   if [ -z "$NO_READLINE" ] ; then
      apt_install libreadline-dev
   fi
   set +e
   ;;
debian )
   set -e
   apt_install libselinux-dev
   apt_install bzip2
   apt_install gnat
   apt_install git
   apt_install wget
   apt_install bc
   apt_install libssl1.1
   apt_install locate
   if [ -z "$NO_BDB" ] ; then
      apt_install libdb-dev
   fi
   if [ -z "$NO_MYSQL" ] ; then
      apt_install libmariadbclient-dev
      apt_install libmariadb-dev-compat # R Pi 4
      apt_install mariadb-server
   fi
   if [ -z "$NO_POSTGRES" ] ; then
      apt_install postgresql-client
      apt_install postgresql-server-dev-all
   fi
   if [ -z "$NO_SOUND" ] ; then
      apt_install libgstreamer1.0-dev
   fi
   if [ -z "$NO_SDL" ] ; then
      apt_install libsdl1.2-dev
      apt_install libsdl-image1.2-dev
   fi
   if [ -z "$NO_MEMCACHED" ] ; then
      apt_install memcached
   fi
   if [ -z "$NO_READLINE" ] ; then
      apt_install libreadline-dev
   fi
   set +e
   ;;
freebsd )
   set -e
   echo "y" | pkg install bash
   echo "y" | pkg install gmake
   echo "y" | pkg install bzip2
   echo "y" | pkg install gcc6-aux  # Ada support expires 2022-02
   # echo "y" | pkg install xmlada
   echo "y" | pkg install git
   echo "y" | pkg install wget
   if [ -z "$NO_MYSQL" ] ; then
      # echo "y" | pkg install libmysqlclient-dev
      echo "y" | pkg install mysql57-client
      echo "y" | pkg install mysql57-server
   fi
   if [ -z "$NO_POSTGRES" ] ; then
      echo "y" | pkg install postgresql12-client
      echo "y" | pkg install postgresql12-server
   fi
   if [ -z "$NO_SOUND" ] ; then
      # echo "y" | pkg install postgresql-server-dev-all
      echo "y" | pkg install gstreamer1
   fi
   if [ -z "$NO_SDL" ] ; then
      echo "y" | pkg install sdl # libsdl1.2-dev
      echo "y" | pkg install sdl_image # libsdl-image1.2-dev
   fi
   if [ -z "$NO_MEMCACHED" ] ; then
      echo "y" | pkg install memcached
   fi
   if [ -z "$NO_READLINE" ] ; then
      echo "y" | pkg install readline
   fi
   if [ -z "$NO_BDB" ] ; then
      # echo "y" | pkg install libdb-dev
      echo "y" | pkg install db18
   fi
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

