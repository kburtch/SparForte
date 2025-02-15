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
      echo "  --without-locate      do not install GNU locate"
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
  --without-locate)
      NO_LOCATE=1
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
     echo $@
     sudo -u root dnf install -q -y $@
  else
     echo $@
     dnf install -q -y $@
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

pacman_install () {
  if [ -n "$HAS_SUDO" ] ; then
     sudo pacman --sync --needed --quiet --noconfirm $@
  else
     pacman --sync --needed --quiet --noconfirm $@
  fi
}


# Detect the Linux distribution
# ----------------------------------------------------------------------------

# CentOS/Red hat

RHVERSION=""
if test -f "/etc/redhat-release" ; then
   DISTRO="redhat"
   TMP=`grep -F " 7." "/etc/redhat-release"`
   if [ -n "$TMP" ] ; then
      RHVERSION="7"
   fi
   TMP=`grep -F " 8." "/etc/redhat-release"`
   if [ -n "$TMP" ] ; then
      RHVERSION="8"
   fi
   TMP=`grep -F " 9." "/etc/redhat-release"`
   if [ -n "$TMP" ] ; then
      RHVERSION="9"
   fi
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
   TMP=`grep -F Ubuntu /etc/issue`
   if [ "$TMP" != "" ] ; then
      DISTRO="ubuntu"
   fi
   TMP=`grep -F Mint /etc/issue`
   if [ "$TMP" != "" ] ; then
      DISTRO="ubuntu"
   fi
   TMP=`grep -F SUSE /etc/issue`
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

# Arch Linux

if [ -f "/etc/arch-release" ] ; then
   DISTRO="arch"
fi

# Alpine
# Run as root because Alpine is intended for containers

if [ -f "/etc/alpine-release" ] ; then
   DISTRO="alpine"
   if [ "$LOGNAME" != "root" ] ; then
      echo "Please run as the root user"
      exit 192
   fi
fi

# Install software dependences
# ----------------------------------------------------------------------------

case "$DISTRO" in
alpine)
   set -e
   apk add ncurses-terminfo
   apk add make
   apk add bzip2
   apk add git
   apk add openssl
   apk add build-base # include GCC and libraries
   apk add gcc-gnat
   if [ -z "$NO_LOCATE" ] ; then
      apk add findutils-locate
   fi
   if [ -z "$NO_SOUND" ] ; then
      apk add gstreamer-dev
   fi
   if [ -z "$NO_BDB" ] ; then
      echo "At the time this script was written, Alpine has no BDB support"
      echo "Please use the --without-bdb option"
      exit 192
   fi
   if [ -z "$NO_MYSQL" ] ; then
      apk add mariadb
      apk add mariadb-client
      apk add mariadb-connector-c-dev
   fi
   if [ -z "$NO_POSTGRES" ] ; then
      apk add postgresql-dev
      apk add postgresql-client
   fi
   if [ -z "$NO_SDL" ] ; then
      echo "At the time this script was written, Alpine has no SDL support"
      echo "Please use the --without-sdl --without-opengl options"
      exit 192
   fi
   if [ -z "$NO_MEMCACHED" ] ; then
      apk add memcached
   fi
   if [ -z "$NO_READLINE" ] ; then
      apk add readline-dev
   fi
   if [ -z "$NO_PCRE" ] ; then
      apk add pcre-dev
   fi
   set +e
   ;;

arch )
   # Arch Linux does not distinguish between "dev" and core versions of a
   # package.  Endeavor OS installs some of these by default.
   set -e
   # Endeavor OS has these but a container may not
   pacman_install make
   pacman_install git
   if [ -z "$NO_LOCATE" ] ; then
      pacman_install plocate
   fi
   #
   pacman_install bzip2
   pacman_install gcc-ada
   pacman_install bc
   if [ -z "$NO_SOUND" ] ; then
      pacman_install gstreamer
   fi
   # BDB
   if [ -z "$NO_BDB" ] ; then
      echo "There is an AUR note for 2021 that libdb is no longer"
      echo "building due to an error.  If this fails to install,"
      echo " provision and configure using the --without-bdb option"
      pacman_install libdb
   fi
   if [ -z "$NO_MYSQL" ] ; then
      pacman_install mariadb
      pacman_install mariadb-clients
   fi
   if [ -z "$NO_POSTGRES" ] ; then
      pacman_install postgresql
   fi
   if [ -z "$NO_SDL" ] ; then
      pacman_install sdl_image
      if [ -z "$NO_OPENGL" ] ; then
         pacman_install mesa
         pacman_install glu
      fi
   fi
   if [ -z "$NO_MEMCACHED" ] ; then
      pacman_install memcached
   fi
   if [ -z "$NO_READLINE" ] ; then
      pacman_install readline
   fi
   if [ -z "$NO_PCRE" ] ; then
      # Note: pcre2 works but Arch renames the package and
      # breaks the C code.  We fall back to pcre version 1.
      pacman_install pcre
   fi
   #sudo -u root apt-get -q -y install libselinux-dev
   #sudo -u root apt-get -q -y install libdb-dev
   #sudo -u root apt-get -q -y install libmysqlclient-dev
   #sudo -u root apt-get -q -y install postgresql-client
   #sudo -u root apt-get -q -y install postgresql-server-dev-all
   #sudo -u root apt-get -q -y install libsdl1.2-dev
   # "autoremove" - prune and remove unnecessary packages
   sudo paccache -r
   sudo paccache -ruk0
   yay -Yc
   set +e
   ;;

redhat )
   if [ -n "$HAS_SUDO" ] ; then
      sudo -u root yum list installed | grep -F epel-release >/dev/null
   else
      yum list installed | grep -F epel-release >/dev/null
   fi
   if [ $? -eq 1 ] ; then
      set -e
      if [ -n "$HAS_SUDO" ] ; then
         if [ "$RHVERSION" != "7" ] ; then
            sudo dnf config-manager --set-enabled
	        sudo dnf install https://dl.fedoraproject.org/pub/epel/epel-release-latest-9.noarch.rpm
	     else
            sudo -u root rpm -Uvh http://download.fedoraproject.org/pub/epel/7/x86_64/Packages/e/epel-release-latest-7.noarch.rpm
	     fi
      else
         if [ "$RHVERSION" != "7" ] ; then
            dnf config-manager --set-enabled
            dnf install http://download.fedoraproject.org/pub/epel/7/x86_64/Packages/e/epel-release-latest-7.noarch.rpm
	     else
            rpm -Uvh http://download.fedoraproject.org/pub/epel/7/x86_64/Packages/e/epel-release-latest-7.noarch.rpm
	     fi
      fi
   fi
   # Containers may not have these
   yum_install make
   #
   yum_install bzip2
   yum_install gcc-gnat
   yum_install git
   yum_install bc
   if [ -z "$NO_LOCATE" ] ; then
      yum_install mlocate
   fi
   if [ -z "$NO_SOUND" ] ; then
      yum_install gstreamer1
      yum_install gstreamer1-devel
   fi
   if [ -z "$NO_BDB" ] ; then
      yum_install libdb-devel
   fi
   if [ -z "$NO_MYSQL" ] ; then
      yum_install mariadb
      yum_install mariadb-server
      # Unfortunately, dnf doesn't return an error status for something not
      # found
      TMP=`dnf search mariadb-connector-c-devel 2>&1 | grep -F "No match"`
      if [ -z "$TMP" ] ; then
	 # Newer versions of Red Hat
         yum_install mariadb-connector-c-devel
      else
	 # Older versions of Red Hat
         yum_install mariadb-devel
      fi
   fi
   if [ -z "$NO_POSTGRES" ] ; then
      yum_install postgresql
      yum_install postgresql-devel
      yum_install postgresql-server
   fi
   if [ -z "$NO_SDL" ] ; then
      if [ "$RHVERSION" != "9" ] ; then
	 echo "Please provision using --without-sdl.  SDL1.2 is not included "
	 echo "with Red Hat 9"
	 exit 1
         yum_install SDL
         # Does not exist with Red Hat 9 but is also included with SDL
         yum_install SDL-devel
         yum_install SDL_image
      fi
      if [ -z "$NO_OPENGL" ] ; then
         # On Red Hat 9, this will error because SDL-devel does not exist
         # SparForte will still build but not SDL will not work.
         yum_install SDL_image-devel
         yum_install mesa-libGL-devel
         yum_install mesa-libGLU-devel
      fi
   fi
   if [ -z "$NO_MEMCACHED" ] ; then
      yum_install memcached
   fi
   if [ -z "$NO_READLINE" ] ; then
      yum_install readline-devel
   fi
   set +e
   ;;
suse)
   set -e
   zypper_install make
   zypper_install gcc-ada
   zypper_install git
   zypper_install libopenssl-devel
   if [ -z "$NO_LOCATE" ] ; then
      zypper_install mlocate
   fi
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
   if [ -z "$NO_PCRE" ] ; then
      zypper_install pcre-devel
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
   if [ -z "$NO_LOCATE" ] ; then
      apt_install locate
   fi
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
   apt_install gcc
   set +e
   apt_install libssl1.1
   if [ $? -ne 0 ] ; then
      apt_install libssl3
   fi
   set -e
   if [ -z "$NO_LOCATE" ] ; then
      apt_install locate
   fi
   if [ -z "$NO_BDB" ] ; then
      apt_install libdb-dev
   fi
   if [ -z "$NO_MYSQL" ] ; then
      set +e
      apt_install libmariadb-dev-compat # R Pi 4
      if [ $? -ne 0 ] ; then
         # Raspberry Pi 3 and older
         apt_install libmariadbclient-dev
      fi
      set -e
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
   if [ -z "$NO_PCRE" ] ; then
      apt_install libpcre3-dev
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

if [ -z "$NO_LOCATE" ] ; then
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
fi

# Done
# ----------------------------------------------------------------------------

echo "OK"

