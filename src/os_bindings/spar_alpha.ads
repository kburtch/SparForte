------------------------------------------------------------------------------
-- Linux Imported kernel syscalls / standard C functions                    --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  This is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C, System.Address_To_Access_Conversions;
use  Interfaces.C;

package spar_os is


------------------------------------------------------------------------------
-- General Operating System Declarations
------------------------------------------------------------------------------

directory_delimiter : constant character := '/';
-- O/S pathname directory separator (O/S dependant)

eol_characters : constant string := "" & ASCII.LF;
-- O/S end-of-line characters in text files

type unsigned32 is mod 2**32;
-- mod type allows boolean operations on bits

type byte is new short_short_integer range -128..127;
for byte'size use 8;
-- 8-bit signed byte

type aPID is new integer;
-- a process ID

function linux_system( s : string ) return integer;
pragma import( C, linux_system, "system" );
-- used for BUSH system() function


------------------------------------------------------------------------------
-- Files
------------------------------------------------------------------------------

type aFileDescriptor is new integer;
stdin  : constant aFileDescriptor := 0;
stdout : constant aFileDescriptor := 1;
stderr : constant aFileDescriptor := 2;

type anOpenFlag is new integer;
O_RDONLY   : constant anOpenFlag := 0;
O_WRONLY   : constant anOpenFlag := 1;
O_CREAT    : constant anOpenFLag := 8#100#;
O_TRUNC    : constant anOpenFlag := 8#1000#;
O_APPEND   : constant anOpenFlag := 8#2000#;
O_NONBLOCK : constant anOpenFlag := 8#4000#;
-- /usr/include/bits/fcntl.h

function getpid return aPID;
pragma import( C, getpid );

procedure read( result : out long_integer; fd : aFileDescriptor; char : in out character;
  count : long_integer );
pragma import( C, read );
pragma import_valued_procedure( read );

procedure write( result : out long_integer; fd : aFileDescriptor; char : in out character;
  count : long_integer );
pragma import( C, write );
pragma import_valued_procedure( write );

function open( path : string; flags : anOpenFlag; mode : integer ) return aFileDescriptor;
pragma import( C, open );

procedure close( fd : aFileDescriptor );
pragma import( C, close );

function unlink( s : string ) return integer;
pragma import( C, unlink );

WHENCE_SEEK_SET : constant integer := 0;
WHENCE_SEEK_CUR : constant integer := 1;
WHENCE_SEEK_END : constant integer := 2;

function lseek( fd : aFileDescriptor; offset : long_integer; whence : integer )
  return long_integer;
pragma import( C, lseek );

function dup( oldfd : aFileDescriptor ) return aFileDescriptor;
pragma import( C, dup );

function dup2( oldfd, newfd : aFileDescriptor ) return aFileDescriptor;
pragma import( C, dup2 );

type mode_t is new unsigned_long;

function umask( mask :mode_t ) return mode_t;
pragma import( C, umask );


------------------------------------------------------------------------------
-- Terminal Control  
------------------------------------------------------------------------------

tput_style : constant string := "terminfo";

function isatty( fd : aFileDescriptor ) return integer;
pragma import( C, isatty );
-- standard C library function returns 1 (true) if file is a tty
-- ioctl variations

type winsz_info is record
  row    : short_integer;
  col    : short_integer;
  xpixel : short_integer;
  ypixel : short_integer;
end record;
type winsz_req is new integer;
TIOCGWINSZ : constant winsz_req := 1074295912; -- Linux/Alpha

procedure ioctl_TIOCGWINSZ(
  result : out integer;         -- -1 on failure
  fd     : aFileDescriptor;     -- tty file to use
  req    : winsz_req;           -- TIOCGWINSZ code
  info   : in out winsz_info ); -- pointer to result record
pragma import( C, ioctl_TIOCGWINSZ, "ioctl" );
pragma import_valued_procedure( ioctl_TIOCGWINSZ );
-- get window size

-- man 7 termio, /usr/include/bits/termios.h
-- mod types can have binary bit operations
type localflags is new unsigned32;
type controlflags is new unsigned32;
type inputflags is new unsigned32;
type outputflags is new unsigned32;
type resarray is array ( 1..14 ) of character; -- was 15 on Intel
pragma pack( resarray );
-- Linux/Alpha has different ordering for these entries
type termios is record
     c_iflag : inputflags;   -- input modes
     c_oflag : unsigned32;   -- output modes
     c_cflag : controlflags; -- control modes
     c_lflag : localflags;   -- local modes (including ICANON)
     cc_eof  : character;    -- VEOF = 4
     cc_eol  : character;    -- VEOL = 11
     cc_eol2 : character;    -- VEOL2 = 16
     cc_erase: character;    -- VERASE = 2
     cc_weras: character;    -- VWERASE = 14
     cc_kill : character;    -- VKILL = 3
     cc_reprt: character;    -- VREPRINT = 12
     cc_swtc : character;    -- VSWTC = 7
     cc_intr : character;    -- VINTR = 0
     cc_quit : character;    -- VQUIT = 1
     cc_susp : character;    -- VSUSP = 10
     cc_missing : character; -- Missing on Alpha
     cc_start: character;    -- VSTART = 8
     cc_stop : character;    -- VSTOP = 9
     cc_lnext: character;    -- VLNEXT = 15
     cc_disc : character;    -- VDISCARD = 13
     cc_min  : character;    -- VMIN = 6
     cc_time : character;    -- VTIME = 5
     cc_res  : resarray;     -- 15 more, unused
     c_line  : character;    -- line discipline
     c_ispd  : unsigned32;   -- input speed
     c_ospd  : unsigned32;   -- output speed
     c_line1 : character;    -- line discipline?
     c_line2 : character;    -- line discipline?
     c_line3 : character;    -- line discipline?
end record;
pragma pack( termios );

HUPCL  : constant controlflags := 16#0040000#; -- Hang up on last close
ISIG   : constant localflags := 16#0000080#; -- Enable signals
ICANON : constant localflags := 16#0000100#; -- Canonical input (erase and kill or suspend
XCASE  : constant localflags := 16#0004000#; -- Canonical upper/lower presentation
ECHO   : constant localflags := 16#0000008#; -- Enable echo
ECHOE  : constant localflags := 16#0000002#; -- Echo ERASE as an error-correcting backspace
ECHOK  : constant localflags := 16#0000004#; -- Echo KILL
ECHONL : constant localflags := 16#0000010#; -- Echo '\n'
NOFLSH : constant localflags := 16#8000000#; -- Disable flush after interrupt, quit
TOSTOP : constant localflags := 16#0400000#; -- 
ECHOCTL: constant localflags := 16#0000040#; -- Echo ctrl chars as char?
IXON   : constant inputflags := 8#0001000#; -- Enable start/stop output control
IXANY  : constant inputflags := 8#0004000#; -- Enable any character to restart output
IXOFF  : constant inputflags := 8#0002000#; -- Enable start/stop input control
INLCR  : constant inputflags := 8#0000100#; -- Map NL to CR on input
IGNCR  : constant inputflags := 8#0000200#; -- Ignore CR
ICRNL  : constant inputflags := 8#0000400#; -- Map CR to NL on input

type getattr_req is new integer;
TCGETATTR : constant getattr_req := 1076655123; -- Linux/Alpha TCGETS (= tcgetattr)

procedure ioctl_getattr( result : out integer;
     fd : aFileDescriptor;
     cmd : getattr_req;
     t : in out termios );
pragma import( C, ioctl_getattr, "ioctl" );
pragma import_valued_procedure( ioctl_getattr );
-- get the attributes of the current tty device

type setattr_req is new integer;
TCSETATTR : constant setattr_req := -2144570348; -- Linux/Alpha TCSETS (= tcsetattr)

procedure ioctl_setattr( result : out integer;
     fd : aFileDescriptor;
     cmd : setattr_req;
     t : in out termios );
pragma import( C, ioctl_setattr, "ioctl" );
pragma import_valued_procedure( ioctl_setattr );
-- set the attributes of the current tty device

function tcdrain( fd : aFileDescriptor ) return integer;
pragma import( C, tcdrain );
-- flush a tty file (eg. standard output)


------------------------------------------------------------------------------
-- Sound Hardware
------------------------------------------------------------------------------

-- various CDROM ioctl functions as mentioned in the
-- CDROM documentation in /usr/doc/kernel... and
-- /usr/include/linux/cdrom.h

type cdromplaytrkind_req is new integer;
CDROMPLAYTRKIND : constant cdromplaytrkind_req := 16#5304#;

type cdrom_ti is record
     start_track, start_index : byte;
     end_track, end_index : byte;
end record;
pragma pack( cdrom_ti );

procedure ioctl_playtrkind( result : out integer;
     fid : aFileDescriptor;
     cmd : cdromplaytrkind_req;
     info : in out cdrom_ti );
pragma import( C, ioctl_playtrkind, "ioctl" );
pragma import_valued_procedure( ioctl_playtrkind );
-- play a range of tracks on an audio CD

type aDummyParam is new integer;
-- define this as a separate type to make sure nothing
-- important is used as a third parameter to ioctl

type cdromstop_req is new integer;
CDROMSTOP   : constant cdromstop_req := 16#5307#;

procedure ioctl_cdromstop( result : out integer;
     fid : aFileDescriptor;
     id  : cdromstop_req;
     ignored : in out aDummyParam );
pragma import( C, ioctl_cdromstop, "ioctl" );
pragma import_valued_procedure( ioctl_cdromstop );
-- spin down an CD

type cdromstart_req is new integer;
CDROMSTART  : constant cdromstart_req := 16#5308#;

procedure ioctl_cdromstart( result : out integer;
     fid : aFileDescriptor;
     id  : cdromstart_req;
     ignored : in out aDummyParam );
pragma import( C, ioctl_cdromstart, "ioctl" );
pragma import_valued_procedure( ioctl_cdromstart );
-- spin up a CD


------------------------------------------------------------------------------
-- Directories
------------------------------------------------------------------------------

procedure getcwd( buffer : in out string; buffer_size : long_integer );
pragma import( C, getcwd );
-- get the current working directory

function chdir( path : string ) return integer;
pragma import( C, chdir );
-- change the current working directory

type aLinuxPath is array( 1..1024 ) of character;
type aPathPtr is access all aLinuxPath;


------------------------------------------------------------------------------
-- Temporary Files
------------------------------------------------------------------------------

procedure mkstemp( result : out aFileDescriptor; template : in out string );
pragma import( C, mkstemp );
pragma import_valued_procedure( mkstemp );
-- create a temp file path


------------------------------------------------------------------------------
-- Processes
------------------------------------------------------------------------------

function execv( path : string; C_args : system.address ) return integer;
pragma import( C, execv );
-- run a command with the specified arguments.  The current process
-- will be replaced so always fork before this command.  path is a
-- null-terminated string.  C_args is a list of parameters, including
-- the command name, ending with a null address.  This is similar
-- to GNAT.OS_Lib spawn, but spawn doesn't like to be interrupted by
-- signals so we can't use it with Linux.

function fork return aPID;
pragma import( C, fork );
-- create a new process

procedure wait( pid : out aPID; status : in out integer );
pragma import( C, wait );
pragma import_valued_procedure( wait );
-- wait for child processes to finish

type waitOptions is new integer;

WNOHANG   : constant waitOptions := 1;
WUNTRACED : constant waitOptions := 2;

procedure waitpid( pid : out aPID; in_pid : aPID; stat_loc : in out integer;
options : waitOptions );
pragma import( C, waitpid );
pragma import_valued_procedure( waitpid );
-- wait for a specific process to complete


------------------------------------------------------------------------------
-- Operating System Errors
------------------------------------------------------------------------------

--errno : integer;
--pragma import( C, errno );
-- standard error number variable
-- Broken for GCC 3.x/GNAT 5.x.  See C_errno below.

EPERM   : constant integer := 1;      -- Not super-user
ENOENT  : constant integer := 2;      -- No such file or directory
ESRCH   : constant integer := 3;      -- No such process
EINTR   : constant integer := 4;      -- interrupted system call
EIO     : constant integer := 5;      -- I/O error
ENXIO   : constant integer := 6;      -- No such device or address
E2BIG   : constant integer := 7;      -- Arg list too long
ENOEXEC : constant integer := 8;      -- Exec format error
EBADF   : constant integer := 9;      -- Bad file number
ECHILD  : constant integer := 10;     -- No children
EAGAIN  : constant integer := 11;     -- No more processes
ENOMEM  : constant integer := 12;     -- Not enough core
EACCES  : constant integer := 13;     -- Permission denied
EFAULT  : constant integer := 14;     -- Bad address
EBUSY   : constant integer := 16;     -- Mount device busy
EEXIST  : constant integer := 17;     -- File exists
EXDEV   : constant integer := 18;     -- Cross-device link
ENODEV  : constant integer := 19;     -- No such device
ENOTDIR : constant integer := 20;     -- Not a directory
EISDIR  : constant integer := 21;     -- Is a directory
EINVAL  : constant integer := 22;     -- Invalid argument
ENFILE  : constant integer := 23;     -- File table overflow
EMFILE  : constant integer := 24;     -- Too many open files
ENOTTY  : constant integer := 25;     -- Not a typewriter
EFBIG   : constant integer := 27;     -- File too large
ENOSPC  : constant integer := 28;     -- No space left on device
ESPIPE  : constant integer := 29;     -- Illegal seek
EROFS   : constant integer := 30;     -- Read only file system
EMLINK  : constant integer := 31;     -- Too many links
EPIPE   : constant integer := 32;     -- Broken pipe
EDEADLK : constant integer := 45;     -- A deadlock would occur
ENOLCK  : constant integer := 46;     -- System record lock table was full
EILSEQ  : constant integer := 47;     -- Illegal byte sequence
ELIBBAD : constant integer := 80;     -- Bad Library / Can't Run Interpreter
ENOTEMPTY : constant integer := 247;    -- Directory not empty
ENAMETOOLONG : constant integer := 248;    -- File name too long
ENOSYS  : constant integer := 251;    -- Function not implemented

type anErrorBuffer is new string( 1..256 );
type anErrorPtr is access all anErrorBuffer;

function strerror( i : integer ) return anErrorPtr;
pragma import( C, strerror );
-- return an error message for error code i

------------------------------------------------------------------------------
-- Networking
--
-- Socket related definitions
--
-- These are the kernel calls and types we need to create and use a basic
-- TCP/IP (Internet) socket.
--
-- type aSocketFD is new aFileDescriptor;
-- in gnat 3.13 & 3.14 gives "Valued_Procedure has no effect for convention Ada"
-- pragma convention will not override this message, so we'll resort to this:
------------------------------------------------------------------------------

type aSocketFD is new integer;
-- a socket file descriptor is an integer -- man socket
-- make this a new integer for strong typing purposes

type aProtocolFamily is new unsigned_short;
AF_INET : constant aProtocolFamily := 2;

-- Internet protocol PF_Net defined as 2 in
-- /usr/include/sys/socket.h
-- Make this a new integer for strong typing purposes

type aSocketType is new int;
SOCK_STREAM : constant aSocketType := 1;

-- this is for a steady connection.  Defined as 1 in
-- /usr/include/linux/socket.h
-- Make this a new integer for strong typing purposes

type aNetProtocol is new int;
IPPROTO_TCP : constant aNetProtocol := 6;

-- The number of the TCP/IP protocol
-- TCP protocol defined as 6 in /etc/protocols
-- See man 5 protocols
-- Make this a new integer for strong typing purposes

type aNetDomain is new integer;
PF_INET : constant aNetDomain := 2;

-- The number of the Internet domain
-- Make this a new integer for strong typing purposes

type aInAddr is record
     addr : unsigned := 0;
end record;
pragma warnings( off ); -- hide warning about wasted bits
for aInAddr'size use 96;
pragma warnings( off );
-- A sockaddr_in record is defined as 16 bytes long (or 96 bits)
-- Request Ada to use 16 bytes to represent this record

type aSocketAddr is record
     family : aProtocolFamily := AF_INET; -- protocol (AF_INET for TCP/IP)
     port   : unsigned_short := 0;  -- the port number (eg 80 for web)
     ip     : aInAddr;              -- IP number
end record;
-- an Internet socket address
-- defined in /usr/src/linux/include/linux/socket.h
-- and /usr/src/linux/include/linux/in.h

function socket( domain   : aNetDomain;
                 stype    : aSocketType;
                 protocol : aNetProtocol )
return aSocketFD;
pragma import( C, socket );
-- initialize a communication socket.  -1 if error

procedure bind( result : out int; sockfd : aSocketFD;
  sa : in out aSocketAddr; addrlen : int );
pragma import( C, bind );
pragma import_valued_procedure( bind );
-- give socket a name. 0 if successful

procedure Connect( result : out int; socket : aSocketFD;
  sa : in out aSocketAddr; addrlen : int );
pragma import( C, connect );
pragma import_valued_procedure( connect );
-- connect to a (Internet) server.  0 if successful

package addrListPtrs is new System.Address_To_Access_Conversions( System.Address );
-- We need to use C pointers with the address list because this is
-- a pointer to a pointer in C.  This will allow us to dereference
-- the C pointers in Ada.

subtype addrListPtr is System.Address;
-- easier to read than System.Address

type aHostEnt is record
     h_name      : System.Address;    -- pointer to offical name of host
     h_aliases   : System.Address;    -- pointer to alias list
     h_addrtype  : int     := 0;      -- host address type (PF_INET)
     h_length    : int     := 0;      -- length of address
     h_addr_list : addrListPtr;       -- pointer to list IP addresses
                                      -- we only want first one
end record;
-- defined in man gethostbyname

package HEptrs is new System.Address_To_Access_Conversions( aHostEnt );
-- Again, we need to work with C pointers here
subtype aHEptr is System.Address;
-- and this is easier to read
use HEptrs;
-- use makes = (equals) visible

function getHostByName( cname : string ) return aHEptr;
pragma import( C, getHostByName );
-- look up a host by it's name, returning the IP number

function htons( s : unsigned_short ) return unsigned_short;
pragma import( C, htons );
-- acronym: host to network short -- on Intel x86 platforms,
-- switches the byte order on a short integer to the network
-- Most Significant Byte first standard of the Internet

procedure memcpy( dest, src : System.Address; numbytes : int );
pragma import( C, memcpy);
-- Copies bytes from one C pointer to another.  We could probably
-- use unchecked_conversion, but the C examples use this.


------------------------------------------------------------------------------
-- Pipes
------------------------------------------------------------------------------

type aPipeEnd is ( outOfPipe, intoPipe );
type aPipe is array (aPipeEnd) of aFileDescriptor;
pragma pack( aPipe );

procedure pipe( result : out integer; thePipe : in out aPipe );
pragma import( C, pipe );
pragma import_valued_procedure( pipe );
-- create a new pipe


------------------------------------------------------------------------------
-- Environment Variables
------------------------------------------------------------------------------

function putenv( assignment : string ) return integer;
pragma import( C, putenv );
-- export an environment variable in the form VAR=VAL & ASCII.NUL

function unsetenv( assignment : string ) return integer;
pragma import( C, unsetenv );
-- remove a variable from the environment in the form of VAR & ASCII.NUL

------------------------------------------------------------------------------
-- C "glue" Functions
--
-- These are declared in c_os.c.  These include stat() and signal handlers.
------------------------------------------------------------------------------

function C_errno return integer;
pragma import( C, C_errno, "C_errno" );
procedure C_reset_errno;
pragma import( C, C_reset_errno, "C_reset_errno" );
--  Gnat 5.x won't import status properly.  These C wrapper functions are
-- a workaround.

function C_WEXITSTATUS( waitpid_status : integer ) return integer;
pragma import( C, C_WEXITSTATUS, "C_WEXITSTATUS" );
-- convert a waitpid_status to a status code from 0 to 255

function C_is_executable_file( path : string ) return boolean;
pragma import( C, C_is_executable_file, "C_is_executable_file" );
--  True if a file can be executed by the shell

function C_is_executable( path : string ) return boolean;
pragma import( C, C_is_executable, "C_is_executable" );
--  True if a file or special file can be executed by the shell

function C_is_readable_file( path : string ) return boolean;
pragma import( C, C_is_readable_file, "C_is_readable_file" );
--  True if a file can be read by the shell

function C_is_readable( path : string ) return boolean;
pragma import( C, C_is_readable, "C_is_readable" );
--  True if a regular or special file and be read by the shell

function C_is_includable_file( path : string ) return boolean;
pragma import( C, C_is_includable_file, "C_is_includable_file" );
--  True if a file exists, is readable, not world writable and has data
-- (that is, that it has permissions for an include file)

function C_is_waiting_file( path : string ) return boolean;
pragma import( C, C_is_waiting_file, "C_is_waiting_file" );
--  True if a file exists, is readable and has data

function C_is_secure_dir( path : string ) return boolean;
pragma import( C, C_is_secure_dir, "C_is_secure_dir" );
--  True if a dir exists, is readable, not world writable.

function C_file_length( path : string ) return long_integer;
pragma import( C, C_file_length, "C_file_length" );
--  Return length of file

procedure C_file_modify_time( path : string; year, month, day, seconds : out integer );
pragma import( C, C_file_modify_time, "C_file_modify_time" );
--  Return modify time of the file

procedure C_file_change_time( path : string; year, month, day, seconds : out integer );
pragma import( C, C_file_change_time, "C_file_change_time" );
--  Return change time of the file

procedure C_file_access_time( path : string; year, month, day, seconds : out integer );
pragma import( C, C_file_access_time, "C_file_access_time" );
--  Return access time of the file

procedure C_day_of_week( wday : out integer; year, month, day : integer );
pragma import( C, C_day_of_week, "C_day_of_week" );
--  Return modify time of the file

function C_install_sigint_handler( flag : system.address ) return boolean;
pragma import( C, C_install_sigint_handler, "C_install_sigint_handler" );
--  Mark an Ada boolean variable that will be TRUE if SIGINT occurs

function C_install_sigchld_handler( flag : system.address ) return boolean;
pragma import( C, C_install_sigchld_handler, "C_install_sigchld_handler" );
--  Mark an Ada boolean variable that will be TRUE if SIGCHLD occurs

function C_install_sigwinch_handler( flag : system.address ) return boolean;
pragma import( C, C_install_sigwinch_handler, "C_install_sigwinch_handler" );
--  Mark an Ada boolean variable that will be TRUE if SIGWINCH occurs

end spar_os;

