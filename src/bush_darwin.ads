------------------------------------------------------------------------------
-- Darwin Imported kernel syscalls / standard C functions                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2015 Free Software Foundation                   --
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
-- This is maintained at http://www.sparforte.com                           --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C, System.Address_To_Access_Conversions;
use  Interfaces.C;

package bush_os is

------------------------------------------------------------------------------
-- General Operating System Declarations
------------------------------------------------------------------------------

directory_delimiter : constant character := '/';
-- O/S pathname directory separator (O/S dependant)

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

type anOpenFlag is new int;
O_RDONLY   : constant anOpenFlag := 0;
O_WRONLY   : constant anOpenFlag := 1;
O_CREAT    : constant anOpenFLag := 16#0200#;
O_TRUNC    : constant anOpenFlag := 16#0400#;
O_APPEND   : constant anOpenFlag := 16#0008#;
O_NONBLOCK : constant anOpenFlag := 16#0004#;
O_SYNC     : constant anOpenFlag := 16#0080#;
-- /usr/include/fcntl.h

type aModeType is new short;

function getpid return aPID;
pragma import( C, getpid );

procedure read( result : out size_t; fd : aFileDescriptor; buffer : in system.address;
  count : size_t );
pragma import( C, read );
pragma import_valued_procedure( read );
procedure readchar( result : out size_t; fd : aFileDescriptor; char : in out character;
  count : long_integer );
pragma import( C, readchar, "read" );
pragma import_valued_procedure( readchar, "read" );

procedure write( result : out size_t; fd : aFileDescriptor; buffer : in system.address;
  count : size_t );
pragma import( C, write );
pragma import_valued_procedure( write );
procedure writechar( result : out size_t; fd : aFileDescriptor; char : in out character;
  count : long_integer );
pragma import( C, writechar, "write" );
pragma import_valued_procedure( writechar, "write" );

function open( path : string; flags : anOpenFlag; mode : aModeType ) return aFileDescriptor;
pragma import( C, open );
-- Darwin is short_integer mode

function close( fd : aFileDescriptor ) return int;
pragma import( C, close );

function fdatasync( fd : aFileDescriptor ) return int;
pragma import( C, fdatasync, "fsync" );
-- Darwin does not support fdatasync...use fsync instead.

function unlink( s : string ) return int;
pragma import( C, unlink );

WHENCE_SEEK_SET : constant integer := 0;
WHENCE_SEEK_CUR : constant integer := 1;
WHENCE_SEEK_END : constant integer := 2;
-- in stdio.h (no WHENCE in Darwin)

function lseek( fd : aFileDescriptor; offset : long_long_integer; whence : integer )
  return long_long_integer;
pragma import( C, lseek );
-- Darwin has long long offset and return

function dup( oldfd : aFileDescriptor ) return aFileDescriptor;
pragma import( C, dup );

function dup2( oldfd, newfd : aFileDescriptor ) return aFileDescriptor;
pragma import( C, dup2 );


------------------------------------------------------------------------------
-- Terminal Control
------------------------------------------------------------------------------

tput_style : constant string := "termcap";

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
type winsz_req is new unsigned_long;
TIOCGWINSZ : constant winsz_req := 16#40087468#; -- Darwin
-- IOCTL codes are unsigned long in Darwin
-- /usr/include/sys/ttycom.h

procedure ioctl_TIOCGWINSZ(
  result : out integer;         -- -1 on failure
  fd     : aFileDescriptor;     -- tty file to use
  req    : winsz_req;           -- TIOCGWINSZ code
  info   : in out winsz_info ); -- pointer to result record
pragma import( C, ioctl_TIOCGWINSZ, "ioctl" );
pragma import_valued_procedure( ioctl_TIOCGWINSZ );
-- get window size

-- Darwin does it this way
-- man 7 termio, /usr/include/sys/termios.h
-- mod types can have binary bit operations
type termios is record
     c_iflag : unsigned_long;   -- input modes
     c_oflag : unsigned_long;   -- output modes
     c_cflag : unsigned_long; -- control modes
     c_lflag : unsigned_long;   -- local modes (including ICANON)
     cc_eof  : character;    -- VEOF = 0
     cc_eol  : character;    -- VEOL = 1
     cc_eol2 : character;    -- VEOL2 = 2
     cc_erase: character;    -- VERASE = 3
     cc_weras: character;    -- VWERASE = 4
     cc_kill : character;    -- VKILL = 5
     cc_reprt: character;    -- VREPRINT = 6
     cc_spare: character;    -- spare = 7
     cc_intr : character;    -- VINTR = 8
     cc_quit : character;    -- VQUIT = 9
     cc_susp : character;    -- VSUSP = 10
     cc_dsusp: character;    -- VDSUSP = 11
     cc_start: character;    -- VSTART = 12
     cc_stop : character;    -- VSTOP = 13
     cc_lnext: character;    -- VLNEXT = 14
     cc_disc : character;    -- VDISCARD = 15
     cc_min  : character;    -- VMIN = 16
     cc_time : character;    -- VTIME = 17
     cc_status: character;    -- VSTATUS = 18
     cc_spare2: character;    -- spare = 19
     c_ispd  : unsigned_long;   -- input speed
     c_ospd  : unsigned_long;   -- output speed
end record;
pragma pack( termios );

HUPCL  : constant unsigned_long := 16#4000#;   -- Hang up on last close
ISIG   : constant unsigned_long := 16#0080#;     -- Enable signals
ICANON : constant unsigned_long := 16#0100#;     -- Canonical input (erase and kill or suspend
--XCASE  : constant unsigned_long := 8#0000004#; -- Canonical upper/lower presentation
ECHO   : constant unsigned_long := 16#0008#;     -- Enable echo
ECHOE  : constant unsigned_long := 16#0002#;     -- Echo ERASE as an error-correcting backspace
ECHOK  : constant unsigned_long := 16#0004#;     -- Echo KILL (FreeBSD ECHOKE)
ECHONL : constant unsigned_long := 16#0010#;     -- Echo '\n'
NOFLSH : constant unsigned_long := 16#80000000#; -- Disable flush after interrupt, quit
TOSTOP : constant unsigned_long := 16#00400000#; --
ECHOCTL: constant unsigned_long := 16#0040#;     -- Echo ctrl chars as char?
IXON   : constant unsigned_long := 16#0200#;    -- Enable start/stop output control
IXANY  : constant unsigned_long := 16#0800#;    -- Enable any character to restart output
IXOFF  : constant unsigned_long := 16#0400#; -- Enable start/stop input control
INLCR  : constant unsigned_long := 16#0040#; -- Map NL to CR on input
IGNCR  : constant unsigned_long := 16#0080#; -- Ignore CR
ICRNL  : constant unsigned_long := 16#0100#; -- Map CR to NL on input

type getattr_req is new unsigned_long;
TCGETATTR : constant getattr_req := 16#40487413#; -- Darwin TIOCGETA

procedure ioctl_getattr( result : out integer;
     fd : aFileDescriptor;
     cmd : getattr_req;
     t : in out termios );
pragma import( C, ioctl_getattr, "ioctl" );
pragma import_valued_procedure( ioctl_getattr );
-- get the attributes of the current tty device

type setattr_req is new unsigned_long;
TCSETATTR : constant setattr_req := 16#80487414#; -- Darwin TIOCSETA

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

-- TODO: none of this works
-- in Darwin, /opt/local/include/cdio/cdio.h (macports)

type cdromplaytrkind_req is new long_integer;
CDROMPLAYTRKIND : constant cdromplaytrkind_req := -2147196159; -- Darwin CDIOCPLAYTRACKS

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

type cdromstop_req is new long_integer;
CDROMSTOP   : constant cdromstop_req := 536896279; -- Darwin CDIOCSTOP

procedure ioctl_cdromstop( result : out integer;
     fid : aFileDescriptor;
     id  : cdromstop_req;
     ignored : in out aDummyParam );
pragma import( C, ioctl_cdromstop, "ioctl" );
pragma import_valued_procedure( ioctl_cdromstop );
-- spin down an CD

type cdromstart_req is new long_integer;
CDROMSTART  : constant cdromstart_req := 536896278; -- Darwin CDIOCSTART

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
-- signals so we can't use it with Unix.

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
-- Broken for GCC 3.x/GNAT 5.x. See C_errno below.

EPERM          : constant integer :=  1; -- Operation not permitted
ENOENT         : constant integer :=  2; -- No such file or directory
ESRCH          : constant integer :=  3; -- No such process
EINTR          : constant integer :=  4; -- Interrupted system call
EIO            : constant integer :=  5; -- Input/output error
ENXIO          : constant integer :=  6; -- Device not configured
E2BIG          : constant integer :=  7; -- Argument list too long
ENOEXEC        : constant integer :=  8; -- Exec format error
EBADF          : constant integer :=  9; -- Bad file descriptor
ECHILD         : constant integer := 10; -- No child processes
EDEADLK        : constant integer := 11; -- Resource deadlock avoided
ENOMEM         : constant integer := 12; -- Cannot allocate memory
EACCES         : constant integer := 13; -- Permission denied
EFAULT         : constant integer := 14; -- Bad address
ENOTBLK        : constant integer := 15; -- Block device required
EBUSY          : constant integer := 16; -- Device busy
EEXIST         : constant integer := 17; -- File exists
EXDEV          : constant integer := 18; -- Cross-device link
ENODEV         : constant integer := 19; -- Operation not supported by device
ENOTDIR        : constant integer := 20; -- Not a directory
EISDIR         : constant integer := 21; -- Is a directory
EINVAL         : constant integer := 22; -- Invalid argument
ENFILE         : constant integer := 23; -- Too many open files in system
EMFILE         : constant integer := 24; -- Too many open files
ENOTTY         : constant integer := 25; -- Inappropriate ioctl for device
ETXTBSY        : constant integer := 26; -- Text file busy
EFBIG          : constant integer := 27; -- File too large
ENOSPC         : constant integer := 28; -- No space left on device
ESPIPE         : constant integer := 29; -- Illegal seek
EROFS          : constant integer := 30; -- Read-only file system
EMLINK         : constant integer := 31; -- Too many links
EPIPE          : constant integer := 32; -- Broken pipe
EDOM           : constant integer := 33; -- Numerical argument out of domain
ERANGE         : constant integer := 34; -- Result too large
EAGAIN         : constant integer := 35; -- Resource temporarily unavailable
EWOULDBLOCK    : constant integer := 35; -- same as EAGAIN in FreeBSD
EINPROGRESS    : constant integer := 36; -- Operation now in progress
EALREADY       : constant integer := 37; -- Operation already in progress
ENOTSOCK       : constant integer := 38; -- Socket operation on non-socket
EDESTADDRREQ   : constant integer := 39; -- Destination address required
EMSGSIZE       : constant integer := 40; -- Message too long
EPROTOTYPE     : constant integer := 41; -- Protocol wrong type for socket
ENOPROTOOPT    : constant integer := 42; -- Protocol not available
EPROTONOSUPPORT: constant integer := 43; -- Protocol not supported
ESOCKTNOSUPPORT: constant integer := 44; -- Socket type not supported
EOPNOTSUPP     : constant integer := 45; -- Operation not supported
EPFNOSUPPORT   : constant integer := 46; -- Protocol family not supported
EAFNOSUPPORT   : constant integer := 47; -- Address family not supported
EADDRINUSE     : constant integer := 48; -- Address already in use
EADDRNOTAVAIL  : constant integer := 49; -- Can't assign requested address
ENETDOWN       : constant integer := 50; -- Network is down
ENETUNREACH    : constant integer := 51; -- Network is unreachable
ENETRESET      : constant integer := 52; -- Network dropped connection on reset
ECONNABORTED   : constant integer := 53; -- Software caused connection abort
ECONNRESET     : constant integer := 54; -- Connection reset by peer
ENOBUFS        : constant integer := 55; -- No buffer space available
EISCONN        : constant integer := 56; -- Socket is already connected
ENOTCONN       : constant integer := 57; -- Socket is not connected
ESHUTDOWN      : constant integer := 58; -- Can't send after socket shutdown
ETOOMANYREFS   : constant integer := 59; -- Too many references: can't splice
ETIMEDOUT      : constant integer := 60; -- Operation timed out
ECONNREFUSED   : constant integer := 61; -- Connection refused
ELOOP          : constant integer := 62; -- Too many levels of symbolic links
ENAMETOOLONG   : constant integer := 63; -- File name too long
EHOSTDOWN      : constant integer := 64; -- Host is down
EHOSTUNREACH   : constant integer := 65; -- No route to host
ENOTEMPTY      : constant integer := 66; -- Directory not empty
EPROCLIM       : constant integer := 67; -- Too many processes
EUSERS         : constant integer := 68; -- Too many users
EDQUOT         : constant integer := 69; -- Disc quota exceeded
ESTALE         : constant integer := 70; -- Stale NFS file handle
EREMOTE        : constant integer := 71; -- Too many levels of remote in path
EBADRPC        : constant integer := 72; -- RPC struct is bad
ERPCMISMATCH   : constant integer := 73; -- RPC version wrong
EPROGUNAVAIL   : constant integer := 74; -- RPC prog. not avail
EPROGMISMATCH  : constant integer := 75; -- Program version wrong
EPROCUNAVAIL   : constant integer := 76; -- Bad procedure for program
ENOLCK         : constant integer := 77; -- No locks available
ENOSYS         : constant integer := 78; -- Function not implemented
EFTYPE         : constant integer := 79; -- Inappropriate file type or format
EAUTH          : constant integer := 80; -- Authentication error
ENEEDAUTH      : constant integer := 81; -- Need authenticator
EPWROFF        : constant integer := 82; -- Device power is off
EDEVERR        : constant integer := 83; -- Device error
EOVERFLOW      : constant integer := 84; -- Value too large to be stored in data type
EBADEXEC       : constant integer := 85; -- Bad executable
EBADARCH       : constant integer := 86; -- Bad CPU type in executable
ESHLIBVERS     : constant integer := 87; -- Shared library version mismatch
EBADMACHO      : constant integer := 88; -- Malformed Macho file
ECANCELED      : constant integer := 89; -- Operation canceled
EIDRM          : constant integer := 90; -- Identifier removed
ENOMSG         : constant integer := 91; -- No message of desired type
EILSEQ         : constant integer := 92; -- Illegal byte sequence
ENOATTR        : constant integer := 93; -- Attribute not found
EBADMSG        : constant integer := 94; -- Bad message
EMULTIHOP      : constant integer := 95; -- Reserved
ENODATA        : constant integer := 96; -- No message available on STREAM
ENOLINK        : constant integer := 97; -- Reserved
ENOSR          : constant integer := 98; -- No STREAM resources
ENOSTR         : constant integer := 99; -- Not a STREAM
EPROTO         : constant integer := 100; -- Protocol error
ETIME          : constant integer := 101; -- STREAM ioctl timeout
--EOPNOTSUPP     : constant integer := 102; -- Operation not supported on socket
ENOPOLICY      : constant integer := 103; -- No such policy registered
ENOTRECOVERABLE: constant integer := 104; -- State not recoverable
EOWNERDEAD     : constant integer := 105; -- Previous owner died
EQFULL         : constant integer := 106; -- Interface output queue is full
ELAST          : constant integer := 106; -- Must be equal largest errno
ELIBBAD        : constant integer := -1; -- Linux error (N/A on Darwin)

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

type aSocketFD is new int;
-- a socket file descriptor is an integer -- man socket
-- make this a new integer for strong typing purposes

type aProtocolFamily is new unsigned_char;
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

type aNetDomain is new int;
PF_INET : constant aNetDomain := 2;

-- The number of the Internet domain
-- Make this a new integer for strong typing purposes

type aInAddr is record
     addr : unsigned := 0;
end record;
pragma warnings( off ); -- hide warning about wasted bits
for aInAddr'size use 96;
pragma warnings( off );
-- A sockaddr record is defined as 16 bytes long (or 96 bits)
-- Request Ada to use 16 bytes to represent this record

type aSocketAddr is record
     len    : unsigned_char;
     family : aProtocolFamily := AF_INET; -- protocol (AF_INET for TCP/IP)
     port   : unsigned_short := 0;  -- theport number (eg 80 for web)
     ip     : aInAddr;              -- IP number
end record;
-- an Internet socket address
-- defined in /usr/include/sys/socket.h

function socket( domain   : aNetDomain;
                 stype    : aSocketType;
                 protocol : aNetProtocol )
return aSocketFD;
pragma import( C, socket );
-- initialize a communication socket.  -1 if error

procedure bind( result : out int; sockfd : aSocketFD;
  sa : in out aSocketAddr; addrlen : unsigned );
pragma import( C, bind );
pragma import_valued_procedure( bind );
-- give socket a name. 0 if successful

procedure connect( result : out int; socket : aSocketFD;
  sa : in out aSocketAddr; addrlen : unsigned );
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
pragma import( C, gethostbyname );
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

function C_is_waiting_file( path : string ) return boolean;
pragma import( C, C_is_waiting_file, "C_is_waiting_file" );
--  True if a file exists, is readable and has data

function C_is_includable_file( path : string ) return boolean;
pragma import( C, C_is_includable_file, "C_is_includable_file" );
--  True if a file exists, is readable, not world writable and has data
-- (that is, that it has permissions for an include file)

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

end bush_os;

