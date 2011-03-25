
--  PEGASOFT SOCKETS
--
-- Linux/*NIX basic operating system file and socket routines.  The purpose
-- of this package is to work around the differences between C and Ada
-- strings, hide the worst C-specific details and to add exception handling.
-- These routines throw exceptions but do not write errors to standard
-- error.
-----------------------------------------------------------------------------
pragma ada_2005;

with ada.unchecked_deallocation,
     ada.characters.latin_1,
     gnat.utf_32,
     ada.IO_Exceptions,
     ada.strings.unbounded,
     interfaces.c,
     bush_os;
use  ada.strings.unbounded,
     interfaces.c,
     bush_os;

package pegasock is

-- Example:
--
--   mysock := establish( to_unbounded_string( "localhost" ), 80 );
--   put( mysock, "GET /index.html HTTP/1.0" & ASCII.CR & ASCII.LF );
--   put( mysock, "Accept: text/html" & ASCII.CR & ASCII.LF );
--   put( mysock, ASCII.CR & ASCII.LF );
--   contentLength := 0;
-- 
--   put_line( "HTTP header: " );
--   loop
--     get( mysock, s );
--     put_line( to_string( s ) );
--     exit when length( s ) = 2;
--     if slice( s, 1, 15 ) = "Content-Length:" then
--        declare
--          temp : unbounded_string := tail( s, length( s ) - 15 );
--        begin
--          delete( temp, length(temp)-1, length( temp ) );
--          contentLength := integer'value( to_string( temp ) );
--        end;
--     end if;
--   end loop;
-- 
--   if contentLength > 0 then
--     put_line( "HTTP content: " );
--     get( mysock, contentLength, s );
--     put_line( to_string( s ) );
--   end if;

-- The Descriptors
--
-- These are limited private because they contain pointers and O/S file
-- descriptors.  These are not tagged records (objects) because we want
-- to hide these low-level constructs to prevent direct socket access.

type aBufferedFile is limited private;
type aBufferedSocket is limited private;
type aBufferedBroadcast is limited private;  -- not yet implemented

--  FILE MODE
--
-- synchronized - routine will block until data is written by the
--                hardware (a more extreme form of blocking)
-- blocking (normal) - writes will block if the operating system buffer is
--                     full
-- nonblocking - routine will raise a fileutils_wouldblock exception
--               if routine would block
-----------------------------------------------------------------------------

type aFileMode is (blocking, nonblocking, synchronous);

type aSocketMode is (blocking, nonblocking );

--  FILE ACCESS
--
-- Common Linux/UNIX file access permission values
-----------------------------------------------------------------------------

type aFileAccess is ( Acesss_666, Access_644, Access_640, Access_600 );

-- EOL TYPES
--
-- Common end-of-line delimiters for servers, NUL (ASCII NUL (zero), LF
-- (ASCII line feed), CR (ASCII carriage return).
-----------------------------------------------------------------------------

type anEOLType is ( NUL, LF, CRLF, CR, LFCR );

-- Exceptions

name_error           : exception renames Ada.IO_Exceptions.Name_Error;
data_error           : exception renames Ada.IO_Exceptions.Data_Error;
fileutils_wouldblock : exception;


-----------------------------------------------------------------------------
-- UTILITIES
-----------------------------------------------------------------------------

function getEOL( fd : aBufferedFile ) return anEOLType;
procedure setEOL( fd : in out aBufferedFile; eolType : anEOLType );
function isOpen( fd : aBufferedFile ) return boolean;
function isEOF( fd : aBufferedFile ) return boolean;
procedure setReadBufferSize( fd : in out aBufferedFile; size : short_integer );

function getEOL( fd : aBufferedSocket ) return anEOLType;
procedure setEOL( fd : in out aBufferedSocket; eolType : anEOLType );
function isOpen( fd : aBufferedSocket ) return boolean;
function isEOF( fd : aBufferedSocket ) return boolean;
procedure setReadBufferSize( fd : in out aBufferedSocket; size : short_integer );

-----------------------------------------------------------------------------
-- FILES
--
-- The basic functionality is already found in Ada.Text_IO.  Provided here
-- primarily to match up with socket functionality.
-----------------------------------------------------------------------------

procedure open( fd : out aBufferedFile; name : string; mode : aFileMode := blocking; perms : aFileAccess := Access_644 );
procedure overwrite( fd : out aBufferedFile; name : string; mode : aFileMode := blocking; perms : aFileAccess := Access_644 );
procedure append( fd : out aBufferedFile; name : string; mode : aFileMode := blocking; perms : aFileAccess := Access_644 );

function name( fd : aBufferedFile ) return unbounded_string;
function mode( fd : aBufferedFile ) return aFileMode;

procedure get( fd : in out aBufferedFile; ch : out character );
procedure get( fd : in out aBufferedFile; s : out unbounded_string );
procedure get( fd : in out aBufferedFile; bytes : positive; s : out unbounded_string );
procedure get( fd : in out aBufferedFile; ch : out wide_character );
procedure get( fd : in out aBufferedFile; ch : out wide_wide_character );

procedure new_line( fd : in out aBufferedFile );
procedure put( fd : aBufferedFile; c : character );
procedure put( fd : aBufferedFile; s : string );
procedure put( fd : aBufferedFile; s : unbounded_string );
procedure put_line( fd : aBufferedFile; s : string );
procedure put_line( fd : aBufferedFile; s : unbounded_string );
procedure wide_put( fd : aBufferedFile; s : wide_string );
procedure wide_wide_put( fd : aBufferedFile; s : wide_wide_string );

procedure flush( fd : aBufferedFile );

procedure close( fd : in out aBufferedFile );
procedure delete( fd : in out aBufferedFile ); -- Ada symantics

-----------------------------------------------------------------------------
-- SOCKETS
--
-- TCP/IP socket routines.
-----------------------------------------------------------------------------

procedure establish( fd : out aBufferedSocket; host : unbounded_string; port : integer; mode : aSocketMode := blocking );

function host( fd : aBufferedSocket ) return unbounded_string;
function port( fd : aBufferedSocket ) return integer;
function mode( fd : aBufferedSocket ) return aSocketMode;

procedure get( fd : in out aBufferedSocket; ch : out character );
procedure get( fd : in out aBufferedSocket; s : out unbounded_string );
procedure get( fd : in out aBufferedSocket; bytes : positive; s : out unbounded_string );
procedure get( fd : in out aBufferedSocket; ch : out wide_character );
procedure get( fd : in out aBufferedSocket; ch : out wide_wide_character );

procedure new_line( fd : in out aBufferedSocket );
procedure put( fd : aBufferedSocket; c : character );
procedure put( fd : aBufferedSocket; s : string );
procedure put( fd : aBufferedSocket; s : unbounded_string );
procedure put_line( fd : aBufferedSocket; s : string );
procedure put_line( fd : aBufferedSocket; s : unbounded_string );
procedure wide_put( fd : aBufferedSocket; s : wide_string );
procedure wide_wide_put( fd : aBufferedSocket; s : wide_wide_string );

procedure flush( fd : aBufferedSocket );

procedure close( fd : in out aBufferedSocket );

-----------------------------------------------------------------------------
-- UDP SOCKETS
-----------------------------------------------------------------------------
-- not yet written

-- establish
-- put (via send)
-- get (via recv)
-- close

-----------------------------------------------------------------------------
-- OTHER
-----------------------------------------------------------------------------

procedure FileToSocket( socket : out aBufferedSocket; file : aBufferedFile );

-----------------------------------------------------------------------------
-- Aliases with underscored names
-----------------------------------------------------------------------------

subtype a_file_mode is aFileMode;
subtype a_socket_mode is aSocketMode;
subtype a_file_access is aFileAccess;
subtype an_eol_type is anEOLType;

subtype a_file_descriptor is aBufferedFile;
subtype a_socket_descriptor is aBufferedSocket;

function get_eol( fd : aBufferedFile ) return anEOLType renames getEOL;
procedure set_eol ( fd : in out aBufferedFile; eolType : anEOLType ) renames setEOL;
function is_open( fd : aBufferedFile ) return boolean renames isOpen;
function is_eof( fd : aBufferedFile ) return boolean renames isEOF;
procedure set_read_buffer_size( fd : in out aBufferedFile; size : short_integer ) renames setReadBufferSize;

function get_eol( fd : aBufferedSocket ) return anEOLType renames getEOL;
procedure set_eol( fd : in out aBufferedSocket; eolType : anEOLType ) renames setEOL;
function is_open( fd : aBufferedSocket ) return boolean renames isOpen;
function is_eof( fd : aBufferedSocket ) return boolean renames isEOF;
procedure set_read_buffer_size( fd : in out aBufferedSocket; size : short_integer ) renames setReadBufferSize;

pragma inline( new_line );

function C_pegasock_errno return integer;
pragma import( C, C_pegasock_errno, "C_pegasock_errno" );
procedure C_pegasock_reset_errno;
pragma import( C, C_pegasock_reset_errno, "C_pegasock_reset_errno" );
--  Gnat 5.x won't import status properly.  These C wrapper functions are
--  -- a workaround and are located in c_os.c

function OSerror( e : integer ) return string;


-----------------------------------------------------------------------------
private
-----------------------------------------------------------------------------


-- Application I/O Buffer

defaultReadBufferSize : constant integer := 128;
type readBufferPtr is access all string;
procedure free is new Ada.Unchecked_Deallocation( string, readBufferPtr );

type aBufferedFile is record
  fd  : aFileDescriptor := -1;
  eol : character := Ada.Characters.Latin_1.LF;
  eolType : anEolType := LF;
  readBuffer : readBufferPtr := new string(1..defaultReadBufferSize);
  readPos : integer := integer'last;
  amountRead : size_t := 0;
  eof : boolean := false;
  name : unbounded_string := to_unbounded_string( "unknown" );
  mode : aFileMode;
end record;

type aBufferedSocket is record
  fd  : aSocketFD := -1;
  eol : character := Ada.Characters.Latin_1.LF;
  eolType : anEolType := LF;
  readBuffer : readBufferPtr := new string(1..defaultReadBufferSize);
  readPos : integer := integer'last;
  amountRead : size_t := 0;
  eof : boolean := false;
  port : integer := 0;
  host : unbounded_string := to_unbounded_string( "unknown" );
  mode : aSocketMode := blocking;
end record;

type aBufferedBroadcast is record
  fd  : aFileDescriptor := -1;
  -- not yet implemented
end record;

end pegasock;
