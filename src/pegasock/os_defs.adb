
--  PEGASOFT SOCKETS
--
-- Linux/*NIX basic operating system file and socket routines.  The purpose
-- of this package is to work around the differences between C and Ada
-- strings, hide the worst C-specific details and to add exception handling.
-----------------------------------------------------------------------------
pragma ada_2005;

package body os_defs is

function OSerror( e : integer ) return string is
-- return an OS error message for error number e
  lastchar : natural := 0;
  ep       : anErrorPtr;
begin
  ep := strerror( e );
  for i in ep.all'range loop
      if ep(i) = ASCII.NUL then
         lastchar := i-1;
         exit;
      end if;
  end loop;
  return string( ep( 1..lastchar ) );
end OSerror;


begin
  -- When initializing this module, validate the O/S constants defined here
  -- against GNAT / GCC Ada's beliefs.  Comment these out if
  -- System.OS_Constants is not available.  These only take effect when
  -- debugging is enabled with the -gnata gnatmake flag.
  pragma assert( EINTR = System.OS_Constants.EINTR );
  pragma assert( EAGAIN = System.OS_Constants.EAGAIN );
  pragma assert( EWOULDBLOCK = System.OS_Constants.EWOULDBLOCK );
  pragma assert( EALREADY = System.OS_Constants.EALREADY );
  -- pragma assert( O_RDONLY = System.OS_Constants.O_RDONLY );
  -- pragma assert( O_WRONLY = System.OS_Constants.O_WRONLY );
  -- pragma assert( O_CREAT = System.OS_Constants.O_CREAT );
  -- pragma assert( O_TRUNC = System.OS_Constants.O_TRUNC );
  -- pragma assert( O_APPEND = System.OS_Constants.O_APPEND );
  -- pragma assert( O_NONBLOCK = System.OS_Constants.O_NONBLOCK );
  pragma assert( AF_INET = System.OS_Constants.AF_INET );
  pragma assert( SOCK_STREAM = System.OS_Constants.SOCK_STREAM );
  pragma assert( IPPROTO_TCP = System.OS_Constants.IPPROTO_TCP );
  null;
end os_defs;
