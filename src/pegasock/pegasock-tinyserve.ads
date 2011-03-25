-- A Simple, non-threading TCP/IP Socket Server
--
-- e.g. To connect to test, telnet <host> <port>
-----------------------------------------------------------------------------
pragma ada_2005;

with system, interfaces.c, ada.strings.unbounded;
use system, interfaces.c, ada.strings.unbounded;

package pegasock.tinyserve is

type aClientID is new int;

type aSocketServer is private;


-- HOUSEKEEPING


procedure startupTinyServe( socket_data : out aSocketServer;
  host : string;
  port : integer;
  min_recv_buffer_size : integer; -- still used?
  min_send_buffer_size : integer;  -- still used?
  socket_queue_length : integer;
  socket_linger_seconds : integer;
  timeout_secs : integer;
  timeout_usecs : integer );
-- Start up the server.  Use "0" for options to get defaults.

procedure shutdownTinyServe( socket_data : aSocketServer );
-- Shutdown the server and free memory


-- CONNECTIONS


procedure manageConnections( socket_data : aSocketServer; id : out aClientID );
-- check connections and return the first waiting client with data.
-- returns closed socket on timeout or nothing no incoming client data

procedure getNextClient( socket_data : aSocketServer; id : in out aClientID );
-- get the next waiting client (or -1 for none)

procedure getListenerSocket( socket_data : aSocketServer; id : out aClientID );
-- get the listener socket for the server

function countClients( socket_data : aSocketServer ) return integer;
pragma import( C, countClients, "count_clients" );
-- return total number of client connections currently open

function getFDSetSize return int;
pragma import( C, getFDSetSize, "getFDSetSize" );
-- return the maximum number of connected clients possible (as defined by
-- the C constant FD_SETSIZE).  This is not the maximum value for a client ID
-- (a file descriptor) since the application may open files while sockets
-- are connecting.


-- I/O


function clientMightNotBlockOnWrite( socket_data : aSocketServer; client : aBufferedSocket ) return boolean;
-- true of socket is not blocked on write.  no guarantee that a block won't
-- occur on the next write

procedure establish( client : in out aBufferedSocket; id : aClientID );

function isTimeout( fd : aClientID ) return boolean;

-- function isOpen( fd : aClientID ) return boolean;

procedure close( socket_data : aSocketServer; client : in out aBufferedSocket );
-- graceful close a client connection, waiting for communication to finish

procedure drop( socket_data : aSocketServer; client : in out aBufferedSocket );
-- force close a client connection even if communication isn't finished


-----------------------------------------------------------------------------
private
-----------------------------------------------------------------------------

type aSocketServer is new System.Address;
-- this is allocated from C

end pegasock.tinyserve;
