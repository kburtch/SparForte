-- A Simple Socket Server
--
-- e.g. To connect to test, telnet <host> <port>
-----------------------------------------------------------------------------

with ada.text_io,
     bush_os;
use  ada.text_io,
     bush_os;

package body pegasock.tinyserve is

-- C Interface
--
-- C is used to avoid the complications of C's macros, etc.

--type aBufferedFile is new integer;

function initialize_server( port : integer;
  min_recv_buffer_size : integer; min_send_buffer_size : integer;
  socket_queue_length : integer; socket_linger_seconds : integer;
  timeout_secs : integer; timeout_usecs : integer ) return aSocketServer;
pragma import( C, initialize_server );

function manageConnections( socket_data : aSocketServer ) return int;
pragma import( C, manageConnections, "manageConnections" );
-- check connections and return the first waiting client with data

function getNextClient( socket_data : aSocketServer; last_client : aSocketFD ) return aSocketFD;
pragma import( C, getNextClient, "getNextClient" );
-- get the next waiting client (or -1 for none)

procedure closeClient( socket_data : aSocketServer; socket_client : aSocketFD );
pragma import( C, closeClient, "closeClient" );
-- graceful close a client connection, waiting for communication to finish

procedure abortClient( socket_data : aSocketServer; socket_client : aSocketFD );
pragma import( C, abortClient, "abortClient" );
-- force close a client connection even if communication isn't finished

function getListenerSocket( socket_data : aSocketServer ) return aSocketFD;
pragma import( C, getListenerSocket, "getListenerSocket" );
-- get the listener socket for the server

function clientMightNotBlockOnWrite( socket_data : aSocketServer; socket_client : aSocketFD ) return integer;
pragma import(C, clientMightNotBlockOnWrite, "clientMightNotBlockOnWrite" );

procedure get_client_message( socket_listener : aSocketFD );
pragma import( C, get_client_message );

procedure send_client_message( socket_listener : aSocketFD; len : integer );
pragma import( C, send_client_message );

procedure shutdown_server( socket_data : aSocketServer );
pragma import( C, shutdown_server );

-- These are C global variables shared with Ada.

min_recv_buffer_size : integer;
pragma import( C, min_recv_buffer_size );

min_send_buffer_size : integer;
pragma import( C, min_send_buffer_size  );

socket_queue_length : integer;
pragma import( C, socket_queue_length  );

socket_linger_seconds : integer;
pragma import( C, socket_linger_seconds );

socket_error : integer;
pragma import( C, socket_error );

socket_buffer : array( 0..255 ) of character;
pragma import( C, socket_buffer );
-- Don't change buffer size without changing C as well


--  MANAGE CONNECTIONS
-- 
-- Monitor all client connections and return the first one that needs to be
-- serviced (that is, has data waiting to be read).  Performed using
-- operating system select syscall.  0 is returned on a timeout.  Errors
-- will throw a data_error exception.
-----------------------------------------------------------------------------

procedure manageConnections( socket_data : aSocketServer; id : out aClientID ) is
-- check connections and return the first waiting client with data
begin
  id := aClientID( manageConnections( socket_data ) );
  if id < 0 then
     raise data_error with OSError( socket_error );
  end if;
end manageConnections;


--  GET NEXT CLIENT
-- 
-- If there are multiple clients waiting for servicing (has data waiting
-- to be read), get the next one waiting (or -1 for none).
-----------------------------------------------------------------------------

procedure getNextClient( socket_data : aSocketServer; id : in out aClientID ) is
begin
  id := aClientID( getNextClient( socket_data, aSocketFD(id) ) );
end getNextClient;


--  ESTABLISH
--
-- Initialize a socket descriptor for the file descriptor (client ID)
-- returned by manageSockets or getNextClient.
-----------------------------------------------------------------------------

procedure establish( client : in out aBufferedSocket; id : aClientID ) is
begin
  if client.fd > 0 then
     raise name_error with "socket descriptor is still open";
  end if;
  client.fd := aSocketFD(id);
  client.eol := Ada.Characters.Latin_1.LF;
  client.eolType := LF;
  if client.readBuffer = null then
     client.readBuffer := new string(1..defaultReadBufferSize);
  end if;
  client.readPos := integer'last;
  client.amountRead := 0;
  client.eof := false;
  client.host := to_unbounded_string( "unknown" );
  client.mode := blocking;
end establish;


--  CLOSE
-- 
-- Gracefully close a client connection, waiting for communication to finish
-- before closing the socket.  This should be used instead of the standard
-- pegasock socket close (perhaps strong typing could enforce this).
-----------------------------------------------------------------------------

procedure close( socket_data : aSocketServer; client : in out aBufferedSocket ) is
-- graceful close a client connection, waiting for communication to finish
begin
  closeClient( socket_data, client.fd );
  client.fd := -1;
  free( client.readBuffer );
end close;


--  DROP
-- 
-- Forcefully close a client connection even if communication isn't finished.
-----------------------------------------------------------------------------
--
procedure drop( socket_data : aSocketServer; client : in out aBufferedSocket ) is
-- force close a client connection even if communication isn't finished
begin
  abortClient( socket_data, client.fd );
  client.fd := -1;
  free( client.readBuffer );
end drop;


--  GET LISTENER SOCKET
--
-- Return the client ID of the socket listening for new connections.
-----------------------------------------------------------------------------

procedure getListenerSocket( socket_data : aSocketServer; id : out aClientID ) is
-- get the listener socket for the server
begin
  id := aClientID( getListenerSocket( socket_data ) );
end getListenerSocket;


--  CLIENT MIGHT NOT BLOCK ON WRITE
--
-- True if socket is not blocked on write.  This does not guarantee that a
-- block will not occur, only that the first byte written will not block.
-----------------------------------------------------------------------------

function clientMightNotBlockOnWrite( socket_data : aSocketServer; client : aBufferedSocket ) return boolean is
begin
  return clientMightNotBlockOnWrite( socket_data, client.fd ) > 0;
end clientMightNotBlockOnWrite;


--  IS OPEN
-- 
-- Technically, >= 0 but tinyserve uses 0 for a timeout or new connection
-----------------------------------------------------------------------------

--function isOpen( fd : aClientID ) return boolean is
--begin
--  return fd > 0;
--end isOpen;


--  IS OPEN
-- 
-- Technically, 0 is stdin but tinyserve uses 0 for a timeout or new connection
------------------------------------------------------------------------------

function isTimeout( fd : aClientID ) return boolean is
begin
  return fd = 0;
end isTimeout;


--  STARTUP TINYSERVE
--
-- Start up the socket server.  host is the host interface to listen to.
-- port is the network port.  The buffer sizes are the operating system
-- buffer sizes (as set by setsockopt).  queue length is how many clients
-- may queue before getting errors.  linger is how long to leave the socket
-- open while the client hasn't finished reading data.  timeout is the
-- select syscall timeout.  Use 0 for default values.
------------------------------------------------------------------------------

procedure startupTinyServe( socket_data : out aSocketServer;
  host : string;
  port : integer;
  min_recv_buffer_size : integer;
  min_send_buffer_size : integer;
  socket_queue_length : integer;
  socket_linger_seconds : integer;
  timeout_secs : integer;
  timeout_usecs : integer ) is
  c_host : string := host & ASCII.NUL;
begin
  for i in 1..host'length loop
      socket_buffer(i-1) := c_host(i);
  end loop;
  socket_data := initialize_server( port, min_recv_buffer_size,
    min_send_buffer_size, socket_queue_length, socket_linger_seconds,
    timeout_secs, timeout_usecs );
end startupTinyServe;


--  SHUTDOWN TINYSERVER
--
-- Shutdown the server by releasing the sockets and freeing memory.
------------------------------------------------------------------------------

procedure shutdownTinyServe( socket_data : aSocketServer ) is
begin
  shutdown_server( socket_data );
end shutdownTinyServe;

end pegasock.tinyserve;

