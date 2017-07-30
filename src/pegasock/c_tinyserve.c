// This is based on Linux Socket Server by Example by W. Gay

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

// Configuration constants

#define SOCKET_BUFFER_MAX 256
// limited by kernel parameter SOMAXCONN
// /proc/sys/net/core/somaxconn (default 128)
// http://publib.boulder.ibm.com/infocenter/wasinfo/v6r0/index.jsp?topic=/com.ibm.websphere.express.doc/info/exp/ae/tprf_tunelinux.html
// recommends bumping the maximum for this and nextdev_max_backlog to 3000

// Shared with Ada
//
// socket_buffer - string buffer for C to/from Ada strings
// socket_error - error code for last operation

char socket_buffer[SOCKET_BUFFER_MAX];
int socket_error = 0;


// Globals

typedef struct {
  int min_recv_buffer_size;                             // init parameters
  int min_send_buffer_size;
  int socket_queue_length;
  int socket_linger_seconds;
  int max_fd;                                           // highest fd
  int socket_listener;                                  // listen socket fd
  unsigned int len_inet;                                // protocol addr length
  int timeout_secs;                                     // accept timeout
  int timeout_usecs;
  fd_set so_set;                                        // select() fd sets
  fd_set rd_set;
  fd_set wr_set;
} SocketData;


// GET LISTENER SOCKET

int getListenerSocket( SocketData *socket_data ) {
  socket_error = 0;
  return socket_data->socket_listener;
}


//  GET FD SETSIZE
//
// Return the maximum possible number of active connections.
// --------------------------------------------------------------------------

int getFDSetSize() {
  return FD_SETSIZE;
}


// READY SOCKET DATA
//
// Initialize the socket data on startup to read only the listener socket

void ready_socket_data( SocketData *socket_data ) {
  FD_ZERO( &socket_data->so_set );
  FD_SET( socket_data->socket_listener, &socket_data->so_set );
  socket_data->max_fd = socket_data->socket_listener + 1;
}


// PREPARE FOR SELECT
//
// Copy the socket list for reading with select().

void prepare_for_select( SocketData *socket_data ) {
  int z;

  FD_ZERO( &socket_data->rd_set );
  for ( z=0; z<socket_data->max_fd; ++z ) {
     if ( FD_ISSET( z, &socket_data->so_set ) ) {
        FD_SET( z, &socket_data->rd_set );
        FD_SET( z, &socket_data->wr_set );
     }
  }

}


// COUNT_CLIENTS

int count_clients( SocketData *socket_data ) {
  int z;
  int cnt=0;
  for ( z=socket_data->socket_listener+1; z<socket_data->max_fd; ++z ) {
     if ( FD_ISSET( z, &socket_data->so_set ) ) {
        cnt++;
     }
  }
  return cnt;
}


// CLIENT MIGHT NOT BLOCK ON WRITE
//
// True if socket might not block (accept only guarantees that writing one char
// will not block...but more might).

int clientMightNotBlockOnWrite( SocketData *socket_data, int socket_client ) {
   return FD_ISSET( socket_client, &socket_data->wr_set );
} // client_might_not_block_on_write


//  INITIALIZE SERVER
//
// Create a socket (based on the global variables) that can handle
// (accept) new client connections.  The address or IP to listen to
// should be in the buffer.  socket_port is the port to listen to.
//
//  Return the listener socket that clients connect to.

SocketData *initialize_server( int socket_port,
  int min_recv_buffer_size, int min_send_buffer_size,
  int socket_queue_length, int socket_linger_seconds,
  int timeout_secs, int timeout_usecs ) {

  SocketData *socket_data;
  socklen_t optlen = -1;
  int recv_buffer_size = -1;
  int send_buffer_size = -1;
  int default_recv_buffer_size = -1;
  int default_send_buffer_size = -1;
  struct linger so_linger;

  int z = -1;
  struct sockaddr_in adr_inet;
  struct hostent *hp;

  socket_error = 0;

  socket_data = malloc( sizeof( SocketData ) );
  if ( socket_data == NULL ) {
     socket_error = 256;
     return NULL;
  }

  // Record parameters

  socket_data->min_recv_buffer_size = min_recv_buffer_size;
  socket_data->min_send_buffer_size = min_send_buffer_size;
  socket_data->socket_queue_length = socket_queue_length;
  socket_data->socket_linger_seconds = socket_linger_seconds;
  socket_data->timeout_secs = timeout_secs;
  socket_data->timeout_usecs = timeout_usecs;

  if ( !(hp = gethostbyname( socket_buffer ) ) ) {
        socket_error = errno;
        free( socket_data );
	return NULL;
  }
  memset( &adr_inet, 0, sizeof(adr_inet) );                // prepare socket addr
  memcpy( (char *)&adr_inet.sin_addr, (char *)hp->h_addr, hp->h_length );
  adr_inet.sin_port = htons( socket_port );                 // web server port
  adr_inet.sin_family = hp->h_addrtype;              // TCP/IP; open socket

/*
  printf( "Socket: Host %s (%d.%d.%d.%d) Port %d will be where clients connect\n",
           socket_buffer,
	   hp->h_addr_list[0][0],
	   hp->h_addr_list[0][1],
	   hp->h_addr_list[0][2],
	   hp->h_addr_list[0][3],
	   socket_port );
*/

  if ( socket_port > 32767 ) {
     socket_error = 256;
     free( socket_data );
     return NULL;
  }

  socket_data->socket_listener = socket( AF_INET, SOCK_STREAM, 0 );
  if ( socket_data->socket_listener < 0 ) {
     socket_error = errno;
     free( socket_data );
     return NULL;
  }

  // Socket buffer sizes
  // Client sockets will inherit these from accept(2).

  optlen = sizeof( default_send_buffer_size );
  z = getsockopt( socket_data->socket_listener, SOL_SOCKET, SO_SNDBUF,
    &default_send_buffer_size, &optlen );
  if ( z ) default_send_buffer_size = -1;

  optlen = sizeof( default_recv_buffer_size );
  z = getsockopt( socket_data->socket_listener, SOL_SOCKET, SO_SNDBUF,
    &default_recv_buffer_size, &optlen );
  if ( z ) default_recv_buffer_size = -1;

  if ( socket_data->min_send_buffer_size > 0 ) {
     optlen = sizeof( socket_data->min_send_buffer_size );
     z = setsockopt( socket_data->socket_listener, SOL_SOCKET, SO_SNDBUF,
       &min_send_buffer_size, optlen );
     if ( z ) printf( "setsockopt( send size ) failed - %d\n", errno );
  }
  if ( socket_data->min_recv_buffer_size > 0 ) {
     optlen = sizeof( socket_data->min_recv_buffer_size );
     z = setsockopt( socket_data->socket_listener, SOL_SOCKET, SO_SNDBUF,
       &min_recv_buffer_size, optlen );
     if ( z ) printf( "setsockopt( recv size ) failed - %d\n", errno );
  }

  optlen = sizeof( send_buffer_size );
  z = getsockopt( socket_data->socket_listener, SOL_SOCKET, SO_SNDBUF,
    &send_buffer_size, &optlen );
  if ( z ) send_buffer_size = -1;

  optlen = sizeof( recv_buffer_size );
  z = getsockopt( socket_data->socket_listener, SOL_SOCKET, SO_SNDBUF,
    &recv_buffer_size, &optlen );
  if ( z ) recv_buffer_size = -1;

/*
  printf( "Socket send buffer size (bytes): default=%d request=%d actual=%d\n",
    default_send_buffer_size, min_send_buffer_size, send_buffer_size );
  printf( "Socket recv buffer size (bytes): default=%d request=%d actual=%d\n",
    default_recv_buffer_size, min_recv_buffer_size, recv_buffer_size );
*/

  // No error if restarting socket server and address still in use in kernel

  z = 1;
  z = setsockopt( socket_data->socket_listener, SOL_SOCKET, SO_REUSEADDR,
      &z, sizeof( z ) );
  if ( z ) printf( "setsockopt( reuseaddr ) failed - %d\n", errno );

  // Block socket on close if data not all read

  if ( socket_data->socket_linger_seconds > 0 ) {
     so_linger.l_onoff = 1;
     so_linger.l_linger = socket_data->socket_linger_seconds;
     z = setsockopt( socket_data->socket_listener, SOL_SOCKET, SO_LINGER,
         &so_linger, sizeof( so_linger ) );
     if ( z ) printf( "setsockopt( linger ) failed - %d\n", errno );
  }

  // Keep disrupted sockets open for client reconnect

  z = 1;
  z = setsockopt( socket_data->socket_listener, SOL_SOCKET, SO_KEEPALIVE,
      &z, sizeof( z ) );
  if ( z ) printf( "setsockopt( keepalive ) failed - %d\n", errno );


  // Put the socket on the network

  socket_data->len_inet = sizeof( adr_inet );

  z = bind( socket_data->socket_listener, (struct sockaddr *)&adr_inet, socket_data->len_inet );
  if ( z == -1 ) {
     socket_error = errno;
     close( socket_data->socket_listener );
     free( socket_data );
     return NULL;
  }

  // Make it a listener than can receive new clients

  if ( listen( socket_data->socket_listener, socket_data->socket_queue_length ) < 0 ) {
     socket_error = errno;
     close( socket_data->socket_listener );
     free( socket_data );
     return NULL;
  }

  ready_socket_data( socket_data );

  return socket_data;

} // initialize_server


//  MANAGE CONNECTIONS
//
// Wait on all open sockets for a client to connect.  Return the client
// socket, 0 (timeout) or -1 (error).

int manageConnections( SocketData *socket_data ) {
    int client_socket;
    struct sockaddr_in adr_clnt;
    struct timeval socket_timeout;
    int res;
    int c;

    socket_error = 0;
    prepare_for_select( socket_data );
    socket_timeout.tv_sec  = socket_data->timeout_secs;
    socket_timeout.tv_usec = socket_data->timeout_usecs;
    res = select( socket_data->max_fd, &socket_data->rd_set, NULL, NULL, &socket_timeout );
    if ( res < 0 ) {
       socket_error = errno;
       if ( socket_error == EINTR ) {
          client_socket = 0;
       } else {
          printf( "Select failed: %d\n", errno );
          client_socket = -1;
       }
    } else if ( res == 0 ) {
      // zero is a timeout
      client_socket = 0;
    } else if ( FD_ISSET( socket_data->socket_listener, &socket_data->rd_set ) ) {
      // New client connecting - what priority should this be?

retry:
     client_socket = accept( socket_data->socket_listener, (struct sockaddr *)&adr_clnt, &socket_data->len_inet );
     if ( client_socket < 0 ) {
       socket_error = errno;
       if ( socket_error == EINTR ) {
          goto retry;
       } else if ( socket_error == EAGAIN ) {
          // EAGAIN only occurs on non-blocking sockets to indicate no data yet
          socket_error = 0;
       } else {
          printf( "Accept failed: %d\n", errno );
          client_socket = -1;
       }
    } else if ( client_socket > FD_SETSIZE )  {
       printf( "Socket closed: at capacity - FD_SETSIZE exceeded\n" );
       close( client_socket );
       client_socket = -1;
    } else {
        if ( client_socket + 1 > socket_data->max_fd ) {
           socket_data->max_fd = client_socket + 1;
       }
       FD_SET( client_socket, &socket_data->so_set );
       // client_socket = 0; // nothing to do
    }
 } else {
    //printf( "Existing socket\n" );
    // existing client with new data
    // not very efficient here since accept may report multiple
    // clients but I'm only servicing one at a time.
    client_socket = 0;
    for ( c = 0 ; c < socket_data->max_fd ; ++c ) {
        if ( c == socket_data->socket_listener ) // should never occur
           continue;     // but ignore listener
        if ( FD_ISSET( c, &socket_data->rd_set ) )  {
           client_socket = c;
           break;
        }
    }
  }

  return client_socket;

} // get_client_connection


// GET NEXT CLIENT
//
// Get the next client waiting to be handled.

int getNextClient( SocketData *socket_data, int last_client ) {
  int c;

  if ( last_client < 0 ) return -1;
  for ( c = last_client+1; c <= socket_data->max_fd ; c++ ) {
    if ( FD_ISSET( c, &socket_data->rd_set ) ) break;
  }
  if ( c > socket_data->max_fd ) c = -1;
  return c;
} // get_next_client


//  CLOSE CLIENT
//
// Close the client socket.  Clear the client from the select list.

void closeClient( SocketData *socket_data, int socket_client ) {
  int c;

  socket_error = 0;
  shutdown( socket_client, SHUT_RDWR );
  if ( FD_ISSET( socket_client, &socket_data->so_set ) ) {
     FD_CLR( socket_client, &socket_data->so_set );
  }
  close( socket_client );
  // Reduce search set if possible
  for ( c=socket_data->max_fd-1; c >=0 && !FD_ISSET(c,&socket_data->so_set );
      c = socket_data->max_fd-1 ) {
      socket_data->max_fd = c;
  }

} // close_client


// ABORT CLIENT
//
// Close client but do not linger.

void abortClient( SocketData *socket_data, int socket_client ) {
  struct linger so_linger;
  int z;

  so_linger.l_onoff = 1;
  so_linger.l_linger = 0;
  z = setsockopt( socket_client, SOL_SOCKET, SO_LINGER, &so_linger, sizeof( so_linger ) );
  if ( z ) {
     if (errno == EBADF ) {
        printf( "setsockopt( linger ) failed - bad socket or socket already closed?\n" );
     } else {
        printf( "setsockopt( linger ) failed - %d\n", errno );
     }
  }
  closeClient( socket_data, socket_client );
} // abort_client


// SHUTDOWN SERVER

void shutdown_server( SocketData *socket_data ) {
  int c;

  socket_error = 0;
  for ( c = 0 ; c < socket_data->max_fd ; ++c ) {
      if ( FD_ISSET( c, &socket_data->so_set ) )  {
          if ( c != socket_data->socket_listener ) {
             shutdown( c, SHUT_RDWR );
             close( c );
          }
          //abortClient( socket_data, c );
      }
  }
  // close the listener socket
  close( socket_data->socket_listener );
  // free the server description
  free( socket_data );
  socket_data = NULL;
} // shutdown_server


//  GET CLIENT MESSAGE
//
// Receive a string from the client.  If a socket is closed, an empty string
// is returned.
// Note: if buffer becomes full, there will be no \0 at the end.
// --------------------------------------------------------------------------

void get_client_message( int socket_client ) {
  int bytes_read = 0;

  // clear the Ada error code
  socket_error = 0;

retry:
  bytes_read = recv( socket_client, socket_buffer, SOCKET_BUFFER_MAX, 0 );
  if ( bytes_read < 0 ) {
     // handle errors.  If EINTR, retrun so that, as much as possible, the
     // data is read.  Other errors, set the Ada error code.
     if ( errno == EINTR ) goto retry;
     socket_error = errno;
     socket_buffer[0] = '\0';                 // no message
  } else if ( bytes_read < SOCKET_BUFFER_MAX ) {
     // mark end of string but only if buffer is not completely full
     socket_buffer[bytes_read] = '\0';
  }

} // get_client_message


//  SEND CLIENT MESSAGE
//
// Transmit a string to the client. chars must not be zero or an infinite loop
// may occur.
// This could be more efficient if control was returned to Ada on errors or
// partial writes.
// --------------------------------------------------------------------------

void send_client_message( int socket_client, int chars ) {
  int bytes_sent = 0;
  int bytes_left = chars;
  char *buffer   = socket_buffer;

  // clear the Ada error code
  socket_error = 0;

retry:
  // MSG_NOSIGNAL suppresses the broken pipe signal but an error is still
  // returned
#ifdef __APPLE__
  bytes_sent = send( socket_client, buffer, chars, 0 );
#else
  bytes_sent = send( socket_client, buffer, chars, MSG_NOSIGNAL );
#endif

  if ( bytes_sent < 0 ) {
     // handle errors.  If EINTR, retrun so that, as much as possible, the
     // data is sent.  Other errors, set the Ada error code.
     if ( errno == EINTR ) goto retry;
     socket_error = errno;
  } else if ( bytes_left - bytes_sent > 0 ) {
    // if there is more to send, repeat for remainder.  move the buffer ptr
    // forward and decrease the send count
    buffer += bytes_sent;
    bytes_left -= bytes_sent;
    goto retry;
  }


} // send_client_message

