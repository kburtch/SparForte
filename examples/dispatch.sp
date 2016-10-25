#!/usr/local/bin/spar

pragma annotate( summary, "dispatch server-type remote-command" )
       @( param, "server-type - the type of server to run the command on" )
       @( param, "remote-command - the command to run" )
       @( description, "Run a command across a pre-defined set of computers " )
       @( description, "using Secure Shell (SSH)." )
       @( return, "On success, returns status code 0." )
       @( errors, "192 - cannot log into a server" )
       @( errors, "193 - the ssh command failed" )
       @( author, "Ken. O. Burtch" );
pragma license( unrestricted );

procedure dispatch is
  type import_string is new string;

  -- my login account
  LOGNAME : constant import_string := "unknown";
  pragma import( shell, LOGNAME );

  -- node_names is an enumerated list of servers
  -- node_types are types of servers
  -- node_type_array is a list of types for the node_names
  -- node_name_array is a list of hostnames for the node_names

  type node_names is ( host1, host2 );
  type node_types is (application, database, proxy, cache, share, offline );
  type node_type_array is array( host1..host2 ) of node_types;
  type node_name_array is array( host1..host2 ) of string;

  node_name : node_name_array;
  node_type : node_type_array;

  kindStr : string;
  kind    : node_types;

  procedure usage is
    -- show help
  begin
    put( "usage: " ) @ ( source_info.file );
    put_line( " server-type remote-command" );
    new_line;
    put_line( "Run a command across a pre-defined set of computers" );
    put_line( "using Secure Shell (SSH)." );
    new_line;
  end usage;

begin

  -- Make a list of your hosts here

  node_name( host1 ) := "localhost";   node_type( host1 ) := application;
  node_name( host2 ) := "localhost2";  node_type( host2 ) := database;

  -- Parameter handling

  -- There should be two parameters.  Any other number of parameters, or
  -- -h or --help, should show script usage

  command_line.set_exit_status( 0 );                            -- status OK

  if $# /= 2 then                                               -- not 2 params?
     usage;                                                     -- show usage
     return;                                                    -- and quit
  elsif $1 = "-h" or $1 = "--help" then                         -- help request?
     usage;                                                     -- show usage
     return;                                                    -- and quit
  end if;

  -- Convert kind to an item of the enumerated type

  kindStr := $1;
  case kindStr is
  when "application" => kind := application;
  when "database" => kind := database;
  when "proxy" => kind := proxy;
  when "cache" => kind := cache;
  when "share" => kind := share;
  when others =>
       put( standard_error, source_info.file )
            @( standard_error, ": Bad type of server: '" & kindStr & "'" );
       return;
  end case;

  -- Verify machine access: this does not guarantee the machine will
  -- be there but reduces the risk of a machine being down while
  -- running the command.

  for n in arrays.first( node_name )..arrays.last( node_name ) loop
      if node_type( n ) = kind then
         ssh( "-oPreferredAuthentications=publickey",
              LOGNAME & "@" & node_name( n ),
              "exit" );
         if $? /= 0 then
            put( standard_error, source_info.file )
                @( standard_error, ": cannot log into '" & node_name(n) & "'" );
            command_line.set_exit_status( 192 );
            return;
         end if;
      end if;
  end loop;

  -- Run the command sequentially across all the computers of that kind
  -- This can be improved by running ssh in the background and wait-ing
  -- so that the command operate in parallel, but you lose the error
  -- checking of the command results.

  for n in arrays.first( node_name )..arrays.last( node_name ) loop
      if node_type( n ) = kind then
         ? "Running command on " & node_name( n );
         ssh( "-oPreferredAuthentications=publickey",
              LOGNAME & "@" & node_name( n ),
              command_line.argument(2) );
         if $? /= 0 then
            put( standard_error, source_info.file )
                @( standard_error, ": command failed on '" & node_name(n) & "'" );
            command_line.set_exit_status( 193 );
            return;
         end if;
      end if;
  end loop;
end dispatch;

-- VIM editor formatting instructions
-- vim: ft=spar
