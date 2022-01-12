#!/usr/local/bin/spar
pragma annotate( summary, "add_swap" )
       @( description, "Add a Linux swap file to use memory more efficiently" )
       @( description, "on cloud servers and computers with no or small" )
       @( description, "swap partitions." )
       @( description, "Usage: add_swap [-v] megs" )
       @( author, "Ken O. Burth" )
       @( errors, "1 - usage shown" )
       @( errors, "2 - user is not root" )
       @( errors, "3 - command failed" )
       @( errors, "4 - bad size" );
pragma license( unrestricted );
pragma software_model( shell_script );

procedure add_swap is

-- Constants

swapfile : constant string := "/root/swapfile.dat";

-- Environment variables

type import_string is new string;

LOGNAME : constant import_string := "unknown";
pragma import( shell, LOGNAME );

-- Commands we are using

chmod      : limited command := "/bin/chmod";
dd         : limited command := "/bin/dd";
free       : limited command := "/usr/bin/free";
mkswap     : limited command := "/sbin/mkswap";
rm         : limited command := "/bin/rm";
swapon     : limited command := "/sbin/swapon";

-- Command Options / Arguments

verbose_option : boolean := false;   -- -v/--verbose
size_in_megs   : float;              -- size in megs
blocks         : positive;           -- num 10K blocks

-- Other Variables

command_error : exception with "command exception" use 3;


-- USAGE
--
-- Show the built-in help
-----------------------------------------------------------------------------

procedure usage is
begin
  put( source_info.file ) @ ( " [-v] size-in-bytes" );
  new_line;
  put( "size is rounded to 10K blocks" );
  new_line;
  command_line.set_exit_status( 1 );
end usage;


-- CREATE SWAP FILE
--
-- Create the Swap File
-----------------------------------------------------------------------------

procedure create_swap_file is
begin
  -- Use dd to create a zero-filled file of the right size

  if verbose_option then
     put_line( source_info.source_location &
        ": Creating the swap file" );
  end if;
  dd(
     "if=/dev/zero",
     "of=" & swapfile,
     "bs=10240",
     "count=" & strings.image( blocks ) );
  if $? /= 0 then
     raise command_error with "dd failed...swap file in use or are you out of disk space?";
  end if;

  -- Change the access rights

  chmod( "600", swapfile ); -- TRIGGER EXCEPTION
  if $? /= 0 then
     raise command_error with "chmod failed";
  end if;

  -- Format as a swap file

  mkswap( swapfile );
  if $? /= 0 then
     raise command_error with "mkswap failed...is your size large enough?";
  end if;

exception when command_error =>
  if files.exists( swapfile ) then
     rm( swapfile );
  end if;
  raise;
end create_swap_file;


-- ENABLE SWAP FILE
--
-- Enable the Swap File
-----------------------------------------------------------------------------

procedure enable_swap_file is
begin
  if verbose_option then
     put_line( source_info.source_location &
        ": Enabling the swap file" );
  end if;
  swapon( swapfile );
  if $? /= 0 then
     raise command_error with "swapon failed";
  end if;

  -- If verbose, show free space

  if verbose_option then
     free( "-m" );
  end if;

  put_line( source_info.source_location & ": Ready" );
  command_line.set_exit_status( 0 );
end enable_swap_file;

begin

  -- Process Options

  if $# = 0 or $# > 2 then
     usage;
     return;
  elsif $1 = "-h" or $1 = "--help" then
     usage;
     return;
  elsif $1 = "-v" or $1 = "--verbose" then
     verbose_option;
     size_in_megs := numerics.value( $2 );
  else
     size_in_megs := numerics.value( $1 );
  end if;
  if size_in_megs <= 0 then
     put_line( standard_error, source_info.source_location &
       ": Size in megs must be a positive" );
     command_line.set_exit_status( 4 );
     return;
  end if;

  -- Convert size in megabytes to a unit of 10K blocks

  blocks := numerics.rounding( units.mb2bytes( size_in_megs ) / 10240 );

  -- Sanity Checks

  if LOGNAME /= "root" then
     put_line( standard_error, source_info.source_location &
       ": You must be root" );
     command_line.set_exit_status( 2 );
     return;
  end if;

  create_swap_file;
  enable_swap_file;
end add_swap;


-- VIM editor formatting instructions
-- vim: ft=spar

