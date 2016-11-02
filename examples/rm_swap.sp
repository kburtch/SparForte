#!/usr/local/bin/spar
pragma annotate( summary, "rm_swap" )
       @( description, "Remove a Linux swap file added with add_swap.sp" )
       @( description, "Usage: rm_swap [-v]" )
       @( author, "Ken O. Burth" )
       @( errors, "1 - usage shown" )
       @( errors, "2 - user is not root" )
       @( errors, "3 - command failed" );
pragma license( unrestricted );
pragma software_model( shell_script );

procedure rm_swap is

-- Constants

swapfile : constant string := "/root/swapfile.dat";

-- Environment variables

type import_string is new string;

LOGNAME : constant import_string := "unknown";
pragma import( shell, LOGNAME );

-- Commands we are using

swapoff    : constant command := "/sbin/swapoff";
rm         : constant command := "/bin/rm";

-- Command Options / Arguments

verbose_option : boolean := false;   -- -v/--verbose

-- Other Variables

command_error : exception with "command exception" use 3;


-- USAGE
--
-- Show the built-in help
-----------------------------------------------------------------------------

procedure usage is
begin
  put( source_info.file ) @ ( " [-v]" );
  new_line;
  command_line.set_exit_status( 1 );
end usage;


-- CREATE SWAP FILE
--
-- Create the Swap File
-----------------------------------------------------------------------------

procedure delete_swap_file is
begin
  -- Use dd to create a zero-filled file of the right size

  if verbose_option then
     put_line( source_info.source_location &
        ": Deleteing the swap file" );
  end if;
  swapoff( swapfile );
  if $? /= 0 then
     raise command_error with "swapoff failed...is swap not in use?";
  end if;

  -- Change the access rights

  rm( swapfile );
end delete_swap_file;

begin

  -- Process Options

  if $# > 1 then
     usage;
     return;
  elsif $# = 1 then
     if $1 = "-h" or $1 = "--help" then
        usage;
        return;
     elsif $1 = "-v" or $1 = "--verbose" then
        verbose_option;
     end if;
  end if;

  -- Sanity Checks

  if LOGNAME /= "root" then
     put_line( standard_error, source_info.source_location &
       ": You must be root" );
     command_line.set_exit_status( 2 );
     return;
  end if;

  delete_swap_file;
end rm_swap;


-- VIM editor formatting instructions
-- vim: ft=spar

