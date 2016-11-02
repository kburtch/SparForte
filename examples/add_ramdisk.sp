#!/usr/local/bin/spar
pragma annotate( summary, "add_ramdisk" )
       @( description, "Create a Linux ramdisk of a given size" )
       @( description, "Usage: add_ramdisk [size-in-mb]" )
       @( author, "Ken O. Burth" )
       @( errors, "1 - usage shown" )
       @( errors, "2 - user is not root" )
       @( errors, "3 - command failed" );
pragma license( unrestricted );
pragma software_model( shell_script );

procedure add_ramdisk is

-- Constants

mount_point  : constant string := "/media/ramdisk";
default_size : constant positive := 1; -- one mb

-- Environment variables

type import_string is new string;

LOGNAME : constant import_string := "unknown";
pragma import( shell, LOGNAME );

-- Commands we are using

mkdir : constant command := "/bin/mkdir";
mount : constant command := "/bin/mount";
rmdir : constant command := "/bin/rmdir";

-- Other Variables

command_error : exception with "command exception" use 3;
size  : positive;

-- USAGE
--
-- Show the built-in help
-----------------------------------------------------------------------------

procedure usage is
begin
  put( source_info.file ) @ ( " [size-in-mb]" );
  new_line;
  command_line.set_exit_status( 1 );
end usage;


-- CREATE RAMDISK
--
-- Create the ramfile
-----------------------------------------------------------------------------

procedure create_ramdisk is
  size_option : string;
begin
  -- Make the mount point

  mkdir( mount_point );
  if $? /= 0 then
     raise command_error with "mkdir failed...cannot create the mount point";
  end if;

  -- Calculate the size option

  size_option := strings.image( size );
  size_option := strings.delete( size_option, 1, 1 );
  size_option := "size=" & size_option & "m";

  -- Mount the Ramdisk

  mount( "-t", "tmpfs", "none", mount_point, "-o", size_option );
  if $? /= 0 then
     raise command_error with "mount failed";
  end if;

  -- Change the access rights

exception when command_error =>
  rmdir( mount_point );
  raise;
end create_ramdisk;

begin
  -- Process Options

  if $# > 1 then
     usage;
     return;
  elsif $# = 0 then
     size := default_size;
  else
     size := positive( numerics.value( $1 ) );
  end if;

  -- Sanity Checks

  if LOGNAME /= "root" then
     put_line( standard_error, source_info.source_location &
       ": You must be root" );
     command_line.set_exit_status( 2 );
     return;
  end if;

  create_ramdisk;
end add_ramdisk;

-- VIM editor formatting instructions
-- vim: ft=spar

