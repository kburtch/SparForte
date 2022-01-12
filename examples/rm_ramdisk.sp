#!/usr/local/bin/spar
pragma annotate( summary, "rm_ramdisk" )
       @( description, "Remove a Linux ramdisk created by add_ramdisk.sp" )
       @( description, "Usage: rm_ramdisk" )
       @( author, "Ken O. Burth" )
       @( errors, "1 - usage shown" )
       @( errors, "2 - user is not root" )
       @( errors, "3 - command failed" );
pragma license( unrestricted );
pragma software_model( shell_script );

procedure rm_ramdisk is

-- Constants

mount_point  : constant string := "/media/ramdisk";

-- Environment variables

type import_string is new string;

LOGNAME : constant import_string := "unknown";
pragma import( shell, LOGNAME );

-- Commands we are using

umount : limited command := "/bin/umount";
rmdir  : limited command := "/bin/rmdir";

-- Other Variables

command_error : exception with "command exception" use 3;

-- USAGE
--
-- Show the built-in help
-----------------------------------------------------------------------------

procedure usage is
begin
  put( source_info.file );
  new_line;
  command_line.set_exit_status( 1 );
end usage;


-- CREATE RAMDISK
--
-- Create the ramfile
-----------------------------------------------------------------------------

procedure delete_ramdisk is
begin
  -- Unmount the drive

  umount( mount_point );
  if $? /= 0 then
     raise command_error with "umount failed...cannot remove the mount point";
  end if;
exception when command_error =>
  rmdir( mount_point );
  raise;
end delete_ramdisk;

begin
  -- Process Options

  if $# /= 0 then
     usage;
     return;
  end if;

  -- Sanity Checks

  if LOGNAME /= "root" then
     put_line( standard_error, source_info.source_location &
       ": You must be root" );
     command_line.set_exit_status( 2 );
     return;
  end if;

  delete_ramdisk;
end rm_ramdisk;

-- VIM editor formatting instructions
-- vim: ft=spar

