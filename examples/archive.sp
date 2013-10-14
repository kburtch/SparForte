#!/usr/local/bin/spar

pragma annotate( summary, "Archive" );
pragma annotate( description, "A basic directory archive and backup script" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

procedure archive is

  -- Global constants

  expire_period_days : constant natural := 30;
  -- number of days to hold files in the archive directory

  procedure usage is
    -- show help
  begin
    put( "usage: " ) @ ( source_info.file );
    put_line( " source-dir archive-dir" );
    new_line;
    put_line( "Backup files in source directory in the archive directory." );
    put_line( "Subdirectories are not searched.  Archive directory files" );
    put_line( "will be cleaned." );
    new_line;
  end usage;

begin

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

  -- main block that does the work

declare
  source_directory  : constant string := directory_operations.format_pathname( $1 );
  -- first param - convert path to operating system format
  archive_directory : constant string := directory_operations.format_pathname( $2 );
  -- second param - convert to operating system format
  file_name         : string;                                 -- file to bckup
  target_name       : string;                                 -- backup name
  backup_file_list  : string;                                 -- files to bckup
  i                 : natural;
begin

  -- Verify the source and archive directories exist and that they are
  -- directories

  if not files.is_directory( source_directory ) then
     put( standard_error, source_info.file )
         @( standard_error, ": source is not a directory" );
     command_line.set_exit_status( 192 );
     return;
  end if;
  if not files.is_directory( archive_directory ) then
     put( standard_error, source_info.file )
         @( standard_error, ": archive is not a directory" );
     command_line.set_exit_status( 192 );
     return;
  end if;

  -- Since there is no files.is_readable (yet), try to cd to the directory
  -- to make sure SparForte can read the directories.

  if not files.is_readable( source_directory ) then
     put( standard_error, source_info.file )
         @( standard_error, ": cannot access archive directory" );
     command_line.set_exit_status( 192 );
     return;
  end if;

  if not files.is_writable( archive_directory ) then
     put( standard_error, source_info.file )
         @( standard_error, ": cannot access source directory" );
     command_line.set_exit_status( 192 );
     return;
  end if;

  -- Directories look good.  Create a lock file, waiting until any other
  -- copies of the script are finished running.

  lock_files.lock_file( source_info.file & ".lck" );

  -- Expire the archive directory

  cd "$archive_directory" ;
  find . -type f -mtime +"$expire_period_days" -exec rm {} \; ;
  cd - ;

  -- Move to the source directory.  Get a list of files to back up.  Each
  -- line will be separated by a line feed.

  cd "$source_directory" ;
  backup_file_list := `ls -1 ;` ;

  -- Loop through the names of all the files to back up.  strings.field will
  -- return an empty string when the loop has examined all the files.

  i := 1;
  loop
    file_name := strings.field( backup_file_list, i, ASCII.LF );
    exit when file_name = "";

    -- The target name will have the date appended to the base filename.
    -- For example, "test.log" becomes "test-year:month:day.tgz".

    target_name := archive_directory & directory_operations.dir_separator &
      file_name & "-" &
      strings.trim( strings.image( calendar.year( calendar.clock ) ), trim_end.left ) & ":" &
      strings.trim( strings.image( calendar.month( calendar.clock ) ), trim_end.left ) & ":" &
      strings.trim( strings.image( calendar.day( calendar.clock ) ), trim_end.left ) & 
      ".tgz";

    -- If the file is readable, back it up using the tar command

    if files.is_readable_file( file_name ) then
       tar cfz "$target_name" "$file_name" ;
    end if;
    i := @ + 1;
  end loop;

end;

-- Release the lock file and return a successful status

lock_files.unlock_file( source_info.file & ".lck" );
command_line.set_exit_status( 0 );

end archive;

-- VIM editor formatting instructions
-- vim: ft=bush

