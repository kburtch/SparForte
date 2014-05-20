#!/usr/local/bin/spar

pragma annotate( summary, "file_creation" );
pragma annotate( description, "In this task, the job is to create a new empty file called 'output.txt' of" );
pragma annotate( description, "size 0 bytes and an empty directory called 'docs'. This should be done" );
pragma annotate( description, "twice: once 'here', i.e. in the current working directory and once in the" );
pragma annotate( description, "filesystem root. " );
pragma annotate( see_also, "http://rosettacode.org/wiki/File_Creation" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure file_creation is
   file_handle : file_type;
begin
   create (file_handle, out_file, "output.txt");
   close (file_handle);
   directory_operations.make_dir( "docs" );

   create (file_handle, out_file, "/output.txt");
   close (file_handle);
   directory_operations.make_dir( "/docs" );
end file_creation;

-- VIM editor formatting instructions
-- vim: ft=spar

