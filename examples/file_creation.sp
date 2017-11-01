#!/usr/local/bin/spar

pragma annotate( summary, "file_creation" )
              @( description, "In this task, the job is to create a new empty file called 'output.txt' of" )
              @( description, "size 0 bytes and an empty directory called 'docs'. This should be done" )
              @( description, "twice: once 'here', i.e. in the current working directory and once in the" )
              @( description, "filesystem root." )
              @( category, "tutorials" )
              @( author, "Ken O. Burtch" )
              @( see_also, "http://rosettacode.org/wiki/File_Creation" );
pragma license( unrestricted );

pragma software_model( nonstandard );
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

