#!/usr/local/bin/bush

-- A common interview test from Rosetta Code for testing basic programming
-- skills.

pragma annotate( "file_creation" );
pragma annotate( "" );
pragma annotate( "In this task, the job is to create a new empty file called 'output.txt' of" );
pragma annotate( "size 0 bytes and an empty directory called 'docs'. This should be done" );
pragma annotate( "twice: once "here", i.e. in the current working directory and once in the" );
pragma annotate( "filesystem root. " );
pragma annotate( "translated by Ken O. Burtch" );

procedure file_creation is
   file_handle : file_type;
begin
   create (file_handle, out_file, "output.txt");
   close (file_handle);
   -- directory_operations/create_directory is not implemented in Bush.
   mkdir "docs";

   create (file_handle, out_file, "/output.txt");
   close (file_handle);
   -- directory_operations/create_directory is not implemented in Bush.
   mkdir "/docs";
end file_creation;

