#!/usr/local/bin/bush

pragma annotate( "tmpfile" );
pragma annotate( "" );
pragma annotate( "Create a temporary file, securely and exclusively" );
pragma annotate( "(opening it such that there are no possible race" );
pragma annotate( "conditions). It's fine assuming local filesystem" );
pragma annotate( "semantics (NFS or other networking filesystems can" );
pragma annotate( "have signficantly more complicated semantics for" );
pragma annotate( "satisfying the 'no race conditions' criteria). The" );
pragma annotate( "function should automatically resolve name collisions" );
pragma annotate( "and should only fail in cases where permission is" );
pragma annotate( "denied, the filesystem is read-only or full, or similar" );
pragma annotate( "conditions exist (returning an error or raising an" );
pragma annotate( "exception as appropriate to the language/environment)." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Secure_temporary_file" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure tmpfile is
   temp : file_type;
   contents : string;
begin
   ? "Creating a temporary file";
   create( temp );
   put_line( temp,  "Hello World");

   ? "Reading a temporary file";
   reset( temp, in_file);
   contents := get_line( temp );
   put_line( "File contains: " & contents );  

   ? "Discarding a temporary file";
   close( temp );
end tmpfile;

