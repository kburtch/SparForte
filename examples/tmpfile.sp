#!/usr/local/bin/spar

pragma annotate( summary, "tmpfile" );
pragma annotate( description, "Create a temporary file, securely and exclusively" );
pragma annotate( description, "(opening it such that there are no possible race" );
pragma annotate( description, "conditions). It's fine assuming local filesystem" );
pragma annotate( description, "semantics (NFS or other networking filesystems can" );
pragma annotate( description, "have signficantly more complicated semantics for" );
pragma annotate( description, "satisfying the 'no race conditions' criteria). The" );
pragma annotate( description, "function should automatically resolve name collisions" );
pragma annotate( description, "and should only fail in cases where permission is" );
pragma annotate( description, "denied, the filesystem is read-only or full, or similar" );
pragma annotate( description, "conditions exist (returning an error or raising an" );
pragma annotate( description, "exception as appropriate to the language/environment)." );
pragma annotate( category, "scripting" );
pragma annotate( see_also, "http://rosettacode.org/wiki/Secure_temporary_file" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma software_model( nonstandard );
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

-- VIM editor formatting instructions
-- vim: ft=spar

