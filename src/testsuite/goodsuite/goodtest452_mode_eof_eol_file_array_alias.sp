-- testing that text_io.open and text_io.close work with file_type arrays

file_path : constant string := "file_test.tmp";
fa : array(1..1) of file_type;
falias : file_type renames fa(1);
m : file_mode;
b : boolean;

create( falias, out_file, file_path );
reset( falias, in_file );
m := mode( falias );
pragma assert( m = in_file );
b := end_of_file( falias );
pragma assert( b );
b := end_of_line( falias );
pragma assert( not b );
delete( falias );
