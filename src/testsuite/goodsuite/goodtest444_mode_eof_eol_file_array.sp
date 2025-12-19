-- testing that text_io.open and text_io.close work with file_type arrays

file_path : constant string := "file_test.tmp";
fa : array(1..1) of file_type;
m : file_mode;
b : boolean;

create( fa(1), out_file, file_path );
reset( fa(1), in_file );
m := mode( fa(1) );
pragma assert( m = in_file );
b := end_of_file( fa(1) );
pragma assert( b );
b := end_of_line( fa(1) );
pragma assert( not b );
delete( fa(1) );
