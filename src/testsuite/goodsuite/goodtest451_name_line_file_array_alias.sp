-- testing that text_io.open and text_io.close work with file_type arrays

file_path : constant string := "file_test.tmp";
fa : array(1..1) of file_type;
falias : file_type renames fa(1);
s : string;
n : natural;

create( falias, out_file, file_path );
s := name( falias );
pragma assert( s = file_path );
n := line( falias );
pragma assert( n = 0 );
delete( falias );
