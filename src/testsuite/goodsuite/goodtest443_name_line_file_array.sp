-- testing that text_io.open and text_io.close work with file_type arrays

file_path : constant string := "file_test.tmp";
fa : array(1..1) of file_type;
s : string;
n : natural;

create( fa(1), out_file, file_path );
s := name( fa(1) );
pragma assert( s = file_path );
n := line( fa(1) );
pragma assert( n = 0 );
delete( fa(1) );
