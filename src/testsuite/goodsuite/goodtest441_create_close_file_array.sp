-- testing that text_io.open and text_io.close work with file_type arrays

file_path : constant string := "file_test.tmp";
fa : array(1..1) of file_type;

create( fa(1), out_file, file_path );
close( fa(1) );
rm ( file_path );
