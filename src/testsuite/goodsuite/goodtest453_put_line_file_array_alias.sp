-- testing that text_io.open and text_io.close work with file_type arrays

file_path : constant string := "file_test.tmp";
fa : array(1..1) of file_type;
falias : file_type renames fa(1);

create( falias, out_file, file_path );
put_line( falias, "foo" );
delete( falias );
