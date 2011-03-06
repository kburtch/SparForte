#!/usr/local/bin/bush

-- From Rosetta Code - http://rosettacode.org/wiki/File_IO

pragma annotate( "filecopy" );
pragma annotate( "" );
pragma annotate( "The job is to create a file called 'output.txt', and place in it" );
pragma annotate( "the contents of the file 'input.txt'." );
pragma annotate( "by Ken O. Burtch" );

procedure filecopy is

begin
  if not files.is_readable( "input.txt" ) then
     put_line( standard_error, source_info.source_location & ": input file is not readable" );
     command_line.set_exit_status( 192 );
     return;
  end if;

  -- With standard shell commands

  cp input.txt output.txt;

  -- Using built-in capabilities

  pragma restriction( no_external_commands );

  declare
    input : file_type;
    output : file_type;
    line : string;
  begin
    create( output, out_file, "output.txt" );
    open( input, in_file, "input.txt" );
    while not end_of_file( input ) loop
       put_line( output, get_line( input ) );
    end loop;
    close( input ) @ ( output );
  end;

end filecopy;

