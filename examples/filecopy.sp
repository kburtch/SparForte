#!/usr/local/bin/spar

pragma annotate( summary, "filecopy" );
pragma annotate( description, "The job is to create a file called 'output.txt', and place in it" );
pragma annotate( description, "the contents of the file 'input.txt'." );
pragma annotate( see_also, "http://rosettacode.org/wiki/File_IO" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

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
    inputfile : file_type;
    outputfile : file_type;
  begin
    create( outputfile, out_file, "output.txt" );
    open( inputfile, in_file, "input.txt" );
    while not end_of_file( inputfile ) loop
       put_line( outputfile, get_line( inputfile ) );
    end loop;
    close( inputfile ) @ ( outputfile );
  end;

end filecopy;

-- VIM editor formatting instructions
-- vim: ft=spar


