#!/usr/local/bin/spar

pragma annotate( "bases" );
pragma annotate( "" );
pragma annotate( "Print a number to different bases using bc" );
pragma annotate( "" );
pragma annotate( "based on a shell script by Heiner Steven" );
pragma annotate( "http://www.shelldorado.com/scripts/cmds/base.sh.txt" );
pragma annotate( "by Ken O. Burtch" );

procedure bases is

  procedure print_bases( i : string ) is
    ibase  : string;                              -- base of the input number
    number : string;                                   -- the number, cleaned
    dec    : string;                                       -- test conversion
  begin
    -- determine the input base
    if strings.head( i, 2 ) = "0b" then
       ibase := "2";
    elsif strings.head( i, 2 ) = "0x" then
       ibase := "16";
    elsif strings.head( i, 1 ) = "0" then
       ibase := "8";
    elsif strings.head( i, 1 ) >= "1" and strings.head( i, 1 ) <= "9" then
       ibase := "10";
    else
       put_line( standard_error, source_info.source_location & "unknown number format" );
       command_line.set_exit_status( 192 );
       return;
    end if;
    -- strip off the type and convert hex chars to uppercase
    number := `echo $i |  sed -e "s/^0[bBxX]//" | tr "[a-f]" "[A-F]";`;
    -- convert to base 10 to ensure it's valid
    dec := `echo "ibase=$ibase; $number" | bc;`;
    -- if we got a valid result, show the number in other bases
    if strings.length( dec ) > 0 then
       if strings.head( dec, 1 ) >= "0" and strings.head( dec, 1 ) <= "9" then
          echo "obase=16; \"hex=\"; $dec" | bc;
          echo "obase=10; \"dec=\"; $dec" | bc;
          echo "obase=8;  \"oct=\"; $dec" | bc;
          echo "obase=2;  \"bin=\"; $dec" | bc;
       end if;
    end if;
  end print_bases;

  procedure usage is
  begin
    put( source_info.file );
    put( " - print number to different bases" );
    new_line;
    put_line( "usage: " & source_info.file & " [number ...]" );
    new_line;
    put_line( "If no number is given, the numbers are read from standard input." )
           @( "  A number may be" )
           @( "  binary (base 2)		starting with 0b (i.e. 0b1100)" )
           @( "  octal (base 8)		starting with 0  (i.e. 014)" )
           @( "  hexadecimal (base 16)	starting with 0x (i.e. 0xc)" )
           @( "  decimal			otherwise (i.e. 12)" );
  end usage;

begin
  command_line.set_exit_status( 0 );

  if command_line.argument_count = 0 then
     usage;
     return;
  end if;

  for arg in 1..command_line.argument_count loop
      if command_line.argument( arg ) = "-h" then
         usage;
         return;
      elsif command_line.argument( arg ) = "--help" then
         usage;
         return;
      else
         print_bases( command_line.argument( arg ) );
      end if;
  end loop;

end bases;

