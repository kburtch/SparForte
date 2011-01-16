#!/usr/local/bin/bush

pragma annotate( "yorn" );
pragma annotate( "" );
pragma annotate( "Obtain a valid Y or N response from the keyboard. The" );
pragma annotate( "keyboard should be flushed, so that any outstanding keypresses" );
pragma annotate( "are removed, preventing any existing Y or N keypress from" );
pragma annotate( "being evaluated. The response should be obtained as soon as" );
pragma annotate( "Y or N are pressed, and there should be no need to press an" );
pragma annotate( "enter key. " );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Keyboard_Input/Obtain_a_Y_or_N_response" );
pragma annotate( "by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure yorn is
  answer : character;
begin
  put( "Your answer? (Y/N) " );
  loop
    answer := inkey;
    case answer is
    when 'Y'|'y' =>
       answer := 'Y';
       exit;
    when 'N'|'n' =>
       answer := 'N';
       exit;
    when others =>
       null;
    end case;
  end loop;
  put_line( answer );
end yorn;

