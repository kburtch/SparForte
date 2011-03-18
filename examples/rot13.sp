#!/usr/local/bin/bush

pragma annotate( "rot13" );
pragma annotate( "" );
pragma annotate( "Implement a 'rot-13' function (or procedure, class," );
pragma annotate( "subroutine, or other 'callable' object as appropriate" );
pragma annotate( "to your programming environment).  The definition of the" );
pragma annotate( "rot-13 function is to simply replace every letter of the" );
pragma annotate( "ASCII alphabet with the letter which is 'rotated' 13" );
pragma annotate( "characters 'around' the 26 letter alphabet from its" );
pragma annotate( "normal cardinal position (wrapping around from 'z' to" );
pragma annotate( "'a' as necessary). Thus the letters 'abc' become 'nop'" );
pragma annotate( "and so on. Technically rot-13 is a 'monoalphabetic" );
pragma annotate( "substitution cipher' with a trivial 'key'. A proper" );
pragma annotate( "implementation should work on upper and lower case" );
pragma annotate( "letters, preserve case, and pass all non-alphabetic" );
pragma annotate( "characters in the input stream through without" );
pragma annotate( "alteration." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Rot-13" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure rot13 is

  function to_rot13( s : string ) return string is
    ch : character;
    result : string;
  begin
    for i in 1..strings.length( s ) loop
        ch := strings.element( s, i );
        if strings.is_letter( ch ) then
           if (ch in 'A'..'M') or (ch in 'a'..'m' ) then
              ch := strings.val( numerics.pos( ch ) + 13 );
           else
              ch := strings.val( numerics.pos( ch ) - 13 );
           end if;
        end if;
        result := @ & ch;
    end loop;
    return result;
  end to_rot13;

begin
   ? to_rot13( "Big fjords vex quick waltz nymph!" );
end rot13;

