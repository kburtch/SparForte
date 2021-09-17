#!/usr/local/bin/spar

pragma annotate( summary, "write_ownership" )
       @( description, "A demonstration of the ownership of global variables" )
       @( description, "shared by functions in the same expression." )
       @( errors, "This script will fail" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure contracts is

  not_even_error  : exception with "value is not even" use 255;
  too_large_error : exception with "value is too large" use 255;

  type even_natural is new abstract natural
    affirm
      raise not_even_error when even_natural mod 2 = 1;
    end affirm;

  subtype even_two_digit_natural is even_natural
    affirm
      raise too_large_error when even_two_digit_natural > 99;
    end affirm;

  num : even_two_digit_natural;

begin

 -- ERROR: 99 is not even number.

  num := 99;

end contracts;

-- VIM editor formatting instructions
-- vim: ft=spar

