#!/usr/local/bin/spar

pragma annotate( summary, "luhn test of credit card numbers" )
       @( description, "The Luhn test is used by some credit card companies to " )
       @( description, "distinguish valid credit card numbers from what could be a random selection of digits." )
       @( category, "algorithms" )
       @( see_also, "https://rosettacode.org/wiki/Luhn_test_of_credit_card_number" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure luhn is

  bad_digit : exception;

  -- return true if the card number passes the luhn test

  function is_luhn( card_number : string) return boolean is
    card_num_len : constant natural := strings.length( card_number );
    checksum: natural := 0;
    ch : character;
  begin
    for i in reverse 1..card_num_len loop
      ch := strings.element( card_number, i );
      if strings.is_digit( ch ) then
         declare
            ord : constant natural := numerics.pos(ch);
         begin
            if ((card_num_len-1) and (i-1) ) /= 0 then
               checksum := @ + ord;
            else
               checksum := @ + numerics.floor(ord / 5) + ((2*ord) mod 10);
            end if;
         end;
      else
        raise bad_digit;
      end if;
    end loop;
    return checksum mod 10 = 0;
  end is_luhn;

  -- check a credit card and display the result

  procedure check_card( card_number : string ) is
  begin
     put( card_number )
       @( ": " )
       @( is_luhn( card_number ) );
     new_line;
  end check_card;

begin
  check_card("49927398716");
  check_card("49927398717");
  check_card("1234567812345678");
  check_card("1234567812345670");
end luhn;
