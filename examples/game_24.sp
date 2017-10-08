#!/usr/local/bin/spar

pragma annotate( summary, "game_24" )
       @( description, "Write a program that randomly chooses and displays " )
       @( description, "four digits, each from 1..9 (inclusive) with " )
       @( description, "repetitions allowed." )
       @( description, "" )
       @( description, "The program should prompt for the player to enter " )
       @( description, "an arithmetic expression using just those, and " )
       @( description, "all of those four digits, used exactly once each. " )
       @( description, "The program should check then evaluate the " )
       @( description, "expression." )
       @( description, "" )
       @( description, "The goal is for the player to enter an expression " )
       @( description, "that (numerically) evaluates to 24. " )
       @( description, "" )
       @( description, "Based on the Ada version." )
       @( see_also, "http://rosettacode.org/wiki/24_game" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure game_24 is
   subtype operation is character;
   type digit is new positive;
   given_digits : array (1 .. 4) of digit;
begin
   put_line ("24 Game");
   put_line ("Generating 4 digits...");
   for i in arrays.first( given_digits )..arrays.last( given_digits ) loop
      given_digits( i ) := digit( numerics.rnd( 9 ) );
   end loop;
   put ("Your Digits:");
   for i in arrays.first( given_digits )..arrays.last( given_digits ) loop
      put (given_digits (i));
   end loop;
   new_line;
   put_line("Enter your expression, one number and operator at a time.");
   declare
      candidate_value : integer;
      input_operations : array( 1..3) of operation;
      input_digits : array( 1..4 ) of digit;
      unused_digits : array( 1..4 ) of boolean := ( true, true, true, true );
      ch : character;
      op : operation;
   begin
      -- get input
      for i in 1 .. 4 loop
         put( "Digit: " );
         ch := get_line;
         input_digits (i) := digit( numerics.value( string( ch ) ) );
         exit when i = 4;
         put( "Operator: " );
         ch := get_line;
         input_operations (i) := operation( ch );
      end loop;
      -- check input
      for i in arrays.first( input_digits )..arrays.last( input_digits ) loop
         declare
            found : boolean := false;
         begin
            for j in arrays.first( given_digits )..arrays.last( given_digits ) loop
               if unused_digits (j) and given_digits (j) = input_digits (i) then
                  unused_digits (j) := false;
                  found;
                  exit;
               end if;
            end loop;
            if not found then
               put_line ("Illegal Number used:" &
                  strings.image(input_digits (i)));
               return;
            end if;
         end;
      end loop;
      -- check value
      candidate_value := integer (input_digits(1));
      for i in arrays.first( input_operations )..arrays.last( input_operations ) loop
         op := input_operations( i );
         case op is
            when '+' =>
               candidate_value := @ + integer (input_digits (i + 1));
            when '-' =>
               candidate_value := @ - integer (input_digits (i + 1));
            when '*' =>
               candidate_value := @ * integer (input_digits (i + 1));
            when '/' =>
               candidate_value := @ / integer (input_digits (i + 1));
            when others =>
               put_line ("Illegal Op used:" & input_operations (i));
               return;
         end case;
      end loop;
      if candidate_value /= 24 then
         put_line ("Value" & strings.image (candidate_value) & " is not 24!");
      else
         put_line ("You won!");
      end if;
   end;
end game_24;

-- VIM editor formatting instructions
-- vim: ft=spar

