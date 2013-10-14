#!/usr/local/bin/spar

pragma annotate( summary, "monty" );
pragma annotate( description, "Run random simulations of the Monty Hall game. Show the" );
pragma annotate( description, "effects of a strategy of the contestant always keeping" );
pragma annotate( description, "his first guess so it can be contrasted with the" );
pragma annotate( description, "strategy of the contestant always switching his guess." );
pragma annotate( description, "Simulate at least a thousand games using three doors" );
pragma annotate( description, "for each strategy and show the results in such a way as" );
pragma annotate( description, "to make it easy to compare the effects of each strategy." );
pragma annotate( see_also, "http://rosettacode.org/wiki/Monty_Hall_problem" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure monty is
  num_iterations : positive := 100_000;
  type action_type is (stay, switch);
  type prize_type is (goat, pig, car);
  doors : array(1..3) of prize_type;
  type door_index is new positive;

  -- place the prizes behind random doors

  procedure set_prizes is
  begin
    doors( 1 ) := goat;
    doors( 2 ) := pig;
    doors( 3 ) := car;
    arrays.shuffle( doors );
  end set_prizes;

  -- determine if the prize was chosen based on strategy

  function play( action : action_type ) return prize_type is
    chosen : door_index := door_index( numerics.rnd(3) );
    monty : door_index;
  begin
    set_prizes;
    for i in arrays.first(doors)..arrays.last(doors) loop
       if i /= chosen and doors(i) /= car then
          monty := i;
       end if;
    end loop;
    if action = switch then
       for i in arrays.first(doors)..arrays.last(doors) loop
           if i /= monty and i /= chosen then
              chosen := i;
              exit;
           end if;
       end loop;
    end if;
    return doors( chosen );
  end play;

  winners : natural; -- times won
  pct     : float;   -- percentage won

begin
  winners := 0;
  for i in 1..num_iterations loop
      if play( stay ) = car then
         winners := @+1;
      end if;
  end loop;
  pct := float( winners * 100 ) / float( num_iterations );
  put( "Stay: count" ) @ ( winners ) @ ( " = " ) @ ( pct ) @ ( "%" );
  new_line;
  winners := 0;
  for i in 1..num_iterations loop
      if play( switch ) = car then
         winners := @+1;
      end if;
  end loop;
  pct := float( winners * 100 ) / float( num_iterations );
  put( "Switch: count" ) @ ( winners ) @ ( " = " ) @ ( pct ) @ ( "%" );
  new_line;
end monty;

-- VIM editor formatting instructions
-- vim: ft=spar

