#!/usr/local/bin/spar

-- Rock, Scissors, Paper

pragma annotate( summary, "rock" )
       @( description, "Adapted from Basic Computer Games" )
       @( description, "Game of Rock, Scissors, Paper" )
       @( description, "Creative Computing, Morristown, New Jersey, 1978" )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure rock is

  type choices is (none, rock, scissors, paper);
  computer_choice : choices;
  player_choice : choices;

  type game_wins is new natural;
  computer_wins : game_wins := 0;
  player_wins : game_wins := 0;

  game_max  : constant natural := 10;
  game_limit_error : exception;
  type game_limit is new natural
  affirm
    raise game_limit_error when game_limit not in 1..game_max;
  end affirm;
  num_games : game_limit;

  n  : natural;
  ok : boolean;
begin
  put_line( "Game of Rock, Scissors, Paper");

  -- Number of Games

  ok := false;
  while not ok loop
     put( "How many games? (1-10)" );
     begin
        num_games := get_line;
        ok := true;
     exception when others =>
        put_line( "Sorry, but we aren't allowed to play that many" );
     end;
  end loop;

  -- Main Loop

  for game in 1..num_games loop
     new_line;
     put_line( "Game Number" & strings.image( game ) );

     n := numerics.rnd(3);
     case n is
     when 1 =>
        computer_choice := paper;
     when 2 =>
        computer_choice := scissors;
     when 3 =>
        computer_choice := rock;
     when others =>
        put_line( "internal error" );
        return;
     end case;

     put_line( "3=Rock...2=Scissors...1=Paper" );
     put_line( "1...2...3...What's yourchoice?" );
     player_choice := none;

     while player_choice = none loop
        n := get_line;
        case n is
        when 1 =>
           player_choice := paper;
        when 2 =>
           player_choice := scissors;
        when 3 =>
           player_choice := rock;
        when others =>
           put_line( "Invalid" );
        end case;
     end loop;

     put_line( "This is my choice..." );
     put_line( computer_choice );
     if player_choice = computer_choice then
        put_line( "Tie game.  No Winner." );
     elsif computer_choice > player_choice then
        if player_choice /= paper and computer_choice /= rock then
           put_line( "Wow!  I win!!!" );
           computer_wins := @+1;
        else
           put_line( "You win!!!" );
           player_wins := @+1;
        end if;
     elsif computer_choice = paper then
        if player_choice /= rock then
           put_line( "You win!!!" );
           player_wins := @+1;
        else
           put_line( "Wow!  I win!!!" );
           computer_wins := @+1;
        end if;
     else
        put_line( "You win!!!" );
        player_wins := @+1;
     end if;
  end loop;

  -- Game Summary

  new_line;
  put_line( "Here is the final game score:" );
  put( "I have won" ) @ ( integer( computer_wins ) ) @ ( " game(s)." );
  new_line;
  put( "You have won" ) @ ( integer( player_wins ) ) @ ( " game(s)." );
  new_line;
  put( "And" ) @ ( integer( num_games - game_limit(computer_wins+player_wins) ) ) @ ( " game(s) ended in a tie." );
  new_line;
  put_line( "Thanks for playing!!" );
end rock;
-- vim: set ft=spar

