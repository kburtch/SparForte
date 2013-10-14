#!/usr/local/bin/bush

pragma annotate( summary, "8ball" );
pragma annotate( description, "Ask the magic 8-ball" );
pragma annotate( description, "usage: 8ball" );
pragma annotate( author, "Ken O. Burtch" );
pragma annotate( see_also, "http://www.zazzybob.com/bin/8ball.html" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure eight_ball is

  procedure usage is
  begin
     put( "usage: " ) @ ( source_info.file );
     command_line.set_exit_status( 0 );
  end usage;

  question : string;
  answer : positive;

begin

  -- Usage

  if $# /= 0 then
     usage;
     return;
  end if;

  -- Answer questions until an empty line

  loop
    new_line;
    put_line( "Ask the mysterious 8-ball a question..." );

    question := get_line;

    exit when question = "";

    answer := numerics.rnd( 100 );
    if answer < 10 then
      put_line( "It is definite" );
    elsif answer < 20 then
      put_line( "It is looking probable" );
    elsif answer < 30 then
      put_line( "There is great certainty" );
    elsif answer < 40 then
      put_line( "Maybe" );
    elsif answer < 50 then
      put_line( "It doesn't look promising" );
    elsif answer < 60 then
      put_line( "Probably not" );
    elsif answer < 70 then
      put_line( "A definite no" );
    elsif answer < 80 then
      put_line( "All the signs point to yes" );
    elsif answer < 80 then
      put_line( "All the signs point to no" );
    else
      put_line( "Who can be sure? I'm not..." );
    end if;
  end loop;

  command_line.set_exit_status( 0 );
end eight_ball;

-- VIM editor formatting instructions
-- vim: ft=spar

