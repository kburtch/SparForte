#!/usr/local/bin/bush

-- Quest
--
-- An old BASIC exploration game.
-- Coverted to SparForte, Ken O. Burtch, 2003
-- WARNING: Conversion not finished!!
--
-- THIS PROGRAM WAS TAKEN FROM THE JULY 1979 BYTE MAG.
-- BY LEE DUSBABEK FOR THE FAMOUS SKIP HANSON (WB6YMH)

pragma restriction( no_external_commands );


procedure quest is

number_of_rooms : constant integer := 42; -- was M9
first_treasure_room : constant integer := 23; -- was T1
second_treasure_room : constant integer := 12; -- was T2

room : array( 1..42 ) of integer; -- was W
move : array( 1..42 ) of string; -- movement table, M
-- since SparForte only has 1-D arrays, the 43 elements will be fields.
move_sep : character := ',';

score : integer; -- was S

current_room  : natural := 5; -- was N
save_current  : natural; -- was N9
next_room     : natural; -- was I
N0            : natural; -- was N0
moves : integer := 0; -- was M0
moves_to_find_treasure : integer := 0; -- was M6
treasure_room : integer := first_treasure_room; -- was T
pirate_room   : integer := 0; -- was P
P1 : integer := 0;
N8 : integer;
debug : boolean := true; -- was D
J : natural;

-- for input

a  : string;
a1 : positive;
a2 : natural;
a0 : positive;

procedure get_input is
-- 5000 REM
-- Return in a1 the user's choice from string a
-- a = string containing list of characters
-- a2 = length of a
   q2 : string;
   pragma assumption( used, q2 ); -- unfinished
   q : character; -- Q$
begin
   loop
     --q2 := get_line;
     --if strings.length( q2 ) > 0 then
     --   q := strings.element( q2, 1 );
     --else
     --   q := ' ';
     --end if;
     q := strings.to_upper( inkey );
     put( q );
     put( "-" );
     put( a );
     a2 := strings.index( a, q & "" );
     exit when a2 > 0;
  end loop;
  a1 := positive( a2 ); -- WAS a1 := positive( a2 )+1;
end get_input;

procedure tally_score is
 -- 3000 REM
begin
  score := 0;
  if treasure_room = -1 then
     score := score + 5;
  end if;
  if pirate_room = 1 then
     score := score + 10;
  end if;
  for j in 2..number_of_rooms loop
    score := score + room(j);
  end loop;
end tally_score;

procedure show_room is
 -- 8000 REM
begin

--8050 I=INT(N/5)
--8060 J=N-5*I+1

--8100 ON I+1 GOTO 8200,8210,8220,8230,8240,8250,8260,8270,8280,8290
--8200 ON J GOTO 9000,9010,9020,9030,9040
--8210 ON J GOTO 9050,9060,9070,9080,9090
--8220 ON J GOTO 9100,9110,9120,9130,9140
--8230 ON J GOTO 9150,9160,9170,9180,9190
--8240 ON J GOTO 9200,9210,9220,9230,9240
--8250 ON J GOTO 9250,9260,9270,9280,9290
--8260 ON J GOTO 9300,9310,9320,9330,9340
--8270 ON J GOTO 9350,9360,9370,9380,9390
--8280 ON J GOTO 9400,9410,9420,9430,9440
--8290 ON J GOTO 9450,9460,9470,9480,9490

case current_room is
when 1 => put_line( "YOU'RE AT A DEAD END!" );
when 2 => put_line( "YOU CAN'T GO IN THAT DIRECTION" );
          new_line;
when 3 => put_line( "A TUNNEL GOES NORTH-SOUTH." );
          put_line( "THERE IS AN OPENING TO THE WEST." );
when 4 => put_line( "YOU'RE ON THE BRINK OF A PIT." );
when 5 => put_line( "YOU'RE OUTSIDE THE CAVE." );
          put_line( "GO SOUTH TO ENTER." );
when 6 => put_line( "YOU'RE AT THE HOME OF THE GNOME-KING." );
          put_line( "FORTUNATELY, HE'S GONE FOR THE DAY." );
when 7 => put_line( "THE GNOME KING IS HERE!" );
          put_line( "YOU'D BETTER GET OUT!" );
          new_line;
when 8 => put_line( "YOU'RE LOST IN THE WOODS." );
--when 9 => null;
when 10 => put_line( "YOU'RE NOT GOING TO GET FAR, DIGGING" );
           put_line( "THROUGH ROCK." );
           new_line;
when 11 => put_line( "YOU'RE AT THE BOTTOM OF A PIT. A LITTLE" );
           put_line( "STREAM FLOWS OVER THE ROCKS HERE." );
when 12 => put_line( "YOU'RE AT A DEAD END!" );
when 13 => put_line( "YOU'RE AT A WIDE SPOT.  THERE IS A SOOTY PATCH" );
           put_line( "WHERE SOMEBODY HAS RESTED A TORCH AGAINST THE WALL." );
           put_line( "THERE ARE JAGGED ROCKS ABOVE YOU." );
when 14 => put_line( "YOU'RE IN A CANYON.  HIGH ON THE WALL ABOVE YOU" );
           put_line( "IS SCRATCHED THE MESSAGE    'BILBO WAS HERE'" );
when 15 => put_line( "YOU'RE NOT A BIRD.  YOU CAN'T FLY!" );
           new_line;
when 16 => put_line( "YOU'RE IN A LOW CHAMBER.  A TIGHT TUNNEL GOES" );
           put_line( "EAST, AND YOU CAN WALK TO THE SOUTH OR WEST." );
           put_line( "THERE IS LIGHT TO THE NORTH." );
when 17 => put_line( "IT'S A TIGHT SQUEEZE.  YOU CAN'T GET PAST" );
           put_line( "WITH THE TREASURE." );
           new_line;
when 18 => put_line( "I DON'T THINK YOU CAN FIND THE CAVE." );
           put_line( "YOU'RE AT THE TOP OF A CLIMB." );
           put_line( "BELOW YOU A MESSAGE SAYS" );
           put_line( "     'BILBO WAS HERE'" );
when 19 => put_line( "YOU'RE AT THE NORTH SIDE OF A CHASM," );
           put_line( "TOO WIDE TO JUMP.  RINGING ECHOES FROM" );
           put_line( "BELOW ARE THE ONLY INDICATION OF DEPTH." );
when 20 => put_line( "YOU'RE IN XANADU.  BELOW YOU ALPH,  THE SACRED" );
           put_line( "RIVER RUNS THROUGH CAVERNS MEASURELESS TO MAN," );
           put_line( "DOWN TO A SUNLESS SEA." );
when 22 => put_line( "YOU'RE ON THE LEDGE ABOVE THE GUILLOTINE ROOM." );
when 23 => put_line( "I HEAR THE GIANT THERE!!!" );
           put_line( "YOU'D BETTER GO BACK!" );
           new_line;
when 24 => put_line( "YOU'RE IN THE GIANT'S CAVERN.  BETTER" );
           put_line( "NOT BE HERE WHEN THE GIANT COMES!" );
when 25 => put_line( "YOU'RE IN THE QUEST RESEARCH AND DEVELOPMENT AREA." );
           new_line;
           put_line( "I'M SORRY BUT VISITORS ARE NOT ALLOWED. YOU'LL HAVE" );
           put_line( "TO LEAVE." );
           new_line;
when 26 => put_line( "YOU'RE IN THE CRYSTAL PALACE.  THE WALLS RESONATE" );
           put_line( "WTH AWESOME MUSIC." );
when 27 => put_line( "YOU'RE AT THE TOP OF A GIANT STALACTITE." );
           put_line( "YOU COULD SLIDE DOWN, BUT YOU COULDN'T" );
           put_line( "CLIMB BACK UP." );
when 28 => put_line( "YOU'RE IN A LITTLE GROTTO.  THERE IS A" );
           put_line( "BOOK HERE CALLED JANE'S FIGHTING SHIPS," );
           put_line( "DATED 1763." );
when 29 => put_line( "YOU'RE IN THE GUILLOTINE ROOM.  A SHARP" );
           put_line( "ROCK BALANCES PRECARIOUSLY ON THE" );
           put_line( "LEDGE ABOVE YOU." );
when 30 => put_line( "YOU'RE IN A CHUTE, SCRAMBLING DOWN THE" );
           put_line( "ROCKS!  NO WAY TO STOP!!!    HANG ON!" );
           new_line;
when 31 => put_line( "THE TIGHT TUNNEL TURNS A CORNER." );
when 32 => put_line( "YOU'RE IN A LITTLE TWISTY MAZE" );
when 33 => put_line( "YOU'RE IN A LITTLE TWISTING MAZE." );
when 34 => put_line( "YOU'RE IN A TWISTING LITTLE MAZE." );
when 35 => put_line( "YOU'RE IN A TWISTY LITTLE MAZE." );
when 36 => put_line( "YOU'RE IN A PREHISTORIC DWELLING.  ON" );
           put_line( "THE WALL ARE DRAWINGS OF BISON DONE IN" );
           put_line( "RED CLAY.  THE FLOOR IS STREWN WITH" );
           put_line( "BONES, THE REMAINS OF ANCIENT RITUALS." );
           put_line( "A SMALL TUNNEL GOES THROUGH THE FLOOR." );
when 37 => put_line( "YOU'RE IN A BLACK HOLE.  THE" );
           put_line( "FORCE OF GRAVITY IS OVERWHELMING." );
when 38 => put_line( "YOU'RE IN THE LABYRINTHE." );
when 39 => put_line( "YOU'RE IN THE LABYRINTHE." );
           put_line( "IT'S VERY DARK IN HERE." );
when 40 => put_line( "YOU'RE IN THE ASHRAM.  INCENSE IS HEAVY" );
           put_line( "IN THE AIR, AND ALL DIRECTIONS" );
           put_line( "SEEM THE SAME.");
when others =>
       if treasure_room = integer(current_room) then
          new_line;
          put_line("  THE TREASURE IS HERE!!!!");
       elsif treasure_room = second_treasure_room or
            first_treasure_room /= second_treasure_room or
            first_treasure_room = integer(current_room) then
          new_line;
          put_line( "A NOTE ON THE WALL SAYS PIRATES NEVER LEAVE" );
          put_line( "THEIR TREASURE TWICE IN THE SAME PLACE!" );
       end if;
end case;
end show_room;

procedure check_for_pirate is
-- was 4000 REM
begin
  if integer(current_room) = second_treasure_room then
     return;
  end if;
  if pirate_room = 1 then
     return;
  end if;
  if first_treasure_room = second_treasure_room then
     return;
  end if;
  if treasure_room /= -1 then
     return;
  end if;
  if current_room = 16 then
     pirate_room := 160;
  end if;
  if P1 > 0 then
     P1 := P1+1;
  end if;
  if current_room = 3 then
     P1 := P1+1;
  end if;
  if P1<15 then
     return;
  end if;
  new_line;
  put_line( "SUDDENLY THE PIRATE LEAPS OUT OF THE" );
  put_line( "GLOOM AND GRABS THE TREASURE FROM YOU!" );
  put_line( "'HAH!', HE SHOUTS, 'YOU FOUND MY" );
  put_line( "TREASURE, DID YOU?!   WELL, I'LL HIDE" );
  put_line( "IT BETTER THIS TIME!'" );
  put_line( "AND HE DISAPPEARS INTO THE DARKNESS" );
  put_line( "WITH THE TREASURE." );
  pirate_room := 1;
  treasure_room := second_treasure_room;
end check_for_pirate;

procedure check_for_treasure is
begin
  if treasure_room /= integer(current_room) then
     return;
  end if;
  if treasure_room < 0 then
     return;
  end if;
  if moves_to_find_treasure+5>moves then
     return;
  end if;
  put_line( "  DO YOU WANT TO TAKE IT WITH YOU" );
  loop
    a := "YN";
    a2 := 2;
    get_input;
    exit when a1 in 1..2;
    put_line( "WELL?....." );
  end loop;
  if a1 = 1 then
     treasure_room := -1;
     new_line;
     put_line( "OK, LETS GET OUT OF HERE!" );
  else
     new_line;
     put_line( "WE'LL LEAVE IT HERE AND YOU CAN EXPLORE SOME MORE." );
     moves_to_find_treasure := moves;
  end if;
end check_for_treasure;
pragma assumption( used, check_for_treasure ); -- unfinished

procedure show_options is
 -- 7500 REM
begin
  new_line;
  put_line( "TYPE N,S,E,W, U   OR D  FOR NORTH, SOUTH," );
  put_line( "EAST, WEST, UP OR DOWN.  TYPE P FOR SCORE" );
  new_line;
end show_options;

procedure get_command is
-- 7000 REM
--
   allowed : constant string := "NEUDWSP"; -- A$
begin
  new_line;
  put( "                      WHICH WAY" );
  loop
    loop
      a2 := 7;
      a := allowed;
      get_input;
      exit when a1 < 8;
      put_line( "WHICH WAY DO YOU WANT TO GO?" );
      show_options;
      show_room;
    end loop;
    exit when a1 < 7;
    tally_score;
    put( "YOU HAVE" ); put( score ); put( " POINTS!" );
    new_line;
  end loop;
end get_command;

-- Main Program Begins

begin

new_line;
put_line( "WARNING: The conversion isn't finished.  This game is not" );
put_line( "working. -- KB" );
new_line; new_line; new_line;
put_line( "  YOU WERE WALKING THROUGH THE WOODS ONE DAY AND YOU" );
put_line( "CAME ACROSS THE ENTRANCE OF A CAVE, COVERED WITH BRUSH." );
put_line( "  PEOPLE SAY THAT MANY YEARS AGO A PIRATE HID HIS" );
put_line( "TREASURE IN THESE WOODS,  BUT NO ONE HAS EVER FOUND IT." );
put_line( "IT MAY STILL BE HERE,  FOR ALL I KNOW." );

move(1) := "1,0,0,0,0,0,0";
move(2) := "-2,101,-2,0,0,0";
move(3) := "33,2,1,10,106,4";
move(4) := "3,30,2,11,2,1";
move(5) := "8,8,15,10,8,16";
move(6) := "16,3,2,10,2,2";
move(7) := "7,-2,101,-2,0,0,0";
move(8) := "18,18,15,10,18,9";
move(9) := "-2,33,5,1,0,-2";
move(10) := "-2,101,-2,0,0,0";
move(11) := "1,13,4,2,1,2";
move(12) := "36,2,1,2,1,2";
move(13) := "2,37,2,1,11,14";
move(14) := "13,1,19,2,31,31";
move(15) := "-2,101,-2,0,0,0";
move(16) := "5,33,2,10,1,106";
move(17) := "-2,101,-2,0,0,0";
move(18) := "-2,101,8,0,0,0";
move(19) := "224,2,2,14,1,42";
move(20) := "226,1,2,2,25,2";
move(21) := "1,226,2,2,38,25";
move(22) := "-2,33,13,50,29,30";
move(23) := "2,1,2,31,2,2";
move(24) := "-2,101,19,0,0,0";
move(25) := "21,20,2,2,1,19";
move(26) := "-2,65,-2,50,11,14";
move(27) := "2,40,2,2,21,20";
move(28) := "-2,60,221,50,14,19";
move(29) := "2,42,2,13,1,1";
move(30) := "34,34,2,1,4,2";
move(31) := "14,14,23,2,1,2";
move(32) := "-2,101,516,0,0,0";
move(33) := "2,1,2,1,116,3";
move(34) := "1,35,2,1,30,30";
move(35) := "2,1,2,37,34,36";
move(36) := "35,2,1,37,34,12";
move(37) := "2,1,35,2,13,2";
move(38) := "2,21,2,116,1,2";
move(39) := "2,40,2,32,21,26";
move(40) := "40,40,2,2,40,41";
move(41) := "40,40,40,2,40,39";
move(42) := "28,28,28,28,28,28";

new_line;

put_line( "WHEN YOU ANSWER A QUESTION, I LOOK AT ONLY" );
put_line( "THE FIRST LETTER, ALTHOUGH YOU CAN TYPE IN" );
put_line( "THE WHOLE WORD IF YOU WANT TOO." );
show_options;

-- MASTER LOOP for restarting game

loop

for j in 1..number_of_rooms loop
    room(j) := 0;
end loop;

new_line;

loop                                               -- game loop

  show_room;                                       -- show first room
  moves := moves + 1;                              -- first move

  save_current := current_room;                    -- remember where we parked
  N8 := 0;                                         -- NQS

  get_command;
                                    -- get a command

  if current_room /= 1 then
     N0 := current_room;
     a0 := a1;
  end if;
  new_line;
  next_room := numerics.value( strings.field( move(a1), current_room, "," ) );

--  loop

    loop
       if next_room = -2 then
          next_room := save_current;
       end if;
       if debug then
          put_line( standard_error, "DEBUG" & strings.image(current_room) &
            " TO" & strings.image(next_room) );
       end if;
       if next_room >= 500 then
          next_room := next_room - 500;
          delay 2.0;
       else
          exit;
       end if;
    end loop;

    if numerics.truncation(next_room/100) = 0 then
       current_room := next_room;
    elsif numerics.truncation(next_room/100) = 1 then
       current_room := next_room-100;
       if treasure_room = -1 then
          current_room := current_room + 1;
       end if;
    else
       current_room := next_room-200;
       if treasure_room = -1 then
          current_room := current_room + natural(pirate_room);
       end if;
    end if;

    if current_room = 1 then
       for j in 1..6 loop
           strings.replace( move(j), current_room, "2", ',' );
       end loop;
    end if;
    strings.replace( move(7-a0), current_room, strings.image(N0), ',' );

-- 6500 REM

    if N8 /= 2 then
       show_room;
    end if;

    room( current_room ) := 1;
    N8 := 8;

    if numerics.value( strings.field( move( 1 ), current_room, move_sep ) ) = -2 then
       next_room := numerics.value( strings.field( move( 6 ), current_room ) );
       J := -1;

       if numerics.value( strings.field( move(4), current_room, move_sep ) ) > numerics.rnd(100) then
          next_room := numerics.value( strings.field( move(5), current_room ) );
       end if;
       J := -1;
       if numerics.value( strings.field( move(2), current_room, move_sep ) ) > numerics.rnd(100) then
          next_room := numerics.value( strings.field( move(3), current_room, move_sep ) );
       end if;
       if debug then
          put_line( "   DEBUG BOUNCE TO" & strings.image( current_room ) );
       end if;
    end if;

 -- end loop;

  check_for_teasure;
  check_for_pirate;
  exit when treasure_room < 0 or current_room = 5; -- goto 1400

end loop; -- game loop

tally_score;
new_line;
put_line( " CONGRATULATIONS!!   YOU GOT THE TREASURE" );
put( "OUT IN" );
put( moves );
put( "MOVES AND YOU GOT" );
put(score+10 );
put_line( "POINTS!" );
put_line( "WANT TO HUNT AGAIN?" );
loop
  a := "YN";
  a2 := 2;
  gosub get_input;
  exit when a1 in 1..2;
end loop;
exit when a1 = 2;

end loop; -- master loop here

end quest;

-- vim: ft=spar

