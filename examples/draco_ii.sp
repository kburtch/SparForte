#!/usr/local/bin/spar

-- Draco II

pragma annotate( summary, "draco_ii" )
       @( description, "Survive a hostile wilderness and slay monsters." )
       @( description, "Originally written in AppleSoft BASIC" )
       @( description, "by Ken O. Burtch circa 1980" )
       @( description, "Adapted to SparForte in April 2003" )
       @( description, "Usage: draco_ii" )
       @( author, "Ken O. Burtch" );

pragma ada_95;                               -- enforce Ada 95 rules and style

procedure draco_ii is

  -- Terminal Control
  --
  -- Linux/UNIX tput returns a string to control the terminal display

  tty_inverse : string; -- inverse print
  tty_normal  : string; -- normal print

  -- General Player Info

  type import_string is new string;
  LOGNAME : constant import_string := "unknown";-- Linux/UNIX user login name
  pragma import( shell, LOGNAME );              -- get it from the environment

  damage_to_creature : integer;                 -- inflicted on creature (E)
  damage_to_player   : integer;                 -- inflicted on player (D)

  type a_status is (no_status, alive, dead);
  status : a_status := alive;                   -- player's current status

  score : integer := 0;                         -- the socre
  quest_bonus : natural;                        -- quest bonus for level

  specter_paralysis : boolean := false;         -- specter can paralyze player

  type an_attribute is new float;               -- attributes are a unique type

  dex : an_attribute;                           -- player dexterity
  str : an_attribute;                           -- player strength

  hit_points     : integer;                     -- hit points
  level_distance : integer;                     -- total level distance
  distance       : integer;                     -- distance left (DT)
  water          : float;                       -- water left (W)

  -- Player Weapon
  --
  -- There are 4 weapons: the cursed club, the bow (dexterity), the mace
  -- (dexterity+strength) and the sword (strength).

  type a_weapon is (no_weapon, club, bow, mace, sword);

  weapon             : a_weapon;                -- player weapon
  weapon_name        : string;                  -- name as text
  weapon_damage      : float;                   -- damage (attribute adjusted)


  -- Creatures
  --
  -- There are 6 creatures: a giant, a wolf, a serpent, a druid, a dragon and
  -- a specter.

  type a_creature is (no_creature, dork, wolf, serpent, druid, draco, specter );

  creature             : a_creature; -- current creature (C)
  creature_here        : boolean;    -- true if creature present (C1)
  creature_hp          : integer;    -- creature hit points (CP)
  creature_max         : integer;    -- creature's maximum hit poins (C2)
  creature_description : string;     -- description when creature here (CN)
  creature_name        : string;     -- the creature name as text
  creature_evade       : string;     -- message when creature evades (CV)
  creature_attack1     : string;     -- message for light attack (CA(0))
  creature_attack2     : string;     -- message for medium attack (CA(1))
  creature_attack3     : string;     -- message for fierce attack (CA(2))
  creature_damage1     : positive;   -- damage for light attack (CD(0))
  creature_damage2     : positive;   -- damage for medium attack (CD(1))
  creature_damage3     : positive;   -- damage for fierce attack (CD(2))

  -- Items
  --
  -- There are 4 magic items: a lightning staff, a magic spear, a potion of
  -- healing and a ring of fire resistance.

  type an_item is (no_item, staff, spear, potion, ring );
  has_item : array( staff..ring ) of boolean;

  staff_charge : integer; -- number of staff uses left (CH)
  spear_damage : integer; -- amount of damage from spear (SD)


  -- Game variables

  round      : natural;                 -- round or turn (SP)
  level      : natural := 0;            -- current level (L)
  high_name  : string := "nobody";      -- high score player
  high_score : integer := 0;            -- high score (HS)
  high_level : integer := 0;            -- high score level
  cmd        : character;               -- player's last command (CM)


  -- get high score: retrieve high score from file (if the file exists).

  procedure get_high_score is
    f : file_type;
  begin
    if files.exists( "./draco_score.txt" ) then
       open( f, in_file, "./draco_score.txt" );
       high_name  := get_line( f );
       high_score := numerics.value( get_line( f ) );
       high_level := numerics.value( get_line( f ) );
       close( f );
    end if;
  end get_high_score;


  -- set high score: write new high score to file.  If file doesn't exist,
  -- create it.

  procedure set_high_score is
    f : file_type;
  begin
    create( f, out_file, "./draco_score.txt" );
    put_line( f, LOGNAME );
    put_line( f, high_score );
    put_line( f, level );
    close( f );
  end set_high_score;


  -- set attributes: generate a new set of player attributes.  Where did this
  -- formula come from?  I don't remember...

  procedure set_attributes is
  begin
    str := -3;
    dex := -3;
    while dex < -2 and str < -2 loop
      dex := (9.5 - numerics.rnd(14)+2 ) / 2;
      str := (9.5 - numerics.rnd(14)+2 ) / 2;
    end loop;
  end set_attributes;


  -- chose weapon:  allow the player to chose a weapon at the start of the
  -- game or after his weapon was replaced with the cursed club.

  procedure choose_weapon is
    ch : character;
  begin
    put_line( "Strength  =" & strings.image( numerics.rounding( str*2 ) ) );
    put_line( "Dexterity =" & strings.image( numerics.rounding( dex*2 ) ) );
    new_line;
    put_line( "Chose a weapon:" );
    weapon := no_weapon;
    while weapon = no_weapon loop
      put( "Bow, Mace or Sword? (B/M/S) *" & ASCII.BS );
      ch := inkey;
      put_line( ch );
      case ch is
      when 'b'|'B' =>
         weapon := bow;
         weapon_name := "bow";
         weapon_damage := 4 + dex;
      when 'm'|'M' =>
         weapon := mace;
         weapon_name := "mace";
         weapon_damage := 6+ (str + dex)/2;
      when 's'|'S' =>
         weapon := sword;
         weapon_name := "sword";
         weapon_damage := 8 + str;
      when others =>
         put_line( "Pardon?" );
      end case;
    end loop;
  end choose_weapon;


  -- new level: start a new level, chose a creature.

  procedure new_level is
  begin
    level := level + 1;
    hit_points := 20;
    water := 20;
    creature_here := false;
    quest_bonus := 0;

    round := 0;

    case level is
    when 1 => creature := dork;
    when 2 => creature := wolf;
    when 3 => creature := serpent;
    when 4 => creature := druid;
    when 5 => creature := draco;
    when others => creature := specter;
    end case;
    case creature is
    when dork =>
         creature_description := "Dork the Witless stalks you.";
         creature_name := "giant";
         creature_evade := "It bounces harmlessly off his armor.";
         creature_attack1 := "Dork the Witless tries to stomp you.";
         creature_damage1 := 3;
         creature_attack2 := "He pounds you with his mighty fists!";
         creature_damage2 := 4;
         creature_attack3 := "He heaves a rock at your miniscule body!!";
         creature_damage3 := 5;
    when wolf =>
         creature_description := "Something lurks in the shadows.";
         creature_name := "wolf";
         creature_evade := "It dodges your attack.";
         creature_attack1 := "It lashes out with its claws.";
         creature_damage1 := 3;
         creature_attack2 := "The wolf snaps at your arm!";
         creature_damage2 := 4;
         creature_attack3 := "It leaps at your chest, eyes gleaming brightly!!";
         creature_damage3 := 6;
    when serpent =>
         creature_description := "Something lurks in the shadows.";
         creature_name := "giant serpent";
         creature_evade := "It slithers away.";
         creature_attack1 := "It coils around you and squeezes!";
         creature_damage1 := 5;
         creature_attack2 := "You trip and fall, uncoordinated oaf.";
         creature_damage2 := 3;
         creature_attack3 := "The serpent attacks, venom burning holes in the earth!!";
         creature_damage3 := 7;
    when druid =>
         creature_description := "An evil druid smiles.";
         creature_name := "druid";
         creature_evade := "The druid forms a magic barrier and laughs menacingly!";
         creature_attack1 := "He lashes out with his dagger.";
         creature_damage1 := 4;
         creature_attack2 := "With a sudden gesture, a fireball hurls towards you!";
         creature_damage2 := 5;
         creature_attack3 := "He raises his scepter and a bolt of power erupts!!";
         creature_damage3 := 7;
    when draco =>
         creature_description := "Draco the Red circles overhead.";
         creature_name := "dragon";
         creature_evade := "Your weapon bounces harmlessly off its scales.";
         creature_attack1 := "It lashes out with its claws.";
         creature_damage1 := 4;
         creature_attack2 := "Draco rains fireballs down upon you!";
         creature_damage2 := 6;
         creature_attack3 := "A river of deadly fire descends!!";
         creature_damage3 := 9;
    when others =>
         creature_description := "A grim specter materializes. Poof!";
         creature_name := "specter";
         creature_evade := "He dissolves before your eyes!";
         creature_attack1 := "A ghostly blade lashes out.";
         creature_damage1 := 4;
         creature_attack2 := "YOU ARE PETRIFIED WITH HORROR!";
         creature_damage2 := 1;
         creature_attack3 := "A cold hand reaches out for your life!!";
         creature_damage3 := 4;
    end case;
    creature_hp  := 25 + (level-1) * 9;
    creature_max := creature_hp;
    level_distance := 50 * integer( level );
    distance := level_distance;
  end new_level;

  -- show status: show the player's statistics (review stats).

  procedure show_status is
  begin
    put_line( "You have:" );
    put_line( strings.image( numerics.rounding( str * 2 ) ) & " points of strength" );
    put_line( strings.image( numerics.rounding( dex * 2 ) ) & " points of dexterity" );
    put_line( " carrying a " & weapon_name );
    new_line;
    put_line( "with" & strings.image( numerics.rounding( water/2*10 ) ) & "% of water in your bottle" );
    put_line( "and" & strings.image( hit_points / 2 * 10 ) & "% of full health" );
  end show_status;


-- Main Script begins

begin

  -- Initialize Draco II

  -- Inverse printing.  First try terminfo (Linux), then termcap (FreeBSD).

  tty_inverse := `tput( "smso" );`;
  if tty_inverse = "" then
     tty_inverse := `tput( "so" );`;
  end if;
  tty_normal := `tput( "rmso" );`;
  if tty_normal = "" then
     tty_normal := `tput( "se" );`;
  end if;


  clear;
  put_line( tty_inverse & "Draco II" & tty_normal );
  new_line;
  put_line( "Survive a hostile wilderness and slay monsters." );
  put_line( "Originally written by Ken O. Burtch circa 1980" );
  new_line;

  get_high_score;
  put( "Top Dragon Slayer:" & strings.image( high_score ) );
  put( "  level" & strings.image( high_level ) );
  put_line( "  by " & high_name );
  new_line;

  set_attributes;
  choose_weapon;

  -- Player has no magic items

  for i in staff..ring loop
      has_item( i ) := false;
  end loop;

  put( "Press any key to begin *" & ASCII.BS );
  declare
    ch : character;
  begin
    ch := inkey;
  end;

  -- Game loop: loop for each level

  loop

  -- Start a new level.  Player's weapon the club?  Let 'im change it.

  new_level;
  if weapon = club then
     new_line;
     choose_weapon;
  end if;

  -- Main loop: loop for each round

  loop

     -- clear damage from monster and player

     damage_to_player := 0;
     damage_to_creature := 0;

    -- check to see if the creature appears every 4 rounds

    if creature /= no_creature then
       if float(round)/4.0 = numerics.truncation(round/4) then
          creature_here := numerics.rnd(2)=1;
       end if;
    end if;

    -- display menu

    clear;
    put_line( tty_inverse & "Draco II" & tty_normal );
    new_line;
    put_line( "1) Evade            2) Strike with " & weapon_name );
    put_line( "3) Search for water 4) Run like crazy" );
    put_line( "5) Proceed          6) Surrender          R) Review stats" );
    if has_item( staff ) then
       put_line( "7) Wave staff" );
    end if;
    if has_item( spear ) then
       put_line( "8) Throw spear" );
    end if;
    if has_item( potion ) then
       put_line( "9) Drink potion" );
    end if;
    if has_item( ring ) then
       put_line( "The anti-fire ring magic is automatic." );
    end if;

    new_line;
    put( "(Level" & strings.image( level ) & ")" );
    put_line( " There are" & strings.image( distance ) & " miles to go." );
    new_line;

    -- Creature not around?  Then it slowly heals...

    if not creature_here then
       creature_hp := creature_hp + 1;
       if creature_hp > creature_max then
          creature_hp := creature_max;
       end if;
       put_line( "There is no danger to be seen." );
    end if;

    -- Basic stat changes that happen every round

    round := round + 1;
    water := water - 0.5;
    hit_points := hit_points + 0.2;

    -- Basic Stats check

    if water > 0 and water < 7 then
       put_line( tty_inverse & "Water's Low" & tty_normal );
    end if;

    if hit_points < 5 then
       put_line( tty_inverse & "You're badly wounded" & tty_normal );
    elsif hit_points < 10 then
       put_line( tty_inverse & "You're in rough shape" & tty_normal );
    end if;

    if creature_here then
       put_line( creature_description );
       if creature_hp < 10 then
          put_line( tty_inverse & "Blood gushes from its wounds" & tty_normal );
       end if;

       -- Creature attack

       declare
         attack : constant positive := numerics.rnd( 5 );
       begin
         case attack is
         when 1|2 =>
            put_line( creature_attack1 );
            damage_to_player := integer( numerics.rnd( creature_damage1 ) );
         when 3 =>
            put_line( creature_attack2 );
            damage_to_player := integer( numerics.rnd( creature_damage2 ) );
            if (creature = druid or creature = draco) and has_item( ring ) then
               damage_to_player := damage_to_player/2;
            elsif creature = specter then
               damage_to_player := 0;
               damage_to_creature := damage_to_creature/2;
               hit_points := hit_points - damage_to_player;
               creature_hp := creature_hp + damage_to_creature;
               specter_paralysis := true;
            end if;
         when 4 =>
            put_line( creature_attack3 );
            damage_to_player := integer( numerics.rnd( creature_damage3 ) );
            if creature = draco and has_item( ring ) then
               damage_to_player := damage_to_player/2;
            end if;
            if creature = specter and numerics.rnd(20) = 1 then
               put( tty_inverse );
               put( "You scream, and your lifeless soul becomes a specter!!!" );
               put_line( tty_normal );
            elsif creature = draco then
               damage_to_player := damage_to_player/2;
            end if;
         when others =>
            null;
         end case;
       end;
    else
       declare
          scene : positive := numerics.rnd( 100 );
          maybe : constant boolean := numerics.rnd(2)=1;
       begin
          if scene <= 5 then -- rare scenes
             case scene is
             when 1 =>
                 declare
                    gypsie_distance : constant positive := numerics.rnd(7)+2;
                 begin
                    put( "You travel" );
                    put( gypsie_distance );
                    put( " miles with a band of gypsies!" );
                    distance := distance - integer(gypsie_distance);
                 end;
             when 2 =>
                 put_line( "A Ty Fighter soars through the air leaving" );
                 put_line( "a blaze of incadescent destruction." );
                 put_line( "It promptly vanishes into hyperspace!!" );
                 -- graphics effects
                 put_line( "WOW!" );
             when 3 =>
                 put_line( "You drink from a magic spring that restores you!" );
                 water := 20;
                 hit_points := hit_points + integer(numerics.rnd(8))+3;
                 if hit_points > 20 then
                    hit_points := 20;
                 end if;
             when 4 =>
                 declare
                    ch : character;
                    door : integer;
                    reward : positive;
                    reward_bonus : natural;
                 begin
                    put_line( "From the turret of a dark castle you hear a terrified scream" );
                    put_line( "for help!  A strong hero is required!  What will you do?" );
                    put_line( "1) Check it out    2) Keep going" );
                    ch := inkey;
                    put_line( ch );
                    if ch /= "1" then
                       put_line( "Fine, you gutless swine." );
                    else
                       door := integer(numerics.rnd(15))+hit_points/8+(str+dex)/2;
                       if door < 10 then
                          new_line;
                          put_line( "The evil lord of the castle feeds you to his pet tiger!" );
                          status := dead;
                          exit;
                       else
                          new_line;
                          put_line( "You attack the castle, storming through the gates." );
                          put_line( "You rescue the beautiful princess!!" );
                          reward := numerics.rnd( 10 );
                          new_line;
                          put( "Her father awards you with " );
                          case reward is
                          when 1 =>
                            put_line( "a priceless ancient crown." );
                            reward_bonus := 99;
                          when 2 =>
                            put_line( "a rare gold scepter." );
                            reward_bonus := 70;
                          when 3 =>
                            put_line( "bags of brilliant perls." );
                            reward_bonus := 50;
                          when 4 =>
                            put_line( "gorgeous furs." );
                            reward_bonus := 40;
                          when 5 =>
                            put_line( "a beautiful ruby." );
                            reward_bonus := 30;
                          when 6 =>
                            put_line( "a dazzling emerald." );
                            reward_bonus := 25;
                          when 7 =>
                            put_line( "gold coins." );
                            reward_bonus := 20;
                          when 8 =>
                            put_line( "a sliver ring." );
                            reward_bonus := 10;
                          when 9 =>
                            put_line( "a splendid lute." );
                            reward_bonus := 5;
                          when others =>
                            put_line( "his thanks." );
                            reward_bonus := 0;
                          end case;
                          quest_bonus := quest_bonus + reward_bonus;
                          delay 1.0;
                          put( tty_inverse & "BONUS: " & tty_normal );
                          put_line( reward_bonus );
                       end if;
                    end if;
                 end;
              when 5 =>
                 put_line( "Oh, no!  An evil cleric has transformed your magificent weapon" );
                 put_line( "into a useless stick!" );
                 weapon := club;
                 weapon_name := "club";
                 weapon_damage := 3;
              when others =>
                 null;
              end case;
          else -- common scenes
             scene := numerics.rnd( 19 );
             case scene is
             when 1 =>
                put_line( "Oops!  Fell into a hidden pit." );
                hit_points := hit_points - integer(numerics.rnd(3));
             when 2 =>
                put_line( "You enter an enchanted forest." );
                if has_item( spear ) then
                   spear_damage := spear_damage/2;
                   put_line( "*** Your spear seems weaker." );
                end if;
                if has_item( staff ) then
                   if staff_charge > 0 then
                      staff_charge := integer(numerics.rnd(positive(staff_charge)));
                   end if;
                   staff_charge := integer(numerics.rnd(5))-staff_charge;
                   put_line( "*** Your staff glows faintly." );
                end if;
             when 3 =>
                if has_item( potion ) and maybe then
                   put_line( "You fall, and your vial shatters!" );
                   has_item( potion ) := false;
                end if;
              when 4 =>
                put_line( "You spend the night at an inn." );
                water := water + 3;
                hit_points := hit_points + 1;
                if score > 0 then
                   score := score - 1;
                end if;
              when 5 =>
                if not has_item( staff ) and maybe then
                   put_line( "An ancient staff lies in the dust." );
                   has_item( staff ) := true;
                   staff_charge := integer(numerics.rnd( 4 )) + 1;
                end if;
              when 6 =>
                put_line( "You become lost in the wilderness." );
                distance := distance + integer(numerics.rnd( 7 )) - 4;
              when 7 =>
                if not has_item( potion ) and maybe then
                  put_line( "An unopened vial lies on the ground." );
                  has_item( potion ) := true;
                end if;
              when 8 =>
                put_line( "This is an ashen wasteland left by Draco." );
              when 9 =>
                if not has_item( spear ) and maybe then
                   put_line( "A dwarvish warrior gives you his spear to aid you on your quest." );
                   has_item( spear ) := true;
                   spear_damage := 10;
                end if;
              when 10 =>
                put_line( "You traverse a burning desert!" );
                water := water - float(numerics.rnd( 3 ));
              when 11 =>
                if not has_item( ring ) and maybe then
                   put_line( "A golden ring lies on the earth." );
                   has_item( ring ) := true;
                end if;
              when 12 =>
                if cmd = '3' then
                   water := water + 3;
                end if;
                put_line( "You swim a peaceful river." );
              when 13 =>
                declare
                   stolen_points : constant positive := numerics.rnd(30);
                begin
                   put_line( "A band of thieves attacked in the night." );
                   if score <= 0 then
                      put_line( "But you have no treasure to steal." );
                   else
                      put( "They take" & tty_inverse );
                      put( stolen_points );
                      put_line( tty_normal & " points of treasure!" );
                      score := score - integer( stolen_points );
                   end if;
                 end;
              when 14 =>
                declare
                   bad_luck : constant positive := numerics.rnd( 4 );
                begin
                   if has_item( ring ) then
                      case bad_luck is
                      when 1 =>
                         put_line( "Your ring is lost while swimming a river." );
                         has_item( ring ) := false;
                      when 2 =>
                         put_line( "Your ring vanishes without a trace." );
                         has_item( ring ) := false;
                      when others =>
                         put_line( "You catch a pickpocket trying to steal your ring." );
                      end case;
                   end if;
                end;
               when 15 =>
                  put_line( "You follow a trail through the mountains." );
                  distance := distance - 3;
               when 16 =>
                  if cmd = '3' and maybe then
                     put_line( "The spring from which you drink is poison." );
                     status := dead;
                  else
                     put_line( "You avoid a foul-smelling pond." );
                  end if;
               when 17 =>
                  put_line( "You rest beneath a golden-leaved tree." );
                  hit_points := hit_points + 0.5;
               when 18 =>
                  declare
                     strange_luck : constant positive := numerics.rnd( 7 );
                     gold : constant positive := numerics.rnd( 60 )+20;
                  begin
                     if maybe then
                        put_line( "You meet an old hermit" );
                        case strange_luck is
                        when 1 =>
                           put_line( "who gives you water!" );
                           water := water + float(numerics.rnd(4))+5;
                        when 2 =>
                           if creature /= no_creature then
                              put_line( "who informs your pursuer of your presence." );
                              creature_here := true;
                           else
                              put_line( "who threatens and mumbles." );
                           end if;
                        when 3 =>
                           if hit_points < 15 then
                              put_line( "who helps treat your wounds." );
                              hit_points := hit_points + integer(numerics.rnd(4))+5;
                              if hit_points > 20 then
                                 hit_points := 20;
                              end if;
                           else
                              put_line( "who throws rocks at you." );
                              hit_points := hit_points - 1;
                           end if;
                        when 4 =>
                           put_line( "who gives you directions." );
                           distance := distance - integer(numerics.rnd(3))+3;
                        when 5 =>
                           put( "who gives you" & tty_inverse );
                           put( gold );
                           put( tty_normal & " gold pieces!" );
                        when 6 =>
                           put( "who tells you long-winded stories that go no where." );
                        when others =>
                           put( "who tells you he was once a handsome frog" );
                           put( " before some princess kissed him." );
                        end case;
                     end if;
                  end;
               when 19 =>
                  put_line( "You meet a wandering band of elves." );
               when others =>
                  put_line( "internal error: missing scene case" );
               end case;
            end if;
          end;
       end if;

       -- pre status check: if player dead, stop now.

       if water <= 0 then
          put_line( tty_inverse & "Gasp!! You are out of water!" & tty_normal );
          put_line( "You die alone in the wilderness." );
          status := dead;
       elsif hit_points <= 0 then
          status := dead;
       end if;

       -- Level complete or dead?  No more to do.

       if distance <= 0 or status = dead then
          exit;
       end if;

       -- Player Command

       if specter_paralysis then
          specter_paralysis := false;
       else
       loop
         new_line;
         put( "What will you do? *" & ASCII.BS );
         cmd := inkey;
         put_line( cmd );

         -- Handle player's command

         case cmd is
         when '1' =>
           -- evade
           if creature_here then
              if numerics.rnd(2) = 1 then
                 put_line( "You dive behind some boulders." );
                 damage_to_player := 0.5;
              else
                 if damage_to_player > 0 then
                    damage_to_player := integer( numerics.rnd( positive( damage_to_player ) ) );
                 end if;
                 damage_to_player := damage_to_player - integer(dex);
                 if damage_to_player < 1 then
                    damage_to_player := 1;
                 end if;
                 if damage_to_player < 3 then
                    put_line( "You avoid the worst of his attack." );
                 else
                    put_line( "There is no escape!" );
                 end if;
              end if;
              exit;
           else
              put_line( "Why? There is no enemy." );
           end if;
         when '2' =>
           -- attack
           if creature_here then
              declare
                 creature_evades : boolean;
              begin
                 if weapon = bow then
                    creature_evades := numerics.rnd(4)=1;
                 else
                    creature_evades := numerics.rnd(3)=1;
                 end if;
                 damage_to_creature := integer( numerics.rnd( positive( weapon_damage ) ) );
                 if creature_evades then
                    put_line( creature_evade );
                    damage_to_creature := 0;
                 elsif weapon = bow then
                    put_line( "Ttwwaanngg!!" );
                    if damage_to_creature > 3 then
                       delay 0.5;
                       put_line( "Good shot!!!" );
                    end if;
                 else
                    put_line( "Pow!!" );
                    if (damage_to_creature > 5 and weapon = mace) or
                       (damage_to_creature > 7 and weapon = sword) then
                       delay 0.5;
                       put_line( "Great swing!!!" );
                    elsif damage_to_creature = 3 and weapon = club then
                       delay 0.5;
                       put_line( "Bonk!" );
                    end if;
                 end if;
              end;
              exit;
           else
              put_line( "Why? There is no enemy." );
           end if;
         when '3' =>
           -- search for water
           declare
             water_found : constant boolean := numerics.rnd(3)=1;
           begin
             if water_found then
                put_line( "You find a spring and fill your waterbottle." );
                water := 20.0;
             else
                put_line( "Nothing around." );
                water := water - 0.5;
             end if;
           end;
           exit;
         when '4' =>
           -- run like crazy
           water := water - 2.0;
           damage_to_player := damage_to_player * 2 / 3;
           hit_points := hit_points - 0.2;
           distance := numerics.rounding( distance -
              integer( numerics.rnd( numerics.ceiling( hit_points/3) ) )
              - hit_points/4 );
           put_line( "OK" );
           exit;
         when '5' =>
           -- proceed
           distance := distance - integer( numerics.rnd( 7 ) );
           water := water - 0.5;
           put_line( "OK" );
           exit;
         when '6' =>
           -- surrender
           put_line( "You surrender." );
           status := dead;
           exit;
         when '7' =>
           -- magic staff
           if creature_here then
              if has_item( staff ) then
                 staff_charge := staff_charge - 1;
                 if staff_charge < 0 then
                    staff_charge := 0;
                    put_line( "Nothing happens!" );
                 elsif creature = dork then
                    put_line( "Your blast tickles Dork into fits of uncontrolled laughter." );
                 else
                    put_line( "A bolt of lightning strikes the enemy!" );
                    damage_to_creature := integer( numerics.rnd(4) )+3;
                    if creature = specter then
                       damage_to_creature := damage_to_creature * 4;
                       put_line( "The specter howls with rage!" );
                    end if;
                 end if;
                 exit;
              else
                 put_line( "Wave what?" );
              end if;
           else
              put_line( "Why? There is no enemy." );
           end if;
         when '8' =>
           -- magic spear
           if creature_here then
              if has_item( spear ) then
                 has_item( spear ) := false;
                 if creature = serpent then
                    put_line( "The spear bounces harmlessly against its scales." );
                 else
                    put_line( "With a groan, you heave the javelin and it pierces its chest!" );
                    damage_to_creature := integer(numerics.rnd( 4 )) + spear_damage;
                    if creature = draco then
                       damage_to_creature := damage_to_creature * 4;
                       put_line( "The dragon screams in pain!" );
                    end if;
                 end if;
                 exit;
              else
                 put_line( "Throw what?" );
              end if;
           else
              put_line( "Why? There is no enemy." );
           end if;
         when '9' =>
           -- magic potion
           if has_item( potion ) then
              has_item( potion ) := false;
              if level > 2 and hit_points < 5 then
                 put_line( "As if you really had any other option..." );
                 delay 2.0;
              elsif level > 2 and hit_points > 15 then
                 put_line( "What a waste..." );
                 delay 2.0;
              end if;
              put_line( "Gulping down the potion, your wounds are magically healed." );
              hit_points := 20;
              exit;
           else
              put_line( "Drink what?" );
           end if;
         when 'r'|'R' =>
            -- review stats
            show_status;
         when others =>
            put_line( "Pardon?" );
         end case;
       end loop;
       end if; -- paralysis

       -- post command tests

       hit_points := hit_points - damage_to_player;
       if hit_points <= 0 then
          status := dead;
       end if;
       if distance <= 0 or status = dead then
          exit;
       end if;

       -- apply battle damage

       if creature /= no_creature then
          creature_hp := creature_hp - damage_to_creature;
          if creature_hp <= 0 then
             declare
                ch : character;
                bonus_points : constant natural := level * 20;
             begin
                new_line;
                put_line( "You have defeated the " & creature_name & "." );
                delay 1.0;
                put( tty_inverse & "BONUS: " & tty_normal );
                put_line( bonus_points );
                quest_bonus := quest_bonus + bonus_points;
                creature := no_creature;
                creature_here := false;
                put( "Press any key *" & ASCII.BS );
                ch := inkey;
             end;
          else
             delay 2.0;
          end if;
       else
          delay 2.0;
       end if;

  end loop; -- main (round) loop

  -- Level Complete

  if status = alive then
     declare
        item_bonus  : natural := 0;
        level_bonus : natural := 0;
     begin
        clear;
        put_line( tty_inverse & "Draco II" & tty_normal );
        new_line;
        put( "Congratulations!  You made it through level" );
        put( level );
        put_line( "!!" );
        new_line;
        for i in staff..ring loop
            if has_item( i ) then
               item_bonus := item_bonus + 10;
            end if;
        end loop;
        put( "Score:       " );
        put( score, "ZZZZZ9" );
        new_line;
        new_line;
        delay 1.0;
        put( "Quest bonus: " );
        put( quest_bonus, "ZZZZZ9" );
        new_line;
        delay 0.5;
        put( "Item bonus:  " );
        put( item_bonus, "ZZZZZ9" );
        new_line;
        level_bonus := numerics.rounding( (level_distance * 10) / round + level * 10 );
        delay 0.5;
        put( "Level bonus: " );
        put( level_bonus, "ZZZZZ9" );
        new_line;
        score := score + integer( quest_bonus + item_bonus + level_bonus );
        delay 0.5;
        new_line;
        put( "Total score: " );
        put( score, "ZZZZZ9" );
        new_line;
        put( "Press any key *" & ASCII.BS );
        declare
          ch : constant character := inkey;
        begin
          put_line( ch );
        end;
     end;
  elsif status = dead then

     -- Player blew it

     new_line;
     if creature = druid then
        put_line( "The druid laughs as you fall gasping to the earth." );
     else
        put_line( "You made a very nice lunch for a very hungry creature." );
     end if;
     delay 1.0;
     new_line;
     put( "Final score: " );
     put( score, "ZZZZZ9" );
     new_line;
     if score > high_score then
        put_line( tty_inverse & "NEW HIGH SCORE!" & tty_normal );
        high_score := score;
        set_high_score;
     end if;
     put_line( "Do come again sometime!" );
     new_line;
     exit;
  end if;

  end loop; -- game (level) loop

end draco_ii;

-- VIM editor formatting instructions
-- vim: ft=spar

