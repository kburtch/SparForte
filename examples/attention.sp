#!/usr/local/bin/bush

procedure attention is

pragma annotate( "attention" );
pragma annotate( "" );
pragma annotate( "plays a sound to get the user's attention" );
pragma annotate( "" );
pragma annotate( "Translated from a shell script by Heiner Steven" );
pragma annotate( "http://www.shelldorado.com/scripts/cmds/attention.txt" );
pragma annotate( "by Ken O. Burtch" );

sound_path : constant string := "/usr/share/doc/packages/sox/monkey.au";
devdsp     : constant string := "/dev/dsp";
devaudio   : constant string := "/dev/audio";
tty        : constant string := `tty;`;
played     : boolean := false;

procedure usage is
begin
  put( "usage: " ) @ (source_info.file);
  put_line ( "plays a sound to get the user's attention" );
  command_line.set_exit_status( 1 );
end usage;

procedure beeps is
  -- beep 3 times.  If not available, display a string
  tput_beep  : constant string := `tput bel;`;
  beep  : string := tput_beep;
begin
  if strings.length( beep ) = 0 then
     beep := "[Beep]";
  end if;
  put( beep );
  delay 0.5;
  put( beep );
  delay 0.5;
  put( beep );
end beeps;

begin

  if $# /= 0 then
    usage;
    return;
  else
    if files.is_readable_file( sound_path ) then
       -- audio exists? try to play sound if an .au file
       if files.is_writable( devaudio ) then
          if strings.tail( sound_path, 3 ) = ".au" then
             cat "$sound_path" >> "$devaudio";
             played := true;
          end if;
       end if;
       -- dsp exists? try to play sound
       if not played and files.is_writable( devdsp ) then
          sound.play( sound_path );
          played := true;
       end if;
    else
       put_line( standard_error, source_info.source_location & ": sound file doesn't exist or isn't readable" );
    end if;
  end if;
  if not played then
     beeps;
  end if;
  put(source_info.file) @ ( ": Don't Forget!" );
  new_line;

end attention;

