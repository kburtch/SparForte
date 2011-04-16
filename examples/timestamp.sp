#!/usr/local/bin/bush

procedure timestamp is

pragma annotate( "timestamp" );
pragma annotate( "" );
pragma annotate( "prints the current time at interval seconds (default 60)" );
pragma annotate( "" );
pragma annotate( "Translated from a shell script by Heiner Steven" );
pragma annotate( "http://www.shelldorado.com/scripts/cmds/timestamp.txt" );
pragma annotate( "by Ken O. Burtch" );

delay_amount : duration := 60.0;

procedure usage is
begin
  put( "usage: " ) @ (source_info.file);
  put_line (" [interval]" )
         @ ( "prints the current time at interval seconds (default" &
             strings.image( delay_amount ) &
             ")" );
  command_line.set_exit_status( 1 );
end usage;

begin

  if $# = 0 then
    null;
  elsif $# > 1 then
    usage;
    return;
  elsif $1 = "-h" or $1 = "--help" then
    usage;
    return;
  else
    delay_amount := duration( numerics.value( $1 ) );
    if delay_amount = 0 then
       put_line( standard_error, source_info.source_location & ": delay amount must be greater than zero" );
       command_line.set_exit_status( 192 );
       return;
    end if;
  end if;

  loop
    ? "--------------------------------(" & `date;` & ")------";
    delay delay_amount;
  end loop;

end timestamp;

