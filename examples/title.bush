#!/usr/local/bin/bush

-- Title
--
-- Change the title of an xterm window
-- Created by Ken O. Burtch

pragma annotate( "title" );
pragma annotate( "" );
pragma annotate( "Change the title of an xterm window" );
pragma annotate( "usage: title new-title" );

pragma restriction( no_external_commands );

procedure title is

  procedure usage is
  begin
     put( "usage: " ) @ ( source_info.file );
     put_line( " new-title" );
     command_line.set_exit_status( 0 );
  end usage;

begin

  -- Usage

  if $# /= 1 then
     usage;
     return;
  elsif $1 = "-h" or $1 = "--help" then
     usage;
     return;
  end if;

  -- Print out the control sequence to change the title

  declare
    new_title : string := command_line.argument(1);
  begin
    put_line( ASCII.ESC & "]0;" & new_title & ASCII.BEL );
  end;

  command_line.set_exit_status( 0 );

end title;

-- VIM editor formatting instructions -- vim: ft=bush

