#!/usr/local/bin/spar

pragma annotate( summary, "menu" )
              @( description, "A simple menu without pragma ada_95" )
              @( category, "tutorials" )
              @( author, "Ken O. Burtch" )
              @( see_also, "menu2.html" );
pragma license( unrestricted );

trace false;

declare
  reply     : universal_typeless := 0; -- user's reply
  showMenu  : boolean := true;         -- true if menu is shown before prompt
  directory : string := ".";           -- current directory to list
begin

while true loop

  if showMenu then
     put_line( "Main Menu" );
     new_line;
     put_line( "1. ls" )
            @( "2. ls -l" )
            @( "3. change directory" )
            @( "4. SparForte tracing on" )
            @( "5. SparForte tracing off" )
            @( "6. quit" );
     new_line;
     put_line( "The current directory is " & directory );
     new_line;
     showMenu := false;
  end if;

  put( "==> " );
  reply := get_line;
  if reply = 1 then
     cd (directory) ; ls;
  elsif reply = 2 then
     cd (directory) ; ls -l;
  elsif reply = 3 then
     put( "New directory?" );
     directory := get_line;
  elsif reply = 4 then
     trace true;
  elsif reply = 5 then
     trace false;
  elsif reply = 6 then
     exit;
  else
     put_line( "Please type a number between 1 and 6" );
     new_line;
     showMenu;
  end if;
end loop;

put_line( "Bye!" );

end; -- script

-- VIM editor formatting instructions
-- vim: ft=spar

