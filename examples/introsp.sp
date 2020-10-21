#!/usr/local/bin/spar

pragma annotate( summary, "introsp" )
              @( description, "Verify the version/revision of your currently running" )
              @( description, "(compiler/interpreter/byte-compiler/runtime environment/" )
              @( description, "whatever your language uses) and exit if it is too old." )
              @( description, "check whether the variable 'bloop' exists and whether the" )
              @( description, " math-function 'abs()' is available and if yes compute" )
              @( description, "abs(bloop).  Extra credit: Report the number of integer" )
              @( description, "variables in global scope, and their sum." )
              @( category, "tutorials" )
              @( author, "Ken O. Burtch" )
              @( see_also, "http://rosettacode.org/wiki/Introspection" );
pragma license( unrestricted );

procedure introsp is
   bloop : integer := -5;
   e     : string;
   s     : string;
begin

   -- Verify Interpreter

   ? "System Name: " & System.System_Name;
   ? "System Version: " & System.System_Version;

   -- get initial environment

   e := `env;`;

   -- the bloop test

   s := `echo "$e" | grep "^bloop\ " ;` ;
   if strings.length( s ) > 0 then
      ? "bloop exists";
      s := `echo "$e" | grep "^abs\ " ;` ;
      if strings.length( s ) > 0 then
         ? "abs() exists";
         ? "abs(bloop) = " & strings.image( abs( bloop ) );
      else
         ? "abs does not exist";
      end if;
   else
      ? "bloop does not exist";
   end if;

   -- count global integers and their sum
   --
   -- bit of a cheat...we're counting their value at script startup

   declare
     pos : natural := 0;
     ln  : string;
     ch  : character;
     cnt : natural := 0;
     sum_total : integer := 0;
   begin
     while pos < strings.length( e ) loop
       ln := "";
       loop
         pos := @+1;
         exit when pos > strings.length( e );
         ch := strings.element( e, positive( pos ) );
         exit when ch = ASCII.LF;
         ln := @ & ch;
       end loop;
       if strings.index( ln, "identifier of the type integer" ) > 0 then
         ? ln;
         cnt := @ + 1;
         s := strings.trim( strings.field( ln, 2, "|" ), trim_end.both );
         if strings.length( s ) > 0 then
            sum_total := @ + numerics.value( s );
         end if;
       end if;
     end loop;
     ? "There are" & strings.image( cnt ) & " integer variables";
     ? "Their sum is " & strings.image( sum_total );
   end;

end introsp;

-- VIM editor formatting instructions
-- vim: ft=spar

