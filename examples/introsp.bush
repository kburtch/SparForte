#!/usr/local/bin/bush

pragma annotate( "verify the version/revision of your currently running" );
pragma annotate( "(compiler/interpreter/byte-compiler/runtime environment/" );
pragma annotate( "whatever your language uses) and exit if it is too old." );
pragma annotate( "check whether the variable 'bloop' exists and whether the" );
pragma annotate( " math-function 'abs()' is available and if yes compute" );
pragma annotate( "abs(bloop).  Extra credit: Report the number of integer" );
pragma annotate( "variables in global scope, and their sum." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Introspection" );
pragma annotate( "" );
pragma annotate( "by ken O. Burtch" );

procedure introsp is
   bloop : integer := -5;
   e     : string;
   s     : string;
begin

   -- get initial environment

   e := `env;`;

   -- version number is not available (yet) under bush

   -- the bloop test

   s := `echo $e | grep "^bloop\ " ;` ;
   if strings.length( s ) > 0 then
      ? "bloop exists";
      s := `echo $e | grep "^abs\ " ;` ;
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
     l   : string;
     ch  : character;
     cnt : natural := 0;
     sum : integer := 0;
   begin
     while pos < strings.length( e ) loop
       l := "";
       loop
         pos := @+1;
         exit when pos > strings.length( e );
         ch := strings.element( e, positive( pos ) );
         exit when ch = ASCII.LF;
         l := @ & ch;
       end loop;
       if strings.index( l, "identifier of the type integer" ) > 0 then
         ? l;
         cnt := @ + 1;
         s := strings.trim( strings.field( l, 2, "|" ), trim_end.both );
         if strings.length( s ) > 0 then
            sum := @ + numerics.value( s );
         end if;
       end if;
     end loop;
     ? "There are" & strings.image( cnt ) & " integer variables";
     ? "Their sum is " & strings.image( sum );
   end;

end introsp;
