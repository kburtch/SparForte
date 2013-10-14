#!/usr/local/bin/spar

pragma annotate( summary, "gss" );
pragma annotate( description, "greatest sequential sum" );
pragma annotate( description, "Given a sequence of integers, find a continuous subsequence which maximizes the" );
pragma annotate( description, "sum of its elements, that is, the elements of no other single subsequence add" );
pragma annotate( description, "up to a value larger than this one. An empty subsequence is considered to have" );
pragma annotate( description, "the sum 0; thus if all elements are negative, the result must be the empty" );
pragma annotate( description, "sequence. " );
pragma annotate( see_also, "http://rosettacode.org/wiki/Greatest_subsequential_sum" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure gss is

  type int_array is array( 1..11 ) of integer;

  a : int_array := (-1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1);
  length : integer := 11;

  beginmax : integer := 0;
  endmax : integer := -1;
  maxsum : integer := 0;
  sum : integer := 0;

begin
 
 for start in arrays.first(a)..length-1 loop
     sum := 0;
     for finish in start..length-1 loop
        sum := @ + a(finish);
        if sum > maxsum then
           maxsum := sum;
           beginmax := start;
           endmax := finish;
        end if;
     end loop;
  end loop;

  for i in beginmax..endmax loop
      ? a(i);
  end loop; 

end gss;

-- VIM editor formatting instructions
-- vim: ft=spar

