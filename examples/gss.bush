#!/usr/local/bin/bush

-- A common interview test from Rosetta Code for testing basic programming
-- skills.

pragma annotate( "gss" );
pragma annotate( "" );
pragma annotate( "Given a sequence of integers, find a continuous subsequence which maximizes the" );
pragma annotate( "sum of its elements, that is, the elements of no other single subsequence add" );
pragma annotate( "up to a value larger than this one. An empty subsequence is considered to have" );
pragma annotate( "the sum 0; thus if all elements are negative, the result must be the empty" );
pragma annotate( "sequence. " );

procedure greatest_subsequential_sum is

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

end greatest_subsequential_sum;

