#!/usr/local/bin/bush

-- From Rosetta Code: http://rosettacode.org/wiki/Combinations

pragma annotate( "combinations" );
pragma annotate( "" );
pragma annotate( "Given non-negative integers m and n, generate all size m" );
pragma annotate( "combinations of the integers from 0 to n-1 in sorted" );
pragma annotate( " order (each combination is sorted and the entire table" );
pragma annotate( "is sorted" );
pragma annotate( "translated by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure combinations is
  combinations_found : string := "";
  number_of_items : constant natural := 3;
  max_item_value  : constant natural := 5;

  -- get_first_combination
  -- return the first combination (e.g. 0,1,2 for 3 items)

  function get_first_combination return string is
    c : string;
  begin
    for i in 1..number_of_items loop
      c := @ & strings.image( i-1 );
    end loop;
    return c;
  end get_first_combination;

  -- get_last_combination
  -- return the highest value (e.g. 4,4,4 for 3 items
  -- with a maximum value of 5).

  function get_last_combination return string is
    c : string;
  begin
    for i in 1..number_of_items loop
      c := @ & strings.image( max_item_value-1 );
    end loop;
    return c;
  end get_last_combination;
  
  combination : string := get_first_combination;
  last_combination : string := get_last_combination;
  
  item : natural; -- a number from the combination
  bad : boolean; -- true if we know a value is too big
  s : string;    -- a temp string for deleting leading space

begin
  --combination := first_combination;
  put_line( combination );
  while combination /= last_combination loop

    -- the combination is 3 numbers with leading spaces
    -- so the field positions start at 2 (1 is a null string)

    for i in reverse 1..number_of_items loop
        item := numerics.value( strings.field( combination, i+1, ' ') );
        if item < max_item_value-1 then
           item := @+1;
           s := strings.image( item );
           s := strings.delete( s, 1, 1 );
           strings.replace( combination, i+1, s, ' ' );
           bad := false;
           for j in i+1..number_of_items loop
              item := numerics.value( strings.field( combination, j, ' ') );
              if item < max_item_value-1 then
                 item := @+1;
                 s := strings.image( item );
                 s := strings.delete( s, 1, 1 );
                 strings.replace( combination, j+1, s, ' ' );
              else
                 bad := true;
	          end if;
           end loop;
           exit;
        end if;
    end loop;
	if not bad then
	   put_line( combination );
    end if;
  end loop;
end combinations;

