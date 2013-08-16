#!/usr/local/bin/spar

pragma annotate( summary, "minsort" );
pragma annotate( description, "Basic integer min sort, the simplest and slowest kind of sort" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma ada_95;
pragma restriction( no_external_commands );

procedure minsort is

  type data_type is new integer;
  type data_array is array( 1..100 ) of data_type;

  best_value : data_type;
  best       : integer;
  num_data   : data_array;
  data_cnt   : integer := arrays.first( num_data );
  s          : string;

begin

-- Read data

  loop
    put( "Data? (Enter nothing to sort): " );
    s := get_line;
    exit when s = "";
    num_data( data_cnt ) := numerics.value( s );
    data_cnt := data_cnt + 1;
    exit when data_cnt > arrays.last( num_data );
  end loop;
  data_cnt := data_cnt - 1;

-- Sort data
--
-- Start with the lowest array position and try to find a better value
-- for this position.  Continue for all positions (except that last
-- because it will be sorted when last-1 is sorted)
  
  for i in arrays.first(num_data)..data_cnt-1 loop
    best_value := num_data(i);
    best := i;
    for j in i+1..data_cnt loop
      if num_data(j) < best_value then
         best := j;
         best_value := num_data(j);
      end if;
    end loop;
    if best /= i then
       num_data(best) := num_data(i);
       num_data(i) := best_value;
    end if;
  end loop;

-- Show data

  for i in arrays.first(num_data)..data_cnt loop
      put_line( num_data(i) );
  end loop;

end minsort;

-- VIM editor formatting instructions
-- vim: ft=spar

