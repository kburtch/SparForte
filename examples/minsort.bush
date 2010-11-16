#!/usr/local/bin/bush

-- Basic integer min sort, the simplest and slowest kind of sort

pragma annotate( "Min Sort" );
pragma annotate( "" );
pragma annotate( "A simple, slow sort of integers" );
pragma annotate( "by Ken O. Burtch" );

procedure minsort is

  type data_type is new integer;
  type data_array is array( 1..100 ) of data_type;

  best_value : data_type;
  best       : integer;
  data       : data_array;
  data_cnt   : integer := arrays.first( data );
  s          : string;

begin

-- Read data

  loop
    put( "Data? (Enter nothing to sort): " );
    s := get_line;
    exit when s = "";
    data( data_cnt ) := numerics.value( s );
    data_cnt := data_cnt + 1;
    exit when data_cnt > arrays.last( data );
  end loop;
  data_cnt := data_cnt - 1;

-- Sort data
--
-- Start with the lowest array position and try to find a better value
-- for this position.  Continue for all positions (except that last
-- because it will be sorted when last-1 is sorted)
  
  for i in arrays.first(data)..data_cnt-1 loop
    best_value := data(i);
    best := i;
    for j in i+1..data_cnt loop
      if data(j) < best_value then
         best := j;
         best_value := data(j);
      end if;
    end loop;
    if best /= i then
       data(best) := data(i);
       data(i) := best_value;
    end if;
  end loop;

-- Show data

  for i in arrays.first(data)..data_cnt loop
      put_line( data(i) );
  end loop;

end minsort;

-- VIM editor formatting instructions -- vim: ft=bush

