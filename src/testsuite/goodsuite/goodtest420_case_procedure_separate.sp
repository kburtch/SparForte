procedure p is

case procedure separate_case( x : integer; y : out integer ) is
  when 1 => 2;
  when others => 0;
end separate_case;

z : integer := 0;

begin
  separate_case( 1, z );
  pragma assert( z = 2 );
end p;

