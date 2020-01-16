procedure t is
  s : string := "foobar";
  t : string;
begin
  s := s;
  pragma test_report( xml );
  pragma test( `t := s;` );
  pragma test_result( t = "foobar" );
  rm sparforte_test.xml;
end t;

