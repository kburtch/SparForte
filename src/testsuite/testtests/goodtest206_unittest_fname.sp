procedure t is
  s : string := "foobar";
  t : string;
begin
  s := s;
  pragma test_report( xml, "sparforte_test.xml" );
  pragma test( `t := s;` );
  pragma test_result( t = "foobar" );
end t;

