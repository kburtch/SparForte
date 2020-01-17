procedure t is
  s : string := "foobar";
  t : string;
begin
  s := s;
  pragma test_report( text );
  pragma test( `t := s;`, "test 1" );
  pragma test_result( t = "foobar" );
  pragma test( `t := s;`, "test 2" );
  pragma test_result( t = "foobar" );
end t;

