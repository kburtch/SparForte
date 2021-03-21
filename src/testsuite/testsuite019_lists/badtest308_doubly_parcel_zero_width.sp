procedure p is
  sl : doubly_linked_lists.list( string );
  s  : constant string := "test";
begin
  doubly_linked_lists.clear( sl );
  doubly_linked_lists.parcel( s, 0, sl );
end p;

