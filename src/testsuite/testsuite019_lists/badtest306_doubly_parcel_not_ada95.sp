procedure p is
  sl : doubly_linked_lists.list( string );
  s  : constant string := "test";
begin
  pragma ada_95;
  doubly_linked_lists.clear( sl );
  doubly_linked_lists.parcel( s, 40, sl );
end p;

