procedure p is
  sl : doubly_linked_lists.list( natural );
  s  : constant string := "test";
begin
  doubly_linked_lists.clear( sl );
  doubly_linked_lists.parcel( s, 4, sl );
end p;

