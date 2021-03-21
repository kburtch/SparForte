procedure p is
  sl : doubly_linked_lists.list( string );
  n  : constant natural := 5;
begin
  doubly_linked_lists.clear( sl );
  doubly_linked_lists.parcel( n, 0, sl );
end p;

