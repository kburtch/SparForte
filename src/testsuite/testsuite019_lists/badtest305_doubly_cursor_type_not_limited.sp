procedure p is
  type int_cursor is new doubly_linked_lists.cursor( integer );
  c1 : int_list;
begin
  doubly_linked_lists.clear( c1 );
end p;

