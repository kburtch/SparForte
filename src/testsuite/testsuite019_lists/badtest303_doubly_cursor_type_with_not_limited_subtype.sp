procedure p is
  type int_cursor is new abstract doubly_linked_lists.cursor( integer );
  subtype sub_cursor is int_cursor; -- must be limited
  c1 : sub_list;
begin
  doubly_linked_lists.clear( c1 );
end p;

