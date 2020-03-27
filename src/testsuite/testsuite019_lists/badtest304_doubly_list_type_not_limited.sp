procedure p is
  type int_list is new doubly_linked_lists.list( integer );
  il : int_list;
begin
  doubly_linked_lists.clear( il );
end p;

