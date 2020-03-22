procedure p is
  type int_list is new abstract doubly_linked_lists.list( integer );
  subtype sub_list is int_list; -- must be limited
  il : sub_list;
begin
  doubly_linked_lists.clear( il );
end p;

