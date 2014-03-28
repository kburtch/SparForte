type r is record
     foo : integer;
end record;

l1 : doubly_linked_lists.list;
doubly_linked_lists.new_list( l1, r ); -- records not allowed (yet)

