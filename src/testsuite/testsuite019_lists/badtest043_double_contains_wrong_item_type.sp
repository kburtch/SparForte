l1 : doubly_linked_lists.list;
i  : integer := 1;
doubly_linked_lists.new_list( l1, integer );
doubly_linked_lists.append( l1, 1234 );
doubly_linked_lists.append( l1, 2345 );
doubly_linked_lists.append( l1, 3456 );
i := doubly_linked_lists.contains( l1, 1234 ); -- should be boolean

