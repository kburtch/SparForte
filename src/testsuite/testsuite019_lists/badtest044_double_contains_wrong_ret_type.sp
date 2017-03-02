l1 : doubly_linked_lists.list( integer );
i  : integer := 1;
doubly_linked_lists.append( l1, 1234 );
doubly_linked_lists.append( l1, 2345 );
doubly_linked_lists.append( l1, 3456 );
i := doubly_linked_lists.contains( l1, "foo" ); -- should be int

