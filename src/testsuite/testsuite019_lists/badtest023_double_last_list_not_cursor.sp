l1 : doubly_linked_lists.list( integer );
c1 : doubly_linked_lists.cursor( integer );
i  : integer := 1;
doubly_linked_lists.append( l1, 1234 );
doubly_linked_lists.append( l1, 2345 );
doubly_linked_lists.last( l1, l1 ); -- should be a cursor

