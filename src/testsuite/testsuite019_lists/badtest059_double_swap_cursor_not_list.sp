l1 : doubly_linked_lists.list( integer );
c1 : doubly_linked_lists.cursor( integer );
c2 : doubly_linked_lists.cursor( integer );
doubly_linked_lists.append( l1, 1234 );
doubly_linked_lists.append( l1, 2345 );
doubly_linked_lists.append( l1, 3456 );
doubly_linked_lists.first( l1, c1 );
doubly_linked_lists.last( l1, c2 );
doubly_linked_lists.swap( c1, c1, c2 );

