l1 : doubly_linked_lists.list;
l2 : doubly_linked_lists.list;
c1 : doubly_linked_lists.cursor;
doubly_linked_lists.new_list( l1, integer );
doubly_linked_lists.new_list( l2, integer );
doubly_linked_lists.append( l1, 1234 );
doubly_linked_lists.append( l1, 2345 );
doubly_linked_lists.append( l1, 3456 );
doubly_linked_lists.append( l2, 3456 );
doubly_linked_lists.new_cursor( c1, integer );
doubly_linked_lists.first( l2, c1 );
doubly_linked_lists.insert_before( l1, c1, 4567 );

