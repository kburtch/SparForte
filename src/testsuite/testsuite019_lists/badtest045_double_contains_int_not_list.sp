l1 : doubly_linked_lists.list;
i  : integer := 1;
b  : boolean;
doubly_linked_lists.new_list( l1, integer );
doubly_linked_lists.append( l1, 1234 );
doubly_linked_lists.append( l1, 2345 );
doubly_linked_lists.append( l1, 3456 );
b := doubly_linked_lists.contains( i, 2345 ); -- should be list

