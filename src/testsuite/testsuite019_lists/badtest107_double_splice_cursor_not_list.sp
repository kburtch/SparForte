l1 : doubly_linked_lists.list( string );
l2 : doubly_linked_lists.list( string );
c1 : doubly_linked_lists.cursor( string );
doubly_linked_lists.append( l1, "apple" ) @ ( l1, "banana" ) @ ( l1, "cherry" );
doubly_linked_lists.append( l2, "donut" ) @ ( l2, "eclair" ) @ (l2, "gelato" );
doubly_linked_lists.first( l1, c1 );
doubly_linked_lists.next( c1 );
doubly_linked_lists.splice( c1, c1, l2 );  -- should be l1

