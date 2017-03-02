l1 : doubly_linked_lists.list( integer );
c1 : doubly_linked_lists.cursor( integer );
type r is record
   i : integer;
end record;
r1 : r;
doubly_linked_lists.append( l1, 1234 );
doubly_linked_lists.append( l1, 2345 );
doubly_linked_lists.append( l1, 3456 );
doubly_linked_lists.find( l1, r1, c1 );

