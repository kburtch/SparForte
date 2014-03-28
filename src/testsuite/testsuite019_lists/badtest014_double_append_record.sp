  l1 : doubly_linked_lists.list;
  type r is record
     i : integer;
  end record;
  r1 : r;
  a  : array( 0..1 ) of integer;
  doubly_linked_lists.new_list( l1, integer );
  doubly_linked_lists.append( l1, r1 );

