type r is record
     foo : integer;
end record;

l1 : doubly_linked_lists.list( r ); -- records not allowed (yet)
? doubly_linked_lists.length( l1 );

