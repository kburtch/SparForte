meta inches is new meta;
meta policy pci is new meta;

li : limited integer := 5 tagged inches;
pragma assert( tags.contains_unit( li, inches ) );

li2 : limited integer := 5 tagged policy pci;
pragma assert( tags.contains_policy( li2, pci ) );

