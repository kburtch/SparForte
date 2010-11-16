-- Copyright (C) 1994-1998 Grady Booch, David Weller and Simon Wright.
-- All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

-- $Id: bc-containers-lists.ads,v 1.2 2005/02/11 02:59:33 ken Exp $

generic package BC.Containers.Lists is

  -- A single list is a rooted sequence of zero or more items, with a link
  -- from one item to its following item. A double list is a rooted
  -- sequence of zero or more items, with links from one item to its
  -- previous and next items.

  -- Lists are polylithic structures, and hence the semantics of copying,
  -- assignment, and equality involve structural sharing. Care must be
  -- taken in manipulating the same list named by more than one alias.

  -- These classes are not intended to be subclassed.

  -- These abstractions have been carefully constructed to eliminate all
  -- storage leaks, except in the case of intentional abuses. When a list
  -- is manipulated, all items that become unreachable are automatically
  -- reclaimed. Furthermore, this design protects against dangling
  -- references: an item is never reclaimed if there exists a reference to
  -- it.

  -- Unreachable items are those that belong to a list or a sublist whose
  -- head is not designated by any alias. For example, consider the list (A
  -- B C), with the head of the list designated by L1. L1 initially points
  -- to the head of the list, at item A. Invoking the member function Tail
  -- on L1 now causes L1 to point to item B. Because A is now considered
  -- unreachable, the storage associated with item A is reclaimed; the
  -- predecessor of B is now null. Similarly, consider the list (D E F),
  -- with the head of the list designated by both L1 and L2. Both L1 and L2
  -- are aliases that initially point to the head of the list at item
  -- D. Invoking the member function Tail on L1 now causes L1 to point to
  -- item E; L2 is unaffected. Suppose we now invoke the member function
  -- clear on L2. The semantics of this operation are such that only
  -- unreachable items are reclaimed. Thus, the storage associated with
  -- item D is reclaimed, because it is no longer reachable; L2 is now
  -- null, and the predecessor of E is now null. Items E and F are not
  -- reclaimed, because they are reachable through L1.

  -- An alternate abstraction would have been to consider items A and D
  -- reachable for doubly-linked lists in these two circumstances. We
  -- explicitly rejected this design, in order to maintain consistency with
  -- the semantics of the singly-linked list.

  -- It is possible, but not generally desirable, to produce multi-headed
  -- lists. In such cases, the predecessor of the item at the neck of a
  -- multi-headed list points to the most recently attached head.

  -- The singly-linked and doubly-linked lists have a similar protocol,
  -- except that the doubly-linked list adds two operations,
  -- Predecessor and Is_Head. The space semantics of these two classes are
  -- different (the doubly-linked list has one extra pointer per item) and
  -- additionally, their time semantics are slightly different, because of
  -- the optimizations possible with having a previous pointer in the
  -- doubly-linked list class.

end BC.Containers.Lists ;
