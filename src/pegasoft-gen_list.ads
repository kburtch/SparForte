------------------------------------------------------------------------------
-- Generic List Package                                                     --
--                                                                          --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  This is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------
--
-- A generic package for handling general purpose linked lists.
-- eg. package AnIntegerList is new list_manager(integer, "=", ">=");
--     MyIntegerList : AnIntegerList.List;

with Unchecked_Deallocation;

generic

  -- There are three generic parameters:
  --   AListElement -- what the list is made of
  --   >=           -- used by find to match an item
  --   >=           -- used by insert to maintain a sorted list

  type AListElement is private;
  with function "=" (x, y : AListElement ) return boolean is <>;
  with function ">=" (x, y : AListElement ) return boolean is <>;

package pegasoft.gen_list is

---> List Structure
--
-- A List is composed of a list header (List) with a single-linked
-- list chain of records (AListRecord).
--
-- The header contains convenient pointers to the first and last
-- records, as well as a count of the number of records.
--
-- As a quick way to increase speed on small lists that undergo constant
-- change (eg. the event queue), the "FreeCache" field points to a single
-- record (if not null).
--
-- Note: Should be a controlled type (so memory can be discarded
--       during finalization), but no controlled types in gnat 2.0.
--       Make sure all lists are cleared before discarding them!

type AListRecord;
type AListRecordPtr is access AListRecord;
--pragma Controlled( AListRecord ); -- no garbage collection, Ada!
subtype AListIndex is long_integer range 0..long_integer'last;
-- yeah, AListIndex should be it's own type, but then you have to "use"
-- the list to make operations like "+" visible, because it's a generic.
-- That's too inconvenient.

type AListRecord is record
     Data : AListElement;       -- the user data
     Next : AListRecordPtr;     -- pointer to next item
end record;

type List is private;
type ListPtr is access all List;
-- and, yeah, List should be (and was) limited private, but then it can't
-- be used in objects!  Where do these stupid rules come from?!

---> Package Level Operations
--
-- GetAllocation - get number of bytes allocated (for memory leak testing)
-- MemoryLeak    - checks if current number of records is same as allocation

procedure GetAllocation( allocation : out AListIndex );
function  MemoryLeak( allocation : in AListIndex ) return boolean;

---> List Level Operations
--
-- Compact - remove records waiting for deallocation & general tidying
--         - (this procedure's use is optional)
-- Clear   - removes all items, disposes of all allocated memory
-- Copy    - makes one (or two) copies of a list
-- Move    - move list from one list variable to another
-- Swap    - swap lists in two variables

procedure Compact( TheList : in out List );
procedure Clear( TheList : in out List );
procedure Copy( FromList, ToList : in out List );
procedure Copy( FromList, ToList1, ToList2 : in out List );
procedure Move( FromList, ToList : in out List );
procedure Swap( List1, List2 : in out List );

---> Adding Items
--
-- Push = add to front of list
-- Queue = add to end of list
-- Insert = add to a priority queue

procedure Push( TheList : in out List; NewData : AListElement );
procedure Queue( TheList : in out List ; Data : AListElement );
procedure Insert( TheList : in out List ; Data : AListElement );
procedure Insert( TheList : in out List ; atIndex : AListIndex;
                  Data : AListElement );

---> Removing Items
--
-- Free should be declared in the body (oh, well..)
-- Pull - remove an object from the front of a list
-- Clear - remove the nth object from a list; returns Constraint_Error
--         if index is out of range

procedure Free is new Unchecked_Deallocation(
       Object => AListRecord,
       Name   => AListRecordPtr );
procedure Pull( TheList : in out List ; data : in out AListElement );
procedure Pull( TheList : in out List );
procedure Cut( TheLIst : in out List; atIndex : AListIndex;
   data : in out AListElement );
procedure Clear( TheList : in out List ; atIndex : AListIndex );

---> Find and replace
--
-- Find - return nth element of the list
--      - or find next occurance of data
-- Replace - rewrites the data in element n

procedure Replace( TheList : in out List; atIndex : AListIndex;
          data : AListElement );
procedure Find( TheList : in out List ; atIndex : AListIndex ;
          data : in out AListElement );
procedure Find( TheList : in out List ; data : AListElement;
          start : AListIndex := 1; FoundAt : in out AListIndex );

---> List attributes
--
-- Length  = number of items in the list
-- IsEmpty = true of no items in list (what else?!)

function Length( TheList : in List ) return AListIndex;
function IsEmpty( TheList : in List ) return boolean;

---> Sublists and List Arithmetic
--
-- SubList - extract a portion of a list, mainly for list controls
-- Concat  - add two lists together

procedure SubList( TheList : in out List; index, len : AListIndex;
   Result : in out List );
procedure Concat( List1, List2 : List; Result : in out List );

private ----------------------------------------------------------

---> List Definition

type List is record
     First     : AListRecordPtr := null; -- the first record in list (or null)
     Last      : AListRecordPtr := null; -- the last record in list
     Count     : AListIndex     :=    0; -- number of records in the list
     FreeCache : AListRecordPtr := null; -- pointer to a recyclable record
     LastRec   : AListIndex     :=    0; -- last record accessed (or undefined)
     LastPtr   : AListRecordptr := null; -- last record accessed (or nil)
end record;

end pegasoft.gen_list;

