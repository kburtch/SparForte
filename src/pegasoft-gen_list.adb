------------------------------------------------------------------------------
-- GEN LIST (package body)                                                  --
--                                                                          --
-- Part of TextTools                                                        --
-- Designed and Programmed by Ken O. Burtch                                 --
--                                                                          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This is maintained at http://www.vaxxine.com/pegasoft                    --
--                                                                          --
------------------------------------------------------------------------------
--
-- Started September 5/95 by Ken O. Burtch
--
-- Package Body Notes:
--
-- 1. I avoid pragma Inline in order to keep the code compact.  Where
--    I could have used Inline, it was usually better to insert optimzed
--    code to do the same (eg. Push/Pull).

--with Text_IO; use Text_IO; -- for debugging

package body pegasoft.gen_list is
  pragma suppress( all_checks );

NullList : constant List := (null,null,0,null,0,null);

RecordsAllocated : AListIndex := 0;

procedure GetAllocation( allocation : out AListIndex ) is
-- return number of outstanding records allocated
begin
  allocation := RecordsAllocated * (AListElement'size / 8);
end GetAllocation;

function MemoryLeak( allocation : in AListIndex ) return boolean is
-- note if memory leakage has occurred between allocation time & now
begin
  return ( RecordsAllocated * ( AListElement'size / 8 )) /= Allocation;
end MemoryLeak;

procedure Compact( TheList : in out List ) is
-- optimize the list
begin
  if TheList.FreeCache /= null then
     Free( TheList.FreeCache );
  end if;
end Compact;

procedure Clear( TheList : in out List ) is
-- erase the entire list
  TmpPtr, TmpPtr2 : AListRecordPtr;
begin
  TmpPtr := TheList.First;
  while TmpPtr /= null loop
     TmpPtr2 := TmpPtr.next;
     Free( TmpPtr );
     RecordsAllocated := RecordsAllocated - 1;
     TmpPtr := TmpPtr2;
  end loop;
  if TheList.FreeCache /= null then
     Free( TheList.FreeCache );
     RecordsAllocated := RecordsAllocated - 1; -- compensate for one not d'ed.
  end if;
  TheList := NullList;
end Clear;

function Length( TheList : in List ) return AListIndex is
-- return the length of the list
begin
  return TheList.count;
end Length;

function IsEmpty( TheList : in List ) return boolean is
-- could check count, but this is more absolute
begin
  return TheList.First = null;
end IsEmpty;

procedure Push( TheList : in out List; newdata : AListElement ) is
-- add a record to the start of the list & update the count
  TmpPtr : AListRecordPtr;
begin
  if TheList.FreeCache = null then   -- allocate a new record
     TmpPtr := new AListRecord;
     RecordsAllocated := RecordsAllocated + 1;
  else
     TmpPtr := TheList.FreeCache;    -- or grab one from the cache
     TheList.FreeCache := null;
  end if;
  TmpPtr.Data := newdata;            -- put data in it
  if TheList.First = null then       -- add to an empty list
     TheList.First := TmpPtr;
     TheList.Last := TmpPtr;
     TmpPtr.Next := null;
  else
     TmpPtr.next := TheList.first;   -- push it
     TheList.first := TmpPtr;
  end if;
  TheList.count := TheList.count + 1; -- adjust the count
  if TheList.LastPtr /= null then
     TheList.LastRec := TheList.LastRec + 1;
  end if;
end Push;

procedure Queue( TheList : in out List ; Data : AListElement ) is
-- add a record to the end of the list & update the count
  TmpPtr : AListRecordPtr;
begin
  if TheList.FreeCache = null then  -- allocate a new record
     TmpPtr := new AListRecord;
     RecordsAllocated := RecordsAllocated + 1;
  else
     TmpPtr := TheList.FreeCache;   -- or grab one from the cache
     TheList.FreeCache := null;
  end if;
  TmpPtr.Data := Data;              -- assign data
  TmpPtr.Next := null;              -- nothing will be after it
  if TheList.First = null then      -- add to an empty list
     TheList.First := TmpPtr;
  else
     TheList.Last.Next := TmpPtr;   -- or append to the end
  end if;
  TheList.Last := TmpPtr;             -- always the last record
  TheList.Count := TheList.Count + 1; -- add one to the count
end Queue;

procedure Insert( TheList : in out List ; Data : AListElement ) is
-- insert a record into the list in ascending order & update the count
  NewRec : AListRecordPtr;
  TmpPtr, TmpPtr2 : AListRecordPtr;
begin
  TmpPtr2 := null;                      -- ready to search the list
  TmpPtr := TheList.First;
  while TmpPtr /= null loop             -- quit if we reach the end
     exit when TmpPtr.Data >= Data;     --  or if we exceed the sort value
     TmpPtr2 := TmpPtr;
     TmpPtr := TmpPtr.Next;
  end loop;
  if TmpPtr = null then                 -- queue it if we reached the end
     Queue( TheList, Data );
  elsif TmpPtr2 = null then             -- or push it if goes in front
     Push( TheList, Data );
  else
     if TheList.FreeCache = null then   -- else allocate a new record
        NewRec := new AListRecord;
        RecordsAllocated := RecordsAllocated + 1;
     else
        NewRec := TheList.FreeCache;    -- or grab one from the cache
        TheList.FreeCache := null;
     end if;
     NewRec.Data := Data;               -- copy data
     NewRec.Next := TmpPtr;             -- goes before TmpPtr
     TmpPtr2.next := NewRec;            -- and after TmpPtr2
     TheList.Count := TheList.Count + 1; -- update the count
     TheList.LastPtr := null;            -- last position cache
  end if;
end Insert;

procedure Insert( TheList : in out List ; atIndex : AListIndex;
   data : AListElement ) is
-- insert a record into the list at the index & update the count
  NewRec : AListRecordPtr;
  TmpPtr, TmpPtr2 : AListRecordPtr;
  Count : AListIndex;
begin
  if atIndex > 0 and atIndex <= TheList.Count then
    Count := 0;                      -- get ready to search list
    TmpPtr2 := null;
    TmpPtr := TheList.First;
    while TmpPtr /= null loop        -- stop when we hit the end
       Count := Count + 1;
       exit when Count = atIndex;    -- or when we hit the desired record
       TmpPtr2 := TmpPtr;
       TmpPtr := TmpPtr.Next;
    end loop;
    -- note: TmpPtr should NOT be null because of constraint check above
    if TmpPtr2 = null then             -- or push it if goes in front
       Push( TheList, Data );
    else
       if TheList.FreeCache = null then   -- else allocate a new record
          NewRec := new AListRecord;
          RecordsAllocated := RecordsAllocated + 1;
       else
          NewRec := TheList.FreeCache;    -- or grab one from the cache
          TheList.FreeCache := null;
       end if;
       NewRec.Data := Data;               -- copy data
       NewRec.Next := TmpPtr;             -- goes before TmpPtr
       TmpPtr2.next := NewRec;            -- and after TmpPtr2
       TheList.Count := TheList.Count + 1; -- update the count
       TheList.LastRec := atIndex;
       TheList.LastPtr := NewRec;
    end if;
  end if;
end Insert;

procedure Pull( TheList : in out List ; Data : in out AListElement ) is
-- remove a record from the front of the list & update the count
   TmpPtr : AListRecordPtr;
begin
  if TheList.first /= null then             -- if the list isn't empty
     TmpPtr := TheList.First;               -- extract the data
     Data := TheList.First.Data;
     TheList.first := TheList.First.Next;   -- update first pointer
     if TheList.FreeCache = null then       -- add record to free cache
        TheList.FreeCache := TmpPtr;
     else
        Free( TmpPtr );                     -- or deallocate if cache full
        RecordsAllocated := RecordsAllocated - 1;
     end if;
     if TheList.First = null then           -- if last record, fix last
        TheList.Last := null;
     end if;
     TheList.Count := TheList.Count - 1;    -- decrement the count
     TheList.LastPtr := null;               -- assuming the worst
  end if;
end Pull;

procedure Pull( TheList : in out List ) is
-- remove a record from the front of the list and discard it
  data : AListElement;
begin
  Pull( TheList, Data );
end Pull;

procedure Concat( List1, List2 : List; Result : in out List ) is
-- add to lists together and return the results as a third list
-- not very fast -- could rewrite to get rid of queue's
  TmpPtr  : AListRecordPtr;
  TmpList : List;
begin
  TmpPtr := List1.First;             -- move though first list
  while TmpPtr /= null loop
     Queue( TmpList, TmpPtr.Data );  -- add to result list
     TmpPtr := TmpPtr.Next;
  end loop;
  TmpPtr := List2.First;             -- move through second list
  while TmpPtr /= null loop
     Queue( TmpList, TmpPtr.Data );  -- add to result list
     TmpPtr := TmpPtr.Next;
  end loop;
  Clear( Result );                   -- overwrite result
  Result := TmpList;                 -- with new header info
end Concat;

procedure Cut( TheList : in out List ; atIndex : AListIndex;
  data : in out AListElement ) is
-- remove the atIndex-th record from the list and return it
  Count : AListIndex;
  TmpPtr, TmpPtr2 : AListRecordPtr;
begin
  if atIndex > 0 and atIndex <= TheList.Count then
    Count := 0;                      -- get ready to search list
    TmpPtr2 := null;
    TmpPtr := TheList.First;
    while TmpPtr /= null loop        -- stop when we hit the end
       Count := Count + 1;
       exit when Count = atIndex;    -- or when we hit the desired record
       TmpPtr2 := TmpPtr;
       TmpPtr := TmpPtr.Next;
    end loop;
    -- note: TmpPtr should NOT be null because of constraint check above
    data := TmpPtr.data;             -- return the data
    if TmpPtr2 = null then           -- if it's the first then pull
       Pull( TheList );
    else
       TmpPtr2.Next := TmpPtr.Next;  -- else, next follows last one
       if TmpPtr2.Next = null then   -- update last if is the last
          TheList.Last := TmpPtr2;
       end if;
       if TheList.FreeCache = null then -- add to the cache
          TheList.FreeCache := TmpPtr;
       else
          Free( TmpPtr );               -- or discard if cache is full
          RecordsAllocated := RecordsAllocated - 1;
       end if;
       TheList.Count := TheList.Count - 1; -- smaller by 1
       TheList.LastPtr := null;         -- assuming the worst
    end if;
  end if;
end Cut;

procedure Clear( TheList : in out List; atIndex : AListIndex ) is
  Discard : AListElement;
begin
  Cut( TheList, atIndex, Discard );
end Clear;

procedure Replace( TheList : in out List ; atIndex : AListIndex ;
          Data : AListElement ) is
-- replaces the atIndex-th record from the list
  Count  : AListIndex;
  TmpPtr : AListRecordPtr;
begin
  if atIndex > 0 and atIndex <= TheList.Count then
     if TheList.LastPtr /= null and then atIndex >= TheList.LastRec then
        TmpPtr := TheList.LastPtr;      -- use LastPtr if useful
        Count := TheList.LastRec-1;     --   (count inc'd below)
     else
        Count := 0;                     -- start from scratch
        TmpPtr := TheList.First;
     end if;
     while TmpPtr /= null loop
        Count := Count + 1;
        exit when Count = atIndex;
        TmpPtr := TmpPtr.Next;
     end loop;
     -- note: TmpPtr should NOT be null because of constraint check above
     TmpPtr.Data := Data;
  end if;
end Replace;

procedure Find( TheList : in out List ; atIndex : AListIndex ;
          Data : in out AListElement ) is
-- finds the atIndex-th record from the list
  Count : AListIndex;
  TmpPtr : AListRecordPtr;
begin
  if atIndex > 0 and atIndex <= TheList.Count then
     -- was <=
     if TheList.LastPtr /= null and then atIndex >= TheList.LastRec then
        Count := TheList.LastRec-1;    -- use Lastptr if helpful
        TmpPtr := TheList.LastPtr;     --   (count is inc'd below)
     else
        Count := 0;                    -- else start from scratch
        TmpPtr := TheList.first;
     end if;
     while TmpPtr /= null loop
        Count := Count + 1;
        exit when Count = atIndex;
        TmpPtr := TmpPtr.next;
     end loop;
     -- note: TmpPtr should NOT be null because of constraint check above
     data := TmpPtr.data;
     TheList.LastRec := atIndex;  -- last access point
     TheList.LastPtr := TmpPtr;
  end if;
end Find;

procedure Find( TheList : in out List ; data : AListElement;
  start : AListIndex := 1; FoundAt : in out AListIndex ) is
-- locate the first (next) record matching c and return it's index
  Count : AListIndex;
  TmpPtr : AListRecordPtr;
begin
  if start > 0 and start <= TheList.Count then
     if TheList.LastPtr /= null and then start >= TheList.LastRec then
        Count := TheList.LastRec-1; --   (Count is inc'd below)
        TmpPtr := TheList.LastPtr;  -- use LastPtr if it's useful
     else
        Count := 0;                 -- else start from scratch
        TmpPtr := TheList.first;
     end if;
     while TmpPtr /= null loop
       Count := Count + 1;
       exit when (TmpPtr.Data = Data) and then (Count >= Start);
       TmpPtr := TmpPtr.next;
     end loop;
     if TmpPtr = null then
        Count := 0;
     else
       TheList.LastRec := Count;   -- remember where we left off
       TheList.LastPtr := TmpPtr;
     end if;
     FoundAt := Count;
  else -- wierd start or empty list
     FoundAt := 0;
  end if;
end Find;

procedure SubList( TheList : in out List ; index, len : AListIndex;
         result : in out List ) is
-- extract a sublist of length len from position index
  Count   : AListIndex;
  TmpPtr  : AListRecordPtr;
  TmpList : List; -- in case TheList = Result
begin
  if TheList.LastPtr /= null and index >= TheList.LastRec then
     TmpPtr := TheList.LastPtr;            -- use LastPtr if useful
     Count := TheList.LastRec-1;           --   (LastRec inc'd below)
  else
     Count := 0;                           -- else start from scratch
     TmpPtr := TheList.First;
  end if;
  while TmpPtr /= null loop                -- loop until we run out
     Count := Count + 1;
     exit when (Count >= Index);           -- or we find the nth item
     TmpPtr := TmpPtr.Next;
  end loop;
  if TmpPtr /= null then                   -- if we're still in the list
     TheList.LastPtr := TmpPtr;            --    record where we are
     TheList.LastRec := Count;             --    for future speed benefits
  end if;
  Count := 0;                              -- read to build sublist
  while TmpPtr /= null loop                -- loop until we run out
     Queue( TmpList, TmpPtr.data );        -- should rewrite this for speed
     Count := Count + 1;
     exit when (Count >= len );            -- or stop when we have enough
     TmpPtr := TmpPtr.Next;
  end loop;
  Clear( Result );                         -- overwrite result
  Result := TmpList;                       -- with new header info
end SubList;

procedure Copy( FromList, ToList : in out List ) is
-- Make a copy of the ToList
  FromListPtr : AListRecordPtr;
  TmpPtr      : AListRecordPtr;
  TmpList     : List;  -- FromList might be ToList
begin
  FromListPtr := FromList.First;        -- begin at the first record
  if FromListPtr /= null then           -- if there is a first record
     TmpPtr := new AListRecord;         --    add it to the new list
     RecordsAllocated := RecordsAllocated + 1;
     TmpPtr.data := FromListPtr.data;
     TmpList.First := TmpPtr;
     TmpList.Last  := TmpPtr;
     FromListPtr := FromListPtr.next;   --   update pointer
     while FromListPtr /= null loop     -- for any remaining records
       TmpPtr := new AListRecord;       --   create a copy for first list
       RecordsAllocated := RecordsAllocated + 1;
       TmpPtr.data := FromListPtr.data;
       TmpList.Last.Next := TmpPtr;      --   and append it
       TmpList.Last := TmpPtr;
       FromListPtr := FromListPtr.Next; --   update pointer
    end loop;
    TmpList.Count := FromList.Count;    -- fix counts and last.next pointers
    TmpList.Last.Next := null;
    Clear( ToList );                    -- overwrite result
    ToList := TmpList;                  -- with new header info
  end if;
end Copy;

procedure Copy( FromList, ToList1, ToList2 : in out List ) is
-- Make two copies of the FromList
  FromListPtr : AListRecordPtr;
  TmpPtr      : AListRecordptr;
  List1, List2: List; -- ToList1 or ToList2 might be same as FromList
begin
  FromListPtr := FromList.First;        -- begin at the first record
  if FromListPtr /= null then           -- if there is a first record
     TmpPtr := new AListRecord;         --    add it to the first list
     RecordsAllocated := RecordsAllocated + 1;
     TmpPtr.data := FromListPtr.data;
     List1.First := TmpPtr;
     List1.Last  := TmpPtr;
     TmpPtr := new AListRecord;         --    and to the second
     RecordsAllocated := RecordsAllocated + 1;
     TmpPtr.data := FromListPtr.data;
     List2.First := TmpPtr;
     List2.Last  := TmpPtr;
     FromListPtr := FromListPtr.next;   --   update pointer
     while FromListPtr /= null loop     -- for any remaining records
       TmpPtr := new AListRecord;       --   create a copy for first list
       RecordsAllocated := RecordsAllocated + 1;
       TmpPtr.data := FromListPtr.data;
       List1.Last.Next := TmpPtr;     --   and append it
       List1.Last := TmpPtr;
       TmpPtr := new AListRecord;       --   create a copy for second list
       RecordsAllocated := RecordsAllocated + 1;
       TmpPtr.data := FromListPtr.data;
       List2.Last.Next := TmpPtr;     --   and append it, too
       List2.Last := TmpPtr;
       FromListPtr := FromListPtr.Next; --   update pointer
    end loop;
    List1.Count := FromList.Count;    -- fix counts and last.next pointers
    List1.Last.Next := null;
    List2.Count := FromList.Count;
    List2.Last.Next := null;
    Clear( ToList1 );                 -- overwrite result lists
    Clear( ToList2 );
    ToList1 := List1;                 -- with new header info
    ToList2 := List2;
  end if;
end Copy;

procedure Move( FromList, ToList : in out List ) is
-- move a list between two list variables
begin
  if FromList.first /= ToList.first then
     Clear( ToList );
     ToList := FromList;
     FromList := NullList;
  end if;
end Move;

procedure Swap( List1, List2 : in out List ) is
  TmpList : List;
begin
  if List1.first /= List2.first then
     Move( FromList => List1, ToList => TmpList );
     Move( FromList => List2, ToList => List1 );
     Move( FromList => TmpList, ToList => List2 );
  end if;
end Swap;

end pegasoft.gen_list;

