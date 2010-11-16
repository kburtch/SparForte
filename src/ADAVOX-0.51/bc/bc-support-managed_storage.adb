-- Copyright (C) 1994-2001 Grady Booch, Pat Rogers and Simon Wright.
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

-- $Id: bc-support-managed_storage.adb,v 1.2 2005/02/11 02:59:34 ken Exp $

with Ada.Unchecked_Deallocation;
with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Support.Managed_Storage is

  procedure Assert
  is new BC.Support.Exceptions.Assert ("BC.Support.Managed_Storage");

  package PeekPoke is
    new System.Address_To_Access_Conversions( System.Address );

  function Value_At( Location : System.Address ) return System.Address;

  procedure Put( This : in System.Address;  At_Location : in System.Address );

  pragma Inline( Value_At, Put );


  function Value_At( Location : System.Address ) return System.Address is
  begin
    return PeekPoke.To_Pointer(Location).all;
  end Value_At;

  procedure Put( This : in System.Address;  At_Location : in System.Address ) is
  begin
    PeekPoke.To_Pointer(At_Location).all := This;
  end Put;


  procedure Dispose is
    new Ada.Unchecked_Deallocation( Chunk, Chunk_Pointer );


  procedure Initialize( This : in out Pool ) is
  begin
    This.Allocated_Chunk_Size := Aligned( This.Chunk_Size, System.Word_Size/System.Storage_Unit );
  end Initialize;


  procedure Finalize( This : in out Pool ) is
    Temp, Chunk, Ptr : Chunk_Pointer;
  begin
    Purge_Unused_Chunks( This );
    Ptr := This.Head;
    while Ptr /= null loop
      Chunk := Ptr;
      Ptr := Ptr.Next_Sized_Chunk;
      while Chunk /= null loop
        Temp := Chunk;
        Chunk := Chunk.Next_Chunk;
        Dispose( Temp );
      end loop;
    end loop;
  end Finalize;


  function New_Allocation( Size : SSE.Storage_Count ) return Chunk_Pointer is
  begin
    return new Chunk (Size - Pool_Overhead (Alignment => 1));
  end New_Allocation;


  function Pool_Overhead( Type_Overhead  : SSE.Storage_Count := 0;
                          Alignment      : SSE.Storage_Count       ) return SSE.Storage_Count is
  begin
    return Aligned( Chunk_Overhead+Type_Overhead, Alignment );
  end Pool_Overhead;



  procedure Get_Chunk( Result                 :    out Chunk_Pointer;
                       From                   : in out Pool;
                       Requested_Element_Size : in     SSE.Storage_Count;
                       Requested_Alignment    : in     SSE.Storage_Count ) is

    Next, Start, Stop : System.Address;
    Usable_Chunk_Size : SSE.Storage_Count;

    use type System.Address;
  begin
    Usable_Chunk_Size := From.Allocated_Chunk_Size - Aligned( Chunk_Overhead, Requested_Alignment );
    Assert (Requested_Element_Size <= Usable_Chunk_Size,
            BC.Storage_Error'Identity,
            "Get_Chunk",
            BC.Support.Exceptions.Out_Of_Memory);
    if From.Unused /= null then
      Result := From.Unused;
      From.Unused := From.Unused.Next_Chunk;
    else
      Result := New_Allocation( From.Allocated_Chunk_Size );
    end if;
    Result.Element_Size := Requested_Element_Size;
    Result.Alignment := Requested_Alignment;
    Result.Number_Elements := Usable_Chunk_Size / Requested_Element_Size;
    Start := Result.all'Address + Aligned( Chunk_Overhead, Requested_Alignment );
    Stop  := Start + ( (Result.Number_Elements-1) * Result.Element_Size );
    Next  := Start;
    while Next < Stop loop
      Put( Next + Requested_Element_Size, At_Location => Next );
      Next := Next + Requested_Element_Size;
    end loop;
    Put( System.Null_Address, At_Location => Stop );
    Result.Next_Element := Start;
  end Get_Chunk;


  procedure Allocate( The_Pool                 : in out Pool;
                      Storage_Address          :    out System.Address;
                      Size_in_Storage_Elements : in     SSE.Storage_Count;
                      Alignment                : in     SSE.Storage_Count ) is

    Ptr          : Chunk_Pointer;
    Aligned_Size : SSE.Storage_Offset;
    Previous     : Chunk_Pointer;
    Temp         : Chunk_Pointer;

    use type System.Address;
  begin
    Aligned_Size := Aligned( Size_In_Storage_Elements, Alignment );
    if Aligned_Size = 0 then
      raise Storage_Error;
    end if;
    -- look for a chunk with the right element size and alignment, stopping when no point in continuing
    Ptr := The_Pool.Head;
    while Ptr /= null and then
          ( Aligned_Size > Ptr.Element_Size or Ptr.Alignment /= Alignment )
    loop
      Previous := Ptr;
      Ptr := Ptr.Next_Sized_Chunk;
    end loop;
    if Ptr = null then -- didn't find one
      Get_Chunk( Ptr, The_Pool, Aligned_Size, Alignment );
      if Previous /= null then
        Previous.Next_Sized_Chunk := Ptr;
      else -- last was empty
        The_Pool.Head := Ptr;
      end if;
      Ptr.Previous_Sized_Chunk := Previous; -- null or predecessor sized chunk
      Ptr.Next_Sized_Chunk := null;  -- note chunks are reused when possible so this is necessary
      Ptr.Next_Chunk := null;        -- ditto
    elsif ( Aligned_Size /= Ptr.Element_Size ) or ( Ptr.Next_Element = System.Null_Address ) then
      Get_Chunk( Temp, The_Pool, Aligned_Size, Alignment );
      if Previous /= null then -- list wasn't empty
        Previous.Next_Sized_Chunk := Temp;
      else
        The_Pool.Head := Temp;
      end if;
      Temp.Previous_Sized_Chunk := Previous;
      if Aligned_Size /= Ptr.Element_Size then
        Ptr.Previous_Sized_Chunk := Temp;
        Temp.Next_Sized_Chunk := Ptr;
        Temp.Next_Chunk := null;
      elsif Ptr.Next_Element = System.Null_Address then
        Temp.Next_Sized_Chunk := Ptr.Next_Sized_Chunk;
        Temp.Next_Chunk := Ptr;
      end if;
      Ptr := Temp;
    end if;
    Storage_Address := Ptr.Next_Element;
    Ptr.Next_Element := Value_At( Ptr.Next_Element );
  end Allocate;


  procedure Deallocate( The_Pool                 : in out Pool;
                        Storage_Address          : in     System.Address;
                        Size_In_Storage_Elements : in     SSE.Storage_Count;
                        Alignment                : in     SSE.Storage_Count ) is

    Aligned_Size : SSE.Storage_Offset;
    Ptr          : Chunk_Pointer;
  begin
    Aligned_Size := Aligned( Size_In_Storage_Elements, Alignment );
    if Aligned_Size = 0 then
      return;
    end if;
    Ptr := The_Pool.Head;
    while Ptr /= null and then
      ( Aligned_Size /= Ptr.Element_Size or Ptr.Alignment /= Alignment )
    loop
      Ptr := Ptr.Next_Sized_Chunk;
    end loop;
    Put( Ptr.Next_Element, At_Location => Storage_Address );
    Ptr.Next_Element := Storage_Address;
    -- Note that the effect of the above is that the "linked list" of
    -- elements will span chunks. This is necessary since Deallocate is given
    -- an address of the element, not a pointer to the containing chunk.
  end Deallocate;


  function Storage_Size( This : Pool ) return SSE.Storage_Count is
  begin
    return SSE.Storage_Count'Last; -- well, what else can we say!?
  end Storage_Size;


  procedure Preallocate_Chunks( This : in out Pool;  Count : in Positive ) is
    Ptr : Chunk_Pointer;
  begin
    for K in 1 .. Count loop
      Ptr := New_Allocation( This.Allocated_Chunk_Size );
      Ptr.Next_Chunk := This.Unused;
      This.Unused := Ptr;
    end loop;
  end Preallocate_Chunks;


  function Within_Range( Target : System.Address;  Base : Chunk_Pointer; Offset : SSE.Storage_Count ) return Boolean is
    use type System.Address;
  begin
    return Base.all'Address <= Target and Target < Base.all'Address + Offset;
  end Within_Range;


  procedure Reclaim_Unused_Chunks( This : in out Pool ) is
    Ptr               : Chunk_Pointer;
    Previous          : Chunk_Pointer;
    Chunk             : Chunk_Pointer;
    Temp              : Chunk_Pointer;
    Next_Chunk        : Chunk_Pointer;
    Previous_Chunk    : Chunk_Pointer;
    Usable_Chunk_Size : SSE.Storage_Count;
    Element           : System.Address;

    use SSE;
    use type System.Address;
  begin
    Ptr := This.Head;
    while Ptr /= null loop
      Chunk := Ptr;
      -- Compute the maximum number of elements possible, per chunk, within this sized sublist.
      Compute_Max : while Chunk /= null loop
        Usable_Chunk_Size := This.Allocated_Chunk_Size - Aligned( Chunk_Overhead, Chunk.Alignment );
        Chunk.Number_Elements := Usable_Chunk_Size / Chunk.Element_Size;
        Chunk := Chunk.Next_Chunk;
      end loop Compute_Max;
      -- Now we traverse the "linked list" of elements that span chunks, determining the
      -- containing chunk per element and decrementing the corresponding count (computed as
      -- the max, above).
      Element := Ptr.Next_Element;
      Decrement_Counts : while Element /= System.Null_Address loop
        Chunk := Ptr;
        This_Chunk : while Chunk /= null loop
          if Within_Range( Element, Base => Chunk, Offset => This.Chunk_Size ) then
            Chunk.Number_Elements := Chunk.Number_Elements - 1;
            exit This_Chunk; -- stay with this chunk and check next element
          end if;
          Chunk := Chunk.Next_Chunk;
        end loop This_Chunk;
        Element := Value_At( Element ); -- get next element
      end loop Decrement_Counts;
      -- Now walk each sized sublist and remove those no longer used.
      Previous_Chunk := null;
      Chunk := Ptr;
      Reclaiming : while Chunk /= null loop
        if Chunk.Number_Elements = 0 then -- remove it
          if Previous_Chunk /= null then
            Previous_Chunk.Next_Chunk := Chunk.Next_Chunk;
            Chunk.Next_Chunk := This.Unused;
            This.Unused := Chunk;
            Chunk := Previous_Chunk.Next_Chunk;
          else
            Temp := Chunk.Next_Chunk;
            Next_Chunk := Chunk.Next_Sized_Chunk;
            if Temp /= null then
              if Previous /= null then
                Previous.Next_Sized_Chunk := Temp;
              else
                This.Head := Temp;
              end if;
              Temp.Previous_Sized_Chunk := Previous;
              Temp.Next_Sized_Chunk := Next_Chunk;
              Temp.Next_Element := Chunk.Next_Element;
            else
              if Previous /= null then
                Previous.Next_Sized_Chunk := Next_Chunk;
              else
                This.Head := Next_Chunk;
              end if;
            end if;
            if Next_Chunk /= null then
              if Temp /= null then
                Next_Chunk.Previous_Sized_Chunk := Temp;
              else
                Next_Chunk.Previous_Sized_Chunk := Previous;
              end if;
            end if;
            Chunk.Next_Chunk := This.Unused;
            This.Unused := Chunk;
            Chunk := Temp;
          end if;
        else
          Previous_Chunk := Chunk;
          Chunk := Chunk.Next_Chunk;
        end if;
      end loop Reclaiming;
      Previous := Ptr;
      Ptr := Ptr.Next_Sized_Chunk;
    end loop;
  end Reclaim_Unused_Chunks;


  procedure Purge_Unused_Chunks( This : in out Pool ) is
    Current : Chunk_Pointer;
  begin
    while This.Unused /= null loop
      Current := This.Unused;
      This.Unused := This.Unused.Next_Chunk;
      Dispose( Current );
    end loop;
  end Purge_Unused_Chunks;


  function Total_Chunks( This : Pool ) return Natural is
  begin
    return Dirty_Chunks(This) + Unused_Chunks(This);
  end Total_Chunks;


  function Dirty_Chunks( This : Pool ) return Natural is
    Result      : Natural := 0;
    All_Chunks  : Chunk_Pointer;
    Sized_Chunk : Chunk_Pointer;
  begin
    All_Chunks := This.Head;
    while All_Chunks /= null loop
      Sized_Chunk := All_Chunks;
      All_Chunks := All_Chunks.Next_Sized_Chunk;
      while Sized_Chunk /= null loop
        Result := Result + 1;
        Sized_Chunk := Sized_Chunk.Next_Chunk;
      end loop;
    end loop;
    return Result;
  end Dirty_Chunks;


  function Unused_Chunks( This : Pool ) return Natural is
    Ptr    : Chunk_Pointer;
    Result : Natural := 0;
  begin
    Ptr := This.Unused;
    while Ptr /= null loop
      Result := Result + 1;
      Ptr := Ptr.Next_Chunk;
    end loop;
    return Result;
  end Unused_Chunks;


  function Aligned( Size      : SSE.Storage_Count;
                    Alignment : SSE.Storage_Count ) return SSE.Storage_Offset is
    use type SSE.Storage_Count;
  begin
     return ((Size + Alignment - 1) / Alignment) * Alignment;
  end Aligned;


end BC.Support.Managed_Storage;

