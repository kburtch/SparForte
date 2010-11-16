-- Copyright (C) 1994-1999 Grady Booch, Pat Rogers and Simon Wright.
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

-- $Id: bc-support-managed_storage.ads,v 1.2 2005/02/11 02:59:34 ken Exp $

with System.Storage_Pools;
with System.Storage_Elements;

package BC.Support.Managed_Storage is

  pragma Elaborate_Body;

  package SSE renames System.Storage_Elements;
  package SSP renames System.Storage_Pools;

  type Pool( Chunk_Size : SSE.Storage_Count ) is
    new SSP.Root_Storage_Pool with private;
    -- This pool can allocate objects of type Your_Type that are no larger
    -- than Chunk_Size - Pool_Overhead (Alignment => Your_Type'Alignment).
    -- BC.Storage_Error will be raised for attempts to allocate larger
    -- objects.
    -- (Pool_Overhead's Type_Overhead parameter is for use when Your_Type
    -- is unconstrained, so that some additional storage is required to
    -- hold the actual object's constraints).

  procedure Allocate( The_Pool                 : in out Pool;
                      Storage_Address          :    out System.Address;
                      Size_In_Storage_Elements : in     SSE.Storage_Count;
                      Alignment                : in     SSE.Storage_Count );

  procedure Deallocate( The_Pool                 : in out Pool;
                        Storage_Address          : in     System.Address;
                        Size_In_Storage_Elements : in     SSE.Storage_Count;
                        Alignment                : in     SSE.Storage_Count );

  function Storage_Size( This : Pool ) return SSE.Storage_Count;

  function Pool_Overhead( Type_Overhead  : SSE.Storage_Count := 0;
                          Alignment      : SSE.Storage_Count       ) return SSE.Storage_Count;

  procedure Preallocate_Chunks( This : in out Pool;  Count : in Positive );

  procedure Reclaim_Unused_Chunks( This : in out Pool );

  procedure Purge_Unused_Chunks( This : in out Pool );

  function Total_Chunks( This : Pool ) return Natural;

  function Dirty_Chunks( This : Pool ) return Natural;

  function Unused_Chunks( This : Pool ) return Natural;

private

  type Chunk( Payload_Size : SSE.Storage_Count );

  type Chunk_Pointer is access all Chunk;

  type Chunk( Payload_Size : SSE.Storage_Count ) is
    record
      Previous_Sized_Chunk : Chunk_Pointer;
      Next_Sized_Chunk     : Chunk_Pointer;
      Next_Chunk           : Chunk_Pointer;
      Element_Size         : SSE.Storage_Count;
      Alignment            : SSE.Storage_Count;
      Number_Elements      : SSE.Storage_Count;
      Next_Element         : System.Address;
      Payload              : SSE.Storage_Array( 1 .. Payload_Size );
    end record;

  type Pool( Chunk_Size : SSE.Storage_Count ) is
    new SSP.Root_Storage_Pool with
      record
        Head                 : Chunk_Pointer;
        Unused               : Chunk_Pointer;
        Allocated_Chunk_Size : SSE.Storage_Count;
      end record;

  procedure Initialize( This : in out Pool );
  procedure Finalize  ( This : in out Pool );

  function Aligned( Size      : SSE.Storage_Count;
                    Alignment : SSE.Storage_Count ) return SSE.Storage_Offset;

  function New_Allocation( Size : SSE.Storage_Count ) return Chunk_Pointer;

  function Within_Range( Target : System.Address;
                         Base   : Chunk_Pointer;
                         Offset : SSE.Storage_Count ) return Boolean;
  pragma Inline( Within_Range );

  procedure Get_Chunk( Result                 :    out Chunk_Pointer;
                       From                   : in out Pool;
                       Requested_Element_Size : in     SSE.Storage_Count;
                       Requested_Alignment    : in     SSE.Storage_Count );


  use type SSE.Storage_Offset;

  type Empty_Chunk is new Chunk( Payload_Size => 0 );
  Chunk_Overhead : constant SSE.Storage_Count
     := (Empty_Chunk'Size+System.Storage_Unit-1) / System.Storage_Unit;

end BC.Support.Managed_Storage;
