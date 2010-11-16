-- $Id: wc-streams-endian.adb,v 1.2 2005/02/11 02:59:39 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Unchecked_Conversion;

package body WC.Streams.Endian is

   CVSID : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-endian.adb,v 1.2 2005/02/11 02:59:39 ken Exp $";

   type String_8 is new String(1..8);                              -- For conversion of 64 bit values
   type String_4 is new String(1..4);                              -- For conversion of 32 bit values
   type String_2 is new String(1..2);                              -- For conversion of 16 bit values

   type I24 is array(1..3) of Integer_8;
   type U24 is array(1..3) of Unsigned_8;

   --------------------------------------------------
   -- Raise Mapping_Error, if the Mapping is incomplete :
   --------------------------------------------------
   procedure Check_Mapping(Map : Endian_Mapping)
   is
      F :  Endian_Index := Endian_Index(Map'First);
      L :  Endian_Index := Endian_Index(Map'Last);
   begin

      for X in Map'Range loop
         if Map(X) < F or else Map(X) > L then
            raise Mapping_Error;
         end if;
      end loop;

   end Check_Mapping;

   --------------------------------------------------
   -- Establish a Mapping between Magic Value and Actual :
   --------------------------------------------------
   procedure Set_Mapping(M : String; A : String; Map : out Endian_Mapping) is
   begin

      Map := ( 0, others => 0 );

      for X in A'Range loop
         for Y in M'Range loop
            if A(X) = M(Y) then
               Map(Y) := Endian_Index(X);
               exit;
            end if;
         end loop;
      end loop;
      
      Check_Mapping(Map);

   end Set_Mapping;

   --------------------------------------------------
   -- Define an Endian Mapping for the current host :
   --
   -- Argument Magic is the actual magic value (in host's form) :
   -- Argument Actual is the value actually obtained from the file (or net)
   --
   -- Returned is Map4 and Map2 for mapping 32 and 16 bit values respectively
   --
   --------------------------------------------------
   -- This procedure uses 64 bit magic values :
   --------------------------------------------------
   procedure Host_Map(Magic : Unsigned_64; Actual : Unsigned_64; Map8 : out Endian_Mapping_8; Map4 : out Endian_Mapping_4; Map2 : out Endian_Mapping_2)
   is
      function Conv is new Ada.Unchecked_Conversion(Unsigned_64,String_8);

      M :   String_8;
      A :   String_8;
   begin

      M := Conv(Magic);
      A := Conv(Actual);        
      Map8 := (0, others => 0 );
    
      Set_Mapping(String(M),String(A),Endian_Mapping(Map8));

      if Map8(1) > 4 then                             -- This won't work for every possible piece of hardware,
         Map4(1..4) := Endian_Mapping_4(Map8(5..8));  -- but it should cover all modern ones
      else                                            
         Map4(1..4) := Endian_Mapping_4(Map8(1..4));
      end if;

      if Map4 = (1,2,3,4) then
         Map2 := (1,2);              
      else
         Map2 := (2,1);
      end if;

   end Host_Map;

   --------------------------------------------------
   -- This procedure uses 32 bit magic values :
   --------------------------------------------------
   procedure Host_Map(Magic : Unsigned_32; Actual : Unsigned_32; Map4 : out Endian_Mapping_4; Map2 : out Endian_Mapping_2)
   is
      function Conv is new Ada.Unchecked_Conversion(Unsigned_32,String_4);

      M :   String_4;
      A :   String_4;
   begin

      M     := Conv(Magic);
      A     := Conv(Actual);        
      Map4  := (0,0,0,0);

      Set_Mapping(String(M),String(A),Endian_Mapping(Map4));

      if M(1) = A(1) then
         Map2 := (1,2);
      else
         Map2 := (2,1);
      end if;

      if Map2 = (0,0) then
         raise Program_Error;
      end if;

   end Host_Map;

   --------------------------------------------------
   -- This procedure uses 16 bit magic values :
   --------------------------------------------------
   procedure Host_Map(Magic : Unsigned_16; Actual : Unsigned_16; Map2 : out Endian_Mapping_2)
   is
      function Conv is new Ada.Unchecked_Conversion(Unsigned_16,String_2);

      M :   String_2;
      A :   String_2;
   begin

      M := Conv(Magic);
      A := Conv(Actual);        

      Set_Mapping(String(M),String(A),Endian_Mapping(Map2));

   end Host_Map;

   --------------------------------------------------
   -- Establish mappings based upon 64 bit magic #
   --------------------------------------------------
   procedure Map(Strm : in out Endian_Stream_Type; Magic : Unsigned_64; Actual : Unsigned_64) is
   begin

        Host_Map(Magic,Actual,Strm.Map_64,Strm.Map_32,Strm.Map_16);

   end Map;

   --------------------------------------------------
   -- Establish mappings based upon 32 bit magic #
   --------------------------------------------------
   procedure Map(Strm : in out Endian_Stream_Type; Magic : Unsigned_32; Actual : Unsigned_32) is
   begin

      Host_Map(Magic,Actual,Strm.Map_32,Strm.Map_16);
      if Strm.Map_32 = (1,2,3,4) then
         Strm.Map_64 := (1,2,3,4,5,6,7,8);      -- Assume that 64 bit values are the same
      else
         Strm.Map_64 := (8,7,6,5,4,3,2,1);      -- This is not always a safe assumption..
      end if;                                   -- some hardware is wierd.

   end Map;

   --------------------------------------------------
   -- Establish mappings based upon 16 bit magic # :
   --------------------------------------------------
   procedure Map(Strm : in out Endian_Stream_Type; Magic : Unsigned_16; Actual : Unsigned_16) is
   begin

      Host_Map(Magic,Actual,Strm.Map_16);
      if Strm.Map_16 = (1,2) then
         Strm.Map_32 := (1,2,3,4);              -- Assume this for 32 bit values
         Strm.Map_64 := (1,2,3,4,5,6,7,8);      -- Assume this ordering for 64 bit values
      else
         Strm.Map_32 := (4,3,2,1);              -- This is not always correct for some hardware
         Strm.Map_64 := (8,7,6,5,4,3,2,1);      -- Ditto.
      end if;

   end Map;

   --------------------------------------------------
   -- Perform an Endian Conversion according to the Map
   --------------------------------------------------
   procedure Endian_Conversion(E : in out String; Map : Endian_Mapping) is
      Temp :   String(1..E'Last);
   begin

      for X in Map'Range loop
         Temp(X) := E(Integer(Map(X)));
      end loop;
      E := Temp;

   end Endian_Conversion;

   --------------------------------------------------
   -- Endian Convert an Integer_64 value :
   --------------------------------------------------
   function Map_Integer_64(Val : Integer_64; Map : Endian_Mapping_8) return Integer_64
   is
      function To_Str is new Ada.Unchecked_Conversion(Integer_64,String_8);
      function To_Int is new Ada.Unchecked_Conversion(String_8,Integer_64);

      E :  String_8;
   begin

      if Map = (1,2,3,4) then
         return Val;                 -- No mapping required
      end if;

      E := To_Str(Val);
      Endian_Conversion(String(E),Endian_Mapping(Map));
      return To_Int(E);

   end Map_Integer_64;

   --------------------------------------------------
   -- Endian Convert an Unsigned_64 value :
   --------------------------------------------------
   function Map_Unsigned_64(Val : Unsigned_64; Map : Endian_Mapping_8) return Unsigned_64
   is
      function To_Str is new Ada.Unchecked_Conversion(Unsigned_64,String_8);
      function To_Int is new Ada.Unchecked_Conversion(String_8,Unsigned_64);

      E :  String_8;
   begin

      if Map = (1,2,3,4) then
         return Val;                 -- No change required
      end if;

      E := To_Str(Val);
      Endian_Conversion(String(E),Endian_Mapping(Map));
      return To_Int(E);

   end Map_Unsigned_64;

   --------------------------------------------------
   -- Endian Convert an Integer_32 value :
   --------------------------------------------------
   function Map_Integer_32(Val : Integer_32; Map : Endian_Mapping_4) return Integer_32
   is
      function To_Str is new Ada.Unchecked_Conversion(Integer_32,String_4);
      function To_Int is new Ada.Unchecked_Conversion(String_4,Integer_32);

      E :  String_4;
   begin

      if Map = (1,2,3,4) then
         return Val;                 -- No mapping required
      end if;
      
      E := To_Str(Val);
      Endian_Conversion(String(E),Endian_Mapping(Map));
      return To_Int(E);

   end Map_Integer_32;

   --------------------------------------------------
   -- Endian Convert an Unsigned_32 value :
   --------------------------------------------------
   function Map_Unsigned_32(Val : Unsigned_32; Map : Endian_Mapping_4) return Unsigned_32
   is
      function To_Str is new Ada.Unchecked_Conversion(Unsigned_32,String_4);
      function To_Int is new Ada.Unchecked_Conversion(String_4,Unsigned_32);

      E :  String_4;
   begin

      if Map = (1,2,3,4) then
         return Val;                 -- No change required
      end if;

      E := To_Str(Val);
      Endian_Conversion(String(E),Endian_Mapping(Map));
      return To_Int(E);

   end Map_Unsigned_32;

   --------------------------------------------------
   -- Map the unusual Endian_I24  type :
   --------------------------------------------------
   function Map_Integer_I24(Val : I24; Map : Endian_Mapping_4) return Endian_I24 is
   begin

      if Map = (1,2,3,4) then
         return Endian_I24(Val);     -- No change is required
      end if;

      if Map = (4,3,2,1) then
         return Endian_I24'( Val(3), Val(2), Val(1) );
      end if;

      raise Mapping_Error;            -- Cannot support the others (unless the offset was known)

   end Map_Integer_I24;

   --------------------------------------------------
   -- Map the unusual Endian_U24  type :
   --------------------------------------------------
   function Map_Integer_U24(Val : U24; Map : Endian_Mapping_4) return Endian_U24 is
   begin

      if Map = (1,2,3,4) then
         return Endian_U24(Val);     -- No change is required
      end if;

      if Map = (4,3,2,1) then
         return Endian_U24'( Val(3), Val(2), Val(1) );
      end if;

      raise Mapping_Error;            -- Cannot support the others (unless the offset was known)

   end Map_Integer_U24;

   --------------------------------------------------
   -- Endian Convert an Integer_16 value :
   --------------------------------------------------
   function Map_Integer_16(Val : Integer_16; Map : Endian_Mapping_2) return Integer_16
   is
      function To_Str is new Ada.Unchecked_Conversion(Integer_16,String_2);
      function To_Int is new Ada.Unchecked_Conversion(String_2,Integer_16);

      E :  String_2;
   begin

      if Map = (1,2) then
         return Val;                 -- No change required
      end if;

      E := To_Str(Val);
      Endian_Conversion(String(E),Endian_Mapping(Map));
      return To_Int(E);
      
   end Map_Integer_16;

   --------------------------------------------------
   -- Endian Convert an Unsigned_16 value :
   --------------------------------------------------
   function Map_Unsigned_16(Val : Unsigned_16; Map : Endian_Mapping_2) return Unsigned_16
   is
      function To_Str is new Ada.Unchecked_Conversion(Unsigned_16,String_2);
      function To_U16 is new Ada.Unchecked_Conversion(String_2,Unsigned_16);

      E :  String_2;
   begin
      if Map = (1,2) then
         return Val;                 -- No change required
      end if;
      
      E := To_Str(Val);
      Endian_Conversion(String(E),Endian_Mapping(Map));
      return To_U16(E);

   end Map_Unsigned_16;

   --------------------------------------------------
   -- Register the Endian Mapping for 64 bit Integers
   --------------------------------------------------
   procedure Map_64(Strm : in out Endian_Stream_Type'Class; Mapping : Endian_Mapping_8) is
   begin

      Strm.Map_64 := Mapping;

   end Map_64;

   --------------------------------------------------
   -- Register the Endian Mapping for 32 bit Integers
   --------------------------------------------------
   procedure Map_32(Strm : in out Endian_Stream_Type'Class; Mapping : Endian_Mapping_4) is
   begin

      Strm.Map_32 := Mapping;

   end Map_32;

   --------------------------------------------------
   -- Register the Endian Mapping for 16 bit Integers
   --------------------------------------------------
   procedure Map_16(Strm : in out Endian_Stream_Type'Class; Mapping : Endian_Mapping_2) is
   begin

      Strm.Map_16 := Mapping;

   end Map_16;

   --------------------------------------------------
   -- Read the Endian_I64 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Read_Endian_I64(Strm : access Root_Stream_Type'Class; Item : out Endian_I64) is
      I : Integer_64;
   begin

      Integer_64'Read(Strm,I);
      Item := Endian_I64(Map_Integer_64(I,Endian_Stream(Strm).Map_64));

   end Read_Endian_I64;

   --------------------------------------------------
   -- Read the Endian_U64 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Read_Endian_U64(Strm : access Root_Stream_Type'Class; Item : out Endian_U64) is
      I : Unsigned_64;
   begin

      Unsigned_64'Read(Strm,I);
      Item := Endian_U64(Map_Unsigned_64(I,Endian_Stream(Strm).Map_64));

   end Read_Endian_U64;

   --------------------------------------------------
   -- Read the Endian_I32 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Read_Endian_I32(Strm : access Root_Stream_Type'Class; Item : out Endian_I32) is
      I : Integer_32;
   begin

      Integer_32'Read(Strm,I);
      Item := Endian_I32(Map_Integer_32(I,Endian_Stream(Strm).Map_32));

   end Read_Endian_I32;

   --------------------------------------------------
   -- Read the Endian_U32 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Read_Endian_U32(Strm : access Root_Stream_Type'Class; Item : out Endian_U32) is
      I : Unsigned_32;
   begin

      Unsigned_32'Read(Strm,I);
      Item := Endian_U32(Map_Unsigned_32(I,Endian_Stream(Strm).Map_32));

   end Read_Endian_U32;

   --------------------------------------------------
   -- Read the Endian_I16 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Read_Endian_I16(Strm : access Root_Stream_Type'Class; Item : out Endian_I16) is
      I : Integer_16;
   begin

      Integer_16'Read(Strm,I);
      Item := Endian_I16(Map_Integer_16(I,Endian_Stream(Strm).Map_16));

   end Read_Endian_I16;

   --------------------------------------------------
   -- Read the Endian_U16 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Read_Endian_U16(Strm : access Root_Stream_Type'Class; Item : out Endian_U16) is
      I : Unsigned_16;
   begin

      Unsigned_16'Read(Strm,I);
      Item := Endian_U16(Map_Unsigned_16(I,Endian_Stream(Strm).Map_16));

   end Read_Endian_U16;

   --------------------------------------------------
   -- Read the unusual Endian_I24 Data Type with Endian Mapping :
   --------------------------------------------------
   procedure Read_Endian_I24(Strm : access Root_Stream_Type'Class; Item : out Endian_I24) is
      I : I24;
   begin
        
      I24'Read(Strm,I);
      Item := Endian_I24(Map_Integer_I24(I,Endian_Stream(Strm).Map_32));

   end Read_Endian_I24;

   --------------------------------------------------
   -- Read the unusual Endian_U24 Data Type with Endian Mapping :
   --------------------------------------------------
   procedure Read_Endian_U24(Strm : access Root_Stream_Type'Class; Item : out Endian_U24) is
      I : U24;
   begin
        
      U24'Read(Strm,I);
      Item := Endian_U24(Map_Integer_U24(I,Endian_Stream(Strm).Map_32));

   end Read_Endian_U24;

   --------------------------------------------------
   -- Write the Endian_I64 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Write_Endian_I64(Strm : access Root_Stream_Type'Class; Item : Endian_I64) is
      I : Integer_64 := Integer_64(Item);
   begin

      I := Map_Integer_64(I,Endian_Stream(Strm).Map_64);
      Integer_64'Write(Strm,I);

   end Write_Endian_I64;

   --------------------------------------------------
   -- Write the Endian_U64 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Write_Endian_U64(Strm : access Root_Stream_Type'Class; Item : Endian_U64) is
      U : Unsigned_64 := Unsigned_64(Item);
   begin

      U := Map_Unsigned_64(U,Endian_Stream(Strm).Map_64);
      Unsigned_64'Write(Strm,U);

   end Write_Endian_U64;

   --------------------------------------------------
   -- Write the Endian_I32 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Write_Endian_I32(Strm : access Root_Stream_Type'Class; Item : Endian_I32) is
      I : Integer_32 := Integer_32(Item);
   begin

      I := Map_Integer_32(I,Endian_Stream(Strm).Map_32);
      Integer_32'Write(Strm,I);

   end Write_Endian_I32;

   --------------------------------------------------
   -- Write the Endian_U32 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Write_Endian_U32(Strm : access Root_Stream_Type'Class; Item : Endian_U32) is
      U : Unsigned_32 := Unsigned_32(Item);
   begin

      U := Map_Unsigned_32(U,Endian_Stream(Strm).Map_32);
      Unsigned_32'Write(Strm,U);

   end Write_Endian_U32;

   --------------------------------------------------
   -- Write the unusual Endian_I24 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Write_Endian_I24(Strm : access Root_Stream_Type'Class; Item : Endian_I24) is
      I : I24;
   begin

      I := I24(Map_Integer_I24(I24(Item),Endian_Stream(Strm).Map_32));
      I24'Write(Strm,I);

   end Write_Endian_I24;

   --------------------------------------------------
   -- Write the unusual Endian_U24 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Write_Endian_U24(Strm : access Root_Stream_Type'Class; Item : Endian_U24) is
      U : U24;
   begin

      U := U24(Map_Integer_U24(U24(Item),Endian_Stream(Strm).Map_32));
      U24'Write(Strm,U);

   end Write_Endian_U24;

   --------------------------------------------------
   -- Write the Endian_I16 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Write_Endian_I16(Strm : access Root_Stream_Type'Class; Item : Endian_I16) is
      I : Integer_16 := Integer_16(Item);
   begin

      I := Map_Integer_16(I,Endian_Stream(Strm).Map_16);
      Integer_16'Write(Strm,I);

   end Write_Endian_I16;

   --------------------------------------------------
   -- Write the Endian_U16 Data Type with Endian Mapping
   --------------------------------------------------
   procedure Write_Endian_U16(Strm : access Root_Stream_Type'Class; Item : Endian_U16) is
      U : Unsigned_16 := Unsigned_16(Item);
   begin

      U := Map_Unsigned_16(U,Endian_Stream(Strm).Map_16);
      Unsigned_16'Write(Strm,U);

   end Write_Endian_U16;

   --------------------------------------------------
   -- Map this Stream like another :
   --------------------------------------------------
   procedure Map_Like(Strm : in out Endian_Stream_Type'Class; Like_Strm : Endian_Stream_Type'Class) is
   begin

      Strm.Map_16 := Like_Strm.Map_16;
      Strm.Map_32 := Like_Strm.Map_32;
      Strm.Map_64 := Like_Strm.Map_64;

   end Map_Like;

end WC.Streams.Endian;
