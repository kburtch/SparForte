-- $Id: wc-streams-endian.ads,v 1.2 2005/02/11 02:59:39 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License
--
--  --------------------------------------------------------------------------------
--
--  ESTABLISHING ENDIAN MAPPING:
--
--  To illustrate setting the Endian Mapping based upon a Magic Value (32
--  bits), the *.au sound file format example will be used.
--
--  -- .au files start with this magic 32 bit value
--  Strm:               Endian_Stream_Type;     -- Or derived
--  Au_Magic_Value:     Unsigned_32 constant := 16#2E736E64#;
--  Actual_Magic:       Unsigned_32;
--
--  ...
--  Unsigned_32'Read(Strm,Actual_Magic);        -- Read actual magic value
--  Map(Strm.All,Au_Magic_Value,Actual_Magic);  -- Establishes endian conversion
--    
--  --------------------------------------------------------------------------------
--
--  MANUAL MAPPING OVERRIDE:
--
--  To manually establish a reverse conversion for 32 and 16 bit values:
--
--  Map_32(Strm,Endian_Mapping_4'(4,3,2,1));
--  Map_16(Strm,Endian_Mapping_2'(2,1));
--
--  --------------------------------------------------------------------------------
--  
--  IMPORTANT!
--  
--  In order for endian conversion to occur on your data types, the values
--  must be derived from the types Endian_I32 etc. Use "type" or "subtype"
--  if you wish to use different type names.
--  
--  Examples:
--  
--      type Smart_Int is new Endian_I32;       -- Endian converted
--      subtype Smart_Short is Endian_I16;      -- Endian converted
--  
--      type Dumb_Int is new Integer_32;        -- No endian conversion
--      type Dumb_Short is Integer_16;          -- No endian conversion
--  
-- --------------------------------------------------------------------------------

with Interfaces, Ada.Streams;
use Interfaces, Ada.Streams;

package WC.Streams.Endian is

   Mapping_Error :      exception;              -- Raised if a Mapping fails (check your magic values)

   --------------------------------------------------
   -- Use these basic data types if you want automatic
   -- endian conversions to occur when reading and
   -- writing to the stream.
   --------------------------------------------------
   type Endian_I64 is new Integer_64;
   type Endian_I32 is new Integer_32;
   type Endian_I16 is new Integer_16;
   type Endian_I8  is new Integer_8;

   type Endian_U64 is new Unsigned_64;
   type Endian_U32 is new Unsigned_32;
   type Endian_U16 is new Unsigned_16;
   type Endian_U8  is new Unsigned_8;

   --------------------------------------------------
   -- Specialized Types:
   --------------------------------------------------
   type Endian_IArray is array(Endian_U16 range <>) of Integer_8;
   type Endian_UArray is array(Endian_U16 range <>) of Unsigned_8;

   type Endian_I24 is new Endian_IArray(1..3);
   type Endian_U24 is new Endian_UArray(1..3);

   --------------------------------------------------
   -- Endian Conversion Types :
   --------------------------------------------------
   type Endian_Index is new Integer range 0..8;
   type Endian_Mapping is array(Positive range <>) of Endian_Index;
   type Endian_Mapping_2 is new Endian_Mapping(1..2);
   type Endian_Mapping_4 is new Endian_Mapping(1..4);
   type Endian_Mapping_8 is new Endian_Mapping(1..8);

   --------------------------------------------------
   -- These convert Integer/Unsigned 32 and 16 bit values :
   --------------------------------------------------
   function Map_Integer_64(Val : Integer_64; Map : Endian_Mapping_8) return Integer_64;
   function Map_Integer_32(Val : Integer_32; Map : Endian_Mapping_4) return Integer_32;
   function Map_Integer_16(Val : Integer_16; Map : Endian_Mapping_2) return Integer_16;

   function Map_Unsigned_64(Val : Unsigned_64; Map : Endian_Mapping_8) return Unsigned_64;
   function Map_Unsigned_32(Val : Unsigned_32; Map : Endian_Mapping_4) return Unsigned_32;
   function Map_Unsigned_16(Val : Unsigned_16; Map : Endian_Mapping_2) return Unsigned_16;

   --------------------------------------------------
   -- The Endian Stream Abstract Type :
   --------------------------------------------------
   type Endian_Stream_Type is abstract new Root_Stream_Type with private;
   type Endian_Stream is access all Endian_Stream_Type'Class;

   --------------------------------------------------
   -- Establish the Endian mapping based upon Magic
   -- number, and actual number read.
   --------------------------------------------------
   procedure Map(Strm : in out Endian_Stream_Type; Magic : Unsigned_64; Actual : Unsigned_64);
   procedure Map(Strm : in out Endian_Stream_Type; Magic : Unsigned_32; Actual : Unsigned_32);
   procedure Map(Strm : in out Endian_Stream_Type; Magic : Unsigned_16; Actual : Unsigned_16);

   --------------------------------------------------
   -- Set the Mapping tables for automatic endian
   -- conversion. Note, that the default is to ignore
   -- endian conversions altogether.
   --------------------------------------------------
   procedure Map_64(Strm : in out Endian_Stream_Type'Class; Mapping : Endian_Mapping_8);
   procedure Map_32(Strm : in out Endian_Stream_Type'Class; Mapping : Endian_Mapping_4);
   procedure Map_16(Strm : in out Endian_Stream_Type'Class; Mapping : Endian_Mapping_2);

   procedure Map_Like(Strm : in out Endian_Stream_Type'Class; Like_Strm : Endian_Stream_Type'Class);

private

   --------------------------------------------------
   -- This Stream Type simply adds the Endian Mapping
   -- tables for conversions.
   --------------------------------------------------
   type Endian_Stream_Type is abstract new Root_Stream_Type with
   record
      Map_16 :           Endian_Mapping_2    := (1,2);               -- Default is no endian mapping
      Map_32 :           Endian_Mapping_4    := (1,2,3,4);           -- Default is no endian mapping
      Map_64 :           Endian_Mapping_8    := (1,2,3,4,5,6,7,8);   -- Default is no endian mapping
   end record;

   --------------------------------------------------
   -- Endian Smart Read Routines :
   --------------------------------------------------
   procedure Read_Endian_I64(Strm : access Root_Stream_Type'Class; Item : out Endian_I64);
   procedure Read_Endian_I32(Strm : access Root_Stream_Type'Class; Item : out Endian_I32);
   procedure Read_Endian_I24(Strm : access Root_Stream_Type'Class; Item : out Endian_I24);
   procedure Read_Endian_I16(Strm : access Root_Stream_Type'Class; Item : out Endian_I16);

   for Endian_I64'Read use Read_Endian_I64;
   for Endian_I32'Read use Read_Endian_I32;
   for Endian_I24'Read use Read_Endian_I24;
   for Endian_I16'Read use Read_Endian_I16;

   procedure Read_Endian_U64(Strm : access Root_Stream_Type'Class; Item : out Endian_U64);
   procedure Read_Endian_U32(Strm : access Root_Stream_Type'Class; Item : out Endian_U32);
   procedure Read_Endian_U24(Strm : access Root_Stream_Type'Class; Item : out Endian_U24);
   procedure Read_Endian_U16(Strm : access Root_Stream_Type'Class; Item : out Endian_U16);

   for Endian_U64'Read use Read_Endian_U64;
   for Endian_U32'Read use Read_Endian_U32;
   for Endian_U24'Read use Read_Endian_U24;
   -- for Endian_U16'Read use Read_Endian_U16; DEBUG

   --------------------------------------------------
   -- Endian Smart Write Routines :
   --------------------------------------------------
   procedure Write_Endian_I64(Strm : access Root_Stream_Type'Class; Item : Endian_I64);
   procedure Write_Endian_I32(Strm : access Root_Stream_Type'Class; Item : Endian_I32);
   procedure Write_Endian_I24(Strm : access Root_Stream_Type'Class; Item : Endian_I24);
   procedure Write_Endian_I16(Strm : access Root_Stream_Type'Class; Item : Endian_I16);

   for Endian_I64'Write use Write_Endian_I64;
   for Endian_I32'Write use Write_Endian_I32;
   for Endian_I24'Write use Write_Endian_I24;
   for Endian_I16'Write use Write_Endian_I16;

   procedure Write_Endian_U64(Strm : access Root_Stream_Type'Class; Item : Endian_U64);
   procedure Write_Endian_U32(Strm : access Root_Stream_Type'Class; Item : Endian_U32);
   procedure Write_Endian_U24(Strm : access Root_Stream_Type'Class; Item : Endian_U24);
   procedure Write_Endian_U16(Strm : access Root_Stream_Type'Class; Item : Endian_U16);

   for Endian_U64'Write use Write_Endian_U64;
   for Endian_U32'Write use Write_Endian_U32;
   for Endian_U24'Write use Write_Endian_U24;
   -- for Endian_U16'Write use Write_Endian_U16; DEBUG

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-endian.ads,v 1.2 2005/02/11 02:59:39 ken Exp $";

end WC.Streams.Endian;
