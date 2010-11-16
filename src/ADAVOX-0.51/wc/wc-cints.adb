-- $Id: wc-cints.adb,v 1.2 2005/02/11 02:59:36 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Unchecked_Conversion;

package body WC.CInts is

   CVSID : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-cints.adb,v 1.2 2005/02/11 02:59:36 ken Exp $";

   --------------------------------------------------
   -- Suport for 32-bit Conversions :
   --------------------------------------------------
   function To_Int(U : UInt) return Int
   is
      function UInt_2_Int is new Ada.Unchecked_Conversion(UInt,Int);
   begin

      return UInt_2_Int(U);

   end To_Int;

   function To_UInt(I : Int) return UInt
   is
      function Int_2_UInt is new Ada.Unchecked_Conversion(Int,UInt);
   begin

      return Int_2_UInt(I);

   end To_UInt;

   --------------------------------------------------
   -- Suport for 16-bit Conversions :
   --------------------------------------------------
   function To_Short(U : UShort) return Short
   is
      function UShort_2_Short is new Ada.Unchecked_Conversion(UShort,Short);
   begin

      return UShort_2_Short(U);

   end To_Short;

   function To_UShort(I : Short) return UShort
   is
      function Short_2_UShort is new Ada.Unchecked_Conversion(Short,UShort);
   begin

      return Short_2_UShort(I);

   end To_UShort;

   --------------------------------------------------
   -- Suport for 32-bit Int :
   --------------------------------------------------
   function "and"(L, R : Int) return Int is
   begin

      return To_Int( To_UInt(L) and To_UInt(R) );

   end "and";

   function "or"(L, R : Int) return Int is
   begin

      return To_Int( To_UInt(L) or To_UInt(R) );

   end "or";

   function "xor"(L, R : Int) return Int is
   begin

      return To_Int( To_UInt(L) xor To_UInt(R) );

   end "xor";
    
   function "not"(Unary : Int) return Int is
   begin

      return To_Int( not To_UInt(Unary) );

   end "not";

   function shift(I : Int; Count : Short) return Int is
   begin

      if Count = 0 then
         return I;               -- No shift at all
      elsif Count < 0 then
         return To_Int( Shift_Left( To_UInt(I), Natural(-Count) ) );
      end if;
      
      return To_Int( Shift_Right( To_UInt(I), Natural(Count) ) );

   end shift;

   function Shift_R(I : Int; Count : UShort) return Int is
   begin

      if Count = 0 then
         return I;               -- No shift at all
      end if;

      return To_Int( Shift_Right( To_UInt(I), Natural(Count) ) );

   end Shift_R;

   function Shift_L(I : Int; Count : UShort) return Int is
   begin

      if Count = 0 then
         return I;               -- No shift at all
      end if;

      return To_Int( Shift_Left( To_UInt(I), Natural(Count) ) );

   end Shift_L;

   --------------------------------------------------
   -- Suport for 16-bit Short :
   --------------------------------------------------
   function "and"(L, R : Short) return Short is
   begin

      return To_Short( To_UShort(L) and To_UShort(R) );

   end "and";

   function "or"(L, R : Short) return Short is
   begin

      return To_Short( To_UShort(L) or To_UShort(R) );

   end "or";

   function "xor"(L, R : Short) return Short is
   begin

      return To_Short( To_UShort(L) xor To_UShort(R) );

   end "xor";
    
   function "not"(Unary : Short) return Short is
   begin

      return To_Short( not To_UShort(Unary) );

   end "not";

   function shift(I : Short; Count : Short) return Short is
   begin

      if Count = 0 then
         return I;               -- No shift at all
      elsif Count < 0 then
         return To_Short( Shift_Left( To_UShort(I), Natural(-Count) ) );
      end if;
      
      return To_Short( Shift_Right( To_UShort(I), Natural(Count) ) );

   end shift;

   function Shift_R(I : Short; Count : UShort) return Short is
   begin

      if Count = 0 then
         return I;               -- No shift at all
      end if;

      return To_Short( Shift_Right( To_UShort(I), Natural(Count) ) );

   end Shift_R;

   function Shift_L(I : Short; Count : UShort) return Short is
   begin

      if Count = 0 then
         return I;               -- No shift at all
      end if;

      return To_Short( Shift_Left( To_UShort(I), Natural(Count) ) );

   end Shift_L;

end WC.CInts;
