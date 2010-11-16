-- $Id: wc-cints.ads,v 1.2 2005/02/11 02:59:37 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Interfaces; use Interfaces;

package WC.CInts is

   type Int is new Integer_32;
   type UInt is new Unsigned_32;
   type Short is new Integer_16;
   type UShort is new Unsigned_16;

   function "and"  (L, R : Int) return Int;
   function "or"   (L, R : Int) return Int;
   function "xor"  (L, R : Int) return Int;

   function "not"  (Unary : Int) return Int;

   function Shift  (I : Int; Count : Short)  return Int;
   function Shift_L(I : Int; Count : UShort) return Int;
   function Shift_R(I : Int; Count : UShort) return Int;

   function "and"  (L, R : Short) return Short;
   function "or"   (L, R : Short) return Short;
   function "xor"  (L, R : Short) return Short;

   function "not"  (Unary : Short) return Short;

   function Shift  (I : Short; Count : Short)  return Short;
   function Shift_L(I : Short; Count : UShort) return Short;
   function Shift_R(I : Short; Count : UShort) return Short;

private

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-cints.ads,v 1.2 2005/02/11 02:59:37 ken Exp $";

end WC.CInts;

