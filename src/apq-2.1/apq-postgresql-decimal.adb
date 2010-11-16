-- $Id: apq-postgresql-decimal.adb,v 1.2 2005/02/11 02:59:43 ken Exp $
-- Copyright(c) 2002, Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)
-- or
-- GNU Public License 2 (GPL2)
-- 
--     This program is free software; you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation; either version 2 of the License, or
--     (at your option) any later version.
-- 
--     This program is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
-- 
--     You should have received a copy of the GNU General Public License
--     along with this program; if not, write to the Free Software
--     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

with Interfaces.C, Interfaces.C.Strings;
use Interfaces.C, Interfaces.C.Strings;

package body APQ.PostgreSQL.Decimal is

   type numeric_ex is (
      C_No_Error,
      C_Numeric_Format,
      C_Numeric_Overflow,
      C_Undefined_Result,
      C_Divide_By_Zero
   );
   for numeric_ex use (
      0,    -- No_Error
      1,    -- Numeric_Format
      2,    -- Numeric_Overflow
      3,    -- Undefined_Result
      4     -- Divide_By_Zero
   );
   for numeric_ex'Size use 32;

   function numeric_global_rscale return Rscale_Type;
   pragma Import(C,numeric_global_rscale,"numeric_global_rscale");

   procedure numeric_free(num : Numeric_Type);
   pragma Import(C,numeric_free,"numeric_free");

   procedure free_cstring(ptr : chars_ptr);
   pragma Import(C,free_cstring,"numeric_free");

   function numeric_isnan(num : Numeric_Type) return int;
   pragma Import(C,numeric_isnan,"numeric_isnan");

   function numeric_in(str : System.Address; precision, scale : int; ex : System.Address) return Numeric_Type;
   pragma Import(C,numeric_in,"numeric_in");

   function numeric_out(num : Numeric_Type) return chars_ptr;
   pragma Import(C,numeric_out,"numeric_out");

   function numeric(num : Numeric_Type; precision, scale : int; ex : System.Address) return Numeric_Type;
   pragma Import(C,numeric,"numeric");

   function numeric_uplus(num : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_uplus,"numeric_uplus");

   function numeric_add(num1, num2 : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_add,"numeric_add");

   function numeric_sub(num1, num2 : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_sub,"numeric_sub");

   function numeric_mul(num1, num2 : Numeric_Type; global_rscale : System.Address) return Numeric_Type;
   pragma Import(C,numeric_mul,"numeric_mul");

   function numeric_div(num1, num2 : Numeric_Type; global_rscale, ex : System.Address) return Numeric_Type;
   pragma Import(C,numeric_div,"numeric_div");

   function numeric_abs(num : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_abs,"numeric_abs");
   
   function numeric_uminus(num : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_uminus,"numeric_uminus");
   
   function numeric_sign(num : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_sign,"numeric_sign");
   
   function numeric_round(num : Numeric_Type; Scale : int) return Numeric_Type;
   pragma Import(C,numeric_round,"numeric_round");
   
   function numeric_trunc(num : Numeric_Type; Scale : int) return Numeric_Type;
   pragma Import(C,numeric_trunc,"numeric_trunc");
   
   function numeric_ceil(num : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_ceil,"numeric_ceil");
   
   function numeric_floor(num : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_floor,"numeric_floor");
   
   function numeric_eq(num1, num2 : Numeric_Type) return int;
   pragma Import(C,numeric_eq,"numeric_eq");

   function numeric_ne(num1, num2 : Numeric_Type) return int;
   pragma Import(C,numeric_ne,"numeric_ne");

   function numeric_gt(num1, num2 : Numeric_Type) return int;
   pragma Import(C,numeric_gt,"numeric_gt");

   function numeric_ge(num1, num2 : Numeric_Type) return int;
   pragma Import(C,numeric_ge,"numeric_ge");

   function numeric_lt(num1, num2 : Numeric_Type) return int;
   pragma Import(C,numeric_lt,"numeric_lt");

   function numeric_le(num1, num2 : Numeric_Type) return int;
   pragma Import(C,numeric_le,"numeric_le");

   function numeric_smaller(num1, num2 : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_smaller,"numeric_smaller");
   
   function numeric_larger(num1, num2 : Numeric_Type) return Numeric_Type;
   pragma Import(C,numeric_larger,"numeric_larger");
   
   function numeric_sqrt(num : Numeric_Type; global_rscale, ex : System.Address) return Numeric_Type;
   pragma Import(C,numeric_sqrt,"numeric_sqrt");
   
   function numeric_exp(num : Numeric_Type; global_rscale, ex : System.Address) return Numeric_Type;
   pragma Import(C,numeric_exp,"numeric_exp");
   
   function numeric_ln(num : Numeric_Type; global_rscale, ex : System.Address) return Numeric_Type;
   pragma Import(C,numeric_ln,"numeric_ln");
   
   function numeric_log(num, base : Numeric_Type; global_rscale, ex : System.Address) return Numeric_Type;
   pragma Import(C,numeric_log,"numeric_log");
   
   function numeric_power(x, y : Numeric_Type; global_rscale, ex : System.Address) return Numeric_Type;
   pragma Import(C,numeric_power,"numeric_power");
   
   procedure Free(Num : in out Numeric_Type) is
   begin
      if Num /= Null_Numeric then
         numeric_free(Num);
         Num := Null_Numeric;
      end if;
   end Free;

   procedure Raise_Exception(Ex : numeric_ex) is
   begin
      case Ex is
         when C_No_Error =>         return;
         when C_Numeric_Format =>   raise Decimal_Format;
         when C_Numeric_Overflow => raise Decimal_Overflow;
         when C_Undefined_Result => raise Undefined_Result;
         when C_Divide_By_Zero =>   raise Divide_By_Zero;
      end case;
   end Raise_Exception;

   procedure Convert(DT : in out Decimal_Type; S : String; Precision : Precision_Type := 0; Scale : Scale_Type := 2) is
      C_String :  char_array := To_C(S);
      P :         int := int(Precision);
      Sc :        int := int(Scale);
      Ex :        numeric_ex;
   begin
      if DT.Numeric /= Null_Numeric then
         Free(DT.Numeric);
      end if;

      DT.Numeric := numeric_in(C_String'Address,P,Sc,Ex'Address);

      if Ex /= C_No_Error then
         Raise_Exception(Ex);
      end if;
   end Convert;

   function Is_Nan(DT : Decimal_Type) return Boolean is
   begin
      return DT.Numeric = Null_Numeric or else numeric_isnan(DT.Numeric) /= 0;
   end Is_Nan;

   function To_String(DT : Decimal_Type) return String is
   begin
      if Is_Nan(DT) then
         raise Decimal_NaN;
      end if;
      declare
         C_Ptr : chars_ptr := numeric_out(DT.Numeric);
      begin
         if C_Ptr = Null_Ptr then
            return "NULL";
         else
            declare
               S : String := To_Ada(Value(C_Ptr));
            begin
               free_cstring(C_Ptr);
               return S;
            end;
         end if;
      end;
   end To_String;

   function Constrain(DT : Decimal_Type; Precision : Precision_Type; Scale : Scale_Type) return Decimal_Type is
      R :   Decimal_Type;
      P :   int := int(Precision);
      S :   int := int(Scale);
      E :   numeric_ex;
   begin
      if Is_Nan(DT) then
         raise Decimal_NaN;
      else
         if Precision /= 0 then
            R.Numeric := numeric(DT.Numeric,P,S,E'Address);  -- Set precision and scale
            if E /= C_No_Error then
               Raise_Exception(E);
            end if;
         else
            R.Numeric := numeric_uplus(DT.Numeric);          -- Just copy
         end if;
      end if;
      return R;
   end Constrain;

   procedure Initialize(DT : in out Decimal_Type) is
   begin
      DT.Global_Rscale := numeric_global_rscale;
   end Initialize;

   procedure Finalize(DT : in out Decimal_Type) is
   begin
      Free(DT.Numeric);
   end Finalize;

   procedure Adjust(DT : in out Decimal_Type) is
      Num : Numeric_Type := DT.Numeric;
   begin
      if DT.Numeric = Null_Numeric or else Is_NaN(DT) then
         return;     -- Nothing further to adjust
      end if;
      DT.Numeric := numeric_uplus(DT.Numeric);
   end Adjust;

   function "+"(Left, Right : Decimal_Type) return Decimal_Type is
      R : Decimal_Type;
   begin
      R.Numeric := numeric_add(Left.Numeric,Right.Numeric);
      if Is_Nan(R) then
         raise Decimal_NaN;
      end if;
      return R;
   end "+";

   function "-"(Left, Right : Decimal_Type) return Decimal_Type is
      R : Decimal_Type;
   begin
      R.Numeric := numeric_sub(Left.Numeric,Right.Numeric);
      if Is_Nan(R) then
         raise Decimal_NaN;
      end if;
      return R;
   end "-";

   function "*"(Left, Right : Decimal_Type) return Decimal_Type is
      R : Decimal_Type;
   begin
      R.Numeric := numeric_mul(Left.Numeric,Right.Numeric,R.Global_Rscale'Address);
      if Is_Nan(R) then
         raise Decimal_NaN;
      end if;
      return R;
   end "*";

   function "/"(Left, Right : Decimal_Type) return Decimal_Type is
      R : Decimal_Type;
      E : numeric_ex;
   begin
      R.Numeric := numeric_div(Left.Numeric,Right.Numeric,R.Global_Rscale'Address,E'Address);
      if E /= C_No_Error then
         Raise_Exception(E);
      end if;
      if Is_Nan(R) then
         raise Decimal_NaN;
      end if;
      return R;
   end "/";

   Const_Nan : Decimal_Type;
   Const_Zero, Const_One, Const_Two, Const_Ten : Decimal_Type;     -- Constants after elaboration

   function NaN return Decimal_Type is
   begin
      return Const_NaN;
   end Nan;
   
   function Zero return Decimal_Type is
   begin
      return Const_Zero;
   end Zero;

   function One return Decimal_Type is
   begin
      return Const_One;
   end One;

   function Two return Decimal_Type is
   begin
      return Const_Two;
   end Two;

   function Ten return Decimal_Type is
   begin
      return Const_Ten;
   end Ten;

   function Abs_Value(DT : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(DT) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
      begin
         R.Numeric := numeric_abs(DT.Numeric);
         return R;
      end;
   end Abs_Value;

   function Sign(DT : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(DT) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
      begin
         R.Numeric := numeric_sign(DT.Numeric);
         return R;
      end;
   end Sign;

   function Round(DT : Decimal_Type; Scale : Scale_Type) return Decimal_Type is
   begin
      if Is_NaN(DT) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
      begin
         R.Numeric := numeric_round(DT.Numeric,int(Scale));
         return R;
      end;
   end Round;

   function Trunc(DT : Decimal_Type; Scale : Scale_Type) return Decimal_Type is
   begin
      if Is_NaN(DT) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
      begin
         R.Numeric := numeric_trunc(DT.Numeric,int(Scale));
         return R;
      end;
   end Trunc;

   function Ceil(DT : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(DT) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
      begin
         R.Numeric := numeric_ceil(DT.Numeric);
         return R;
      end;
   end Ceil;

   function Floor(DT : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(DT) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
      begin
         R.Numeric := numeric_floor(DT.Numeric);
         return R;
      end;
   end Floor;

   function "-"(DT : Decimal_Type) return Decimal_Type is
      R : Decimal_Type;
   begin
      if Is_NaN(DT) then
         raise Decimal_NaN;
      end if;
      R.Numeric := numeric_uminus(DT.Numeric);
      return R;
   end "-";

   function "="(Left, Right : Decimal_Type) return Boolean is
   begin
      if Is_NaN(Left) or else Is_Nan(Right) then
         raise Decimal_NaN;
      end if;
      return numeric_eq(Left.Numeric,Right.Numeric) /= 0;
   end "=";

   function ">"(Left, Right : Decimal_Type) return Boolean is
   begin
      if Is_NaN(Left) or else Is_Nan(Right) then
         raise Decimal_NaN;
      end if;
      return numeric_gt(Left.Numeric,Right.Numeric) /= 0;
   end ">";

   function ">="(Left, Right : Decimal_Type) return Boolean is
   begin
      if Is_NaN(Left) or else Is_Nan(Right) then
         raise Decimal_NaN;
      end if;
      return numeric_ge(Left.Numeric,Right.Numeric) /= 0;
   end ">=";
   
   function "<"(Left, Right : Decimal_Type) return Boolean is
   begin
      if Is_NaN(Left) or else Is_Nan(Right) then
         raise Decimal_NaN;
      end if;
      return numeric_lt(Left.Numeric,Right.Numeric) /= 0;
   end "<";
   
   function "<="(Left, Right : Decimal_Type) return Boolean is
   begin
      if Is_NaN(Left) or else Is_Nan(Right) then
         raise Decimal_NaN;
      end if;
      return numeric_le(Left.Numeric,Right.Numeric) /= 0;
   end "<=";

   function Min_Value(Left, Right : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(Left) or else Is_Nan(Right) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
      begin
         R.Numeric := numeric_smaller(Left.Numeric,Right.Numeric);
         return R;
      end;
   end Min_Value;

   function Max_Value(Left, Right : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(Left) or else Is_Nan(Right) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
      begin
         R.Numeric := numeric_larger(Left.Numeric,Right.Numeric);
         return R;
      end;
   end Max_Value;

   function Sqrt(X : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(X)  then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
         E : numeric_ex;
      begin
         R.Numeric := numeric_sqrt(X.Numeric,R.Global_Rscale'Address,E'Address);
         if E /= C_No_Error then
            Raise_Exception(E);
         end if;
         return R;
      end;
   end Sqrt;
   
   function Exp(X : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(X)  then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
         E : numeric_ex;
      begin
         R.Numeric := numeric_exp(X.Numeric,R.Global_Rscale'Address,E'Address);
         if E /= C_No_Error then
            Raise_Exception(E);
         end if;
         return R;
      end;
   end Exp;
   
   function Ln(X : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(X)  then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
         E : numeric_ex;
      begin
         R.Numeric := numeric_ln(X.Numeric,R.Global_Rscale'Address,E'Address);
         if E /= C_No_Error then
            Raise_Exception(E);
         end if;
         return R;
      end;
   end Ln;
   
   function Log(X, Base : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(X) or else Is_Nan(Base) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
         E : numeric_ex;
      begin
         R.Numeric := numeric_log(Base.Numeric,X.Numeric,R.Global_Rscale'Address,E'Address);
         if E /= C_No_Error then
            Raise_Exception(E);
         end if;
         return R;
      end;
   end Log;
   
   function Log10(X : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(X) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
         E : numeric_ex;
      begin
         R.Numeric := numeric_log(Ten.Numeric,X.Numeric,R.Global_Rscale'Address,E'Address);
         if E /= C_No_Error then
            Raise_Exception(E);
         end if;
         return R;
      end;
   end Log10;

   function Power(X, Y : Decimal_Type) return Decimal_Type is
   begin
      if Is_NaN(X) or else Is_Nan(Y) then
         raise Decimal_NaN;
      end if;
      declare
         R : Decimal_Type;
         E : numeric_ex;
      begin
         R.Numeric := numeric_power(X.Numeric,Y.Numeric,R.Global_Rscale'Address,E'Address);
         if E /= C_No_Error then
            Raise_Exception(E);
         end if;
         return R;
      end;
   end Power;

   procedure Append(Query : in out PostgreSQL.Client.Query_Type; DT : Decimal_Type'Class; After : String := "") is
      use PostgreSQL.Client;
   begin
      Append(Query,To_String(DT),After);
   end Append;

   function Value(Query : PostgreSQL.Client.Query_Type; CX : Column_Index_Type) return Decimal_Type is
      use PostgreSQL.Client;
   begin
      if Is_Null(Query,CX) then
         return NaN;
      else
         declare
            S : String := Value(Query,CX);
            R : Decimal_Type;
         begin
            Convert(R,S);
            return R;
         end;
      end if;
   end Value;

begin

   Convert(Const_Zero,"0");
   Convert(Const_One,"1");
   Convert(Const_Two,"2");
   Convert(Const_Ten,"10");

end APQ.PostgreSQL.Decimal;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/apq-postgresql-decimal.adb,v $
