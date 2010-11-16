-- $Id: apq-postgresql-decimal.ads,v 1.2 2005/02/11 02:59:43 ken Exp $
-- Copyright (c) 2002, Warren W. Gay VE3WWG
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

with System;
with Interfaces;
with Ada.Finalization;
with APQ.PostgreSQL.Client;

package APQ.PostgreSQL.Decimal is

   Decimal_NaN :           exception;
   Decimal_Format :        exception;
   Decimal_Overflow :      exception;
   Undefined_Result :      exception;
   Divide_By_Zero :        exception;

   type Decimal_Type is new Ada.Finalization.Controlled with private;

   type Precision_Type is range 0..32767;       -- Implementation may not actually live up to these limits
   type Scale_Type is range 0..32767;           -- Ditto.

   function Is_Nan(DT : Decimal_Type) return Boolean;

   procedure Convert(DT : in out Decimal_Type; S : String; Precision : Precision_Type := 0; Scale : Scale_Type := 2);
   function To_String(DT : Decimal_Type) return String;
   function Constrain(DT : Decimal_Type; Precision : Precision_Type; Scale : Scale_Type) return Decimal_Type;

   function Abs_Value(DT : Decimal_Type) return Decimal_Type;
   function Sign(DT : Decimal_Type) return Decimal_Type;
   function Ceil(DT : Decimal_Type) return Decimal_Type;
   function Floor(DT : Decimal_Type) return Decimal_Type;

   function Round(DT : Decimal_Type; Scale : Scale_Type) return Decimal_Type;
   function Trunc(DT : Decimal_Type; Scale : Scale_Type) return Decimal_Type;

   function Min_Value(Left, Right : Decimal_Type) return Decimal_Type;
   function Max_Value(Left, Right : Decimal_Type) return Decimal_Type;

   function Sqrt(X : Decimal_Type) return Decimal_Type;
   function Exp(X : Decimal_Type) return Decimal_Type;
   function Ln(X : Decimal_Type) return Decimal_Type;
   function Log10(X : Decimal_Type) return Decimal_Type;

   function Log(X, Base : Decimal_Type) return Decimal_Type;
   function Power(X, Y : Decimal_Type) return Decimal_Type;

   function "+"(Left, Right : Decimal_Type) return Decimal_Type;
   function "-"(Left, Right : Decimal_Type) return Decimal_Type;
   function "-"(DT : Decimal_Type) return Decimal_Type;
   function "*"(Left, Right : Decimal_Type) return Decimal_Type;
   function "/"(Left, Right : Decimal_Type) return Decimal_Type;

   function "="(Left, Right : Decimal_Type) return Boolean;
   function ">"(Left, Right : Decimal_Type) return Boolean;
   function ">="(Left, Right : Decimal_Type) return Boolean;
   function "<"(Left, Right : Decimal_Type) return Boolean;
   function "<="(Left, Right : Decimal_Type) return Boolean;

   function NaN return Decimal_Type;
   function Zero return Decimal_Type;
   function One return Decimal_Type;
   function Two return Decimal_Type;
   function Ten return Decimal_Type;

   procedure Append(Query : in out PostgreSQL.Client.Query_Type; DT : Decimal_Type'Class; After : String := "");
   function Value(Query : PostgreSQL.Client.Query_Type; CX : Column_Index_Type) return Decimal_Type;

private

   type Rscale_Type is range -2 ** 31 .. 2 ** 31 - 1;
   type Numeric_Type is new System.Address;

   Null_Numeric : constant Numeric_Type := Numeric_Type(System.Null_Address);

   type Decimal_Type is new Ada.Finalization.Controlled with
      record
         Global_Rscale :   Rscale_Type;
         Precision :       Precision_Type := 0;
         Scale :           Scale_Type := 0;
         Numeric :         Numeric_Type := Null_Numeric;
      end record;

   procedure Initialize(DT : in out Decimal_Type);
   procedure Finalize(DT : in out Decimal_Type);
   procedure Adjust(DT : in out Decimal_Type);

end APQ.PostgreSQL.Decimal;

-- End $Source: /home/cvsroot/bush/src/apq-2.1/apq-postgresql-decimal.ads,v $
