-- $Id: gnat_info.adb,v 1.2 2005/02/11 02:59:44 ken Exp $
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
--
-- Usage: gnat_info [-S|-A] [-M] [options]
--    -S    Output in shell assignment mode
--    -A    Output all values as shell assignments
--    -M    Output variables in Makefile format (no quotes)
--
--    -r    Get GNAT root directory
--    -v    Get GNAT version
--    -b    Get GNAT bin directory
--    -B    Get GNAT Bindings directory
--    -w    Get GNAT Windows binding directory
--    -i    Get GNAT adainclude directory
--    -l    Get GNAT adalib directory
--
--    -a    Get APQ version, if installed

with Ada.Text_IO;
with Ada.Characters.Latin_1;
with GNAT.Command_Line;
with Win32_GNAT_Info;

use Ada.Text_IO;
use Ada.Characters.Latin_1;
use GNAT.Command_Line;
use Win32_GNAT_Info;

procedure GNAT_Info is
   S : Boolean := False;
   M : Boolean := False;

   procedure Emit(Varname, Val : String) is
   begin
      if S then
         Put(Varname & "=");
         if not M then
            Put("""");
         end if;
      end if;
      Put(Val);
      if S then
         if not M then
            Put_Line("""");
         end if;
      else
         New_Line;
      end if;
   end Emit;

begin

   loop
      case GetOpt("S A M r v b B w i l a") is
         when NUL =>
            exit;
         when 'S' =>
            S := True;
         When 'M' =>
            M := True;
         when 'r' =>
            Emit("GNAT_ROOT",Root);
         when 'v' =>
            Emit("GNAT_VERSION",Version);
         when 'b' =>
            Emit("GNAT_BIN",Bin);
         when 'B' =>
            Emit("GNAT_BINDINGS",Bindings_Directory);
         when 'w' =>
            Emit("GNAT_WIN32_BINDINGS",Win32Ada_Bindings_Directory);
         when 'i' =>
            Emit("GNAT_ADAINCLUDE",ADAINCLUDE);
         when 'l' =>
            Emit("GNAT_ADALIB",ADALIB);
         when 'A' =>
            S := True;
            Emit("GNAT_VERSION",Version);
            Emit("GNAT_ROOT",Root);
            Emit("GNAT_BIN",Bin);
            Emit("GNAT_BINDINGS",Bindings_Directory);
            Emit("GNAT_WIN32_BINDINGS",Win32Ada_Bindings_Directory);
            Emit("GNAT_ADAINCLUDE",ADAINCLUDE);
            Emit("GNAT_ADALIB",ADALIB);
         when 'a' =>
            begin
               Emit("APQ_VERSION",APQ_Version);
            exception
               when others =>
                  Emit("APQ_VERSION","NOT INSTALLED");
            end;
         when others =>
            raise Program_Error;
      end case;
   end loop;

end GNAT_Info;
