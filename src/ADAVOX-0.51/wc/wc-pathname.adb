-- $Id: wc-pathname.adb,v 1.2 2005/02/11 02:59:37 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Strings, Ada.Strings.Fixed;

package body WC.Pathname is

   CVSID : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-pathname.adb,v 1.2 2005/02/11 02:59:37 ken Exp $";

   --------------------------------------------------
   -- RETURN THE BASENAME OF A PATHNAME 
   --------------------------------------------------
   function Basename(Path : String; Semantic : Pathname_Semantics := Raise_Exceptions) return String
   is
      use Ada.Strings, Ada.Strings.Fixed;

      X : Natural;
   begin

      X := Index(Path,"/",Backward);
      if X = 0 then
         return Path;                     -- Entire pathname is the basename
      else
         if Semantic = Raise_Exceptions and X = Path'Last then
            raise No_Basename;            -- No basename is present
         end if;
         return Path(X+1..Path'Last);
      end if;
   end Basename;

   --------------------------------------------------
   -- RETURN THE DIRECTORY COMPONENT OF THE PATHNAME
   --------------------------------------------------
   function Directory(Path : String;  Semantic : Pathname_Semantics := Raise_Exceptions) return String
   is
      use Ada.Strings, Ada.Strings.Fixed;

      X : Natural;
   begin

      X := Index(Path,"/",Backward);
      if X = 0 then
         if Semantic = Raise_Exceptions then
            raise No_Directory;        -- No directory compoentn
         end if;
         return Path;
      else
         return Path(Path'first..X-1);
      end if;

   end Directory;

   --------------------------------------------------
   -- RETURN THE FILE'S SUFFIX, IF ANY
   --------------------------------------------------
   function Suffix(Path : String;  Semantic : Pathname_Semantics := Raise_Exceptions) return String
   is
      use Ada.Strings, Ada.Strings.Fixed;

      X : Natural;
   begin

      X := Index(Path,".",Backward);
      if X = 0 then
         if Semantic = Raise_Exceptions then
            raise No_Suffix;           -- No file suffix present
         end if;
         return "";
      else
         return Path(X+1..Path'Last);
      end if;
   end Suffix;

end WC.Pathname;
