-- $Id: wc-pathname.ads,v 1.2 2005/02/11 02:59:37 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

package WC.Pathname is

   type Pathname_Semantics is ( Raise_Exceptions, No_Exceptions );

   --------------------------------------------------
   -- Pathname functions 
   --------------------------------------------------
   -- By default these functions raise
   -- exceptions when the entity being sought is
   -- not present. For example, exception
   -- No_Basename is raised by the Basename()
   -- function if the basename of the path is
   -- missing.
   -- 
   -- If the No_Exceptions is chosen instead,
   -- the function returns a null string when
   -- the requested component is not present.
   -- Depending upon the calling requirements,
   -- this may often be more convenient.
   --------------------------------------------------
   -- NOTE: For the Basename(), if no slash is in
   -- the pathname, the entire pathname is considered
   -- to be the basename!
   --------------------------------------------------
   -- If no slash is present in the pathname given to
   -- Directory(), then no directory is assumed to be
   -- present!
   --------------------------------------------------
   function Basename(Path : String; Semantic : Pathname_Semantics := Raise_Exceptions) return String;
   function Directory(Path : String; Semantic : Pathname_Semantics := Raise_Exceptions) return String;
   function Suffix(Path : String; Semantic : Pathname_Semantics := Raise_Exceptions) return String;

   --------------------------------------------------
   -- Exceptions
   --------------------------------------------------

   No_Basename :           exception;
   No_Directory :          exception;
   No_Suffix :             exception;

private

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-pathname.ads,v 1.2 2005/02/11 02:59:37 ken Exp $";

end WC.Pathname;
