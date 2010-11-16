-- $Id: wc-host-glue.ads,v 1.2 2005/02/11 02:59:37 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Interfaces;

use Interfaces;

package WC.Host.Glue is

   Arg_Error :      exception;             -- An argument was incorrectly specified
   Bad_Unit_Size :  exception;             -- Bad UNIT size (type)
   IO_Error :       exception;             -- An I/O error has occurred
   Seek_Error :     exception;             -- Seek failed

   function Fetch_Error return Errno_Type;
   procedure Store_Error(Err : Errno_Type);

   function Strerror(Errnum : Errno_Type) return String;

   generic
      type Fildes is range <>;            -- File descriptor: file_descriptor or integer
      type Unit is (<>);                  -- Usually character
      type Index is mod <>;               -- Subscript type of MODULAR
      type Unit_Array is array(Index range <>) of Unit;
      type Count is range <>;             -- Count to read, count to return
   procedure Read_M(Fd : Fildes; Buf : out Unit_Array; Unit_Count : Count; Units_Read : out Count);

   generic
      type Fildes is range <>;            -- File descriptor: file_descriptor or integer
      type Unit is (<>);                  -- Usually character
      type Index is range <>;             -- Subscript type of INTEGER
      type Unit_Array is array(Index range <>) of Unit;
      type Count is range <>;             -- Count to read, count to return
   procedure Read_I(Fd : Fildes; Buf : out Unit_Array; Unit_Count : Count; Units_Read : out Count);

   generic
      type T is private;
   procedure Generic_Read(Fd : in File_Descriptor; Item : out T);                                                                                                                       

   function Lseek(Fd : File_Descriptor; Offset : Off_T; Whence : Integer) return Off_T;

   function File_Position(Fd : File_Descriptor) return Off_T;
   function File_Size(Fd : File_Descriptor) return Off_T;
   function Open(Pathname : String; File_Access : Integer; Mode : Integer) return File_Descriptor;
   procedure Close(Fd : File_Descriptor);

   procedure Pipe(Fd1, Fd2 : out File_Descriptor);

private

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-host-glue.ads,v 1.2 2005/02/11 02:59:37 ken Exp $";

end WC.Host.Glue;
