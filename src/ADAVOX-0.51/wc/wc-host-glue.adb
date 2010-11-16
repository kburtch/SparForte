-- $Id: wc-host-glue.adb,v 1.2 2005/02/11 02:59:37 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Text_Io;
use Ada.Text_Io;

with System, Unchecked_Conversion, Interfaces, Ada.Characters.Latin_1, Ada.Io_Exceptions;

with Interfaces.C;
with Interfaces.C.Strings;

use System, Interfaces, Ada.Characters.Latin_1, Ada.Io_Exceptions;

package body WC.Host.Glue is

   CVSID :      constant String := "$Id: wc-host-glue.adb,v 1.2 2005/02/11 02:59:37 ken Exp $";

   type Char_Ptr is access all Character;

   --------------------------------------------------
   -- C read(2) function :
   --------------------------------------------------
   function C_Read (Fd : File_Descriptor; Buf : System.Address; Cnt : Integer) return Integer;
   pragma Import(C,C_Read,"read");

   --------------------------------------------------
   -- C pipe(2) function :
   --------------------------------------------------
   function C_Pipe(Fds : System.Address) return Integer;
   pragma Import(C,C_Pipe,"pipe");

   --------------------------------------------------
   -- Ada binding to strerror(3) :
   --------------------------------------------------
   function Strerror(Errnum : Errno_Type) return String
   is
      use Interfaces.C, Interfaces.C.Strings;
      function C_strerror(Errno : Errno_Type) return Chars_Ptr;
      pragma import(C,C_strerror,"strerror");
      V :   Char_Array := Value(C_Strerror(errnum));
   begin

      return To_Ada(V);

   end strerror;

   --------------------------------------------------
   -- Ada function to fetch the C errno value :
   --------------------------------------------------
   function Fetch_Error return Errno_Type
   is
      function C_Fetch_Error return Errno_Type;
      pragma import(C,C_Fetch_Error,"fetch_error");
   begin

      return C_Fetch_Error;

   end Fetch_Error;

   --------------------------------------------------
   -- Ada procedure to store a new errno value :
   --------------------------------------------------
   procedure store_error(Err : Errno_Type)
   is
      procedure C_store_error(err : Errno_Type);
      pragma import(C,C_store_error,"store_error");
   begin

      C_store_error(err);

   end store_error;

   procedure read_m(fd : FILDES; buf : out UNIT_ARRAY; unit_count : COUNT; units_read : out COUNT)
   is
      Bufsiz :     Natural := Natural(Buf'length);     -- UNIT length of buffer

      Byte_Cnt :   Natural;                            -- Bytes to read
      Unit_Cnt :   Natural := 0;                       -- # of units read

      Offset :     Index := Buf'first;
      N :          Integer;

      function C_Read (Fd : Integer; Buf : System.Address; Cnt : Integer) return Integer;
      pragma import(C,C_read,"read");
   begin

      if Unit'size /= 8 then
         raise Bad_Unit_Size;            -- Must be 8-bit units
      end if;

      if Natural(Unit_Count) > Bufsiz then
         Byte_Cnt := Bufsiz;             -- Limit request to buffer size
      else
         Byte_Cnt := Natural(Unit_Count);
      end if;
            
      loop
         exit when Byte_Cnt = 0;
         N := C_Read(File_Descriptor(Fd),Buf(Offset)'address,Byte_Cnt);
         if N > 0 then
            Byte_Cnt := Byte_Cnt - Natural(N);
            Offset := Offset + Index(N);
            Unit_Cnt := Unit_Cnt + Natural(N);
         end if;
      
         exit when N = 0;
         exit when N = -1 and Fetch_Error /= EINTR;
      end loop;

      if N < 0 then
         raise IO_Error;                 -- NB: The count so far cannot be returned here
      end if;

      Units_Read := Count(Unit_Cnt);      -- This may NOT be the full count
   end Read_M;

   procedure Read_I(Fd : Fildes; Buf : out Unit_Array; Unit_Count : Count; Units_Read : out Count)
   is
      Bufsiz :     Natural := Natural(Buf'length);     -- UNIT length of buffer

      Byte_Cnt :   Natural;                            -- Bytes to read
      Unit_Cnt :   Natural := 0;                       -- # of units read

      Offset :     Index := Buf'first;
      N :          Integer;

   begin

      if Unit'size /= 8 then
         raise Bad_Unit_Size;            -- Must be 8-bit units
      end if;

      if Natural(Unit_Count) > Bufsiz then
         Byte_Cnt := Bufsiz;             -- Limit request to buffer size
      else
         Byte_Cnt := Natural(Unit_Count);
      end if;
            
      loop
         exit when Byte_Cnt = 0;
         N := C_Read(File_Descriptor(Fd),Buf(Offset)'address,Byte_Cnt);
         if N > 0 then
            Byte_Cnt := Byte_Cnt - Natural(N);
            Offset := Offset + Index(N);
            Unit_Cnt := Unit_Cnt + Natural(N);
         end if;
      
         exit when N = 0;
         exit when N = -1 and Fetch_Error /= EINTR;
      end loop;

      if N < 0 then
         raise IO_Error;                 -- NB: The count so far cannot be returned here
      end if;

      Units_Read := Count(Unit_Cnt);      -- This may NOT be the full count

   end Read_I;

   --------------------------------------------------
   -- Ada binding for lseek(2) operation :
   --------------------------------------------------
   function Lseek(Fd : File_Descriptor; Offset : Off_T; Whence : Integer) return Off_T
   is
      function C_Lseek(Fd : Integer; Offset : Off_T; Whence : Integer) return Off_T;
      pragma import(C,C_Lseek,"lseek");
   begin

      return C_Lseek(Integer(Fd),Offset,Whence);

   end lseek;
                                                          
   --------------------------------------------------
   -- Ada binding to open(2) :
   --------------------------------------------------
   function Open(Pathname : String; File_Access : Integer; Mode : Integer) return File_Descriptor
   is
      function C_Open(Path : System.Address; File_Access : Integer; Mode : Integer) return File_Descriptor;
      pragma import(C,C_Open,"open");
      C_Path :     String(1..Pathname'length+1);
   begin

      C_Path(1..Pathname'length) := Pathname;
      C_Path(Pathname'length+1) := Nul;
      return C_Open(C_Path(1)'address,File_Access,Mode);

   end Open;

   --------------------------------------------------
   -- Ada binding to close(2) :
   --------------------------------------------------
   procedure Close(Fd : File_Descriptor)
   is
      function C_Close(Fd : File_Descriptor) return Integer;
      pragma import(C,C_Close,"close");
      Status : Integer;
   begin

      Status := C_Close(Fd);
      if Status /= 0 then
         raise IO_Error;
      end if;

   end Close;

   --------------------------------------------------
   -- Return the File's Size in bytes :
   --------------------------------------------------
   function File_Size(Fd : File_Descriptor) return Off_T
   is
      Prevoff :    Off_T;
      Endoff :     Off_T;
   begin

      Prevoff := Lseek(Fd,0,Seek_Cur);
      if Prevoff < 0 then
         raise Seek_Error;
      end if;

      Endoff := Lseek(Fd,0,Seek_End);
      if endoff < 0 then
         raise Seek_Error;
      end if;

      Prevoff := Lseek(Fd,Prevoff,Seek_Set);
      if Prevoff < 0 then
         raise Seek_Error;
      end if;

      return Endoff;

   end File_Size;
                                                                                                                               
   --------------------------------------------------
   -- Return the file's Offset in bytes :
   --------------------------------------------------
   function File_Position(Fd : File_Descriptor) return Off_T
   is
      Fpos :       Off_T;
   begin

      Fpos := Lseek(Fd,0,Seek_Cur);

      if Fpos < 0 then
         raise Seek_Error;
      end if;
      return Fpos;

   end File_Position;

   --------------------------------------------------
   -- Generic read(2) function :
   --------------------------------------------------
   procedure Generic_Read(Fd : in File_Descriptor; Item : out T)
   is
      Result :     Integer;
   begin

      if Item'size rem Character'size /= 0 then
         raise IO_Error;
      end if;

      Result := C_Read(Fd,Item'address,Integer(Item'size / Character'size));

      if Result < Item'size / Character'size then
         raise Ada.IO_Exceptions.End_Error;
      end if;

   end Generic_Read;

   --------------------------------------------------
   -- Create an unamed Pipe :
   --------------------------------------------------
   procedure Pipe(Fd1, Fd2 : out File_Descriptor)
   is
      Pair :   array(1..2) of File_Descriptor := ( -1, -1 );
      Z :      Integer;
   begin

      Z := C_Pipe(Pair'address);
      if Z = -1 then
         raise IO_Error;
      end if;

      Fd1 := Pair(1);
      Fd2 := Pair(2);

   end Pipe;

end WC.Host.Glue;
