package body Ustrings is

  Input_Line_Buffer_Length : constant := 1024;
    -- If an input line is longer, Get_Line will recurse to read in the line.


  procedure Swap(Left, Right : in out Unbounded_String) is
    -- Implement Swap.  This is the portable but slow approach.
    Temporary : Unbounded_String;
  begin
    Temporary := Left;
    Left := Right;
    Right := Temporary;
  end Swap;

  -- Implement Unbounded_String I/O by calling Text_IO String routines.


  -- Get_Line gets a line of text, limited only by the maximum number of
  -- characters in an Unbounded_String.  It reads characters into a buffer
  -- and if that isn't enough, recurses to read the rest.

  procedure Get_Line (File : in File_Type; Item : out Unbounded_String) is

    function More_Input return Unbounded_String is
       Input : String (1 .. Input_Line_Buffer_Length);
       Last  : Natural;
    begin
       Get_Line (File, Input, Last);
       if Last < Input'Last then
          return   To_Unbounded_String (Input(1..Last));
       else
          return   To_Unbounded_String (Input(1..Last)) & More_Input;
       end if;
    end More_Input;

  begin
      Item := More_Input;
  end Get_Line;


  procedure Get_Line(Item : out Unbounded_String) is
  begin
    Get_Line(Current_Input, Item);
  end Get_Line;

  procedure Put(File : in File_Type; Item : in Unbounded_String) is
  begin
    Put(File, To_String(Item));
  end Put;

  procedure Put(Item : in Unbounded_String) is
  begin
    Put(Current_Output, To_String(Item));
  end Put;

  procedure Put_Line(File : in File_Type; Item : in Unbounded_String) is
  begin
    Put(File, Item);
    New_Line(File);
  end Put_Line;

  procedure Put_Line(Item : in Unbounded_String) is
  begin
    Put(Current_Output, Item);
    New_Line;
  end Put_Line;

end Ustrings;
