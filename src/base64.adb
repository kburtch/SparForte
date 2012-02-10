
with Ada.Streams;
package body Base64 is

  subtype Six_Bits is Ada.Streams.Stream_Element range 0 .. 63;

  From_String: constant array (Character) of Six_Bits
    := ('A' => 0,'B' => 1,'C' => 2,'D' => 3,'E' => 4,'F' => 5,'G' => 6,
        'H' => 7,'I' => 8,'J' => 9,'K' =>10,'L' =>11,'M' =>12,'N' =>13,
        'O' =>14,'P' =>15,'Q' =>16,'R' =>17,'S' =>18,'T' =>19,'U' =>20,
        'V' =>21,'W' =>22,'X' =>23,'Y' =>24,'Z' =>25,'a' =>26,'b' =>27,
        'c' =>28,'d' =>29,'e' =>30,'f' =>31,'g' =>32,'h' =>33,'i' =>34,
        'j' =>35,'k' =>36,'l' =>37,'m' =>38,'n' =>39,'o' =>40,'p' =>41,
        'q' =>42,'r' =>43,'s' =>44,'t' =>45,'u' =>46,'v' =>47,'w' =>48,
        'x' =>49,'y' =>50,'z' =>51,'0' =>52,'1' =>53,'2' =>54,'3' =>55,
        '4' =>56,'5' =>57,'6' =>58,'7' =>59,'8' =>60,'9' =>61,'+' =>62,
        '/' =>63,
        others => 0);

  procedure Decode(Source  : in     String;
                   Target  :    out Ada.Streams.Stream_Element_Array;
                   Last    :    out Ada.Streams.Stream_Element_Offset) is
  -- decode Source into Target(Target'first .. Last)
  -- Note: it may be appropriate to prescan Source for '=',
  -- indicating termination, or for illegitimate characters,
  -- indicating corruption, before calling Decode.
    use type Ada.Streams.Stream_Element;
    use type Ada.Streams.Stream_Element_Offset;
    D       : Six_Bits;
    type Slots is mod 4;
    Slot    : Slots := 0;
  begin
    Last := Target'first - 1;
    for Si in Source'range loop
      D := From_String(Source(Si));
      if D /= 0 or else Source(Si) = 'A' then
        -- OK source
        case Slot is
          when 0 =>
            Last := Last + 1;
            Target(Last) := 4 * D;            -- dddddd00 ........ ........
          when 1 =>
            Target(Last) := Target(Last) + D / 16;
            exit when Last = Target'last
              and then (Si = Source'last or else Source(Si + 1) = '=')
              and then (D mod 16) = 0;
            Last := Last + 1;
            Target(Last) := (D mod 16) * 16;  -- dddddddd dddd0000 ........
          when 2 =>
            Target(Last) := Target(Last) + D / 4;
            exit when Last = Target'last
              and then (Si = Source'last or else Source(Si + 1) = '=')
              and then (D mod 4) = 0;
            Last := Last + 1;
            Target(Last) := (D mod 4) * 64;   -- dddddddd dddddddd dd000000
          when 3 =>
            Target(Last) := Target(Last) + D; -- dddddddd dddddddd dddddddd
        end case;
        Slot := Slot + 1;
      elsif Source(Si) = '=' then
        exit; -- terminator encountered
      end if; -- silently ignore whitespace, lf, garbage, ...
    end loop;
  end Decode;


  To_String: constant array (Six_Bits) of Character
    := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  procedure Encode(Source  : in     Ada.Streams.Stream_Element_Array;
                   Target  :    out String;
                   Last    :    out Natural) is
  -- Target is filled in four character increments, except that
  -- a CR-LF pair is inserted after every 76 characters.
  -- Target'length must be at least:
  -- Output_Quad_Count: constant := (Source'length + 2) / 3;
  -- Output_Byte_Count: constant := 4 * Output_Quad_Count;
  -- Target'length = Output_Byte_Count + 2 * (Output_Byte_Count / 76)
  -- Constraint_Error will be raised if Target isn't long enough.
    use type Ada.Streams.Stream_Element;
    use type Ada.Streams.Stream_Element_Offset;
    D       : Six_Bits;
    type Slots is mod 3;
    Slot    : Slots := 0;
    Output_Line_Length: Natural := 0;
  begin
    Last := Target'first - 1;
    for Si in Source'range loop
      case Slot is
        when 0 =>
          if Output_Line_Length = 76 then
            Last := Last + 2;
            Target(Last - 1) := Ascii.Cr;
            Target(Last) := Ascii.Lf;
            Output_Line_Length := 0;
          end if;
          Output_Line_Length := Output_Line_Length + 4;
          Last := Last + 4;
          Target(Last - 3) := To_String(Source(Si) / 4);
          D := (Source(Si) mod 4) * 16;
          Target(Last - 2) := To_String(D);
          Target(Last - 1) := '=';
          Target(Last) := '=';
          -- dddddd dd0000  = =
        when 1 =>
          D := D + Source(Si) / 16;
          Target(Last - 2) := To_String(D);
          D := (Source(Si) mod 16) * 4;
          Target(Last - 1) := To_String(D);
          -- dddddd dddddd dddd00 =
        when 2 =>
          D := D + Source(Si) / 64;
          Target(Last - 1) := To_String(D);
          Target(Last) := To_String(Source(Si) mod 64);
          -- dddddd dddddd dddddd dddddd
      end case;
      Slot := Slot + 1;
    end loop;
  end Encode;

   function Encode_Length(Length : Ada.Streams.Stream_Element_Offset) return
        Natural
    is
        use type Ada.Streams.Stream_Element_Offset;
        Output_Quad_Count : constant Natural := (Natural(Length) + 2) / 3;
        Output_Byte_Count : constant Natural := 4 * Output_Quad_Count;
        Result : constant Natural := Output_Byte_Count +
                2 * (Output_Byte_Count / 76);
     begin
        if Output_Byte_Count mod 76 = 0 then
            return Result - 2;
        else
            return Result;
        end if;
    end Encode_Length;

    procedure Decode_Length(Source        : in     String;
                            Source_Last   :    out Natural;
                            Target_Length :    out
                                  Ada.Streams.Stream_Element_Offset;
                            Complete      :    out Boolean) is
        Valid_Char_Count : Natural; -- range 0 .. Source'Length
        Valid_Char_Count_In_Range : Natural; -- Could do this with math, too
        Cur_Index : Positive; -- range Source'First .. Source'Last + 1
        use type Ada.Streams.Stream_Element; -- for From_String /=
    begin
        Source_Last := Source'First - 1;
        Valid_Char_Count := 0;
        Valid_Char_Count_In_Range := 0;
        Cur_Index := Source'First;
        while Cur_Index <= Source'Last and then
            Source(Cur_Index) /= '=' and then
            Source(Cur_Index) /= '-'
        loop
            -- Odd if condition, because we use the same value
            -- for 'A' as we do for "Nope!"
            if Source(Cur_Index) = 'A' or
                    From_String(Source(Cur_Index)) /= 0 then
                Valid_Char_Count := Valid_Char_Count + 1;
                if Valid_Char_Count mod 4 = 0 then
                    Source_Last := Cur_Index;
                    Valid_Char_Count_In_Range := Valid_Char_Count;
                end if;
            end if;
            Cur_Index := Cur_Index + 1;
        end loop;
        Complete := Cur_Index <= Source'Last;
        if Complete then
            -- Take care of dribbles of valid characters before the '=' or '-'
            Valid_Char_Count_In_Range := Valid_Char_Count;
            Source_Last := Cur_Index - 1;
        end if;
        if Valid_Char_Count_In_Range mod 4 = 0 then
            Target_Length := Ada.Streams.Stream_Element_Offset(
                    (Valid_Char_Count_In_Range / 4) * 3);
        else
            Target_Length := Ada.Streams.Stream_Element_Offset(
                    (Valid_Char_Count_In_Range / 4) * 3 +
                        (Valid_Char_Count_In_Range mod 4) - 1);
        end if;
    end Decode_Length;

  procedure Encode_Stream(From  : in out Ada.Streams.Stream_Io.File_Type;
                          To    : in out Ada.Text_Io.File_Type) is
    Octet_Buffer : Ada.Streams.Stream_Element_Array(1..Base64.Octets_Per_Line);
    Octet_Buffer_Last : Ada.Streams.Stream_Element_Count;
    String_Buffer : String(1..Base64.Characters_Per_Line);
    String_Buffer_Last : Natural range 0 .. Base64.Characters_Per_Line;
    use type Ada.Streams.Stream_Element_Offset;
  begin
    while not Ada.Streams.Stream_Io.End_Of_File(From) loop
        Ada.Streams.Stream_Io.Read(From, Octet_Buffer, Octet_Buffer_Last);
          -- You never get short reads from Stream_Io.Read
        Base64.Encode(Octet_Buffer(1..Octet_Buffer_Last), String_Buffer,
              String_Buffer_Last);
        Ada.Text_Io.Put_Line(To, String_Buffer(1..String_Buffer_Last));
    end loop;
  end Encode_Stream;

  procedure Decode_Stream(From  : in out Ada.Text_Io.File_Type;
                          To    : in out Ada.Streams.Stream_Io.File_Type) is
    First_Char : Character;
    End_Line   : Boolean;
    In_Line    : String(1..1400); -- 1400 because of SMTP
    Line_Last  : Natural;
    Octets     : Ada.Streams.Stream_Element_Array(1..1400);
    Octets_Last: Ada.Streams.Stream_Element_Offset;
    use type Ada.Streams.Stream_Element_Offset;
  begin
    loop -- Loop over lines
        exit when Ada.Text_Io.End_Of_File(From);
        Ada.Text_Io.Look_Ahead(From, First_Char, End_Line);
        exit when not End_Line and then First_Char = '-';
        Ada.Text_Io.Get_Line(From, In_Line, Line_Last);
        if 0 < Line_Last then
            Decode(In_Line(1..Line_Last), Octets, Octets_Last);
            if 0 < Octets_Last then
                Ada.Streams.Stream_Io.Write(To, Octets(1..Octets_Last));
            end if;
        end if;
    end loop;
  end Decode_Stream;

end Base64;
