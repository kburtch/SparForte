with ada.text_io,
     ada.strings.unbounded,
     readline;
use  ada.text_io,
     ada.strings.unbounded,
     readline;

procedure rl is

procedure getLine_withReadline( line : out unbounded_string; keepHistory : boolean := false ) is
begin
  line := to_unbounded_string( read_line( "$ " ) );
end getLine_withReadline;

  line : unbounded_string;
begin
   loop
      getLine_withReadline( line );
      Put_Line ("You typed: " & to_string( Line ) );
   end loop;
exception when end_error =>
   put_line( "done" );
end rl;


