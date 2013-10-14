#!/usr/local/bin/spar

pragma annotate( summary, "moire" );
pragma annotate( description, "Draw a moire pattern and erase it using inverse pen mode" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure moire is
  c    : pen.canvas_id;
  s    : string;
begin
  pen.new_window_canvas( 200,200,32,c);
  pen.set_pen_mode( c, pen_mode.invert );
  for y in reverse 1..99 loop
      pen.move_to( c, 1, y );
      pen.line_to( c, (100-y), 1 );
      pen.move_to( c, 99, (100-y) );
      pen.line_to( c, y, 99 );
  end loop;
  delay 5;
  for y in reverse 1..99 loop
      pen.move_to( c, 1, y );
      pen.line_to( c, (100-y), 1 );
      pen.move_to( c, 99, (100-y) );
      pen.line_to( c, y, 99 );
  end loop;
  ? "Press return";
  s := get_line;

end moire;

-- VIM editor formatting instructions
-- vim: ft=spar

