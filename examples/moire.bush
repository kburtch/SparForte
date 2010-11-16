#!/usr/local/bin/bush

pragma annotate( "moire" );
pragma annotate( "" );
pragma annotate( "Draw a moire pattern and erase it using inverse pen mode" );
pragma annotate( "by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure moire is
  c    : pen.canvas_id;
  r    : pen.rect;
  s    : string;
  fill : boolean;
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

