#!/usr/local/bin/bush

pragma annotate( "chessboard" );
pragma annotate( "" );
pragma annotate( "Draw a chessboard with graphics using the Business Shell" );
pragma annotate( "by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure chessboard is

  c    : pen.canvas_id;
  r    : pen.rect;
  s    : string;
  fill : boolean;

begin

  pen.new_window_canvas( 200,200,32,c);

  for y in 1..8 loop
      fill := y mod 2 = 1;
      pen.set_rect( r, 10, 10*y, 20, 10*(y+1) );
      for x in 1..8 loop
          pen.frame_rect( c, r );
          if fill then
             pen.paint_rect( c, r );
          end if;
          pen.offset_rect( r, 10, 0 );
          fill := not @;
      end loop;
  end loop;

  ? "Press return";
  s := get_line;

end chessboard;

