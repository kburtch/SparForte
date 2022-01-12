#!/usr/local/bin/spar

pragma annotate( summary, "chessboard" );
pragma annotate( description, "Draw a chessboard with graphics using SparForte" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure chessboard is

  c    : limited pen.canvas_id;
  r    : limited pen.rect;
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

-- VIM editor formatting instructions
-- vim: ft=spar

