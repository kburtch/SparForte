#!/usr/local/bin/spar

pragma annotate( "geometry" );
pragma annotate( "" );
pragma annotate( "Draw geometric shapes with SparForte" );
pragma annotate( "by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure geometry is

  c    : pen.canvas_id;
  r    : pen.rect;
  s    : string;

-- the c2

  procedure draw_triangle( c2 : in out pen.canvas_id; x : pen.coordinate; y : pen.coordinate ) is
  begin
      pen.move_to( c2, x, y );
      pen.line( c2, 5, -10 );
      pen.line( c2, 5,  10 );
      pen.line( c2, -10, 0 );
  end draw_triangle;

begin

  pen.new_window_canvas( 200,200,32,c);

  -- a filled square

  pen.set_rect( r, 10, 10, 20, 20 );
  pen.set_pen_mode( c, pen_mode.invert );
  pen.set_pen_ink( c, 100.0, 0.0, 0.0 );
  for x in 1..35 loop
      pen.paint_rect( c, r );
      if x /= 35 then
         delay 0.02;
         pen.paint_rect( c, r );
         pen.offset_rect( r, 2, 0 );
      end if;
  end loop;

  -- a framed square

  pen.set_rect( r, 10, 20, 20, 30 );
  pen.set_pen_mode( c, pen_mode.invert );
  pen.set_pen_ink( c, 0.0, 100.0, 0.0 );
  for x in 1..35 loop
      pen.frame_rect( c, r );
      if x /= 35 then
         delay 0.02;
         pen.frame_rect( c, r );
         pen.offset_rect( r, 2, 0 );
      end if;
  end loop;

  -- a filled circle

  pen.set_rect( r, 10, 30, 20, 40 );
  pen.set_pen_mode( c, pen_mode.invert );
  pen.set_pen_ink( c, 0.0, 0.0, 100.0 );
  for x in 1..35 loop
      pen.paint_ellipse( c, r );
      if x /= 35 then
         delay 0.02;
         pen.paint_ellipse( c, r );
         pen.offset_rect( r, 2, 0 );
      end if;
  end loop;

  -- a framed circle

  pen.set_rect( r, 10, 40, 20, 50 );
  pen.set_pen_mode( c, pen_mode.invert );
  pen.set_pen_ink( c, 100.0, 100.0, 0.0 );
  for x in 1..35 loop
      pen.frame_ellipse( c, r );
      if x /= 35 then
         delay 0.02;
         pen.frame_ellipse( c, r );
         pen.offset_rect( r, 2, 0 );
      end if;
  end loop;

  -- a triangle

  pen.set_rect( r, 10, 40, 20, 60 );
  pen.set_pen_mode( c, pen_mode.invert );
  pen.set_pen_ink( c, 0.0, 100.0, 100.0 );
  for x in 1..35 loop
      draw_triangle( c, r.left, r.bottom );
      if x /= 35 then
         delay 0.02;
         draw_triangle( c, r.left, r.bottom );
         pen.offset_rect( r, 2, 0 );
      end if;
  end loop;


  ? "Press return";
  s := get_line;

end geometry;

