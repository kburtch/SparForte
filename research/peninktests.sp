procedure geometry is
  c : pen.canvas_id;
  r : pen.rect;
  s : string;

procedure frame_rects is
begin
  pen.set_rect( r, 10, 5, 20, 15 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 100, 0, 0 );
  pen.frame_rect( c, r );
  pen.set_pen_mode( c, pen_mode.copy );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, (1+i)*10, 0, 0 );
      pen.frame_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 20, 20, 30 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 100, 0, 0 );
  pen.frame_rect( c, r );
  pen.set_pen_mode( c, pen_mode.invert );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, (1+i)*10, 0, 0 );
      pen.frame_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 35, 20, 45 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 0, 100, 0 );
  pen.frame_rect( c, r );
  pen.set_pen_mode( c, pen_mode.add );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, 0, (1+i)*10, 0 );
      pen.frame_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 50, 20, 60 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 0, 100, 0 );
  pen.frame_rect( c, r );
  pen.set_pen_mode( c, pen_mode.subtract );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, 0, (1+i)*10, 0 );
      pen.frame_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 65, 20, 75 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 0, 0, 100 );
  pen.frame_rect( c, r );
  pen.set_pen_mode( c, pen_mode.off );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, 0, 0, (1+i)*10 );
      pen.frame_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 80, 20, 90 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 0, 0, 100 );
  pen.frame_rect( c, r );
  pen.set_pen_mode( c, pen_mode.average );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, 0, 0, (1+i)*10 );
      pen.frame_rect( c, r );
  end loop;
end frame_rects;

procedure fill_rects is
begin
  pen.set_rect( r, 10, 5, 20, 15 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 100, 0, 0 );
  pen.paint_rect( c, r );
  pen.set_pen_mode( c, pen_mode.copy );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, (1+i)*10, 0, 0 );
      pen.paint_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 20, 20, 30 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 100, 0, 0 );
  pen.paint_rect( c, r );
  pen.set_pen_mode( c, pen_mode.invert );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, (1+i)*10, 0, 0 );
      pen.paint_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 35, 20, 45 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 0, 100, 0 );
  pen.paint_rect( c, r );
  pen.set_pen_mode( c, pen_mode.add );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, 0, (1+i)*10, 0 );
      pen.paint_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 50, 20, 60 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 0, 100, 0 );
  pen.paint_rect( c, r );
  pen.set_pen_mode( c, pen_mode.subtract );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, 0, (1+i)*10, 0 );
      pen.paint_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 65, 20, 75 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 0, 0, 100 );
  pen.paint_rect( c, r );
  pen.set_pen_mode( c, pen_mode.off );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, 0, 0, (1+i)*10 );
      pen.paint_rect( c, r );
  end loop;

  pen.set_rect( r, 10, 80, 20, 90 );
  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, 0, 0, 100 );
  pen.paint_rect( c, r );
  pen.set_pen_mode( c, pen_mode.average );
  for i in 1..7 loop
      pen.offset_rect( r, 10, 1 );
      pen.set_pen_ink( c, 0, 0, (1+i)*10 );
      pen.paint_rect( c, r );
  end loop;
end fill_rects;

begin

  pen.new_window_canvas( 200, 200, 32, c );
  ? "pen mode: copy, invert, add, subtract, off, average";

  frame_rects;
  delay 5;

  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, pen_color_name.grey );
  pen.clear( c );
  frame_rects;
  delay 5;

  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, pen_color_name.white );
  pen.clear( c );
  frame_rects;
  delay 5;

  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, pen_color_name.black );
  pen.clear( c );
  fill_rects;
  delay 5;

  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, pen_color_name.grey );
  pen.clear( c );
  fill_rects;
  delay 5;

  pen.set_pen_mode( c, pen_mode.copy );
  pen.set_pen_ink( c, pen_color_name.white );
  pen.clear( c );
  fill_rects;
  delay 5;

  ? "Press return";
  s := get_line;
end geometry;

