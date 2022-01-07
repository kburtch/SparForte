#!/usr/local/bin/spar

pragma annotate( summary, "weave" );
pragma annotate( description, "Draw a weave pattern and erase it using inverse pen mode" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure weave is

  max_iterations : constant integer := 500;
  tail_length : constant integer := 15;

  c : limited pen.canvas_id;

  -- draw four sets of reflected lines, different colours for each of
  -- the lines

  procedure draw_reflected_lines( x1 : float; y1 : float; x2 : float; y2 : float ) is
  begin
    pen.set_pen_ink( c, 100, 0, 0 );
    pen.move_to( c, pen.coordinate( x1 ), pen.coordinate( y1 ) );
    pen.line_to( c, pen.coordinate( x2 ), pen.coordinate( y2 ) );
    pen.set_pen_ink( c, 0, 100, 0 );
    pen.move_to( c, 100 - pen.coordinate( x1 ), pen.coordinate( y1 ) );
    pen.line_to( c, 100 - pen.coordinate( x2 ), pen.coordinate( y2 ) );
    pen.set_pen_ink( c, 0, 0, 100 );
    pen.move_to( c, pen.coordinate( x1 ), 100 - pen.coordinate( y1 ) );
    pen.line_to( c, pen.coordinate( x2 ), 100 - pen.coordinate( y2 ) );
    pen.set_pen_ink( c, 100, 100, 0 );
    pen.move_to( c, 100 - pen.coordinate( x1 ), 100 - pen.coordinate( y1 ) );
    pen.line_to( c, 100 - pen.coordinate( x2 ), 100 - pen.coordinate( y2 ) );
  end draw_reflected_lines;

  -- reverse direction at a speed of 0.5 to 1.5 pixels

  function bounce( direction_delta : float ) return float is
  begin
    return numerics.copy_sign( numerics.random+0.5, -direction_delta );
  end bounce;

  -- These arrays contain the coordinates of the lines that have been
  -- drawn.  pos is the position where the next values will be saved

  type line_array is array( 1..tail_length ) of float;
  x1_array : line_array;
  y1_array : line_array;
  x2_array : line_array;
  y2_array : line_array;
  pos      : integer := 1;

  -- current coordinates of the latest line and direction deltas

  x1   : float;
  y1   : float;
  x2   : float;
  y2   : float;
  dx1  : float;
  dy1  : float;
  dx2  : float;
  dy2  : float;

  -- iteration counter

  cnt  : integer := 0;

  s    : string;

begin

  -- create a drawing canvas and set the pen mode to invert so that
  -- drawing a line twice will erase it

  pen.new_window_canvas( 200,200,32,c);
  pen.set_title( c, "Weave" );
  pen.set_pen_mode( c, pen_mode.invert );

  -- set the initial location and speed of the lines

  x1 := 10.0;
  y1 := 10.0;
  x2 := 30.0;
  y2 := 30.0;
  dx1:= 1.0;
  dy1:= 1.0;
  dx2:= -1.0;
  dy2:= 1.0;

  -- use x1 = -1 to represent no data at that position

  for i in arrays.first( x1_array ) .. arrays.last( x1_array ) loop
     x1_array(i) := -1;
  end loop;

  -- draw the very first lines

  pen.wait_to_reveal( c );
  draw_reflected_lines( x1, y1, x2, y2 );
  pen.reveal( c );
  delay 0.03; -- about 30 frames/second

  loop
     pen.wait_to_reveal( c );

     -- something to erase? then erase it by redrawing it

     if x1_array( pos ) > 0 then
--put( "Erase: " ) @ ( head ) @ ( ":" ) @ ( x1_array(head) ) @ ( "," ) @ ( y1_array(head) ) @ ( " " ) @ ( x2_array(head) ) @ ( "," ) @ ( y2_array(head) ); new_line;
        draw_reflected_lines( x1_array( pos ), y1_array( pos ),
           x2_array( pos ), y2_array( pos ) );
     end if;

     -- remember the position of the latest lines

     x1_array( pos ) := x1;
     y1_array( pos ) := y1;
     x2_array( pos ) := x2;
     y2_array( pos ) := y2;

     -- for each coordiate, move forward.  if a coordinate encounters
     -- the side of the window, reverse the direction for horizontal,
     -- vertical or both

     if x1 + dx1 < 1 then
        dx1 := bounce( @ );
     elsif x1 + dx1 > 99 then
        dx1 := bounce( @ );
     end if;
     x1 := @ + dx1;
     if y1 + dy1 < 1 then
        dy1 := bounce( @ );
     elsif y1 + dy1 > 99 then
        dy1 := bounce( @ );
     end if;
     y1 := @ + dy1;
     if x2 + dx2 < 1 then
        dx2 := bounce( @ );
     elsif x2 + dx2 > 99 then
        dx2 := bounce( @ );
     end if;
     x2 := @ + dx2;
     if y2 + dy2 < 1 then
        dy2 := bounce( @ );
     elsif y2 + dy2 > 99 then
        dy2 := bounce( @ );
     end if;
     y2 := @ + dy2;

     -- draw the new lines and update the display

     draw_reflected_lines( x1, y1, x2, y2 );
     pen.reveal( c );
     delay 0.03;

     -- count the iterations.  quit when max iterations is reached

     cnt := @ + 1;
     exit when cnt > max_iterations;

     -- advance the save position.  Rollover back to first position
     -- if the end of the array is reached.

     if pos = arrays.last( x1_array ) then
        pos := 1;
     else
        pos := @+1;
     end if;
  end loop;

  ? "Press return";
  s := get_line;

end weave;

-- VIM editor formatting instructions
-- vim: ft=spar

