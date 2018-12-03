#!/usr/local/bin/spar

pragma annotate( summary, "graph_series.sp" )
              @( description, "Plot one or more series of data points on " )
              @( description, "a graph.  The maximum number of series is 3. " )
              @( description, "A history of 30 data points is maintained. " )
              @( description, "The graph will be saved as a bmp file. If " )
              @( description, "Imagemagick is installed, it will also be " )
              @( description, "saved as a png file." )
              @( param, "the graph name" )
              @( param, "one, two or three data points" )
              @( author, "Ken O. Burtch" );
pragma license( unrestricted );
pragma software_model( shell_script );

procedure graph is
  graph_name : string;
  num_series : natural;

  type data_arrays is array(1..30) of float;

  data_file : file_type;
  data_array1 : data_arrays;
  data_array2 : data_arrays;
  data_array3 : data_arrays;
  data_point : universal_numeric;
  data_file_name : string;

  canvas : pen.canvas_id;

  -- For Y-axis scale, 1 = 0..100, 2 = 0..200, etc.
  base_y_axis_scale : constant float := 3;
  y_axis_scale : float := base_y_axis_scale;
  candidate_scale : float;

  --  TO X
  --
  -- Constrain the given horizontal coordinate to the graph.
  ----------------------------------------------------------------------------

  function to_x( f : float ) return pen.coordinate is
     x : pen.coordinate;
  begin
     x := pen.coordinate( f );
     if x < 2 then
        x := 2;
     elsif x > 98 then
        x := 98;
     end if;
     return x;
  end to_x;


  --  TO Y
  --
  -- Constrain the given vertical coordinate to the graph.  Also flip
  -- origin to bottom of display.
  ----------------------------------------------------------------------------

  function to_y( f : float ) return pen.coordinate is
     y : pen.coordinate;
  begin
     y := pen.coordinate( 100 - f );
     if y < 2 then
        y := 2;
     elsif y > 98 then
        y := 98;
     end if;
     return y;
  end to_y;


  --  DRAW GRAPH BACKGROUND
  --
  -- Draw the graph background and borders
  ----------------------------------------------------------------------------

  procedure draw_graph_background is
    x : pen.coordinate;
  begin
    pen.wait_to_reveal( canvas );
    pen.set_pen_ink(canvas, 20, 20, 20);
    for i in arrays.first(data_array1)..arrays.last(data_array1) loop
        data_point := natural(i) - natural(arrays.first(data_array1));
        data_point := 1+(100 * data_point) / float(arrays.length( data_array1) );
        x := pen.coordinate( data_point );
        pen.vline( canvas, x, 1, 99 );
    end loop;
    for i in 1..9 loop
        pen.hline( canvas, 1, 99, i*10 );
    end loop;
    pen.set_pen_ink(canvas, pen_color_name.white );
    pen.hline(canvas,1,99,1);
    pen.hline(canvas,1,99,99);
    pen.vline(canvas,1,1,99);
    pen.vline(canvas,99,1,99);
    pen.reveal( canvas );
  end draw_graph_background;


  --  DRAW SERIES
  --
  -- Plot a series of points on the graph in the given colours.
  ----------------------------------------------------------------------------

  procedure draw_series( series : in out data_arrays; scale : float;
     point_color : pen.color_name; line_color : pen.color_name ) is
     x : pen.coordinate;
     y : pen.coordinate;
  begin
     pen.wait_to_reveal( canvas );
     pen.set_pen_ink(canvas, point_color);
     for i in arrays.first(series)..arrays.last(series) loop
         y := to_y( series(i) / scale );
         data_point := natural(i) - natural(arrays.first(series));
         data_point := 1+(100 * data_point) / float(arrays.length(series) );
         x := to_x( data_point );
         pen.move_to(canvas, x, y-0.5 );
         pen.line_to(canvas, x+0.5, y)
                   @(canvas, x, y+0.5)
                   @(canvas, x-0.5, y)
                   @(canvas, x, y-0.5);
     end loop;
     pen.set_pen_ink(canvas, line_color);
     for i in arrays.first(series)..arrays.last(series) loop
         y := to_y( series(i) / scale );
         data_point := natural(i) - natural(arrays.first(series));
         data_point := 1+(100 * data_point) / float(arrays.length( series ) );
         x := to_x( data_point );
         if i = 1 then
            pen.move_to( canvas, x, y );
         end if;
         pen.line_to( canvas, x, y );
     end loop;
     pen.reveal( canvas );
  end draw_series;


  -- USAGE
  --
  -- Show the help based on the pragma annotate documentation
  ----------------------------------------------------------------------------

  procedure usage is
  begin
    help( source_info.enclosing_entity );
  end usage;

begin

  -- parameter handling

  if $# < 2 then
     put_line( "At least two parameters are required" );
     command_line.set_exit_status(192);
     return;
  end if;

  if $1 = "-h" or $1 = "--help" then
     usage;
     command_line.set_exit_status(192);
     return;
  end if;

  graph_name := strings.trim( $1 );
  if graph_name = "" then
     put_line( "the graph name is empty" );
     command_line.set_exit_status(192);
     return;
  end if;

  num_series := $# - 1;
  if num_series > 3 then
     put_line( "the maximum number of series is 3" );
     command_line.set_exit_status(192);
     return;
  end if;

  -- read the history

  data_file_name := graph_name & ".dat";
  if files.exists( "data/" & data_file_name ) then
     open( data_file, in_file, "data/" & data_file_name );
     for i in arrays.first(data_array1)..arrays.last(data_array1) loop
         data_point := numerics.value( get_line( data_file ));
         data_array1(i) := data_point;
         candidate_scale := numerics.ceiling( data_point / 100 );
         if candidate_scale > y_axis_scale then
            y_axis_scale := candidate_scale;
         end if;
         data_point := numerics.value( get_line( data_file ));
         data_array2(i) := data_point;
         candidate_scale := numerics.ceiling( data_point / 100 );
         if candidate_scale > y_axis_scale then
            y_axis_scale := candidate_scale;
         end if;
         data_point := numerics.value( get_line( data_file ));
         data_array3(i) := data_point;
         candidate_scale := numerics.ceiling( data_point / 100 );
         if candidate_scale > y_axis_scale then
            y_axis_scale := candidate_scale;
         end if;
     end loop;
     close( data_file );
  else
     for i in arrays.first(data_array1)..arrays.last(data_array1) loop
         data_array1(i) := 0;
         data_array2(i) := 0;
         data_array3(i) := 0;
     end loop;
  end if;

  -- Record the new data points in the series

  arrays.shift_left( data_array1 );
  data_point := natural( numerics.value( $2 ));
  data_array1( arrays.last( data_array1 ) ) := data_point;
  if num_series > 1 then
     arrays.shift_left( data_array2 );
     data_point := natural( numerics.value( $3 ));
     data_array2( arrays.last( data_array2 ) ) := data_point;
  end if;
  if num_series > 2 then
     arrays.shift_left( data_array3 );
     data_point := natural( numerics.value( $4 ));
     data_array3( arrays.last( data_array3 ) ) := data_point;
  end if;

  -- Note that off-screen canvases are broken.  We will need a frame buffer.
  begin
    pen.new_window_canvas(512, 512, 32, canvas );
  exception when others =>
    put_line( "Failed to create new canvas" );
    return;
  end;

  -- Draw the graph

  draw_graph_background;
  draw_series( data_array1, y_axis_scale, pen_color_name.darkred, pen_color_name.red );
  if num_series > 1 then
     draw_series( data_array2, y_axis_scale, pen_color_name.darkgreen, pen_color_name.green );
  end if;
  if num_series > 2 then
     draw_series( data_array3, y_axis_scale, pen_color_name.darkblue, pen_color_name.blue );
  end if;

  -- Save the graph

  pen.save_canvas( graph_name & ".bmp", canvas );
  pen.close_canvas( canvas );

  -- This depends on imagemagick convert being installed

  convert( graph_name & ".bmp", graph_name & ".png" );
  mv( graph_name & ".png", "/var/www/html/pegasoft/ssds/" );
  chown( "webadmin:apache", "/var/www/html/pegasoft/ssds/" & graph_name & ".png" );
  chmod( "750", "/var/www/html/pegasoft/ssds/" & graph_name & ".png" );
  rm( graph_name & ".bmp" );

  -- Save the history data

  create( data_file, out_file, "data/" & data_file_name );
  for i in arrays.first(data_array1)..arrays.last(data_array1) loop
      put_line( data_file, strings.image( data_array1(i) ) )
             @( data_file, strings.image( data_array2(i) ) )
             @( data_file, strings.image( data_array3(i) ) );
  end loop;
  close( data_file );
end graph;

-- VIM editor formatting instructions
-- vim: ft=spar
