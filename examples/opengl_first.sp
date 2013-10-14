#!/usr/local/bin/spar

pragma annotate( summary, "opengl_first" )
              @( description, "A first example of OpenGL: display a triangle" )
              @( description, "and rectangle side-by-side." )
              @( description, "Based on code was created by Jeff Molofee 1999" )
              @( description, "and ported to Linux/SDL by Ti Leggett 2001)" )
              @( see_also, "http://nehe.gamedev.net" )
              @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure opengl_first is
  width  : constant universal_numeric := 320;
  height : constant universal_numeric := 200;
  c : pen.canvas_id;
  s : string;
begin

  -- initialize opengl

  pen.new_gl_window_canvas( width, height, 32, c );
  pen.set_title( c, "OpenGL First" );
  pen.glshademodel( pen.gl_smooth );
  pen.glclearcolor( 0.5294, 0.8078, 0.9216, 1.0 ); -- sky blue
  pen.glcleardepth( 1 );

  -- Enable 3-D

  pen.glenable( pen.gl_depth_test );
  pen.gldepthfunc( pen.gl_lequal );
  pen.glhint( pen.gl_perspective_correction_hint, pen.gl_nicest );

  -- Clear the canvas

  pen.glclear( pen.gl_color_buffer_bit or pen.gl_depth_buffer_bit );

  -- Setup view

  pen.glviewport( 0, 0, width, height );
  pen.glmatrixmode( pen.gl_projection );
  pen.glloadidentity;
  pen.gluperspective( 45, width/height, 0.1, 100 );
  pen.glmatrixmode( pen.gl_modelview );
  pen.glloadidentity;

  -- Move left and back

  pen.gltranslated( -1.5, 0, -6 );

  -- Draw a white triangle

  pen.glbegin( pen.gl_triangles );
    pen.glcolor3( pen_color_name.white );
    pen.glvertex3d( 0, 1, 0 );
    pen.glvertex3d( -1, -1, 0 );
    pen.glvertex3d( 1, -1, 0 );
  pen.glend;

  -- Move right

  pen.gltranslated( 3, 0, 0 );

  -- Draw a black square

  pen.glbegin( pen.gl_quads );
    pen.glcolor3( pen_color_name.black );
    pen.glvertex3d( -1,  1, 0 );
    pen.glvertex3d(  1,  1, 0 );
    pen.glvertex3d(  1, -1, 0 );
    pen.glvertex3d( -1, -1, 0 );
  pen.glend;

  -- Display the scene

  pen.reveal_now( c );

  -- Wait to close

  put_line( "Press any key" );
  s := get_line;
  pen.close_canvas( c );

end opengl_first;

-- VIM editor formatting instructions
-- vim: ft=spar

