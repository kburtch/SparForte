#!/usr/local/bin/spar

pragma annotate( summary, "opengl_second" )
              @( description, "A second example of OpenGL: animate a" )
              @( description, "rotating triangle and rectangle side-by-side." )
              @( description, "Based on code was created by Jeff Molofee 1999" )
              @( description, "and ported to Linux/SDL by Ti Leggett 2001)" )
              @( see_also, "http://nehe.gamedev.net" )
              @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure opengl_second is
  width  : constant universal_numeric := 320;
  height : constant universal_numeric := 200;
  c : pen.canvas_id;
  i : integer;
begin

  -- initialize opengl

  pen.new_gl_window_canvas( width, height, 32, c );
  pen.set_title( c, "OpenGL Second Example" );
  pen.glshademodel( pen.gl_smooth );
  pen.glclearcolor( 0.5294, 0.8078, 0.9216, 1.0 ); -- sky blue
  pen.glcleardepth( 1 );

  -- Enable 3-D

  pen.glenable( pen.gl_depth_test );
  pen.gldepthfunc( pen.gl_lequal );
  pen.glhint( pen.gl_perspective_correction_hint, pen.gl_nicest );

  -- Setup view

  pen.glviewport( 0, 0, width, height );
  pen.glmatrixmode( pen.gl_projection );
  pen.gluperspective( 45, width/height, 0.1, 100 );
  pen.glmatrixmode( pen.gl_modelview );

  loop

    i := 0;

    loop
      i := ( @ + 1 ) mod 180;
      exit when i = 0;

      -- Clear the canvas

      pen.glclear( pen.gl_color_buffer_bit or pen.gl_depth_buffer_bit );

      -- Start at the center

      pen.glloadidentity;

      -- Move left and back

      pen.gltranslated( -1.5, 0, -6 );
      pen.glrotated( pen.gldouble(i), 0, 1, 0 );

      -- Draw a white triangle

      pen.glbegin( pen.gl_triangles );
        pen.glcolor3( pen_color_name.white );
        pen.glvertex3d( 0, 1, 0 ) @ ( -1, -1, 0 ) @ ( 1, -1, 0 );
      pen.glend;

      -- Move right

      pen.glloadidentity;
      pen.gltranslated( 1.5, 0, -6 );
      pen.glrotated( pen.gldouble(i), 1, 0, 0 );

      -- Draw a black square

      pen.glbegin( pen.gl_quads );
        pen.glcolor3( pen_color_name.black );
        pen.glvertex3d( -1,  1, 0 ) @ (  1,  1, 0 ) @ (  1, -1, 0 ) @ ( -1, -1, 0 );
      pen.glend;

      -- Display the scene and wait 1/60th of a second

      pen.reveal_now( c );
      delay( 0.015 );

    end loop;

    -- Ask to do it again

    put( "Quit (Y/N)? " );
    exit when strings.to_upper( get_line ) = "Y";

  end loop;

  pen.close_canvas( c );

end opengl_second;

-- VIM editor formatting instructions
-- vim: ft=spar

