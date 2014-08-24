------------------------------------------------------------------------------
-- Pen Package Parser                                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2012 Free Software Foundation              --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  This is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- This is maintained at http://www.pegasoft.ca                             --
--                                                                          --
------------------------------------------------------------------------------
    with interfaces.C;
    use  interfaces.C;

with text_io;use text_io;
with system,
    interfaces.c,
    pen,
    bush_os.sdl,
    bush_os.opengl,
    user_io,
    world,
    scanner,
    scanner_res,
    string_util,
    -- scanner_arrays,
    parser_aux,
    parser_params,
    parser;
use pen,
    bush_os,
    bush_os.sdl,
    bush_os.opengl,
    user_io,
    world,
    scanner,
    scanner_res,
    string_util,
    -- scanner_arrays,
    parser_aux,
    parser_params,
    parser,
    bush_os;

package body parser_pen is

penRunning : boolean := false;
-- some versions of SDL take control of the screen if video is started so
-- we'll only start SDL when a canvas is created.

raiseGlExceptions : boolean := true;
-- true if user wants GL errors raised as exceptions

----> Utils

procedure bushRect2penRect( pen_rect : out aRect; bush_rect : identifier ) is
  left_field_t, top_field_t, right_field_t, bottom_field_t : identifier;
begin
  findField( bush_rect, 1, left_field_t );
     pen_rect.left := aCoordinate'Value( to_string( identifiers( left_field_t ).value ) );
  findField( bush_rect, 2, top_field_t );
     pen_rect.top := aCoordinate'Value( to_string( identifiers( top_field_t ).value ) );
  findField( bush_rect, 3, right_field_t );
     pen_rect.right := aCoordinate'Value( to_string( identifiers( right_field_t ).value ) );
  findField( bush_rect, 4, bottom_field_t );
     pen_rect.bottom := aCoordinate'Value( to_string( identifiers( bottom_field_t ).value ) );
end bushRect2penRect;

procedure penRect2bushRect( pen_rect : aRect; bush_rect : identifier ) is
  left_field_t, top_field_t, right_field_t, bottom_field_t : identifier;
begin
  findField( bush_rect, 1, left_field_t );
     identifiers( left_field_t ).value := to_unbounded_string( long_float( pen_rect.left ) );
  findField( bush_rect, 2, top_field_t );
     identifiers( top_field_t ).value := to_unbounded_string( long_float( pen_rect.top ) );
  findField( bush_rect, 3, right_field_t );
     identifiers( right_field_t ).value := to_unbounded_string( long_float( pen_rect.right ) );
  findField( bush_rect, 4, bottom_field_t );
     identifiers( bottom_field_t ).value := to_unbounded_string( long_float( pen_rect.bottom ) );
end penRect2bushRect;

procedure check_opengl_err is
  err_id : GLerrors;
begin
  -- TODO: need message, function  glGetString( name : GLenum ) return system.address
  err_id := glGetError;
  if err_id /= GL_NO_ERROR then
     -- TODO this is a hack until glGetString is done
     case err_id is
     when GL_INVALID_ENUM => err( "OpenGL error: invalid enum" );
     when GL_INVALID_VALUE => err( "OpenGL error: invalid value" );
     when GL_INVALID_OPERATION => err( "OpenGL error: invalid operation" );
     when GL_STACK_OVERFLOW => err( "OpenGL error: stack overflow" );
     when GL_STACK_UNDERFLOW => err( "OpenGL error: stack underflow" );
     when GL_OUT_OF_MEMORY => err( "OpenGL error: out of memory" );
     when others =>
        err( "OpenGL error: id" & err_id'img );
     end case;
  end if;
end check_opengl_err;

----> GL Exceptions

procedure ParsePenRaiseGlErrors is
  -- Syntax: pen.raise_gl_errors( bool );
  -- Source: N/A
  val  : unbounded_string;
  kind : identifier;
begin
  expect( pen_raise_gl_errors_t );
  ParseSingleNumericParameter( val, kind, boolean_t );
  if isExecutingCommand then
     declare
       flag : boolean := val = to_unbounded_string( "1" );
     begin
       raiseGlExceptions := flag;
     exception when others =>
       err( "exception was raised" );
     end;
  end if;
end ParsePenRaiseGlErrors;

-- TODO: IsRaisingGlErrors

----> Rects

procedure ParsePenSetRect is
  -- Syntax: pen.set_rect( rec, left, top, right, bottom );
  -- Source: Pen.setRect
  record_ref : reference;
  record_type : identifier := pen_rect_t;
  -- record_id : identifier;
  left_val,  top_val,  right_val,  bottom_val  : unbounded_string;
  left_type, top_type, right_type, bottom_type : identifier;
begin
  expect( pen_set_rect_t );
  expect( symbol_t, "(" );
  -- ParseIdentifier( record_id );
  ParseOutParameter( record_ref, record_type );
  if baseTypesOk( record_type, pen_rect_t ) then
  -- if baseTypesOk( identifiers( record_id ).kind, pen_rect_t ) then
     expect( symbol_t, "," );
     ParseExpression( left_val, left_type );
     if baseTypesOk( left_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( top_val, top_type );
        if baseTypesOk( top_type, pen_coordinate_t ) then
           expect( symbol_t, "," );
           ParseExpression( right_val, right_type );
           if baseTypesOk( right_type, pen_coordinate_t ) then
              expect( symbol_t, "," );
              ParseExpression( bottom_val, bottom_type );
              if baseTypesOk( bottom_type, pen_coordinate_t ) then
                 expect( symbol_t, ")" );
              end if;
           end if;
        end if;
     end if;
  end if;
  if isExecutingCommand then
     declare
       left_field_t, top_field_t, right_field_t, bottom_field_t : identifier;
     begin
       findField( record_ref.id, 1, left_field_t );
          identifiers( left_field_t ).value := left_val;
       findField( record_ref.id, 2, top_field_t );
          identifiers( top_field_t ).value := top_val;
       findField( record_ref.id, 3, right_field_t );
          identifiers( right_field_t ).value := right_val;
       findField( record_ref.id, 4, bottom_field_t );
          identifiers( bottom_field_t ).value := bottom_val;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenSetRect;

procedure ParsePenIsEmptyRect( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := pen.is_empty_rect( rec );
  -- Source: Pen.isEmptyRect
  record_id : identifier;
begin
  kind := boolean_t;
  expect( pen_is_empty_rect_t );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, pen_rect_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     declare
       pen_rect : aRect;
     begin
       bushRect2penRect( pen_rect, record_id );
       result := to_bush_boolean( isEmptyRect( pen_rect ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenIsEmptyRect;

procedure ParsePenOffsetRect is
  -- Syntax: pen.offset_rect( rect, dx, dy );
  -- Source: Pen.offsetRect
  record_id : identifier;
  dx_val, dy_val : unbounded_string;
  dx_type, dy_type : identifier;
begin
  expect( pen_offset_rect_t );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, pen_rect_t ) then
     expect( symbol_t, "," );
     ParseExpression( dx_val, dx_type );
     if baseTypesOk( dx_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( dy_val, dy_type );
        if baseTypesOk( dy_type, pen_coordinate_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  if isExecutingCommand then
     declare
       pen_rect : aRect;
     begin
       bushRect2penRect( pen_rect, record_id );
       offsetRect( pen_rect, aCoordinate( to_numeric( dx_val ) ),aCoordinate( to_numeric( dy_val ) ) );
       penRect2bushRect( pen_rect, record_id );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenOffsetRect;

procedure ParsePenInsetRect is
  -- Syntax: pen.inset_rect( rect, dx, dy );
  -- Source: Pen.insetRect
  record_id : identifier;
  dx_val, dy_val : unbounded_string;
  dx_type, dy_type : identifier;
begin
  expect( pen_inset_rect_t );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, pen_rect_t ) then
     expect( symbol_t, "," );
     ParseExpression( dx_val, dx_type );
     if baseTypesOk( dx_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( dy_val, dy_type );
        if baseTypesOk( dy_type, pen_coordinate_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  if isExecutingCommand then
     declare
       pen_rect : aRect;
     begin
       bushRect2penRect( pen_rect, record_id );
       insetRect( pen_rect, aCoordinate( to_numeric( dx_val ) ),aCoordinate( to_numeric( dy_val ) ) );
       penRect2bushRect( pen_rect, record_id );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenInsetRect;

procedure ParsePenIntersectRect is
  -- Syntax: pen.intersect_rect( rect, r1, r2 );
  -- Note: BUSH 1.0.2 cannot return record values so must use procedural version
  -- Source: Pen.intersectRect
  record_id : identifier;
  record1_id : identifier;
  record2_id : identifier;
begin
  expect( pen_intersect_rect_t );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, pen_rect_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( record1_id );
     if baseTypesOk( identifiers( record1_id ).kind, pen_rect_t ) then
        expect( symbol_t, "," );
        ParseIdentifier( record2_id );
        if baseTypesOk( identifiers( record2_id ).kind, pen_rect_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  if isExecutingCommand then
     declare
       pen_rect : aRect;
       pen_rect1 : aRect;
       pen_rect2 : aRect;
     begin
       bushRect2penRect( pen_rect1, record1_id );
       bushRect2penRect( pen_rect2, record2_id );
       intersectRect( pen_rect, pen_rect1, pen_rect2 );
       penRect2bushRect( pen_rect, record_id );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenIntersectRect;

procedure ParsePenInsideRect( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := pen.inside_rect( rect, rect2 );
  -- Source: Pen.insideRect
  record_id : identifier;
  record2_id : identifier;
  --x_val, y_val : unbounded_string;
  --x_type, y_type : identifier;
begin
  kind := boolean_t;
  expect( pen_inside_rect_t );
  expect( symbol_t, "(" );
  ParseIdentifier( record_id );
  if baseTypesOk( identifiers( record_id ).kind, pen_rect_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( record2_id );
     if baseTypesOk( identifiers( record_id ).kind, pen_rect_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  if isExecutingCommand then
     declare
       pen_rect : aRect;
       pen_rect2 : aRect;
     begin
       bushRect2penRect( pen_rect, record_id );
       bushRect2penRect( pen_rect2, record2_id );
       result := to_bush_boolean( insideRect( pen_rect, pen_rect2 ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenInsideRect;

procedure ParsePenInRect( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: b := pen.in_rect( rect, x, y );
  -- Source: Pen.inRect
  record_id : identifier;
  x_val, y_val : unbounded_string;
  x_type, y_type : identifier;
begin
  kind := boolean_t;
  expect( pen_in_rect_t );
  expect( symbol_t, "(" );
     ParseExpression( x_val, x_type );
     if baseTypesOk( x_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( y_val, y_type );
        if baseTypesOk( y_type, pen_coordinate_t ) then
           expect( symbol_t, "," );
           ParseIdentifier( record_id );
           if baseTypesOk( identifiers( record_id ).kind, pen_rect_t ) then
              expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  if isExecutingCommand then
     declare
       pen_rect : aRect;
     begin
       bushRect2penRect( pen_rect, record_id );
       result := to_bush_boolean( inRect( ACoordinate( to_numeric( x_val ) ), ACoordinate( to_numeric( y_val ) ), pen_rect ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenInRect;

procedure ParsePenSetPenMode is
  -- Syntax: pen.set_pen_mode( canvas_id, mode );
  -- Source: Pen.setPenMode
  canvas_id : identifier;
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_set_pen_mode_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( mode_val, mode_type );
     if baseTypesOk( mode_type, pen_pen_mode_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     begin
       setPenMode( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
          aPenMode'val( natural( to_numeric( mode_val ) ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenSetPenMode;

procedure ParsePenGetPenMode( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: mode := pen.get_pen_mode( canvas_id );
  -- Source: Pen.getPenMode
  canvas_id : identifier;
begin
  kind := pen_pen_mode_t;
  expect( pen_get_pen_mode_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     begin
       result := to_unbounded_string( aPenMode'pos( getPenMode(
          aCanvasID( to_numeric( identifiers( canvas_id ).value ) )) )'img );
       delete( result, 1, 1 );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenGetPenMode;

procedure ParsePenSetPenInk is
  -- Syntax: pen.set_pen_ink( canvas_id, r, g, b ) or pen.set_pen_ink( canvas_id, color_name );
  -- Source: Pen.setPenInk
  canvas_id : identifier;
  R_val,  G_val,  B_val  : unbounded_string;
  R_type, G_type, B_type : identifier;
  colorNameVersion : boolean := false;
begin
  expect( pen_set_pen_ink_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( R_val, R_type );
     if getBaseType( R_type ) = pen_pen_color_name_t then
        expect( symbol_t, ")" );
        colorNameVersion := true;
     elsif baseTypesOk( R_type, pen_rgbcomponent_t ) then
        expect( symbol_t, "," );
        ParseExpression( G_val, G_type );
        if baseTypesOk( G_type, pen_rgbcomponent_t ) then
           expect( symbol_t, "," );
           ParseExpression( B_val, B_type );
           if baseTypesOk( B_type, pen_rgbcomponent_t ) then
             expect( symbol_t, ")" );
           end if;
        end if;
     end if;
  end if;

  if isExecutingCommand then
     if colorNameVersion then
        begin
          setPenInk( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
             aColourName'val( integer( to_numeric( R_val ) ) ) );
        exception when others =>
           err( "exception raised" );
        end;
     else
        begin
          setPenInk( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
             aRGBComponent( to_numeric( R_val ) ), aRGBComponent( to_numeric( G_val ) ), aRGBComponent( to_numeric( B_val ) ) );
        exception when others =>
           err( "exception raised" );
        end;
     end if;
  end if;
end ParsePenSetPenInk;

procedure ParsePenGetPenInk is
  -- Syntax: pen.get_pen_ink( canvas_id, r, g, b );
  -- Source: Pen.getPenInk
  canvas_id : identifier;
  R_id, G_id, B_id : identifier;
begin
  expect( pen_get_pen_ink_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( R_id );
     if baseTypesOk( identifiers( R_id ).kind, pen_rgbcomponent_t ) then
        expect( symbol_t, "," );
        ParseIdentifier( G_id );
        if baseTypesOk( identifiers( G_id ).kind, pen_rgbcomponent_t ) then
           expect( symbol_t, "," );
           ParseIdentifier( B_id );
           if baseTypesOk( identifiers( B_id ).kind, pen_rgbcomponent_t ) then
             expect( symbol_t, ")" );
           end if;
        end if;
     end if;
  end if;

  if isExecutingCommand then
     declare
       R, G, B : aRGBComponent;
     begin
       getPenInk( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
       R, G, B );
       identifiers( R_id ).value := to_unbounded_string( R'img );
       identifiers( G_id ).value := to_unbounded_string( G'img );
       identifiers( B_id ).value := to_unbounded_string( B'img );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenGetPenInk;

procedure ParsePenSetPenBrush is
  -- Syntax: pen.set_pen_brush( canvas_id );
  -- Source: Pen.setPenBrush
  canvas_id : identifier;
  brush_val : unbounded_string;
  brush_type: identifier;
begin
  expect( pen_set_pen_brush_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( brush_val, brush_type );
     if baseTypesOk( brush_type, pen_pen_brush_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     begin
       setPenBrush( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
          aPenBrush'val( natural( to_numeric( brush_val ) ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenSetPenBrush;

procedure ParsePenGetPenBrush( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: brush := pen.get_pen_brush( canvas_id );
  -- Source: Pen.getPenBrush
  canvas_id : identifier;
begin
  kind := pen_pen_brush_t;
  expect( pen_get_pen_brush_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     begin
       result := to_unbounded_string( aPenBrush'pos( getPenBrush( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ) ) )'img );
       delete( result, 1, 1 );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenGetPenBrush;

procedure ParsePenSetPenPattern is
  -- Syntax: pen.set_pen_pattern( canvas_id, brush_canvas_id );
  -- Source: Pen.setPenPattern
  canvas_id : identifier;
  brush_id  : identifier;
begin
  expect( pen_set_pen_pattern_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( brush_id );
     if baseTypesOk( identifiers( brush_id ).kind, pen_canvas_id_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     begin
       setPenPattern( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
          aCanvasID( to_numeric( identifiers( brush_id ).value ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenSetPenPattern;

--procedure ParsePenGetPenPattern( result : out unbounded_string ) is
--  -- Syntax: pattern_canvas_id := pen.get_pen_pattern( canvas_id );
--  -- Source: Pen.getPenMode
--  canvas_id : identifier;
--begin
--  expect( pen_get_pen_pattern_t );
--  expect( symbol_t, "(" );
--  ParseIdentifier( canvas_id );
--  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
--     expect( symbol_t, ")" );
--  end if;
--
--  if isExecutingCommand then
--     begin
--       result := to_unbounded_string( aCanvasID'image( getPenPattern( aCanvasID( to_numeric( canvas_id ) ) ) ) );
--       delete( result, 1, 1 );
--     exception when others =>
--        err( "exception raised" );
--     end;
--  end if;
--end ParsePenGetPenPattern;

procedure ParsePenNewScreenCanvas is
  -- Syntax: pen.new_screen_canvas( H_Res, V_Res, C_Res, canvas_id );
  -- Source: Pen.newScreenCanvas
  h_val,  v_val,  c_val : unbounded_string;
  h_type, v_type, c_type : identifier;
  canvas_ref : reference;
  canvas_type : identifier := pen_canvas_id_t;
  C_Res : positive;
begin
  expect( pen_new_screen_canvas_t );
  --if inputMode = interactive or inputMode = breakout then
  --   err( "screen canvas is not allowed in an interactive session" );
  --end if;
  -- But what about when it is only option? ie. non-X session
  expect( symbol_t, "(" );
  ParseExpression( h_val, h_type );
  if baseTypesOk( h_type, positive_t ) then
     expect( symbol_t, "," );
     ParseExpression( v_val, v_type );
     if baseTypesOk( v_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( c_val, c_type );
        if baseTypesOk( c_type, positive_t ) then
           C_Res := positive( to_numeric( C_val ) );
           if C_Res /= 8 and C_Res /= 16 and C_Res /= 24 and C_Res /= 32 then
              err( "pixel bit resolution must be 8, 16, 24 or 32" );
           end if;
           expect( symbol_t, "," );
           ParseOutParameter( canvas_ref, canvas_type );
           if baseTypesOk( canvas_type, pen_canvas_id_t ) then
              expect( symbol_t, ")" );
           end if;
        end if;
     end if;
  end if;
  if isExecutingCommand then
     declare
        id : aCanvasID;
     begin
-- some versions of SDL take control of the screen if video is started so
-- we'll only start SDL when a canvas is created.
        if not penRunning then
           pen.startupPen;
           penRunning := true;
        end if;
        newScreenCanvas( positive( to_numeric( h_val ) ),
                         positive( to_numeric( v_val ) ),
                         C_Res, id );
        identifiers( canvas_ref.id ).value := to_unbounded_string( long_float( id ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenNewScreenCanvas;

procedure ParsePenNewGLScreenCanvas is
  -- Syntax: pen.new_gl_screen_canvas( H_Res, V_Res, C_Res, canvas_id );
  -- Source: Pen.newGLScreenCanvas
  h_val,  v_val,  c_val : unbounded_string;
  h_type, v_type, c_type : identifier;
  canvas_ref : reference;
  canvas_type : identifier := pen_canvas_id_t;
  C_Res : positive;
begin
  expect( pen_new_gl_screen_canvas_t );
  --if inputMode = interactive or inputMode = breakout then
  --   err( "screen canvas is not allowed in an interactive session" );
  --end if;
  -- But what about when it is only option? ie. non-X session
  expect( symbol_t, "(" );
  ParseExpression( h_val, h_type );
  if baseTypesOk( h_type, positive_t ) then
     expect( symbol_t, "," );
     ParseExpression( v_val, v_type );
     if baseTypesOk( v_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( c_val, c_type );
        if baseTypesOk( c_type, positive_t ) then
           C_Res := positive( to_numeric( C_val ) );
           if C_Res /= 8 and C_Res /= 16 and C_Res /= 24 and C_Res /= 32 then
              err( "pixel bit resolution must be 8, 16, 24 or 32" );
           end if;
           expect( symbol_t, "," );
           ParseOutParameter( canvas_ref, canvas_type );
           if baseTypesOk( canvas_type, pen_canvas_id_t ) then
              expect( symbol_t, ")" );
           end if;
        end if;
     end if;
  end if;
  if isExecutingCommand then
     declare
        id : aCanvasID;
     begin
-- some versions of SDL take control of the screen if video is started so
-- we'll only start SDL when a canvas is created.
        if not penRunning then
           pen.startupPen;
           penRunning := true;
        end if;
        newGLScreenCanvas( positive( to_numeric( h_val ) ),
                         positive( to_numeric( v_val ) ),
                         C_Res, id );
        identifiers( canvas_ref.id ).value := to_unbounded_string( long_float( id ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenNewGLScreenCanvas;

procedure ParsePenNewWindowCanvas is
  -- Syntax: pen.new_window_canvas( H_Res, V_Res, C_Res, canvas_id );
  -- Source: Pen.newWindowCanvas
  h_val,  v_val,  c_val : unbounded_string;
  h_type, v_type, c_type : identifier;
  canvas_ref : reference;
  canvas_type : identifier := pen_canvas_id_t;
  C_Res : positive;
begin
  expect( pen_new_window_canvas_t );
  expect( symbol_t, "(" );
  ParseExpression( h_val, h_type );
  if baseTypesOk( h_type, positive_t ) then
     expect( symbol_t, "," );
     ParseExpression( v_val, v_type );
     if baseTypesOk( v_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( c_val, c_type );
        if baseTypesOk( c_type, positive_t ) then
           C_Res := positive( to_numeric( C_val ) );
           if C_Res /= 8 and C_Res /= 16 and C_Res /= 24 and C_Res /= 32 then
              err( "pixel bit resolution must be 8, 16, 24 or 32" );
           end if;
           expect( symbol_t, "," );
           ParseOutParameter( canvas_ref, canvas_type );
           if baseTypesOk( canvas_type, pen_canvas_id_t ) then
              expect( symbol_t, ")" );
           end if;
        end if;
     end if;
  end if;
  if isExecutingCommand then
     declare
        id : aCanvasID;
     begin
-- some versions of SDL take control of the screen if video is started so
-- we'll only start SDL when a canvas is created.
        if not penRunning then
           pen.startupPen;
           penRunning := true;
        end if;
        newWindowCanvas( positive( to_numeric( h_val ) ),
                         positive( to_numeric( v_val ) ),
                         C_Res, id );
        identifiers( canvas_ref.id ).value := to_unbounded_string( long_float( id ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenNewWindowCanvas;

procedure ParsePenNewGlWindowCanvas is
  -- Syntax: pen.new_gl_window_canvas( H_Res, V_Res, C_Res, canvas_id );
  -- Source: Pen.newGLWindowCanvas
  h_val,  v_val,  c_val : unbounded_string;
  h_type, v_type, c_type : identifier;
  canvas_ref : reference;
  canvas_type : identifier := pen_canvas_id_t;
  C_Res : positive;
begin
  expect( pen_new_gl_window_canvas_t );
  expect( symbol_t, "(" );
  ParseExpression( h_val, h_type );
  if baseTypesOk( h_type, positive_t ) then
     expect( symbol_t, "," );
     ParseExpression( v_val, v_type );
     if baseTypesOk( v_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( c_val, c_type );
        if baseTypesOk( c_type, positive_t ) then
           C_Res := positive( to_numeric( C_val ) );
           if C_Res /= 8 and C_Res /= 16 and C_Res /= 24 and C_Res /= 32 then
              err( "pixel bit resolution must be 8, 16, 24 or 32" );
           end if;
           expect( symbol_t, "," );
           ParseOutParameter( canvas_ref, canvas_type );
           if baseTypesOk( canvas_type, pen_canvas_id_t ) then
              expect( symbol_t, ")" );
           end if;
        end if;
     end if;
  end if;
  if isExecutingCommand then
     declare
        id : aCanvasID;
     begin
-- some versions of SDL take control of the screen if video is started so
-- we'll only start SDL when a canvas is created.
        if not penRunning then
           pen.startupPen;
           penRunning := true;
        end if;
        newGLWindowCanvas( positive( to_numeric( h_val ) ),
                         positive( to_numeric( v_val ) ),
                         C_Res, id );
        identifiers( canvas_ref.id ).value := to_unbounded_string( long_float( id ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenNewGLWindowCanvas;

procedure ParsePenNewCanvas is
  -- Syntax: pen.new_canvas( H_Res, V_Res, old_canvas_id, canvas_id );
  --         pen.new_canvas( path, canvas_id );
  -- Source: Pen.newCanvas
  h_val,  v_val,  c_val : unbounded_string;
  h_type, v_type, c_type : identifier;
  canvas_ref : reference;
  canvas_type : identifier := pen_canvas_id_t;
  loading : boolean := false;
begin
  expect( pen_new_canvas_t );
  expect( symbol_t, "(" );
  ParseExpression( h_val, h_type );
  if h_type = uni_numeric_t or else getBaseType( h_type ) = positive_t then
     expect( symbol_t, "," );
     ParseExpression( v_val, v_type );
     if baseTypesOk( v_type, positive_t ) then
        expect( symbol_t, "," );
        ParseExpression( c_val, c_type );
        if baseTypesOk( c_type, pen_canvas_id_t ) then
           expect( symbol_t, "," );
           ParseOutParameter( canvas_ref, canvas_type );
           if baseTypesOk( canvas_type, pen_canvas_id_t ) then
              expect( symbol_t, ")" );
           end if;
        end if;
    end if;
  elsif getUniType( h_type ) = uni_string_t then
     loading := true;
     expect( symbol_t, "," );
     ParseOutParameter( canvas_ref, canvas_type );
     if baseTypesOk( canvas_type, pen_canvas_id_t ) then
        expect( symbol_t, ")" );
     end if;
  else
     err( "positive or string expected" );
  end if;
  if isExecutingCommand then
     declare
        id : aCanvasID;
        old_canvas_id : aCanvasID;
     begin
-- some versions of SDL take control of the screen if video is started so
-- we'll only start SDL when a canvas is created.
        if not penRunning then
           pen.startupPen;
           penRunning := true;
        end if;
        if loading then
           newCanvas( to_string( h_val ), id );
        else
           old_canvas_id := aCanvasId( to_numeric( C_val ) );
           newCanvas( positive( to_numeric( h_val ) ),
                         positive( to_numeric( v_val ) ),
                         old_canvas_id, id );
        end if;
        identifiers( canvas_ref.id ).value := to_unbounded_string( long_float( id ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenNewCanvas;

procedure ParsePenSaveCanvas is
  -- Syntax: pen.save_canvas( H_Res, V_Res, old_canvas_id, canvas_id );
  --         pen.save_canvas( path, canvas_id );
  -- Source: Pen.saveCanvas
  h_val       : unbounded_string;
  h_type      : identifier;
  canvas_id   : identifier;
begin
  if rshOpt then
     err( "not allowed in a " & optional_bold( "restricted shell" ) );
  end if;
  expect( pen_save_canvas_t );
  expect( symbol_t, "(" );
  ParseExpression( h_val, h_type );
  expect( symbol_t, "," );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
        saveCanvas( to_string( h_val ), aCanvasID( to_numeric( identifiers( canvas_id ).value ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenSaveCanvas;

procedure ParsePenMoveTo is
  -- Syntax: pen.move_to( canvas_id, dx, dy );
  -- Source: Pen.MoveTo
  canvas_id : identifier;
  x_val, y_val : unbounded_string;
  x_type, y_type : identifier;
begin
  expect( pen_move_to_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( x_val, x_type );
     if baseTypesOk( x_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( y_val, y_type );
        if baseTypesOk( y_type, pen_coordinate_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;

  if isExecutingCommand then
     begin
       moveTo( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
             aCoordinate( to_numeric( x_val ) ),
             aCoordinate( to_numeric( y_val ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenMoveTo;

procedure ParsePenMove is
  -- Syntax: pen.move( canvas_id, dx, dy );
  -- Source: Pen.Move
  canvas_id : identifier;
  dx_val, dy_val : unbounded_string;
  dx_type, dy_type : identifier;
begin
  expect( pen_move_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( dx_val, dx_type );
     if baseTypesOk( dx_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( dy_val, dy_type );
        if baseTypesOk( dy_type, pen_coordinate_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;

  if isExecutingCommand then
     begin
       move( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
             aCoordinate( to_numeric( dx_val ) ),
             aCoordinate( to_numeric( dy_val ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenMove;

procedure ParsePenLineTo is
  -- Syntax: pen.line_to( canvas_id, dx, dy );
  -- Source: Pen.LineTo
  canvas_id : identifier;
  x_val, y_val : unbounded_string;
  x_type, y_type : identifier;
begin
  expect( pen_line_to_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( x_val, x_type );
     if baseTypesOk( x_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( y_val, y_type );
        if baseTypesOk( y_type, pen_coordinate_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;

  if isExecutingCommand then
     begin
       lineTo( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
             aCoordinate( to_numeric( x_val ) ),
             aCoordinate( to_numeric( y_val ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenLineTo;

procedure ParsePenLine is
  -- Syntax: pen.line( canvas_id, dx, dy );
  -- Source: Pen.Line
  canvas_id : identifier;
  dx_val, dy_val : unbounded_string;
  dx_type, dy_type : identifier;
begin
  expect( pen_line_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( dx_val, dx_type );
     if baseTypesOk( dx_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( dy_val, dy_type );
        if baseTypesOk( dy_type, pen_coordinate_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;

  if isExecutingCommand then
     begin
       line( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
             aCoordinate( to_numeric( dx_val ) ),
             aCoordinate( to_numeric( dy_val ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenLine;

procedure ParsePenHLine is
  -- Syntax: pen.hline( canvas_id, x1, x2, y );
  -- Source: Pen.HLine
  canvas_id : identifier;
  x1_val, x2_val, y_val : unbounded_string;
  x1_type, x2_type, y_type : identifier;
begin
  expect( pen_hline_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( x1_val, x1_type );
     if baseTypesOk( x1_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( x2_val, x2_type );
        if baseTypesOk( x2_type, pen_coordinate_t ) then
           expect( symbol_t, "," );
           ParseExpression( y_val, y_type );
           if baseTypesOk( y_type, pen_coordinate_t ) then
              expect( symbol_t, ")" );
           end if;
        end if;
     end if;
  end if;

  if isExecutingCommand then
     begin
       hline( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
              aCoordinate( to_numeric( x1_val ) ),
              aCoordinate( to_numeric( x2_val ) ),
              aCoordinate( to_numeric( y_val ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenHLine;

procedure ParsePenVLine is
  -- Syntax: pen.vline( canvas_id, x, y1, y2 );
  -- Source: Pen.VLine
  canvas_id : identifier;
  x_val, y1_val, y2_val : unbounded_string;
  x_type, y1_type, y2_type : identifier;
begin
  expect( pen_vline_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( x_val, x_type );
     if baseTypesOk( x_type, pen_coordinate_t ) then
        expect( symbol_t, "," );
        ParseExpression( y1_val, y1_type );
        if baseTypesOk( y1_type, pen_coordinate_t ) then
           expect( symbol_t, "," );
           ParseExpression( y2_val, y2_type );
           if baseTypesOk( y2_type, pen_coordinate_t ) then
              expect( symbol_t, ")" );
           end if;
        end if;
     end if;
  end if;

  if isExecutingCommand then
     begin
       vline( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
              aCoordinate( to_numeric( x_val ) ),
              aCoordinate( to_numeric( y1_val ) ),
              aCoordinate( to_numeric( y2_val ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenVLine;

procedure ParsePenFrameRect is
  -- Syntax: pen.frame_rect( canvas_id, r );
  -- Source: Pen.FrameRect
  canvas_id : identifier;
  rect_id   : identifier;
begin
  expect( pen_frame_rect_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( rect_id );
     if baseTypesOk( identifiers( rect_id ).kind, pen_rect_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     declare
       pen_rect : aRect;
     begin
       bushRect2penRect( pen_rect, rect_id );
       frameRect( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
          pen_rect );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenFrameRect;

procedure ParsePenPaintRect is
  -- Syntax: pen.paint_rect( canvas_id, r );
  -- Source: Pen.PaintRect
  canvas_id : identifier;
  rect_id   : identifier;
begin
  expect( pen_paint_rect_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( rect_id );
     if baseTypesOk( identifiers( rect_id ).kind, pen_rect_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     declare
       pen_rect : aRect;
     begin
       bushRect2penRect( pen_rect, rect_id );
       paintRect( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
          pen_rect );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenPaintRect;

procedure ParsePenFillRect is
  -- Syntax: pen.fill_rect( canvas_id, rct, r, g, b );
  -- Syntax: pen.fill_rect( canvas_id, rct, color_name );
  -- Source: Pen.FillRect
  canvas_id : identifier;
  rect_id   : identifier;
  rexpr_val : unbounded_string;
  rexpr_type: identifier;
  gexpr_val : unbounded_string;
  gexpr_type: identifier;
  bexpr_val : unbounded_string;
  bexpr_type: identifier;
  colorNameVersion : boolean := false;
begin
  expect( pen_fill_rect_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( rect_id );
     if baseTypesOk( identifiers( rect_id ).kind, pen_rect_t ) then
        expect( symbol_t, "," );
        ParseExpression( rexpr_val, rexpr_type );
        if getBaseType( rexpr_type ) = pen_pen_color_name_t then
           expect( symbol_t, ")" );
           colorNameVersion := true;
        elsif baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
           expect( symbol_t, "," );
           ParseExpression( gexpr_val, gexpr_type );
           if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
              expect( symbol_t, "," );
              ParseExpression( bexpr_val, bexpr_type );
              if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
                 expect( symbol_t, ")" );
              end if;
           end if;
        end if;
     end if;
  end if;

  if isExecutingCommand then
     if colorNameVersion then
        declare
          pen_rect : aRect;
        begin
          bushRect2penRect( pen_rect, rect_id );
          fillRect( aCanvasID( to_numeric( canvas_id ) ),
            pen_rect,
            aColourName'val( integer( to_numeric( rexpr_val ) ) ) );
        exception when others =>
          err( "exception raised" );
        end;
     else
        declare
          pen_rect : aRect;
        begin
          bushRect2penRect( pen_rect, rect_id );
          fillRect( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
             pen_rect,
             aRGBComponent( to_numeric( rexpr_val ) ),
             aRGBComponent( to_numeric( gexpr_val ) ),
             aRGBComponent( to_numeric( bexpr_val ) ) );
        exception when others =>
          err( "exception raised" );
        end;
     end if;
  end if;
end ParsePenFillRect;

procedure ParsePenFrameEllipse is
  -- Syntax: pen.frame_ellipse( canvas_id, r );
  -- Source: Pen.FrameEllipse
  canvas_id : identifier;
  rect_id   : identifier;
begin
  expect( pen_frame_ellipse_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( rect_id );
     if baseTypesOk( identifiers( rect_id ).kind, pen_rect_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     declare
       pen_rect : aRect;
     begin
       bushRect2penRect( pen_rect, rect_id );
       frameEllipse( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
          pen_rect );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenFrameEllipse;

procedure ParsePenPaintEllipse is
  -- Syntax: pen.paint_ellipse( canvas_id, r );
  -- Source: Pen.PaintEllipse
  canvas_id : identifier;
  rect_id   : identifier;
begin
  expect( pen_paint_ellipse_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( rect_id );
     if baseTypesOk( identifiers( rect_id ).kind, pen_rect_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     declare
       pen_rect : aRect;
     begin
       bushRect2penRect( pen_rect, rect_id );
       paintEllipse( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
          pen_rect );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenPaintEllipse;

procedure ParsePenFillEllipse is
  -- Syntax: pen.fill_ellipse( canvas_id, r );
  -- Source: Pen.FillEllipse
  canvas_id : identifier;
  rect_id   : identifier;
  rexpr_val : unbounded_string;
  rexpr_type: identifier;
  gexpr_val : unbounded_string;
  gexpr_type: identifier;
  bexpr_val : unbounded_string;
  bexpr_type: identifier;
  colorNameVersion : boolean := false;
begin
  expect( pen_fill_ellipse_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( rect_id );
     if baseTypesOk( identifiers( rect_id ).kind, pen_rect_t ) then
        expect( symbol_t, "," );
        ParseExpression( rexpr_val, rexpr_type );
        if getBaseType( rexpr_type ) = pen_pen_color_name_t then
           expect( symbol_t, ")" );
           colorNameVersion := true;
        elsif baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
           expect( symbol_t, "," );
           ParseExpression( gexpr_val, gexpr_type );
           if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
              expect( symbol_t, "," );
              ParseExpression( bexpr_val, bexpr_type );
              if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
                 expect( symbol_t, ")" );
              end if;
           end if;
        end if;
     end if;
  end if;

  if isExecutingCommand then
     if colorNameVersion then
        declare
          pen_rect : aRect;
        begin
          bushRect2penRect( pen_rect, rect_id );
          fillEllipse( aCanvasID( to_numeric( canvas_id ) ),
            pen_rect,
            aColourName'val( integer( to_numeric( rexpr_val ) ) ) );
        exception when others =>
          err( "exception raised" );
        end;
     else
        declare
          pen_rect : aRect;
        begin
          bushRect2penRect( pen_rect, rect_id );
          fillEllipse( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
             pen_rect,
             aRGBComponent( to_numeric( rexpr_val ) ),
             aRGBComponent( to_numeric( gexpr_val ) ),
             aRGBComponent( to_numeric( bexpr_val ) ) );
        exception when others =>
          err( "exception raised" );
        end;
     end if;
  end if;
end ParsePenFillEllipse;

procedure ParsePenClear is
  -- Syntax: pen.clear( canvas_id );
  -- Syntax: pen.clear( canvas_id, r, g, b );
  -- Syntax: pen.clear( canvas_id, rct, color_name );
  -- Source: Pen.Clear
  canvas_id : identifier;
  rexpr_val : unbounded_string;
  rexpr_type: identifier;
  gexpr_val : unbounded_string;
  gexpr_type: identifier;
  bexpr_val : unbounded_string;
  bexpr_type: identifier;
  noParamVersion : boolean := false;
  colorNameVersion : boolean := false;
begin
  expect( pen_clear_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if token = symbol_t and identifiers( token ).value = "," then
     if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
        expect( symbol_t, "," );
        ParseExpression( rexpr_val, rexpr_type );
        if getBaseType( rexpr_type ) = pen_pen_color_name_t then
           expect( symbol_t, ")" );
           colorNameVersion := true;
        elsif baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
           expect( symbol_t, "," );
           ParseExpression( gexpr_val, gexpr_type );
           if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
              expect( symbol_t, "," );
              ParseExpression( bexpr_val, bexpr_type );
              if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
                 expect( symbol_t, ")" );
              end if;
           end if;
        end if;
     end if;
  else
     expect( symbol_t, ")" );
     noParamVersion := true;
  end if;

  if isExecutingCommand then
     if noParamVersion then
        begin
          clear( aCanvasID( to_numeric( canvas_id ) ) );
        exception when others =>
          err( "exception raised" );
        end;
     elsif colorNameVersion then
        begin
          clear( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
            aColourName'val( integer( to_numeric( rexpr_val ) ) ) );
        exception when others =>
          err( "exception raised" );
        end;
     else
        begin
          clear( aCanvasID( to_numeric( canvas_id ) ),
             aRGBComponent( to_numeric( rexpr_val ) ),
             aRGBComponent( to_numeric( gexpr_val ) ),
             aRGBComponent( to_numeric( bexpr_val ) ) );
        exception when others =>
          err( "exception raised" );
        end;
     end if;
  end if;
end ParsePenClear;

procedure ParsePenSetTitle is
  -- Syntax: pen.set_title( canvas_id, "title" );
  -- Source: Pen.SetTitle
  canvas_id : identifier;
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  expect( pen_set_title_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseExpression( expr_val, expr_type );
     if baseTypesOk( expr_type, uni_string_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     begin
       setTitle( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
         expr_val );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenSetTitle;

procedure ParsePenCloseCanvas is
  -- Syntax: pen.close_canvas( canvas_id );
  -- Source: Pen.FrameRect
  canvas_id : identifier;
begin
  expect( pen_close_canvas_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     begin
       closeCanvas( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ) );
       if canvasList.length( pen.canvas ) = 0 then
          pen.shutdownPen;
          penRunning := false;
       end if;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenCloseCanvas;

procedure ParsePenWaitToReveal is
  -- Syntax: pen.wait_to_reveal( canvas_id );
  -- Source: Pen.WaitToReveal
  canvas_id : identifier;
begin
  expect( pen_wait_to_reveal_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
        WaitToReveal( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenWaitToReveal;

procedure ParsePenReveal is
  -- Syntax: pen.reveal( canvas_id );
  -- Source: Pen.Reveal
  canvas_id : identifier;
begin
  expect( pen_reveal_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
        Reveal( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenReveal;

procedure ParsePenRevealNow is
  -- Syntax: pen.RevealNow( canvas_id );
  -- Source: Pen.RevealNow
  canvas_id : identifier;
begin
  expect( pen_reveal_now_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, ")" );
  end if;
  if isExecutingCommand then
     begin
        RevealNow( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenRevealNow;

procedure ParsePenGreyscale( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: rgb := pen.greyscale( r, g, b );
  -- Source: Pen.Greyscale
  rexpr_val : unbounded_string;
  rexpr_type: identifier;
  gexpr_val : unbounded_string;
  gexpr_type: identifier;
  bexpr_val : unbounded_string;
  bexpr_type: identifier;
begin
  kind := pen_rgbcomponent_t;
  expect( pen_greyscale_t );
  expect( symbol_t, "(" );
  ParseExpression( rexpr_val, rexpr_type );
  if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
     expect( symbol_t, "," );
     ParseExpression( gexpr_val, gexpr_type );
     if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
        expect( symbol_t, "," );
        ParseExpression( bexpr_val, bexpr_type );
        if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
            expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  if isExecutingCommand then
     begin
       result := to_unbounded_string(
                   aRGBComponent'image( greyscale(
                     aRGBComponent( to_numeric( rexpr_val ) ),
                     aRGBComponent( to_numeric( gexpr_val ) ),
                     aRGBComponent( to_numeric( bexpr_val ) )
                   )
                 ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenGreyscale;

procedure ParsePenBlend is
  -- Syntax: pen.blend( r1, g1, b1, r2, g2, b2, r, g, b );
  -- Source: Pen.Blend
  rexpr_val : unbounded_string;
  rexpr_type: identifier;
  gexpr_val : unbounded_string;
  gexpr_type: identifier;
  bexpr_val : unbounded_string;
  bexpr_type: identifier;
  rexpr2_val : unbounded_string;
  rexpr2_type: identifier;
  gexpr2_val : unbounded_string;
  gexpr2_type: identifier;
  bexpr2_val : unbounded_string;
  bexpr2_type: identifier;
  r3 : reference;
  g3 : reference;
  b3 : reference;
  blend_r : aRGBComponent;
  blend_g : aRGBComponent;
  blend_b : aRGBComponent;
begin
  expect( pen_blend_t );
  expect( symbol_t, "(" );
  ParseExpression( rexpr_val, rexpr_type );
  if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
     expect( symbol_t, "," );
     ParseExpression( gexpr_val, gexpr_type );
     if baseTypesOk( gexpr_type, pen_rgbcomponent_t ) then
        expect( symbol_t, "," );
        ParseExpression( bexpr_val, bexpr_type );
        if baseTypesOk( bexpr_type, pen_rgbcomponent_t ) then
           expect( symbol_t, "," );
           ParseExpression( rexpr2_val, rexpr2_type );
           if baseTypesOk( rexpr2_type, pen_rgbcomponent_t ) then
              expect( symbol_t, "," );
              ParseExpression( bexpr2_val, bexpr2_type );
              if baseTypesOk( bexpr2_type, pen_rgbcomponent_t ) then
                 expect( symbol_t, "," );
                 ParseExpression( gexpr2_val, gexpr2_type );
                 if baseTypesOk( gexpr2_type, pen_rgbcomponent_t ) then
                    expect( symbol_t, "," );
                    ParseOutParameter( r3, pen_rgbcomponent_t );
                    if baseTypesOk( r3.kind, pen_rgbcomponent_t ) then
                       expect( symbol_t, "," );
                       ParseOutParameter( g3, pen_rgbcomponent_t );
                       if baseTypesOk( g3.kind, pen_rgbcomponent_t ) then
                          expect( symbol_t, "," );
                          ParseOutParameter( b3, pen_rgbcomponent_t );
                          if baseTypesOk( b3.kind, pen_rgbcomponent_t ) then
                             expect( symbol_t, ")" );
                          end if;
                       end if;
                    end if;
                 end if;
              end if;
           end if;
        end if;
     end if;
  end if;
  if isExecutingCommand then
     begin
       blend(
          aRGBComponent( to_numeric( rexpr_val ) ),
          aRGBComponent( to_numeric( gexpr_val ) ),
          aRGBComponent( to_numeric( bexpr_val ) ),
          aRGBComponent( to_numeric( rexpr2_val ) ),
          aRGBComponent( to_numeric( gexpr2_val ) ),
          aRGBComponent( to_numeric( bexpr2_val ) ),
          blend_r,
          blend_g,
          blend_b
       );
       AssignParameter( r3, to_unbounded_string( long_float( blend_r ) ) );
       AssignParameter( g3, to_unbounded_string( long_float( blend_g ) ) );
       AssignParameter( b3, to_unbounded_string( long_float( blend_b ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenBlend;

procedure ParsePenFade is
  -- Syntax: pen.fade( r1, g1, b1, percent, r, g, b );
  -- Source: Pen.Fade
  rexpr_val : unbounded_string;
  rexpr_type: identifier;
  gexpr_val : unbounded_string;
  gexpr_type: identifier;
  bexpr_val : unbounded_string;
  bexpr_type: identifier;
  pexpr_val : unbounded_string;
  pexpr_type: identifier;
  r3 : reference;
  g3 : reference;
  b3 : reference;
  blend_r : aRGBComponent;
  blend_g : aRGBComponent;
  blend_b : aRGBComponent;
begin
  expect( pen_fade_t );
  expect( symbol_t, "(" );
  ParseExpression( rexpr_val, rexpr_type );
  if baseTypesOk( rexpr_type, pen_rgbcomponent_t ) then
     expect( symbol_t, "," );
     ParseExpression( gexpr_val, gexpr_type );
     if baseTypesOk( gexpr_type, pen_rgbcomponent_t ) then
        expect( symbol_t, "," );
        ParseExpression( bexpr_val, bexpr_type );
        if baseTypesOk( bexpr_type, pen_rgbcomponent_t ) then
           expect( symbol_t, "," );
           ParseExpression( pexpr_val, pexpr_type );
           if baseTypesOk( pexpr_type, float_t ) then
              expect( symbol_t, "," );
              ParseOutParameter( r3, pen_rgbcomponent_t );
              if baseTypesOk( r3.kind, pen_rgbcomponent_t ) then
                 expect( symbol_t, "," );
                 ParseOutParameter( g3, pen_rgbcomponent_t );
                 if baseTypesOk( g3.kind, pen_rgbcomponent_t ) then
                    expect( symbol_t, "," );
                    ParseOutParameter( b3, pen_rgbcomponent_t );
                    if baseTypesOk( b3.kind, pen_rgbcomponent_t ) then
                       expect( symbol_t, ")" );
                    end if;
                 end if;
              end if;
           end if;
        end if;
     end if;
  end if;
  if isExecutingCommand then
     begin
       fade(
          aRGBComponent( to_numeric( rexpr_val ) ),
          aRGBComponent( to_numeric( gexpr_val ) ),
          aRGBComponent( to_numeric( bexpr_val ) ),
          float( to_numeric( pexpr_val ) ),
          blend_r,
          blend_g,
          blend_b
       );
       AssignParameter( r3, to_unbounded_string( long_float( blend_r ) ) );
       AssignParameter( g3, to_unbounded_string( long_float( blend_g ) ) );
       AssignParameter( b3, to_unbounded_string( long_float( blend_b ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenFade;

procedure ParsePenPlot is
  -- Syntax: pen.plot( c, a );
  -- Source: Pen.Plot
-- THIS IS INCOMPLETE
  canvas_id : identifier;
  array_var_id : identifier;
  first, last : long_integer;
  -- array_id : arrayID;
begin
  expect( pen_plot_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( array_var_id );
     if not (class_ok( array_var_id, varClass ) and identifiers( array_var_id ).list) then
        err( "Array expected" );
     end if;
     if uniTypesOK( identifiers( array_var_id ).kind, uni_numeric_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  if isExecutingCommand then
     -- array_id := arrayID( to_numeric( identifiers( array_var_id ).value ) );
     first := identifiers( array_var_id ).avalue'first;
     last  := identifiers( array_var_id ).avalue'last;
     declare
        values : plotValues( first..last );
     begin
        -- copy values into an Ada array
        for i in first..last loop
            values(i) := to_numeric( identifiers( array_var_id ).avalue( i ) );
        end loop;
        Plot( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ), values );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenPlot;

procedure ParsePenSetFont is
  -- Syntax: pen.set_font( c, f, p );
  -- Source: Pen.SetFont
  canvas_id : identifier;
  font_val : unbounded_string;
  font_type: identifier;
  points_val : unbounded_string;
  points_type: identifier;
begin
  expect( pen_set_font_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     ParseExpression( font_val, font_type );
     if baseTypesOk( font_type, string_t ) then
        expect( symbol_t, "," );
        ParseExpression( points_val, points_type );
        if baseTypesOk( points_type, natural_t ) then
           expect( symbol_t, ")" );
        end if;
     end if;
  end if;
  if isExecutingCommand then
     begin
        setFont(
           aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
           font_val,
           natural( to_numeric( points_val ) ) );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenSetFont;

procedure ParsePenPut is
  -- Syntax: pen.put( c, s );
  -- Source: Pen.Put
  canvas_id : identifier;
  msg_val : unbounded_string;
  msg_type: identifier;
begin
  expect( pen_put_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     ParseExpression( msg_val, msg_type );
     if baseTypesOk( msg_type, natural_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  if isExecutingCommand then
     begin
        put(
           aCanvasID( to_numeric( identifiers( canvas_id ).value ) ),
           msg_val ) ;
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenPut;


-- OpenGL

procedure ParsePenglClearIndex is
  -- Syntax: glClearIndex( c : GLfloat );
  -- Source: bush_os.opengl.glClearIndex
  c_val  : unbounded_string;
  c_type : identifier;
begin
  expect( pen_glclearindex_t );
  ParseSingleNumericParameter( c_val, c_type, pen_glfloat_t ); -- c : GLfloat
  if isExecutingCommand then
    begin
      glClearIndex( GLfloat( to_numeric( c_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglClearIndex;

procedure ParsePenglClearColor is
  -- Syntax: glClearColor( red, green, blue, alpha: GLclampf );
  -- Source: bush_os.opengl.glClearColor
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glclearcolor_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glclampf_t ); -- red : GLclampf
  ParseNextNumericParameter( green_val, green_type, pen_glclampf_t ); -- green : GLclampf
  ParseNextNumericParameter( blue_val, blue_type, pen_glclampf_t ); -- blue : GLclampf
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glclampf_t ); -- alpha : GLclampf
  if isExecutingCommand then
    begin
      glClearColor( GLclampf( to_numeric( red_val ) ), GLclampf( to_numeric( green_val ) ), GLclampf( to_numeric( blue_val ) ), GLclampf( to_numeric( alpha_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglClearColor;

procedure ParsePenglClear is
  -- Syntax: glClear( mask: GLbitfield );
  -- Source: bush_os.opengl.glClear
  mask_val  : unbounded_string;
  mask_type : identifier;
begin
  expect( pen_glclear_t );
  ParseSingleNumericParameter( mask_val, mask_type, pen_glbitfield_t ); -- mask : GLbitfield
  if isExecutingCommand then
    begin
      glClear( GLbitfield( to_numeric( mask_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglClear;

procedure ParsePenglIndexMask is
  -- Syntax: glIndexMask( mask : GLuint );
  -- Source: bush_os.opengl.glIndexMask
  mask_val  : unbounded_string;
  mask_type : identifier;
begin
  expect( pen_glindexmask_t );
  ParseSingleNumericParameter( mask_val, mask_type, pen_gluint_t ); -- mask : GLuint
  if isExecutingCommand then
    begin
      glIndexMask( GLuint( to_numeric( mask_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexMask;

procedure ParsePenglColorMask is
  -- Syntax: glColorMask( red, green, blue, alpha : GLboolean );
  -- Source: bush_os.opengl.glColorMask
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolormask_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glboolean_t ); -- red : GLboolean
  ParseNextNumericParameter( green_val, green_type, pen_glboolean_t ); -- green : GLboolean
  ParseNextNumericParameter( blue_val, blue_type, pen_glboolean_t ); -- blue : GLboolean
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glboolean_t ); -- alpha : GLboolean
  if isExecutingCommand then
    begin
      glColorMask( GLboolean( to_numeric( red_val ) ), GLboolean( to_numeric( green_val ) ), GLboolean( to_numeric( blue_val ) ), GLboolean( to_numeric( alpha_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColorMask;

procedure ParsePenglAlphaFunc is
  -- Syntax: glAlphaFunc( func : GLalphacompare; ref : GLclampf );
  -- Source: bush_os.opengl.glAlphaFunc
  func_val  : unbounded_string;
  func_type : identifier;
  ref_val  : unbounded_string;
  ref_type : identifier;
begin
  expect( pen_glalphafunc_t );
  ParseFirstNumericParameter( func_val, func_type, pen_glalphacompare_t ); -- func : GLalphacompare
  ParseLastNumericParameter( ref_val, ref_type, pen_glclampf_t ); -- ref : GLclampf
  if isExecutingCommand then
    begin
      glAlphaFunc( GLalphacompare( to_numeric( func_val ) ), GLclampf( to_numeric( ref_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglAlphaFunc;

procedure ParsePenglBlendFunc is
  -- Syntax: glBlendFunc( sfactor, dfactor : GLblending );
  -- Source: bush_os.opengl.glBlendFunc
  sfactor_val  : unbounded_string;
  sfactor_type : identifier;
  dfactor_val  : unbounded_string;
  dfactor_type : identifier;
begin
  expect( pen_glblendfunc_t );
  ParseFirstNumericParameter( sfactor_val, sfactor_type, pen_glblending_t ); -- sfactor : GLblending
  ParseLastNumericParameter( dfactor_val, dfactor_type, pen_glblending_t ); -- dfactor : GLblending
  if isExecutingCommand then
    begin
      glBlendFunc( GLblending( to_numeric( sfactor_val ) ), GLblending( to_numeric( dfactor_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglBlendFunc;

procedure ParsePenglLogicOp is
  -- Syntax: glLogicOp( opcode : GLlogicops );
  -- Source: bush_os.opengl.glLogicOp
  opcode_val  : unbounded_string;
  opcode_type : identifier;
begin
  expect( pen_gllogicop_t );
  ParseSingleNumericParameter( opcode_val, opcode_type, pen_gllogicops_t ); -- opcode : GLlogicops
  if isExecutingCommand then
    begin
      glLogicOp( GLlogicops( to_numeric( opcode_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLogicOp;

procedure ParsePenglCullFace is
  -- Syntax: glCullFace( mode : GLbuffers );
  -- Source: bush_os.opengl.glCullFace
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glcullface_t );
  ParseSingleNumericParameter( mode_val, mode_type, pen_glbuffers_t ); -- mode : GLbuffers
  if isExecutingCommand then
    begin
      glCullFace( GLbuffers( to_numeric( mode_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCullFace;

procedure ParsePenglFrontFace is
  -- Syntax: glFrontFace( mode : GLpolygons );
  -- Source: bush_os.opengl.glFrontFace
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glfrontface_t );
  ParseSingleNumericParameter( mode_val, mode_type, pen_glpolygons_t ); -- mode : GLpolygons
  if isExecutingCommand then
    begin
      glFrontFace( GLpolygons( to_numeric( mode_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFrontFace;

procedure ParsePenglPointSize is
  -- Syntax: glPointSize( size : GLfloat );
  -- Source: bush_os.opengl.glPointSize
  size_val  : unbounded_string;
  size_type : identifier;
begin
  expect( pen_glpointsize_t );
  ParseSingleNumericParameter( size_val, size_type, pen_glfloat_t ); -- size : GLfloat
  if isExecutingCommand then
    begin
      glPointSize( GLfloat( to_numeric( size_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPointSize;

procedure ParsePenglLineWidth is
  -- Syntax: glLineWidth( width : GLfloat );
  -- Source: bush_os.opengl.glLineWidth
  width_val  : unbounded_string;
  width_type : identifier;
begin
  expect( pen_gllinewidth_t );
  ParseSingleNumericParameter( width_val, width_type, pen_glfloat_t ); -- width : GLfloat
  if isExecutingCommand then
    begin
      glLineWidth( GLfloat( to_numeric( width_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLineWidth;

procedure ParsePenglLineStipple is
  -- Syntax: glLineStipple( factor : GLint; pattern : GLushort );
  -- Source: bush_os.opengl.glLineStipple
  factor_val  : unbounded_string;
  factor_type : identifier;
  pattern_val  : unbounded_string;
  pattern_type : identifier;
begin
  expect( pen_gllinestipple_t );
  ParseFirstNumericParameter( factor_val, factor_type, pen_glint_t ); -- factor : GLint
  ParseLastNumericParameter( pattern_val, pattern_type, pen_glushort_t ); -- pattern : GLushort
  if isExecutingCommand then
    begin
      glLineStipple( GLint( to_numeric( factor_val ) ), GLushort( to_numeric( pattern_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLineStipple;

procedure ParsePenglPolygonMode is
  -- Syntax: glPolygonMode( face : GLbuffers; mode : GLpolygons );
  -- Source: bush_os.opengl.glPolygonMode
  face_val  : unbounded_string;
  face_type : identifier;
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glpolygonmode_t );
  ParseFirstNumericParameter( face_val, face_type, pen_glbuffers_t ); -- face : GLbuffers
  ParseLastNumericParameter( mode_val, mode_type, pen_glpolygons_t ); -- mode : GLpolygons
  if isExecutingCommand then
    begin
      glPolygonMode( GLbuffers( to_numeric( face_val ) ), GLpolygons( to_numeric( mode_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPolygonMode;

procedure ParsePenglPolygonOffset is
  -- Syntax: glPolygonOffset( factor : GLfloat; units : GLfloat );
  -- Source: bush_os.opengl.glPolygonOffset
  factor_val  : unbounded_string;
  factor_type : identifier;
  units_val  : unbounded_string;
  units_type : identifier;
begin
  expect( pen_glpolygonoffset_t );
  ParseFirstNumericParameter( factor_val, factor_type, pen_glfloat_t ); -- factor : GLfloat
  ParseLastNumericParameter( units_val, units_type, pen_glfloat_t ); -- units : GLfloat
  if isExecutingCommand then
    begin
      glPolygonOffset( GLfloat( to_numeric( factor_val ) ), GLfloat( to_numeric( units_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPolygonOffset;

procedure ParsePenglPolygonStipple is
  -- Syntax: glPolygonStipple( mask : in out GLubyte );
  -- Source: bush_os.opengl.glPolygonStipple
  mask_id : identifier;
begin
  expect( pen_glpolygonstipple_t );
  ParseSingleInOutParameter( mask_id, pen_glubyte_t ); -- mask : GLubyte
  if isExecutingCommand then
    declare
      mask : GLUbyte := GLubyte( to_numeric( identifiers( mask_id ).value ) );
    begin
      glPolygonStipple( mask );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPolygonStipple;

procedure ParsePenglGetPolygonStipple is
  -- Syntax: glGetPolygonStipple( mask : GLubyte );
  -- Source: bush_os.opengl.glGetPolygonStipple
  mask_val  : unbounded_string;
  mask_type : identifier;
begin
  expect( pen_glgetpolygonstipple_t );
  ParseSingleNumericParameter( mask_val, mask_type, pen_glubyte_t ); -- mask : GLubyte
  if isExecutingCommand then
    begin
      glGetPolygonStipple( GLubyte( to_numeric( mask_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetPolygonStipple;

procedure ParsePenglEdgeFlag is
  -- Syntax: glEdgeFlag( flag : GLboolean );
  -- Source: bush_os.opengl.glEdgeFlag
  flag_val  : unbounded_string;
  flag_type : identifier;
begin
  expect( pen_gledgeflag_t );
  ParseSingleNumericParameter( flag_val, flag_type, pen_glboolean_t ); -- flag : GLboolean
  if isExecutingCommand then
    begin
      glEdgeFlag( GLboolean( to_numeric( flag_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEdgeFlag;

procedure ParsePenglEdgeFlagv is
  -- Syntax: glEdgeFlagv( flag : in out GLboolean );
  -- Source: bush_os.opengl.glEdgeFlagv
  flag_id : identifier;
begin
  expect( pen_gledgeflagv_t );
  ParseSingleInOutParameter( flag_id, pen_glboolean_t ); -- flag : GLboolean
  if isExecutingCommand then
    declare
      flag : GLboolean := GLboolean( to_numeric( identifiers( flag_id ).value ) );
    begin
      glEdgeFlagv( flag );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEdgeFlagv;

procedure ParsePenglScissor is
  -- Syntax: glScissor( x, y : GLint; width, height : GLsizei );
  -- Source: bush_os.opengl.glScissor
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
begin
  expect( pen_glscissor_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseLastNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  if isExecutingCommand then
    begin
      glScissor( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglScissor;

-- TODO: to return array
--procedure ParsePenglClipPlane is
--  -- Syntax: glClipPlane( plane : GLusercplane; equation : in out GLdouble );
--  -- Source: bush_os.opengl.glClipPlane
--  plane_val  : unbounded_string;
--  plane_type : identifier;
--  equation_val  : unbounded_string;
--  equation_type : identifier;
--begin
--  expect( pen_glclipplane_t );
--  ParseFirstNumericParameter( plane_val, plane_type, pen_glusercplane_t ); -- plane : GLusercplane
--  ParseLastInOutNumericParameter( equation_val, equation_type, pen_gldouble_t ); -- equation : GLdouble
--  if isExecutingCommand then
--    declare
--      equation : double_vertex_4d;
--    begin
--      glClipPlane( GLusercplane( to_numeric( plane_val ) ), equation( equation'first )'address );
--    exception when others =>
--      err( "exception raised" );
--    end;
--  end if;
--end ParsePenglClipPlane;

-- TODO: to return array
--procedure ParsePenglGetClipPlane is
--  -- Syntax: glGetClipPlane( plane : GLusercplane; equation : in out GLdouble );
--  -- Source: bush_os.opengl.glGetClipPlane
--  plane_val  : unbounded_string;
--  plane_type : identifier;
--  equation_val  : unbounded_string;
--  equation_type : identifier;
--begin
--  expect( pen_glgetclipplane_t );
--  ParseFirstNumericParameter( plane_val, plane_type, pen_glusercplane_t ); -- plane : GLusercplane
--  ParseLastNumericParameter( equation_val, equation_type, pen_gldouble_t ); -- equation : GLdouble
--  if isExecutingCommand then
--    begin
--      glGetClipPlane( GLusercplane( to_numeric( plane_val ) ), GLdouble( to_numeric( equation_val ) ) );
--    exception when others =>
--      err( "exception raised" );
--    end;
--  end if;
--end ParsePenglGetClipPlane;

procedure ParsePenglDrawBuffer is
  -- Syntax: glDrawBuffer( mode : GLbuffers );
  -- Source: bush_os.opengl.glDrawBuffer
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_gldrawbuffer_t );
  ParseSingleNumericParameter( mode_val, mode_type, pen_glbuffers_t ); -- mode : GLbuffers
  if isExecutingCommand then
    begin
      glDrawBuffer( GLbuffers( to_numeric( mode_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDrawBuffer;

procedure ParsePenglReadBuffer is
  -- Syntax: glReadBuffer( mode : GLbuffers );
  -- Source: bush_os.opengl.glReadBuffer
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glreadbuffer_t );
  ParseSingleNumericParameter( mode_val, mode_type, pen_glbuffers_t ); -- mode : GLbuffers
  if isExecutingCommand then
    begin
      glReadBuffer( GLbuffers( to_numeric( mode_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglReadBuffer;

procedure ParsePenglEnable is
  -- Syntax: glEnable( cap : GLenum ); -- left as GLenum
  --         glEnable( cap : GLdbuffer );
  --         glEnable( cap : GLblending );
  -- Source: bush_os.opengl.glEnable
  cap_val  : unbounded_string;
  cap_type : identifier;
  baseType : identifier;
begin
  expect( pen_glenable_t );
  ParseSingleNumericParameter( cap_val, cap_type );
  baseType := getBaseType( cap_type );
  if baseType = pen_gldbuffer_t then
     null;
  elsif baseType = pen_glblending_t then
     null;
  elsif baseType /= pen_glenum_t then
     err( "expected type of pen.glenum, pen.glblending, pen.gldbuffer" ); --TODO
  end if;
  if isExecutingCommand then
    begin
      glEnable( GLenum( to_numeric( cap_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEnable;

procedure ParsePenglDisable is
  -- Syntax: glDisable( cap : GLenum ); -- left as GLenum
  -- Source: bush_os.opengl.glDisable
  cap_val  : unbounded_string;
  cap_type : identifier;
begin
  expect( pen_gldisable_t );
  ParseSingleNumericParameter( cap_val, cap_type, pen_glenum_t ); -- cap : GLenum
  if isExecutingCommand then
    begin
      glDisable( GLenum( to_numeric( cap_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDisable;

procedure ParsePenglIsEnabled( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: glIsEnabled( cap : GLenum ) return GLboolean; -- left as GLenum
  -- Source: bush_os.opengl.glIsEnabled
  cap_val  : unbounded_string;
  cap_type : identifier;
begin
  kind := boolean_t;
  expect( pen_glisenabled_t );
  ParseSingleNumericParameter( cap_val, cap_type, pen_glenum_t ); -- cap : GLenum
  if isExecutingCommand then
    begin
      result := to_unbounded_string( long_float( glIsEnabled( GLenum( to_numeric( cap_val ) ) ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIsEnabled;

procedure ParsePenglEnableClientState is
  -- Syntax: glEnableClientState( cap : GLenum ); -- /* 1.1 */
  -- Source: bush_os.opengl.glEnableClientState
  cap_val  : unbounded_string;
  cap_type : identifier;
begin
  expect( pen_glenableclientstate_t );

  ParseSingleNumericParameter( cap_val, cap_type, pen_glenum_t ); -- cap : GLenum
  if isExecutingCommand then
    begin
      glEnableClientState( GLenum( to_numeric( cap_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEnableClientState;

procedure ParsePenglDisableClientState is
  -- Syntax: glDisableClientState( cap : GLenum ); -- /* 1.1 */
  -- Source: bush_os.opengl.glDisableClientState
  cap_val  : unbounded_string;
  cap_type : identifier;
begin
  expect( pen_gldisableclientstate_t );
  ParseSingleNumericParameter( cap_val, cap_type, pen_glenum_t ); -- cap : GLenum
  if isExecutingCommand then
    begin
      glDisableClientState( GLenum( to_numeric( cap_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDisableClientState;

-- TODO: this is a buffer overflow risk: the return size depends on the first param
--procedure ParsePenglGetBooleanv is
--  -- Syntax: glGetBooleanv( pname : GLenum; params : in out GLboolean );
--  -- Source: bush_os.opengl.glGetBooleanv
--  pname_val  : unbounded_string;
--  pname_type : identifier;
--  params_id  : unbounded_string;
--begin
--  expect( pen_glgetbooleanv_t );
--  ParseFirstNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
--  ParseLastInOutParameter( params_id, pen_glboolean_t ); -- params : GLboolean
--  if isExecutingCommand then
--    declare
--      params : GLboolean;
--    begin
--      glGetBooleanv( GLenum( to_numeric( pname_val ) ), params );
--    exception when others =>
--      err( "exception raised" );
--    end;
--  end if;
--end ParsePenglGetBooleanv;

-- TODO: this is a buffer overflow risk: the return size depends on the first param
--procedure ParsePenglGetDoublev is
--  -- Syntax: glGetDoublev( pname : GLenum; params : in out GLdouble );
--  -- Source: bush_os.opengl.glGetDoublev
--  pname_val  : unbounded_string;
--  pname_type : identifier;
--  params_val  : unbounded_string;
--  params_type : identifier;
--begin
--  expect( pen_glgetdoublev_t );
--  ParseFirstNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
--  ParseLastNumericParameter( params_val, params_type, pen_gldouble_t ); -- params : GLdouble
--  if isExecutingCommand then
--    begin
--      glGetDoublev( GLenum( to_numeric( pname_val ) ), GLdouble( to_numeric( params_val ) ) );
--    exception when others =>
--      err( "exception raised" );
--    end;
--  end if;
--end ParsePenglGetDoublev;

-- TODO: this is a buffer overflow risk: the return size depends on the first param
--procedure ParsePenglGetFloatv is
--  -- Syntax: glGetFloatv( pname : GLenum; params : in out GLfloat );
--  -- Source: bush_os.opengl.glGetFloatv
--  pname_val  : unbounded_string;
--  pname_type : identifier;
--  params_val  : unbounded_string;
--  params_type : identifier;
--begin
--  expect( pen_glgetfloatv_t );
--  ParseFirstNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
--  ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
--  if isExecutingCommand then
--    begin
--      glGetFloatv( GLenum( to_numeric( pname_val ) ), GLfloat( to_numeric( params_val ) ) );
--    exception when others =>
--      err( "exception raised" );
--    end;
--  end if;
--end ParsePenglGetFloatv;

-- TODO: this is a buffer overflow risk: the return size depends on the first param
--procedure ParsePenglGetIntegerv is
--  -- Syntax: glGetIntegerv( pname : GLenum; params : in out GLint );
--  -- Source: bush_os.opengl.glGetIntegerv
--  pname_val  : unbounded_string;
--  pname_type : identifier;
--  params_val  : unbounded_string;
--  params_type : identifier;
--begin
--  expect( pen_glgetintegerv_t );
--  ParseFirstNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
--  ParseLastNumericParameter( params_val, params_type, pen_glint_t ); -- params : GLint
--  if isExecutingCommand then
--    begin
--      glGetIntegerv( GLenum( to_numeric( pname_val ) ), GLint( to_numeric( params_val ) ) );
--    exception when others =>
--      err( "exception raised" );
--    end;
--  end if;
--end ParsePenglGetIntegerv;

procedure ParsePenglPushAttrib is
  -- Syntax: glPushAttrib( mask : GLbitfield );
  -- Source: bush_os.opengl.glPushAttrib
  mask_val  : unbounded_string;
  mask_type : identifier;
begin
  expect( pen_glpushattrib_t );
  ParseSingleNumericParameter( mask_val, mask_type, pen_glbitfield_t ); -- mask : GLbitfield
  if isExecutingCommand then
    begin
      glPushAttrib( GLbitfield( to_numeric( mask_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPushAttrib;

procedure ParsePenglPopAttrib is
  -- Syntax: glPopAttrib;
  -- Source: bush_os.opengl.glPopAttrib
begin
  expect( pen_glpopattrib_t );
  if isExecutingCommand then
    begin
      glPopAttrib;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPopAttrib;

procedure ParsePenglPushClientAttrib is
  -- Syntax: glPushClientAttrib( mask : GLbitfield ); -- /* 1.1 */
  -- Source: bush_os.opengl.glPushClientAttrib
  mask_val  : unbounded_string;
  mask_type : identifier;
begin
  expect( pen_glpushclientattrib_t );
  ParseSingleNumericParameter( mask_val, mask_type, pen_glbitfield_t ); -- mask : GLbitfield
  if isExecutingCommand then
    begin
      glPushClientAttrib( GLbitfield( to_numeric( mask_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPushClientAttrib;

procedure ParsePenglPopClientAttrib is
  -- Syntax: glPopClientAttrib; -- /* 1.1 */
  -- Source: bush_os.opengl.glPopClientAttrib
begin
  expect( pen_glpopclientattrib_t );
  if isExecutingCommand then
    begin
      glPopClientAttrib;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPopClientAttrib;

procedure ParsePenglRenderMode( result : out unbounded_string ) is
  -- Syntax: glRenderMode( mode : GLrendermodes ) return GLint;
  -- Source: bush_os.opengl.glRenderMode
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glrendermode_t );
  ParseSingleNumericParameter( mode_val, mode_type, pen_glrendermodes_t ); -- mode : GLrendermodes
  if isExecutingCommand then
    begin
      result := to_unbounded_string( long_float( glRenderMode( GLrendermodes( to_numeric( mode_val ) ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRenderMode;

procedure ParsePenglGetError( result : out unbounded_string; kind : out identifier ) is
  -- Syntax: glGetError return GLenum;
  -- Source: bush_os.opengl.glGetError
begin
  kind := pen_glerrors_t;
  expect( pen_glgeterror_t );
  if isExecutingCommand then
    declare
      errNum : GLerrors := glGetError;
    begin
      result := to_unbounded_string( long_float( errNum ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetError;

procedure ParsePenglGetString( result : out unbounded_string ) is
  -- Syntax: glGetString( name : GLenum ) return system.address; -- GLUbyte*
  -- Source: bush_os.opengl.glGetString
  name_val  : unbounded_string;
  name_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgetstring_t );
  ParseSingleNumericParameter( name_val, name_type, pen_glenum_t ); -- name : GLenum
  if isExecutingCommand then
    declare
      c_string_loc : system.address;
    begin
      c_string_loc := glGetString( GLenum( to_numeric( name_val ) ) );
      result := null_unbounded_string; -- TODO: convert C string to ada string
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetString;

procedure ParsePenglFinish is
  -- Syntax: glFinish;
  -- Source: bush_os.opengl.glFinish
begin
  expect( pen_glfinish_t );
  if isExecutingCommand then
    begin
      glFinish;
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFinish;

procedure ParsePenglFlush is
  -- Syntax: glFlush;
  -- Source: bush_os.opengl.glFlush
begin
  expect( pen_glflush_t );
  if isExecutingCommand then
    begin
      glFlush;
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFlush;

procedure ParsePenglHint is
  -- Syntax: glHint( target : GLhints; mode : GLhintmodes );
  -- Source: bush_os.opengl.glHint
  target_val  : unbounded_string;
  target_type : identifier;
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glhint_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glhints_t ); -- target : GLhints
  ParseLastNumericParameter( mode_val, mode_type, pen_glhintmodes_t ); -- mode : GLhintmodes
  if isExecutingCommand then
    begin
      glHint( GLhints( to_numeric( target_val ) ), GLhintmodes( to_numeric( mode_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglHint;

procedure ParsePenglClearDepth is
  -- Syntax: glClearDepth( depth : GLclampd );
  -- Source: bush_os.opengl.glClearDepth
  depth_val  : unbounded_string;
  depth_type : identifier;
begin
  expect( pen_glcleardepth_t );
  ParseSingleNumericParameter( depth_val, depth_type, pen_glclampd_t ); -- depth : GLclampd
  if isExecutingCommand then
    begin
      glClearDepth( GLclampd( to_numeric( depth_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglClearDepth;

procedure ParsePenglDepthFunc is
  -- Syntax: glDepthFunc( func : GLalphacompare );
  -- Source: bush_os.opengl.glDepthFunc
  func_val  : unbounded_string;
  func_type : identifier;
begin
  expect( pen_gldepthfunc_t );
  ParseSingleNumericParameter( func_val, func_type, pen_glalphacompare_t ); -- func : GLalphacompare
  if isExecutingCommand then
    begin
      glDepthFunc( GLalphacompare( to_numeric( func_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDepthFunc;

procedure ParsePenglDepthMask is
  -- Syntax: glDepthMask( flag : GLboolean );
  -- Source: bush_os.opengl.glDepthMask
  flag_val  : unbounded_string;
  flag_type : identifier;
begin
  expect( pen_gldepthmask_t );
  ParseSingleNumericParameter( flag_val, flag_type, pen_glboolean_t ); -- flag : GLboolean
  if isExecutingCommand then
    begin
      glDepthMask( GLboolean( to_numeric( flag_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDepthMask;

procedure ParsePenglDepthRange is
  -- Syntax: glDepthRange( near_val, far_val : GLclampd );
  -- Source: bush_os.opengl.glDepthRange
  near_val_val  : unbounded_string;
  near_val_type : identifier;
  far_val_val  : unbounded_string;
  far_val_type : identifier;
begin
  expect( pen_gldepthrange_t );
  ParseFirstNumericParameter( near_val_val, near_val_type, pen_glclampd_t ); -- near_val : GLclampd
  ParseLastNumericParameter( far_val_val, far_val_type, pen_glclampd_t ); -- far_val : GLclampd
  if isExecutingCommand then
    begin
      glDepthRange( GLclampd( to_numeric( near_val_val ) ), GLclampd( to_numeric( far_val_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDepthRange;

procedure ParsePenglClearAccum is
  -- Syntax: glClearAccum( red, green, blue, alpha : GLfloat );
  -- Source: bush_os.opengl.glClearAccum
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glclearaccum_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glfloat_t ); -- red : GLfloat
  ParseNextNumericParameter( green_val, green_type, pen_glfloat_t ); -- green : GLfloat
  ParseNextNumericParameter( blue_val, blue_type, pen_glfloat_t ); -- blue : GLfloat
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glfloat_t ); -- alpha : GLfloat
  if isExecutingCommand then
    begin
      glClearAccum( GLfloat( to_numeric( red_val ) ), GLfloat( to_numeric( green_val ) ), GLfloat( to_numeric( blue_val ) ), GLfloat( to_numeric( alpha_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglClearAccum;

procedure ParsePenglAccum is
  -- Syntax: glAccum( op : GLenum; value : GLfloat );
  -- Source: bush_os.opengl.glAccum
  op_val  : unbounded_string;
  op_type : identifier;
  value_val  : unbounded_string;
  value_type : identifier;
begin
  expect( pen_glaccum_t );
  ParseFirstNumericParameter( op_val, op_type, pen_glenum_t ); -- op : GLenum
  ParseLastNumericParameter( value_val, value_type, pen_glfloat_t ); -- value : GLfloat
  if isExecutingCommand then
    begin
      glAccum( GLenum( to_numeric( op_val ) ), GLfloat( to_numeric( value_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglAccum;

procedure ParsePenglMatrixMode is
  -- Syntax: glMatrixMode( mode : GLmodes );
  -- Source: bush_os.opengl.glMatrixMode
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glmatrixmode_t );
  ParseSingleNumericParameter( mode_val, mode_type, pen_glmodes_t ); -- mode : GLmodes
  if isExecutingCommand then
    begin
      glMatrixMode( GLmodes( to_numeric( mode_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMatrixMode;

procedure ParsePenglOrtho is
  -- Syntax: glOrtho( left, right, bottom, top, near_val, far_val : GLdouble );
  -- Source: bush_os.opengl.glOrtho
  left_val  : unbounded_string;
  left_type : identifier;
  right_val  : unbounded_string;
  right_type : identifier;
  bottom_val  : unbounded_string;
  bottom_type : identifier;
  top_val  : unbounded_string;
  top_type : identifier;
  near_val_val  : unbounded_string;
  near_val_type : identifier;
  far_val_val  : unbounded_string;
  far_val_type : identifier;
begin
  expect( pen_glortho_t );
  ParseFirstNumericParameter( left_val, left_type, pen_gldouble_t ); -- left : GLdouble
  ParseNextNumericParameter( right_val, right_type, pen_gldouble_t ); -- right : GLdouble
  ParseNextNumericParameter( bottom_val, bottom_type, pen_gldouble_t ); -- bottom : GLdouble
  ParseNextNumericParameter( top_val, top_type, pen_gldouble_t ); -- top : GLdouble
  ParseNextNumericParameter( near_val_val, near_val_type, pen_gldouble_t ); -- near_val : GLdouble
  ParseLastNumericParameter( far_val_val, far_val_type, pen_gldouble_t ); -- far_val : GLdouble
  if isExecutingCommand then
    begin
      glOrtho( GLdouble( to_numeric( left_val ) ), GLdouble( to_numeric( right_val ) ), GLdouble( to_numeric( bottom_val ) ), GLdouble( to_numeric( top_val ) ), GLdouble( to_numeric( near_val_val ) ), GLdouble( to_numeric( far_val_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglOrtho;

procedure ParsePenglFrustum is
  -- Syntax: glFrustum( left, right, bottom, top, near_val, far_val : GLdouble );
  -- Source: bush_os.opengl.glFrustum
  left_val  : unbounded_string;
  left_type : identifier;
  right_val  : unbounded_string;
  right_type : identifier;
  bottom_val  : unbounded_string;
  bottom_type : identifier;
  top_val  : unbounded_string;
  top_type : identifier;
  near_val_val  : unbounded_string;
  near_val_type : identifier;
  far_val_val  : unbounded_string;
  far_val_type : identifier;
begin
  expect( pen_glfrustum_t );
  ParseFirstNumericParameter( left_val, left_type, pen_gldouble_t ); -- left : GLdouble
  ParseNextNumericParameter( right_val, right_type, pen_gldouble_t ); -- right : GLdouble
  ParseNextNumericParameter( bottom_val, bottom_type, pen_gldouble_t ); -- bottom : GLdouble
  ParseNextNumericParameter( top_val, top_type, pen_gldouble_t ); -- top : GLdouble
  ParseNextNumericParameter( near_val_val, near_val_type, pen_gldouble_t ); -- near_val : GLdouble
  ParseLastNumericParameter( far_val_val, far_val_type, pen_gldouble_t ); -- far_val : GLdouble
  if isExecutingCommand then
    begin
      glFrustum( GLdouble( to_numeric( left_val ) ), GLdouble( to_numeric( right_val ) ), GLdouble( to_numeric( bottom_val ) ), GLdouble( to_numeric( top_val ) ), GLdouble( to_numeric( near_val_val ) ), GLdouble( to_numeric( far_val_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFrustum;

procedure ParsePenglViewport is
  -- Syntax: glViewport( x, y : GLint; width, height : GLsizei );
  -- Source: bush_os.opengl.glViewport
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
begin
  expect( pen_glviewport_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseLastNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  if isExecutingCommand then
    begin
      glViewport( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglViewport;

procedure ParsePenglPushMatrix is
  -- Syntax: glPushMatrix;
  -- Source: bush_os.opengl.glPushMatrix
begin
  expect( pen_glpushmatrix_t );
  if isExecutingCommand then
    begin
      glPushMatrix;
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPushMatrix;

procedure ParsePenglPopMatrix is
  -- Syntax: glPopMatrix;
  -- Source: bush_os.opengl.glPopMatrix
begin
  expect( pen_glpopmatrix_t );
  if isExecutingCommand then
    begin
      glPopMatrix;
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPopMatrix;

procedure ParsePenglLoadIdentity is
  -- Syntax: glLoadIdentity;
  -- Source: bush_os.opengl.glLoadIdentity
begin
  expect( pen_glloadidentity_t );
  if isExecutingCommand then
    begin
      glLoadIdentity;
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLoadIdentity;

procedure ParsePenglLoadMatrixd is
  -- Syntax: glLoadMatrixd( m : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glLoadMatrixd
  m_val  : unbounded_string;
  -- m_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glloadmatrixd_t );
  -- TODO: pen_gl_double_array_matrix_t must be a 16 value sparforte array
  --ParseSingleNumericParameter( m_val, m_type, pen_gl_double_array_matrix_t ); -- m : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..15 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glLoadMatrixd( param_ptr );
      free( param_array );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLoadMatrixd;

procedure ParsePenglLoadMatrixf is
  -- Syntax: glLoadMatrixf( m : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glLoadMatrixf
  m_val  : unbounded_string;
--  m_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glloadmatrixf_t );
  -- TODO: pen_gl_float_array_matrix_t must be a 16 value sparforte array
  --ParseSingleNumericParameter( m_val, m_type, pen_gl_double_array_matrix_t ); -- m : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..15 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glLoadMatrixf( param_ptr );
      free( param_array );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLoadMatrixf;

procedure ParsePenglMultMatrixd is
  -- Syntax: glMultMatrixd( m : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glMultMatrixd
  m_val  : unbounded_string;
--  m_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultmatrixd_t );
  -- TODO: pen_gl_double_array_matrix_t must be a 16 value sparforte array
  --ParseSingleNumericParameter( m_val, m_type, pen_gl_double_array_matrix_t ); -- m : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..15 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glMultMatrixd( param_ptr );
      free( param_array );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultMatrixd;

procedure ParsePenglMultMatrixf is
  -- Syntax: glMultMatrixf( m : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glMultMatrixf
  m_val  : unbounded_string;
--  m_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultmatrixf_t );
  -- TODO: pen_gl_float_array_matrix_t must be a 16 value sparforte array
  --ParseSingleNumericParameter( m_val, m_type, pen_gl_float_array_matrix_t ); -- m : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..15 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glMultMatrixf( param_ptr );
      free( param_array );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultMatrixf;

procedure ParsePenglRotated is
  -- Syntax: glRotated( angle, x, y, z : GLdouble );
  -- Source: bush_os.opengl.glRotated
  angle_val  : unbounded_string;
  angle_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glrotated_t );
  ParseFirstNumericParameter( angle_val, angle_type, pen_gldouble_t ); -- angle : GLdouble
  ParseNextNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseNextNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  ParseLastNumericParameter( z_val, z_type, pen_gldouble_t ); -- z : GLdouble
  if isExecutingCommand then
    begin
      glRotated( GLdouble( to_numeric( angle_val ) ), GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ), GLdouble( to_numeric( z_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRotated;

procedure ParsePenglRotatef is
  -- Syntax: glRotatef( angle, x, y, z : GLfloat );
  -- Source: bush_os.opengl.glRotatef
  angle_val  : unbounded_string;
  angle_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glrotatef_t );
  ParseFirstNumericParameter( angle_val, angle_type, pen_glfloat_t ); -- angle : GLfloat
  ParseNextNumericParameter( x_val, x_type, pen_glfloat_t ); -- x : GLfloat
  ParseNextNumericParameter( y_val, y_type, pen_glfloat_t ); -- y : GLfloat
  ParseLastNumericParameter( z_val, z_type, pen_glfloat_t ); -- z : GLfloat
  if isExecutingCommand then
    begin
      glRotatef( GLfloat( to_numeric( angle_val ) ), GLfloat( to_numeric( x_val ) ), GLfloat( to_numeric( y_val ) ), GLfloat( to_numeric( z_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRotatef;

procedure ParsePenglScaled is
  -- Syntax: glScaled( x, y, z : GLdouble );
  -- Source: bush_os.opengl.glScaled
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glscaled_t );
  ParseFirstNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseNextNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  ParseLastNumericParameter( z_val, z_type, pen_gldouble_t ); -- z : GLdouble
  if isExecutingCommand then
    begin
      glScaled( GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ), GLdouble( to_numeric( z_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglScaled;

procedure ParsePenglScalef is
  -- Syntax: glScalef( x, y, z : GLfloat );
  -- Source: bush_os.opengl.glScalef
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glscalef_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glfloat_t ); -- x : GLfloat
  ParseNextNumericParameter( y_val, y_type, pen_glfloat_t ); -- y : GLfloat
  ParseLastNumericParameter( z_val, z_type, pen_glfloat_t ); -- z : GLfloat
  if isExecutingCommand then
    begin
      glScalef( GLfloat( to_numeric( x_val ) ), GLfloat( to_numeric( y_val ) ), GLfloat( to_numeric( z_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglScalef;

procedure ParsePenglTranslated is
  -- Syntax: glTranslated( x, y, z : GLdouble );
  -- Source: bush_os.opengl.glTranslated
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_gltranslated_t );
  ParseFirstNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseNextNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  ParseLastNumericParameter( z_val, z_type, pen_gldouble_t ); -- z : GLdouble
  if isExecutingCommand then
    begin
      glTranslated( GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ), GLdouble( to_numeric( z_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTranslated;

procedure ParsePenglTranslatef is
  -- Syntax: glTranslatef( x, y,z : GLfloat );
  -- Source: bush_os.opengl.glTranslatef
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_gltranslatef_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glfloat_t ); -- x : GLfloat
  ParseNextNumericParameter( y_val, y_type, pen_glfloat_t ); -- y : GLfloat
  ParseLastNumericParameter( z_val, z_type, pen_glfloat_t ); -- z : GLfloat
  if isExecutingCommand then
    begin
      glTranslatef( GLfloat( to_numeric( x_val ) ), GLfloat( to_numeric( y_val ) ), GLfloat( to_numeric( z_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTranslatef;

procedure ParsePenglIsList( result : out unbounded_string ) is
  -- Syntax: glIsList( list : GLuint ) return GLboolean;
  -- Source: bush_os.opengl.glIsList
  list_val  : unbounded_string;
  list_type : identifier;
begin
  expect( pen_glislist_t );
  ParseSingleNumericParameter( list_val, list_type, pen_gluint_t ); -- list : GLuint
  if isExecutingCommand then
    begin
      result := to_unbounded_string( long_float( glIsList( GLuint( to_numeric( list_val ) ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIsList;

procedure ParsePenglDeleteLists is
  -- Syntax: glDeleteLists( list : GLuint; rng : GLsizei );
  -- Source: bush_os.opengl.glDeleteLists
  list_val  : unbounded_string;
  list_type : identifier;
  rng_val  : unbounded_string;
  rng_type : identifier;
begin
  expect( pen_gldeletelists_t );
  ParseFirstNumericParameter( list_val, list_type, pen_gluint_t ); -- list : GLuint
  ParseLastNumericParameter( rng_val, rng_type, pen_glsizei_t ); -- rng : GLsizei
  if isExecutingCommand then
    begin
      glDeleteLists( GLuint( to_numeric( list_val ) ), GLsizei( to_numeric( rng_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDeleteLists;

procedure ParsePenglGenLists( result : out unbounded_string ) is
  -- Syntax: glGenLists( rng : GLsizei ) return GLuint;
  -- Source: bush_os.opengl.glGenLists
  rng_val  : unbounded_string;
  rng_type : identifier;
begin
  expect( pen_glgenlists_t );
  ParseSingleNumericParameter( rng_val, rng_type, pen_glsizei_t ); -- rng : GLsizei
  if isExecutingCommand then
    begin
      result := to_unbounded_string( long_float( glGenLists( GLsizei( to_numeric( rng_val ) ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGenLists;

procedure ParsePenglNewList is
  -- Syntax: glNewList( list : GLuint; mode : GLdlists );
  -- Source: bush_os.opengl.glNewList
  list_val  : unbounded_string;
  list_type : identifier;
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glnewlist_t );
  ParseFirstNumericParameter( list_val, list_type, pen_gluint_t ); -- list : GLuint
  ParseLastNumericParameter( mode_val, mode_type, pen_gldlists_t ); -- mode : GLdlists
  if isExecutingCommand then
    begin
      glNewList( GLuint( to_numeric( list_val ) ), GLdlists( to_numeric( mode_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNewList;

procedure ParsePenglEndList is
  -- Syntax: glEndList;
  -- Source: bush_os.opengl.glEndList
begin
  expect( pen_glendlist_t );
  if isExecutingCommand then
    begin
      glEndList;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEndList;

procedure ParsePenglCallList is
  -- Syntax: glCallList( list : GLuint );
  -- Source: bush_os.opengl.glCallList
  list_val  : unbounded_string;
  list_type : identifier;
begin
  expect( pen_glcalllist_t );
  ParseSingleNumericParameter( list_val, list_type, pen_gluint_t ); -- list : GLuint
  if isExecutingCommand then
    begin
      glCallList( GLuint( to_numeric( list_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCallList;

procedure ParsePenglCallLists is
  -- Syntax: glCallLists( n : GLsizei; kind : GLtypes; lists : System.address );
  -- Source: bush_os.opengl.glCallLists
  n_val  : unbounded_string;
  n_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  lists_val  : unbounded_string;
  --lists_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcalllists_t );
  ParseFirstNumericParameter( n_val, n_type, pen_glsizei_t ); -- n : GLsizei
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: system.address is a pointer to a GL call list.
  --ParseLastNumericParameter( lists_val, lists_type, pen_system.address_t ); -- lists : System.address
  if isExecutingCommand then
    declare
      fakeList : integer;
      callList : system.address := fakeList'address;
    begin
      glCallLists( GLsizei( to_numeric( n_val ) ), GLtypes( to_numeric( kind_val ) ), callList );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCallLists;

procedure ParsePenglListBase is
  -- Syntax: glListBase( base : GLuint );
  -- Source: bush_os.opengl.glListBase
  base_val  : unbounded_string;
  base_type : identifier;
begin
  expect( pen_gllistbase_t );
  ParseSingleNumericParameter( base_val, base_type, pen_gluint_t ); -- base : GLuint
  if isExecutingCommand then
    begin
      glListBase( GLuint( to_numeric( base_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglListBase;

procedure ParsePenglBegin is
  -- Syntax: glBegin( mode : GLprimitives );
  -- Source: bush_os.opengl.glBegin
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glbegin_t );
  ParseSingleNumericParameter( mode_val, mode_type, pen_glprimitives_t ); -- mode : GLprimitives
  if isExecutingCommand then
    begin
      glBegin( GLprimitives( to_numeric( mode_val ) ) );
      -- do not check errors: only allowed after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglBegin;

procedure ParsePenglEnd is
  -- Syntax: glEnd;
  -- Source: bush_os.opengl.glEnd
begin
  expect( pen_glend_t );
  if isExecutingCommand then
    begin
      glEnd;
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEnd;

procedure ParsePenglVertex2d is
  -- Syntax: glVertex2d( x, y : GLdouble );
  -- Source: bush_os.opengl.glVertex2d
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
begin
  expect( pen_glvertex2d_t );
  ParseFirstNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseLastNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  if isExecutingCommand then
    begin
      glVertex2d( GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex2d;

procedure ParsePenglVertex2f is
  -- Syntax: glVertex2f( x, y : GLfloat );
  -- Source: bush_os.opengl.glVertex2f
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
begin
  expect( pen_glvertex2f_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glfloat_t ); -- x : GLfloat
  ParseLastNumericParameter( y_val, y_type, pen_glfloat_t ); -- y : GLfloat
  if isExecutingCommand then
    begin
      glVertex2f( GLfloat( to_numeric( x_val ) ), GLfloat( to_numeric( y_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex2f;

procedure ParsePenglVertex2i is
  -- Syntax: glVertex2i( x, y : GLint );
  -- Source: bush_os.opengl.glVertex2i
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
begin
  expect( pen_glvertex2i_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseLastNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  if isExecutingCommand then
    begin
      glVertex2i( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex2i;

procedure ParsePenglVertex2s is
  -- Syntax: glVertex2s( x, y : GLshort );
  -- Source: bush_os.opengl.glVertex2s
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
begin
  expect( pen_glvertex2s_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glshort_t ); -- x : GLshort
  ParseLastNumericParameter( y_val, y_type, pen_glshort_t ); -- y : GLshort
  if isExecutingCommand then
    begin
      glVertex2s( GLshort( to_numeric( x_val ) ), GLshort( to_numeric( y_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex2s;

procedure ParsePenglVertex3d is
  -- Syntax: glVertex3d( x, y, z : GLdouble );
  -- Source: bush_os.opengl.glVertex3d
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glvertex3d_t );
  ParseFirstNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseNextNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  ParseLastNumericParameter( z_val, z_type, pen_gldouble_t ); -- z : GLdouble
  if isExecutingCommand then
    begin
      glVertex3d( GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ), GLdouble( to_numeric( z_val ) ) );
      -- do not check errors: only allowed after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex3d;

procedure ParsePenglVertex3f is
  -- Syntax: glVertex3f( x, y, z : GLfloat );
  -- Source: bush_os.opengl.glVertex3f
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glvertex3f_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glfloat_t ); -- x : GLfloat
  ParseNextNumericParameter( y_val, y_type, pen_glfloat_t ); -- y : GLfloat
  ParseLastNumericParameter( z_val, z_type, pen_glfloat_t ); -- z : GLfloat
  if isExecutingCommand then
    begin
      glVertex3f( GLfloat( to_numeric( x_val ) ), GLfloat( to_numeric( y_val ) ), GLfloat( to_numeric( z_val ) ) );
      -- do not check errors: only allowed after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex3f;

procedure ParsePenglVertex3i is
  -- Syntax: glVertex3i( x, y, z : GLint );
  -- Source: bush_os.opengl.glVertex3i
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glvertex3i_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseLastNumericParameter( z_val, z_type, pen_glint_t ); -- z : GLint
  if isExecutingCommand then
    begin
      glVertex3i( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLint( to_numeric( z_val ) ) );
      -- do not check errors: only allowed after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex3i;

procedure ParsePenglVertex3s is
  -- Syntax: glVertex3s( x, y, z : GLshort );
  -- Source: bush_os.opengl.glVertex3s
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glvertex3s_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glshort_t ); -- x : GLshort
  ParseNextNumericParameter( y_val, y_type, pen_glshort_t ); -- y : GLshort
  ParseLastNumericParameter( z_val, z_type, pen_glshort_t ); -- z : GLshort
  if isExecutingCommand then
    begin
      glVertex3s( GLshort( to_numeric( x_val ) ), GLshort( to_numeric( y_val ) ), GLshort( to_numeric( z_val ) ) );
      -- do not check errors: only allowed after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex3s;

procedure ParsePenglVertex4d is
  -- Syntax: glVertex4d( x, y, z, w : GLdouble );
  -- Source: bush_os.opengl.glVertex4d
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
  w_val  : unbounded_string;
  w_type : identifier;
begin
  expect( pen_glvertex4d_t );
  ParseFirstNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseNextNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  ParseNextNumericParameter( z_val, z_type, pen_gldouble_t ); -- z : GLdouble
  ParseLastNumericParameter( w_val, w_type, pen_gldouble_t ); -- w : GLdouble
  if isExecutingCommand then
    begin
      glVertex4d( GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ), GLdouble( to_numeric( z_val ) ), GLdouble( to_numeric( w_val ) ) );
      -- do not check errors: only allowed after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex4d;

procedure ParsePenglVertex4f is
  -- Syntax: glVertex4f( x, y, z, w : GLfloat );
  -- Source: bush_os.opengl.glVertex4f
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
  w_val  : unbounded_string;
  w_type : identifier;
begin
  expect( pen_glvertex4f_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glfloat_t ); -- x : GLfloat
  ParseNextNumericParameter( y_val, y_type, pen_glfloat_t ); -- y : GLfloat
  ParseNextNumericParameter( z_val, z_type, pen_glfloat_t ); -- z : GLfloat
  ParseLastNumericParameter( w_val, w_type, pen_glfloat_t ); -- w : GLfloat
  if isExecutingCommand then
    begin
      glVertex4f( GLfloat( to_numeric( x_val ) ), GLfloat( to_numeric( y_val ) ), GLfloat( to_numeric( z_val ) ), GLfloat( to_numeric( w_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex4f;

procedure ParsePenglVertex4i is
  -- Syntax: glVertex4i( x, y, z, w : GLint );
  -- Source: bush_os.opengl.glVertex4i
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
  w_val  : unbounded_string;
  w_type : identifier;
begin
  expect( pen_glvertex4i_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( z_val, z_type, pen_glint_t ); -- z : GLint
  ParseLastNumericParameter( w_val, w_type, pen_glint_t ); -- w : GLint
  if isExecutingCommand then
    begin
      glVertex4i( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLint( to_numeric( z_val ) ), GLint( to_numeric( w_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex4i;

procedure ParsePenglVertex4s is
  -- Syntax: glVertex4s( x, y, z, w : GLshort );
  -- Source: bush_os.opengl.glVertex4s
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
  w_val  : unbounded_string;
  w_type : identifier;
begin
  expect( pen_glvertex4s_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glshort_t ); -- x : GLshort
  ParseNextNumericParameter( y_val, y_type, pen_glshort_t ); -- y : GLshort
  ParseNextNumericParameter( z_val, z_type, pen_glshort_t ); -- z : GLshort
  ParseLastNumericParameter( w_val, w_type, pen_glshort_t ); -- w : GLshort
  if isExecutingCommand then
    begin
      glVertex4s( GLshort( to_numeric( x_val ) ), GLshort( to_numeric( y_val ) ), GLshort( to_numeric( z_val ) ), GLshort( to_numeric( w_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex4s;

procedure ParsePenglVertex2dv is
  -- Syntax: glVertex2dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glVertex2dv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex2dv_t );
  ParseSingleInOutParameter( id, pen_gl_double_array_t ); -- x : GLdouble
  if isExecutingCommand then
    declare
      param_ptr   : GL_Double_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Double_Array_Conv.To_Address( theArray.gl_da );
      glVertex2dv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex2dv;

procedure ParsePenglVertex2fv is
  -- Syntax: glVertex2fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glVertex2fv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex2fv_t );
  ParseSingleInOutParameter( id, pen_gl_float_array_t ); -- x : GLfloat
  if isExecutingCommand then
    declare
      param_ptr   : GL_Float_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Float_Array_Conv.To_Address( theArray.gl_fa );
      glVertex2fv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex2fv;

procedure ParsePenglVertex2iv is
  -- Syntax: glVertex2iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glVertex2iv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex2iv_t );
  ParseSingleInOutParameter( id, pen_gl_int_array_t ); -- x : GLint
  if isExecutingCommand then
    declare
      param_ptr   : GL_Int_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Int_Array_Conv.To_Address( theArray.gl_ia );
      glVertex2iv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex2iv;

procedure ParsePenglVertex2sv is
  -- Syntax: glVertex2sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glVertex2sv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex2sv_t );
  ParseSingleInOutParameter( id, pen_gl_short_array_t ); -- x : GLshort
  if isExecutingCommand then
    declare
      param_ptr   : GL_Short_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Short_Array_Conv.To_Address( theArray.gl_sa );
      glVertex2sv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex2sv;

procedure ParsePenglVertex3dv is
  -- Syntax: glVertex3dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glVertex3dv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex3dv_t );
  ParseSingleInOutParameter( id, pen_gl_double_array_t ); -- x : GLdouble
  if isExecutingCommand then
    declare
      param_ptr   : GL_Double_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Double_Array_Conv.To_Address( theArray.gl_da );
      glVertex3dv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex3dv;

procedure ParsePenglVertex3fv is
  -- Syntax: glVertex3fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glVertex3fv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex3fv_t );
  ParseSingleInOutParameter( id, pen_gl_float_array_t ); -- x : GLint
  if isExecutingCommand then
    declare
      param_ptr   : GL_Float_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Float_Array_Conv.To_Address( theArray.gl_fa );
      glVertex3fv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex3fv;

procedure ParsePenglVertex3iv is
  -- Syntax: glVertex3iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glVertex3iv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex3iv_t );
  ParseSingleInOutParameter( id, pen_gl_int_array_t ); -- x : GLint
  if isExecutingCommand then
    declare
      param_ptr   : GL_Int_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Int_Array_Conv.To_Address( theArray.gl_ia );
      glVertex3iv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex3iv;

procedure ParsePenglVertex3sv is
  -- Syntax: glVertex3sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glVertex3sv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex3sv_t );
  ParseSingleInOutParameter( id, pen_gl_short_array_t ); -- x : GLshort
  if isExecutingCommand then
    declare
      param_ptr   : GL_Short_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Short_Array_Conv.To_Address( theArray.gl_sa );
      glVertex3sv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex3sv;

procedure ParsePenglVertex4dv is
  -- Syntax: glVertex4dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glVertex4dv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex4dv_t );
  ParseSingleInOutParameter( id, pen_gl_double_array_t ); -- x : GLdouble
  if isExecutingCommand then
    declare
      param_ptr   : GL_Double_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Double_Array_Conv.To_Address( theArray.gl_da );
      glVertex4dv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex4dv;

procedure ParsePenglVertex4fv is
  -- Syntax: glVertex4fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glVertex4fv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex4fv_t );
  ParseSingleInOutParameter( id, pen_gl_float_array_t ); -- x : GLfloat
  if isExecutingCommand then
    declare
      param_ptr   : GL_Float_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Float_Array_Conv.To_Address( theArray.gl_fa );
      glVertex4fv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex4fv;

procedure ParsePenglVertex4iv is
  -- Syntax: glVertex4iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glVertex4iv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex4iv_t );
  ParseSingleInOutParameter( id, pen_gl_int_array_t ); -- x : GLint
  if isExecutingCommand then
    declare
      param_ptr   : GL_Int_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Int_Array_Conv.To_Address( theArray.gl_ia );
      glVertex4iv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex4iv;

procedure ParsePenglVertex4sv is
  -- Syntax: glVertex4sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glVertex4sv
  id       : identifier;
  theArray : resPtr;
begin
  expect( pen_glvertex4sv_t );
  ParseSingleInOutParameter( id, pen_gl_short_array_t ); -- x : GLshort
  if isExecutingCommand then
    declare
      param_ptr   : GL_Short_Array_Ptr;
    begin
      findResource( to_resource_id( identifiers( id ).value ), theArray );
      param_ptr := GL_Short_Array_Conv.To_Address( theArray.gl_sa );
      glVertex4sv( param_ptr );
    exception when storage_error =>
      err( "storage error raised" );
    when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertex4sv;

procedure ParsePenglNormal3b is
  -- Syntax: glNormal3b( nx, ny, nz : GLbyte );
  -- Source: bush_os.opengl.glNormal3b
  nx_val  : unbounded_string;
  nx_type : identifier;
  ny_val  : unbounded_string;
  ny_type : identifier;
  nz_val  : unbounded_string;
  nz_type : identifier;
begin
  expect( pen_glnormal3b_t );
  ParseFirstNumericParameter( nx_val, nx_type, pen_glbyte_t ); -- nx : GLbyte
  ParseNextNumericParameter( ny_val, ny_type, pen_glbyte_t ); -- ny : GLbyte
  ParseLastNumericParameter( nz_val, nz_type, pen_glbyte_t ); -- nz : GLbyte
  if isExecutingCommand then
    begin
      glNormal3b( GLbyte( to_numeric( nx_val ) ), GLbyte( to_numeric( ny_val ) ), GLbyte( to_numeric( nz_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3b;

procedure ParsePenglNormal3d is
  -- Syntax: glNormal3d( nx, ny, nz : GLdouble );
  -- Source: bush_os.opengl.glNormal3d
  nx_val  : unbounded_string;
  nx_type : identifier;
  ny_val  : unbounded_string;
  ny_type : identifier;
  nz_val  : unbounded_string;
  nz_type : identifier;
begin
  expect( pen_glnormal3d_t );
  ParseFirstNumericParameter( nx_val, nx_type, pen_gldouble_t ); -- nx : GLdouble
  ParseNextNumericParameter( ny_val, ny_type, pen_gldouble_t ); -- ny : GLdouble
  ParseLastNumericParameter( nz_val, nz_type, pen_gldouble_t ); -- nz : GLdouble
  if isExecutingCommand then
    begin
      glNormal3d( GLdouble( to_numeric( nx_val ) ), GLdouble( to_numeric( ny_val ) ), GLdouble( to_numeric( nz_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3d;

procedure ParsePenglNormal3f is
  -- Syntax: glNormal3f( nx, ny, nz : GLfloat );
  -- Source: bush_os.opengl.glNormal3f
  nx_val  : unbounded_string;
  nx_type : identifier;
  ny_val  : unbounded_string;
  ny_type : identifier;
  nz_val  : unbounded_string;
  nz_type : identifier;
begin
  expect( pen_glnormal3f_t );
  ParseFirstNumericParameter( nx_val, nx_type, pen_glfloat_t ); -- nx : GLfloat
  ParseNextNumericParameter( ny_val, ny_type, pen_glfloat_t ); -- ny : GLfloat
  ParseLastNumericParameter( nz_val, nz_type, pen_glfloat_t ); -- nz : GLfloat
  if isExecutingCommand then
    begin
      glNormal3f( GLfloat( to_numeric( nx_val ) ), GLfloat( to_numeric( ny_val ) ), GLfloat( to_numeric( nz_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3f;

procedure ParsePenglNormal3i is
  -- Syntax: glNormal3i( nx, ny, nz : GLint );
  -- Source: bush_os.opengl.glNormal3i
  nx_val  : unbounded_string;
  nx_type : identifier;
  ny_val  : unbounded_string;
  ny_type : identifier;
  nz_val  : unbounded_string;
  nz_type : identifier;
begin
  expect( pen_glnormal3i_t );
  ParseFirstNumericParameter( nx_val, nx_type, pen_glint_t ); -- nx : GLint
  ParseNextNumericParameter( ny_val, ny_type, pen_glint_t ); -- ny : GLint
  ParseLastNumericParameter( nz_val, nz_type, pen_glint_t ); -- nz : GLint
  if isExecutingCommand then
    begin
      glNormal3i( GLint( to_numeric( nx_val ) ), GLint( to_numeric( ny_val ) ), GLint( to_numeric( nz_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3i;

procedure ParsePenglNormal3s is
  -- Syntax: glNormal3s( nx, ny, nz : GLshort );
  -- Source: bush_os.opengl.glNormal3s
  nx_val  : unbounded_string;
  nx_type : identifier;
  ny_val  : unbounded_string;
  ny_type : identifier;
  nz_val  : unbounded_string;
  nz_type : identifier;
begin
  expect( pen_glnormal3s_t );
  ParseFirstNumericParameter( nx_val, nx_type, pen_glshort_t ); -- nx : GLshort
  ParseNextNumericParameter( ny_val, ny_type, pen_glshort_t ); -- ny : GLshort
  ParseLastNumericParameter( nz_val, nz_type, pen_glshort_t ); -- nz : GLshort
  if isExecutingCommand then
    begin
      glNormal3s( GLshort( to_numeric( nx_val ) ), GLshort( to_numeric( ny_val ) ), GLshort( to_numeric( nz_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3s;

procedure ParsePenglNormal3bv is
  -- Syntax: glNormal3bv( v : GL_Byte_Array_Ptr );
  -- Source: bush_os.opengl.glNormal3bv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glnormal3bv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_byte_array_ptr_t ); -- v : GL_Byte_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Byte_Array_Access := new byte_array( 0..2 );
      param_ptr   : GL_Byte_Array_Ptr := GL_Byte_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glNormal3bv( GL_Byte_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3bv;

procedure ParsePenglNormal3dv is
  -- Syntax: glNormal3dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glNormal3dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glnormal3dv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..2 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glNormal3dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3dv;

procedure ParsePenglNormal3fv is
  -- Syntax: glNormal3fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glNormal3fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glnormal3fv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..2 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glNormal3fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3fv;

procedure ParsePenglNormal3iv is
  -- Syntax: glNormal3iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glNormal3iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glnormal3iv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..2 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glNormal3iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3iv;

procedure ParsePenglNormal3sv is
  -- Syntax: glNormal3sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glNormal3sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glnormal3sv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..2 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glNormal3sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormal3sv;

procedure ParsePenglIndexd is
  -- Syntax: glIndexd( c : GLdouble );
  -- Source: bush_os.opengl.glIndexd
  c_val  : unbounded_string;
  c_type : identifier;
begin
  expect( pen_glindexd_t );
  ParseSingleNumericParameter( c_val, c_type, pen_gldouble_t ); -- c : GLdouble
  if isExecutingCommand then
    begin
      glIndexd( GLdouble( to_numeric( c_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexd;

procedure ParsePenglIndexf is
  -- Syntax: glIndexf( c : GLfloat );
  -- Source: bush_os.opengl.glIndexf
  c_val  : unbounded_string;
  c_type : identifier;
begin
  expect( pen_glindexf_t );
  ParseSingleNumericParameter( c_val, c_type, pen_glfloat_t ); -- c : GLfloat
  if isExecutingCommand then
    begin
      glIndexf( GLfloat( to_numeric( c_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexf;

procedure ParsePenglIndexi is
  -- Syntax: glIndexi( c : GLint );
  -- Source: bush_os.opengl.glIndexi
  c_val  : unbounded_string;
  c_type : identifier;
begin
  expect( pen_glindexi_t );
  ParseSingleNumericParameter( c_val, c_type, pen_glint_t ); -- c : GLint
  if isExecutingCommand then
    begin
      glIndexi( GLint( to_numeric( c_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexi;

procedure ParsePenglIndexs is
  -- Syntax: glIndexs( c : GLshort );
  -- Source: bush_os.opengl.glIndexs
  c_val  : unbounded_string;
  c_type : identifier;
begin
  expect( pen_glindexs_t );
  ParseSingleNumericParameter( c_val, c_type, pen_glshort_t ); -- c : GLshort
  if isExecutingCommand then
    begin
      glIndexs( GLshort( to_numeric( c_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexs;

procedure ParsePenglIndexub is
  -- Syntax: glIndexub( c : GLubyte ); -- /* 1.1 */
  -- Source: bush_os.opengl.glIndexub
  c_val  : unbounded_string;
  c_type : identifier;
begin
  expect( pen_glindexub_t );
  ParseSingleNumericParameter( c_val, c_type, pen_glubyte_t ); -- c : GLubyte
  if isExecutingCommand then
    begin
      glIndexub( GLubyte( to_numeric( c_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexub;

procedure ParsePenglIndexdv is
  -- Syntax: glIndexdv( c : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glIndexdv
  c_val  : unbounded_string;
--  c_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glindexdv_t );
  -- TODO: must be a 1 value sparforte array
  --ParseSingleNumericParameter( c_val, c_type, pen_gl_double_array_ptr_t ); -- c : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..0 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glIndexdv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexdv;

procedure ParsePenglIndexfv is
  -- Syntax: glIndexfv( c : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glIndexfv
  c_val  : unbounded_string;
--  c_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glindexfv_t );
  -- TODO: must be a 1 value sparforte array
  --ParseSingleNumericParameter( c_val, c_type, pen_gl_float_array_ptr_t ); -- c : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glIndexfv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexfv;

procedure ParsePenglIndexiv is
  -- Syntax: glIndexiv( c : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glIndexiv
  c_val  : unbounded_string;
--  c_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glindexiv_t );
  -- TODO: must be a 1 value sparforte array
  --ParseSingleNumericParameter( c_val, c_type, pen_gl_int_array_ptr_t ); -- c : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glIndexiv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexiv;

procedure ParsePenglIndexsv is
  -- Syntax: glIndexsv( c : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glIndexsv
  c_val  : unbounded_string;
--  c_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glindexsv_t );
  -- TODO: must be a 1 value sparforte array
  --ParseSingleNumericParameter( c_val, c_type, pen_gl_short_array_ptr_t ); -- c : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..0 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glIndexsv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexsv;

procedure ParsePenglIndexubv is
  -- Syntax: glIndexubv( c : GL_UByte_Array_Ptr ); -- /* 1.1 */
  -- Source: bush_os.opengl.glIndexubv
  c_val  : unbounded_string;
--  c_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glindexubv_t );
  -- TODO: must be a 1 value sparforte array
  --ParseSingleNumericParameter( c_val, c_type, pen_gl_ubyte_array_ptr_t ); -- c : GL_UByte_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_UByte_Array_Access := new ubyte_array( 0..0 );
      param_ptr   : GL_UByte_Array_Ptr := GL_UByte_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glIndexubv( GL_UByte_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexubv;

procedure ParsePenglColor3 is
  -- Syntax: glColor( colour_name );
  -- Source: N/A
  nameVal   : unbounded_string;
  nameType  : identifier;
begin
  expect( pen_glcolor3_t );
  ParseSingleNumericParameter( nameVal, nameType, pen_pen_color_name_t );
  if isExecutingCommand then
    declare
      cn : AColourName := aColourName'val( integer( to_numeric( nameVal ) ) );
    begin
      glColor3f( GLfloat( ColourNames( cn ).red ),
                 GLfloat( ColourNames( cn ).green ),
                 GLfloat( ColourNames( cn ).blue ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3;

procedure ParsePenglColor3b is
  -- Syntax: glColor3b( red, green, blue : GLbyte );
  -- Source: bush_os.opengl.glColor3b
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
begin
  expect( pen_glcolor3b_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glbyte_t ); -- red : GLbyte
  ParseNextNumericParameter( green_val, green_type, pen_glbyte_t ); -- green : GLbyte
  ParseLastNumericParameter( blue_val, blue_type, pen_glbyte_t ); -- blue : GLbyte
  if isExecutingCommand then
    begin
      glColor3b( GLbyte( to_numeric( red_val ) ), GLbyte( to_numeric( green_val ) ), GLbyte( to_numeric( blue_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3b;

procedure ParsePenglColor3d is
  -- Syntax: glColor3d( red, green, blue : GLdouble );
  -- Source: bush_os.opengl.glColor3d
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
begin
  expect( pen_glcolor3d_t );
  ParseFirstNumericParameter( red_val, red_type, pen_gldouble_t ); -- red : GLdouble
  ParseNextNumericParameter( green_val, green_type, pen_gldouble_t ); -- green : GLdouble
  ParseLastNumericParameter( blue_val, blue_type, pen_gldouble_t ); -- blue : GLdouble
  if isExecutingCommand then
    begin
      glColor3d( GLdouble( to_numeric( red_val ) ), GLdouble( to_numeric( green_val ) ), GLdouble( to_numeric( blue_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3d;

procedure ParsePenglColor3f is
  -- Syntax: glColor3f( red, green, blue : GLfloat );
  -- Source: bush_os.opengl.glColor3f
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
begin
  expect( pen_glcolor3f_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glfloat_t ); -- red : GLfloat
  ParseNextNumericParameter( green_val, green_type, pen_glfloat_t ); -- green : GLfloat
  ParseLastNumericParameter( blue_val, blue_type, pen_glfloat_t ); -- blue : GLfloat
  if isExecutingCommand then
    begin
      glColor3f( GLfloat( to_numeric( red_val ) ), GLfloat( to_numeric( green_val ) ), GLfloat( to_numeric( blue_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3f;

procedure ParsePenglColor3i is
  -- Syntax: glColor3i( red, green, blue : GLint );
  -- Source: bush_os.opengl.glColor3i
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
begin
  expect( pen_glcolor3i_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glint_t ); -- red : GLint
  ParseNextNumericParameter( green_val, green_type, pen_glint_t ); -- green : GLint
  ParseLastNumericParameter( blue_val, blue_type, pen_glint_t ); -- blue : GLint
  if isExecutingCommand then
    begin
      glColor3i( GLint( to_numeric( red_val ) ), GLint( to_numeric( green_val ) ), GLint( to_numeric( blue_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3i;

procedure ParsePenglColor3s is
  -- Syntax: glColor3s( red, green, blue : GLshort );
  -- Source: bush_os.opengl.glColor3s
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
begin
  expect( pen_glcolor3s_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glshort_t ); -- red : GLshort
  ParseNextNumericParameter( green_val, green_type, pen_glshort_t ); -- green : GLshort
  ParseLastNumericParameter( blue_val, blue_type, pen_glshort_t ); -- blue : GLshort
  if isExecutingCommand then
    begin
      glColor3s( GLshort( to_numeric( red_val ) ), GLshort( to_numeric( green_val ) ), GLshort( to_numeric( blue_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3s;

procedure ParsePenglColor3ub is
  -- Syntax: glColor3ub( red, green, blue : GLubyte );
  -- Source: bush_os.opengl.glColor3ub
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
begin
  expect( pen_glcolor3ub_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glubyte_t ); -- red : GLubyte
  ParseNextNumericParameter( green_val, green_type, pen_glubyte_t ); -- green : GLubyte
  ParseLastNumericParameter( blue_val, blue_type, pen_glubyte_t ); -- blue : GLubyte
  if isExecutingCommand then
    begin
      glColor3ub( GLubyte( to_numeric( red_val ) ), GLubyte( to_numeric( green_val ) ), GLubyte( to_numeric( blue_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3ub;

procedure ParsePenglColor3ui is
  -- Syntax: glColor3ui( red, green, blue : GLuint );
  -- Source: bush_os.opengl.glColor3ui
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
begin
  expect( pen_glcolor3ui_t );
  ParseFirstNumericParameter( red_val, red_type, pen_gluint_t ); -- red : GLuint
  ParseNextNumericParameter( green_val, green_type, pen_gluint_t ); -- green : GLuint
  ParseLastNumericParameter( blue_val, blue_type, pen_gluint_t ); -- blue : GLuint
  if isExecutingCommand then
    begin
      glColor3ui( GLuint( to_numeric( red_val ) ), GLuint( to_numeric( green_val ) ), GLuint( to_numeric( blue_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3ui;

procedure ParsePenglColor3us is
  -- Syntax: glColor3us( red, green, blue : GLushort );
  -- Source: bush_os.opengl.glColor3us
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
begin
  expect( pen_glcolor3us_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glushort_t ); -- red : GLushort
  ParseNextNumericParameter( green_val, green_type, pen_glushort_t ); -- green : GLushort
  ParseLastNumericParameter( blue_val, blue_type, pen_glushort_t ); -- blue : GLushort
  if isExecutingCommand then
    begin
      glColor3us( GLushort( to_numeric( red_val ) ), GLushort( to_numeric( green_val ) ), GLushort( to_numeric( blue_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3us;

procedure ParsePenglColor4 is
  -- Syntax: glColor( colour_name );
  -- Source: N/A
  nameVal   : unbounded_string;
  nameType  : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolor4_t );
  ParseFirstNumericParameter( nameVal, nameType, pen_pen_color_name_t );
  ParseFirstNumericParameter( nameVal, nameType, pen_pen_color_name_t );
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glfloat_t );
  if isExecutingCommand then
    declare
      cn : AColourName := aColourName'val( integer( to_numeric( nameVal ) ) );
    begin
      glColor4f( GLfloat( ColourNames( cn ).red ),
                 GLfloat( ColourNames( cn ).green ),
                 GLfloat( ColourNames( cn ).blue ),
                 GLfloat( to_numeric( alpha_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4;

procedure ParsePenglColor4b is
  -- Syntax: glColor4b( red, green, blue, alpha : GLbyte );
  -- Source: bush_os.opengl.glColor4b
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolor4b_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glbyte_t ); -- red : GLbyte
  ParseNextNumericParameter( green_val, green_type, pen_glbyte_t ); -- green : GLbyte
  ParseNextNumericParameter( blue_val, blue_type, pen_glbyte_t ); -- blue : GLbyte
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glbyte_t ); -- alpha : GLbyte
  if isExecutingCommand then
    begin
      glColor4b( GLbyte( to_numeric( red_val ) ), GLbyte( to_numeric( green_val ) ), GLbyte( to_numeric( blue_val ) ), GLbyte( to_numeric( alpha_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4b;

procedure ParsePenglColor4d is
  -- Syntax: glColor4d( red, green, blue, alpha : GLdouble );
  -- Source: bush_os.opengl.glColor4d
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolor4d_t );
  ParseFirstNumericParameter( red_val, red_type, pen_gldouble_t ); -- red : GLdouble
  ParseNextNumericParameter( green_val, green_type, pen_gldouble_t ); -- green : GLdouble
  ParseNextNumericParameter( blue_val, blue_type, pen_gldouble_t ); -- blue : GLdouble
  ParseLastNumericParameter( alpha_val, alpha_type, pen_gldouble_t ); -- alpha : GLdouble
  if isExecutingCommand then
    begin
      glColor4d( GLdouble( to_numeric( red_val ) ), GLdouble( to_numeric( green_val ) ), GLdouble( to_numeric( blue_val ) ), GLdouble( to_numeric( alpha_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4d;

procedure ParsePenglColor4f is
  -- Syntax: glColor4f( red, green, blue, alpha : GLfloat );
  -- Source: bush_os.opengl.glColor4f
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolor4f_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glfloat_t ); -- red : GLfloat
  ParseNextNumericParameter( green_val, green_type, pen_glfloat_t ); -- green : GLfloat
  ParseNextNumericParameter( blue_val, blue_type, pen_glfloat_t ); -- blue : GLfloat
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glfloat_t ); -- alpha : GLfloat
  if isExecutingCommand then
    begin
      glColor4f( GLfloat( to_numeric( red_val ) ), GLfloat( to_numeric( green_val ) ), GLfloat( to_numeric( blue_val ) ), GLfloat( to_numeric( alpha_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4f;

procedure ParsePenglColor4i is
  -- Syntax: glColor4i( red, green, blue, alpha : GLint );
  -- Source: bush_os.opengl.glColor4i
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolor4i_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glint_t ); -- red : GLint
  ParseNextNumericParameter( green_val, green_type, pen_glint_t ); -- green : GLint
  ParseNextNumericParameter( blue_val, blue_type, pen_glint_t ); -- blue : GLint
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glint_t ); -- alpha : GLint
  if isExecutingCommand then
    begin
      glColor4i( GLint( to_numeric( red_val ) ), GLint( to_numeric( green_val ) ), GLint( to_numeric( blue_val ) ), GLint( to_numeric( alpha_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4i;

procedure ParsePenglColor4s is
  -- Syntax: glColor4s( red, green, blue, alpha : GLshort );
  -- Source: bush_os.opengl.glColor4s
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolor4s_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glshort_t ); -- red : GLshort
  ParseNextNumericParameter( green_val, green_type, pen_glshort_t ); -- green : GLshort
  ParseNextNumericParameter( blue_val, blue_type, pen_glshort_t ); -- blue : GLshort
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glshort_t ); -- alpha : GLshort
  if isExecutingCommand then
    begin
      glColor4s( GLshort( to_numeric( red_val ) ), GLshort( to_numeric( green_val ) ), GLshort( to_numeric( blue_val ) ), GLshort( to_numeric( alpha_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4s;

procedure ParsePenglColor4ub is
  -- Syntax: glColor4ub( red, green, blue, alpha : GLubyte );
  -- Source: bush_os.opengl.glColor4ub
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolor4ub_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glubyte_t ); -- red : GLubyte
  ParseNextNumericParameter( green_val, green_type, pen_glubyte_t ); -- green : GLubyte
  ParseNextNumericParameter( blue_val, blue_type, pen_glubyte_t ); -- blue : GLubyte
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glubyte_t ); -- alpha : GLubyte
  if isExecutingCommand then
    begin
      glColor4ub( GLubyte( to_numeric( red_val ) ), GLubyte( to_numeric( green_val ) ), GLubyte( to_numeric( blue_val ) ), GLubyte( to_numeric( alpha_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4ub;

procedure ParsePenglColor4ui is
  -- Syntax: glColor4ui( red, green, blue, alpha : GLuint );
  -- Source: bush_os.opengl.glColor4ui
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolor4ui_t );
  ParseFirstNumericParameter( red_val, red_type, pen_gluint_t ); -- red : GLuint
  ParseNextNumericParameter( green_val, green_type, pen_gluint_t ); -- green : GLuint
  ParseNextNumericParameter( blue_val, blue_type, pen_gluint_t ); -- blue : GLuint
  ParseLastNumericParameter( alpha_val, alpha_type, pen_gluint_t ); -- alpha : GLuint
  if isExecutingCommand then
    begin
      glColor4ui( GLuint( to_numeric( red_val ) ), GLuint( to_numeric( green_val ) ), GLuint( to_numeric( blue_val ) ), GLuint( to_numeric( alpha_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4ui;

procedure ParsePenglColor4us is
  -- Syntax: glColor4us( red, green, blue, alpha : GLushort );
  -- Source: bush_os.opengl.glColor4us
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glcolor4us_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glushort_t ); -- red : GLushort
  ParseNextNumericParameter( green_val, green_type, pen_glushort_t ); -- green : GLushort
  ParseNextNumericParameter( blue_val, blue_type, pen_glushort_t ); -- blue : GLushort
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glushort_t ); -- alpha : GLushort
  if isExecutingCommand then
    begin
      glColor4us( GLushort( to_numeric( red_val ) ), GLushort( to_numeric( green_val ) ), GLushort( to_numeric( blue_val ) ), GLushort( to_numeric( alpha_val ) ) );
      -- do not check errors: glcolor may be called during glbegin which
      -- requires error checking only after glEnd
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4us;

procedure ParsePenglColor3bv is
  -- Syntax: glColor3bv( v : GL_Byte_Array_Ptr );
  -- Source: bush_os.opengl.glColor3bv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor3bv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_byte_array_ptr_t ); -- v : GL_Byte_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Byte_Array_Access := new byte_array( 0..2 );
      param_ptr   : GL_Byte_Array_Ptr := GL_Byte_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor3bv( GL_Byte_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3bv;

procedure ParsePenglColor3dv is
  -- Syntax: glColor3dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glColor3dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor3dv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..2 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor3dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3dv;

procedure ParsePenglColor3fv is
  -- Syntax: glColor3fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glColor3fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor3fv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..2 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor3fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3fv;

procedure ParsePenglColor3iv is
  -- Syntax: glColor3iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glColor3iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor3iv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..2 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor3iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3iv;

procedure ParsePenglColor3sv is
  -- Syntax: glColor3sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glColor3sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor3sv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..2 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor3sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3sv;

procedure ParsePenglColor3ubv is
  -- Syntax: glColor3ubv( v : GL_UByte_Array_Ptr );
  -- Source: bush_os.opengl.glColor3ubv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor3ubv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_ubyte_array_ptr_t ); -- v : GL_UByte_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_UByte_Array_Access := new ubyte_array( 0..2 );
      param_ptr   : GL_UByte_Array_Ptr := GL_UByte_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor3ubv( GL_UByte_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3ubv;

procedure ParsePenglColor3uiv is
  -- Syntax: glColor3uiv( v : GL_UInt_Array_Ptr );
  -- Source: bush_os.opengl.glColor3uiv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor3uiv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_uint_array_ptr_t ); -- v : GL_UInt_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_UInt_Array_Access := new uint_array( 0..2 );
      param_ptr   : GL_UInt_Array_Ptr := GL_UInt_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor3uiv( GL_UInt_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3uiv;

procedure ParsePenglColor3usv is
  -- Syntax: glColor3usv( v : GL_UShort_Array_Ptr );
  -- Source: bush_os.opengl.glColor3usv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor3usv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_ushort_array_ptr_t ); -- v : GL_UShort_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_UShort_Array_Access := new ushort_array( 0..2 );
      param_ptr   : GL_UShort_Array_Ptr := GL_UShort_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor3usv( GL_UShort_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor3usv;

procedure ParsePenglColor4bv is
  -- Syntax: glColor4bv( v : GL_Byte_Array_Ptr );
  -- Source: bush_os.opengl.glColor4bv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor4bv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_byte_array_ptr_t ); -- v : GL_Byte_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Byte_Array_Access := new byte_array( 0..3 );
      param_ptr   : GL_Byte_Array_Ptr := GL_Byte_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor4bv( GL_Byte_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4bv;

procedure ParsePenglColor4dv is
  -- Syntax: glColor4dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glColor4dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor4dv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..3 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor4dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4dv;

procedure ParsePenglColor4fv is
  -- Syntax: glColor4fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glColor4fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor4fv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..3 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor4fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4fv;

procedure ParsePenglColor4iv is
  -- Syntax: glColor4iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glColor4iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor4iv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..3 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor4iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4iv;

procedure ParsePenglColor4sv is
  -- Syntax: glColor4sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glColor4sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor4sv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..3 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor4sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4sv;

procedure ParsePenglColor4ubv is
  -- Syntax: glColor4ubv( v : GL_UByte_Array_Ptr );
  -- Source: bush_os.opengl.glColor4ubv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor4ubv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_ubyte_array_ptr_t ); -- v : GL_UByte_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_UByte_Array_Access := new ubyte_array( 0..3 );
      param_ptr   : GL_UByte_Array_Ptr := GL_UByte_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor4ubv( GL_UByte_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4ubv;

procedure ParsePenglColor4uiv is
  -- Syntax: glColor4uiv( v : GL_UInt_Array_Ptr );
  -- Source: bush_os.opengl.glColor4uiv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor4uiv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_uint_array_ptr_t ); -- v : GL_UInt_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_UInt_Array_Access := new uint_array( 0..3 );
      param_ptr   : GL_UInt_Array_Ptr := GL_UInt_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor4uiv( GL_UInt_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4uiv;

procedure ParsePenglColor4usv is
  -- Syntax: glColor4usv( v : GL_UShort_Array_Ptr );
  -- Source: bush_os.opengl.glColor4usv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolor4usv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_ushort_array_ptr_t ); -- v : GL_UShort_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_UShort_Array_Access := new ushort_array( 0..3 );
      param_ptr   : GL_UShort_Array_Ptr := GL_UShort_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glColor4usv( GL_UShort_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColor4usv;

procedure ParsePenglTexCoord1d is
  -- Syntax: glTexCoord1d( s : GLdouble );
  -- Source: bush_os.opengl.glTexCoord1d
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_gltexcoord1d_t );
  ParseSingleNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  if isExecutingCommand then
    begin
      glTexCoord1d( GLdouble( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord1d;

procedure ParsePenglTexCoord1f is
  -- Syntax: glTexCoord1f( s : GLfloat );
  -- Source: bush_os.opengl.glTexCoord1f
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_gltexcoord1f_t );
  ParseSingleNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  if isExecutingCommand then
    begin
      glTexCoord1f( GLfloat( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord1f;

procedure ParsePenglTexCoord1i is
  -- Syntax: glTexCoord1i( s : GLint );
  -- Source: bush_os.opengl.glTexCoord1i
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_gltexcoord1i_t );
  ParseSingleNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  if isExecutingCommand then
    begin
      glTexCoord1i( GLint( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord1i;

procedure ParsePenglTexCoord1s is
  -- Syntax: glTexCoord1s( s : GLshort );
  -- Source: bush_os.opengl.glTexCoord1s
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_gltexcoord1s_t );
  ParseSingleNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  if isExecutingCommand then
    begin
      glTexCoord1s( GLshort( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord1s;

procedure ParsePenglTexCoord2d is
  -- Syntax: glTexCoord2d( s, t : GLdouble );
  -- Source: bush_os.opengl.glTexCoord2d
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_gltexcoord2d_t );
  ParseFirstNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  ParseLastNumericParameter( t_val, t_type, pen_gldouble_t ); -- t : GLdouble
  if isExecutingCommand then
    begin
      glTexCoord2d( GLdouble( to_numeric( s_val ) ), GLdouble( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord2d;

procedure ParsePenglTexCoord2f is
  -- Syntax: glTexCoord2f( s, t : GLfloat );
  -- Source: bush_os.opengl.glTexCoord2f
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_gltexcoord2f_t );
  ParseFirstNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  ParseLastNumericParameter( t_val, t_type, pen_glfloat_t ); -- t : GLfloat
  if isExecutingCommand then
    begin
      glTexCoord2f( GLfloat( to_numeric( s_val ) ), GLfloat( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord2f;

procedure ParsePenglTexCoord2i is
  -- Syntax: glTexCoord2i( s, t : GLint );
  -- Source: bush_os.opengl.glTexCoord2i
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_gltexcoord2i_t );
  ParseFirstNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  ParseLastNumericParameter( t_val, t_type, pen_glint_t ); -- t : GLint
  if isExecutingCommand then
    begin
      glTexCoord2i( GLint( to_numeric( s_val ) ), GLint( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord2i;

procedure ParsePenglTexCoord2s is
  -- Syntax: glTexCoord2s( s, t : GLshort );
  -- Source: bush_os.opengl.glTexCoord2s
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_gltexcoord2s_t );
  ParseFirstNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  ParseLastNumericParameter( t_val, t_type, pen_glshort_t ); -- t : GLshort
  if isExecutingCommand then
    begin
      glTexCoord2s( GLshort( to_numeric( s_val ) ), GLshort( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord2s;

procedure ParsePenglTexCoord3d is
  -- Syntax: glTexCoord3d( s, t, r : GLdouble );
  -- Source: bush_os.opengl.glTexCoord3d
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_gltexcoord3d_t );
  ParseFirstNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  ParseNextNumericParameter( t_val, t_type, pen_gldouble_t ); -- t : GLdouble
  ParseLastNumericParameter( r_val, r_type, pen_gldouble_t ); -- r : GLdouble
  if isExecutingCommand then
    begin
      glTexCoord3d( GLdouble( to_numeric( s_val ) ), GLdouble( to_numeric( t_val ) ), GLdouble( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord3d;

procedure ParsePenglTexCoord3f is
  -- Syntax: glTexCoord3f( s, t, r : GLfloat );
  -- Source: bush_os.opengl.glTexCoord3f
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_gltexcoord3f_t );
  ParseFirstNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  ParseNextNumericParameter( t_val, t_type, pen_glfloat_t ); -- t : GLfloat
  ParseLastNumericParameter( r_val, r_type, pen_glfloat_t ); -- r : GLfloat
  if isExecutingCommand then
    begin
      glTexCoord3f( GLfloat( to_numeric( s_val ) ), GLfloat( to_numeric( t_val ) ), GLfloat( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord3f;

procedure ParsePenglTexCoord3i is
  -- Syntax: glTexCoord3i( s, t, r : GLint );
  -- Source: bush_os.opengl.glTexCoord3i
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_gltexcoord3i_t );
  ParseFirstNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  ParseNextNumericParameter( t_val, t_type, pen_glint_t ); -- t : GLint
  ParseLastNumericParameter( r_val, r_type, pen_glint_t ); -- r : GLint
  if isExecutingCommand then
    begin
      glTexCoord3i( GLint( to_numeric( s_val ) ), GLint( to_numeric( t_val ) ), GLint( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord3i;

procedure ParsePenglTexCoord3s is
  -- Syntax: glTexCoord3s( s, t, r : GLshort );
  -- Source: bush_os.opengl.glTexCoord3s
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_gltexcoord3s_t );
  ParseFirstNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  ParseNextNumericParameter( t_val, t_type, pen_glshort_t ); -- t : GLshort
  ParseLastNumericParameter( r_val, r_type, pen_glshort_t ); -- r : GLshort
  if isExecutingCommand then
    begin
      glTexCoord3s( GLshort( to_numeric( s_val ) ), GLshort( to_numeric( t_val ) ), GLshort( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord3s;

procedure ParsePenglTexCoord4d is
  -- Syntax: glTexCoord4d( s, t, r, q : GLdouble );
  -- Source: bush_os.opengl.glTexCoord4d
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_gltexcoord4d_t );
  ParseFirstNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  ParseNextNumericParameter( t_val, t_type, pen_gldouble_t ); -- t : GLdouble
  ParseNextNumericParameter( r_val, r_type, pen_gldouble_t ); -- r : GLdouble
  ParseLastNumericParameter( q_val, q_type, pen_gldouble_t ); -- q : GLdouble
  if isExecutingCommand then
    begin
      glTexCoord4d( GLdouble( to_numeric( s_val ) ), GLdouble( to_numeric( t_val ) ), GLdouble( to_numeric( r_val ) ), GLdouble( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord4d;

procedure ParsePenglTexCoord4f is
  -- Syntax: glTexCoord4f( s, t, r, q : GLfloat );
  -- Source: bush_os.opengl.glTexCoord4f
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_gltexcoord4f_t );
  ParseFirstNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  ParseNextNumericParameter( t_val, t_type, pen_glfloat_t ); -- t : GLfloat
  ParseNextNumericParameter( r_val, r_type, pen_glfloat_t ); -- r : GLfloat
  ParseLastNumericParameter( q_val, q_type, pen_glfloat_t ); -- q : GLfloat
  if isExecutingCommand then
    begin
      glTexCoord4f( GLfloat( to_numeric( s_val ) ), GLfloat( to_numeric( t_val ) ), GLfloat( to_numeric( r_val ) ), GLfloat( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord4f;

procedure ParsePenglTexCoord4i is
  -- Syntax: glTexCoord4i( s, t, r, q : GLint );
  -- Source: bush_os.opengl.glTexCoord4i
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_gltexcoord4i_t );
  ParseFirstNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  ParseNextNumericParameter( t_val, t_type, pen_glint_t ); -- t : GLint
  ParseNextNumericParameter( r_val, r_type, pen_glint_t ); -- r : GLint
  ParseLastNumericParameter( q_val, q_type, pen_glint_t ); -- q : GLint
  if isExecutingCommand then
    begin
      glTexCoord4i( GLint( to_numeric( s_val ) ), GLint( to_numeric( t_val ) ), GLint( to_numeric( r_val ) ), GLint( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord4i;

procedure ParsePenglTexCoord4s is
  -- Syntax: glTexCoord4s( s, t, r, q : GLshort );
  -- Source: bush_os.opengl.glTexCoord4s
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_gltexcoord4s_t );
  ParseFirstNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  ParseNextNumericParameter( t_val, t_type, pen_glshort_t ); -- t : GLshort
  ParseNextNumericParameter( r_val, r_type, pen_glshort_t ); -- r : GLshort
  ParseLastNumericParameter( q_val, q_type, pen_glshort_t ); -- q : GLshort
  if isExecutingCommand then
    begin
      glTexCoord4s( GLshort( to_numeric( s_val ) ), GLshort( to_numeric( t_val ) ), GLshort( to_numeric( r_val ) ), GLshort( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord4s;

procedure ParsePenglTexCoord1dv is
  -- Syntax: glTexCoord1dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord1dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord1dv_t );
  -- TODO: must be a 1 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..0 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord1dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord1dv;

procedure ParsePenglTexCoord1fv is
  -- Syntax: glTexCoord1fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord1fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord1fv_t );
  -- TODO: must be a 1 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord1fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord1fv;

procedure ParsePenglTexCoord1iv is
  -- Syntax: glTexCoord1iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord1iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord1iv_t );
  -- TODO: must be a 1 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord1iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord1iv;

procedure ParsePenglTexCoord1sv is
  -- Syntax: glTexCoord1sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord1sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord1sv_t );
  -- TODO: must be a 1 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..0 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord1sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord1sv;

procedure ParsePenglTexCoord2dv is
  -- Syntax: glTexCoord2dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord2dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord2dv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..1 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord2dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord2dv;

procedure ParsePenglTexCoord2fv is
  -- Syntax: glTexCoord2fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord2fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord2fv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..1 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord2fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord2fv;

procedure ParsePenglTexCoord2iv is
  -- Syntax: glTexCoord2iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord2iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord2iv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..1 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord2iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord2iv;

procedure ParsePenglTexCoord2sv is
  -- Syntax: glTexCoord2sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord2sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord2sv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..1 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord2sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord2sv;

procedure ParsePenglTexCoord3dv is
  -- Syntax: glTexCoord3dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord3dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord3dv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..2 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord3dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord3dv;

procedure ParsePenglTexCoord3fv is
  -- Syntax: glTexCoord3fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord3fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord3fv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..2 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord3fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord3fv;

procedure ParsePenglTexCoord3iv is
  -- Syntax: glTexCoord3iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord3iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord3iv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..2 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord3iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord3iv;

procedure ParsePenglTexCoord3sv is
  -- Syntax: glTexCoord3sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord3sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord3sv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..2 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord3sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord3sv;

procedure ParsePenglTexCoord4dv is
  -- Syntax: glTexCoord4dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord4dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord4dv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..3 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord4dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord4dv;

procedure ParsePenglTexCoord4fv is
  -- Syntax: glTexCoord4fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord4fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord4fv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..3 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord4fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord4fv;

procedure ParsePenglTexCoord4iv is
  -- Syntax: glTexCoord4iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord4iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord4iv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..3 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord4iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord4iv;

procedure ParsePenglTexCoord4sv is
  -- Syntax: glTexCoord4sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glTexCoord4sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoord4sv_t );
  -- TODO: must be a 4 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..3 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glTexCoord4sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoord4sv;

procedure ParsePenglRasterPos2d is
  -- Syntax: glRasterPos2d( x, y : GLdouble );
  -- Source: bush_os.opengl.glRasterPos2d
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
begin
  expect( pen_glrasterpos2d_t );
  ParseFirstNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseLastNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  if isExecutingCommand then
    begin
      glRasterPos2d( GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos2d;

procedure ParsePenglRasterPos2f is
  -- Syntax: glRasterPos2f( x, y : GLfloat );
  -- Source: bush_os.opengl.glRasterPos2f
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
begin
  expect( pen_glrasterpos2f_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glfloat_t ); -- x : GLfloat
  ParseLastNumericParameter( y_val, y_type, pen_glfloat_t ); -- y : GLfloat
  if isExecutingCommand then
    begin
      glRasterPos2f( GLfloat( to_numeric( x_val ) ), GLfloat( to_numeric( y_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos2f;

procedure ParsePenglRasterPos2i is
  -- Syntax: glRasterPos2i( x, y : GLint );
  -- Source: bush_os.opengl.glRasterPos2i
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
begin
  expect( pen_glrasterpos2i_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseLastNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  if isExecutingCommand then
    begin
      glRasterPos2i( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos2i;

procedure ParsePenglRasterPos2s is
  -- Syntax: glRasterPos2s( x, y : GLshort );
  -- Source: bush_os.opengl.glRasterPos2s
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
begin
  expect( pen_glrasterpos2s_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glshort_t ); -- x : GLshort
  ParseLastNumericParameter( y_val, y_type, pen_glshort_t ); -- y : GLshort
  if isExecutingCommand then
    begin
      glRasterPos2s( GLshort( to_numeric( x_val ) ), GLshort( to_numeric( y_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos2s;

procedure ParsePenglRasterPos3d is
  -- Syntax: glRasterPos3d( x, y, z : GLdouble );
  -- Source: bush_os.opengl.glRasterPos3d
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glrasterpos3d_t );
  ParseFirstNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseNextNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  ParseLastNumericParameter( z_val, z_type, pen_gldouble_t ); -- z : GLdouble
  if isExecutingCommand then
    begin
      glRasterPos3d( GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ), GLdouble( to_numeric( z_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos3d;

procedure ParsePenglRasterPos3f is
  -- Syntax: glRasterPos3f( x, y, z : GLfloat );
  -- Source: bush_os.opengl.glRasterPos3f
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glrasterpos3f_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glfloat_t ); -- x : GLfloat
  ParseNextNumericParameter( y_val, y_type, pen_glfloat_t ); -- y : GLfloat
  ParseLastNumericParameter( z_val, z_type, pen_glfloat_t ); -- z : GLfloat
  if isExecutingCommand then
    begin
      glRasterPos3f( GLfloat( to_numeric( x_val ) ), GLfloat( to_numeric( y_val ) ), GLfloat( to_numeric( z_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos3f;

procedure ParsePenglRasterPos3i is
  -- Syntax: glRasterPos3i( x, y, z : GLint );
  -- Source: bush_os.opengl.glRasterPos3i
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glrasterpos3i_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseLastNumericParameter( z_val, z_type, pen_glint_t ); -- z : GLint
  if isExecutingCommand then
    begin
      glRasterPos3i( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLint( to_numeric( z_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos3i;

procedure ParsePenglRasterPos3s is
  -- Syntax: glRasterPos3s( x, y, z : GLshort );
  -- Source: bush_os.opengl.glRasterPos3s
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
begin
  expect( pen_glrasterpos3s_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glshort_t ); -- x : GLshort
  ParseNextNumericParameter( y_val, y_type, pen_glshort_t ); -- y : GLshort
  ParseLastNumericParameter( z_val, z_type, pen_glshort_t ); -- z : GLshort
  if isExecutingCommand then
    begin
      glRasterPos3s( GLshort( to_numeric( x_val ) ), GLshort( to_numeric( y_val ) ), GLshort( to_numeric( z_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos3s;

procedure ParsePenglRasterPos4d is
  -- Syntax: glRasterPos4d( x, y, z, w : GLdouble );
  -- Source: bush_os.opengl.glRasterPos4d
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
  w_val  : unbounded_string;
  w_type : identifier;
begin
  expect( pen_glrasterpos4d_t );
  ParseFirstNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseNextNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  ParseNextNumericParameter( z_val, z_type, pen_gldouble_t ); -- z : GLdouble
  ParseLastNumericParameter( w_val, w_type, pen_gldouble_t ); -- w : GLdouble
  if isExecutingCommand then
    begin
      glRasterPos4d( GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ), GLdouble( to_numeric( z_val ) ), GLdouble( to_numeric( w_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos4d;

procedure ParsePenglRasterPos4f is
  -- Syntax: glRasterPos4f( x, y, z, w : GLfloat );
  -- Source: bush_os.opengl.glRasterPos4f
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
  w_val  : unbounded_string;
  w_type : identifier;
begin
  expect( pen_glrasterpos4f_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glfloat_t ); -- x : GLfloat
  ParseNextNumericParameter( y_val, y_type, pen_glfloat_t ); -- y : GLfloat
  ParseNextNumericParameter( z_val, z_type, pen_glfloat_t ); -- z : GLfloat
  ParseLastNumericParameter( w_val, w_type, pen_glfloat_t ); -- w : GLfloat
  if isExecutingCommand then
    begin
      glRasterPos4f( GLfloat( to_numeric( x_val ) ), GLfloat( to_numeric( y_val ) ), GLfloat( to_numeric( z_val ) ), GLfloat( to_numeric( w_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos4f;

procedure ParsePenglRasterPos4i is
  -- Syntax: glRasterPos4i( x, y, z, w : GLint );
  -- Source: bush_os.opengl.glRasterPos4i
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
  w_val  : unbounded_string;
  w_type : identifier;
begin
  expect( pen_glrasterpos4i_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( z_val, z_type, pen_glint_t ); -- z : GLint
  ParseLastNumericParameter( w_val, w_type, pen_glint_t ); -- w : GLint
  if isExecutingCommand then
    begin
      glRasterPos4i( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLint( to_numeric( z_val ) ), GLint( to_numeric( w_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos4i;

procedure ParsePenglRasterPos4s is
  -- Syntax: glRasterPos4s( x, y, z, w : GLshort );
  -- Source: bush_os.opengl.glRasterPos4s
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  z_val  : unbounded_string;
  z_type : identifier;
  w_val  : unbounded_string;
  w_type : identifier;
begin
  expect( pen_glrasterpos4s_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glshort_t ); -- x : GLshort
  ParseNextNumericParameter( y_val, y_type, pen_glshort_t ); -- y : GLshort
  ParseNextNumericParameter( z_val, z_type, pen_glshort_t ); -- z : GLshort
  ParseLastNumericParameter( w_val, w_type, pen_glshort_t ); -- w : GLshort
  if isExecutingCommand then
    begin
      glRasterPos4s( GLshort( to_numeric( x_val ) ), GLshort( to_numeric( y_val ) ), GLshort( to_numeric( z_val ) ), GLshort( to_numeric( w_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos4s;

procedure ParsePenglRasterPos2dv is
  -- Syntax: glRasterPos2dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos2dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos2dv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..1 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos2dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos2dv;

procedure ParsePenglRasterPos2fv is
  -- Syntax: glRasterPos2fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos2fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos2fv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..1 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos2fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos2fv;

procedure ParsePenglRasterPos2iv is
  -- Syntax: glRasterPos2iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos2iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos2iv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..1 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos2iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos2iv;

procedure ParsePenglRasterPos2sv is
  -- Syntax: glRasterPos2sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos2sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos2sv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..1 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos2sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos2sv;

procedure ParsePenglRasterPos3dv is
  -- Syntax: glRasterPos3dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos3dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos3dv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..2 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos3dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos3dv;

procedure ParsePenglRasterPos3fv is
  -- Syntax: glRasterPos3fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos3fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos3fv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..2 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos3fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos3fv;

procedure ParsePenglRasterPos3iv is
  -- Syntax: glRasterPos3iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos3iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos3iv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..2 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos3iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos3iv;

procedure ParsePenglRasterPos3sv is
  -- Syntax: glRasterPos3sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos3sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos3sv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..2 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos3sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos3sv;

procedure ParsePenglRasterPos4dv is
  -- Syntax: glRasterPos4dv( v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos4dv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos4dv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Double_Array_Access := new double_array( 0..3 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos4dv( GL_Double_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos4dv;

procedure ParsePenglRasterPos4fv is
  -- Syntax: glRasterPos4fv( v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos4fv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos4fv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Float_Array_Access := new float_array( 0..3 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos4fv( GL_Float_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos4fv;

procedure ParsePenglRasterPos4iv is
  -- Syntax: glRasterPos4iv( v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos4iv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos4iv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Int_Array_Access := new int_array( 0..3 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos4iv( GL_Int_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos4iv;

procedure ParsePenglRasterPos4sv is
  -- Syntax: glRasterPos4sv( v : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glRasterPos4sv
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrasterpos4sv_t );
  -- TODO: must be a 3 value sparforte array
  --ParseSingleNumericParameter( v_val, v_type, pen_gl_short_array_ptr_t ); -- v : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array : GL_Short_Array_Access := new short_array( 0..3 );
      param_ptr   : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glRasterPos4sv( GL_Short_Array_Ptr( param_ptr ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRasterPos4sv;

procedure ParsePenglRectd is
  -- Syntax: glRectd( x1, y1, x2, y2 : GLdouble );
  -- Source: bush_os.opengl.glRectd
  x1_val  : unbounded_string;
  x1_type : identifier;
  y1_val  : unbounded_string;
  y1_type : identifier;
  x2_val  : unbounded_string;
  x2_type : identifier;
  y2_val  : unbounded_string;
  y2_type : identifier;
begin
  expect( pen_glrectd_t );
  ParseFirstNumericParameter( x1_val, x1_type, pen_gldouble_t ); -- x1 : GLdouble
  ParseNextNumericParameter( y1_val, y1_type, pen_gldouble_t ); -- y1 : GLdouble
  ParseNextNumericParameter( x2_val, x2_type, pen_gldouble_t ); -- x2 : GLdouble
  ParseLastNumericParameter( y2_val, y2_type, pen_gldouble_t ); -- y2 : GLdouble
  if isExecutingCommand then
    begin
      glRectd( GLdouble( to_numeric( x1_val ) ), GLdouble( to_numeric( y1_val ) ), GLdouble( to_numeric( x2_val ) ), GLdouble( to_numeric( y2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRectd;

procedure ParsePenglRectf is
  -- Syntax: glRectf( x1, y1, x2, y2 : GLfloat );
  -- Source: bush_os.opengl.glRectf
  x1_val  : unbounded_string;
  x1_type : identifier;
  y1_val  : unbounded_string;
  y1_type : identifier;
  x2_val  : unbounded_string;
  x2_type : identifier;
  y2_val  : unbounded_string;
  y2_type : identifier;
begin
  expect( pen_glrectf_t );
  ParseFirstNumericParameter( x1_val, x1_type, pen_glfloat_t ); -- x1 : GLfloat
  ParseNextNumericParameter( y1_val, y1_type, pen_glfloat_t ); -- y1 : GLfloat
  ParseNextNumericParameter( x2_val, x2_type, pen_glfloat_t ); -- x2 : GLfloat
  ParseLastNumericParameter( y2_val, y2_type, pen_glfloat_t ); -- y2 : GLfloat
  if isExecutingCommand then
    begin
      glRectf( GLfloat( to_numeric( x1_val ) ), GLfloat( to_numeric( y1_val ) ), GLfloat( to_numeric( x2_val ) ), GLfloat( to_numeric( y2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRectf;

procedure ParsePenglRecti is
  -- Syntax: glRecti( x1, y1, x2, y2 : GLint );
  -- Source: bush_os.opengl.glRecti
  x1_val  : unbounded_string;
  x1_type : identifier;
  y1_val  : unbounded_string;
  y1_type : identifier;
  x2_val  : unbounded_string;
  x2_type : identifier;
  y2_val  : unbounded_string;
  y2_type : identifier;
begin
  expect( pen_glrecti_t );
  ParseFirstNumericParameter( x1_val, x1_type, pen_glint_t ); -- x1 : GLint
  ParseNextNumericParameter( y1_val, y1_type, pen_glint_t ); -- y1 : GLint
  ParseNextNumericParameter( x2_val, x2_type, pen_glint_t ); -- x2 : GLint
  ParseLastNumericParameter( y2_val, y2_type, pen_glint_t ); -- y2 : GLint
  if isExecutingCommand then
    begin
      glRecti( GLint( to_numeric( x1_val ) ), GLint( to_numeric( y1_val ) ), GLint( to_numeric( x2_val ) ), GLint( to_numeric( y2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRecti;

procedure ParsePenglRects is
  -- Syntax: glRects( x1, y1, x2, y2 : GLshort );
  -- Source: bush_os.opengl.glRects
  x1_val  : unbounded_string;
  x1_type : identifier;
  y1_val  : unbounded_string;
  y1_type : identifier;
  x2_val  : unbounded_string;
  x2_type : identifier;
  y2_val  : unbounded_string;
  y2_type : identifier;
begin
  expect( pen_glrects_t );
  ParseFirstNumericParameter( x1_val, x1_type, pen_glshort_t ); -- x1 : GLshort
  ParseNextNumericParameter( y1_val, y1_type, pen_glshort_t ); -- y1 : GLshort
  ParseNextNumericParameter( x2_val, x2_type, pen_glshort_t ); -- x2 : GLshort
  ParseLastNumericParameter( y2_val, y2_type, pen_glshort_t ); -- y2 : GLshort
  if isExecutingCommand then
    begin
      glRects( GLshort( to_numeric( x1_val ) ), GLshort( to_numeric( y1_val ) ), GLshort( to_numeric( x2_val ) ), GLshort( to_numeric( y2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRects;

procedure ParsePenglRectdv is
  -- Syntax: glRectdv( v1, v2 : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glRectdv
  v1_val  : unbounded_string;
--  v1_type : identifier;
  v2_val  : unbounded_string;
--  v2_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrectdv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseFirstNumericParameter( v1_val, v1_type, pen_gl_double_array_ptr_t ); -- v1 : GL_Double_Array_Ptr
  --ParseLastNumericParameter( v2_val, v2_type, pen_gl_double_array_ptr_t ); -- v2 : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      param_array1: GL_Double_Array_Access := new double_array( 0..1 );
      param_ptr1  : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array1 );
      param_array2: GL_Double_Array_Access := new double_array( 0..1 );
      param_ptr2  : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array2 );
    begin
      -- TODO: load sparforte array into the array param_array
      glRectdv( GL_Double_Array_Ptr( param_ptr1 ), GL_Double_Array_Ptr( param_ptr2 ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRectdv;

procedure ParsePenglRectfv is
  -- Syntax: glRectfv( v1, v2 : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glRectfv
  v1_val  : unbounded_string;
--  v1_type : identifier;
  v2_val  : unbounded_string;
--  v2_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrectfv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseFirstNumericParameter( v1_val, v1_type, pen_gl_float_array_ptr_t ); -- v1 : GL_Float_Array_Ptr
  --ParseLastNumericParameter( v2_val, v2_type, pen_gl_float_array_ptr_t ); -- v2 : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      param_array1: GL_Double_Array_Access := new double_array( 0..1 );
      param_ptr1  : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array1 );
      param_array2: GL_Double_Array_Access := new double_array( 0..1 );
      param_ptr2  : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array2 );
    begin
      -- TODO: load sparforte array into the array param_array
      glRectfv( GL_Float_Array_Ptr( param_ptr1 ), GL_Float_Array_Ptr( param_ptr2 ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRectfv;

procedure ParsePenglRectiv is
  -- Syntax: glRectiv( v1, v2 : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glRectiv
  v1_val  : unbounded_string;
--  v1_type : identifier;
  v2_val  : unbounded_string;
--  v2_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrectiv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseFirstNumericParameter( v1_val, v1_type, pen_gl_int_array_ptr_t ); -- v1 : GL_Int_Array_Ptr
  --ParseLastNumericParameter( v2_val, v2_type, pen_gl_int_array_ptr_t ); -- v2 : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      param_array1: GL_Int_Array_Access := new int_array( 0..1 );
      param_ptr1  : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array1 );
      param_array2: GL_Int_Array_Access := new int_array( 0..1 );
      param_ptr2  : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array2 );
    begin
      -- TODO: load sparforte array into the array param_array
      glRectiv( GL_Int_Array_Ptr( param_ptr1 ), GL_Int_Array_Ptr( param_ptr2 ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRectiv;

procedure ParsePenglRectsv is
  -- Syntax: glRectsv( v1, v2 : GL_Short_Array_Ptr );
  -- Source: bush_os.opengl.glRectsv
  v1_val  : unbounded_string;
--  v1_type : identifier;
  v2_val  : unbounded_string;
--  v2_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glrectsv_t );
  -- TODO: must be a 2 value sparforte array
  --ParseFirstNumericParameter( v1_val, v1_type, pen_gl_short_array_ptr_t ); -- v1 : GL_Short_Array_Ptr
  --ParseLastNumericParameter( v2_val, v2_type, pen_gl_short_array_ptr_t ); -- v2 : GL_Short_Array_Ptr
  if isExecutingCommand then
    declare
      param_array1: GL_Short_Array_Access := new short_array( 0..1 );
      param_ptr1  : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array1 );
      param_array2: GL_Short_Array_Access := new short_array( 0..1 );
      param_ptr2  : GL_Short_Array_Ptr := GL_Short_Array_Conv.To_Address( param_array2 );
    begin
      -- TODO: load sparforte array into the array param_array
      glRectsv( param_ptr1, param_ptr2 );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglRectsv;

procedure ParsePenglVertexPointer is
  -- Syntax: glVertexPointer( size : GLint; kind : GLtypes; stride : GLsizei; ptr : System.address );
  -- Source: bush_os.opengl.glVertexPointer
  size_val  : unbounded_string;
  size_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  stride_val  : unbounded_string;
  stride_type : identifier;
  ptr_val  : unbounded_string;
--  ptr_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glvertexpointer_t );
  ParseFirstNumericParameter( size_val, size_type, pen_glint_t ); -- size : GLint
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  ParseNextNumericParameter( stride_val, stride_type, pen_glsizei_t ); -- stride : GLsizei
  -- TODO: returns a pointer to a C vertex structure, but we don't support that in SparForte
  --ParseLastNumericParameter( ptr_val, ptr_type, pen_system.address_t ); -- ptr : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glVertexPointer( GLint( to_numeric( size_val ) ), GLtypes( to_numeric( kind_val ) ), GLsizei( to_numeric( stride_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglVertexPointer;

procedure ParsePenglNormalPointer is
  -- Syntax: glNormalPointer( kind : GLtypes; stride : GLsizei; ptr : System.address );
  -- Source: bush_os.opengl.glNormalPointer
  kind_val  : unbounded_string;
  kind_type : identifier;
  stride_val  : unbounded_string;
  stride_type : identifier;
  ptr_val  : unbounded_string;
--  ptr_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glnormalpointer_t );
  ParseFirstNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  ParseNextNumericParameter( stride_val, stride_type, pen_glsizei_t ); -- stride : GLsizei
  -- TODO: returns a pointer to a C normals array, but we don't support that in SparForte
  -- ParseLastNumericParameter( ptr_val, ptr_type, pen_system.address_t ); -- ptr : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glNormalPointer( GLtypes( to_numeric( kind_val ) ), GLsizei( to_numeric( stride_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglNormalPointer;

procedure ParsePenglColorPointer is
  -- Syntax: glColorPointer( size : GLint; kind : GLtypes; stride : GLsizei; ptr : System.address );
  -- Source: bush_os.opengl.glColorPointer
  size_val  : unbounded_string;
  size_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  stride_val  : unbounded_string;
  stride_type : identifier;
  ptr_val  : unbounded_string;
--  ptr_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glcolorpointer_t );
  ParseFirstNumericParameter( size_val, size_type, pen_glint_t ); -- size : GLint
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  ParseNextNumericParameter( stride_val, stride_type, pen_glsizei_t ); -- stride : GLsizei
  -- TODO: returns a pointer to a C component array, but we don't support that in SparForte
  -- ParseLastNumericParameter( ptr_val, ptr_type, pen_system.address_t ); -- ptr : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glColorPointer( GLint( to_numeric( size_val ) ), GLtypes( to_numeric( kind_val ) ), GLsizei( to_numeric( stride_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColorPointer;

procedure ParsePenglIndexPointer is
  -- Syntax: glIndexPointer( kind : GLtypes; stride : GLsizei; ptr : System.address );
  -- Source: bush_os.opengl.glIndexPointer
  kind_val  : unbounded_string;
  kind_type : identifier;
  stride_val  : unbounded_string;
  stride_type : identifier;
  ptr_val  : unbounded_string;
--  ptr_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glindexpointer_t );
  ParseFirstNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  ParseNextNumericParameter( stride_val, stride_type, pen_glsizei_t ); -- stride : GLsizei
  -- TODO: returns a pointer to a C color index array, but we don't support that in SparForte
  --ParseLastNumericParameter( ptr_val, ptr_type, pen_system.address_t ); -- ptr : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glIndexPointer( GLtypes( to_numeric( kind_val ) ), GLsizei( to_numeric( stride_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIndexPointer;

procedure ParsePenglTexCoordPointer is
  -- Syntax: glTexCoordPointer( size : GLint; kind : GLtypes; stride : GLsizei; ptr : System.address );
  -- Source: bush_os.opengl.glTexCoordPointer
  size_val  : unbounded_string;
  size_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  stride_val  : unbounded_string;
  stride_type : identifier;
  ptr_val  : unbounded_string;
--  ptr_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexcoordpointer_t );
  ParseFirstNumericParameter( size_val, size_type, pen_glint_t ); -- size : GLint
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  ParseNextNumericParameter( stride_val, stride_type, pen_glsizei_t ); -- stride : GLsizei
  -- TODO: returns a pointer to a C texture coord array, but we don't support that in SparForte
  --ParseLastNumericParameter( ptr_val, ptr_type, pen_system.address_t ); -- ptr : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glTexCoordPointer( GLint( to_numeric( size_val ) ), GLtypes( to_numeric( kind_val ) ), GLsizei( to_numeric( stride_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexCoordPointer;

procedure ParsePenglEdgeFlagPointer is
  -- Syntax: glEdgeFlagPointer( stride : GLsizei; ptr : System.address );
  -- Source: bush_os.opengl.glEdgeFlagPointer
  stride_val  : unbounded_string;
  stride_type : identifier;
  ptr_val  : unbounded_string;
--  ptr_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gledgeflagpointer_t );
  ParseFirstNumericParameter( stride_val, stride_type, pen_glsizei_t ); -- stride : GLsizei
  -- TODO: returns a pointer to a C boolean edge flag array, but we don't support that in SparForte
  -- ParseLastNumericParameter( ptr_val, ptr_type, pen_system.address_t ); -- ptr : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glEdgeFlagPointer( GLsizei( to_numeric( stride_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEdgeFlagPointer;

procedure ParsePenglGetPointerv is
  -- Syntax: glGetPointerv( pname : GLvertexarrays; params : in out System.address );
  -- Source: bush_os.opengl.glGetPointerv
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgetpointerv_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glvertexarrays_t ); -- pname : GLvertexarrays
  -- TODO: returns a pointer to a C structure, but we don't support that in SparForte
  -- no man page for this call?
  -- ParseLastNumericParameter( params_val, params_type, pen_system.address_t ); -- params : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glGetPointerv( GLvertexarrays( to_numeric( pname_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetPointerv;

procedure ParsePenglArrayElement is
  -- Syntax: glArrayElement( i : GLint );
  -- Source: bush_os.opengl.glArrayElement
  i_val  : unbounded_string;
  i_type : identifier;
begin
  expect( pen_glarrayelement_t );
  ParseSingleNumericParameter( i_val, i_type, pen_glint_t ); -- i : GLint
  if isExecutingCommand then
    begin
      glArrayElement( GLint( to_numeric( i_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglArrayElement;

procedure ParsePenglDrawArrays is
  -- Syntax: glDrawArrays( mode : GLprimitives; first : GLint; count : GLsizei );
  -- Source: bush_os.opengl.glDrawArrays
  mode_val  : unbounded_string;
  mode_type : identifier;
  first_val  : unbounded_string;
  first_type : identifier;
  count_val  : unbounded_string;
  count_type : identifier;
begin
  expect( pen_gldrawarrays_t );
  ParseFirstNumericParameter( mode_val, mode_type, pen_glprimitives_t ); -- mode : GLprimitives
  ParseNextNumericParameter( first_val, first_type, pen_glint_t ); -- first : GLint
  ParseLastNumericParameter( count_val, count_type, pen_glsizei_t ); -- count : GLsizei
  if isExecutingCommand then
    begin
      glDrawArrays( GLprimitives( to_numeric( mode_val ) ), GLint( to_numeric( first_val ) ), GLsizei( to_numeric( count_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDrawArrays;

procedure ParsePenglDrawElements is
  -- Syntax: glDrawElements( mode : GLtypes; count : GLsizei; kind : GLprimitives; indices : System.address );
  -- Source: bush_os.opengl.glDrawElements
  mode_val  : unbounded_string;
  mode_type : identifier;
  count_val  : unbounded_string;
  count_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  indices_val  : unbounded_string;
--  indices_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gldrawelements_t );
  ParseFirstNumericParameter( mode_val, mode_type, pen_gltypes_t ); -- mode : GLtypes
  ParseNextNumericParameter( count_val, count_type, pen_glsizei_t ); -- count : GLsizei
  ParseNextNumericParameter( kind_val, kind_type, pen_glprimitives_t ); -- kind : GLprimitives
  -- TODO: must be am array of geometric primitives, but at this time SparForte doesn't
  -- have arrays with arbitrary bounds.  Also, no type defined yet for primitives.
  --ParseLastNumericParameter( indices_val, indices_type, pen_system.address_t ); -- indices : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glDrawElements( GLtypes( to_numeric( mode_val ) ), GLsizei( to_numeric( count_val ) ), GLprimitives( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDrawElements;

procedure ParsePenglInterleavedArrays is
  -- Syntax: glInterleavedArrays( format : GLvertexarrays; stride : GLsizei; pointer : System.address );
  -- Source: bush_os.opengl.glInterleavedArrays
  format_val  : unbounded_string;
  format_type : identifier;
  stride_val  : unbounded_string;
  stride_type : identifier;
  pointer_val  : unbounded_string;
--  pointer_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glinterleavedarrays_t );
  ParseFirstNumericParameter( format_val, format_type, pen_glvertexarrays_t ); -- format : GLvertexarrays
  ParseNextNumericParameter( stride_val, stride_type, pen_glsizei_t ); -- stride : GLsizei
  -- TODO: the interleaved arrays can't be easily defined in SparForte at the time of writing
  --ParseLastNumericParameter( pointer_val, pointer_type, pen_system.address_t ); -- pointer : System.address
  if isExecutingCommand then
    declare
      dummy_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( dummy_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glInterleavedArrays( GLvertexarrays( to_numeric( format_val ) ), GLsizei( to_numeric( stride_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglInterleavedArrays;

procedure ParsePenglShadeModel is
  -- Syntax: glShadeModel( mode : GLlighting );
  -- Source: bush_os.opengl.glShadeModel
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glshademodel_t );
  ParseSingleNumericParameter( mode_val, mode_type, pen_gllighting_t ); -- mode : GLlighting
  if isExecutingCommand then
    begin
      glShadeModel( GLlighting( to_numeric( mode_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglShadeModel;

procedure ParsePenglLightf is
  -- Syntax: glLightf( light : GLenum; pname : GLlighting; param : GLfloat );
  -- Source: bush_os.opengl.glLightf
  light_val  : unbounded_string;
  light_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gllightf_t );
  ParseFirstNumericParameter( light_val, light_type, pen_glenum_t ); -- light : GLenum
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    begin
      glLightf( GLenum( to_numeric( light_val ) ), GLlighting( to_numeric( pname_val ) ), GLfloat( to_numeric( param_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLightf;

procedure ParsePenglLighti is
  -- Syntax: glLighti( light : GLenum; pname : GLlighting; param : GLint );
  -- Source: bush_os.opengl.glLighti
  light_val  : unbounded_string;
  light_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gllighti_t );
  ParseFirstNumericParameter( light_val, light_type, pen_glenum_t ); -- light : GLenum
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    begin
      glLighti( GLenum( to_numeric( light_val ) ), GLlighting( to_numeric( pname_val ) ), GLint( to_numeric( param_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLighti;

procedure ParsePenglLightfv is
  -- Syntax: glLightfv( light : GLenum; pname : GLlighting; params : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glLightfv
  light_val  : unbounded_string;
  light_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gllightfv_t );
  ParseFirstNumericParameter( light_val, light_type, pen_glenum_t ); -- light : GLenum
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  -- TODO: must be a multiple value sparforte array
  --ParseLastNumericParameter( params_val, params_type, pen_gl_float_array_ptr_t ); -- params : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glLightfv( GLenum( to_numeric( light_val ) ), GLlighting( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLightfv;

procedure ParsePenglLightiv is
  -- Syntax: glLightiv( light : GLenum; pname : GLlighting; params : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glLightiv
  light_val  : unbounded_string;
  light_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gllightiv_t );
  ParseFirstNumericParameter( light_val, light_type, pen_glenum_t ); -- light : GLenum
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  -- TODO: must be a multiple value sparforte array
  -- ParseLastNumericParameter( params_val, params_type, pen_gl_int_array_ptr_t ); -- params : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glLightiv( GLenum( to_numeric( light_val ) ), GLlighting( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLightiv;

procedure ParsePenglGetLightfv is
  -- Syntax: glGetLightfv( light : GLenum; pname : GLlighting; params : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glGetLightfv
  light_val  : unbounded_string;
  light_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgetlightfv_t );
  ParseFirstNumericParameter( light_val, light_type, pen_glenum_t ); -- light : GLenum
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  -- TODO: returns a pointer to a C array, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gl_float_array_ptr_t ); -- params : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glGetLightfv( GLenum( to_numeric( light_val ) ), GLlighting( to_numeric( pname_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetLightfv;

procedure ParsePenglGetLightiv is
  -- Syntax: glGetLightiv( light : GLenum; pname : GLlighting; params : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glGetLightiv
  light_val  : unbounded_string;
  light_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgetlightiv_t );
  ParseFirstNumericParameter( light_val, light_type, pen_glenum_t ); -- light : GLenum
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  -- TODO: returns a pointer to a C array, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gl_int_array_ptr_t ); -- params : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glGetLightiv( GLenum( to_numeric( light_val ) ), GLlighting( to_numeric( pname_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetLightiv;

procedure ParsePenglLightModelf is
  -- Syntax: glLightModelf( pname : GLenum; param : GLfloat );
  -- Source: bush_os.opengl.glLightModelf
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gllightmodelf_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
  ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    begin
      glLightModelf( GLenum( to_numeric( pname_val ) ), GLfloat( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLightModelf;

procedure ParsePenglLightModeli is
  -- Syntax: glLightModeli( pname : GLenum; param : GLint );
  -- Source: bush_os.opengl.glLightModeli
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gllightmodeli_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
  ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    begin
      glLightModeli( GLenum( to_numeric( pname_val ) ), GLint( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLightModeli;

procedure ParsePenglLightModelfv is
  -- Syntax: glLightModelfv( pname : GLenum; params : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glLightModelfv
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gllightmodelfv_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
  -- TODO: takes a multivalue C float array of parameters
  --ParseLastNumericParameter( params_val, params_type, pen_gl_float_array_ptr_t ); -- params : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glLightModelfv( GLenum( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLightModelfv;

procedure ParsePenglLightModeliv is
  -- Syntax: glLightModeliv( pname : GLenum; params : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glLightModeliv
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gllightmodeliv_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gl_int_array_ptr_t ); -- params : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glLightModeliv( GLenum( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLightModeliv;

procedure ParsePenglMaterialf is
  -- Syntax: glMaterialf( face : GLbuffers; pname : GLlighting; param : GLfloat );
  -- Source: bush_os.opengl.glMaterialf
  face_val  : unbounded_string;
  face_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_glmaterialf_t );
  ParseFirstNumericParameter( face_val, face_type, pen_glbuffers_t ); -- face : GLbuffers
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    begin
      glMaterialf( GLbuffers( to_numeric( face_val ) ), GLlighting( to_numeric( pname_val ) ), GLfloat( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMaterialf;

procedure ParsePenglMateriali is
  -- Syntax: glMateriali( face : GLbuffers; pname : GLlighting; param : GLint );
  -- Source: bush_os.opengl.glMateriali
  face_val  : unbounded_string;
  face_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_glmateriali_t );
  ParseFirstNumericParameter( face_val, face_type, pen_glbuffers_t ); -- face : GLbuffers
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    begin
      glMateriali( GLbuffers( to_numeric( face_val ) ), GLlighting( to_numeric( pname_val ) ), GLint( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMateriali;

procedure ParsePenglMaterialfv is
  -- Syntax: glMaterialfv( face : GLbuffers; pname : GLlighting; param : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glMaterialfv
  face_val  : unbounded_string;
  face_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
--  param_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glmaterialfv_t );
  ParseFirstNumericParameter( face_val, face_type, pen_glbuffers_t ); -- face : GLbuffers
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_gl_float_array_ptr_t ); -- param : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: 4 seems to be the maximum size (currently) for the return data
      param_array : GL_Float_Array_Access := new float_array( 0..3 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glMaterialfv( GLbuffers( to_numeric( face_val ) ), GLlighting( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMaterialfv;

procedure ParsePenglMaterialiv is
  -- Syntax: glMaterialiv( face : GLbuffers; pname : GLlighting; param : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glMaterialiv
  face_val  : unbounded_string;
  face_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
--  param_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glmaterialiv_t );
  ParseFirstNumericParameter( face_val, face_type, pen_glbuffers_t ); -- face : GLbuffers
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_gl_int_array_ptr_t ); -- param : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glMaterialiv( GLbuffers( to_numeric( face_val ) ), GLlighting( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMaterialiv;

procedure ParsePenglGetMaterialfv is
  -- Syntax: glGetMaterialfv( face : GLbuffers; pname : GLlighting; param : in out GLfloat );
  -- Source: bush_os.opengl.glGetMaterialfv
  face_val  : unbounded_string;
  face_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
--  param_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgetmaterialfv_t );
  ParseFirstNumericParameter( face_val, face_type, pen_glbuffers_t ); -- face : GLbuffers
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glGetMaterialfv( GLbuffers( to_numeric( face_val ) ), GLlighting( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetMaterialfv;

procedure ParsePenglGetMaterialiv is
  -- Syntax: glGetMaterialiv( face : GLbuffers; pname : GLlighting; param : in out GLint );
  -- Source: bush_os.opengl.glGetMaterialiv
  face_val  : unbounded_string;
  face_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
--  param_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgetmaterialiv_t );
  ParseFirstNumericParameter( face_val, face_type, pen_glbuffers_t ); -- face : GLbuffers
  ParseNextNumericParameter( pname_val, pname_type, pen_gllighting_t ); -- pname : GLlighting
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    declare
      -- TODO: 4 seems to be the maximum size (currently) for the return data
      param_array : GL_Int_Array_Access := new int_array( 0..3 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      -- TODO: load sparforte array into the array param_array
      glGetMaterialiv( GLbuffers( to_numeric( face_val ) ), GLlighting( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetMaterialiv;

procedure ParsePenglColorMaterial is
  -- Syntax: glColorMaterial( face : GLbuffers; mode : GLlighting );
  -- Source: bush_os.opengl.glColorMaterial
  face_val  : unbounded_string;
  face_type : identifier;
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glcolormaterial_t );
  ParseFirstNumericParameter( face_val, face_type, pen_glbuffers_t ); -- face : GLbuffers
  ParseLastNumericParameter( mode_val, mode_type, pen_gllighting_t ); -- mode : GLlighting
  if isExecutingCommand then
    begin
      glColorMaterial( GLbuffers( to_numeric( face_val ) ), GLlighting( to_numeric( mode_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColorMaterial;

procedure ParsePenglPixelZoom is
  -- Syntax: glPixelZoom( xfactor, yfactor : GLfloat );
  -- Source: bush_os.opengl.glPixelZoom
  xfactor_val  : unbounded_string;
  xfactor_type : identifier;
  yfactor_val  : unbounded_string;
  yfactor_type : identifier;
begin
  expect( pen_glpixelzoom_t );
  ParseFirstNumericParameter( xfactor_val, xfactor_type, pen_glfloat_t ); -- xfactor : GLfloat
  ParseLastNumericParameter( yfactor_val, yfactor_type, pen_glfloat_t ); -- yfactor : GLfloat
  if isExecutingCommand then
    begin
      glPixelZoom( GLfloat( to_numeric( xfactor_val ) ), GLfloat( to_numeric( yfactor_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPixelZoom;

procedure ParsePenglPixelStoref is
  -- Syntax: glPixelStoref( pname : GLpixelmode; param : GLfloat );
  -- Source: bush_os.opengl.glPixelStoref
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_glpixelstoref_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glpixelmode_t ); -- pname : GLpixelmode
  ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    begin
      glPixelStoref( GLpixelmode( to_numeric( pname_val ) ), GLfloat( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPixelStoref;

procedure ParsePenglPixelStorei is
  -- Syntax: glPixelStorei( pname : GLpixelmode; param : GLint );
  -- Source: bush_os.opengl.glPixelStorei
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_glpixelstorei_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glpixelmode_t ); -- pname : GLpixelmode
  ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    begin
      glPixelStorei( GLpixelmode( to_numeric( pname_val ) ), GLint( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPixelStorei;

procedure ParsePenglPixelTransferf is
  -- Syntax: glPixelTransferf( pname : GLpixelmode; param : GLfloat );
  -- Source: bush_os.opengl.glPixelTransferf
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_glpixeltransferf_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glpixelmode_t ); -- pname : GLpixelmode
  ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    begin
      glPixelTransferf( GLpixelmode( to_numeric( pname_val ) ), GLfloat( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPixelTransferf;

procedure ParsePenglPixelTransferi is
  -- Syntax: glPixelTransferi( pname : GLpixelmode; param : GLint );
  -- Source: bush_os.opengl.glPixelTransferi
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_glpixeltransferi_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glpixelmode_t ); -- pname : GLpixelmode
  ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    begin
      glPixelTransferi( GLpixelmode( to_numeric( pname_val ) ), GLint( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPixelTransferi;

procedure ParsePenglPixelMapfv is
  -- Syntax: glPixelMapfv( map : GLpixelmode; mapsize : GLsizei; values : in out GLfloat );
  -- Source: bush_os.opengl.glPixelMapfv
  map_val  : unbounded_string;
  map_type : identifier;
  mapsize_val  : unbounded_string;
  mapsize_type : identifier;
  values_val  : unbounded_string;
--  values_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glpixelmapfv_t );
  ParseFirstNumericParameter( map_val, map_type, pen_glpixelmode_t ); -- map : GLpixelmode
  ParseNextNumericParameter( mapsize_val, mapsize_type, pen_glsizei_t ); -- mapsize : GLsizei
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
 -- ParseLastNumericParameter( values_val, values_type, pen_glfloat_t ); -- values : GLfloat
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glPixelMapfv( GLpixelmode( to_numeric( map_val ) ), GLsizei( to_numeric( mapsize_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPixelMapfv;

procedure ParsePenglPixelMapuiv is
  -- Syntax: glPixelMapuiv( map : GLpixelmode; mapsize : GLsizei; values : in out GLuint );
  -- Source: bush_os.opengl.glPixelMapuiv
  map_val  : unbounded_string;
  map_type : identifier;
  mapsize_val  : unbounded_string;
  mapsize_type : identifier;
  values_val  : unbounded_string;
  --values_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glpixelmapuiv_t );
  ParseFirstNumericParameter( map_val, map_type, pen_glpixelmode_t ); -- map : GLpixelmode
  ParseNextNumericParameter( mapsize_val, mapsize_type, pen_glsizei_t ); -- mapsize : GLsizei
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( values_val, values_type, pen_gluint_t ); -- values : GLuint
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_UInt_Array_Access := new uint_array( 0..0 );
      param_ptr   : GL_UInt_Array_Ptr := GL_UInt_Array_Conv.To_Address( param_array );
    begin
      glPixelMapuiv( GLpixelmode( to_numeric( map_val ) ), GLsizei( to_numeric( mapsize_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPixelMapuiv;

procedure ParsePenglPixelMapusv is
  -- Syntax: glPixelMapusv( map : GLpixelmode; mapsize : GLsizei; values : in out GLushort );
  -- Source: bush_os.opengl.glPixelMapusv
  map_val  : unbounded_string;
  map_type : identifier;
  mapsize_val  : unbounded_string;
  mapsize_type : identifier;
  values_val  : unbounded_string;
  --values_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glpixelmapusv_t );
  ParseFirstNumericParameter( map_val, map_type, pen_glpixelmode_t ); -- map : GLpixelmode
  ParseNextNumericParameter( mapsize_val, mapsize_type, pen_glsizei_t ); -- mapsize : GLsizei
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( values_val, values_type, pen_glushort_t ); -- values : GLushort
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_UShort_Array_Access := new ushort_array( 0..0 );
      param_ptr   : GL_UShort_Array_Ptr := GL_UShort_Array_Conv.To_Address( param_array );
    begin
      glPixelMapusv( GLpixelmode( to_numeric( map_val ) ), GLsizei( to_numeric( mapsize_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPixelMapusv;

procedure ParsePenglGetPixelMapfv is
  -- Syntax: glGetPixelMapfv( map : GLpixelmode; values : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glGetPixelMapfv
  map_val  : unbounded_string;
  map_type : identifier;
  values_val  : unbounded_string;
--  values_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgetpixelmapfv_t );
  ParseFirstNumericParameter( map_val, map_type, pen_glpixelmode_t ); -- map : GLpixelmode
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( values_val, values_type, pen_gl_float_array_ptr_t ); -- values : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glGetPixelMapfv( GLpixelmode( to_numeric( map_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetPixelMapfv;

procedure ParsePenglGetPixelMapuiv is
  -- Syntax: glGetPixelMapuiv( map : GLpixelmode; values : GL_UInt_Array_Ptr );
  -- Source: bush_os.opengl.glGetPixelMapuiv
  map_val  : unbounded_string;
  map_type : identifier;
  values_val  : unbounded_string;
--  values_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgetpixelmapuiv_t );
  ParseFirstNumericParameter( map_val, map_type, pen_glpixelmode_t ); -- map : GLpixelmode
  -- TODO: takes a multivalue C uint array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( values_val, values_type, pen_gl_uint_array_ptr_t ); -- values : GL_UInt_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_UInt_Array_Access := new uint_array( 0..0 );
      param_ptr   : GL_UInt_Array_Ptr := GL_UInt_Array_Conv.To_Address( param_array );
    begin
      glGetPixelMapuiv( GLpixelmode( to_numeric( map_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetPixelMapuiv;

procedure ParsePenglGetPixelMapusv is
  -- Syntax: glGetPixelMapusv( map : GLpixelmode; values : GL_UShort_Array_Ptr );
  -- Source: bush_os.opengl.glGetPixelMapusv
  map_val  : unbounded_string;
  map_type : identifier;
  values_val  : unbounded_string;
--  values_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgetpixelmapusv_t );
  ParseFirstNumericParameter( map_val, map_type, pen_glpixelmode_t ); -- map : GLpixelmode
  -- TODO: takes a multivalue C ushort array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( values_val, values_type, pen_gl_ushort_array_ptr_t ); -- values : GL_UShort_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_UShort_Array_Access := new ushort_array( 0..0 );
      param_ptr   : GL_UShort_Array_Ptr := GL_UShort_Array_Conv.To_Address( param_array );
    begin
      glGetPixelMapusv( GLpixelmode( to_numeric( map_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetPixelMapusv;

procedure ParsePenglBitmap is
  -- Syntax: glBitmap( width, height : GLsizei; xorig, yorig : GLfloat; xmove, ymove : GLfloat; bitmap : GL_UByte_Array_Ptr );
  -- Source: bush_os.opengl.glBitmap
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  xorig_val  : unbounded_string;
  xorig_type : identifier;
  yorig_val  : unbounded_string;
  yorig_type : identifier;
  xmove_val  : unbounded_string;
  xmove_type : identifier;
  ymove_val  : unbounded_string;
  ymove_type : identifier;
  bitmap_val  : unbounded_string;
--  bitmap_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glbitmap_t );
  ParseFirstNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( xorig_val, xorig_type, pen_glfloat_t ); -- xorig : GLfloat
  ParseNextNumericParameter( yorig_val, yorig_type, pen_glfloat_t ); -- yorig : GLfloat
  ParseNextNumericParameter( xmove_val, xmove_type, pen_glfloat_t ); -- xmove : GLfloat
  ParseNextNumericParameter( ymove_val, ymove_type, pen_glfloat_t ); -- ymove : GLfloat
  -- TODO: takes a multivalue C ubyte array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( bitmap_val, bitmap_type, pen_gl_ubyte_array_ptr_t ); -- bitmap : GL_UByte_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_UByte_Array_Access := new ubyte_array( 0..0 );
      param_ptr   : GL_UByte_Array_Ptr := GL_UByte_Array_Conv.To_Address( param_array );
    begin
      glBitmap( GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLfloat( to_numeric( xorig_val ) ), GLfloat( to_numeric( yorig_val ) ), GLfloat( to_numeric( xmove_val ) ), GLfloat( to_numeric( ymove_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglBitmap;

procedure ParsePenglReadPixels is
  -- Syntax: glReadPixels( x, y : GLint; width, height : GLsizei; format : GLenum; kind : GLtypes; pixels : System.address );
  -- Source: bush_os.opengl.glReadPixels
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  pixels_val  : unbounded_string;
--  pixels_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glreadpixels_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glenum_t ); -- format : GLenum
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: we can probably do this but we'll need to size a destination array.  Come back to this later
  --ParseLastNumericParameter( pixels_val, pixels_type, pen_system.address_t ); -- pixels : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glReadPixels( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLenum( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglReadPixels;

procedure ParsePenglDrawPixels is
  -- Syntax: glDrawPixels( width, height : GLsizei; format : GLbuffers; kind : GLtypes; pixels : System.address );
  -- Source: bush_os.opengl.glDrawPixels
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  pixels_val  : unbounded_string;
--  pixels_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gldrawpixels_t );
  ParseFirstNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: we can probably do this but we'll need to size and populate a source array.  Come back to this later
  --ParseLastNumericParameter( pixels_val, pixels_type, pen_system.address_t ); -- pixels : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glDrawPixels( GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDrawPixels;

procedure ParsePenglCopyPixels is
  -- Syntax: glCopyPixels( x, y : GLint; width, height : GLsizei; kind : GLbuffers );
  -- Source: bush_os.opengl.glCopyPixels
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
begin
  expect( pen_glcopypixels_t );
  ParseFirstNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseLastNumericParameter( kind_val, kind_type, pen_glbuffers_t ); -- kind : GLbuffers
  if isExecutingCommand then
    begin
      glCopyPixels( GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLbuffers( to_numeric( kind_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyPixels;

procedure ParsePenglStencilFunc is
  -- Syntax: glStencilFunc( func : GLenum; ref : GLint; mask : GLuint );
  -- Source: bush_os.opengl.glStencilFunc
  func_val  : unbounded_string;
  func_type : identifier;
  ref_val  : unbounded_string;
  ref_type : identifier;
  mask_val  : unbounded_string;
  mask_type : identifier;
begin
  expect( pen_glstencilfunc_t );
  ParseFirstNumericParameter( func_val, func_type, pen_glenum_t ); -- func : GLenum
  ParseNextNumericParameter( ref_val, ref_type, pen_glint_t ); -- ref : GLint
  ParseLastNumericParameter( mask_val, mask_type, pen_gluint_t ); -- mask : GLuint
  if isExecutingCommand then
    begin
      glStencilFunc( GLenum( to_numeric( func_val ) ), GLint( to_numeric( ref_val ) ), GLuint( to_numeric( mask_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglStencilFunc;

procedure ParsePenglStencilMask is
  -- Syntax: glStencilMask( mask : GLuint );
  -- Source: bush_os.opengl.glStencilMask
  mask_val  : unbounded_string;
  mask_type : identifier;
begin
  expect( pen_glstencilmask_t );
  ParseSingleNumericParameter( mask_val, mask_type, pen_gluint_t ); -- mask : GLuint
  if isExecutingCommand then
    begin
      glStencilMask( GLuint( to_numeric( mask_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglStencilMask;

procedure ParsePenglStencilOp is
  -- Syntax: glStencilOp( fail : GLstencil; zfail : GLstencil; zpass : GLstencil );
  -- Source: bush_os.opengl.glStencilOp
  fail_val  : unbounded_string;
  fail_type : identifier;
  zfail_val  : unbounded_string;
  zfail_type : identifier;
  zpass_val  : unbounded_string;
  zpass_type : identifier;
begin
  expect( pen_glstencilop_t );
  ParseFirstNumericParameter( fail_val, fail_type, pen_glstencil_t ); -- fail : GLstencil
  ParseNextNumericParameter( zfail_val, zfail_type, pen_glstencil_t ); -- zfail : GLstencil
  ParseLastNumericParameter( zpass_val, zpass_type, pen_glstencil_t ); -- zpass : GLstencil
  if isExecutingCommand then
    begin
      glStencilOp( GLstencil( to_numeric( fail_val ) ), GLstencil( to_numeric( zfail_val ) ), GLstencil( to_numeric( zpass_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglStencilOp;

procedure ParsePenglClearStencil is
  -- Syntax: glClearStencil( s : GLint );
  -- Source: bush_os.opengl.glClearStencil
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_glclearstencil_t );
  ParseSingleNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  if isExecutingCommand then
    begin
      glClearStencil( GLint( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglClearStencil;

procedure ParsePenglTexGend is
  -- Syntax: glTexGend( coord : GLtexturemapping; pname : GLtexturemapping; param : GLdouble );
  -- Source: bush_os.opengl.glTexGend
  coord_val  : unbounded_string;
  coord_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gltexgend_t );
  ParseFirstNumericParameter( coord_val, coord_type, pen_gltexturemapping_t ); -- coord : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  ParseLastNumericParameter( param_val, param_type, pen_gldouble_t ); -- param : GLdouble
  if isExecutingCommand then
    begin
      glTexGend( GLtexturemapping( to_numeric( coord_val ) ), GLtexturemapping( to_numeric( pname_val ) ), GLdouble( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexGend;

procedure ParsePenglTexGenf is
  -- Syntax: glTexGenf( coord : GLtexturemapping; pname : GLtexturemapping; param : GLfloat );
  -- Source: bush_os.opengl.glTexGenf
  coord_val  : unbounded_string;
  coord_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gltexgenf_t );
  ParseFirstNumericParameter( coord_val, coord_type, pen_gltexturemapping_t ); -- coord : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    begin
      glTexGenf( GLtexturemapping( to_numeric( coord_val ) ), GLtexturemapping( to_numeric( pname_val ) ), GLfloat( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexGenf;

procedure ParsePenglTexGeni is
  -- Syntax: glTexGeni( coord : GLtexturemapping; pname : GLtexturemapping; param : GLint );
  -- Source: bush_os.opengl.glTexGeni
  coord_val  : unbounded_string;
  coord_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gltexgeni_t );
  ParseFirstNumericParameter( coord_val, coord_type, pen_gltexturemapping_t ); -- coord : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    begin
      glTexGeni( GLtexturemapping( to_numeric( coord_val ) ), GLtexturemapping( to_numeric( pname_val ) ), GLint( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexGeni;

procedure ParsePenglTexGendv is
  -- Syntax: glTexGendv( coord : GLtexturemapping; pname : GLtexturemapping; params : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glTexGendv
  coord_val  : unbounded_string;
  coord_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexgendv_t );
  ParseFirstNumericParameter( coord_val, coord_type, pen_gltexturemapping_t ); -- coord : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C double array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gl_double_array_ptr_t ); -- params : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Double_Array_Access := new double_array( 0..0 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      glTexGendv( GLtexturemapping( to_numeric( coord_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexGendv;

procedure ParsePenglTexGenfv is
  -- Syntax: glTexGenfv( coord : GLtexturemapping; pname : GLtexturemapping; params : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glTexGenfv
  coord_val  : unbounded_string;
  coord_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexgenfv_t );
  ParseFirstNumericParameter( coord_val, coord_type, pen_gltexturemapping_t ); -- coord : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gl_float_array_ptr_t ); -- params : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glTexGenfv( GLtexturemapping( to_numeric( coord_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexGenfv;

procedure ParsePenglTexGeniv is
  -- Syntax: glTexGeniv( coord : GLtexturemapping; pname : GLtexturemapping; params : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glTexGeniv
  coord_val  : unbounded_string;
  coord_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexgeniv_t );
  ParseFirstNumericParameter( coord_val, coord_type, pen_gltexturemapping_t ); -- coord : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gl_int_array_ptr_t ); -- params : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glTexGeniv( GLtexturemapping( to_numeric( coord_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexGeniv;

procedure ParsePenglGetTexGendv is
  -- Syntax: glGetTexGendv( coord : GLtexturemapping; pname : GLtexturemapping; params : in out GLdouble );
  -- Source: bush_os.opengl.glGetTexGendv
  coord_val  : unbounded_string;
  coord_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgettexgendv_t );
  ParseFirstNumericParameter( coord_val, coord_type, pen_gltexturemapping_t ); -- coord : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C double array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gldouble_t ); -- params : GLdouble
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Double_Array_Access := new double_array( 0..0 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      glGetTexGendv( GLtexturemapping( to_numeric( coord_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexGendv;

procedure ParsePenglGetTexGenfv is
  -- Syntax: glGetTexGenfv( coord : GLtexturemapping; pname : GLtexturemapping; params : in out GLfloat );
  -- Source: bush_os.opengl.glGetTexGenfv
  coord_val  : unbounded_string;
  coord_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgettexgenfv_t );
  ParseFirstNumericParameter( coord_val, coord_type, pen_gltexturemapping_t ); -- coord : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glGetTexGenfv( GLtexturemapping( to_numeric( coord_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexGenfv;

procedure ParsePenglGetTexGeniv is
  -- Syntax: glGetTexGeniv( coord : GLtexturemapping; pname : GLtexturemapping; params : in out GLint );
  -- Source: bush_os.opengl.glGetTexGeniv
  coord_val  : unbounded_string;
  coord_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgettexgeniv_t );
  ParseFirstNumericParameter( coord_val, coord_type, pen_gltexturemapping_t ); -- coord : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_glint_t ); -- params : GLint
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glGetTexGeniv( GLtexturemapping( to_numeric( coord_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexGeniv;

procedure ParsePenglTexEnvf is
  -- Syntax: glTexEnvf( target : GLtexturemapping; pname : GLtexturemapping; param : GLfloat );
  -- Source: bush_os.opengl.glTexEnvf
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gltexenvf_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    begin
      glTexEnvf( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), GLfloat( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexEnvf;

procedure ParsePenglTexEnvi is
  -- Syntax: glTexEnvi( target : GLtexturemapping; pname : GLtexturemapping; param : GLint );
  -- Source: bush_os.opengl.glTexEnvi
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gltexenvi_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    begin
      glTexEnvi( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), GLint( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexEnvi;

procedure ParsePenglTexEnvfv is
  -- Syntax: glTexEnvfv( target : GLtexturemapping; pname : GLtexturemapping; param : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glTexEnvfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
--  param_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexenvfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_gl_float_array_ptr_t ); -- param : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glTexEnvfv( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexEnvfv;

procedure ParsePenglTexEnviv is
  -- Syntax: glTexEnviv( target : GLtexturemapping; pname : GLtexturemapping; param : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glTexEnviv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
--  param_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexenviv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_gl_int_array_ptr_t ); -- param : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: this will have to accept arrays of different sizes
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glTexEnviv( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexEnviv;

procedure ParsePenglGetTexEnvfv is
  -- Syntax: glGetTexEnvfv( target : GLtexturemapping; pname : GLtexturemapping; param : in out GLfloat );
  -- Source: bush_os.opengl.glGetTexEnvfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
--  param_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgettexenvfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    declare
      -- worst case: 4-number colour
      param_array : GL_Float_Array_Access := new float_array( 0..3 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glGetTexEnvfv( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexEnvfv;

procedure ParsePenglGetTexEnviv is
  -- Syntax: glGetTexEnviv( target : GLtexturemapping; pname : GLtexturemapping; param : in out GLint );
  -- Source: bush_os.opengl.glGetTexEnviv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
--  param_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgettexenviv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    declare
      -- worst case: 4-number colour
      param_array : GL_Int_Array_Access := new int_array( 0..3 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glGetTexEnviv( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexEnviv;

procedure ParsePenglTexParameterf is
  -- Syntax: glTexParameterf( target : GLtexturemapping; pname : GLtexturemapping; param : GLfloat );
  -- Source: bush_os.opengl.glTexParameterf
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gltexparameterf_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    begin
      glTexParameterf( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), GLfloat( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexParameterf;

procedure ParsePenglTexParameteri is
  -- Syntax: glTexParameteri( target : GLtexturemapping; pname : GLtexturemapping; param : GLint );
  -- Source: bush_os.opengl.glTexParameteri
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_gltexparameteri_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    begin
      glTexParameteri( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), GLint( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexParameteri;

procedure ParsePenglTexParameterfv is
  -- Syntax: glTexParameterfv( target : GLtexturemapping; pname : GLtexturemapping; params : in out GLfloat );
  -- Source: bush_os.opengl.glTexParameterfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexparameterfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glTexParameterfv( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexParameterfv;

procedure ParsePenglTexParameteriv is
  -- Syntax: glTexParameteriv( target : GLtexturemapping; pname : GLtexturemapping; params : in out GLint );
  -- Source: bush_os.opengl.glTexParameteriv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_gltexparameteriv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_glint_t ); -- params : GLint
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glTexParameteriv( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexParameteriv;

procedure ParsePenglGetTexParameterfv is
  -- Syntax: glGetTexParameterfv( target : GLtexturemapping; pname : GLtexturemapping; params : in out GLfloat );
  -- Source: bush_os.opengl.glGetTexParameterfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgettexparameterfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glGetTexParameterfv( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexParameterfv;

procedure ParsePenglGetTexParameteriv is
  -- Syntax: glGetTexParameteriv( target : GLtexturemapping; pname : GLtexturemapping; params : in out GLint );
  -- Source: bush_os.opengl.glGetTexParameteriv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgettexparameteriv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( pname_val, pname_type, pen_gltexturemapping_t ); -- pname : GLtexturemapping
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_glint_t ); -- params : GLint
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glGetTexParameteriv( GLtexturemapping( to_numeric( target_val ) ), GLtexturemapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexParameteriv;

procedure ParsePenglGetTexLevelParameterfv is
  -- Syntax: glGetTexLevelParameterfv( target : GLtexturemapping; level : GLint; pname : GLenum; params : in out GLfloat );
  -- Source: bush_os.opengl.glGetTexLevelParameterfv
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgettexlevelparameterfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glGetTexLevelParameterfv( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLenum( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexLevelParameterfv;

procedure ParsePenglGetTexLevelParameteriv is
  -- Syntax: glGetTexLevelParameteriv( target : GLtexturemapping; level : GLint; pname : GLenum; params : in out GLint );
  -- Source: bush_os.opengl.glGetTexLevelParameteriv
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glgettexlevelparameteriv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( pname_val, pname_type, pen_glenum_t ); -- pname : GLenum
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_glint_t ); -- params : GLint
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glGetTexLevelParameteriv( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLenum( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexLevelParameteriv;

procedure ParsePenglTexImage1D is
  -- Syntax: glTexImage1D( target : GLtexturemapping; level : GLint; internalFormat : GLint; width : GLsizei; border : GLint; format : GLbuffers; kind : GLtypes; pixels : System.address );
  -- Source: bush_os.opengl.glTexImage1D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  internalFormat_val  : unbounded_string;
  internalFormat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  border_val  : unbounded_string;
  border_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  pixels_val  : unbounded_string;
--  pixels_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glteximage1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( internalFormat_val, internalFormat_type, pen_glint_t ); -- internalFormat : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( border_val, border_type, pen_glint_t ); -- border : GLint
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: we can probably do this but we'll need to size and populate a source array.  Come back to this later
  --ParseLastNumericParameter( pixels_val, pixels_type, pen_system.address_t ); -- pixels : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glTexImage1D( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( internalFormat_val ) ), GLsizei( to_numeric( width_val ) ), GLint( to_numeric( border_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexImage1D;

procedure ParsePenglTexImage2D is
  -- Syntax: glTexImage2D( target : GLenum; level : GLint; internalFormat : GLint; width : GLsizei; height : GLsizei; border : GLint; format : GLenum; kind : GLenum; pixels : System.address );
  -- Source: bush_os.opengl.glTexImage2D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  internalFormat_val  : unbounded_string;
  internalFormat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  border_val  : unbounded_string;
  border_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  pixels_val  : unbounded_string;
--  pixels_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glteximage2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( internalFormat_val, internalFormat_type, pen_glint_t ); -- internalFormat : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( border_val, border_type, pen_glint_t ); -- border : GLint
  ParseNextNumericParameter( format_val, format_type, pen_glenum_t ); -- format : GLenum
  ParseNextNumericParameter( kind_val, kind_type, pen_glenum_t ); -- kind : GLenum
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( pixels_val, pixels_type, pen_system.address_t ); -- pixels : System.address
  if isExecutingCommand then
     declare
      -- TODO: determine the size and type of the array
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glTexImage2D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( internalFormat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLint( to_numeric( border_val ) ), GLenum( to_numeric( format_val ) ), GLenum( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexImage2D;

procedure ParsePenglGetTexImage is
  -- Syntax: glGetTexImage( target : GLtexturemapping; level : GLint; format : GLbuffers; kind : GLtypes; pixels : System.address );
  -- Source: bush_os.opengl.glGetTexImage
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  pixels_val  : unbounded_string;
  --pixels_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetteximage_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: copying the result to a sparforte array
  --ParseLastNumericParameter( pixels_val, pixels_type, pen_system.address_t ); -- pixels : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glGetTexImage( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetTexImage;

procedure ParsePenglGenTextures is
  -- Syntax: glGenTextures( n : GLsizei; textures : GL_UInt_Array_Ptr );
  -- Source: bush_os.opengl.glGenTextures
  n_val  : unbounded_string;
  n_type : identifier;
  textures_val  : unbounded_string;
--  textures_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgentextures_t );
  ParseFirstNumericParameter( n_val, n_type, pen_glsizei_t ); -- n : GLsizei
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( textures_val, textures_type, pen_gl_uint_array_ptr_t ); -- textures : GL_UInt_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_UInt_Array_Access := new uint_array( 0..0 );
      param_ptr   : GL_UInt_Array_Ptr := GL_UInt_Array_Conv.To_Address( param_array );
    begin
      glGenTextures( GLsizei( to_numeric( n_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGenTextures;

procedure ParsePenglDeleteTextures is
  -- Syntax: glDeleteTextures( n : GLsizei; textures : GL_UInt_Array_Ptr );
  -- Source: bush_os.opengl.glDeleteTextures
  n_val  : unbounded_string;
  n_type : identifier;
  textures_val  : unbounded_string;
--  textures_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gldeletetextures_t );
  ParseFirstNumericParameter( n_val, n_type, pen_glsizei_t ); -- n : GLsizei
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( textures_val, textures_type, pen_gl_uint_array_ptr_t ); -- textures : GL_UInt_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_UInt_Array_Access := new uint_array( 0..0 );
      param_ptr   : GL_UInt_Array_Ptr := GL_UInt_Array_Conv.To_Address( param_array );
    begin
      glDeleteTextures( GLsizei( to_numeric( n_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDeleteTextures;

procedure ParsePenglBindTexture is
  -- Syntax: glBindTexture( target : GLtexturemapping; texture : GLuint );
  -- Source: bush_os.opengl.glBindTexture
  target_val  : unbounded_string;
  target_type : identifier;
  texture_val  : unbounded_string;
  texture_type : identifier;
begin
  expect( pen_glbindtexture_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseLastNumericParameter( texture_val, texture_type, pen_gluint_t ); -- texture : GLuint
  if isExecutingCommand then
    begin
      glBindTexture( GLtexturemapping( to_numeric( target_val ) ), GLuint( to_numeric( texture_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglBindTexture;

procedure ParsePenglPrioritizeTextures is
  -- Syntax: glPrioritizeTextures( n : GLsizei; textures : GL_UInt_Array_Ptr; priorities : GL_Clampf_Array_Ptr );
  -- Source: bush_os.opengl.glPrioritizeTextures
  n_val  : unbounded_string;
  n_type : identifier;
  textures_val  : unbounded_string;
--  textures_type : identifier;
  priorities_val  : unbounded_string;
--  priorities_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glprioritizetextures_t );
  ParseFirstNumericParameter( n_val, n_type, pen_glsizei_t ); -- n : GLsizei
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseNextNumericParameter( textures_val, textures_type, pen_gl_uint_array_ptr_t ); -- textures : GL_UInt_Array_Ptr
  -- TODO: clampf array not defined yet
  --ParseLastNumericParameter( priorities_val, priorities_type, pen_gl_clampf_array_ptr_t ); -- priorities : GL_Clampf_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_UInt_Array_Access := new uint_array( 0..0 );
      param_ptr   : GL_UInt_Array_Ptr := GL_UInt_Array_Conv.To_Address( param_array );
    begin
      null; --glPrioritizeTextures( GLsizei( to_numeric( n_val ) ), param_ptr, GL_Clampf_Array_Ptr( to_numeric( priorities_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPrioritizeTextures;

procedure ParsePenglAreTexturesResident( result : out unbounded_string ) is
  -- Syntax: glAreTexturesResident( n : GLsizei; textures : GL_UInt_Array_Ptr; residences : GL_Boolean_Array_Ptr ) return GLboolean;
  -- Source: bush_os.opengl.glAreTexturesResident
  n_val  : unbounded_string;
  n_type : identifier;
  textures_val  : unbounded_string;
--  textures_type : identifier;
  residences_val  : unbounded_string;
--  residences_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glaretexturesresident_t );
  ParseFirstNumericParameter( n_val, n_type, pen_glsizei_t ); -- n : GLsizei
-- TODO: takes a multivalue C uint/bool array of parameters, but we don't support that in SparForte
  --ParseNextNumericParameter( textures_val, textures_type, pen_gl_uint_array_ptr_t ); -- textures : GL_UInt_Array_Ptr
  --ParseLastNumericParameter( residences_val, residences_type, pen_gl_boolean_array_ptr_t ); -- residences : GL_Boolean_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
      param2_array : GL_Boolean_Array_Access := new bool_array( 0..0 );
      param2_ptr   : GL_Boolean_Array_Ptr := GL_Boolean_Array_Conv.To_Address( param2_array );
    begin
      result := to_unbounded_string( long_float( glAreTexturesResident( GLsizei( to_numeric( n_val ) ), param_ptr, param2_ptr ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglAreTexturesResident;

procedure ParsePenglIsTexture( result : out unbounded_string ) is
  -- Syntax: glIsTexture( texture : GL_UInt_Array_Ptr ) return GLboolean;
  -- Source: bush_os.opengl.glIsTexture
  texture_val  : unbounded_string;
  texture_type : identifier;
begin
  expect( pen_glistexture_t );
  ParseSingleNumericParameter( texture_val, texture_type, pen_gluint_t ); -- texture : GL_UInt_Array_Ptr
  if isExecutingCommand then
    begin
      result := to_unbounded_string( long_float( glIsTexture( GLuint( to_numeric( texture_val ) ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglIsTexture;

procedure ParsePenglTexSubImage1D is
  -- Syntax: glTexSubImage1D( target : GLtexturemapping; level : GLint; xoffset : GLint; width : GLsizei; format : GLbuffers; kind : GLtypes; pixels : System.address );
  -- Source: bush_os.opengl.glTexSubImage1D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  xoffset_val  : unbounded_string;
  xoffset_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  pixels_val  : unbounded_string;
  --pixels_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gltexsubimage1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( xoffset_val, xoffset_type, pen_glint_t ); -- xoffset : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: pixels are an array of types based on the parameters.  Come back to this later.
  --ParseLastNumericParameter( pixels_val, pixels_type, pen_system.address_t ); -- pixels : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glTexSubImage1D( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( xoffset_val ) ), GLsizei( to_numeric( width_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexSubImage1D;

procedure ParsePenglTexSubImage2D is
  -- Syntax: glTexSubImage2D( target : GLtexturemapping; level : GLint; xoffset, yoffset : GLint; width, height : GLsizei; format : GLbuffers; kind : GLtypes; pixels : System.address );
  -- Source: bush_os.opengl.glTexSubImage2D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  xoffset_val  : unbounded_string;
  xoffset_type : identifier;
  yoffset_val  : unbounded_string;
  yoffset_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  pixels_val  : unbounded_string;
  --pixels_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gltexsubimage2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( xoffset_val, xoffset_type, pen_glint_t ); -- xoffset : GLint
  ParseNextNumericParameter( yoffset_val, yoffset_type, pen_glint_t ); -- yoffset : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: pixels are an array of types based on the parameters.  Come back to this later.
  --ParseLastNumericParameter( pixels_val, pixels_type, pen_system.address_t ); -- pixels : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glTexSubImage2D( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( xoffset_val ) ), GLint( to_numeric( yoffset_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexSubImage2D;

procedure ParsePenglCopyTexImage1D is
  -- Syntax: glCopyTexImage1D( target : GLtexturemapping; level : GLint; internalformat : GLenum; x, y : GLint; width : GLsizei; border : GLint );
  -- Source: bush_os.opengl.glCopyTexImage1D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  border_val  : unbounded_string;
  border_type : identifier;
begin
  expect( pen_glcopyteximage1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glenum_t ); -- internalformat : GLenum
  ParseNextNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseLastNumericParameter( border_val, border_type, pen_glint_t ); -- border : GLint
  if isExecutingCommand then
    begin
      glCopyTexImage1D( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLenum( to_numeric( internalformat_val ) ), GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ), GLint( to_numeric( border_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyTexImage1D;

procedure ParsePenglCopyTexImage2D is
  -- Syntax: glCopyTexImage2D( target : GLtexturemapping; level : GLint; internalformat : GLenum; x, y : GLint; width, height : GLsizei; border : GLint );
  -- Source: bush_os.opengl.glCopyTexImage2D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  border_val  : unbounded_string;
  border_type : identifier;
begin
  expect( pen_glcopyteximage2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glenum_t ); -- internalformat : GLenum
  ParseNextNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseLastNumericParameter( border_val, border_type, pen_glint_t ); -- border : GLint
  if isExecutingCommand then
    begin
      glCopyTexImage2D( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLenum( to_numeric( internalformat_val ) ), GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLint( to_numeric( border_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyTexImage2D;

procedure ParsePenglCopyTexSubImage1D is
  -- Syntax: glCopyTexSubImage1D( target : GLtexturemapping; level : GLint; xoffset : GLint; x, y : GLint; width : GLsizei );
  -- Source: bush_os.opengl.glCopyTexSubImage1D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  xoffset_val  : unbounded_string;
  xoffset_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
begin
  expect( pen_glcopytexsubimage1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( xoffset_val, xoffset_type, pen_glint_t ); -- xoffset : GLint
  ParseNextNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseLastNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  if isExecutingCommand then
    begin
      glCopyTexSubImage1D( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( xoffset_val ) ), GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyTexSubImage1D;

procedure ParsePenglCopyTexSubImage2D is
  -- Syntax: glCopyTexSubImage2D( target : GLtexturemapping; level : GLint; xoffset, yoffset : GLint; x, y : GLint; width, height : GLsizei );
  -- Source: bush_os.opengl.glCopyTexSubImage2D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  xoffset_val  : unbounded_string;
  xoffset_type : identifier;
  yoffset_val  : unbounded_string;
  yoffset_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
begin
  expect( pen_glcopytexsubimage2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_gltexturemapping_t ); -- target : GLtexturemapping
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( xoffset_val, xoffset_type, pen_glint_t ); -- xoffset : GLint
  ParseNextNumericParameter( yoffset_val, yoffset_type, pen_glint_t ); -- yoffset : GLint
  ParseNextNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseLastNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  if isExecutingCommand then
    begin
      glCopyTexSubImage2D( GLtexturemapping( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( xoffset_val ) ), GLint( to_numeric( yoffset_val ) ), GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyTexSubImage2D;

procedure ParsePenglMap1d is
  -- Syntax: glMap1d( target : GLevaluators; u1, u2 : GLdouble; stride : GLint; order : GLint; points : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glMap1d
  target_val  : unbounded_string;
  target_type : identifier;
  u1_val  : unbounded_string;
  u1_type : identifier;
  u2_val  : unbounded_string;
  u2_type : identifier;
  stride_val  : unbounded_string;
  stride_type : identifier;
  order_val  : unbounded_string;
  order_type : identifier;
  points_val  : unbounded_string;
--  points_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmap1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glevaluators_t ); -- target : GLevaluators
  ParseNextNumericParameter( u1_val, u1_type, pen_gldouble_t ); -- u1 : GLdouble
  ParseNextNumericParameter( u2_val, u2_type, pen_gldouble_t ); -- u2 : GLdouble
  ParseNextNumericParameter( stride_val, stride_type, pen_glint_t ); -- stride : GLint
  ParseNextNumericParameter( order_val, order_type, pen_glint_t ); -- order : GLint
  -- TODO: takes a multivalue C double array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( points_val, points_type, pen_gl_double_array_ptr_t ); -- points : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size, copy from SparForte array
      param_array : GL_Double_Array_Access := new double_array( 0..0 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      glMap1d( GLevaluators( to_numeric( target_val ) ), GLdouble( to_numeric( u1_val ) ), GLdouble( to_numeric( u2_val ) ), GLint( to_numeric( stride_val ) ), GLint( to_numeric( order_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMap1d;

procedure ParsePenglMap1f is
  -- Syntax: glMap1f( target : GLevaluators; u1, u2 : GLfloat; stride : GLint; order : GLint; points : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glMap1f
  target_val  : unbounded_string;
  target_type : identifier;
  u1_val  : unbounded_string;
  u1_type : identifier;
  u2_val  : unbounded_string;
  u2_type : identifier;
  stride_val  : unbounded_string;
  stride_type : identifier;
  order_val  : unbounded_string;
  order_type : identifier;
  points_val  : unbounded_string;
--  points_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmap1f_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glevaluators_t ); -- target : GLevaluators
  ParseNextNumericParameter( u1_val, u1_type, pen_glfloat_t ); -- u1 : GLfloat
  ParseNextNumericParameter( u2_val, u2_type, pen_glfloat_t ); -- u2 : GLfloat
  ParseNextNumericParameter( stride_val, stride_type, pen_glint_t ); -- stride : GLint
  ParseNextNumericParameter( order_val, order_type, pen_glint_t ); -- order : GLint
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( points_val, points_type, pen_gl_float_array_ptr_t ); -- points : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size, copy from sparforte array
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glMap1f( GLevaluators( to_numeric( target_val ) ), GLfloat( to_numeric( u1_val ) ), GLfloat( to_numeric( u2_val ) ), GLint( to_numeric( stride_val ) ), GLint( to_numeric( order_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMap1f;

procedure ParsePenglMap2d is
  -- Syntax: glMap2d( target : GLevaluators; u1, u2 : GLdouble; ustride : GLint; uorder : GLint; v1, v2 : GLdouble; vstride : GLint; vorder : GLint; points : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glMap2d
  target_val  : unbounded_string;
  target_type : identifier;
  u1_val  : unbounded_string;
  u1_type : identifier;
  u2_val  : unbounded_string;
  u2_type : identifier;
  ustride_val  : unbounded_string;
  ustride_type : identifier;
  uorder_val  : unbounded_string;
  uorder_type : identifier;
  v1_val  : unbounded_string;
  v1_type : identifier;
  v2_val  : unbounded_string;
  v2_type : identifier;
  vstride_val  : unbounded_string;
  vstride_type : identifier;
  vorder_val  : unbounded_string;
  vorder_type : identifier;
  points_val  : unbounded_string;
  --points_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmap2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glevaluators_t ); -- target : GLevaluators
  ParseNextNumericParameter( u1_val, u1_type, pen_gldouble_t ); -- u1 : GLdouble
  ParseNextNumericParameter( u2_val, u2_type, pen_gldouble_t ); -- u2 : GLdouble
  ParseNextNumericParameter( ustride_val, ustride_type, pen_glint_t ); -- ustride : GLint
  ParseNextNumericParameter( uorder_val, uorder_type, pen_glint_t ); -- uorder : GLint
  ParseNextNumericParameter( v1_val, v1_type, pen_gldouble_t ); -- v1 : GLdouble
  ParseNextNumericParameter( v2_val, v2_type, pen_gldouble_t ); -- v2 : GLdouble
  ParseNextNumericParameter( vstride_val, vstride_type, pen_glint_t ); -- vstride : GLint
  ParseNextNumericParameter( vorder_val, vorder_type, pen_glint_t ); -- vorder : GLint
  -- TODO: takes a multivalue C double array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( points_val, points_type, pen_gl_double_array_ptr_t ); -- points : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Double_Array_Access := new double_array( 0..0 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      glMap2d( GLevaluators( to_numeric( target_val ) ), GLdouble( to_numeric( u1_val ) ), GLdouble( to_numeric( u2_val ) ), GLint( to_numeric( ustride_val ) ), GLint( to_numeric( uorder_val ) ), GLdouble( to_numeric( v1_val ) ), GLdouble( to_numeric( v2_val ) ), GLint( to_numeric( vstride_val ) ), GLint( to_numeric( vorder_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMap2d;

procedure ParsePenglMap2f is
  -- Syntax: glMap2f( target : GLevaluators; u1, u2 : GLfloat; ustride : GLint; uorder : GLint; v1, v2 : GLfloat; vstride : GLint; vorder : GLint; points : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glMap2f
  target_val  : unbounded_string;
  target_type : identifier;
  u1_val  : unbounded_string;
  u1_type : identifier;
  u2_val  : unbounded_string;
  u2_type : identifier;
  ustride_val  : unbounded_string;
  ustride_type : identifier;
  uorder_val  : unbounded_string;
  uorder_type : identifier;
  v1_val  : unbounded_string;
  v1_type : identifier;
  v2_val  : unbounded_string;
  v2_type : identifier;
  vstride_val  : unbounded_string;
  vstride_type : identifier;
  vorder_val  : unbounded_string;
  vorder_type : identifier;
  points_val  : unbounded_string;
  --points_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmap2f_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glevaluators_t ); -- target : GLevaluators
  ParseNextNumericParameter( u1_val, u1_type, pen_glfloat_t ); -- u1 : GLfloat
  ParseNextNumericParameter( u2_val, u2_type, pen_glfloat_t ); -- u2 : GLfloat
  ParseNextNumericParameter( ustride_val, ustride_type, pen_glint_t ); -- ustride : GLint
  ParseNextNumericParameter( uorder_val, uorder_type, pen_glint_t ); -- uorder : GLint
  ParseNextNumericParameter( v1_val, v1_type, pen_glfloat_t ); -- v1 : GLfloat
  ParseNextNumericParameter( v2_val, v2_type, pen_glfloat_t ); -- v2 : GLfloat
  ParseNextNumericParameter( vstride_val, vstride_type, pen_glint_t ); -- vstride : GLint
  ParseNextNumericParameter( vorder_val, vorder_type, pen_glint_t ); -- vorder : GLint
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( points_val, points_type, pen_gl_float_array_ptr_t ); -- points : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glMap2f( GLevaluators( to_numeric( target_val ) ), GLfloat( to_numeric( u1_val ) ), GLfloat( to_numeric( u2_val ) ), GLint( to_numeric( ustride_val ) ), GLint( to_numeric( uorder_val ) ), GLfloat( to_numeric( v1_val ) ), GLfloat( to_numeric( v2_val ) ), GLint( to_numeric( vstride_val ) ), GLint( to_numeric( vorder_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMap2f;

procedure ParsePenglGetMapdv is
  -- Syntax: glGetMapdv( target, query : GLevaluators; v : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glGetMapdv
  target_val  : unbounded_string;
  target_type : identifier;
  query_val  : unbounded_string;
  query_type : identifier;
  v_val  : unbounded_string;
  --v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetmapdv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glevaluators_t ); -- target : GLevaluators
  ParseNextNumericParameter( query_val, query_type, pen_glevaluators_t ); -- query : GLevaluators
  -- TODO: takes a multivalue C double array of parameters, but we don't support that in SparForte
--  ParseLastNumericParameter( v_val, v_type, pen_gl_double_array_ptr_t ); -- v : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Double_Array_Access := new double_array( 0..0 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      glGetMapdv( GLevaluators( to_numeric( target_val ) ), GLevaluators( to_numeric( query_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetMapdv;

procedure ParsePenglGetMapfv is
  -- Syntax: glGetMapfv( target, query : GLevaluators; v : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glGetMapfv
  target_val  : unbounded_string;
  target_type : identifier;
  query_val  : unbounded_string;
  query_type : identifier;
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetmapfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glevaluators_t ); -- target : GLevaluators
  ParseNextNumericParameter( query_val, query_type, pen_glevaluators_t ); -- query : GLevaluators
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( v_val, v_type, pen_gl_float_array_ptr_t ); -- v : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glGetMapfv( GLevaluators( to_numeric( target_val ) ), GLevaluators( to_numeric( query_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetMapfv;

procedure ParsePenglGetMapiv is
  -- Syntax: glGetMapiv( target, query : GLevaluators; v : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glGetMapiv
  target_val  : unbounded_string;
  target_type : identifier;
  query_val  : unbounded_string;
  query_type : identifier;
  v_val  : unbounded_string;
--  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetmapiv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glevaluators_t ); -- target : GLevaluators
  ParseNextNumericParameter( query_val, query_type, pen_glevaluators_t ); -- query : GLevaluators
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( v_val, v_type, pen_gl_int_array_ptr_t ); -- v : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glGetMapiv( GLevaluators( to_numeric( target_val ) ), GLevaluators( to_numeric( query_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetMapiv;

procedure ParsePenglEvalCoord1d is
  -- Syntax: glEvalCoord1d( u : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glEvalCoord1d
  u_val  : unbounded_string;
--  u_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glevalcoord1d_t );
  -- TODO: define this
  --ParseSingleNumericParameter( u_val, u_type, pen_gl_double_array_ptr_t ); -- u : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: populate the value
      param_array : GL_Double_Array_Access := new double_array( 0..0 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      glEvalCoord1d( param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalCoord1d;

procedure ParsePenglEvalCoord1f is
  -- Syntax: glEvalCoord1f( u : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glEvalCoord1f
  u_val  : unbounded_string;
--  u_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glevalcoord1f_t );
  -- TODO: define this
  -- ParseSingleNumericParameter( u_val, u_type, pen_gl_float_array_ptr_t ); -- u : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: populate the value
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glEvalCoord1f( param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalCoord1f;

procedure ParsePenglEvalCoord1dv is
  -- Syntax: glEvalCoord1dv( u : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glEvalCoord1dv
  u_val  : unbounded_string;
--  u_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glevalcoord1dv_t );
  -- TODO: define this
  -- ParseSingleNumericParameter( u_val, u_type, pen_gl_double_array_ptr_t ); -- u : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: populate the values
      param_array : GL_Double_Array_Access := new double_array( 0..0 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      glEvalCoord1dv( param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalCoord1dv;

procedure ParsePenglEvalCoord1fv is
  -- Syntax: glEvalCoord1fv( u : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glEvalCoord1fv
  u_val  : unbounded_string;
--  u_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glevalcoord1fv_t );
  -- TODO: define this
  --ParseSingleNumericParameter( u_val, u_type, pen_gl_float_array_ptr_t ); -- u : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: populate the values
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glEvalCoord1fv( param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalCoord1fv;

procedure ParsePenglEvalCoord2d is
  -- Syntax: glEvalCoord2d( u, v : GLdouble );
  -- Source: bush_os.opengl.glEvalCoord2d
  u_val  : unbounded_string;
  u_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  expect( pen_glevalcoord2d_t );
  ParseFirstNumericParameter( u_val, u_type, pen_gldouble_t ); -- u : GLdouble
  ParseLastNumericParameter( v_val, v_type, pen_gldouble_t ); -- v : GLdouble
  if isExecutingCommand then
    begin
      glEvalCoord2d( GLdouble( to_numeric( u_val ) ), GLdouble( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalCoord2d;

procedure ParsePenglEvalCoord2f is
  -- Syntax: glEvalCoord2f( u, v : GLfloat );
  -- Source: bush_os.opengl.glEvalCoord2f
  u_val  : unbounded_string;
  u_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  expect( pen_glevalcoord2f_t );
  ParseFirstNumericParameter( u_val, u_type, pen_glfloat_t ); -- u : GLfloat
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    begin
      glEvalCoord2f( GLfloat( to_numeric( u_val ) ), GLfloat( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalCoord2f;

procedure ParsePenglEvalCoord2dv is
  -- Syntax: glEvalCoord2dv( u : GL_Double_Array_Ptr );
  -- Source: bush_os.opengl.glEvalCoord2dv
  u_val  : unbounded_string;
--  u_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glevalcoord2dv_t );
  -- TODO: define this
  --ParseSingleNumericParameter( u_val, u_type, pen_gl_double_array_ptr_t ); -- u : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: populate the values
      param_array : GL_Double_Array_Access := new double_array( 0..1 );
      param_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( param_array );
    begin
      glEvalCoord2dv( param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalCoord2dv;

procedure ParsePenglEvalCoord2fv is
  -- Syntax: glEvalCoord2fv( u : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glEvalCoord2fv
  u_val  : unbounded_string;
--  u_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glevalcoord2fv_t );
  -- TODO: define this
  --ParseSingleNumericParameter( u_val, u_type, pen_gl_float_array_ptr_t ); -- u : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: populate the values
      param_array : GL_Float_Array_Access := new float_array( 0..1 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glEvalCoord2fv( param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalCoord2fv;

procedure ParsePenglMapGrid1d is
  -- Syntax: glMapGrid1d( un : GLint; u1, u2 : GLdouble );
  -- Source: bush_os.opengl.glMapGrid1d
  un_val  : unbounded_string;
  un_type : identifier;
  u1_val  : unbounded_string;
  u1_type : identifier;
  u2_val  : unbounded_string;
  u2_type : identifier;
begin
  expect( pen_glmapgrid1d_t );
  ParseFirstNumericParameter( un_val, un_type, pen_glint_t ); -- un : GLint
  ParseNextNumericParameter( u1_val, u1_type, pen_gldouble_t ); -- u1 : GLdouble
  ParseLastNumericParameter( u2_val, u2_type, pen_gldouble_t ); -- u2 : GLdouble
  if isExecutingCommand then
    begin
      glMapGrid1d( GLint( to_numeric( un_val ) ), GLdouble( to_numeric( u1_val ) ), GLdouble( to_numeric( u2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMapGrid1d;

procedure ParsePenglMapGrid1f is
  -- Syntax: glMapGrid1f( un : GLint; u1, u2 : GLfloat );
  -- Source: bush_os.opengl.glMapGrid1f
  un_val  : unbounded_string;
  un_type : identifier;
  u1_val  : unbounded_string;
  u1_type : identifier;
  u2_val  : unbounded_string;
  u2_type : identifier;
begin
  expect( pen_glmapgrid1f_t );
  ParseFirstNumericParameter( un_val, un_type, pen_glint_t ); -- un : GLint
  ParseNextNumericParameter( u1_val, u1_type, pen_glfloat_t ); -- u1 : GLfloat
  ParseLastNumericParameter( u2_val, u2_type, pen_glfloat_t ); -- u2 : GLfloat
  if isExecutingCommand then
    begin
      glMapGrid1f( GLint( to_numeric( un_val ) ), GLfloat( to_numeric( u1_val ) ), GLfloat( to_numeric( u2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMapGrid1f;

procedure ParsePenglMapGrid2d is
  -- Syntax: glMapGrid2d( un : GLint; u1, u2 : GLdouble; vn : GLint; v1, v2 : GLdouble );
  -- Source: bush_os.opengl.glMapGrid2d
  un_val  : unbounded_string;
  un_type : identifier;
  u1_val  : unbounded_string;
  u1_type : identifier;
  u2_val  : unbounded_string;
  u2_type : identifier;
  vn_val  : unbounded_string;
  vn_type : identifier;
  v1_val  : unbounded_string;
  v1_type : identifier;
  v2_val  : unbounded_string;
  v2_type : identifier;
begin
  expect( pen_glmapgrid2d_t );
  ParseFirstNumericParameter( un_val, un_type, pen_glint_t ); -- un : GLint
  ParseNextNumericParameter( u1_val, u1_type, pen_gldouble_t ); -- u1 : GLdouble
  ParseNextNumericParameter( u2_val, u2_type, pen_gldouble_t ); -- u2 : GLdouble
  ParseNextNumericParameter( vn_val, vn_type, pen_glint_t ); -- vn : GLint
  ParseNextNumericParameter( v1_val, v1_type, pen_gldouble_t ); -- v1 : GLdouble
  ParseLastNumericParameter( v2_val, v2_type, pen_gldouble_t ); -- v2 : GLdouble
  if isExecutingCommand then
    begin
      glMapGrid2d( GLint( to_numeric( un_val ) ), GLdouble( to_numeric( u1_val ) ), GLdouble( to_numeric( u2_val ) ), GLint( to_numeric( vn_val ) ), GLdouble( to_numeric( v1_val ) ), GLdouble( to_numeric( v2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMapGrid2d;

procedure ParsePenglMapGrid2f is
  -- Syntax: glMapGrid2f( un : GLint; u1, u2 : GLfloat; vn : GLint; v1, v2 : GLfloat );
  -- Source: bush_os.opengl.glMapGrid2f
  un_val  : unbounded_string;
  un_type : identifier;
  u1_val  : unbounded_string;
  u1_type : identifier;
  u2_val  : unbounded_string;
  u2_type : identifier;
  vn_val  : unbounded_string;
  vn_type : identifier;
  v1_val  : unbounded_string;
  v1_type : identifier;
  v2_val  : unbounded_string;
  v2_type : identifier;
begin
  expect( pen_glmapgrid2f_t );
  ParseFirstNumericParameter( un_val, un_type, pen_glint_t ); -- un : GLint
  ParseNextNumericParameter( u1_val, u1_type, pen_glfloat_t ); -- u1 : GLfloat
  ParseNextNumericParameter( u2_val, u2_type, pen_glfloat_t ); -- u2 : GLfloat
  ParseNextNumericParameter( vn_val, vn_type, pen_glint_t ); -- vn : GLint
  ParseNextNumericParameter( v1_val, v1_type, pen_glfloat_t ); -- v1 : GLfloat
  ParseLastNumericParameter( v2_val, v2_type, pen_glfloat_t ); -- v2 : GLfloat
  if isExecutingCommand then
    begin
      glMapGrid2f( GLint( to_numeric( un_val ) ), GLfloat( to_numeric( u1_val ) ), GLfloat( to_numeric( u2_val ) ), GLint( to_numeric( vn_val ) ), GLfloat( to_numeric( v1_val ) ), GLfloat( to_numeric( v2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMapGrid2f;

procedure ParsePenglEvalPoint1 is
  -- Syntax: glEvalPoint1( i : GLint );
  -- Source: bush_os.opengl.glEvalPoint1
  i_val  : unbounded_string;
  i_type : identifier;
begin
  expect( pen_glevalpoint1_t );
  ParseSingleNumericParameter( i_val, i_type, pen_glint_t ); -- i : GLint
  if isExecutingCommand then
    begin
      glEvalPoint1( GLint( to_numeric( i_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalPoint1;

procedure ParsePenglEvalPoint2 is
  -- Syntax: glEvalPoint2( i, j : GLint );
  -- Source: bush_os.opengl.glEvalPoint2
  i_val  : unbounded_string;
  i_type : identifier;
  j_val  : unbounded_string;
  j_type : identifier;
begin
  expect( pen_glevalpoint2_t );
  ParseFirstNumericParameter( i_val, i_type, pen_glint_t ); -- i : GLint
  ParseLastNumericParameter( j_val, j_type, pen_glint_t ); -- j : GLint
  if isExecutingCommand then
    begin
      glEvalPoint2( GLint( to_numeric( i_val ) ), GLint( to_numeric( j_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalPoint2;

procedure ParsePenglEvalMesh1 is
  -- Syntax: glEvalMesh1( mode : GLpolygons; i1, i2 : GLint );
  -- Source: bush_os.opengl.glEvalMesh1
  mode_val  : unbounded_string;
  mode_type : identifier;
  i1_val  : unbounded_string;
  i1_type : identifier;
  i2_val  : unbounded_string;
  i2_type : identifier;
begin
  expect( pen_glevalmesh1_t );
  ParseFirstNumericParameter( mode_val, mode_type, pen_glpolygons_t ); -- mode : GLpolygons
  ParseNextNumericParameter( i1_val, i1_type, pen_glint_t ); -- i1 : GLint
  ParseLastNumericParameter( i2_val, i2_type, pen_glint_t ); -- i2 : GLint
  if isExecutingCommand then
    begin
      glEvalMesh1( GLpolygons( to_numeric( mode_val ) ), GLint( to_numeric( i1_val ) ), GLint( to_numeric( i2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalMesh1;

procedure ParsePenglEvalMesh2 is
  -- Syntax: glEvalMesh2( mode : GLpolygons; i1, i2 :GLint; j1, j2 : GLint );
  -- Source: bush_os.opengl.glEvalMesh2
  mode_val  : unbounded_string;
  mode_type : identifier;
  i1_val  : unbounded_string;
  i1_type : identifier;
  i2_val  : unbounded_string;
  i2_type : identifier;
  j1_val  : unbounded_string;
  j1_type : identifier;
  j2_val  : unbounded_string;
  j2_type : identifier;
begin
  expect( pen_glevalmesh2_t );
  ParseFirstNumericParameter( mode_val, mode_type, pen_glpolygons_t ); -- mode : GLpolygons
  ParseNextNumericParameter( i1_val, i1_type, pen_glint_t ); -- i1 : GLint
  ParseNextNumericParameter( i2_val, i2_type, pen_glint_t ); -- i2 : GLint
  ParseNextNumericParameter( j1_val, j1_type, pen_glint_t ); -- j1 : GLint
  ParseLastNumericParameter( j2_val, j2_type, pen_glint_t ); -- j2 : GLint
  if isExecutingCommand then
    begin
      glEvalMesh2( GLpolygons( to_numeric( mode_val ) ), GLint( to_numeric( i1_val ) ), GLint( to_numeric( i2_val ) ), GLint( to_numeric( j1_val ) ), GLint( to_numeric( j2_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEvalMesh2;

procedure ParsePenglFogf is
  -- Syntax: glFogf( pname : GLfog; param : GLfloat );
  -- Source: bush_os.opengl.glFogf
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_glfogf_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glfog_t ); -- pname : GLfog
  ParseLastNumericParameter( param_val, param_type, pen_glfloat_t ); -- param : GLfloat
  if isExecutingCommand then
    begin
      glFogf( GLfog( to_numeric( pname_val ) ), GLfloat( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFogf;

procedure ParsePenglFogi is
  -- Syntax: glFogi( pname : GLfog; param : GLint );
  -- Source: bush_os.opengl.glFogi
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  param_type : identifier;
begin
  expect( pen_glfogi_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glfog_t ); -- pname : GLfog
  ParseLastNumericParameter( param_val, param_type, pen_glint_t ); -- param : GLint
  if isExecutingCommand then
    begin
      glFogi( GLfog( to_numeric( pname_val ) ), GLint( to_numeric( param_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFogi;

procedure ParsePenglFogfv is
  -- Syntax: glFogfv( pname : GLfog; param : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glFogfv
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  --param_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glfogfv_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glfog_t ); -- pname : GLfog
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_gl_float_array_ptr_t ); -- param : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glFogfv( GLfog( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFogfv;

procedure ParsePenglFogiv is
  -- Syntax: glFogiv( pname : GLfog; param : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glFogiv
  pname_val  : unbounded_string;
  pname_type : identifier;
  param_val  : unbounded_string;
  --param_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glfogiv_t );
  ParseFirstNumericParameter( pname_val, pname_type, pen_glfog_t ); -- pname : GLfog
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( param_val, param_type, pen_gl_int_array_ptr_t ); -- param : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glFogiv( GLfog( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFogiv;

procedure ParsePenglFeedbackBuffer is
  -- Syntax: glFeedbackBuffer( size : GLsizei; kind : GLfeedback; buffer : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glFeedbackBuffer
  size_val  : unbounded_string;
  size_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  buffer_val  : unbounded_string;
--  buffer_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glfeedbackbuffer_t );
  ParseFirstNumericParameter( size_val, size_type, pen_glsizei_t ); -- size : GLsizei
  ParseNextNumericParameter( kind_val, kind_type, pen_glfeedback_t ); -- kind : GLfeedback
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( buffer_val, buffer_type, pen_gl_float_array_ptr_t ); -- buffer : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glFeedbackBuffer( GLsizei( to_numeric( size_val ) ), GLfeedback( to_numeric( kind_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFeedbackBuffer;

procedure ParsePenglPassThrough is
  -- Syntax: glPassThrough( token : GLfloat );
  -- Source: bush_os.opengl.glPassThrough
  token_val  : unbounded_string;
  token_type : identifier;
begin
  expect( pen_glpassthrough_t );
  ParseSingleNumericParameter( token_val, token_type, pen_glfloat_t ); -- token : GLfloat
  if isExecutingCommand then
    begin
      glPassThrough( GLfloat( to_numeric( token_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPassThrough;

procedure ParsePenglSelectBuffer is
  -- Syntax: glSelectBuffer( kind : GLsizei; buffer : GL_UInt_Array_Ptr );
  -- Source: bush_os.opengl.glSelectBuffer
  kind_val  : unbounded_string;
  kind_type : identifier;
  buffer_val  : unbounded_string;
--  buffer_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glselectbuffer_t );
  ParseFirstNumericParameter( kind_val, kind_type, pen_glsizei_t ); -- kind : GLsizei
  -- TODO: takes a multivalue C uint array of parameters, but we don't support that in SparForte
  -- ParseLastNumericParameter( buffer_val, buffer_type, pen_gl_uint_array_ptr_t ); -- buffer : GL_UInt_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_UInt_Array_Access := new uint_array( 0..0 );
      param_ptr   : GL_UInt_Array_Ptr := GL_UInt_Array_Conv.To_Address( param_array );
    begin
      glSelectBuffer( GLsizei( to_numeric( kind_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglSelectBuffer;

procedure ParsePenglInitNames is
  -- Syntax: glInitNames;
  -- Source: bush_os.opengl.glInitNames
begin
  expect( pen_glinitnames_t );
  if isExecutingCommand then
    begin
      glInitNames;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglInitNames;

procedure ParsePenglLoadName is
  -- Syntax: glLoadName( name : GLuint );
  -- Source: bush_os.opengl.glLoadName
  name_val  : unbounded_string;
  name_type : identifier;
begin
  expect( pen_glloadname_t );
  ParseSingleNumericParameter( name_val, name_type, pen_gluint_t ); -- name : GLuint
  if isExecutingCommand then
    begin
      glLoadName( GLuint( to_numeric( name_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLoadName;

procedure ParsePenglPushName is
  -- Syntax: glPushName( name : GLuint );
  -- Source: bush_os.opengl.glPushName
  name_val  : unbounded_string;
  name_type : identifier;
begin
  expect( pen_glpushname_t );
  ParseSingleNumericParameter( name_val, name_type, pen_gluint_t ); -- name : GLuint
  if isExecutingCommand then
    begin
      glPushName( GLuint( to_numeric( name_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPushName;

procedure ParsePenglPopName is
  -- Syntax: glPopName;
  -- Source: bush_os.opengl.glPopName
begin
  expect( pen_glpopname_t );
  if isExecutingCommand then
    begin
      glPopName;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglPopName;

procedure ParsePenglDrawRangeElements is
  -- Syntax: glDrawRangeElements( mode : GLprimitives; start : GLuint; done : GLuint; count : GLsizei; kind : GLtypes; indices : System.address );
  -- Source: bush_os.opengl.glDrawRangeElements
  mode_val  : unbounded_string;
  mode_type : identifier;
  start_val  : unbounded_string;
  start_type : identifier;
  done_val  : unbounded_string;
  done_type : identifier;
  count_val  : unbounded_string;
  count_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  indices_val  : unbounded_string;
--  indices_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gldrawrangeelements_t );
  ParseFirstNumericParameter( mode_val, mode_type, pen_glprimitives_t ); -- mode : GLprimitives
  ParseNextNumericParameter( start_val, start_type, pen_gluint_t ); -- start : GLuint
  ParseNextNumericParameter( done_val, done_type, pen_gluint_t ); -- done : GLuint
  ParseNextNumericParameter( count_val, count_type, pen_glsizei_t ); -- count : GLsizei
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( indices_val, indices_type, pen_system.address_t ); -- indices : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glDrawRangeElements( GLprimitives( to_numeric( mode_val ) ), GLuint( to_numeric( start_val ) ), GLuint( to_numeric( done_val ) ), GLsizei( to_numeric( count_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglDrawRangeElements;

procedure ParsePenglTexImage3D is
  -- Syntax: glTexImage3D( target : GLenum; level : GLint; internalFormat : GLint; width, height, depth : GLsizei; border : GLint; format : GLbuffers; kind : GLtypes; pixels : System.address );
  -- Source: bush_os.opengl.glTexImage3D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  internalFormat_val  : unbounded_string;
  internalFormat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  depth_val  : unbounded_string;
  depth_type : identifier;
  border_val  : unbounded_string;
  border_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  pixels_val  : unbounded_string;
--  pixels_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glteximage3d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( internalFormat_val, internalFormat_type, pen_glint_t ); -- internalFormat : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( depth_val, depth_type, pen_glsizei_t ); -- depth : GLsizei
  ParseNextNumericParameter( border_val, border_type, pen_glint_t ); -- border : GLint
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( pixels_val, pixels_type, pen_system.address_t ); -- pixels : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glTexImage3D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( internalFormat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLsizei( to_numeric( depth_val ) ), GLint( to_numeric( border_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexImage3D;

procedure ParsePenglTexSubImage3D is
  -- Syntax: glTexSubImage3D( target : GLenum; level : GLint; xoffset, yoffset, zoffset : GLint; width, height, depth : GLsizei; format : GLbuffers; kind : GLtypes; pixels : System.address );
  -- Source: bush_os.opengl.glTexSubImage3D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  xoffset_val  : unbounded_string;
  xoffset_type : identifier;
  yoffset_val  : unbounded_string;
  yoffset_type : identifier;
  zoffset_val  : unbounded_string;
  zoffset_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  depth_val  : unbounded_string;
  depth_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  pixels_val  : unbounded_string;
--  pixels_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gltexsubimage3d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( xoffset_val, xoffset_type, pen_glint_t ); -- xoffset : GLint
  ParseNextNumericParameter( yoffset_val, yoffset_type, pen_glint_t ); -- yoffset : GLint
  ParseNextNumericParameter( zoffset_val, zoffset_type, pen_glint_t ); -- zoffset : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( depth_val, depth_type, pen_glsizei_t ); -- depth : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( pixels_val, pixels_type, pen_system.address_t ); -- pixels : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glTexSubImage3D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( xoffset_val ) ), GLint( to_numeric( yoffset_val ) ), GLint( to_numeric( zoffset_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLsizei( to_numeric( depth_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglTexSubImage3D;

procedure ParsePenglCopyTexSubImage3D is
  -- Syntax: glCopyTexSubImage3D( target : GLenum; level : GLint; xoffset, yoffset, zoffset : GLint; x, y : GLint; width, height : GLsizei );
  -- Source: bush_os.opengl.glCopyTexSubImage3D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  xoffset_val  : unbounded_string;
  xoffset_type : identifier;
  yoffset_val  : unbounded_string;
  yoffset_type : identifier;
  zoffset_val  : unbounded_string;
  zoffset_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
begin
  expect( pen_glcopytexsubimage3d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( xoffset_val, xoffset_type, pen_glint_t ); -- xoffset : GLint
  ParseNextNumericParameter( yoffset_val, yoffset_type, pen_glint_t ); -- yoffset : GLint
  ParseNextNumericParameter( zoffset_val, zoffset_type, pen_glint_t ); -- zoffset : GLint
  ParseNextNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseLastNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  if isExecutingCommand then
    begin
      glCopyTexSubImage3D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( xoffset_val ) ), GLint( to_numeric( yoffset_val ) ), GLint( to_numeric( zoffset_val ) ), GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyTexSubImage3D;

procedure ParsePenglColorTable is
  -- Syntax: glColorTable( target : GLenum; internalformat : GLenum; width : GLsizei; format : GLenum; kind : GLenum; table : System.address );
  -- Source: bush_os.opengl.glColorTable
  target_val  : unbounded_string;
  target_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  table_val  : unbounded_string;
--  table_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcolortable_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glenum_t ); -- internalformat : GLenum
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glenum_t ); -- format : GLenum
  ParseNextNumericParameter( kind_val, kind_type, pen_glenum_t ); -- kind : GLenum
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( table_val, table_type, pen_system.address_t ); -- table : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glColorTable( GLenum( to_numeric( target_val ) ), GLenum( to_numeric( internalformat_val ) ), GLsizei( to_numeric( width_val ) ), GLenum( to_numeric( format_val ) ), GLenum( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColorTable;

procedure ParsePenglColorSubTable is
  -- Syntax: glColorSubTable( target : GLarbmapping; start : GLsizei; count : GLsizei; format : GLbuffers; kind : GLtypes; data : System.address );
  -- Source: bush_os.opengl.glColorSubTable
  target_val  : unbounded_string;
  target_type : identifier;
  start_val  : unbounded_string;
  start_type : identifier;
  count_val  : unbounded_string;
  count_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcolorsubtable_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( start_val, start_type, pen_glsizei_t ); -- start : GLsizei
  ParseNextNumericParameter( count_val, count_type, pen_glsizei_t ); -- count : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glColorSubTable( GLarbmapping( to_numeric( target_val ) ), GLsizei( to_numeric( start_val ) ), GLsizei( to_numeric( count_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColorSubTable;

procedure ParsePenglColorTableParameteriv is
  -- Syntax: glColorTableParameteriv( target : GLarbmapping; pname : GLarbmapping; params : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glColorTableParameteriv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
  --params_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcolortableparameteriv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gl_int_array_ptr_t ); -- params : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glColorTableParameteriv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColorTableParameteriv;

procedure ParsePenglColorTableParameterfv is
  -- Syntax: glColorTableParameterfv( target : GLarbmapping; pname : GLarbmapping; params : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glColorTableParameterfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcolortableparameterfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  -- ParseLastNumericParameter( params_val, params_type, pen_gl_float_array_ptr_t ); -- params : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glColorTableParameterfv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglColorTableParameterfv;

procedure ParsePenglCopyColorSubTable is
  -- Syntax: glCopyColorSubTable( target : GLarbmapping; start : GLsizei; x, y : GLint; width : GLsizei );
  -- Source: bush_os.opengl.glCopyColorSubTable
  target_val  : unbounded_string;
  target_type : identifier;
  start_val  : unbounded_string;
  start_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
begin
  expect( pen_glcopycolorsubtable_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( start_val, start_type, pen_glsizei_t ); -- start : GLsizei
  ParseNextNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseLastNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  if isExecutingCommand then
    begin
      glCopyColorSubTable( GLarbmapping( to_numeric( target_val ) ), GLsizei( to_numeric( start_val ) ), GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyColorSubTable;

procedure ParsePenglCopyColorTable is
  -- Syntax: glCopyColorTable( target : GLarbmapping; internalformat : GLenum; x, y : GLint; width : GLsizei );
  -- Source: bush_os.opengl.glCopyColorTable
  target_val  : unbounded_string;
  target_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
begin
  expect( pen_glcopycolortable_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glenum_t ); -- internalformat : GLenum
  ParseNextNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseLastNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  if isExecutingCommand then
    begin
      glCopyColorTable( GLarbmapping( to_numeric( target_val ) ), GLenum( to_numeric( internalformat_val ) ), GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyColorTable;

procedure ParsePenglGetColorTable is
  -- Syntax: glGetColorTable( target : GLarbmapping; format : GLbuffers; kind : GLtypes; table : System.address );
  -- Source: bush_os.opengl.glGetColorTable
  target_val  : unbounded_string;
  target_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  table_val  : unbounded_string;
--  table_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetcolortable_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( table_val, table_type, pen_system.address_t ); -- table : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glGetColorTable( GLarbmapping( to_numeric( target_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetColorTable;

procedure ParsePenglGetColorTableParameterfv is
  -- Syntax: glGetColorTableParameterfv( target : GLarbmapping; pname : GLarbmapping; params : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.glGetColorTableParameterfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetcolortableparameterfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  -- TODO: takes a multivalue C float array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gl_float_array_ptr_t ); -- params : GL_Float_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glGetColorTableParameterfv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetColorTableParameterfv;

procedure ParsePenglGetColorTableParameteriv is
  -- Syntax: glGetColorTableParameteriv( target : GLarbmapping; pname : GLarbmapping; params : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.glGetColorTableParameteriv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetcolortableparameteriv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  -- TODO: takes a multivalue C int array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( params_val, params_type, pen_gl_int_array_ptr_t ); -- params : GL_Int_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glGetColorTableParameteriv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetColorTableParameteriv;

procedure ParsePenglBlendEquation is
  -- Syntax: glBlendEquation( mode : GLarbmapping );
  -- Source: bush_os.opengl.glBlendEquation
  mode_val  : unbounded_string;
  mode_type : identifier;
begin
  expect( pen_glblendequation_t );
  ParseSingleNumericParameter( mode_val, mode_type, pen_glarbmapping_t ); -- mode : GLarbmapping
  if isExecutingCommand then
    begin
      glBlendEquation( GLarbmapping( to_numeric( mode_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglBlendEquation;

procedure ParsePenglBlendColor is
  -- Syntax: glBlendColor( red, green, blue, alpha : GLclampf );
  -- Source: bush_os.opengl.glBlendColor
  red_val  : unbounded_string;
  red_type : identifier;
  green_val  : unbounded_string;
  green_type : identifier;
  blue_val  : unbounded_string;
  blue_type : identifier;
  alpha_val  : unbounded_string;
  alpha_type : identifier;
begin
  expect( pen_glblendcolor_t );
  ParseFirstNumericParameter( red_val, red_type, pen_glclampf_t ); -- red : GLclampf
  ParseNextNumericParameter( green_val, green_type, pen_glclampf_t ); -- green : GLclampf
  ParseNextNumericParameter( blue_val, blue_type, pen_glclampf_t ); -- blue : GLclampf
  ParseLastNumericParameter( alpha_val, alpha_type, pen_glclampf_t ); -- alpha : GLclampf
  if isExecutingCommand then
    begin
      glBlendColor( GLclampf( to_numeric( red_val ) ), GLclampf( to_numeric( green_val ) ), GLclampf( to_numeric( blue_val ) ), GLclampf( to_numeric( alpha_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglBlendColor;

procedure ParsePenglHistogram is
  -- Syntax: glHistogram( target : GLarbmapping; width : GLsizei; internalformat : GLenum; sink : GLboolean );
  -- Source: bush_os.opengl.glHistogram
  target_val  : unbounded_string;
  target_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  sink_val  : unbounded_string;
  sink_type : identifier;
begin
  expect( pen_glhistogram_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glenum_t ); -- internalformat : GLenum
  ParseLastNumericParameter( sink_val, sink_type, pen_glboolean_t ); -- sink : GLboolean
  if isExecutingCommand then
    begin
      glHistogram( GLarbmapping( to_numeric( target_val ) ), GLsizei( to_numeric( width_val ) ), GLenum( to_numeric( internalformat_val ) ), GLboolean( to_numeric( sink_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglHistogram;

procedure ParsePenglResetHistogram is
  -- Syntax: glResetHistogram( target : GLarbmapping );
  -- Source: bush_os.opengl.glResetHistogram
  target_val  : unbounded_string;
  target_type : identifier;
begin
  expect( pen_glresethistogram_t );
  ParseSingleNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  if isExecutingCommand then
    begin
      glResetHistogram( GLarbmapping( to_numeric( target_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglResetHistogram;

procedure ParsePenglGetHistogram is
  -- Syntax: glGetHistogram( target : GLarbmapping; reset : GLboolean; format : GLbuffers; kind : GLtypes; values : System.address );
  -- Source: bush_os.opengl.glGetHistogram
  target_val  : unbounded_string;
  target_type : identifier;
  reset_val  : unbounded_string;
  reset_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  values_val  : unbounded_string;
--  values_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgethistogram_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( reset_val, reset_type, pen_glboolean_t ); -- reset : GLboolean
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
--  ParseLastNumericParameter( values_val, values_type, pen_system.address_t ); -- values : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glGetHistogram( GLarbmapping( to_numeric( target_val ) ), GLboolean( to_numeric( reset_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetHistogram;

procedure ParsePenglGetHistogramParameterfv( result : out unbounded_string ) is
  -- Syntax: glGetHistogramParameterfv( target : GLarbmapping; pname : GLarbmapping; params : in out GLfloat );
  -- Source: bush_os.opengl.glGetHistogramParameterfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
  params_type : identifier;
begin
  expect( pen_glgethistogramparameterfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
  if isExecutingCommand then
    declare
      param : GLFloat := 0.0;
    begin
      glGetHistogramParameterfv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param );
      result := to_unbounded_string( GLFloat'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetHistogramParameterfv;

procedure ParsePenglGetHistogramParameteriv( result : out unbounded_string ) is
  -- Syntax: glGetHistogramParameteriv( target : GLarbmapping; pname : GLarbmapping; params : in out GLint );
  -- Source: bush_os.opengl.glGetHistogramParameteriv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
  params_type : identifier;
begin
  expect( pen_glgethistogramparameteriv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  ParseLastNumericParameter( params_val, params_type, pen_glint_t ); -- params : GLint
  if isExecutingCommand then
    declare
      param : GLInt := 0;
    begin
      glGetHistogramParameteriv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param );
      result := to_unbounded_string( GLInt'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetHistogramParameteriv;

procedure ParsePenglMinmax is
  -- Syntax: glMinmax( target : GLarbmapping; internalformat : GLenum; sink : GLboolean );
  -- Source: bush_os.opengl.glMinmax
  target_val  : unbounded_string;
  target_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  sink_val  : unbounded_string;
  sink_type : identifier;
begin
  expect( pen_glminmax_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glenum_t ); -- internalformat : GLenum
  ParseLastNumericParameter( sink_val, sink_type, pen_glboolean_t ); -- sink : GLboolean
  if isExecutingCommand then
    begin
      glMinmax( GLarbmapping( to_numeric( target_val ) ), GLenum( to_numeric( internalformat_val ) ), GLboolean( to_numeric( sink_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMinmax;

procedure ParsePenglResetMinmax is
  -- Syntax: glResetMinmax( target : GLarbmapping );
  -- Source: bush_os.opengl.glResetMinmax
  target_val  : unbounded_string;
  target_type : identifier;
begin
  expect( pen_glresetminmax_t );
  ParseSingleNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  if isExecutingCommand then
    begin
      glResetMinmax( GLarbmapping( to_numeric( target_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglResetMinmax;

procedure ParsePenglGetMinmax is
  -- Syntax: glGetMinmax( target : GLarbmapping; reset : GLboolean; format : GLbuffers; kind : GLtypes; values : System.address );
  -- Source: bush_os.opengl.glGetMinmax
  target_val  : unbounded_string;
  target_type : identifier;
  reset_val  : unbounded_string;
  reset_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  values_val  : unbounded_string;
--  values_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetminmax_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( reset_val, reset_type, pen_glboolean_t ); -- reset : GLboolean
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitypes array of parameters, but we don't support that in SparForte
  -- ParseLastNumericParameter( values_val, values_type, pen_system.address_t ); -- values : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glGetMinmax( GLarbmapping( to_numeric( target_val ) ), GLboolean( to_numeric( reset_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetMinmax;

procedure ParsePenglGetMinmaxParameterfv is
  -- Syntax: glGetMinmaxParameterfv( target : GLarbmapping; pname : GLarbmapping; params : in out GLfloat );
  -- Source: bush_os.opengl.glGetMinmaxParameterfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetminmaxparameterfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  -- TODO: returns two different sizes of arrays
  --ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Float_Array_Access := new float_array( 0..0 );
      param_ptr   : GL_Float_Array_Ptr := GL_Float_Array_Conv.To_Address( param_array );
    begin
      glGetMinmaxParameterfv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetMinmaxParameterfv;

procedure ParsePenglGetMinmaxParameteriv is
  -- Syntax: glGetMinmaxParameteriv( target : GLarbmapping; pname : GLarbmapping; params : in out GLint );
  -- Source: bush_os.opengl.glGetMinmaxParameteriv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
--  params_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetminmaxparameteriv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  -- TODO: returns two different sizes of arrays
  --ParseLastNumericParameter( params_val, params_type, pen_glint_t ); -- params : GLint
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      param_array : GL_Int_Array_Access := new int_array( 0..0 );
      param_ptr   : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( param_array );
    begin
      glGetMinmaxParameteriv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetMinmaxParameteriv;

procedure ParsePenglConvolutionFilter1D is
  -- Syntax: glConvolutionFilter1D( target : GLbuffers; internalformat : GLbuffers; width : GLsizei; format : GLbuffers; kind : GLtypes; image : System.address );
  -- Source: bush_os.opengl.glConvolutionFilter1D
  target_val  : unbounded_string;
  target_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  image_val  : unbounded_string;
--  image_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glconvolutionfilter1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glbuffers_t ); -- target : GLbuffers
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glbuffers_t ); -- internalformat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( image_val, image_type, pen_system.address_t ); -- image : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glConvolutionFilter1D( GLbuffers( to_numeric( target_val ) ), GLbuffers( to_numeric( internalformat_val ) ), GLsizei( to_numeric( width_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglConvolutionFilter1D;

procedure ParsePenglConvolutionFilter2D is
  -- Syntax: glConvolutionFilter2D( target : GLbuffers; internalformat : GLbuffers; width, height : GLsizei; format : GLbuffers; kind : GLtypes; image : System.address );
  -- Source: bush_os.opengl.glConvolutionFilter2D
  target_val  : unbounded_string;
  target_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  image_val  : unbounded_string;
--  image_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glconvolutionfilter2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glbuffers_t ); -- target : GLbuffers
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glbuffers_t ); -- internalformat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( image_val, image_type, pen_system.address_t ); -- image : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glConvolutionFilter2D( GLbuffers( to_numeric( target_val ) ), GLbuffers( to_numeric( internalformat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglConvolutionFilter2D;

procedure ParsePenglConvolutionParameterf is
  -- Syntax: glConvolutionParameterf( target : GLarbmapping; pname : GLarbmapping; params : GLfloat );
  -- Source: bush_os.opengl.glConvolutionParameterf
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
  params_type : identifier;
begin
  expect( pen_glconvolutionparameterf_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
  if isExecutingCommand then
    begin
      glConvolutionParameterf( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), GLfloat( to_numeric( params_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglConvolutionParameterf;

procedure ParsePenglConvolutionParameterfv( result : out unbounded_string ) is
  -- Syntax: glConvolutionParameterfv( target : GLarbmapping; pname : GLarbmapping; params : in out GLfloat );
  -- Source: bush_os.opengl.glConvolutionParameterfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
  params_type : identifier;
begin
  expect( pen_glconvolutionparameterfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
  if isExecutingCommand then
    declare
      param : GLFloat := 0.0;
    begin
      -- TODO: two different variations in the opengl binding
      glConvolutionParameterfv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param );
      result := to_unbounded_string( GLfloat'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglConvolutionParameterfv;

procedure ParsePenglConvolutionParameteri( result : out unbounded_string ) is
  -- Syntax: glConvolutionParameteri( target : GLarbmapping; pname : GLarbmapping; params : GLint );
  -- Source: bush_os.opengl.glConvolutionParameteri
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
  params_type : identifier;
begin
  expect( pen_glconvolutionparameteri_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  ParseLastNumericParameter( params_val, params_type, pen_glint_t ); -- params : GLint
  if isExecutingCommand then
    declare
      param : GLInt := 0;
    begin
      -- TODO: two different variations in the opengl binding
      glConvolutionParameteri( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param );
      result := to_unbounded_string( GLint'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglConvolutionParameteri;

procedure ParsePenglCopyConvolutionFilter1D is
  -- Syntax: glCopyConvolutionFilter1D( target : GLarbmapping; internalformat : GLarbmapping; x, y : GLint; width : GLsizei );
  -- Source: bush_os.opengl.glCopyConvolutionFilter1D
  target_val  : unbounded_string;
  target_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
begin
  expect( pen_glcopyconvolutionfilter1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glarbmapping_t ); -- internalformat : GLarbmapping
  ParseNextNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseLastNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  if isExecutingCommand then
    begin
      glCopyConvolutionFilter1D( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( internalformat_val ) ), GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyConvolutionFilter1D;

procedure ParsePenglCopyConvolutionFilter2D is
  -- Syntax: glCopyConvolutionFilter2D( target : GLarbmapping; internalformat : GLarbmapping; x, y : GLint; width, height : GLsizei );
  -- Source: bush_os.opengl.glCopyConvolutionFilter2D
  target_val  : unbounded_string;
  target_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
begin
  expect( pen_glcopyconvolutionfilter2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glarbmapping_t ); -- internalformat : GLarbmapping
  ParseNextNumericParameter( x_val, x_type, pen_glint_t ); -- x : GLint
  ParseNextNumericParameter( y_val, y_type, pen_glint_t ); -- y : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseLastNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  if isExecutingCommand then
    begin
      glCopyConvolutionFilter2D( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( internalformat_val ) ), GLint( to_numeric( x_val ) ), GLint( to_numeric( y_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCopyConvolutionFilter2D;

procedure ParsePenglGetConvolutionFilter is
  -- Syntax: glGetConvolutionFilter( target : GLarbmapping; format : GLbuffers; kind : GLtypes; image : System.address );
  -- Source: bush_os.opengl.glGetConvolutionFilter
  target_val  : unbounded_string;
  target_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  image_val  : unbounded_string;
--  image_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetconvolutionfilter_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  --ParseLastNumericParameter( image_val, image_type, pen_system.address_t ); -- image : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      glGetConvolutionFilter( GLarbmapping( to_numeric( target_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetConvolutionFilter;

procedure ParsePenglGetConvolutionParameterfv( result : out unbounded_string ) is
  -- Syntax: glGetConvolutionParameterfv( target : GLarbmapping; pname : GLarbmapping; params : in out GLfloat );
  -- Source: bush_os.opengl.glGetConvolutionParameterfv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
  params_type : identifier;
begin
  expect( pen_glgetconvolutionparameterfv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  ParseLastNumericParameter( params_val, params_type, pen_glfloat_t ); -- params : GLfloat
  if isExecutingCommand then
    declare
      param : GLFloat := 0.0;
    begin
      glGetConvolutionParameterfv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param );
      result := to_unbounded_string( GLFloat'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetConvolutionParameterfv;

procedure ParsePenglGetConvolutionParameteriv( result : out unbounded_string ) is
  -- Syntax: glGetConvolutionParameteriv( target : GLarbmapping; pname : GLarbmapping; params : in out GLint );
  -- Source: bush_os.opengl.glGetConvolutionParameteriv
  target_val  : unbounded_string;
  target_type : identifier;
  pname_val  : unbounded_string;
  pname_type : identifier;
  params_val  : unbounded_string;
  params_type : identifier;
begin
  expect( pen_glgetconvolutionparameteriv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( pname_val, pname_type, pen_glarbmapping_t ); -- pname : GLarbmapping
  ParseLastNumericParameter( params_val, params_type, pen_glint_t ); -- params : GLint
  if isExecutingCommand then
    declare
      param : GLInt := 0;
    begin
      glGetConvolutionParameteriv( GLarbmapping( to_numeric( target_val ) ), GLarbmapping( to_numeric( pname_val ) ), param );
      result := to_unbounded_string( GLInt'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetConvolutionParameteriv;

procedure ParsePenglSeparableFilter2D is
  -- Syntax: glSeparableFilter2D( target : GLarbmapping; internalformat : GLbuffers; width, height : GLsizei; format : GLbuffers; kind : GLtypes; row, column : System.address );
  -- Source: bush_os.opengl.glSeparableFilter2D
  target_val  : unbounded_string;
  target_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  row_val  : unbounded_string;
--  row_type : identifier;
  column_val  : unbounded_string;
--  column_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glseparablefilter2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glbuffers_t ); -- internalformat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
--  ParseNextNumericParameter( row_val, row_type, pen_system.address_t ); -- row : System.address
--  ParseLastNumericParameter( column_val, column_type, pen_system.address_t ); -- column : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
      dummy2 : boolean;
      dummy2_ptr : system.address := dummy2'address;
    begin
      -- TODO: two parameters are single dimensional arrays
      glSeparableFilter2D( GLarbmapping( to_numeric( target_val ) ), GLbuffers( to_numeric( internalformat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr, dummy2_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglSeparableFilter2D;

procedure ParsePenglGetSeparableFilter is
  -- Syntax: glGetSeparableFilter( target : GLarbmapping; format : GLbuffers; kind : GLtypes; row, column, span : System.address );
  -- Source: bush_os.opengl.glGetSeparableFilter
  target_val  : unbounded_string;
  target_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  row_val  : unbounded_string;
  -- row_type : identifier;
  column_val  : unbounded_string;
  -- column_type : identifier;
  span_val  : unbounded_string;
  -- span_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetseparablefilter_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glarbmapping_t ); -- target : GLarbmapping
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: takes a multivalue C multitype array of parameters, but we don't support that in SparForte
  -- ParseNextNumericParameter( row_val, row_type, pen_system.address_t ); -- row : System.address
  -- ParseNextNumericParameter( column_val, column_type, pen_system.address_t ); -- column : System.address
  -- ParseLastNumericParameter( span_val, span_type, pen_system.address_t ); -- span : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
      dummy2 : boolean;
      dummy2_ptr : system.address := dummy2'address;
      dummy3 : boolean;
      dummy3_ptr : system.address := dummy3'address;
    begin
      -- TODO: two one dimensional arrays
      glGetSeparableFilter( GLarbmapping( to_numeric( target_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr, dummy2_ptr, dummy3_ptr );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetSeparableFilter;

procedure ParsePenglActiveTexture is
  -- Syntax: glActiveTexture( texture : GLenum );
  -- Source: bush_os.opengl.glActiveTexture
  texture_val  : unbounded_string;
  texture_type : identifier;
begin
  expect( pen_glactivetexture_t );
  ParseSingleNumericParameter( texture_val, texture_type, pen_glenum_t ); -- texture : GLenum
  if isExecutingCommand then
    begin
      glActiveTexture( GLenum( to_numeric( texture_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglActiveTexture;

procedure ParsePenglClientActiveTexture is
  -- Syntax: glClientActiveTexture( texture : GLenum );
  -- Source: bush_os.opengl.glClientActiveTexture
  texture_val  : unbounded_string;
  texture_type : identifier;
begin
  expect( pen_glclientactivetexture_t );
  ParseSingleNumericParameter( texture_val, texture_type, pen_glenum_t ); -- texture : GLenum
  if isExecutingCommand then
    begin
      glClientActiveTexture( GLenum( to_numeric( texture_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglClientActiveTexture;

procedure ParsePenglCompressedTexImage1D is
  -- Syntax: glCompressedTexImage1D( target : GLenum; level : GLint; internalformat : GLbuffers; width : GLsizei; border : GLint; imageSize : GLsizei; data : System.address );
  -- Source: bush_os.opengl.glCompressedTexImage1D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  border_val  : unbounded_string;
  border_type : identifier;
  imageSize_val  : unbounded_string;
  imageSize_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcompressedteximage1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glbuffers_t ); -- internalformat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( border_val, border_type, pen_glint_t ); -- border : GLint
  ParseNextNumericParameter( imageSize_val, imageSize_type, pen_glsizei_t ); -- imageSize : GLsizei
  -- TODO: no man page for this?
  --ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    begin
      null; --glCompressedTexImage1D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLbuffers( to_numeric( internalformat_val ) ), GLsizei( to_numeric( width_val ) ), GLint( to_numeric( border_val ) ), GLsizei( to_numeric( imageSize_val ) ), System.address( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCompressedTexImage1D;

procedure ParsePenglCompressedTexImage2D is
  -- Syntax: glCompressedTexImage2D( target : GLenum; level : GLint; internalformat : GLbuffers; width, height : GLsizei; border : GLint; imageSize : GLsizei; data : System.address );
  -- Source: bush_os.opengl.glCompressedTexImage2D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  border_val  : unbounded_string;
  border_type : identifier;
  imageSize_val  : unbounded_string;
  imageSize_type : identifier;
  data_val  : unbounded_string;
 -- data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcompressedteximage2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glbuffers_t ); -- internalformat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( border_val, border_type, pen_glint_t ); -- border : GLint
  ParseNextNumericParameter( imageSize_val, imageSize_type, pen_glsizei_t ); -- imageSize : GLsizei
  -- TODO: no man page for this?
  --ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    begin
      null; --glCompressedTexImage2D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLbuffers( to_numeric( internalformat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLint( to_numeric( border_val ) ), GLsizei( to_numeric( imageSize_val ) ), System.address( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCompressedTexImage2D;

procedure ParsePenglCompressedTexImage3D is
  -- Syntax: glCompressedTexImage3D( target : GLenum; level : GLint; internalformat : GLbuffers; width, height, depth : GLsizei; border : GLint; imageSize : GLsizei; data : System.address );
  -- Source: bush_os.opengl.glCompressedTexImage3D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  internalformat_val  : unbounded_string;
  internalformat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  depth_val  : unbounded_string;
  depth_type : identifier;
  border_val  : unbounded_string;
  border_type : identifier;
  imageSize_val  : unbounded_string;
  imageSize_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcompressedteximage3d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( internalformat_val, internalformat_type, pen_glbuffers_t ); -- internalformat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( depth_val, depth_type, pen_glsizei_t ); -- depth : GLsizei
  ParseNextNumericParameter( border_val, border_type, pen_glint_t ); -- border : GLint
  ParseNextNumericParameter( imageSize_val, imageSize_type, pen_glsizei_t ); -- imageSize : GLsizei
  -- TODO: no man page for this?
  --ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    begin
      null; -- glCompressedTexImage3D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLbuffers( to_numeric( internalformat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLsizei( to_numeric( depth_val ) ), GLint( to_numeric( border_val ) ), GLsizei( to_numeric( imageSize_val ) ), System.address( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCompressedTexImage3D;

procedure ParsePenglCompressedTexSubImage1D is
  -- Syntax: glCompressedTexSubImage1D( target : GLenum; level : GLint; xoffset : GLint; width : GLsizei; format : GLbuffers; imageSize : GLsizei; data : System.address );
  -- Source: bush_os.opengl.glCompressedTexSubImage1D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  xoffset_val  : unbounded_string;
  xoffset_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  imageSize_val  : unbounded_string;
  imageSize_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcompressedtexsubimage1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( xoffset_val, xoffset_type, pen_glint_t ); -- xoffset : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( imageSize_val, imageSize_type, pen_glsizei_t ); -- imageSize : GLsizei
  --ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    begin
      -- TODO: no man page?
      null; -- glCompressedTexSubImage1D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( xoffset_val ) ), GLsizei( to_numeric( width_val ) ), GLbuffers( to_numeric( format_val ) ), GLsizei( to_numeric( imageSize_val ) ), System.address( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCompressedTexSubImage1D;

procedure ParsePenglCompressedTexSubImage2D is
  -- Syntax: glCompressedTexSubImage2D( target : GLenum; level : GLint; xoffset, yoffset : GLint; width, height : GLsizei; format : GLbuffers; imageSize : GLsizei; data : System.address );
  -- Source: bush_os.opengl.glCompressedTexSubImage2D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  xoffset_val  : unbounded_string;
  xoffset_type : identifier;
  yoffset_val  : unbounded_string;
  yoffset_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  imageSize_val  : unbounded_string;
  imageSize_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcompressedtexsubimage2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( xoffset_val, xoffset_type, pen_glint_t ); -- xoffset : GLint
  ParseNextNumericParameter( yoffset_val, yoffset_type, pen_glint_t ); -- yoffset : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( imageSize_val, imageSize_type, pen_glsizei_t ); -- imageSize : GLsizei
  --ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    begin
      null; -- glCompressedTexSubImage2D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( xoffset_val ) ), GLint( to_numeric( yoffset_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLbuffers( to_numeric( format_val ) ), GLsizei( to_numeric( imageSize_val ) ), System.address( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCompressedTexSubImage2D;

procedure ParsePenglCompressedTexSubImage3D is
  -- Syntax: glCompressedTexSubImage3D( target : GLenum; level : GLint; xoffset, yoffset, zoffset : GLint; width, height, depth : GLsizei; format : GLbuffers; imageSize : GLsizei; data : System.address );
  -- Source: bush_os.opengl.glCompressedTexSubImage3D
  target_val  : unbounded_string;
  target_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  xoffset_val  : unbounded_string;
  xoffset_type : identifier;
  yoffset_val  : unbounded_string;
  yoffset_type : identifier;
  zoffset_val  : unbounded_string;
  zoffset_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  depth_val  : unbounded_string;
  depth_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  imageSize_val  : unbounded_string;
  imageSize_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcompressedtexsubimage3d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( xoffset_val, xoffset_type, pen_glint_t ); -- xoffset : GLint
  ParseNextNumericParameter( yoffset_val, yoffset_type, pen_glint_t ); -- yoffset : GLint
  ParseNextNumericParameter( zoffset_val, zoffset_type, pen_glint_t ); -- zoffset : GLint
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( depth_val, depth_type, pen_glsizei_t ); -- depth : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( imageSize_val, imageSize_type, pen_glsizei_t ); -- imageSize : GLsizei
  --ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    begin
      -- TODO: no man page?
      null; --glCompressedTexSubImage3D( GLenum( to_numeric( target_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( xoffset_val ) ), GLint( to_numeric( yoffset_val ) ), GLint( to_numeric( zoffset_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLsizei( to_numeric( depth_val ) ), GLbuffers( to_numeric( format_val ) ), GLsizei( to_numeric( imageSize_val ) ), System.address( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCompressedTexSubImage3D;

procedure ParsePenglGetCompressedTexImage is
  -- Syntax: glGetCompressedTexImage( target : GLenum; lod : GLint; img : System.address );
  -- Source: bush_os.opengl.glGetCompressedTexImage
  target_val  : unbounded_string;
  target_type : identifier;
  lod_val  : unbounded_string;
  lod_type : identifier;
  img_val  : unbounded_string;
--  img_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetcompressedteximage_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( lod_val, lod_type, pen_glint_t ); -- lod : GLint
  --ParseLastNumericParameter( img_val, img_type, pen_system.address_t ); -- img : System.address
  if isExecutingCommand then
    begin
      -- TODO: no man page?
      null; -- glGetCompressedTexImage( GLenum( to_numeric( target_val ) ), GLint( to_numeric( lod_val ) ), System.address( to_numeric( img_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetCompressedTexImage;

procedure ParsePenglMultiTexCoord1d is
  -- Syntax: glMultiTexCoord1d( target : GLenum; s : GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord1d
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_glmultitexcoord1d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  if isExecutingCommand then
    begin
      glMultiTexCoord1d( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1d;

procedure ParsePenglMultiTexCoord1dv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord1dv( target : GLenum; v : in out GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord1dv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord1dv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_gldouble_t ); -- v : GLdouble
  if isExecutingCommand then
    declare
      param : GLdouble := 0.0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      glMultiTexCoord1dv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLdouble'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1dv;

procedure ParsePenglMultiTexCoord1f is
  -- Syntax: glMultiTexCoord1f( target : GLenum; s : GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord1f
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_glmultitexcoord1f_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  if isExecutingCommand then
    begin
      glMultiTexCoord1f( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1f;

procedure ParsePenglMultiTexCoord1fv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord1fv( target : GLenum; v : in out GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord1fv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord1fv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    declare
      param : GLfloat := 0.0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      glMultiTexCoord1fv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLfloat'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1fv;

procedure ParsePenglMultiTexCoord1i is
  -- Syntax: glMultiTexCoord1i( target : GLenum; s : GLint );
  -- Source: bush_os.opengl.glMultiTexCoord1i
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_glmultitexcoord1i_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  if isExecutingCommand then
    begin
      glMultiTexCoord1i( GLenum( to_numeric( target_val ) ), GLint( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1i;

procedure ParsePenglMultiTexCoord1iv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord1iv( target : GLenum; v : in out GLint );
  -- Source: bush_os.opengl.glMultiTexCoord1iv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord1iv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glint_t ); -- v : GLint
  if isExecutingCommand then
    declare
      param : GLint := 0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      glMultiTexCoord1iv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLint'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1iv;

procedure ParsePenglMultiTexCoord1s is
  -- Syntax: glMultiTexCoord1s( target : GLenum; s : GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord1s
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_glmultitexcoord1s_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  if isExecutingCommand then
    begin
      glMultiTexCoord1s( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1s;

procedure ParsePenglMultiTexCoord1sv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord1sv( target : GLenum; v : in out GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord1sv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord1sv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glshort_t ); -- v : GLshort
  if isExecutingCommand then
    declare
      param : GLshort := 0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      glMultiTexCoord1sv( GLenum( to_numeric( target_val ) ),  param );
      result := to_unbounded_string( GLshort'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1sv;

procedure ParsePenglMultiTexCoord2d is
  -- Syntax: glMultiTexCoord2d( target : GLenum; s, t : GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord2d
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_glmultitexcoord2d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  ParseLastNumericParameter( t_val, t_type, pen_gldouble_t ); -- t : GLdouble
  if isExecutingCommand then
    begin
      glMultiTexCoord2d( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( s_val ) ), GLdouble( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2d;

procedure ParsePenglMultiTexCoord2dv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord2dv( target : GLenum; v : in out GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord2dv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord2dv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_gldouble_t ); -- v : GLdouble
  if isExecutingCommand then
    declare
      param : GLdouble := 0.0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to two doubles?
      glMultiTexCoord2dv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLdouble'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2dv;

procedure ParsePenglMultiTexCoord2f is
  -- Syntax: glMultiTexCoord2f( target : GLenum; s, t : GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord2f
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_glmultitexcoord2f_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  ParseLastNumericParameter( t_val, t_type, pen_glfloat_t ); -- t : GLfloat
  if isExecutingCommand then
    begin
      glMultiTexCoord2f( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( s_val ) ), GLfloat( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2f;

procedure ParsePenglMultiTexCoord2fv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord2fv( target : GLenum; v : in out GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord2fv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord2fv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    declare
      param : GLfloat := 0.0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to two floats?
      glMultiTexCoord2fv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLfloat'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2fv;

procedure ParsePenglMultiTexCoord2i is
  -- Syntax: glMultiTexCoord2i( target : GLenum; s, t : GLint );
  -- Source: bush_os.opengl.glMultiTexCoord2i
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_glmultitexcoord2i_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  ParseLastNumericParameter( t_val, t_type, pen_glint_t ); -- t : GLint
  if isExecutingCommand then
    begin
      glMultiTexCoord2i( GLenum( to_numeric( target_val ) ), GLint( to_numeric( s_val ) ), GLint( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2i;

procedure ParsePenglMultiTexCoord2iv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord2iv( target : GLenum; v : in out GLint );
  -- Source: bush_os.opengl.glMultiTexCoord2iv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord2iv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glint_t ); -- v : GLint
  if isExecutingCommand then
    declare
      param : GLint := 0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to two ints?
      glMultiTexCoord2iv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLint'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2iv;

procedure ParsePenglMultiTexCoord2s is
  -- Syntax: glMultiTexCoord2s( target : GLenum; s, t : GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord2s
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_glmultitexcoord2s_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  ParseLastNumericParameter( t_val, t_type, pen_glshort_t ); -- t : GLshort
  if isExecutingCommand then
    begin
      glMultiTexCoord2s( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( s_val ) ), GLshort( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2s;

procedure ParsePenglMultiTexCoord2sv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord2sv( target : GLenum; v : in out GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord2sv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord2sv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glshort_t ); -- v : GLshort
  if isExecutingCommand then
    declare
      param : GLshort := 0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to two shorts?
      glMultiTexCoord2sv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLshort'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2sv;

procedure ParsePenglMultiTexCoord3d is
  -- Syntax: glMultiTexCoord3d( target : GLenum; s, t, r : GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord3d
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_glmultitexcoord3d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  ParseNextNumericParameter( t_val, t_type, pen_gldouble_t ); -- t : GLdouble
  ParseLastNumericParameter( r_val, r_type, pen_gldouble_t ); -- r : GLdouble
  if isExecutingCommand then
    begin
      glMultiTexCoord3d( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( s_val ) ), GLdouble( to_numeric( t_val ) ), GLdouble( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3d;

procedure ParsePenglMultiTexCoord3dv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord3dv( target : GLenum; v : in out GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord3dv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord3dv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_gldouble_t ); -- v : GLdouble
  if isExecutingCommand then
    declare
      param : GLdouble := 0.0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to three doubles?
      glMultiTexCoord3dv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLdouble'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3dv;

procedure ParsePenglMultiTexCoord3f is
  -- Syntax: glMultiTexCoord3f( target : GLenum; s, t, r : GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord3f
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_glmultitexcoord3f_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  ParseNextNumericParameter( t_val, t_type, pen_glfloat_t ); -- t : GLfloat
  ParseLastNumericParameter( r_val, r_type, pen_glfloat_t ); -- r : GLfloat
  if isExecutingCommand then
    begin
      glMultiTexCoord3f( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( s_val ) ), GLfloat( to_numeric( t_val ) ), GLfloat( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3f;

procedure ParsePenglMultiTexCoord3fv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord3fv( target : GLenum; v : in out GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord3fv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord3fv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    declare
      param : GLfloat := 0.0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to three float?
      glMultiTexCoord3fv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLfloat'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3fv;

procedure ParsePenglMultiTexCoord3i is
  -- Syntax: glMultiTexCoord3i( target : GLenum; s, t, r : GLint );
  -- Source: bush_os.opengl.glMultiTexCoord3i
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_glmultitexcoord3i_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  ParseNextNumericParameter( t_val, t_type, pen_glint_t ); -- t : GLint
  ParseLastNumericParameter( r_val, r_type, pen_glint_t ); -- r : GLint
  if isExecutingCommand then
    begin
      glMultiTexCoord3i( GLenum( to_numeric( target_val ) ), GLint( to_numeric( s_val ) ), GLint( to_numeric( t_val ) ), GLint( to_numeric( r_val ) )  );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3i;

procedure ParsePenglMultiTexCoord3iv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord3iv( target : GLenum; v : in out GLint );
  -- Source: bush_os.opengl.glMultiTexCoord3iv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord3iv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glint_t ); -- v : GLint
  if isExecutingCommand then
    declare
      param : GLint := 0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to three float?
      glMultiTexCoord3iv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLint'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3iv;

procedure ParsePenglMultiTexCoord3s is
  -- Syntax: glMultiTexCoord3s( target : GLenum; s, t, r : GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord3s
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_glmultitexcoord3s_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  ParseNextNumericParameter( t_val, t_type, pen_glshort_t ); -- t : GLshort
  ParseLastNumericParameter( r_val, r_type, pen_glshort_t ); -- r : GLshort
  if isExecutingCommand then
    begin
      glMultiTexCoord3s( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( s_val ) ), GLshort( to_numeric( t_val ) ), GLshort( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3s;

procedure ParsePenglMultiTexCoord3sv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord3sv( target : GLenum; v : in out GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord3sv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord3sv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glshort_t ); -- v : GLshort
  if isExecutingCommand then
    declare
      param : GLshort := 0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to three float?
      glMultiTexCoord3sv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLshort'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3sv;

procedure ParsePenglMultiTexCoord4d is
  -- Syntax: glMultiTexCoord4d( target : GLenum; s, t, r, q : GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord4d
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_glmultitexcoord4d_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  ParseNextNumericParameter( t_val, t_type, pen_gldouble_t ); -- t : GLdouble
  ParseNextNumericParameter( r_val, r_type, pen_gldouble_t ); -- r : GLdouble
  ParseLastNumericParameter( q_val, q_type, pen_gldouble_t ); -- q : GLdouble
  if isExecutingCommand then
    begin
      glMultiTexCoord4d( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( s_val ) ), GLdouble( to_numeric( t_val ) ), GLdouble( to_numeric( r_val ) ), GLdouble( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4d;

procedure ParsePenglMultiTexCoord4dv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord4dv( target : GLenum; v : in out GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord4dv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord4dv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_gldouble_t ); -- v : GLdouble
  if isExecutingCommand then
    declare
      param : GLdouble := 0.0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to four double?
      glMultiTexCoord4dv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLdouble'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4dv;

procedure ParsePenglMultiTexCoord4f is
  -- Syntax: glMultiTexCoord4f( target : GLenum; s, t, r, q : GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord4f
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_glmultitexcoord4f_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  ParseNextNumericParameter( t_val, t_type, pen_glfloat_t ); -- t : GLfloat
  ParseNextNumericParameter( r_val, r_type, pen_glfloat_t ); -- r : GLfloat
  ParseLastNumericParameter( q_val, q_type, pen_glfloat_t ); -- q : GLfloat
  if isExecutingCommand then
    begin
      glMultiTexCoord4f( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( s_val ) ), GLfloat( to_numeric( t_val ) ), GLfloat( to_numeric( r_val ) ), GLfloat( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4f;

procedure ParsePenglMultiTexCoord4fv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord4fv( target : GLenum; v : in out GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord4fv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord4fv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    declare
      param : GLfloat := 0.0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to four float?
      glMultiTexCoord4fv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLfloat'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4fv;

procedure ParsePenglMultiTexCoord4i is
  -- Syntax: glMultiTexCoord4i( target : GLenum; s, t, r, q : GLint );
  -- Source: bush_os.opengl.glMultiTexCoord4i
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_glmultitexcoord4i_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  ParseNextNumericParameter( t_val, t_type, pen_glint_t ); -- t : GLint
  ParseNextNumericParameter( r_val, r_type, pen_glint_t ); -- r : GLint
  ParseLastNumericParameter( q_val, q_type, pen_glint_t ); -- q : GLint
  if isExecutingCommand then
    begin
      glMultiTexCoord4i( GLenum( to_numeric( target_val ) ), GLint( to_numeric( s_val ) ), GLint( to_numeric( t_val ) ), GLint( to_numeric( r_val ) ), GLint( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4i;

procedure ParsePenglMultiTexCoord4iv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord4iv( target : GLenum; v : in out GLint );
  -- Source: bush_os.opengl.glMultiTexCoord4iv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord4iv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glint_t ); -- v : GLint
  if isExecutingCommand then
    declare
      param : GLint := 0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to four int?
      glMultiTexCoord4iv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLint'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4iv;

procedure ParsePenglMultiTexCoord4s is
  -- Syntax: glMultiTexCoord4s( target : GLenum; s, t, r, q : GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord4s
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_glmultitexcoord4s_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  ParseNextNumericParameter( t_val, t_type, pen_glshort_t ); -- t : GLshort
  ParseNextNumericParameter( r_val, r_type, pen_glshort_t ); -- r : GLshort
  ParseLastNumericParameter( q_val, q_type, pen_glshort_t ); -- q : GLshort
  if isExecutingCommand then
    begin
      glMultiTexCoord4s( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( s_val ) ), GLshort( to_numeric( t_val ) ), GLshort( to_numeric( r_val ) ), GLshort( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4s;

procedure ParsePenglMultiTexCoord4sv( result : out unbounded_string ) is
  -- Syntax: glMultiTexCoord4sv( target : GLenum; v : in out GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord4sv
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord4sv_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glshort_t ); -- v : GLshort
  if isExecutingCommand then
    declare
      param : GLshort := 0;
    begin
      -- TODO: no man page? do we need to assign the value before calling?
      -- TODO: pointer to four short?
      glMultiTexCoord4sv( GLenum( to_numeric( target_val ) ), param );
      result := to_unbounded_string( GLshort'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4sv;

procedure ParsePenglLoadTransposeMatrixd is
  -- TODO: matrix operations not implemented
  -- Syntax: --glLoadTransposeMatrixd( GLdouble m[16] );
  -- Source: bush_os.opengl.glLoadTransposeMatrixd
  --GLdoublem[16]_val  : unbounded_string;
  --GLdoublem[16]_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glloadtransposematrixd_t );
  null; --ParseSingleNumericParameter( GLdoublem[16]_val, GLdoublem[16]_type, pen_gldoublem[16]_t ); -- GLdoublem[16] : GLdoublem[16]
  if isExecutingCommand then
    begin
      null; --glLoadTransposeMatrixd( GLdoublem[16]( to_numeric( GLdoublem[16]_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLoadTransposeMatrixd;

procedure ParsePenglLoadTransposeMatrixf is
  -- TODO: matrix operations not implemented
  -- Syntax: --glLoadTransposeMatrixf( GLfloat m[16] );
  -- Source: bush_os.opengl.glLoadTransposeMatrixf
  --GLfloatm[16]_val  : unbounded_string;
  --GLfloatm[16]_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glloadtransposematrixf_t );
  null; --ParseSingleNumericParameter( GLfloatm[16]_val, GLfloatm[16]_type, pen_glfloatm[16]_t ); -- GLfloatm[16] : GLfloatm[16]
  if isExecutingCommand then
    begin
      null; --glLoadTransposeMatrixf( GLfloatm[16]( to_numeric( GLfloatm[16]_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglLoadTransposeMatrixf;

procedure ParsePenglMultTransposeMatrixd is
  -- TODO: matrix operations not implemented
  -- Syntax: --glMultTransposeMatrixd( GLdouble m[16] );
  -- Source: bush_os.opengl.glMultTransposeMatrixd
  --GLdoublem[16]_val  : unbounded_string;
  --GLdoublem[16]_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glmulttransposematrixd_t );
  null; --ParseSingleNumericParameter( GLdoublem[16]_val, GLdoublem[16]_type, pen_gldoublem[16]_t ); -- GLdoublem[16] : GLdoublem[16]
  if isExecutingCommand then
    begin
      null; --glMultTransposeMatrixd( GLdoublem[16]( to_numeric( GLdoublem[16]_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultTransposeMatrixd;

procedure ParsePenglMultTransposeMatrixf is
  -- TODO: matrix operations not implemented
  -- Syntax: --glMultTransposeMatrixf( GLfloat m[16] );
  -- Source: bush_os.opengl.glMultTransposeMatrixf
  --GLfloatm[16]_val  : unbounded_string;
  --GLfloatm[16]_type : identifier;
begin
  err( "Not yet implemented" );
  expect( pen_glmulttransposematrixf_t );
  --ParseSingleNumericParameter( GLfloatm[16]_val, GLfloatm[16]_type, pen_glfloatm[16]_t ); -- GLfloatm[16] : GLfloatm[16]
  if isExecutingCommand then
    begin
      null; --glMultTransposeMatrixf( GLfloatm[16]( to_numeric( GLfloatm[16]_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultTransposeMatrixf;

procedure ParsePenglSampleCoverage is
  -- Syntax: glSampleCoverage( value : GLclampf; invert : GLboolean );
  -- Source: bush_os.opengl.glSampleCoverage
  value_val  : unbounded_string;
  value_type : identifier;
  invert_val  : unbounded_string;
  invert_type : identifier;
begin
  expect( pen_glsamplecoverage_t );
  ParseFirstNumericParameter( value_val, value_type, pen_glclampf_t ); -- value : GLclampf
  ParseLastNumericParameter( invert_val, invert_type, pen_glboolean_t ); -- invert : GLboolean
  if isExecutingCommand then
    begin
      glSampleCoverage( GLclampf( to_numeric( value_val ) ), GLboolean( to_numeric( invert_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglSampleCoverage;

procedure ParsePenglActiveTextureARB is
  -- Syntax: glActiveTextureARB( texture : GLenum );
  -- Source: bush_os.opengl.glActiveTextureARB
  texture_val  : unbounded_string;
  texture_type : identifier;
begin
  expect( pen_glactivetexturearb_t );
  ParseSingleNumericParameter( texture_val, texture_type, pen_glenum_t ); -- texture : GLenum
  if isExecutingCommand then
    begin
      glActiveTextureARB( GLenum( to_numeric( texture_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglActiveTextureARB;

procedure ParsePenglClientActiveTextureARB is
  -- Syntax: glClientActiveTextureARB( texture : GLenum );
  -- Source: bush_os.opengl.glClientActiveTextureARB
  texture_val  : unbounded_string;
  texture_type : identifier;
begin
  expect( pen_glclientactivetexturearb_t );
  ParseSingleNumericParameter( texture_val, texture_type, pen_glenum_t ); -- texture : GLenum
  if isExecutingCommand then
    begin
      glClientActiveTextureARB( GLenum( to_numeric( texture_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglClientActiveTextureARB;

procedure ParsePenglMultiTexCoord1dARB is
  -- Syntax: glMultiTexCoord1dARB( target : GLenum; s : GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord1dARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_glmultitexcoord1darb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  if isExecutingCommand then
    begin
      glMultiTexCoord1dARB( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1dARB;

procedure ParsePenglMultiTexCoord1dvARB is
  -- Syntax: glMultiTexCoord1dvARB( target : GLenum; v : in out GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord1dvARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord1dvarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_gldouble_t ); -- v : GLdouble
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord1dvARB( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1dvARB;

procedure ParsePenglMultiTexCoord1fARB is
  -- Syntax: glMultiTexCoord1fARB( target : GLenum; s : GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord1fARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_glmultitexcoord1farb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  if isExecutingCommand then
    begin
      glMultiTexCoord1fARB( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1fARB;

procedure ParsePenglMultiTexCoord1fvARB is
  -- Syntax: glMultiTexCoord1fvARB( target : GLenum; v : in out GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord1fvARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord1fvarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord1fvARB( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1fvARB;

procedure ParsePenglMultiTexCoord1iARB is
  -- Syntax: glMultiTexCoord1iARB( target : GLenum; s : GLint );
  -- Source: bush_os.opengl.glMultiTexCoord1iARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_glmultitexcoord1iarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  if isExecutingCommand then
    begin
      glMultiTexCoord1iARB( GLenum( to_numeric( target_val ) ), GLint( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1iARB;

procedure ParsePenglMultiTexCoord1ivARB is
  -- Syntax: glMultiTexCoord1ivARB( target : GLenum; v : in out GLint );
  -- Source: bush_os.opengl.glMultiTexCoord1ivARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord1ivarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glint_t ); -- v : GLint
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord1ivARB( GLenum( to_numeric( target_val ) ), GLint( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1ivARB;

procedure ParsePenglMultiTexCoord1sARB is
  -- Syntax: glMultiTexCoord1sARB( target : GLenum; s : GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord1sARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
begin
  expect( pen_glmultitexcoord1sarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  if isExecutingCommand then
    begin
      glMultiTexCoord1sARB( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( s_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1sARB;

procedure ParsePenglMultiTexCoord1svARB is
  -- Syntax: glMultiTexCoord1svARB( target : GLenum; v : in out GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord1svARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord1svarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glshort_t ); -- v : GLshort
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord1svARB( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord1svARB;

procedure ParsePenglMultiTexCoord2dARB is
  -- Syntax: glMultiTexCoord2dARB( target : GLenum; s, t : GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord2dARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_glmultitexcoord2darb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  ParseLastNumericParameter( t_val, t_type, pen_gldouble_t ); -- t : GLdouble
  if isExecutingCommand then
    begin
      glMultiTexCoord2dARB( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( s_val ) ), GLdouble( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2dARB;

procedure ParsePenglMultiTexCoord2dvARB is
  -- Syntax: glMultiTexCoord2dvARB( target : GLenum; v : in out GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord2dvARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord2dvarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_gldouble_t ); -- v : GLdouble
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord2dvARB( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2dvARB;

procedure ParsePenglMultiTexCoord2fARB is
  -- Syntax: glMultiTexCoord2fARB( target : GLenum; s, t : GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord2fARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_glmultitexcoord2farb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  ParseLastNumericParameter( t_val, t_type, pen_glfloat_t ); -- t : GLfloat
  if isExecutingCommand then
    begin
      glMultiTexCoord2fARB( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( s_val ) ), GLfloat( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2fARB;

procedure ParsePenglMultiTexCoord2fvARB is
  -- Syntax: glMultiTexCoord2fvARB( target : GLenum; v : in out GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord2fvARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord2fvarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord2fvARB( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2fvARB;

procedure ParsePenglMultiTexCoord2iARB is
  -- Syntax: glMultiTexCoord2iARB( target : GLenum; s, t : GLint );
  -- Source: bush_os.opengl.glMultiTexCoord2iARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_glmultitexcoord2iarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  ParseLastNumericParameter( t_val, t_type, pen_glint_t ); -- t : GLint
  if isExecutingCommand then
    begin
      glMultiTexCoord2iARB( GLenum( to_numeric( target_val ) ), GLint( to_numeric( s_val ) ), GLint( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2iARB;

procedure ParsePenglMultiTexCoord2ivARB is
  -- Syntax: glMultiTexCoord2ivARB( target : GLenum; v : in out GLint );
  -- Source: bush_os.opengl.glMultiTexCoord2ivARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord2ivarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glint_t ); -- v : GLint
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord2ivARB( GLenum( to_numeric( target_val ) ), GLint( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2ivARB;

procedure ParsePenglMultiTexCoord2sARB is
  -- Syntax: glMultiTexCoord2sARB( target : GLenum; s, t : GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord2sARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
begin
  expect( pen_glmultitexcoord2sarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  ParseLastNumericParameter( t_val, t_type, pen_glshort_t ); -- t : GLshort
  if isExecutingCommand then
    begin
      glMultiTexCoord2sARB( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( s_val ) ), GLshort( to_numeric( t_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2sARB;

procedure ParsePenglMultiTexCoord2svARB is
  -- Syntax: glMultiTexCoord2svARB( target : GLenum; v : in out GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord2svARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord2svarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glshort_t ); -- v : GLshort
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord2svARB( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord2svARB;

procedure ParsePenglMultiTexCoord3dARB is
  -- Syntax: glMultiTexCoord3dARB( target : GLenum; s, t, r : GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord3dARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_glmultitexcoord3darb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  ParseNextNumericParameter( t_val, t_type, pen_gldouble_t ); -- t : GLdouble
  ParseLastNumericParameter( r_val, r_type, pen_gldouble_t ); -- r : GLdouble
  if isExecutingCommand then
    begin
      glMultiTexCoord3dARB( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( s_val ) ), GLdouble( to_numeric( t_val ) ), GLdouble( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3dARB;

procedure ParsePenglMultiTexCoord3dvARB is
  -- Syntax: glMultiTexCoord3dvARB( target : GLenum; v : in out GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord3dvARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord3dvarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_gldouble_t ); -- v : GLdouble
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord3dvARB( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3dvARB;

procedure ParsePenglMultiTexCoord3fARB is
  -- Syntax: glMultiTexCoord3fARB( target : GLenum; s, t, r : GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord3fARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_glmultitexcoord3farb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  ParseNextNumericParameter( t_val, t_type, pen_glfloat_t ); -- t : GLfloat
  ParseLastNumericParameter( r_val, r_type, pen_glfloat_t ); -- r : GLfloat
  if isExecutingCommand then
    begin
      glMultiTexCoord3fARB( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( s_val ) ), GLfloat( to_numeric( t_val ) ), GLfloat( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3fARB;

procedure ParsePenglMultiTexCoord3fvARB is
  -- Syntax: glMultiTexCoord3fvARB( target : GLenum; v : in out GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord3fvARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord3fvarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord3fvARB( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3fvARB;

procedure ParsePenglMultiTexCoord3iARB is
  -- Syntax: glMultiTexCoord3iARB( target : GLenum; s, t, r : GLint );
  -- Source: bush_os.opengl.glMultiTexCoord3iARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_glmultitexcoord3iarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  ParseNextNumericParameter( t_val, t_type, pen_glint_t ); -- t : GLint
  ParseLastNumericParameter( r_val, r_type, pen_glint_t ); -- r : GLint
  if isExecutingCommand then
    begin
      glMultiTexCoord3iARB( GLenum( to_numeric( target_val ) ), GLint( to_numeric( s_val ) ), GLint( to_numeric( t_val ) ), GLint( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3iARB;

procedure ParsePenglMultiTexCoord3ivARB is
  -- Syntax: glMultiTexCoord3ivARB( target : GLenum; v : in out GLint );
  -- Source: bush_os.opengl.glMultiTexCoord3ivARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord3ivarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glint_t ); -- v : GLint
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord3ivARB( GLenum( to_numeric( target_val ) ), GLint( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3ivARB;

procedure ParsePenglMultiTexCoord3sARB is
  -- Syntax: glMultiTexCoord3sARB( target : GLenum; s, t, r : GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord3sARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
begin
  expect( pen_glmultitexcoord3sarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  ParseNextNumericParameter( t_val, t_type, pen_glshort_t ); -- t : GLshort
  ParseLastNumericParameter( r_val, r_type, pen_glshort_t ); -- r : GLshort
  if isExecutingCommand then
    begin
      glMultiTexCoord3sARB( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( s_val ) ), GLshort( to_numeric( t_val ) ), GLshort( to_numeric( r_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3sARB;

procedure ParsePenglMultiTexCoord3svARB is
  -- Syntax: glMultiTexCoord3svARB( target : GLenum; v : in out GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord3svARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord3svarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glshort_t ); -- v : GLshort
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord3svARB( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord3svARB;

procedure ParsePenglMultiTexCoord4dARB is
  -- Syntax: glMultiTexCoord4dARB( target : GLenum; s, t, r, q : GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord4dARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_glmultitexcoord4darb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_gldouble_t ); -- s : GLdouble
  ParseNextNumericParameter( t_val, t_type, pen_gldouble_t ); -- t : GLdouble
  ParseNextNumericParameter( r_val, r_type, pen_gldouble_t ); -- r : GLdouble
  ParseLastNumericParameter( q_val, q_type, pen_gldouble_t ); -- q : GLdouble
  if isExecutingCommand then
    begin
      glMultiTexCoord4dARB( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( s_val ) ), GLdouble( to_numeric( t_val ) ), GLdouble( to_numeric( r_val ) ), GLdouble( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4dARB;

procedure ParsePenglMultiTexCoord4dvARB is
  -- Syntax: glMultiTexCoord4dvARB( target : GLenum; v : in out GLdouble );
  -- Source: bush_os.opengl.glMultiTexCoord4dvARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord4dvarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_gldouble_t ); -- v : GLdouble
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord4dvARB( GLenum( to_numeric( target_val ) ), GLdouble( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4dvARB;

procedure ParsePenglMultiTexCoord4fARB is
  -- Syntax: glMultiTexCoord4fARB( target : GLenum; s, t, r, q : GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord4fARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_glmultitexcoord4farb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glfloat_t ); -- s : GLfloat
  ParseNextNumericParameter( t_val, t_type, pen_glfloat_t ); -- t : GLfloat
  ParseNextNumericParameter( r_val, r_type, pen_glfloat_t ); -- r : GLfloat
  ParseLastNumericParameter( q_val, q_type, pen_glfloat_t ); -- q : GLfloat
  if isExecutingCommand then
    begin
      glMultiTexCoord4fARB( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( s_val ) ), GLfloat( to_numeric( t_val ) ), GLfloat( to_numeric( r_val ) ), GLfloat( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4fARB;

procedure ParsePenglMultiTexCoord4fvARB is
  -- Syntax: glMultiTexCoord4fvARB( target : GLenum; v : in out GLfloat );
  -- Source: bush_os.opengl.glMultiTexCoord4fvARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord4fvarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord4fvARB( GLenum( to_numeric( target_val ) ), GLfloat( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4fvARB;

procedure ParsePenglMultiTexCoord4iARB is
  -- Syntax: glMultiTexCoord4iARB( target : GLenum; s, t, r, q : GLint );
  -- Source: bush_os.opengl.glMultiTexCoord4iARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_glmultitexcoord4iarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glint_t ); -- s : GLint
  ParseNextNumericParameter( t_val, t_type, pen_glint_t ); -- t : GLint
  ParseNextNumericParameter( r_val, r_type, pen_glint_t ); -- r : GLint
  ParseLastNumericParameter( q_val, q_type, pen_glint_t ); -- q : GLint
  if isExecutingCommand then
    begin
      glMultiTexCoord4iARB( GLenum( to_numeric( target_val ) ), GLint( to_numeric( s_val ) ), GLint( to_numeric( t_val ) ), GLint( to_numeric( r_val ) ), GLint( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4iARB;

procedure ParsePenglMultiTexCoord4ivARB is
  -- Syntax: glMultiTexCoord4ivARB( target : GLenum; v : in out GLint );
  -- Source: bush_os.opengl.glMultiTexCoord4ivARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord4ivarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glint_t ); -- v : GLint
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord4ivARB( GLenum( to_numeric( target_val ) ), GLint( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4ivARB;

procedure ParsePenglMultiTexCoord4sARB is
  -- Syntax: glMultiTexCoord4sARB( target : GLenum; s, t, r, q : GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord4sARB
  target_val  : unbounded_string;
  target_type : identifier;
  s_val  : unbounded_string;
  s_type : identifier;
  t_val  : unbounded_string;
  t_type : identifier;
  r_val  : unbounded_string;
  r_type : identifier;
  q_val  : unbounded_string;
  q_type : identifier;
begin
  expect( pen_glmultitexcoord4sarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( s_val, s_type, pen_glshort_t ); -- s : GLshort
  ParseNextNumericParameter( t_val, t_type, pen_glshort_t ); -- t : GLshort
  ParseNextNumericParameter( r_val, r_type, pen_glshort_t ); -- r : GLshort
  ParseLastNumericParameter( q_val, q_type, pen_glshort_t ); -- q : GLshort
  if isExecutingCommand then
    begin
      glMultiTexCoord4sARB( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( s_val ) ), GLshort( to_numeric( t_val ) ), GLshort( to_numeric( r_val ) ), GLshort( to_numeric( q_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4sARB;

procedure ParsePenglMultiTexCoord4svARB is
  -- Syntax: glMultiTexCoord4svARB( target : GLenum; v : in out GLshort );
  -- Source: bush_os.opengl.glMultiTexCoord4svARB
  target_val  : unbounded_string;
  target_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glmultitexcoord4svarb_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseLastNumericParameter( v_val, v_type, pen_glshort_t ); -- v : GLshort
  if isExecutingCommand then
    begin
      -- TODO: no man page for this
      null; -- glMultiTexCoord4svARB( GLenum( to_numeric( target_val ) ), GLshort( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglMultiTexCoord4svARB;

procedure ParsePenglCreateDebugObjectMESA is
-- ParsePenglCreateDebugObjectMESA( result : out unbounded_string ) is
  -- Syntax: --glCreateDebugObjectMESA return GLhandleARB;
  -- Source: bush_os.opengl.glCreateDebugObjectMESA
begin
  err( "not yet complete" );
  expect( pen_glcreatedebugobjectmesa_t );
  if isExecutingCommand then
    begin
      -- TODO: MESA specific
      null; -- esult := to_unbounded_string( long_float( glCreateDebugObjectMESA ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglCreateDebugObjectMESA;

procedure ParsePenglClearDebugLogMESA is
  -- Syntax: --glClearDebugLogMESA( obj : GLhandleARB; logType : GLenum; shaderType : GLenum);
  -- Source: bush_os.opengl.glClearDebugLogMESA
  obj_val  : unbounded_string;
--  obj_type : identifier;
  logType_val  : unbounded_string;
  logType_type : identifier;
  shaderType_val  : unbounded_string;
  shaderType_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glcleardebuglogmesa_t );
  -- TODO: GLhandleARB not defined
  -- ParseFirstNumericParameter( obj_val, obj_type, pen_glhandlearb_t ); -- obj : GLhandleARB
  ParseNextNumericParameter( logType_val, logType_type, pen_glenum_t ); -- logType : GLenum
  ParseLastNumericParameter( shaderType_val, shaderType_type, pen_glenum_t ); -- shaderType : GLenum
  if isExecutingCommand then
    begin
-- TODO: MESA specific
      null; -- glClearDebugLogMESA( GLhandleARB( to_numeric( obj_val ) ), GLenum( to_numeric( logType_val ) ), GLenum( to_numeric( shaderType_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglClearDebugLogMESA;

procedure ParsePenglGetDebugLogMESA is
  -- Syntax: --glGetDebugLogMESA( obj : GLhandleARB; logType : GLenum; shaderType : GLenum; maxLength : GLsizei; length : in out GLsizei; debugLog : in out GLcharARB );
  -- Source: bush_os.opengl.glGetDebugLogMESA
  obj_val  : unbounded_string;
--  obj_type : identifier;
  logType_val  : unbounded_string;
  logType_type : identifier;
  shaderType_val  : unbounded_string;
  shaderType_type : identifier;
  maxLength_val  : unbounded_string;
  maxLength_type : identifier;
  length_val  : unbounded_string;
  length_type : identifier;
  debugLog_val  : unbounded_string;
--  debugLog_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetdebuglogmesa_t );
  -- TODO: GLhandleARB not defined
  -- ParseFirstNumericParameter( obj_val, obj_type, pen_glhandlearb_t ); -- obj : GLhandleARB
  ParseNextNumericParameter( logType_val, logType_type, pen_glenum_t ); -- logType : GLenum
  ParseNextNumericParameter( shaderType_val, shaderType_type, pen_glenum_t ); -- shaderType : GLenum
  ParseNextNumericParameter( maxLength_val, maxLength_type, pen_glsizei_t ); -- maxLength : GLsizei
  ParseNextNumericParameter( length_val, length_type, pen_glsizei_t ); -- length : GLsizei
  -- TODO: GLhandleARB not defined
  -- ParseLastNumericParameter( debugLog_val, debugLog_type, pen_glchararb_t ); -- debugLog : GLcharARB
  if isExecutingCommand then
    begin
-- TODO: MESA specific
      null; -- glGetDebugLogMESA( GLhandleARB( to_numeric( obj_val ) ), GLenum( to_numeric( logType_val ) ), GLenum( to_numeric( shaderType_val ) ), GLsizei( to_numeric( maxLength_val ) ), GLsizei( to_numeric( length_val ) ), GLcharARB( to_numeric( debugLog_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetDebugLogMESA;

procedure ParsePenglGetDebugLogLengthMESA is
--procedure ParsePenglGetDebugLogLengthMESA( result : out unbounded_string ) is
  -- Syntax: --glGetDebugLogLengthMESA ( obj : GLhandleARB; logType : GLenum; shaderType : GLenum ) return GLsizei;
  -- Source: bush_os.opengl.glGetDebugLogLengthMESA
  obj_val  : unbounded_string;
--  obj_type : identifier;
  logType_val  : unbounded_string;
  logType_type : identifier;
  shaderType_val  : unbounded_string;
  shaderType_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetdebugloglengthmesa_t );
  -- TODO: GLhandleARB not defined
  -- ParseFirstNumericParameter( obj_val, obj_type, pen_glhandlearb_t ); -- obj : GLhandleARB
  ParseNextNumericParameter( logType_val, logType_type, pen_glenum_t ); -- logType : GLenum
  ParseLastNumericParameter( shaderType_val, shaderType_type, pen_glenum_t ); -- shaderType : GLenum
  if isExecutingCommand then
    begin
-- TODO: MESA specific
      null; -- result := to_unbounded_string( long_float( glGetDebugLogLengthMESA( GLhandleARB( to_numeric( obj_val ) ), GLenum( to_numeric( logType_val ) ), GLenum( to_numeric( shaderType_val ) ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetDebugLogLengthMESA;

procedure ParsePenglProgramCallbackMESA is
  -- Syntax: --glProgramCallbackMESA( target : GLenum; callback : GLprogramcallbackMESA; data : System.address );
  -- Source: bush_os.opengl.glProgramCallbackMESA
  target_val  : unbounded_string;
  target_type : identifier;
  callback_val  : unbounded_string;
  callback_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glprogramcallbackmesa_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( callback_val, callback_type, pen_glprogramcallbackmesa_t ); -- callback : GLprogramcallbackMESA
  -- TODO: system.address not defined
  -- ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    begin
-- TODO: MESA specific
      null; -- glProgramCallbackMESA( GLenum( to_numeric( target_val ) ), GLprogramcallbackMESA( to_numeric( callback_val ) ), System.address( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglProgramCallbackMESA;

procedure ParsePenglGetProgramRegisterfvMESA is
  -- Syntax: --glGetProgramRegisterfvMESA( target : GLenum; len : GLsizei; name : in out GLubyte; v : in out GLfloat );
  -- Source: bush_os.opengl.glGetProgramRegisterfvMESA
  target_val  : unbounded_string;
  target_type : identifier;
  len_val  : unbounded_string;
  len_type : identifier;
  name_val  : unbounded_string;
  name_type : identifier;
  v_val  : unbounded_string;
  v_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glgetprogramregisterfvmesa_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( len_val, len_type, pen_glsizei_t ); -- len : GLsizei
  ParseNextNumericParameter( name_val, name_type, pen_glubyte_t ); -- name : GLubyte
  ParseLastNumericParameter( v_val, v_type, pen_glfloat_t ); -- v : GLfloat
  if isExecutingCommand then
    begin
-- TODO: MESA specific
      null; -- glGetProgramRegisterfvMESA( GLenum( to_numeric( target_val ) ), GLsizei( to_numeric( len_val ) ), GLubyte( to_numeric( name_val ) ), GLfloat( to_numeric( v_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglGetProgramRegisterfvMESA;

procedure ParsePenglFramebufferTextureLayerEXT is
  -- Syntax: --glFramebufferTextureLayerEXT( target : GLenum; attachment : GLenum; texture : GLuint; level : GLint; layer : GLint );
  -- Source: bush_os.opengl.glFramebufferTextureLayerEXT
  target_val  : unbounded_string;
  target_type : identifier;
  attachment_val  : unbounded_string;
  attachment_type : identifier;
  texture_val  : unbounded_string;
  texture_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  layer_val  : unbounded_string;
  layer_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glframebuffertexturelayerext_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( attachment_val, attachment_type, pen_glenum_t ); -- attachment : GLenum
  ParseNextNumericParameter( texture_val, texture_type, pen_gluint_t ); -- texture : GLuint
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseLastNumericParameter( layer_val, layer_type, pen_glint_t ); -- layer : GLint
  if isExecutingCommand then
    begin
-- TODO: MESA specific
      null; -- glFramebufferTextureLayerEXT( GLenum( to_numeric( target_val ) ), GLenum( to_numeric( attachment_val ) ), GLuint( to_numeric( texture_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( layer_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglFramebufferTextureLayerEXT;

procedure ParsePenglBlendEquationSeparateATI is
  -- Syntax: --glBlendEquationSeparateATI( modeRGB, modeA : GLenum );
  -- Source: bush_os.opengl.glBlendEquationSeparateATI
  modeRGB_val  : unbounded_string;
  modeRGB_type : identifier;
  modeA_val  : unbounded_string;
  modeA_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glblendequationseparateati_t );
  ParseFirstNumericParameter( modeRGB_val, modeRGB_type, pen_glenum_t ); -- modeRGB : GLenum
  ParseLastNumericParameter( modeA_val, modeA_type, pen_glenum_t ); -- modeA : GLenum
  if isExecutingCommand then
    begin
-- TODO: MESA specific
      null; -- glBlendEquationSeparateATI( GLenum( to_numeric( modeRGB_val ) ), GLenum( to_numeric( modeA_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglBlendEquationSeparateATI;

procedure ParsePenglEGLImageTargetTexture2DOES is
  -- Syntax: --glEGLImageTargetTexture2DOES ( target : GLenum; image : GLeglImageOES );
  -- Source: bush_os.opengl.glEGLImageTargetTexture2DOES
  target_val  : unbounded_string;
  target_type : identifier;
  image_val  : unbounded_string;
--  image_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gleglimagetargettexture2does_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  -- TODO: gleglimagesoes not defined
  -- ParseLastNumericParameter( image_val, image_type, pen_gleglimageoes_t ); -- image : GLeglImageOES
  if isExecutingCommand then
    begin
-- TODO: MESA specific
      null; -- glEGLImageTargetTexture2DOES( GLenum( to_numeric( target_val ) ), GLeglImageOES( to_numeric( image_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEGLImageTargetTexture2DOES;

procedure ParsePenglEGLImageTargetRenderbufferStorageOES is
  -- Syntax: --glEGLImageTargetRenderbufferStorageOES (target : GLenum; image : GLeglImageOES );
  -- Source: bush_os.opengl.glEGLImageTargetRenderbufferStorageOES
  target_val  : unbounded_string;
  target_type : identifier;
  image_val  : unbounded_string;
--  image_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gleglimagetargetrenderbufferstorageoes_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  -- TODO: gleglimagesoes not defined
  -- ParseLastNumericParameter( image_val, image_type, pen_gleglimageoes_t ); -- image : GLeglImageOES
  if isExecutingCommand then
    begin
-- TODO: MESA specific
      null; -- glEGLImageTargetRenderbufferStorageOES( GLenum( to_numeric( target_val ) ), GLeglImageOES( to_numeric( image_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePenglEGLImageTargetRenderbufferStorageOES;

procedure ParsePengluBeginCurve is
  -- Syntax: gluBeginCurve ( nurb : GLUnurbs_Ptr );
  -- Source: bush_os.opengl.gluBeginCurve
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubegincurve_t );
  -- TODO: we don't have a NURBS object yet in SparForte
  -- ParseSingleNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  if isExecutingCommand then
    begin
      null; -- gluBeginCurve( GLUnurbs_Ptr( to_numeric( nurb_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBeginCurve;

procedure ParsePengluBeginPolygon is
  -- Syntax: gluBeginPolygon ( tess : GLUtessellator_Ptr);
  -- Source: bush_os.opengl.gluBeginPolygon
  tess_val  : unbounded_string;
--  tess_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubeginpolygon_t );
  -- TODO: we don't have a  tessellation yet in SparForte
  --ParseSingleNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  if isExecutingCommand then
    begin
     null; -- gluBeginPolygon( GLUtessellator_Ptr( to_numeric( tess_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBeginPolygon;

procedure ParsePengluBeginSurface is
  -- Syntax: gluBeginSurface ( nurb : GLUnurbs_Ptr );
  -- Source: bush_os.opengl.gluBeginSurface
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubeginsurface_t );
  -- TODO: we don't have a NURBS object yet in SparForte
  --ParseSingleNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  if isExecutingCommand then
    begin
      null; -- gluBeginSurface( GLUnurbs_Ptr( to_numeric( nurb_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBeginSurface;

procedure ParsePengluBeginTrim is
  -- Syntax: gluBeginTrim ( nurb : GLUnurbs_Ptr);
  -- Source: bush_os.opengl.gluBeginTrim
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubegintrim_t );
  -- TODO: we don't have a NURBS object yet in SparForte
  --ParseSingleNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  if isExecutingCommand then
    begin
      null; -- gluBeginTrim( GLUnurbs_Ptr( to_numeric( nurb_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBeginTrim;

procedure ParsePengluBuild1DMipmapLevels( result : out unbounded_string ) is
  -- Syntax: gluBuild1DMipmapLevels ( target : GLenum; internalFormat : GLbuffers; width : GLsizei; format : GLbuffers; kind : GLtypes; level, base, max : GLint; data : System.address) return GLint;
  -- Source: bush_os.opengl.gluBuild1DMipmapLevels
  target_val  : unbounded_string;
  target_type : identifier;
  internalFormat_val  : unbounded_string;
  internalFormat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  base_val  : unbounded_string;
  base_type : identifier;
  max_val  : unbounded_string;
  max_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubuild1dmipmaplevels_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( internalFormat_val, internalFormat_type, pen_glbuffers_t ); -- internalFormat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( base_val, base_type, pen_glint_t ); -- base : GLint
  ParseNextNumericParameter( max_val, max_type, pen_glint_t ); -- max : GLint
  -- TODO: no man page?
  -- ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      -- TODO: no man page?
      result := to_unbounded_string( long_float( gluBuild1DMipmapLevels( GLenum( to_numeric( target_val ) ), GLbuffers( to_numeric( internalFormat_val ) ), GLsizei( to_numeric( width_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( base_val ) ), GLint( to_numeric( max_val ) ), dummy_ptr ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBuild1DMipmapLevels;

procedure ParsePengluBuild1DMipmaps( result : out unbounded_string ) is
  -- Syntax: gluBuild1DMipmaps ( target : GLenum; internalFormat : GLbuffers; width : GLsizei; format : GLbuffers; kind : GLtypes; data : System.address) return GLint;
  -- Source: bush_os.opengl.gluBuild1DMipmaps
  target_val  : unbounded_string;
  target_type : identifier;
  internalFormat_val  : unbounded_string;
  internalFormat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubuild1dmipmaps_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( internalFormat_val, internalFormat_type, pen_glbuffers_t ); -- internalFormat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: the map is multiple values in different possible types
  -- ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      result := to_unbounded_string( long_float( gluBuild1DMipmaps( GLenum( to_numeric( target_val ) ), GLbuffers( to_numeric( internalFormat_val ) ), GLsizei( to_numeric( width_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBuild1DMipmaps;

procedure ParsePengluBuild2DMipmapLevels( result : out unbounded_string ) is
  -- Syntax: gluBuild2DMipmapLevels ( target : GLenum; internalFormat : GLbuffers; width, height : GLsizei; format : GLbuffers; kind : GLtypes; level, base, max : GLint; data : System.address) return GLint;
  -- Source: bush_os.opengl.gluBuild2DMipmapLevels
  target_val  : unbounded_string;
  target_type : identifier;
  internalFormat_val  : unbounded_string;
  internalFormat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  base_val  : unbounded_string;
  base_type : identifier;
  max_val  : unbounded_string;
  max_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubuild2dmipmaplevels_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( internalFormat_val, internalFormat_type, pen_glbuffers_t ); -- internalFormat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( base_val, base_type, pen_glint_t ); -- base : GLint
  ParseNextNumericParameter( max_val, max_type, pen_glint_t ); -- max : GLint
  -- TODO: the map is multiple values in different possible types
  -- ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      result := to_unbounded_string( long_float( gluBuild2DMipmapLevels( GLenum( to_numeric( target_val ) ), GLbuffers( to_numeric( internalFormat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( base_val ) ), GLint( to_numeric( max_val ) ), dummy_ptr ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBuild2DMipmapLevels;

procedure ParsePengluBuild2DMipmaps( result : out unbounded_string ) is
  -- Syntax: gluBuild2DMipmaps ( target : GLenum; internalFormat : GLbuffers; width, height : GLsizei; format : GLbuffers; kind : GLtypes; data : System.address) return GLint;
  -- Source: bush_os.opengl.gluBuild2DMipmaps
  target_val  : unbounded_string;
  target_type : identifier;
  internalFormat_val  : unbounded_string;
  internalFormat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubuild2dmipmaps_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( internalFormat_val, internalFormat_type, pen_glbuffers_t ); -- internalFormat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: the map is multiple values in different possible types
  -- ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      result := to_unbounded_string( long_float( gluBuild2DMipmaps( GLenum( to_numeric( target_val ) ), GLbuffers( to_numeric( internalFormat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBuild2DMipmaps;

procedure ParsePengluBuild3DMipmapLevels( result : out unbounded_string ) is
  -- Syntax: gluBuild3DMipmapLevels ( taraget : GLenum; internalFormat : GLbuffers; width, height, depth : GLsizei; format : GLbuffers; kind : GLtypes; level, base, max : GLint; data : System.address) return GLint;
  -- Source: bush_os.opengl.gluBuild3DMipmapLevels
  taraget_val  : unbounded_string;
  taraget_type : identifier;
  internalFormat_val  : unbounded_string;
  internalFormat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  depth_val  : unbounded_string;
  depth_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  level_val  : unbounded_string;
  level_type : identifier;
  base_val  : unbounded_string;
  base_type : identifier;
  max_val  : unbounded_string;
  max_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubuild3dmipmaplevels_t );
  ParseFirstNumericParameter( taraget_val, taraget_type, pen_glenum_t ); -- taraget : GLenum
  ParseNextNumericParameter( internalFormat_val, internalFormat_type, pen_glbuffers_t ); -- internalFormat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( depth_val, depth_type, pen_glsizei_t ); -- depth : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  ParseNextNumericParameter( level_val, level_type, pen_glint_t ); -- level : GLint
  ParseNextNumericParameter( base_val, base_type, pen_glint_t ); -- base : GLint
  ParseNextNumericParameter( max_val, max_type, pen_glint_t ); -- max : GLint
  -- TODO: the map is multiple values in different possible types
  --ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      result := to_unbounded_string( long_float( gluBuild3DMipmapLevels( GLenum( to_numeric( taraget_val ) ), GLbuffers( to_numeric( internalFormat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLsizei( to_numeric( depth_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), GLint( to_numeric( level_val ) ), GLint( to_numeric( base_val ) ), GLint( to_numeric( max_val ) ), dummy_ptr ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBuild3DMipmapLevels;

procedure ParsePengluBuild3DMipmaps( result : out unbounded_string ) is
  -- Syntax: gluBuild3DMipmaps ( target : GLenum; internalFormat : GLbuffers; width, height, depth : GLsizei; format : GLbuffers; kind : GLtypes; data : System.address ) return GLint;
  -- Source: bush_os.opengl.gluBuild3DMipmaps
  target_val  : unbounded_string;
  target_type : identifier;
  internalFormat_val  : unbounded_string;
  internalFormat_type : identifier;
  width_val  : unbounded_string;
  width_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  depth_val  : unbounded_string;
  depth_type : identifier;
  format_val  : unbounded_string;
  format_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glubuild3dmipmaps_t );
  ParseFirstNumericParameter( target_val, target_type, pen_glenum_t ); -- target : GLenum
  ParseNextNumericParameter( internalFormat_val, internalFormat_type, pen_glbuffers_t ); -- internalFormat : GLbuffers
  ParseNextNumericParameter( width_val, width_type, pen_glsizei_t ); -- width : GLsizei
  ParseNextNumericParameter( height_val, height_type, pen_glsizei_t ); -- height : GLsizei
  ParseNextNumericParameter( depth_val, depth_type, pen_glsizei_t ); -- depth : GLsizei
  ParseNextNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( kind_val, kind_type, pen_gltypes_t ); -- kind : GLtypes
  -- TODO: the map is multiple values in different possible types
  -- ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
    begin
      result := to_unbounded_string( long_float( gluBuild3DMipmaps( GLenum( to_numeric( target_val ) ), GLbuffers( to_numeric( internalFormat_val ) ), GLsizei( to_numeric( width_val ) ), GLsizei( to_numeric( height_val ) ), GLsizei( to_numeric( depth_val ) ), GLbuffers( to_numeric( format_val ) ), GLtypes( to_numeric( kind_val ) ), dummy_ptr ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluBuild3DMipmaps;

procedure ParsePengluCheckExtension  is
--procedure ParsePengluCheckExtension( result : out unbounded_string ) is
  -- Syntax: gluCheckExtension ( extName, extString : char_array) return GLboolean;
  -- Source: bush_os.opengl.gluCheckExtension
  extName_val  : unbounded_string;
--  extName_type : identifier;
  extString_val  : unbounded_string;
--  extString_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glucheckextension_t );
  -- TODO: no man page?
  -- ParseFirstNumericParameter( extName_val, extName_type, pen_char_array_t ); -- extName : char_array
  -- ParseLastNumericParameter( extString_val, extString_type, pen_char_array_t ); -- extString : char_array
  if isExecutingCommand then
    begin
      null; -- result := to_unbounded_string( long_float( gluCheckExtension( char_array( to_numeric( extName_val ) ), char_array( to_numeric( extString_val ) ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluCheckExtension;

procedure ParsePengluCylinder is
  -- Syntax: gluCylinder (quad : GLUquadratic_Ptr; base, top, height : GLdouble; slices : GLint; stacks : GLint);
  -- Source: bush_os.opengl.gluCylinder
  quad_val  : unbounded_string;
--  quad_type : identifier;
  base_val  : unbounded_string;
  base_type : identifier;
  top_val  : unbounded_string;
  top_type : identifier;
  height_val  : unbounded_string;
  height_type : identifier;
  slices_val  : unbounded_string;
  slices_type : identifier;
  stacks_val  : unbounded_string;
  stacks_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glucylinder_t );
  -- TODO: no gluCylinder in Sparforte
  --ParseFirstNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  ParseNextNumericParameter( base_val, base_type, pen_gldouble_t ); -- base : GLdouble
  ParseNextNumericParameter( top_val, top_type, pen_gldouble_t ); -- top : GLdouble
  ParseNextNumericParameter( height_val, height_type, pen_gldouble_t ); -- height : GLdouble
  ParseNextNumericParameter( slices_val, slices_type, pen_glint_t ); -- slices : GLint
  ParseLastNumericParameter( stacks_val, stacks_type, pen_glint_t ); -- stacks : GLint
  if isExecutingCommand then
    begin
      null; -- gluCylinder( GLUquadratic_Ptr( to_numeric( quad_val ) ), GLdouble( to_numeric( base_val ) ), GLdouble( to_numeric( top_val ) ), GLdouble( to_numeric( height_val ) ), GLint( to_numeric( slices_val ) ), GLint( to_numeric( stacks_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluCylinder;

procedure ParsePengluDeleteNurbsRenderer is
  -- Syntax: gluDeleteNurbsRenderer ( nurb : GLUnurbs_Ptr );
  -- Source: bush_os.opengl.gluDeleteNurbsRenderer
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gludeletenurbsrenderer_t );
  -- TODO: no NURBS object in SparForte yet
  -- ParseSingleNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  if isExecutingCommand then
    begin
      null; -- gluDeleteNurbsRenderer( GLUnurbs_Ptr( to_numeric( nurb_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluDeleteNurbsRenderer;

procedure ParsePengluDeleteQuadric is
  -- Syntax: gluDeleteQuadric ( quad : GLUquadratic_Ptr );
  -- Source: bush_os.opengl.gluDeleteQuadric
  quad_val  : unbounded_string;
--  quad_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gludeletequadric_t );
  -- TODO: no quadratic object in SparForte yet
  -- ParseSingleNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  if isExecutingCommand then
    begin
      null; -- gluDeleteQuadric( GLUquadratic_Ptr( to_numeric( quad_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluDeleteQuadric;

procedure ParsePengluDeleteTess is
  -- Syntax: gluDeleteTess ( tess : GLUtessellator_Ptr );
  -- Source: bush_os.opengl.gluDeleteTess
  tess_val  : unbounded_string;
--  tess_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gludeletetess_t );
  -- TODO: no tessellation object in SparForte yet
  -- ParseSingleNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  if isExecutingCommand then
    begin
      null; -- gluDeleteTess( GLUtessellator_Ptr( to_numeric( tess_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluDeleteTess;

procedure ParsePengluDisk is
  -- Syntax: gluDisk ( quad : GLUquadratic_Ptr; inner, outer : GLdouble; slices : GLint; loops : GLint );
  -- Source: bush_os.opengl.gluDisk
  quad_val  : unbounded_string;
--  quad_type : identifier;
  inner_val  : unbounded_string;
  inner_type : identifier;
  outer_val  : unbounded_string;
  outer_type : identifier;
  slices_val  : unbounded_string;
  slices_type : identifier;
  loops_val  : unbounded_string;
  loops_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gludisk_t );
  -- TODO: no quadrics object yet in SparForte
  -- ParseFirstNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  ParseNextNumericParameter( inner_val, inner_type, pen_gldouble_t ); -- inner : GLdouble
  ParseNextNumericParameter( outer_val, outer_type, pen_gldouble_t ); -- outer : GLdouble
  ParseNextNumericParameter( slices_val, slices_type, pen_glint_t ); -- slices : GLint
  ParseLastNumericParameter( loops_val, loops_type, pen_glint_t ); -- loops : GLint
  if isExecutingCommand then
    begin
      null; -- gluDisk( GLUquadratic_Ptr( to_numeric( quad_val ) ), GLdouble( to_numeric( inner_val ) ), GLdouble( to_numeric( outer_val ) ), GLint( to_numeric( slices_val ) ), GLint( to_numeric( loops_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluDisk;

procedure ParsePengluEndCurve is
  -- Syntax: gluEndCurve ( nurb : GLUnurbs_Ptr );
  -- Source: bush_os.opengl.gluEndCurve
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluendcurve_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseSingleNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  if isExecutingCommand then
    begin
      null; -- gluEndCurve( GLUnurbs_Ptr( to_numeric( nurb_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluEndCurve;

procedure ParsePengluEndPolygon is
  -- Syntax: gluEndPolygon ( tess : GLUtessellator_Ptr );
  -- Source: bush_os.opengl.gluEndPolygon
  tess_val  : unbounded_string;
--  tess_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluendpolygon_t );
  -- TODO: no tessellation object yet in SparForte
  -- ParseSingleNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  if isExecutingCommand then
    begin
      null; -- gluEndPolygon( GLUtessellator_Ptr( to_numeric( tess_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluEndPolygon;

procedure ParsePengluEndSurface is
  -- Syntax: gluEndSurface ( nurb : GLUnurbs_Ptr );
  -- Source: bush_os.opengl.gluEndSurface
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluendsurface_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseSingleNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  if isExecutingCommand then
    begin
      null; -- gluEndSurface( GLUnurbs_Ptr( to_numeric( nurb_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluEndSurface;

procedure ParsePengluEndTrim is
  -- Syntax: gluEndTrim ( nurb : GLUnurbs_Ptr );
  -- Source: bush_os.opengl.gluEndTrim
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluendtrim_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseSingleNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  if isExecutingCommand then
    begin
      null; -- gluEndTrim( GLUnurbs_Ptr( to_numeric( nurb_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluEndTrim;

procedure ParsePengluErrorString is
--procedure ParsePengluErrorString( result : out unbounded_string ) is
  -- Syntax: gluErrorString ( error : GLenum ) return char_array_ptr;
  -- Source: bush_os.opengl.gluErrorString
  error_val  : unbounded_string;
  error_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluerrorstring_t );
  ParseSingleNumericParameter( error_val, error_type, pen_glenum_t ); -- error : GLenum
  if isExecutingCommand then
    begin
      -- TODO: char_array_ptr is an address to a C string that must be converted to an
      -- unbounded string.
      null; -- result := to_unbounded_string( gluErrorString( GLenum( to_numeric( error_val ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluErrorString;

procedure ParsePengluGetNurbsProperty is
  -- Syntax: gluGetNurbsProperty ( nurb : GLUnurbs_Ptr; property : GLUnurbsproperties; data : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.gluGetNurbsProperty
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
  property_val  : unbounded_string;
  property_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glugetnurbsproperty_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseFirstNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  ParseNextNumericParameter( property_val, property_type, pen_glunurbsproperties_t ); -- property : GLUnurbsproperties
 -- TODO: find out the size of returned data
 -- ParseLastNumericParameter( data_val, data_type, pen_gl_float_array_ptr_t ); -- data : GL_Float_Array_Ptr
  if isExecutingCommand then
    begin
      null; -- gluGetNurbsProperty( GLUnurbs_Ptr( to_numeric( nurb_val ) ), GLUnurbsproperties( to_numeric( property_val ) ), GL_Float_Array_Ptr( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluGetNurbsProperty;

procedure ParsePengluGetString is
--procedure ParsePengluGetString( result : out unbounded_string ) is
  -- Syntax: gluGetString ( name : GLenum ) return char_array_ptr;
  -- Source: bush_os.opengl.gluGetString
  name_val  : unbounded_string;
  name_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glugetstring_t );
  ParseSingleNumericParameter( name_val, name_type, pen_glenum_t ); -- name : GLenum
  if isExecutingCommand then
    begin
      -- TODO: char_array_ptr is a system.address but we need to convert the C array to an
      -- unbounded_string
      null; -- result := to_unbounded_string( long_float( gluGetString( GLenum( to_numeric( name_val ) ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluGetString;

procedure ParsePengluGetTessProperty is
  -- Syntax: gluGetTessProperty (tess : GLUtessellator_Ptr; which : GLUtessproperties; data : GL_Float_Array_Ptr );
  -- Source: bush_os.opengl.gluGetTessProperty
  tess_val  : unbounded_string;
--  tess_type : identifier;
  which_val  : unbounded_string;
  which_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glugettessproperty_t );
  -- TODO: no tessellation object yet for SparForte
  -- ParseFirstNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  ParseNextNumericParameter( which_val, which_type, pen_glutessproperties_t ); -- which : GLUtessproperties
  -- TODO: determine how many values may be returned
  -- ParseLastNumericParameter( data_val, data_type, pen_gl_float_array_ptr_t ); -- data : GL_Float_Array_Ptr
  if isExecutingCommand then
    begin
      null; -- gluGetTessProperty( GLUtessellator_Ptr( to_numeric( tess_val ) ), GLUtessproperties( to_numeric( which_val ) ), GL_Float_Array_Ptr( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluGetTessProperty;

procedure ParsePengluLoadSamplingMatrices is
  -- Syntax: gluLoadSamplingMatrices ( nurb : GLUnurbs_Ptr; model : GL_Float_Array_Ptr; perspective : GL_Float_Array_Ptr; view : GL_Int_Array_Ptr );
  -- Source: bush_os.opengl.gluLoadSamplingMatrices
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
  model_val  : unbounded_string;
--  model_type : identifier;
  perspective_val  : unbounded_string;
--  perspective_type : identifier;
  view_val  : unbounded_string;
--  view_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluloadsamplingmatrices_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseFirstNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  -- ParseNextNumericParameter( model_val, model_type, pen_gl_float_array_ptr_t ); -- model : GL_Float_Array_Ptr
  -- ParseNextNumericParameter( perspective_val, perspective_type, pen_gl_float_array_ptr_t ); -- perspective : GL_Float_Array_Ptr
  -- ParseLastNumericParameter( view_val, view_type, pen_gl_int_array_ptr_t ); -- view : GL_Int_Array_Ptr
  if isExecutingCommand then
    begin
      -- TODO: determine size of matrixes
      null; -- gluLoadSamplingMatrices( GLUnurbs_Ptr( to_numeric( nurb_val ) ), GL_Float_Array_Ptr( to_numeric( model_val ) ), GL_Float_Array_Ptr( to_numeric( perspective_val ) ), GL_Int_Array_Ptr( to_numeric( view_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluLoadSamplingMatrices;

procedure ParsePengluLookAt is
  -- Syntax: gluLookAt ( eyeX, eyeY, eyeZ, centerX, centerY, centerZ, upX, upY, upZ : GLdouble );
  -- Source: bush_os.opengl.gluLookAt
  eyeX_val  : unbounded_string;
  eyeX_type : identifier;
  eyeY_val  : unbounded_string;
  eyeY_type : identifier;
  eyeZ_val  : unbounded_string;
  eyeZ_type : identifier;
  centerX_val  : unbounded_string;
  centerX_type : identifier;
  centerY_val  : unbounded_string;
  centerY_type : identifier;
  centerZ_val  : unbounded_string;
  centerZ_type : identifier;
  upX_val  : unbounded_string;
  upX_type : identifier;
  upY_val  : unbounded_string;
  upY_type : identifier;
  upZ_val  : unbounded_string;
  upZ_type : identifier;
begin
  expect( pen_glulookat_t );
  ParseFirstNumericParameter( eyeX_val, eyeX_type, pen_gldouble_t ); -- eyeX : GLdouble
  ParseNextNumericParameter( eyeY_val, eyeY_type, pen_gldouble_t ); -- eyeY : GLdouble
  ParseNextNumericParameter( eyeZ_val, eyeZ_type, pen_gldouble_t ); -- eyeZ : GLdouble
  ParseNextNumericParameter( centerX_val, centerX_type, pen_gldouble_t ); -- centerX : GLdouble
  ParseNextNumericParameter( centerY_val, centerY_type, pen_gldouble_t ); -- centerY : GLdouble
  ParseNextNumericParameter( centerZ_val, centerZ_type, pen_gldouble_t ); -- centerZ : GLdouble
  ParseNextNumericParameter( upX_val, upX_type, pen_gldouble_t ); -- upX : GLdouble
  ParseNextNumericParameter( upY_val, upY_type, pen_gldouble_t ); -- upY : GLdouble
  ParseLastNumericParameter( upZ_val, upZ_type, pen_gldouble_t ); -- upZ : GLdouble
  if isExecutingCommand then
    begin
      gluLookAt( GLdouble( to_numeric( eyeX_val ) ), GLdouble( to_numeric( eyeY_val ) ), GLdouble( to_numeric( eyeZ_val ) ), GLdouble( to_numeric( centerX_val ) ), GLdouble( to_numeric( centerY_val ) ), GLdouble( to_numeric( centerZ_val ) ), GLdouble( to_numeric( upX_val ) ), GLdouble( to_numeric( upY_val ) ), GLdouble( to_numeric( upZ_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluLookAt;

procedure ParsePengluNewNurbsRenderer is
--procedure ParsePengluNewNurbsRenderer( result : out unbounded_string ) is
  -- Syntax: gluNewNurbsRenderer return GLUnurbs_Ptr;
  -- Source: bush_os.opengl.gluNewNurbsRenderer
begin
  err( "not yet complete" );
  expect( pen_glunewnurbsrenderer_t );
  if isExecutingCommand then
    begin
      -- TODO: returns a system.address but SparForte doesn't handle this yet
      null; -- result := to_unbounded_string( long_float( gluNewNurbsRenderer ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNewNurbsRenderer;

procedure ParsePengluNewQuadric is
--procedure ParsePengluNewQuadric( result : out unbounded_string ) is
  -- Syntax: gluNewQuadric return GLUquadratic_Ptr;
  -- Source: bush_os.opengl.gluNewQuadric
begin
  err( "not yet complete" );
  expect( pen_glunewquadric_t );
  if isExecutingCommand then
    begin
      -- TODO: returns a system.address but SparForte doesn't handle this yet
      null; -- result := to_unbounded_string( long_float( gluNewQuadric ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNewQuadric;

procedure ParsePengluNewTess is
--procedure ParsePengluNewTess( result : out unbounded_string ) is
  -- Syntax: gluNewTess return GLUtessellator_Ptr;
  -- Source: bush_os.opengl.gluNewTess
begin
  err( "not yet complete" );
  expect( pen_glunewtess_t );
  if isExecutingCommand then
    begin
      -- TODO: returns a system.address but SparForte doesn't handle this yet
      null; -- result := to_unbounded_string( long_float( gluNewTess ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNewTess;

procedure ParsePengluNextContour is
  -- Syntax: gluNextContour ( tess : GLUtessellator_Ptr; kind : GLUtesscontour );
  -- Source: bush_os.opengl.gluNextContour
  tess_val  : unbounded_string;
--  tess_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glunextcontour_t );
  -- TODO: no tessellation object in SparForte yet
  -- ParseFirstNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  ParseLastNumericParameter( kind_val, kind_type, pen_glutesscontour_t ); -- kind : GLUtesscontour
  if isExecutingCommand then
    begin
      null; -- gluNextContour( GLUtessellator_Ptr( to_numeric( tess_val ) ), GLUtesscontour( to_numeric( kind_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNextContour;

procedure ParsePengluNurbsCallback is
  -- Syntax: gluNurbsCallback ( nurb : GLUnurbs_Ptr; which : GLUnurbscallbacks; CallBackFunc : System.address );
  -- Source: bush_os.opengl.gluNurbsCallback
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
  which_val  : unbounded_string;
  which_type : identifier;
  CallBackFunc_val  : unbounded_string;
--  CallBackFunc_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glunurbscallback_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseFirstNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  ParseNextNumericParameter( which_val, which_type, pen_glunurbscallbacks_t ); -- which : GLUnurbscallbacks
  -- TODO: no callbacks yet in SparForte
  -- ParseLastNumericParameter( CallBackFunc_val, CallBackFunc_type, pen_system.address_t ); -- CallBackFunc : System.address
  if isExecutingCommand then
    begin
      null; -- gluNurbsCallback( GLUnurbs_Ptr( to_numeric( nurb_val ) ), GLUnurbscallbacks( to_numeric( which_val ) ), System.address( to_numeric( CallBackFunc_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNurbsCallback;

procedure ParsePengluNurbsCallbackData is
  -- Syntax: gluNurbsCallbackData ( nurb : GLUnurbs_Ptr; userData : System.address );
  -- Source: bush_os.opengl.gluNurbsCallbackData
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
  userData_val  : unbounded_string;
--  userData_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glunurbscallbackdata_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseFirstNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  -- TODO: no callbacks yet in SparForte
  -- ParseLastNumericParameter( userData_val, userData_type, pen_system.address_t ); -- userData : System.address
  if isExecutingCommand then
    begin
      null; -- gluNurbsCallbackData( GLUnurbs_Ptr( to_numeric( nurb_val ) ), System.address( to_numeric( userData_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNurbsCallbackData;

procedure ParsePengluNurbsCallbackDataEXT is
  -- Syntax: gluNurbsCallbackDataEXT ( nurb : GLUnurbs_Ptr; userData : System.address );
  -- Source: bush_os.opengl.gluNurbsCallbackDataEXT
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
  userData_val  : unbounded_string;
--  userData_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glunurbscallbackdataext_t );
  -- TODO: no NURBS object yet in SparForte
  --ParseFirstNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  -- TODO: no callbacks yet in SparForte
  -- ParseLastNumericParameter( userData_val, userData_type, pen_system.address_t ); -- userData : System.address
  if isExecutingCommand then
    begin
      null; -- gluNurbsCallbackDataEXT( GLUnurbs_Ptr( to_numeric( nurb_val ) ), System.address( to_numeric( userData_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNurbsCallbackDataEXT;

procedure ParsePengluNurbsCurve is
  -- Syntax: gluNurbsCurve ( nurb : GLUnurbs_Ptr; knotCount : GLint; knots : GL_Float_Array_Ptr; stride : GLint; control : GL_Float_Array_Ptr; order : GLint; kind : GLevaluators );
  -- Source: bush_os.opengl.gluNurbsCurve
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
  knotCount_val  : unbounded_string;
--  knotCount_type : identifier;
  knots_val  : unbounded_string;
--  knots_type : identifier;
  stride_val  : unbounded_string;
--  stride_type : identifier;
  control_val  : unbounded_string;
--  control_type : identifier;
  order_val  : unbounded_string;
  order_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glunurbscurve_t );
  -- TODO: no NURBS objecdt yet in SparFOrte
  -- ParseFirstNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  -- ParseNextNumericParameter( knotCount_val, knotCount_type, pen_glint_t ); -- knotCount : GLint
  -- ParseNextNumericParameter( knots_val, knots_type, pen_gl_float_array_ptr_t ); -- knots : GL_Float_Array_Ptr
  -- ParseNextNumericParameter( stride_val, stride_type, pen_glint_t ); -- stride : GLint
  -- ParseNextNumericParameter( control_val, control_type, pen_gl_float_array_ptr_t ); -- control : GL_Float_Array_Ptr
  ParseNextNumericParameter( order_val, order_type, pen_glint_t ); -- order : GLint
  ParseLastNumericParameter( kind_val, kind_type, pen_glevaluators_t ); -- kind : GLevaluators
  if isExecutingCommand then
    begin
      null; -- gluNurbsCurve( GLUnurbs_Ptr( to_numeric( nurb_val ) ), GLint( to_numeric( knotCount_val ) ), GL_Float_Array_Ptr( to_numeric( knots_val ) ), GLint( to_numeric( stride_val ) ), GL_Float_Array_Ptr( to_numeric( control_val ) ), GLint( to_numeric( order_val ) ), GLevaluators( to_numeric( kind_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNurbsCurve;

procedure ParsePengluNurbsProperty is
  -- Syntax: gluNurbsProperty ( nurb : GLUnurbs_Ptr; property : GLUnurbsproperties; value : GLfloat );
  -- Source: bush_os.opengl.gluNurbsProperty
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
  property_val  : unbounded_string;
  property_type : identifier;
  value_val  : unbounded_string;
  value_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glunurbsproperty_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseFirstNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  ParseNextNumericParameter( property_val, property_type, pen_glunurbsproperties_t ); -- property : GLUnurbsproperties
  ParseLastNumericParameter( value_val, value_type, pen_glfloat_t ); -- value : GLfloat
  if isExecutingCommand then
    begin
      null; -- gluNurbsProperty( GLUnurbs_Ptr( to_numeric( nurb_val ) ), GLUnurbsproperties( to_numeric( property_val ) ), GLfloat( to_numeric( value_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNurbsProperty;

procedure ParsePengluNurbsSurface is
  -- Syntax: gluNurbsSurface ( nurb : GLUnurbs_Ptr; sKnotCount : GLint; sKnots : GL_Float_Array_Ptr; tKnotCount : GLint; tKnots : GL_Float_Array_Ptr; sStride, tStride : GLint; control : GL_Float_Array_Ptr; sOrder, tOrder : GLint; kind : GLenum );
  -- Source: bush_os.opengl.gluNurbsSurface
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
  sKnotCount_val  : unbounded_string;
  sKnotCount_type : identifier;
  sKnots_val  : unbounded_string;
--  sKnots_type : identifier;
  tKnotCount_val  : unbounded_string;
  tKnotCount_type : identifier;
  tKnots_val  : unbounded_string;
--  tKnots_type : identifier;
  sStride_val  : unbounded_string;
  sStride_type : identifier;
  tStride_val  : unbounded_string;
  tStride_type : identifier;
  control_val  : unbounded_string;
--  control_type : identifier;
  sOrder_val  : unbounded_string;
  sOrder_type : identifier;
  tOrder_val  : unbounded_string;
  tOrder_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glunurbssurface_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseFirstNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  ParseNextNumericParameter( sKnotCount_val, sKnotCount_type, pen_glint_t ); -- sKnotCount : GLint
  --ParseNextNumericParameter( sKnots_val, sKnots_type, pen_gl_float_array_ptr_t ); -- sKnots : GL_Float_Array_Ptr
  ParseNextNumericParameter( tKnotCount_val, tKnotCount_type, pen_glint_t ); -- tKnotCount : GLint
  -- ParseNextNumericParameter( tKnots_val, tKnots_type, pen_gl_float_array_ptr_t ); -- tKnots : GL_Float_Array_Ptr
  ParseNextNumericParameter( sStride_val, sStride_type, pen_glint_t ); -- sStride : GLint
  ParseNextNumericParameter( tStride_val, tStride_type, pen_glint_t ); -- tStride : GLint
  -- ParseNextNumericParameter( control_val, control_type, pen_gl_float_array_ptr_t ); -- control : GL_Float_Array_Ptr
  ParseNextNumericParameter( sOrder_val, sOrder_type, pen_glint_t ); -- sOrder : GLint
  ParseNextNumericParameter( tOrder_val, tOrder_type, pen_glint_t ); -- tOrder : GLint
  ParseLastNumericParameter( kind_val, kind_type, pen_glenum_t ); -- kind : GLenum
  if isExecutingCommand then
    begin
      null; -- gluNurbsSurface( GLUnurbs_Ptr( to_numeric( nurb_val ) ), GLint( to_numeric( sKnotCount_val ) ), GL_Float_Array_Ptr( to_numeric( sKnots_val ) ), GLint( to_numeric( tKnotCount_val ) ), GL_Float_Array_Ptr( to_numeric( tKnots_val ) ), GLint( to_numeric( sStride_val ) ), GLint( to_numeric( tStride_val ) ), GL_Float_Array_Ptr( to_numeric( control_val ) ), GLint( to_numeric( sOrder_val ) ), GLint( to_numeric( tOrder_val ) ), GLenum( to_numeric( kind_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluNurbsSurface;

procedure ParsePengluOrtho2D is
  -- Syntax: gluOrtho2D ( left, right, bottom, top : GLdouble );
  -- Source: bush_os.opengl.gluOrtho2D
  left_val  : unbounded_string;
  left_type : identifier;
  right_val  : unbounded_string;
  right_type : identifier;
  bottom_val  : unbounded_string;
  bottom_type : identifier;
  top_val  : unbounded_string;
  top_type : identifier;
begin
  expect( pen_gluortho2d_t );
  ParseFirstNumericParameter( left_val, left_type, pen_gldouble_t ); -- left : GLdouble
  ParseNextNumericParameter( right_val, right_type, pen_gldouble_t ); -- right : GLdouble
  ParseNextNumericParameter( bottom_val, bottom_type, pen_gldouble_t ); -- bottom : GLdouble
  ParseLastNumericParameter( top_val, top_type, pen_gldouble_t ); -- top : GLdouble
  if isExecutingCommand then
    begin
      gluOrtho2D( GLdouble( to_numeric( left_val ) ), GLdouble( to_numeric( right_val ) ), GLdouble( to_numeric( bottom_val ) ), GLdouble( to_numeric( top_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluOrtho2D;

procedure ParsePengluPartialDisk is
  -- Syntax: gluPartialDisk ( quad : GLUquadratic_Ptr; inner, outer : GLdouble; slices : GLint; loops : GLint; start : GLdouble; sweep : GLdouble );
  -- Source: bush_os.opengl.gluPartialDisk
  quad_val  : unbounded_string;
--  quad_type : identifier;
  inner_val  : unbounded_string;
  inner_type : identifier;
  outer_val  : unbounded_string;
  outer_type : identifier;
  slices_val  : unbounded_string;
  slices_type : identifier;
  loops_val  : unbounded_string;
  loops_type : identifier;
  start_val  : unbounded_string;
  start_type : identifier;
  sweep_val  : unbounded_string;
  sweep_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glupartialdisk_t );
  -- TODO: no quadrics object yet in SparForte
  -- ParseFirstNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  ParseNextNumericParameter( inner_val, inner_type, pen_gldouble_t ); -- inner : GLdouble
  ParseNextNumericParameter( outer_val, outer_type, pen_gldouble_t ); -- outer : GLdouble
  ParseNextNumericParameter( slices_val, slices_type, pen_glint_t ); -- slices : GLint
  ParseNextNumericParameter( loops_val, loops_type, pen_glint_t ); -- loops : GLint
  ParseNextNumericParameter( start_val, start_type, pen_gldouble_t ); -- start : GLdouble
  ParseLastNumericParameter( sweep_val, sweep_type, pen_gldouble_t ); -- sweep : GLdouble
  if isExecutingCommand then
    begin
      null; -- gluPartialDisk( GLUquadratic_Ptr( to_numeric( quad_val ) ), GLdouble( to_numeric( inner_val ) ), GLdouble( to_numeric( outer_val ) ), GLint( to_numeric( slices_val ) ), GLint( to_numeric( loops_val ) ), GLdouble( to_numeric( start_val ) ), GLdouble( to_numeric( sweep_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluPartialDisk;

procedure ParsePengluPerspective is
  -- Syntax: gluPerspective ( fovy, aspect, zNear, zFar : GLdouble );
  -- Source: bush_os.opengl.gluPerspective
  fovy_val  : unbounded_string;
  fovy_type : identifier;
  aspect_val  : unbounded_string;
  aspect_type : identifier;
  zNear_val  : unbounded_string;
  zNear_type : identifier;
  zFar_val  : unbounded_string;
  zFar_type : identifier;
begin
  expect( pen_gluperspective_t );
  ParseFirstNumericParameter( fovy_val, fovy_type, pen_gldouble_t ); -- fovy : GLdouble
  ParseNextNumericParameter( aspect_val, aspect_type, pen_gldouble_t ); -- aspect : GLdouble
  ParseNextNumericParameter( zNear_val, zNear_type, pen_gldouble_t ); -- zNear : GLdouble
  ParseLastNumericParameter( zFar_val, zFar_type, pen_gldouble_t ); -- zFar : GLdouble
  if isExecutingCommand then
    begin
      gluPerspective( GLdouble( to_numeric( fovy_val ) ), GLdouble( to_numeric( aspect_val ) ), GLdouble( to_numeric( zNear_val ) ), GLdouble( to_numeric( zFar_val ) ) );
      if raiseGlExceptions then
         check_opengl_err;
      end if;
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluPerspective;

procedure ParsePengluPickMatrix( result : out unbounded_string ) is
  -- Syntax: gluPickMatrix ( x, y : GLdouble; delX, delY : GLdouble; viewport : in out GLint );
  -- Source: bush_os.opengl.gluPickMatrix
  x_val  : unbounded_string;
  x_type : identifier;
  y_val  : unbounded_string;
  y_type : identifier;
  delX_val  : unbounded_string;
  delX_type : identifier;
  delY_val  : unbounded_string;
  delY_type : identifier;
  viewport_val  : unbounded_string;
  viewport_type : identifier;
begin
  expect( pen_glupickmatrix_t );
  ParseFirstNumericParameter( x_val, x_type, pen_gldouble_t ); -- x : GLdouble
  ParseNextNumericParameter( y_val, y_type, pen_gldouble_t ); -- y : GLdouble
  ParseNextNumericParameter( delX_val, delX_type, pen_gldouble_t ); -- delX : GLdouble
  ParseNextNumericParameter( delY_val, delY_type, pen_gldouble_t ); -- delY : GLdouble
  ParseLastNumericParameter( viewport_val, viewport_type, pen_glint_t ); -- viewport : GLint
  if isExecutingCommand then
    declare
      param : GLint := 0;
    begin
      gluPickMatrix( GLdouble( to_numeric( x_val ) ), GLdouble( to_numeric( y_val ) ), GLdouble( to_numeric( delX_val ) ), GLdouble( to_numeric( delY_val ) ), param );
      result := to_unbounded_string( GLint'image( param ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluPickMatrix;

procedure ParsePengluProject is
--procedure ParsePengluProject( result : out unbounded_string ) is
  -- Syntax: gluProject ( objX, objY, objZ : GLdouble; model, proj : GL_Double_Array_Ptr; view : GL_Int_Array_Ptr; winX, winY, winZ : GL_Double_Array_Ptr ) return GLint;
  -- Source: bush_os.opengl.gluProject
  objX_val  : unbounded_string;
  objX_type : identifier;
  objY_val  : unbounded_string;
  objY_type : identifier;
  objZ_val  : unbounded_string;
  objZ_type : identifier;
  model_val  : unbounded_string;
--  model_type : identifier;
  proj_val  : unbounded_string;
--  proj_type : identifier;
  view_val  : unbounded_string;
--  view_type : identifier;
  winX_val  : unbounded_string;
--  winX_type : identifier;
  winY_val  : unbounded_string;
--  winY_type : identifier;
  winZ_val  : unbounded_string;
--  winZ_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluproject_t );
  ParseFirstNumericParameter( objX_val, objX_type, pen_gldouble_t ); -- objX : GLdouble
  ParseNextNumericParameter( objY_val, objY_type, pen_gldouble_t ); -- objY : GLdouble
  ParseNextNumericParameter( objZ_val, objZ_type, pen_gldouble_t ); -- objZ : GLdouble
  -- TODO: this is the model matrix: how bit?  4 doubles?? need to check.
  -- ParseNextNumericParameter( model_val, model_type, pen_gl_double_array_ptr_t ); -- model : GL_Double_Array_Ptr
  -- TODO: this is the projection matrix: how bit?  4 doubles?? need to check.
  -- ParseNextNumericParameter( proj_val, proj_type, pen_gl_double_array_ptr_t ); -- proj : GL_Double_Array_Ptr
  -- TODO: how big? need to check
  -- ParseNextNumericParameter( view_val, view_type, pen_gl_int_array_ptr_t ); -- view : GL_Int_Array_Ptr
  --ParseNextNumericParameter( winX_val, winX_type, pen_gl_double_array_ptr_t ); -- winX : GL_Double_Array_Ptr
  --ParseNextNumericParameter( winY_val, winY_type, pen_gl_double_array_ptr_t ); -- winY : GL_Double_Array_Ptr
  --ParseLastNumericParameter( winZ_val, winZ_type, pen_gl_double_array_ptr_t ); -- winZ : GL_Double_Array_Ptr
  if isExecutingCommand then
    declare
      -- TODO: determine the size
      model_array : GL_Double_Array_Access := new double_array( 0..0 );
      model_ptr   : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( model_array );
      proj_array  : GL_Double_Array_Access := new double_array( 0..0 );
      proj_ptr    : GL_Double_Array_Ptr := GL_Double_Array_Conv.To_Address( proj_array );
      view_array  : GL_Int_Array_Access := new int_array( 0..0 );
      view_ptr    : GL_Int_Array_Ptr := GL_Int_Array_Conv.To_Address( view_array );
      -- winX : GLdouble;
      -- winY : GLdouble;
      -- winZ : GLdouble;
    begin
      -- TODO: in SparForte, functions cannot return values
      null; -- result := to_unbounded_string( long_float( gluProject( GLdouble( to_numeric( objX_val ) ), GLdouble( to_numeric( objY_val ) ), GLdouble( to_numeric( objZ_val ) ), model_ptr, proj_ptr, view_ptr, winX, winY, winZ ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluProject;

procedure ParsePengluPwlCurve is
  -- Syntax: gluPwlCurve ( nurb : GLUnurbs_Ptr; count : GLint; data : GL_Float_Array_Ptr; stride : GLint; kind : GLevaluators );
  -- Source: bush_os.opengl.gluPwlCurve
  nurb_val  : unbounded_string;
--  nurb_type : identifier;
  count_val  : unbounded_string;
  count_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
  stride_val  : unbounded_string;
  stride_type : identifier;
  kind_val  : unbounded_string;
  kind_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glupwlcurve_t );
  -- TODO: no NURBS object yet in SparForte
  -- ParseFirstNumericParameter( nurb_val, nurb_type, pen_glunurbs_ptr_t ); -- nurb : GLUnurbs_Ptr
  ParseNextNumericParameter( count_val, count_type, pen_glint_t ); -- count : GLint
  --ParseNextNumericParameter( data_val, data_type, pen_gl_float_array_ptr_t ); -- data : GL_Float_Array_Ptr
  ParseNextNumericParameter( stride_val, stride_type, pen_glint_t ); -- stride : GLint
  ParseLastNumericParameter( kind_val, kind_type, pen_glevaluators_t ); -- kind : GLevaluators
  if isExecutingCommand then
    begin
      null;-- gluPwlCurve( GLUnurbs_Ptr( to_numeric( nurb_val ) ), GLint( to_numeric( count_val ) ), GL_Float_Array_Ptr( to_numeric( data_val ) ), GLint( to_numeric( stride_val ) ), GLevaluators( to_numeric( kind_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluPwlCurve;

procedure ParsePengluQuadricCallback is
  -- Syntax: gluQuadricCallback ( quad : GLUquadratic_Ptr; which : GLUnurbscallbacks; CallBackFunc : System.address );
  -- Source: bush_os.opengl.gluQuadricCallback
  quad_val  : unbounded_string;
--  quad_type : identifier;
  which_val  : unbounded_string;
  which_type : identifier;
  CallBackFunc_val  : unbounded_string;
--  CallBackFunc_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluquadriccallback_t );
  -- TODO: no quad object in SparForte
  --ParseFirstNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  ParseNextNumericParameter( which_val, which_type, pen_glunurbscallbacks_t ); -- which : GLUnurbscallbacks
  -- ParseLastNumericParameter( CallBackFunc_val, CallBackFunc_type, pen_system.address_t ); -- CallBackFunc : System.address
  if isExecutingCommand then
    begin
      -- TODO: no callbacks in SparForte
      null; -- gluQuadricCallback( GLUquadratic_Ptr( to_numeric( quad_val ) ), GLUnurbscallbacks( to_numeric( which_val ) ), System.address( to_numeric( CallBackFunc_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluQuadricCallback;

procedure ParsePengluQuadricDrawStyle is
  -- Syntax: gluQuadricDrawStyle ( quad : GLUquadratic_Ptr; draw : GLUquaddrawstyle );
  -- Source: bush_os.opengl.gluQuadricDrawStyle
  quad_val  : unbounded_string;
--  quad_type : identifier;
  draw_val  : unbounded_string;
  draw_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluquadricdrawstyle_t );
  -- TODO: no quad object in SparForte
  -- ParseFirstNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  ParseLastNumericParameter( draw_val, draw_type, pen_gluquaddrawstyle_t ); -- draw : GLUquaddrawstyle
  if isExecutingCommand then
    begin
      null; -- gluQuadricDrawStyle( GLUquadratic_Ptr( to_numeric( quad_val ) ), GLUquaddrawstyle( to_numeric( draw_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluQuadricDrawStyle;

procedure ParsePengluQuadricNormals is
  -- Syntax: gluQuadricNormals ( quad : GLUquadratic_Ptr; normal : GLUquadricnormal );
  -- Source: bush_os.opengl.gluQuadricNormals
  quad_val  : unbounded_string;
--  quad_type : identifier;
  normal_val  : unbounded_string;
  normal_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluquadricnormals_t );
  -- TODO: no quad object in SparForte yet
  -- ParseFirstNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  ParseLastNumericParameter( normal_val, normal_type, pen_gluquadricnormal_t ); -- normal : GLUquadricnormal
  if isExecutingCommand then
    begin
      null; -- gluQuadricNormals( GLUquadratic_Ptr( to_numeric( quad_val ) ), GLUquadricnormal( to_numeric( normal_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluQuadricNormals;

procedure ParsePengluQuadricOrientation is
  -- Syntax: gluQuadricOrientation ( quad : GLUquadratic_Ptr; orientation : GLUquadorientation );
  -- Source: bush_os.opengl.gluQuadricOrientation
  quad_val  : unbounded_string;
--  quad_type : identifier;
  orientation_val  : unbounded_string;
  orientation_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluquadricorientation_t );
  -- TODO: no quadrics object in SparForte yet
  -- ParseFirstNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  ParseLastNumericParameter( orientation_val, orientation_type, pen_gluquadorientation_t ); -- orientation : GLUquadorientation
  if isExecutingCommand then
    begin
      null; -- gluQuadricOrientation( GLUquadratic_Ptr( to_numeric( quad_val ) ), GLUquadorientation( to_numeric( orientation_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluQuadricOrientation;

procedure ParsePengluQuadricTexture is
  -- Syntax: gluQuadricTexture ( quad : GLUquadratic_Ptr; texture : GLboolean );
  -- Source: bush_os.opengl.gluQuadricTexture
  quad_val  : unbounded_string;
--  quad_type : identifier;
  texture_val  : unbounded_string;
  texture_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluquadrictexture_t );
  -- TODO: no quadrics object yet in SparForte
  -- ParseFirstNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  ParseLastNumericParameter( texture_val, texture_type, pen_glboolean_t ); -- texture : GLboolean
  if isExecutingCommand then
    begin
      null; -- gluQuadricTexture( GLUquadratic_Ptr( to_numeric( quad_val ) ), GLboolean( to_numeric( texture_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluQuadricTexture;

procedure ParsePengluScaleImage( result : out unbounded_string ) is
  -- Syntax: gluScaleImage ( format : GLbuffers; wIn, hIn : GLsizei; typeIn : GLtypes; dataIn : System.address; wOut, hOut : GLsizei; typeOut : GLtypes; dataOut : System.address ) return GLint;
  -- Source: bush_os.opengl.gluScaleImage
  format_val  : unbounded_string;
  format_type : identifier;
  wIn_val  : unbounded_string;
  wIn_type : identifier;
  hIn_val  : unbounded_string;
  hIn_type : identifier;
  typeIn_val  : unbounded_string;
  typeIn_type : identifier;
  dataIn_val  : unbounded_string;
--  dataIn_type : identifier;
  wOut_val  : unbounded_string;
  wOut_type : identifier;
  hOut_val  : unbounded_string;
  hOut_type : identifier;
  typeOut_val  : unbounded_string;
  typeOut_type : identifier;
  dataOut_val  : unbounded_string;
--  dataOut_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluscaleimage_t );
  ParseFirstNumericParameter( format_val, format_type, pen_glbuffers_t ); -- format : GLbuffers
  ParseNextNumericParameter( wIn_val, wIn_type, pen_glsizei_t ); -- wIn : GLsizei
  ParseNextNumericParameter( hIn_val, hIn_type, pen_glsizei_t ); -- hIn : GLsizei
  ParseNextNumericParameter( typeIn_val, typeIn_type, pen_gltypes_t ); -- typeIn : GLtypes
  -- TODO: system address means variable array
  -- ParseNextNumericParameter( dataIn_val, dataIn_type, pen_system.address_t ); -- dataIn : System.address
  ParseNextNumericParameter( wOut_val, wOut_type, pen_glsizei_t ); -- wOut : GLsizei
  ParseNextNumericParameter( hOut_val, hOut_type, pen_glsizei_t ); -- hOut : GLsizei
  ParseNextNumericParameter( typeOut_val, typeOut_type, pen_gltypes_t ); -- typeOut : GLtypes
  -- TODO: system address means variable array
  -- ParseLastNumericParameter( dataOut_val, dataOut_type, pen_system.address_t ); -- dataOut : System.address
  if isExecutingCommand then
    declare
      dummy : boolean;
      dummy_ptr : system.address := dummy'address;
      dummy2 : boolean;
      dummy2_ptr : system.address := dummy2'address;
    begin
      result := to_unbounded_string( long_float( gluScaleImage( GLbuffers( to_numeric( format_val ) ), GLsizei( to_numeric( wIn_val ) ), GLsizei( to_numeric( hIn_val ) ), GLtypes( to_numeric( typeIn_val ) ), dummy_ptr, GLsizei( to_numeric( wOut_val ) ), GLsizei( to_numeric( hOut_val ) ), GLtypes( to_numeric( typeOut_val ) ), dummy2_ptr ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluScaleImage;

procedure ParsePengluSphere is
  -- Syntax: gluSphere ( quad : GLUquadratic_Ptr; radius : GLdouble; slices : GLint; stacks : GLint );
  -- Source: bush_os.opengl.gluSphere
  quad_val  : unbounded_string;
--  quad_type : identifier;
  radius_val  : unbounded_string;
  radius_type : identifier;
  slices_val  : unbounded_string;
  slices_type : identifier;
  stacks_val  : unbounded_string;
  stacks_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glusphere_t );
  -- TODO: no quadrics object yet in SparForte
  --ParseFirstNumericParameter( quad_val, quad_type, pen_gluquadratic_ptr_t ); -- quad : GLUquadratic_Ptr
  ParseNextNumericParameter( radius_val, radius_type, pen_gldouble_t ); -- radius : GLdouble
  ParseNextNumericParameter( slices_val, slices_type, pen_glint_t ); -- slices : GLint
  ParseLastNumericParameter( stacks_val, stacks_type, pen_glint_t ); -- stacks : GLint
  if isExecutingCommand then
    begin
      null; -- gluSphere( GLUquadratic_Ptr( to_numeric( quad_val ) ), GLdouble( to_numeric( radius_val ) ), GLint( to_numeric( slices_val ) ), GLint( to_numeric( stacks_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluSphere;

procedure ParsePengluTessBeginContour is
  -- Syntax: gluTessBeginContour ( tess : GLUtessellator_Ptr );
  -- Source: bush_os.opengl.gluTessBeginContour
  tess_val  : unbounded_string;
--  tess_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glutessbegincontour_t );
  -- TODO: no tessellation object yet in SparForte
  -- ParseSingleNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  if isExecutingCommand then
    begin
      null; -- gluTessBeginContour( GLUtessellator_Ptr( to_numeric( tess_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluTessBeginContour;

procedure ParsePengluTessBeginPolygon is
  -- Syntax: gluTessBeginPolygon ( tess : GLUtessellator_Ptr; data : System.address );
  -- Source: bush_os.opengl.gluTessBeginPolygon
  tess_val  : unbounded_string;
--  tess_type : identifier;
  data_val  : unbounded_string;
--  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glutessbeginpolygon_t );
  -- TODO: no tessellation object yet in SparForte
  -- ParseFirstNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  -- ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    begin
      null;-- gluTessBeginPolygon( GLUtessellator_Ptr( to_numeric( tess_val ) ), System.address( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluTessBeginPolygon;

procedure ParsePengluTessCallback is
  -- Syntax: gluTessCallback (tess : GLUtessellator_Ptr; which : GLUtesscallbacks; CallBackFunc : System.address );
  -- Source: bush_os.opengl.gluTessCallback
  tess_val  : unbounded_string;
--  tess_type : identifier;
  which_val  : unbounded_string;
  which_type : identifier;
  CallBackFunc_val  : unbounded_string;
--  CallBackFunc_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glutesscallback_t );
  -- TODO: no tessellation object yet in SparForte
  -- ParseFirstNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  ParseNextNumericParameter( which_val, which_type, pen_glutesscallbacks_t ); -- which : GLUtesscallbacks
  -- ParseLastNumericParameter( CallBackFunc_val, CallBackFunc_type, pen_system.address_t ); -- CallBackFunc : System.address
  if isExecutingCommand then
    begin
      null; -- gluTessCallback( GLUtessellator_Ptr( to_numeric( tess_val ) ), GLUtesscallbacks( to_numeric( which_val ) ), System.address( to_numeric( CallBackFunc_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluTessCallback;

procedure ParsePengluTessEndContour is
  -- Syntax: gluTessEndContour ( tess : GLUtessellator_Ptr );
  -- Source: bush_os.opengl.gluTessEndContour
  tess_val  : unbounded_string;
--  tess_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glutessendcontour_t );
  -- TODO: no tessellation object yet in SparForte
  -- ParseSingleNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  if isExecutingCommand then
    begin
      null; -- gluTessEndContour( GLUtessellator_Ptr( to_numeric( tess_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluTessEndContour;

procedure ParsePengluTessEndPolygon is
  -- Syntax: gluTessEndPolygon ( tess : GLUtessellator_Ptr );
  -- Source: bush_os.opengl.gluTessEndPolygon
  tess_val  : unbounded_string;
--  tess_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glutessendpolygon_t );
  -- TODO: no tessellation object yet in SparForte
  -- ParseSingleNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  if isExecutingCommand then
    begin
      null; -- gluTessEndPolygon( GLUtessellator_Ptr( to_numeric( tess_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluTessEndPolygon;

procedure ParsePengluTessNormal is
  -- Syntax: gluTessNormal ( tess : GLUtessellator_Ptr; valueX, valueY, valueZ : GLdouble );
  -- Source: bush_os.opengl.gluTessNormal
  tess_val  : unbounded_string;
--  tess_type : identifier;
  valueX_val  : unbounded_string;
  valueX_type : identifier;
  valueY_val  : unbounded_string;
  valueY_type : identifier;
  valueZ_val  : unbounded_string;
  valueZ_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glutessnormal_t );
  -- TODO: no tessellation object yet in SparForte
  --ParseFirstNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  ParseNextNumericParameter( valueX_val, valueX_type, pen_gldouble_t ); -- valueX : GLdouble
  ParseNextNumericParameter( valueY_val, valueY_type, pen_gldouble_t ); -- valueY : GLdouble
  ParseLastNumericParameter( valueZ_val, valueZ_type, pen_gldouble_t ); -- valueZ : GLdouble
  if isExecutingCommand then
    begin
      null; -- gluTessNormal( GLUtessellator_Ptr( to_numeric( tess_val ) ), GLdouble( to_numeric( valueX_val ) ), GLdouble( to_numeric( valueY_val ) ), GLdouble( to_numeric( valueZ_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluTessNormal;

procedure ParsePengluTessProperty is
  -- Syntax: gluTessProperty ( tess : GLUtessellator_Ptr; which : GLUtessproperties; data : GLdouble );
  -- Source: bush_os.opengl.gluTessProperty
  tess_val  : unbounded_string;
  -- tess_type : identifier;
  which_val  : unbounded_string;
  which_type : identifier;
  data_val  : unbounded_string;
  data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glutessproperty_t );
  -- TODO: no tessellation object yet in SparForte
  --ParseFirstNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  ParseNextNumericParameter( which_val, which_type, pen_glutessproperties_t ); -- which : GLUtessproperties
  ParseLastNumericParameter( data_val, data_type, pen_gldouble_t ); -- data : GLdouble
  if isExecutingCommand then
    begin
      null; -- gluTessProperty( GLUtessellator_Ptr( to_numeric( tess_val ) ), GLUtessproperties( to_numeric( which_val ) ), GLdouble( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluTessProperty;

procedure ParsePengluTessVertex is
  -- Syntax: gluTessVertex ( tess : GLUtessellator_Ptr; location : in out GLdouble; data : System.address );
  -- Source: bush_os.opengl.gluTessVertex
  tess_val  : unbounded_string;
  -- tess_type : identifier;
  location_val  : unbounded_string;
  location_type : identifier;
  data_val  : unbounded_string;
  --data_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_glutessvertex_t );
  -- TODO: no tessellation object yet in SparForte
  -- ParseFirstNumericParameter( tess_val, tess_type, pen_glutessellator_ptr_t ); -- tess : GLUtessellator_Ptr
  ParseNextNumericParameter( location_val, location_type, pen_gldouble_t ); -- location : GLdouble
  -- ParseLastNumericParameter( data_val, data_type, pen_system.address_t ); -- data : System.address
  if isExecutingCommand then
    begin
      null; -- gluTessVertex( GLUtessellator_Ptr( to_numeric( tess_val ) ), GLdouble( to_numeric( location_val ) ), System.address( to_numeric( data_val ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluTessVertex;

procedure ParsePengluUnProject is
-- procedure ParsePengluUnProject( result : out unbounded_string ) is
  -- Syntax: gluUnProject ( winX, winY, winZ : GLdouble; model : GL_Double_Array_Ptr; proj : GL_Double_Array_Ptr; view : GL_Int_Array_Ptr; objX, objY, objZ : GL_Double_Array_Ptr) return GLint;
  -- Source: bush_os.opengl.gluUnProject
  winX_val  : unbounded_string;
  winX_type : identifier;
  winY_val  : unbounded_string;
  winY_type : identifier;
  winZ_val  : unbounded_string;
  winZ_type : identifier;
  model_val  : unbounded_string;
--  model_type : identifier;
  proj_val  : unbounded_string;
--  proj_type : identifier;
  view_val  : unbounded_string;
--  view_type : identifier;
  objX_val  : unbounded_string;
--  objX_type : identifier;
  objY_val  : unbounded_string;
--  objY_type : identifier;
  objZ_val  : unbounded_string;
--  objZ_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluunproject_t );
  ParseFirstNumericParameter( winX_val, winX_type, pen_gldouble_t ); -- winX : GLdouble
  ParseNextNumericParameter( winY_val, winY_type, pen_gldouble_t ); -- winY : GLdouble
  ParseNextNumericParameter( winZ_val, winZ_type, pen_gldouble_t ); -- winZ : GLdouble
  -- TODO: figure out sizes for these
  --ParseNextNumericParameter( model_val, model_type, pen_gl_double_array_ptr_t ); -- model : GL_Double_Array_Ptr
  --ParseNextNumericParameter( proj_val, proj_type, pen_gl_double_array_ptr_t ); -- proj : GL_Double_Array_Ptr
  --ParseNextNumericParameter( view_val, view_type, pen_gl_int_array_ptr_t ); -- view : GL_Int_Array_Ptr
  --ParseNextNumericParameter( objX_val, objX_type, pen_gl_double_array_ptr_t ); -- objX : GL_Double_Array_Ptr
  --ParseNextNumericParameter( objY_val, objY_type, pen_gl_double_array_ptr_t ); -- objY : GL_Double_Array_Ptr
  --ParseLastNumericParameter( objZ_val, objZ_type, pen_gl_double_array_ptr_t ); -- objZ : GL_Double_Array_Ptr
  if isExecutingCommand then
    begin
      null; -- result := to_unbounded_string( long_float( gluUnProject( GLdouble( to_numeric( winX_val ) ), GLdouble( to_numeric( winY_val ) ), GLdouble( to_numeric( winZ_val ) ), GL_Double_Array_Ptr( to_numeric( model_val ) ), GL_Double_Array_Ptr( to_numeric( proj_val ) ), GL_Int_Array_Ptr( to_numeric( view_val ) ), GL_Double_Array_Ptr( to_numeric( objX_val ) ), GL_Double_Array_Ptr( to_numeric( objY_val ) ), GL_Double_Array_Ptr( to_numeric( objZ_val ) ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluUnProject;

procedure ParsePengluUnProject4 is
-- procedure ParsePengluUnProject4( result : out unbounded_string ) is
  -- Syntax: gluUnProject4 ( winX, winY, winZ : GLdouble; clipW : GLdouble; model : GL_Double_Array_Ptr; proj : GL_Double_Array_Ptr; view : GL_Int_Array_Ptr; nearVal, farVal : GLdouble; objX, objY, objZ, objW : GL_Double_Array_Ptr ) return GLint;
  -- Source: bush_os.opengl.gluUnProject4
  winX_val  : unbounded_string;
  winX_type : identifier;
  winY_val  : unbounded_string;
  winY_type : identifier;
  winZ_val  : unbounded_string;
  winZ_type : identifier;
  clipW_val  : unbounded_string;
  clipW_type : identifier;
  model_val  : unbounded_string;
 -- model_type : identifier;
  proj_val  : unbounded_string;
  --proj_type : identifier;
  view_val  : unbounded_string;
  --view_type : identifier;
  nearVal_val  : unbounded_string;
  nearVal_type : identifier;
  farVal_val  : unbounded_string;
  farVal_type : identifier;
  objX_val  : unbounded_string;
  --objX_type : identifier;
  objY_val  : unbounded_string;
  --objY_type : identifier;
  objZ_val  : unbounded_string;
  --objZ_type : identifier;
  objW_val  : unbounded_string;
  --objW_type : identifier;
begin
  err( "not yet complete" );
  expect( pen_gluunproject4_t );
  ParseFirstNumericParameter( winX_val, winX_type, pen_gldouble_t ); -- winX : GLdouble
  ParseNextNumericParameter( winY_val, winY_type, pen_gldouble_t ); -- winY : GLdouble
  ParseNextNumericParameter( winZ_val, winZ_type, pen_gldouble_t ); -- winZ : GLdouble
  ParseNextNumericParameter( clipW_val, clipW_type, pen_gldouble_t ); -- clipW : GLdouble
  --ParseNextNumericParameter( model_val, model_type, pen_gl_double_array_ptr_t ); -- model : GL_Double_Array_Ptr
  --ParseNextNumericParameter( proj_val, proj_type, pen_gl_double_array_ptr_t ); -- proj : GL_Double_Array_Ptr
  --ParseNextNumericParameter( view_val, view_type, pen_gl_int_array_ptr_t ); -- view : GL_Int_Array_Ptr
  ParseNextNumericParameter( nearVal_val, nearVal_type, pen_gldouble_t ); -- nearVal : GLdouble
  ParseNextNumericParameter( farVal_val, farVal_type, pen_gldouble_t ); -- farVal : GLdouble
  --ParseNextNumericParameter( objX_val, objX_type, pen_gl_double_array_ptr_t ); -- objX : GL_Double_Array_Ptr
  --ParseNextNumericParameter( objY_val, objY_type, pen_gl_double_array_ptr_t ); -- objY : GL_Double_Array_Ptr
  --ParseNextNumericParameter( objZ_val, objZ_type, pen_gl_double_array_ptr_t ); -- objZ : GL_Double_Array_Ptr
  --ParseLastNumericParameter( objW_val, objW_type, pen_gl_double_array_ptr_t ); -- objW : GL_Double_Array_Ptr
  if isExecutingCommand then
    begin
      null; -- result := to_unbounded_string( long_float( gluUnProject4( GLdouble( to_numeric( winX_val ) ), GLdouble( to_numeric( winY_val ) ), GLdouble( to_numeric( winZ_val ) ), GLdouble( to_numeric( clipW_val ) ), GL_Double_Array_Ptr( to_numeric( model_val ) ), GL_Double_Array_Ptr( to_numeric( proj_val ) ), GL_Int_Array_Ptr( to_numeric( view_val ) ), GLdouble( to_numeric( nearVal_val ) ), GLdouble( to_numeric( farVal_val ) ), GL_Double_Array_Ptr( to_numeric( objX_val ) ), GL_Double_Array_Ptr( to_numeric( objY_val ) ), GL_Double_Array_Ptr( to_numeric( objZ_val ) ), GL_Double_Array_Ptr( to_numeric( objW_val ) ) ) ) );
    exception when others =>
      err( "exception raised" );
    end;
  end if;
end ParsePengluUnProject4;

--- C Resources

procedure ParsePenNewGLByteArray is
  -- Syntax: pen.new_gl_byte_array( a, n );
  -- Ada:    N/A
  resId    : resHandleId;
  ref      : reference;
  len_expr : unbounded_string;
  len_kind : identifier;
  len      : size_t;
begin
  expect( pen_new_gl_byte_array_t );
  ParseFirstOutParameter( ref, pen_gl_byte_array_t );
  baseTypesOK( ref.kind, pen_gl_byte_array_t );
  ParseLastNumericParameter( len_expr, len_kind, natural_t );
  if isExecutingCommand then
     identifiers( ref.id ).resource := true;
     len := size_t( to_numeric( len_expr ) );
     declareResource( resId, gl_byte_array, blocks_top, len );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParsePenNewGLByteArray;

procedure ParsePenSetGLByteArray is
  -- TODO: we could use an easier way to get data in and out
  -- pen.set_gl_byte_array( a, p, v );
  a_id        : identifier;
  pos_val     : unbounded_string;
  pos_type    : identifier;
  newval_val  : unbounded_string;
  newval_type : identifier;
  theArray    : resPtr;
  array_pos   : size_t;
  array_val   : GLbyte;
begin
  expect( pen_set_gl_byte_array_t );
  ParseFirstInOutParameter( a_id, pen_gl_byte_array_t );
  ParseNextNumericParameter( pos_val, pos_type, natural_t );
  ParseLastNumericParameter( newval_val, newval_type, pen_glbyte_t );
  if isExecutingCommand then
     array_pos := size_t( to_numeric( pos_val ) );
     array_val := GLbyte( to_numeric( newval_val ) );
     findResource( to_resource_id( identifiers( a_id ).value ), theArray );
     if array_pos not in theArray.gl_ba'range then
        err( "the array position" & to_string( pos_val ) & " is not in the range" & size_t'image( theArray.gl_ba'first ) & " .." & size_t'image( theArray.gl_ba'last ) );
     else
        theArray.gl_ba( array_pos ) := array_val;
     end if;
  end if;
  exception when others =>
    err( "constraint error" );
end ParsePenSetGLByteArray;

procedure ParsePenNewGLIntArray is
  -- Syntax: pen.new_gl_int_array( a, n );
  -- Ada:    N/A
  resId    : resHandleId;
  ref      : reference;
  len_expr : unbounded_string;
  len_kind : identifier;
  len      : size_t;
begin
  expect( pen_new_gl_int_array_t );
  ParseFirstOutParameter( ref, pen_gl_int_array_t );
  baseTypesOK( ref.kind, pen_gl_int_array_t );
  ParseLastNumericParameter( len_expr, len_kind, natural_t );
  if isExecutingCommand then
     identifiers( ref.id ).resource := true;
     len := size_t( to_numeric( len_expr ) );
     declareResource( resId, gl_int_array, blocks_top, len );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParsePenNewGLIntArray;

procedure ParsePenSetGLIntArray is
  -- TODO: we could use an easier way to get data in and out
  -- pen.set_gl_int_array( a, p, v );
  a_id        : identifier;
  pos_val     : unbounded_string;
  pos_type    : identifier;
  newval_val  : unbounded_string;
  newval_type : identifier;
  theArray    : resPtr;
  array_pos   : size_t;
  array_val   : GLint;
begin
  expect( pen_set_gl_int_array_t );
  ParseFirstInOutParameter( a_id, pen_gl_int_array_t );
  ParseNextNumericParameter( pos_val, pos_type, natural_t );
  ParseLastNumericParameter( newval_val, newval_type, pen_glint_t );
  if isExecutingCommand then
     array_pos := size_t( to_numeric( pos_val ) );
     array_val := GLint( to_numeric( newval_val ) );
     findResource( to_resource_id( identifiers( a_id ).value ), theArray );
     if array_pos not in theArray.gl_ia'range then
        err( "the array position" & to_string( pos_val ) & " is not in the range" & size_t'image( theArray.gl_ia'first ) & " .." & size_t'image( theArray.gl_ia'last ) );
     else
        theArray.gl_ia( array_pos ) := array_val;
     end if;
  end if;
  exception when others =>
    err( "constraint error" );
end ParsePenSetGLIntArray;

procedure ParsePenNewGLShortArray is
  -- Syntax: pen.new_gl_short_array( a, n );
  -- Ada:    N/A
  -- todo : should be pairs/triplets/quads
  resId    : resHandleId;
  ref      : reference;
  len_expr : unbounded_string;
  len_kind : identifier;
  len      : size_t;
begin
  expect( pen_new_gl_short_array_t );
  ParseFirstOutParameter( ref, pen_gl_short_array_t );
  baseTypesOK( ref.kind, pen_gl_short_array_t );
  ParseLastNumericParameter( len_expr, len_kind, natural_t );
  if isExecutingCommand then
     identifiers( ref.id ).resource := true;
     len := size_t( to_numeric( len_expr ) );
     declareResource( resId, gl_short_array, blocks_top, len );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParsePenNewGLShortArray;

procedure ParsePenSetGLShortArray is
  -- TODO: we could use an easier way to get data in and out
  -- pen.set_gl_short_array( a, p, v );
  a_id        : identifier;
  pos_val     : unbounded_string;
  pos_type    : identifier;
  newval_val  : unbounded_string;
  newval_type : identifier;
  theArray    : resPtr;
  array_pos   : size_t;
  array_val   : GLshort;
begin
  expect( pen_set_gl_short_array_t );
  ParseFirstInOutParameter( a_id, pen_gl_short_array_t );
  ParseNextNumericParameter( pos_val, pos_type, natural_t );
  ParseLastNumericParameter( newval_val, newval_type, pen_glshort_t );
  if isExecutingCommand then
     array_pos := size_t( to_numeric( pos_val ) );
     array_val := GLShort( to_numeric( newval_val ) );
     findResource( to_resource_id( identifiers( a_id ).value ), theArray );
     if array_pos not in theArray.gl_sa'range then
        err( "the array position" & to_string( pos_val ) & " is not in the range" & size_t'image( theArray.gl_sa'first ) & " .." & size_t'image( theArray.gl_sa'last ) );
     else
        theArray.gl_sa( array_pos ) := array_val;
     end if;
  end if;
  exception when others =>
    err( "constraint error" );
end ParsePenSetGLShortArray;

procedure ParsePenNewGLFloatArray is
  -- Syntax: pen.new_gl_float_array( a, n );
  -- Ada:    N/A
  resId    : resHandleId;
  ref      : reference;
  len_expr : unbounded_string;
  len_kind : identifier;
  len      : size_t;
begin
  expect( pen_new_gl_float_array_t );
  ParseFirstOutParameter( ref, pen_gl_float_array_t );
  baseTypesOK( ref.kind, pen_gl_float_array_t );
  ParseLastNumericParameter( len_expr, len_kind, natural_t );
  if isExecutingCommand then
     identifiers( ref.id ).resource := true;
     len := size_t( to_numeric( len_expr ) );
     declareResource( resId, gl_float_array, blocks_top, len );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParsePenNewGLFloatArray;

procedure ParsePenSetGLFloatArray is
  -- TODO: we could use an easier way to get data in and out
  -- pen.set_gl_int_array( a, p, v );
  a_id        : identifier;
  pos_val     : unbounded_string;
  pos_type    : identifier;
  newval_val  : unbounded_string;
  newval_type : identifier;
  theArray    : resPtr;
  array_pos   : size_t;
  array_val   : GLfloat;
begin
  expect( pen_set_gl_float_array_t );
  ParseFirstInOutParameter( a_id, pen_gl_float_array_t );
  ParseNextNumericParameter( pos_val, pos_type, natural_t );
  ParseLastNumericParameter( newval_val, newval_type, pen_glfloat_t );
  if isExecutingCommand then
     array_pos := size_t( to_numeric( pos_val ) );
     array_val := GLfloat( to_numeric( newval_val ) );
     findResource( to_resource_id( identifiers( a_id ).value ), theArray );
     if array_pos not in theArray.gl_fa'range then
        err( "the array position" & to_string( pos_val ) & " is not in the range" & size_t'image( theArray.gl_fa'first ) & " .." & size_t'image( theArray.gl_fa'last ) );
     else
        theArray.gl_fa( array_pos ) := array_val;
     end if;
  end if;
  exception when others =>
    err( "constraint error" );
end ParsePenSetGLFloatArray;

procedure ParsePenNewGLDoubleArray is
  -- Syntax: pen.new_gl_double_array( a, n );
  -- Ada:    N/A
  resId    : resHandleId;
  ref      : reference;
  len_expr : unbounded_string;
  len_kind : identifier;
  len      : size_t;
begin
  expect( pen_new_gl_double_array_t );
  ParseFirstOutParameter( ref, pen_gl_double_array_t );
  baseTypesOK( ref.kind, pen_gl_double_array_t );
  ParseLastNumericParameter( len_expr, len_kind, natural_t );
  if isExecutingCommand then
     identifiers( ref.id ).resource := true;
     len := size_t( to_numeric( len_expr ) );
     declareResource( resId, gl_double_array, blocks_top, len );
     AssignParameter( ref, to_unbounded_string( resId ) );
  end if;
end ParsePenNewGLDoubleArray;

procedure ParsePenSetGLDoubleArray is
  -- TODO: we could use an easier way to get data in and out
  -- pen.set_gl_double_array( a, p, v );
  a_id        : identifier;
  pos_val     : unbounded_string;
  pos_type    : identifier;
  newval_val  : unbounded_string;
  newval_type : identifier;
  theArray    : resPtr;
  array_pos   : size_t;
  array_val   : GLdouble;
begin
  expect( pen_set_gl_double_array_t );
  ParseFirstInOutParameter( a_id, pen_gl_double_array_t );
  ParseNextNumericParameter( pos_val, pos_type, natural_t );
  ParseLastNumericParameter( newval_val, newval_type, pen_gldouble_t );
  if isExecutingCommand then
     array_pos := size_t( to_numeric( pos_val ) );
     array_val := GLdouble( to_numeric( newval_val ) );
     findResource( to_resource_id( identifiers( a_id ).value ), theArray );
     if array_pos not in theArray.gl_da'range then
        err( "the array position" & to_string( pos_val ) & " is not in the range" & size_t'image( theArray.gl_da'first ) & " .." & size_t'image( theArray.gl_da'last ) );
     else
        theArray.gl_da( array_pos ) := array_val;
     end if;
  end if;
  exception when others =>
    err( "constraint error" );
end ParsePenSetGLDoubleArray;

-- These are split from StartupPen (see StartupPen)

procedure declarePenColorNames1 is
begin
declareStandardConstant( color_name_aliceblue_t, "pen_color_name.aliceblue", pen_pen_color_name_t, "0" );
declareStandardConstant( color_name_antiquewhite_t, "pen_color_name.antiquewhite", pen_pen_color_name_t, "1" );
declareStandardConstant( color_name_antiquewhite1_t, "pen_color_name.antiquewhite1", pen_pen_color_name_t, "2" );
declareStandardConstant( color_name_antiquewhite2_t, "pen_color_name.antiquewhite2", pen_pen_color_name_t, "3" );
declareStandardConstant( color_name_antiquewhite3_t, "pen_color_name.antiquewhite3", pen_pen_color_name_t, "4" );
declareStandardConstant( color_name_antiquewhite4_t, "pen_color_name.antiquewhite4", pen_pen_color_name_t, "5" );
declareStandardConstant( color_name_aquamarine_t, "pen_color_name.aquamarine", pen_pen_color_name_t, "6" );
declareStandardConstant( color_name_aquamarine1_t, "pen_color_name.aquamarine1", pen_pen_color_name_t, "7" );
declareStandardConstant( color_name_aquamarine2_t, "pen_color_name.aquamarine2", pen_pen_color_name_t, "8" );
declareStandardConstant( color_name_aquamarine3_t, "pen_color_name.aquamarine3", pen_pen_color_name_t, "9" );
declareStandardConstant( color_name_aquamarine4_t, "pen_color_name.aquamarine4", pen_pen_color_name_t, "10" );
declareStandardConstant( color_name_azure_t, "pen_color_name.azure", pen_pen_color_name_t, "11" );
declareStandardConstant( color_name_azure1_t, "pen_color_name.azure1", pen_pen_color_name_t, "12" );
declareStandardConstant( color_name_azure2_t, "pen_color_name.azure2", pen_pen_color_name_t, "13" );
declareStandardConstant( color_name_azure3_t, "pen_color_name.azure3", pen_pen_color_name_t, "14" );
declareStandardConstant( color_name_azure4_t, "pen_color_name.azure4", pen_pen_color_name_t, "15" );
declareStandardConstant( color_name_beige_t, "pen_color_name.beige", pen_pen_color_name_t, "16" );
declareStandardConstant( color_name_bisque_t, "pen_color_name.bisque", pen_pen_color_name_t, "17" );
declareStandardConstant( color_name_bisque1_t, "pen_color_name.bisque1", pen_pen_color_name_t, "18" );
declareStandardConstant( color_name_bisque2_t, "pen_color_name.bisque2", pen_pen_color_name_t, "19" );
declareStandardConstant( color_name_bisque3_t, "pen_color_name.bisque3", pen_pen_color_name_t, "20" );
declareStandardConstant( color_name_bisque4_t, "pen_color_name.bisque4", pen_pen_color_name_t, "21" );
declareStandardConstant( color_name_black_t, "pen_color_name.black", pen_pen_color_name_t, "22" );
declareStandardConstant( color_name_blanchedalmond_t, "pen_color_name.blanchedalmond", pen_pen_color_name_t, "23" );
declareStandardConstant( color_name_blue_t, "pen_color_name.blue", pen_pen_color_name_t, "24" );
declareStandardConstant( color_name_blue1_t, "pen_color_name.blue1", pen_pen_color_name_t, "25" );
declareStandardConstant( color_name_blue2_t, "pen_color_name.blue2", pen_pen_color_name_t, "26" );
declareStandardConstant( color_name_blue3_t, "pen_color_name.blue3", pen_pen_color_name_t, "27" );
declareStandardConstant( color_name_blue4_t, "pen_color_name.blue4", pen_pen_color_name_t, "28" );
declareStandardConstant( color_name_blueviolet_t, "pen_color_name.blueviolet", pen_pen_color_name_t, "29" );
declareStandardConstant( color_name_brown_t, "pen_color_name.brown", pen_pen_color_name_t, "30" );
declareStandardConstant( color_name_brown1_t, "pen_color_name.brown1", pen_pen_color_name_t, "31" );
declareStandardConstant( color_name_brown2_t, "pen_color_name.brown2", pen_pen_color_name_t, "32" );
declareStandardConstant( color_name_brown3_t, "pen_color_name.brown3", pen_pen_color_name_t, "33" );
declareStandardConstant( color_name_brown4_t, "pen_color_name.brown4", pen_pen_color_name_t, "34" );
declareStandardConstant( color_name_burlywood_t, "pen_color_name.burlywood", pen_pen_color_name_t, "35" );
declareStandardConstant( color_name_burlywood1_t, "pen_color_name.burlywood1", pen_pen_color_name_t, "36" );
declareStandardConstant( color_name_burlywood2_t, "pen_color_name.burlywood2", pen_pen_color_name_t, "37" );
declareStandardConstant( color_name_burlywood3_t, "pen_color_name.burlywood3", pen_pen_color_name_t, "38" );
declareStandardConstant( color_name_burlywood4_t, "pen_color_name.burlywood4", pen_pen_color_name_t, "39" );
declareStandardConstant( color_name_cadetblue_t, "pen_color_name.cadetblue", pen_pen_color_name_t, "40" );
declareStandardConstant( color_name_cadetblue1_t, "pen_color_name.cadetblue1", pen_pen_color_name_t, "41" );
declareStandardConstant( color_name_cadetblue2_t, "pen_color_name.cadetblue2", pen_pen_color_name_t, "42" );
declareStandardConstant( color_name_cadetblue3_t, "pen_color_name.cadetblue3", pen_pen_color_name_t, "43" );
declareStandardConstant( color_name_cadetblue4_t, "pen_color_name.cadetblue4", pen_pen_color_name_t, "44" );
declareStandardConstant( color_name_chartreuse_t, "pen_color_name.chartreuse", pen_pen_color_name_t, "45" );
declareStandardConstant( color_name_chartreuse1_t, "pen_color_name.chartreuse1", pen_pen_color_name_t, "46" );
declareStandardConstant( color_name_chartreuse2_t, "pen_color_name.chartreuse2", pen_pen_color_name_t, "47" );
declareStandardConstant( color_name_chartreuse3_t, "pen_color_name.chartreuse3", pen_pen_color_name_t, "48" );
declareStandardConstant( color_name_chartreuse4_t, "pen_color_name.chartreuse4", pen_pen_color_name_t, "49" );
declareStandardConstant( color_name_chocolate_t, "pen_color_name.chocolate", pen_pen_color_name_t, "50" );
declareStandardConstant( color_name_chocolate1_t, "pen_color_name.chocolate1", pen_pen_color_name_t, "51" );
declareStandardConstant( color_name_chocolate2_t, "pen_color_name.chocolate2", pen_pen_color_name_t, "52" );
declareStandardConstant( color_name_chocolate3_t, "pen_color_name.chocolate3", pen_pen_color_name_t, "53" );
declareStandardConstant( color_name_chocolate4_t, "pen_color_name.chocolate4", pen_pen_color_name_t, "54" );
declareStandardConstant( color_name_coral_t, "pen_color_name.coral", pen_pen_color_name_t, "55" );
declareStandardConstant( color_name_coral1_t, "pen_color_name.coral1", pen_pen_color_name_t, "56" );
declareStandardConstant( color_name_coral2_t, "pen_color_name.coral2", pen_pen_color_name_t, "57" );
declareStandardConstant( color_name_coral3_t, "pen_color_name.coral3", pen_pen_color_name_t, "58" );
declareStandardConstant( color_name_coral4_t, "pen_color_name.coral4", pen_pen_color_name_t, "59" );
declareStandardConstant( color_name_cornflowerblue_t, "pen_color_name.cornflowerblue", pen_pen_color_name_t, "60" );
declareStandardConstant( color_name_cornsilk_t, "pen_color_name.cornsilk", pen_pen_color_name_t, "61" );
declareStandardConstant( color_name_cornsilk1_t, "pen_color_name.cornsilk1", pen_pen_color_name_t, "62" );
declareStandardConstant( color_name_cornsilk2_t, "pen_color_name.cornsilk2", pen_pen_color_name_t, "63" );
declareStandardConstant( color_name_cornsilk3_t, "pen_color_name.cornsilk3", pen_pen_color_name_t, "64" );
declareStandardConstant( color_name_cornsilk4_t, "pen_color_name.cornsilk4", pen_pen_color_name_t, "65" );
declareStandardConstant( color_name_cyan_t, "pen_color_name.cyan", pen_pen_color_name_t, "66" );
declareStandardConstant( color_name_cyan1_t, "pen_color_name.cyan1", pen_pen_color_name_t, "67" );
declareStandardConstant( color_name_cyan2_t, "pen_color_name.cyan2", pen_pen_color_name_t, "68" );
declareStandardConstant( color_name_cyan3_t, "pen_color_name.cyan3", pen_pen_color_name_t, "69" );
declareStandardConstant( color_name_cyan4_t, "pen_color_name.cyan4", pen_pen_color_name_t, "70" );
declareStandardConstant( color_name_darkblue_t, "pen_color_name.darkblue", pen_pen_color_name_t, "71" );
declareStandardConstant( color_name_darkcyan_t, "pen_color_name.darkcyan", pen_pen_color_name_t, "72" );
declareStandardConstant( color_name_darkgoldenrod_t, "pen_color_name.darkgoldenrod", pen_pen_color_name_t, "73" );
declareStandardConstant( color_name_darkgoldenrod1_t, "pen_color_name.darkgoldenrod1", pen_pen_color_name_t, "74" );
declareStandardConstant( color_name_darkgoldenrod2_t, "pen_color_name.darkgoldenrod2", pen_pen_color_name_t, "75" );
declareStandardConstant( color_name_darkgoldenrod3_t, "pen_color_name.darkgoldenrod3", pen_pen_color_name_t, "76" );
declareStandardConstant( color_name_darkgoldenrod4_t, "pen_color_name.darkgoldenrod4", pen_pen_color_name_t, "77" );
declareStandardConstant( color_name_darkgreen_t, "pen_color_name.darkgreen", pen_pen_color_name_t, "78" );
declareStandardConstant( color_name_darkgrey_t, "pen_color_name.darkgrey", pen_pen_color_name_t, "79" );
declareStandardConstant( color_name_darkkhaki_t, "pen_color_name.darkkhaki", pen_pen_color_name_t, "80" );
declareStandardConstant( color_name_darkmagenta_t, "pen_color_name.darkmagenta", pen_pen_color_name_t, "81" );
declareStandardConstant( color_name_darkolivegreen_t, "pen_color_name.darkolivegreen", pen_pen_color_name_t, "82" );
declareStandardConstant( color_name_darkolivegreen1_t, "pen_color_name.darkolivegreen1", pen_pen_color_name_t, "83" );
declareStandardConstant( color_name_darkolivegreen2_t, "pen_color_name.darkolivegreen2", pen_pen_color_name_t, "84" );
declareStandardConstant( color_name_darkolivegreen3_t, "pen_color_name.darkolivegreen3", pen_pen_color_name_t, "85" );
declareStandardConstant( color_name_darkolivegreen4_t, "pen_color_name.darkolivegreen4", pen_pen_color_name_t, "86" );
declareStandardConstant( color_name_darkorange_t, "pen_color_name.darkorange", pen_pen_color_name_t, "87" );
declareStandardConstant( color_name_darkorange1_t, "pen_color_name.darkorange1", pen_pen_color_name_t, "88" );
declareStandardConstant( color_name_darkorange2_t, "pen_color_name.darkorange2", pen_pen_color_name_t, "89" );
declareStandardConstant( color_name_darkorange3_t, "pen_color_name.darkorange3", pen_pen_color_name_t, "90" );
declareStandardConstant( color_name_darkorange4_t, "pen_color_name.darkorange4", pen_pen_color_name_t, "91" );
declareStandardConstant( color_name_darkorchid_t, "pen_color_name.darkorchid", pen_pen_color_name_t, "92" );
declareStandardConstant( color_name_darkorchid1_t, "pen_color_name.darkorchid1", pen_pen_color_name_t, "93" );
declareStandardConstant( color_name_darkorchid2_t, "pen_color_name.darkorchid2", pen_pen_color_name_t, "94" );
declareStandardConstant( color_name_darkorchid3_t, "pen_color_name.darkorchid3", pen_pen_color_name_t, "95" );
declareStandardConstant( color_name_darkorchid4_t, "pen_color_name.darkorchid4", pen_pen_color_name_t, "96" );
declareStandardConstant( color_name_darkred_t, "pen_color_name.darkred", pen_pen_color_name_t, "97" );
declareStandardConstant( color_name_darksalmon_t, "pen_color_name.darksalmon", pen_pen_color_name_t, "98" );
declareStandardConstant( color_name_darkseagreen_t, "pen_color_name.darkseagreen", pen_pen_color_name_t, "99" );
declareStandardConstant( color_name_darkseagreen1_t, "pen_color_name.darkseagreen1", pen_pen_color_name_t, "100" );
declareStandardConstant( color_name_darkseagreen2_t, "pen_color_name.darkseagreen2", pen_pen_color_name_t, "101" );
declareStandardConstant( color_name_darkseagreen3_t, "pen_color_name.darkseagreen3", pen_pen_color_name_t, "102" );
declareStandardConstant( color_name_darkseagreen4_t, "pen_color_name.darkseagreen4", pen_pen_color_name_t, "103" );
declareStandardConstant( color_name_darkslateblue_t, "pen_color_name.darkslateblue", pen_pen_color_name_t, "104" );
declareStandardConstant( color_name_darkslategrey_t, "pen_color_name.darkslategrey", pen_pen_color_name_t, "105" );
declareStandardConstant( color_name_darkslategrey1_t, "pen_color_name.darkslategrey1", pen_pen_color_name_t, "106" );
declareStandardConstant( color_name_darkslategrey2_t, "pen_color_name.darkslategrey2", pen_pen_color_name_t, "107" );
declareStandardConstant( color_name_darkslategrey3_t, "pen_color_name.darkslategrey3", pen_pen_color_name_t, "108" );
declareStandardConstant( color_name_darkslategrey4_t, "pen_color_name.darkslategrey4", pen_pen_color_name_t, "109" );
declareStandardConstant( color_name_darkturquoise_t, "pen_color_name.darkturquoise", pen_pen_color_name_t, "110" );
declareStandardConstant( color_name_darkviolet_t, "pen_color_name.darkviolet", pen_pen_color_name_t, "111" );
declareStandardConstant( color_name_deeppink_t, "pen_color_name.deeppink", pen_pen_color_name_t, "112" );
declareStandardConstant( color_name_deeppink1_t, "pen_color_name.deeppink1", pen_pen_color_name_t, "113" );
declareStandardConstant( color_name_deeppink2_t, "pen_color_name.deeppink2", pen_pen_color_name_t, "114" );
declareStandardConstant( color_name_deeppink3_t, "pen_color_name.deeppink3", pen_pen_color_name_t, "115" );
declareStandardConstant( color_name_deeppink4_t, "pen_color_name.deeppink4", pen_pen_color_name_t, "116" );
declareStandardConstant( color_name_deepskyblue_t, "pen_color_name.deepskyblue", pen_pen_color_name_t, "117" );
declareStandardConstant( color_name_deepskyblue1_t, "pen_color_name.deepskyblue1", pen_pen_color_name_t, "118" );
declareStandardConstant( color_name_deepskyblue2_t, "pen_color_name.deepskyblue2", pen_pen_color_name_t, "119" );
declareStandardConstant( color_name_deepskyblue3_t, "pen_color_name.deepskyblue3", pen_pen_color_name_t, "120" );
declareStandardConstant( color_name_deepskyblue4_t, "pen_color_name.deepskyblue4", pen_pen_color_name_t, "121" );
declareStandardConstant( color_name_dimgrey_t, "pen_color_name.dimgrey", pen_pen_color_name_t, "122" );
declareStandardConstant( color_name_dodgerblue_t, "pen_color_name.dodgerblue", pen_pen_color_name_t, "123" );
declareStandardConstant( color_name_dodgerblue1_t, "pen_color_name.dodgerblue1", pen_pen_color_name_t, "124" );
declareStandardConstant( color_name_dodgerblue2_t, "pen_color_name.dodgerblue2", pen_pen_color_name_t, "125" );
declareStandardConstant( color_name_dodgerblue3_t, "pen_color_name.dodgerblue3", pen_pen_color_name_t, "126" );
declareStandardConstant( color_name_dodgerblue4_t, "pen_color_name.dodgerblue4", pen_pen_color_name_t, "127" );
end declarePenColorNames1;

procedure declarePenColorNames2 is
begin
declareStandardConstant( color_name_firebrick_t, "pen_color_name.firebrick", pen_pen_color_name_t, "128" );
declareStandardConstant( color_name_firebrick1_t, "pen_color_name.firebrick1", pen_pen_color_name_t, "129" );
declareStandardConstant( color_name_firebrick2_t, "pen_color_name.firebrick2", pen_pen_color_name_t, "130" );
declareStandardConstant( color_name_firebrick3_t, "pen_color_name.firebrick3", pen_pen_color_name_t, "131" );
declareStandardConstant( color_name_firebrick4_t, "pen_color_name.firebrick4", pen_pen_color_name_t, "132" );
declareStandardConstant( color_name_floralwhite_t, "pen_color_name.floralwhite", pen_pen_color_name_t, "133" );
declareStandardConstant( color_name_forestgreen_t, "pen_color_name.forestgreen", pen_pen_color_name_t, "134" );
declareStandardConstant( color_name_gainsboro_t, "pen_color_name.gainsboro", pen_pen_color_name_t, "135" );
declareStandardConstant( color_name_ghostwhite_t, "pen_color_name.ghostwhite", pen_pen_color_name_t, "136" );
declareStandardConstant( color_name_gold_t, "pen_color_name.gold", pen_pen_color_name_t, "137" );
declareStandardConstant( color_name_gold1_t, "pen_color_name.gold1", pen_pen_color_name_t, "138" );
declareStandardConstant( color_name_gold2_t, "pen_color_name.gold2", pen_pen_color_name_t, "139" );
declareStandardConstant( color_name_gold3_t, "pen_color_name.gold3", pen_pen_color_name_t, "140" );
declareStandardConstant( color_name_gold4_t, "pen_color_name.gold4", pen_pen_color_name_t, "141" );
declareStandardConstant( color_name_goldenrod_t, "pen_color_name.goldenrod", pen_pen_color_name_t, "142" );
declareStandardConstant( color_name_goldenrod1_t, "pen_color_name.goldenrod1", pen_pen_color_name_t, "143" );
declareStandardConstant( color_name_goldenrod2_t, "pen_color_name.goldenrod2", pen_pen_color_name_t, "144" );
declareStandardConstant( color_name_goldenrod3_t, "pen_color_name.goldenrod3", pen_pen_color_name_t, "145" );
declareStandardConstant( color_name_goldenrod4_t, "pen_color_name.goldenrod4", pen_pen_color_name_t, "146" );
declareStandardConstant( color_name_green_t, "pen_color_name.green", pen_pen_color_name_t, "147" );
declareStandardConstant( color_name_green1_t, "pen_color_name.green1", pen_pen_color_name_t, "148" );
declareStandardConstant( color_name_green2_t, "pen_color_name.green2", pen_pen_color_name_t, "149" );
declareStandardConstant( color_name_green3_t, "pen_color_name.green3", pen_pen_color_name_t, "150" );
declareStandardConstant( color_name_green4_t, "pen_color_name.green4", pen_pen_color_name_t, "151" );
declareStandardConstant( color_name_greenyellow_t, "pen_color_name.greenyellow", pen_pen_color_name_t, "152" );
declareStandardConstant( color_name_grey_t, "pen_color_name.grey", pen_pen_color_name_t, "153" );
declareStandardConstant( color_name_honeydew_t, "pen_color_name.honeydew", pen_pen_color_name_t, "154" );
declareStandardConstant( color_name_honeydew1_t, "pen_color_name.honeydew1", pen_pen_color_name_t, "155" );
declareStandardConstant( color_name_honeydew2_t, "pen_color_name.honeydew2", pen_pen_color_name_t, "156" );
declareStandardConstant( color_name_honeydew3_t, "pen_color_name.honeydew3", pen_pen_color_name_t, "157" );
declareStandardConstant( color_name_honeydew4_t, "pen_color_name.honeydew4", pen_pen_color_name_t, "158" );
declareStandardConstant( color_name_hotpink_t, "pen_color_name.hotpink", pen_pen_color_name_t, "159" );
declareStandardConstant( color_name_hotpink1_t, "pen_color_name.hotpink1", pen_pen_color_name_t, "160" );
declareStandardConstant( color_name_hotpink2_t, "pen_color_name.hotpink2", pen_pen_color_name_t, "161" );
declareStandardConstant( color_name_hotpink3_t, "pen_color_name.hotpink3", pen_pen_color_name_t, "162" );
declareStandardConstant( color_name_hotpink4_t, "pen_color_name.hotpink4", pen_pen_color_name_t, "163" );
declareStandardConstant( color_name_indianred_t, "pen_color_name.indianred", pen_pen_color_name_t, "164" );
declareStandardConstant( color_name_indianred1_t, "pen_color_name.indianred1", pen_pen_color_name_t, "165" );
declareStandardConstant( color_name_indianred2_t, "pen_color_name.indianred2", pen_pen_color_name_t, "166" );
declareStandardConstant( color_name_indianred3_t, "pen_color_name.indianred3", pen_pen_color_name_t, "167" );
declareStandardConstant( color_name_indianred4_t, "pen_color_name.indianred4", pen_pen_color_name_t, "168" );
declareStandardConstant( color_name_ivory_t, "pen_color_name.ivory", pen_pen_color_name_t, "169" );
declareStandardConstant( color_name_ivory1_t, "pen_color_name.ivory1", pen_pen_color_name_t, "170" );
declareStandardConstant( color_name_ivory2_t, "pen_color_name.ivory2", pen_pen_color_name_t, "171" );
declareStandardConstant( color_name_ivory3_t, "pen_color_name.ivory3", pen_pen_color_name_t, "172" );
declareStandardConstant( color_name_ivory4_t, "pen_color_name.ivory4", pen_pen_color_name_t, "173" );
end declarePenColorNames2;

procedure declarePenColorNames3 is
begin
declareStandardConstant( color_name_khaki_t, "pen_color_name.khaki", pen_pen_color_name_t, "174" );
declareStandardConstant( color_name_khaki1_t, "pen_color_name.khaki1", pen_pen_color_name_t, "175" );
declareStandardConstant( color_name_khaki2_t, "pen_color_name.khaki2", pen_pen_color_name_t, "176" );
declareStandardConstant( color_name_khaki3_t, "pen_color_name.khaki3", pen_pen_color_name_t, "177" );
declareStandardConstant( color_name_khaki4_t, "pen_color_name.khaki4", pen_pen_color_name_t, "178" );
declareStandardConstant( color_name_lavender_t, "pen_color_name.lavender", pen_pen_color_name_t, "179" );
declareStandardConstant( color_name_lavenderblush_t, "pen_color_name.lavenderblush", pen_pen_color_name_t, "180" );
declareStandardConstant( color_name_lavenderblush1_t, "pen_color_name.lavenderblush1", pen_pen_color_name_t, "181" );
declareStandardConstant( color_name_lavenderblush2_t, "pen_color_name.lavenderblush2", pen_pen_color_name_t, "182" );
declareStandardConstant( color_name_lavenderblush3_t, "pen_color_name.lavenderblush3", pen_pen_color_name_t, "183" );
declareStandardConstant( color_name_lavenderblush4_t, "pen_color_name.lavenderblush4", pen_pen_color_name_t, "184" );
declareStandardConstant( color_name_lawngreen_t, "pen_color_name.lawngreen", pen_pen_color_name_t, "185" );
declareStandardConstant( color_name_lemonchiffon_t, "pen_color_name.lemonchiffon", pen_pen_color_name_t, "186" );
declareStandardConstant( color_name_lemonchiffon1_t, "pen_color_name.lemonchiffon1", pen_pen_color_name_t, "187" );
declareStandardConstant( color_name_lemonchiffon2_t, "pen_color_name.lemonchiffon2", pen_pen_color_name_t, "188" );
declareStandardConstant( color_name_lemonchiffon3_t, "pen_color_name.lemonchiffon3", pen_pen_color_name_t, "189" );
declareStandardConstant( color_name_lemonchiffon4_t, "pen_color_name.lemonchiffon4", pen_pen_color_name_t, "190" );
declareStandardConstant( color_name_lightblue_t, "pen_color_name.lightblue", pen_pen_color_name_t, "191" );
declareStandardConstant( color_name_lightblue1_t, "pen_color_name.lightblue1", pen_pen_color_name_t, "192" );
declareStandardConstant( color_name_lightblue2_t, "pen_color_name.lightblue2", pen_pen_color_name_t, "193" );
declareStandardConstant( color_name_lightblue3_t, "pen_color_name.lightblue3", pen_pen_color_name_t, "194" );
declareStandardConstant( color_name_lightblue4_t, "pen_color_name.lightblue4", pen_pen_color_name_t, "195" );
declareStandardConstant( color_name_lightcoral_t, "pen_color_name.lightcoral", pen_pen_color_name_t, "196" );
declareStandardConstant( color_name_lightcyan_t, "pen_color_name.lightcyan", pen_pen_color_name_t, "197" );
declareStandardConstant( color_name_lightcyan1_t, "pen_color_name.lightcyan1", pen_pen_color_name_t, "198" );
declareStandardConstant( color_name_lightcyan2_t, "pen_color_name.lightcyan2", pen_pen_color_name_t, "199" );
declareStandardConstant( color_name_lightcyan3_t, "pen_color_name.lightcyan3", pen_pen_color_name_t, "200" );
declareStandardConstant( color_name_lightcyan4_t, "pen_color_name.lightcyan4", pen_pen_color_name_t, "201" );
declareStandardConstant( color_name_lightgoldenrod_t, "pen_color_name.lightgoldenrod", pen_pen_color_name_t, "202" );
declareStandardConstant( color_name_lightgoldenrod1_t, "pen_color_name.lightgoldenrod1", pen_pen_color_name_t, "203" );
declareStandardConstant( color_name_lightgoldenrod2_t, "pen_color_name.lightgoldenrod2", pen_pen_color_name_t, "204" );
declareStandardConstant( color_name_lightgoldenrod3_t, "pen_color_name.lightgoldenrod3", pen_pen_color_name_t, "205" );
declareStandardConstant( color_name_lightgoldenrod4_t, "pen_color_name.lightgoldenrod4", pen_pen_color_name_t, "206" );
declareStandardConstant( color_name_lightgoldenrodyellow_t, "pen_color_name.lightgoldenrodyellow", pen_pen_color_name_t, "207" );
declareStandardConstant( color_name_lightgreen_t, "pen_color_name.lightgreen", pen_pen_color_name_t, "208" );
declareStandardConstant( color_name_lightgrey_t, "pen_color_name.lightgrey", pen_pen_color_name_t, "209" );
declareStandardConstant( color_name_lightpink_t, "pen_color_name.lightpink", pen_pen_color_name_t, "210" );
declareStandardConstant( color_name_lightpink1_t, "pen_color_name.lightpink1", pen_pen_color_name_t, "211" );
declareStandardConstant( color_name_lightpink2_t, "pen_color_name.lightpink2", pen_pen_color_name_t, "212" );
declareStandardConstant( color_name_lightpink3_t, "pen_color_name.lightpink3", pen_pen_color_name_t, "213" );
declareStandardConstant( color_name_lightpink4_t, "pen_color_name.lightpink4", pen_pen_color_name_t, "214" );
declareStandardConstant( color_name_lightsalmon_t, "pen_color_name.lightsalmon", pen_pen_color_name_t, "215" );
declareStandardConstant( color_name_lightsalmon1_t, "pen_color_name.lightsalmon1", pen_pen_color_name_t, "216" );
declareStandardConstant( color_name_lightsalmon2_t, "pen_color_name.lightsalmon2", pen_pen_color_name_t, "217" );
declareStandardConstant( color_name_lightsalmon3_t, "pen_color_name.lightsalmon3", pen_pen_color_name_t, "218" );
declareStandardConstant( color_name_lightsalmon4_t, "pen_color_name.lightsalmon4", pen_pen_color_name_t, "219" );
declareStandardConstant( color_name_lightseagreen_t, "pen_color_name.lightseagreen", pen_pen_color_name_t, "220" );
declareStandardConstant( color_name_lightskyblue_t, "pen_color_name.lightskyblue", pen_pen_color_name_t, "221" );
declareStandardConstant( color_name_lightskyblue1_t, "pen_color_name.lightskyblue1", pen_pen_color_name_t, "222" );
declareStandardConstant( color_name_lightskyblue2_t, "pen_color_name.lightskyblue2", pen_pen_color_name_t, "223" );
declareStandardConstant( color_name_lightskyblue3_t, "pen_color_name.lightskyblue3", pen_pen_color_name_t, "224" );
declareStandardConstant( color_name_lightskyblue4_t, "pen_color_name.lightskyblue4", pen_pen_color_name_t, "225" );
declareStandardConstant( color_name_lightslateblue_t, "pen_color_name.lightslateblue", pen_pen_color_name_t, "226" );
declareStandardConstant( color_name_lightslategrey_t, "pen_color_name.lightslategrey", pen_pen_color_name_t, "227" );
declareStandardConstant( color_name_lightsteelblue_t, "pen_color_name.lightsteelblue", pen_pen_color_name_t, "228" );
declareStandardConstant( color_name_lightsteelblue1_t, "pen_color_name.lightsteelblue1", pen_pen_color_name_t, "229" );
declareStandardConstant( color_name_lightsteelblue2_t, "pen_color_name.lightsteelblue2", pen_pen_color_name_t, "230" );
declareStandardConstant( color_name_lightsteelblue3_t, "pen_color_name.lightsteelblue3", pen_pen_color_name_t, "231" );
declareStandardConstant( color_name_lightsteelblue4_t, "pen_color_name.lightsteelblue4", pen_pen_color_name_t, "232" );
declareStandardConstant( color_name_lightyellow_t, "pen_color_name.lightyellow", pen_pen_color_name_t, "233" );
declareStandardConstant( color_name_lightyellow1_t, "pen_color_name.lightyellow1", pen_pen_color_name_t, "234" );
declareStandardConstant( color_name_lightyellow2_t, "pen_color_name.lightyellow2", pen_pen_color_name_t, "235" );
declareStandardConstant( color_name_lightyellow3_t, "pen_color_name.lightyellow3", pen_pen_color_name_t, "236" );
declareStandardConstant( color_name_lightyellow4_t, "pen_color_name.lightyellow4", pen_pen_color_name_t, "237" );
declareStandardConstant( color_name_limegreen_t, "pen_color_name.limegreen", pen_pen_color_name_t, "238" );
declareStandardConstant( color_name_linen_t, "pen_color_name.linen", pen_pen_color_name_t, "239" );
declareStandardConstant( color_name_magenta_t, "pen_color_name.magenta", pen_pen_color_name_t, "240" );
declareStandardConstant( color_name_magenta1_t, "pen_color_name.magenta1", pen_pen_color_name_t, "241" );
declareStandardConstant( color_name_magenta2_t, "pen_color_name.magenta2", pen_pen_color_name_t, "242" );
declareStandardConstant( color_name_magenta3_t, "pen_color_name.magenta3", pen_pen_color_name_t, "243" );
declareStandardConstant( color_name_magenta4_t, "pen_color_name.magenta4", pen_pen_color_name_t, "244" );
declareStandardConstant( color_name_maroon_t, "pen_color_name.maroon", pen_pen_color_name_t, "245" );
declareStandardConstant( color_name_maroon1_t, "pen_color_name.maroon1", pen_pen_color_name_t, "246" );
declareStandardConstant( color_name_maroon2_t, "pen_color_name.maroon2", pen_pen_color_name_t, "247" );
declareStandardConstant( color_name_maroon3_t, "pen_color_name.maroon3", pen_pen_color_name_t, "248" );
declareStandardConstant( color_name_maroon4_t, "pen_color_name.maroon4", pen_pen_color_name_t, "249" );
declareStandardConstant( color_name_mediumaquamarine_t, "pen_color_name.mediumaquamarine", pen_pen_color_name_t, "250" );
declareStandardConstant( color_name_mediumblue_t, "pen_color_name.mediumblue", pen_pen_color_name_t, "251" );
declareStandardConstant( color_name_mediumorchid_t, "pen_color_name.mediumorchid", pen_pen_color_name_t, "252" );
declareStandardConstant( color_name_mediumorchid1_t, "pen_color_name.mediumorchid1", pen_pen_color_name_t, "253" );
declareStandardConstant( color_name_mediumorchid2_t, "pen_color_name.mediumorchid2", pen_pen_color_name_t, "254" );
declareStandardConstant( color_name_mediumorchid3_t, "pen_color_name.mediumorchid3", pen_pen_color_name_t, "255" );
declareStandardConstant( color_name_mediumorchid4_t, "pen_color_name.mediumorchid4", pen_pen_color_name_t, "256" );
declareStandardConstant( color_name_mediumpurple_t, "pen_color_name.mediumpurple", pen_pen_color_name_t, "257" );
declareStandardConstant( color_name_mediumpurple1_t, "pen_color_name.mediumpurple1", pen_pen_color_name_t, "258" );
declareStandardConstant( color_name_mediumpurple2_t, "pen_color_name.mediumpurple2", pen_pen_color_name_t, "259" );
declareStandardConstant( color_name_mediumpurple3_t, "pen_color_name.mediumpurple3", pen_pen_color_name_t, "260" );
declareStandardConstant( color_name_mediumpurple4_t, "pen_color_name.mediumpurple4", pen_pen_color_name_t, "261" );
declareStandardConstant( color_name_mediumseagreen_t, "pen_color_name.mediumseagreen", pen_pen_color_name_t, "262" );
declareStandardConstant( color_name_mediumslateblue_t, "pen_color_name.mediumslateblue", pen_pen_color_name_t, "263" );
declareStandardConstant( color_name_mediumspringgreen_t, "pen_color_name.mediumspringgreen", pen_pen_color_name_t, "264" );
declareStandardConstant( color_name_mediumturquoise_t, "pen_color_name.mediumturquoise", pen_pen_color_name_t, "265" );
declareStandardConstant( color_name_mediumvioletred_t, "pen_color_name.mediumvioletred", pen_pen_color_name_t, "266" );
declareStandardConstant( color_name_midnightblue_t, "pen_color_name.midnightblue", pen_pen_color_name_t, "267" );
declareStandardConstant( color_name_mintcream_t, "pen_color_name.mintcream", pen_pen_color_name_t, "268" );
declareStandardConstant( color_name_mistyrose_t, "pen_color_name.mistyrose", pen_pen_color_name_t, "269" );
declareStandardConstant( color_name_mistyrose1_t, "pen_color_name.mistyrose1", pen_pen_color_name_t, "270" );
declareStandardConstant( color_name_mistyrose2_t, "pen_color_name.mistyrose2", pen_pen_color_name_t, "271" );
declareStandardConstant( color_name_mistyrose3_t, "pen_color_name.mistyrose3", pen_pen_color_name_t, "272" );
declareStandardConstant( color_name_mistyrose4_t, "pen_color_name.mistyrose4", pen_pen_color_name_t, "273" );
end declarePenColorNames3;

procedure declarePenColorNames4 is
begin
declareStandardConstant( color_name_moccasin_t, "pen_color_name.moccasin", pen_pen_color_name_t, "274" );
declareStandardConstant( color_name_navajowhite_t, "pen_color_name.navajowhite", pen_pen_color_name_t, "275" );
declareStandardConstant( color_name_navajowhite1_t, "pen_color_name.navajowhite1", pen_pen_color_name_t, "276" );
declareStandardConstant( color_name_navajowhite2_t, "pen_color_name.navajowhite2", pen_pen_color_name_t, "277" );
declareStandardConstant( color_name_navajowhite3_t, "pen_color_name.navajowhite3", pen_pen_color_name_t, "278" );
declareStandardConstant( color_name_navajowhite4_t, "pen_color_name.navajowhite4", pen_pen_color_name_t, "279" );
declareStandardConstant( color_name_navyblue_t, "pen_color_name.navyblue", pen_pen_color_name_t, "280" );
declareStandardConstant( color_name_oldlace_t, "pen_color_name.oldlace", pen_pen_color_name_t, "281" );
declareStandardConstant( color_name_olivedrab_t, "pen_color_name.olivedrab", pen_pen_color_name_t, "282" );
declareStandardConstant( color_name_olivedrab1_t, "pen_color_name.olivedrab1", pen_pen_color_name_t, "283" );
declareStandardConstant( color_name_olivedrab2_t, "pen_color_name.olivedrab2", pen_pen_color_name_t, "284" );
declareStandardConstant( color_name_olivedrab3_t, "pen_color_name.olivedrab3", pen_pen_color_name_t, "285" );
declareStandardConstant( color_name_olivedrab4_t, "pen_color_name.olivedrab4", pen_pen_color_name_t, "286" );
declareStandardConstant( color_name_orange_t, "pen_color_name.orange", pen_pen_color_name_t, "287" );
declareStandardConstant( color_name_orange1_t, "pen_color_name.orange1", pen_pen_color_name_t, "288" );
declareStandardConstant( color_name_orange2_t, "pen_color_name.orange2", pen_pen_color_name_t, "289" );
declareStandardConstant( color_name_orange3_t, "pen_color_name.orange3", pen_pen_color_name_t, "290" );
declareStandardConstant( color_name_orange4_t, "pen_color_name.orange4", pen_pen_color_name_t, "291" );
declareStandardConstant( color_name_orangered_t, "pen_color_name.orangered", pen_pen_color_name_t, "292" );
declareStandardConstant( color_name_orangered1_t, "pen_color_name.orangered1", pen_pen_color_name_t, "293" );
declareStandardConstant( color_name_orangered2_t, "pen_color_name.orangered2", pen_pen_color_name_t, "294" );
declareStandardConstant( color_name_orangered3_t, "pen_color_name.orangered3", pen_pen_color_name_t, "295" );
declareStandardConstant( color_name_orangered4_t, "pen_color_name.orangered4", pen_pen_color_name_t, "296" );
declareStandardConstant( color_name_orchid_t, "pen_color_name.orchid", pen_pen_color_name_t, "297" );
declareStandardConstant( color_name_orchid1_t, "pen_color_name.orchid1", pen_pen_color_name_t, "298" );
declareStandardConstant( color_name_orchid2_t, "pen_color_name.orchid2", pen_pen_color_name_t, "299" );
declareStandardConstant( color_name_orchid3_t, "pen_color_name.orchid3", pen_pen_color_name_t, "300" );
declareStandardConstant( color_name_orchid4_t, "pen_color_name.orchid4", pen_pen_color_name_t, "301" );
declareStandardConstant( color_name_palegoldenrod_t, "pen_color_name.palegoldenrod", pen_pen_color_name_t, "302" );
declareStandardConstant( color_name_palegreen_t, "pen_color_name.palegreen", pen_pen_color_name_t, "303" );
declareStandardConstant( color_name_palegreen1_t, "pen_color_name.palegreen1", pen_pen_color_name_t, "304" );
declareStandardConstant( color_name_palegreen2_t, "pen_color_name.palegreen2", pen_pen_color_name_t, "305" );
declareStandardConstant( color_name_palegreen3_t, "pen_color_name.palegreen3", pen_pen_color_name_t, "306" );
declareStandardConstant( color_name_palegreen4_t, "pen_color_name.palegreen4", pen_pen_color_name_t, "307" );
declareStandardConstant( color_name_paleturquoise_t, "pen_color_name.paleturquoise", pen_pen_color_name_t, "308" );
declareStandardConstant( color_name_paleturquoise1_t, "pen_color_name.paleturquoise1", pen_pen_color_name_t, "309" );
declareStandardConstant( color_name_paleturquoise2_t, "pen_color_name.paleturquoise2", pen_pen_color_name_t, "310" );
declareStandardConstant( color_name_paleturquoise3_t, "pen_color_name.paleturquoise3", pen_pen_color_name_t, "311" );
declareStandardConstant( color_name_paleturquoise4_t, "pen_color_name.paleturquoise4", pen_pen_color_name_t, "312" );
declareStandardConstant( color_name_palevioletred_t, "pen_color_name.palevioletred", pen_pen_color_name_t, "313" );
declareStandardConstant( color_name_palevioletred1_t, "pen_color_name.palevioletred1", pen_pen_color_name_t, "314" );
declareStandardConstant( color_name_palevioletred2_t, "pen_color_name.palevioletred2", pen_pen_color_name_t, "315" );
declareStandardConstant( color_name_palevioletred3_t, "pen_color_name.palevioletred3", pen_pen_color_name_t, "316" );
declareStandardConstant( color_name_palevioletred4_t, "pen_color_name.palevioletred4", pen_pen_color_name_t, "317" );
declareStandardConstant( color_name_papayawhip_t, "pen_color_name.papayawhip", pen_pen_color_name_t, "318" );
declareStandardConstant( color_name_peachpuff_t, "pen_color_name.peachpuff", pen_pen_color_name_t, "319" );
declareStandardConstant( color_name_peachpuff1_t, "pen_color_name.peachpuff1", pen_pen_color_name_t, "320" );
declareStandardConstant( color_name_peachpuff2_t, "pen_color_name.peachpuff2", pen_pen_color_name_t, "321" );
declareStandardConstant( color_name_peachpuff3_t, "pen_color_name.peachpuff3", pen_pen_color_name_t, "322" );
declareStandardConstant( color_name_peachpuff4_t, "pen_color_name.peachpuff4", pen_pen_color_name_t, "323" );
declareStandardConstant( color_name_peru_t, "pen_color_name.peru", pen_pen_color_name_t, "324" );
declareStandardConstant( color_name_pink_t, "pen_color_name.pink", pen_pen_color_name_t, "325" );
declareStandardConstant( color_name_pink1_t, "pen_color_name.pink1", pen_pen_color_name_t, "326" );
declareStandardConstant( color_name_pink2_t, "pen_color_name.pink2", pen_pen_color_name_t, "327" );
declareStandardConstant( color_name_pink3_t, "pen_color_name.pink3", pen_pen_color_name_t, "328" );
declareStandardConstant( color_name_pink4_t, "pen_color_name.pink4", pen_pen_color_name_t, "329" );
declareStandardConstant( color_name_plum_t, "pen_color_name.plum", pen_pen_color_name_t, "330" );
declareStandardConstant( color_name_plum1_t, "pen_color_name.plum1", pen_pen_color_name_t, "331" );
declareStandardConstant( color_name_plum2_t, "pen_color_name.plum2", pen_pen_color_name_t, "332" );
declareStandardConstant( color_name_plum3_t, "pen_color_name.plum3", pen_pen_color_name_t, "333" );
declareStandardConstant( color_name_plum4_t, "pen_color_name.plum4", pen_pen_color_name_t, "334" );
declareStandardConstant( color_name_powderblue_t, "pen_color_name.powderblue", pen_pen_color_name_t, "335" );
declareStandardConstant( color_name_purple_t, "pen_color_name.purple", pen_pen_color_name_t, "336" );
declareStandardConstant( color_name_purple1_t, "pen_color_name.purple1", pen_pen_color_name_t, "337" );
declareStandardConstant( color_name_purple2_t, "pen_color_name.purple2", pen_pen_color_name_t, "338" );
declareStandardConstant( color_name_purple3_t, "pen_color_name.purple3", pen_pen_color_name_t, "339" );
declareStandardConstant( color_name_purple4_t, "pen_color_name.purple4", pen_pen_color_name_t, "340" );
declareStandardConstant( color_name_red_t, "pen_color_name.red", pen_pen_color_name_t, "341" );
declareStandardConstant( color_name_red1_t, "pen_color_name.red1", pen_pen_color_name_t, "342" );
declareStandardConstant( color_name_red2_t, "pen_color_name.red2", pen_pen_color_name_t, "343" );
declareStandardConstant( color_name_red3_t, "pen_color_name.red3", pen_pen_color_name_t, "344" );
declareStandardConstant( color_name_red4_t, "pen_color_name.red4", pen_pen_color_name_t, "345" );
declareStandardConstant( color_name_rosybrown_t, "pen_color_name.rosybrown", pen_pen_color_name_t, "346" );
declareStandardConstant( color_name_rosybrown1_t, "pen_color_name.rosybrown1", pen_pen_color_name_t, "347" );
declareStandardConstant( color_name_rosybrown2_t, "pen_color_name.rosybrown2", pen_pen_color_name_t, "348" );
declareStandardConstant( color_name_rosybrown3_t, "pen_color_name.rosybrown3", pen_pen_color_name_t, "349" );
declareStandardConstant( color_name_rosybrown4_t, "pen_color_name.rosybrown4", pen_pen_color_name_t, "350" );
declareStandardConstant( color_name_royalblue_t, "pen_color_name.royalblue", pen_pen_color_name_t, "351" );
declareStandardConstant( color_name_royalblue1_t, "pen_color_name.royalblue1", pen_pen_color_name_t, "352" );
declareStandardConstant( color_name_royalblue2_t, "pen_color_name.royalblue2", pen_pen_color_name_t, "353" );
declareStandardConstant( color_name_royalblue3_t, "pen_color_name.royalblue3", pen_pen_color_name_t, "354" );
declareStandardConstant( color_name_royalblue4_t, "pen_color_name.royalblue4", pen_pen_color_name_t, "355" );
declareStandardConstant( color_name_saddlebrown_t, "pen_color_name.saddlebrown", pen_pen_color_name_t, "356" );
declareStandardConstant( color_name_salmon_t, "pen_color_name.salmon", pen_pen_color_name_t, "357" );
declareStandardConstant( color_name_salmon1_t, "pen_color_name.salmon1", pen_pen_color_name_t, "358" );
declareStandardConstant( color_name_salmon2_t, "pen_color_name.salmon2", pen_pen_color_name_t, "359" );
declareStandardConstant( color_name_salmon3_t, "pen_color_name.salmon3", pen_pen_color_name_t, "360" );
declareStandardConstant( color_name_salmon4_t, "pen_color_name.salmon4", pen_pen_color_name_t, "361" );
declareStandardConstant( color_name_sandybrown_t, "pen_color_name.sandybrown", pen_pen_color_name_t, "362" );
declareStandardConstant( color_name_seagreen_t, "pen_color_name.seagreen", pen_pen_color_name_t, "363" );
declareStandardConstant( color_name_seagreen1_t, "pen_color_name.seagreen1", pen_pen_color_name_t, "364" );
declareStandardConstant( color_name_seagreen2_t, "pen_color_name.seagreen2", pen_pen_color_name_t, "365" );
declareStandardConstant( color_name_seagreen3_t, "pen_color_name.seagreen3", pen_pen_color_name_t, "366" );
declareStandardConstant( color_name_seagreen4_t, "pen_color_name.seagreen4", pen_pen_color_name_t, "367" );
declareStandardConstant( color_name_seashell_t, "pen_color_name.seashell", pen_pen_color_name_t, "368" );
declareStandardConstant( color_name_seashell1_t, "pen_color_name.seashell1", pen_pen_color_name_t, "369" );
declareStandardConstant( color_name_seashell2_t, "pen_color_name.seashell2", pen_pen_color_name_t, "370" );
declareStandardConstant( color_name_seashell3_t, "pen_color_name.seashell3", pen_pen_color_name_t, "371" );
declareStandardConstant( color_name_seashell4_t, "pen_color_name.seashell4", pen_pen_color_name_t, "372" );
declareStandardConstant( color_name_sienna_t, "pen_color_name.sienna", pen_pen_color_name_t, "373" );
declareStandardConstant( color_name_sienna1_t, "pen_color_name.sienna1", pen_pen_color_name_t, "374" );
declareStandardConstant( color_name_sienna2_t, "pen_color_name.sienna2", pen_pen_color_name_t, "375" );
declareStandardConstant( color_name_sienna3_t, "pen_color_name.sienna3", pen_pen_color_name_t, "376" );
declareStandardConstant( color_name_sienna4_t, "pen_color_name.sienna4", pen_pen_color_name_t, "377" );
end declarePenColorNames4;

procedure declarePenColorNames5 is
begin
declareStandardConstant( color_name_skyblue_t, "pen_color_name.skyblue", pen_pen_color_name_t, "378" );
declareStandardConstant( color_name_skyblue1_t, "pen_color_name.skyblue1", pen_pen_color_name_t, "379" );
declareStandardConstant( color_name_skyblue2_t, "pen_color_name.skyblue2", pen_pen_color_name_t, "380" );
declareStandardConstant( color_name_skyblue3_t, "pen_color_name.skyblue3", pen_pen_color_name_t, "381" );
declareStandardConstant( color_name_skyblue4_t, "pen_color_name.skyblue4", pen_pen_color_name_t, "382" );
declareStandardConstant( color_name_slateblue_t, "pen_color_name.slateblue", pen_pen_color_name_t, "383" );
declareStandardConstant( color_name_slateblue1_t, "pen_color_name.slateblue1", pen_pen_color_name_t, "384" );
declareStandardConstant( color_name_slateblue2_t, "pen_color_name.slateblue2", pen_pen_color_name_t, "385" );
declareStandardConstant( color_name_slateblue3_t, "pen_color_name.slateblue3", pen_pen_color_name_t, "386" );
declareStandardConstant( color_name_slateblue4_t, "pen_color_name.slateblue4", pen_pen_color_name_t, "387" );
declareStandardConstant( color_name_slategray1_t, "pen_color_name.slategray1", pen_pen_color_name_t, "388" );
declareStandardConstant( color_name_slategray2_t, "pen_color_name.slategray2", pen_pen_color_name_t, "389" );
declareStandardConstant( color_name_slategray3_t, "pen_color_name.slategray3", pen_pen_color_name_t, "390" );
declareStandardConstant( color_name_slategray4_t, "pen_color_name.slategray4", pen_pen_color_name_t, "391" );
declareStandardConstant( color_name_slategrey_t, "pen_color_name.slategrey", pen_pen_color_name_t, "392" );
declareStandardConstant( color_name_snow_t, "pen_color_name.snow", pen_pen_color_name_t, "393" );
declareStandardConstant( color_name_snow1_t, "pen_color_name.snow1", pen_pen_color_name_t, "394" );
declareStandardConstant( color_name_snow2_t, "pen_color_name.snow2", pen_pen_color_name_t, "395" );
declareStandardConstant( color_name_snow3_t, "pen_color_name.snow3", pen_pen_color_name_t, "396" );
declareStandardConstant( color_name_snow4_t, "pen_color_name.snow4", pen_pen_color_name_t, "397" );
declareStandardConstant( color_name_springgreen_t, "pen_color_name.springgreen", pen_pen_color_name_t, "398" );
declareStandardConstant( color_name_springgreen1_t, "pen_color_name.springgreen1", pen_pen_color_name_t, "399" );
declareStandardConstant( color_name_springgreen2_t, "pen_color_name.springgreen2", pen_pen_color_name_t, "400" );
declareStandardConstant( color_name_springgreen3_t, "pen_color_name.springgreen3", pen_pen_color_name_t, "401" );
declareStandardConstant( color_name_springgreen4_t, "pen_color_name.springgreen4", pen_pen_color_name_t, "402" );
declareStandardConstant( color_name_steelblue_t, "pen_color_name.steelblue", pen_pen_color_name_t, "403" );
declareStandardConstant( color_name_steelblue1_t, "pen_color_name.steelblue1", pen_pen_color_name_t, "404" );
declareStandardConstant( color_name_steelblue2_t, "pen_color_name.steelblue2", pen_pen_color_name_t, "405" );
declareStandardConstant( color_name_steelblue3_t, "pen_color_name.steelblue3", pen_pen_color_name_t, "406" );
declareStandardConstant( color_name_steelblue4_t, "pen_color_name.steelblue4", pen_pen_color_name_t, "407" );
declareStandardConstant( color_name_tan_t, "pen_color_name.tan", pen_pen_color_name_t, "408" );
declareStandardConstant( color_name_tan1_t, "pen_color_name.tan1", pen_pen_color_name_t, "409" );
declareStandardConstant( color_name_tan2_t, "pen_color_name.tan2", pen_pen_color_name_t, "410" );
declareStandardConstant( color_name_tan3_t, "pen_color_name.tan3", pen_pen_color_name_t, "411" );
declareStandardConstant( color_name_tan4_t, "pen_color_name.tan4", pen_pen_color_name_t, "412" );
declareStandardConstant( color_name_thistle_t, "pen_color_name.thistle", pen_pen_color_name_t, "413" );
declareStandardConstant( color_name_thistle1_t, "pen_color_name.thistle1", pen_pen_color_name_t, "414" );
declareStandardConstant( color_name_thistle2_t, "pen_color_name.thistle2", pen_pen_color_name_t, "415" );
declareStandardConstant( color_name_thistle3_t, "pen_color_name.thistle3", pen_pen_color_name_t, "416" );
declareStandardConstant( color_name_thistle4_t, "pen_color_name.thistle4", pen_pen_color_name_t, "417" );
declareStandardConstant( color_name_tomato_t, "pen_color_name.tomato", pen_pen_color_name_t, "418" );
declareStandardConstant( color_name_tomato1_t, "pen_color_name.tomato1", pen_pen_color_name_t, "419" );
declareStandardConstant( color_name_tomato2_t, "pen_color_name.tomato2", pen_pen_color_name_t, "420" );
declareStandardConstant( color_name_tomato3_t, "pen_color_name.tomato3", pen_pen_color_name_t, "421" );
declareStandardConstant( color_name_tomato4_t, "pen_color_name.tomato4", pen_pen_color_name_t, "422" );
declareStandardConstant( color_name_turquoise_t, "pen_color_name.turquoise", pen_pen_color_name_t, "423" );
declareStandardConstant( color_name_turquoise1_t, "pen_color_name.turquoise1", pen_pen_color_name_t, "424" );
declareStandardConstant( color_name_turquoise2_t, "pen_color_name.turquoise2", pen_pen_color_name_t, "425" );
declareStandardConstant( color_name_turquoise3_t, "pen_color_name.turquoise3", pen_pen_color_name_t, "426" );
declareStandardConstant( color_name_turquoise4_t, "pen_color_name.turquoise4", pen_pen_color_name_t, "427" );
declareStandardConstant( color_name_violet_t, "pen_color_name.violet", pen_pen_color_name_t, "428" );
declareStandardConstant( color_name_violetred_t, "pen_color_name.violetred", pen_pen_color_name_t, "429" );
declareStandardConstant( color_name_violetred1_t, "pen_color_name.violetred1", pen_pen_color_name_t, "430" );
declareStandardConstant( color_name_violetred2_t, "pen_color_name.violetred2", pen_pen_color_name_t, "431" );
declareStandardConstant( color_name_violetred3_t, "pen_color_name.violetred3", pen_pen_color_name_t, "432" );
declareStandardConstant( color_name_violetred4_t, "pen_color_name.violetred4", pen_pen_color_name_t, "433" );
declareStandardConstant( color_name_wheat_t, "pen_color_name.wheat", pen_pen_color_name_t, "434" );
declareStandardConstant( color_name_wheat1_t, "pen_color_name.wheat1", pen_pen_color_name_t, "435" );
declareStandardConstant( color_name_wheat2_t, "pen_color_name.wheat2", pen_pen_color_name_t, "436" );
declareStandardConstant( color_name_wheat3_t, "pen_color_name.wheat3", pen_pen_color_name_t, "437" );
declareStandardConstant( color_name_wheat4_t, "pen_color_name.wheat4", pen_pen_color_name_t, "438" );
declareStandardConstant( color_name_white_t, "pen_color_name.white", pen_pen_color_name_t, "439" );
declareStandardConstant( color_name_whitesmoke_t, "pen_color_name.whitesmoke", pen_pen_color_name_t, "440" );
declareStandardConstant( color_name_yellow_t, "pen_color_name.yellow", pen_pen_color_name_t, "441" );
declareStandardConstant( color_name_yellow1_t, "pen_color_name.yellow1", pen_pen_color_name_t, "442" );
declareStandardConstant( color_name_yellow2_t, "pen_color_name.yellow2", pen_pen_color_name_t, "443" );
declareStandardConstant( color_name_yellow3_t, "pen_color_name.yellow3", pen_pen_color_name_t, "444" );
declareStandardConstant( color_name_yellow4_t, "pen_color_name.yellow4", pen_pen_color_name_t, "445" );
declareStandardConstant( color_name_yellowgreen_t, "pen_color_name.yellowgreen", pen_pen_color_name_t, "446" );
end declarePenColorNames5;

procedure declarePenStandardTypes is
begin
  -- declare pen package types

  declareIdent( pen_coordinate_t, "pen.coordinate", float_t, typeClass );

  declareIdent( pen_rgbcomponent_t, "pen.rgbcomponent", float_t, typeClass );

  declareIdent( pen_rect_t, "pen.rect", root_record_t, typeClass );

  identifiers( pen_rect_t ).value := to_unbounded_string( "4" );
  declareIdent( pen_rect_left_t, "pen.rect.left", pen_coordinate_t, subClass );
  identifiers( pen_rect_left_t ).field_of := pen_rect_t;
  identifiers( pen_rect_left_t ).value := to_unbounded_string( "1" );
  declareIdent( pen_rect_top_t, "pen.a_rect.top", pen_coordinate_t, subClass );
  identifiers( pen_rect_top_t ).field_of := pen_rect_t;
  identifiers( pen_rect_top_t ).value := to_unbounded_string( "2" );
  declareIdent( pen_rect_right_t, "pen.rect.right", pen_coordinate_t, subClass );
  identifiers( pen_rect_right_t ).field_of := pen_rect_t;
  identifiers( pen_rect_right_t ).value := to_unbounded_string( "3" );
  declareIdent( pen_rect_bottom_t, "pen.rect.bottom", pen_coordinate_t, subClass );
  identifiers( pen_rect_bottom_t ).field_of := pen_rect_t;
  identifiers( pen_rect_bottom_t ).value := to_unbounded_string( "4" );

  declareIdent( pen_canvas_id_t, "pen.canvas_id", long_integer_t, typeClass );

  declareIdent( pen_pen_mode_t, "pen.pen_mode", root_enumerated_t, typeClass );
  declareStandardConstant( pen_mode_invert_t, "pen_mode.invert", pen_pen_mode_t, "0" );
  declareStandardConstant( pen_mode_add_t, "pen_mode.add", pen_pen_mode_t, "1" );
  declareStandardConstant( pen_mode_subtract_t, "pen_mode.subtract", pen_pen_mode_t, "2" );
  declareStandardConstant( pen_mode_average_t, "pen_mode.average", pen_pen_mode_t, "3" );
  declareStandardConstant( pen_mode_copy_t, "pen_mode.copy", pen_pen_mode_t, "4" );
  declareStandardConstant( pen_mode_off_t, "pen_mode.off", pen_pen_mode_t, "5" );

  declareIdent( pen_pen_brush_t, "pen.pen_brush", root_enumerated_t, typeClass );
  declareStandardConstant( pen_brush_undefined_t, "pen_brush.undefined", pen_pen_brush_t, "0" );
  declareStandardConstant( pen_brush_pencil_t, "pen_brush.pencil", pen_pen_brush_t, "1" );
  declareStandardConstant( pen_brush_stretch_t, "pen_brush.stretch", pen_pen_brush_t, "2" );
  declareStandardConstant( pen_brush_tile_t, "pen_brush.tile", pen_pen_brush_t, "3" );
  declareStandardConstant( pen_brush_stamp_t, "pen_brush.stamp", pen_pen_brush_t, "4" );
  declareStandardConstant( pen_brush_smear_t, "pen_brush.smear", pen_pen_brush_t, "5" );

  declareIdent( pen_pen_color_name_t, "pen.color_name", root_enumerated_t, typeClass );

  -- TODO: null rect needs to be declared


end declarePenStandardTypes;

procedure declarePenGLConstants is
  function float_to_string( i : GLenum  ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLboolean ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLtypes ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLprimitives ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLvertexarrays ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLmodes ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLpoints ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLlines ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLpolygons ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLdlists ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLalphacompare ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLdbuffer ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLlighting ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLusercplane ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLabuffer ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLatesting ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLblending ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLrendermodes ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLfeedback ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLselection ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLfog ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLlogicops ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLstencil ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLscissorbox ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLuint ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLbuffers ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLlimits ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLhints ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLtransposemat ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLgets ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLhintmodes ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLtextureenvdot3 ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLobsolete ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLpixelmode ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLutility ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLtextureenvcomb ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLtexturemapping ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLerrors ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLpushbits ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLmultitexture ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLmultisample ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLarbmapping ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLtexturecubemap ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLtexturecomp ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLmultitexturearb ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLdebugshaders ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLevaluators ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLUquaddrawstyle ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLUquadricnormal ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLUquadorientation ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLUnurbscallbacks ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLUnurbsproperties ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLUtesscallbacks ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLUtesscontour ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( i : GLUtessproperties ) return string is
  begin
    return to_string( to_unbounded_string( long_float( i ) ) );
  end float_to_string;

  function float_to_string( us : Interfaces.C.unsigned_short ) return string is
  begin
    return to_string( to_unbounded_string( long_float( us ) )  );
  end float_to_string;

  function float_to_string( lf : long_float  ) return string is
  begin
    return to_string( to_unbounded_string( lf )  );
  end float_to_string;

begin
  -- mainly for compatibility

  -- OpenGL types
  --
  -- Here, i've used natural for unsigned types but they aren't exactly
  -- the same!

  declareIdent( pen_glenum_t,   "pen.glenum", natural_t, typeClass  );
  declareIdent( pen_glboolean_t, "pen.glboolean", natural_t, typeClass  );
  declareIdent( pen_glbitfield_t,"pen.glbitfield", natural_t, typeClass );
  declareIdent( pen_glbyte_t,   "pen.glbyte", integer_t, subClass );
  declareIdent( pen_glshort_t,  "pen.glshort", integer_t, subClass );
  declareIdent( pen_glint_t,    "pen.glint", integer_t, subClass );
  declareIdent( pen_glubyte_t,  "pen.glubyte", natural_t, subClass );
  declareIdent( pen_glushort_t, "pen.glushort", natural_t, subClass );
  declareIdent( pen_gluint_t,   "pen.gluint", natural_t, typeClass );
  declareIdent( pen_glsizei_t, "pen.glsizei", natural_t, typeClass );
  declareIdent( pen_glfloat_t,  "pen.glfloat", float_t, subClass );
  declareIdent( pen_glclampf_t, "pen.glclampf", float_t, typeClass );
  declareIdent( pen_gldouble_t, "pen.gldouble", float_t, subClass );
  declareIdent( pen_glclampd_t, "pen.glclampd", float_t, typeClass );

  -- OpenGL constants

  declareStandardConstant( pen_gl_false_t, "pen.gl_false", pen_glboolean_t, float_to_string( GL_FALSE ) );
  declareStandardConstant( pen_gl_true_t, "pen.gl_true", pen_glboolean_t, float_to_string( GL_TRUE ) );

  declareIdent( pen_gltypes_t, "pen.gltypes", integer_t, typeClass  );

  declareStandardConstant( pen_gl_byte_t, "pen.gl_byte", pen_gltypes_t, float_to_string( GL_BYTE ) );
  declareStandardConstant( pen_gl_unsigned_byte_t, "pen.gl_unsigned_byte", pen_gltypes_t, float_to_string( GL_UNSIGNED_BYTE ) );
  declareStandardConstant( pen_gl_short_t, "pen.gl_short", pen_gltypes_t, float_to_string( GL_SHORT ) );
  declareStandardConstant( pen_gl_unsigned_short_t, "pen.gl_unsigned_short", pen_gltypes_t, float_to_string( GL_UNSIGNED_SHORT ) );
  declareStandardConstant( pen_gl_int_t, "pen.gl_int", pen_gltypes_t, float_to_string( GL_INT ) );
  declareStandardConstant( pen_gl_unsigned_int_t, "pen.gl_unsigned_int", pen_gltypes_t, float_to_string( GL_UNSIGNED_INT ) );
  declareStandardConstant( pen_gl_float_t, "pen.gl_float", pen_gltypes_t, float_to_string( GL_FLOAT ) );
  declareStandardConstant( pen_gl_2_bytes_t, "pen.gl_2_bytes", pen_gltypes_t, float_to_string( GL_2_BYTES ) );
  declareStandardConstant( pen_gl_3_bytes_t, "pen.gl_3_bytes", pen_gltypes_t, float_to_string( GL_3_BYTES ) );
  declareStandardConstant( pen_gl_4_bytes_t, "pen.gl_4_bytes", pen_gltypes_t, float_to_string( GL_4_BYTES ) );
  declareStandardConstant( pen_gl_double_t, "pen.gl_double", pen_gltypes_t, float_to_string( GL_DOUBLE ) );

  declareIdent( pen_glprimitives_t, "pen.glprimitives", integer_t, typeClass  );

  declareStandardConstant( pen_gl_points_t, "pen.gl_points", pen_glprimitives_t, float_to_string( GL_POINTS ) );
  declareStandardConstant( pen_gl_lines_t, "pen.gl_lines", pen_glprimitives_t, float_to_string( GL_LINES ) );
  declareStandardConstant( pen_gl_line_loop_t, "pen.gl_line_loop", pen_glprimitives_t, float_to_string( GL_LINE_LOOP ) );
  declareStandardConstant( pen_gl_line_strip_t, "pen.gl_line_strip", pen_glprimitives_t, float_to_string( GL_LINE_STRIP ) );
  declareStandardConstant( pen_gl_triangles_t, "pen.gl_triangles", pen_glprimitives_t, float_to_string( GL_TRIANGLES ) );
  declareStandardConstant( pen_gl_triangle_strip_t, "pen.gl_triangle_strip", pen_glprimitives_t, float_to_string( GL_TRIANGLE_STRIP ) );
  declareStandardConstant( pen_gl_triangle_fan_t, "pen.gl_triangle_fan", pen_glprimitives_t, float_to_string( GL_TRIANGLE_FAN ) );
  declareStandardConstant( pen_gl_quads_t, "pen.gl_quads", pen_glprimitives_t, float_to_string( GL_QUADS ) );
  declareStandardConstant( pen_gl_quad_strip_t, "pen.gl_quad_strip", pen_glprimitives_t, float_to_string( GL_QUAD_STRIP ) );
  declareStandardConstant( pen_gl_polygon_t, "pen.gl_polygon", pen_glprimitives_t, float_to_string( GL_POLYGON ) );

  declareIdent( pen_glvertexarrays_t, "pen.glvertexarrays", integer_t, typeClass  );

  declareStandardConstant( pen_gl_vertex_array_t, "pen.gl_vertex_array", pen_glvertexarrays_t, float_to_string( GL_VERTEX_ARRAY ) );
  declareStandardConstant( pen_gl_normal_array_t, "pen.gl_normal_array", pen_glvertexarrays_t, float_to_string( GL_NORMAL_ARRAY ) );
  declareStandardConstant( pen_gl_color_array_t, "pen.gl_color_array", pen_glvertexarrays_t, float_to_string( GL_COLOR_ARRAY ) );
  declareStandardConstant( pen_gl_index_array_t, "pen.gl_index_array", pen_glvertexarrays_t, float_to_string( GL_INDEX_ARRAY ) );
  declareStandardConstant( pen_gl_texture_coord_array_t, "pen.gl_texture_coord_array", pen_glvertexarrays_t, float_to_string( GL_TEXTURE_COORD_ARRAY ) );
  declareStandardConstant( pen_gl_edge_flag_array_t, "pen.gl_edge_flag_array", pen_glvertexarrays_t, float_to_string( GL_EDGE_FLAG_ARRAY ) );
  declareStandardConstant( pen_gl_vertex_array_size_t, "pen.gl_vertex_array_size", pen_glvertexarrays_t, float_to_string( GL_VERTEX_ARRAY_SIZE ) );
  declareStandardConstant( pen_gl_vertex_array_type_t, "pen.gl_vertex_array_type", pen_glvertexarrays_t, float_to_string( GL_VERTEX_ARRAY_TYPE ) );
  declareStandardConstant( pen_gl_vertex_array_stride_t, "pen.gl_vertex_array_stride", pen_glvertexarrays_t, float_to_string( GL_VERTEX_ARRAY_STRIDE ) );
  declareStandardConstant( pen_gl_normal_array_type_t, "pen.gl_normal_array_type", pen_glvertexarrays_t, float_to_string( GL_NORMAL_ARRAY_TYPE ) );
  declareStandardConstant( pen_gl_normal_array_stride_t, "pen.gl_normal_array_stride", pen_glvertexarrays_t, float_to_string( GL_NORMAL_ARRAY_STRIDE ) );
  declareStandardConstant( pen_gl_color_array_size_t, "pen.gl_color_array_size", pen_glvertexarrays_t, float_to_string( GL_COLOR_ARRAY_SIZE ) );
  declareStandardConstant( pen_gl_color_array_type_t, "pen.gl_color_array_type", pen_glvertexarrays_t, float_to_string( GL_COLOR_ARRAY_TYPE ) );
  declareStandardConstant( pen_gl_color_array_stride_t, "pen.gl_color_array_stride", pen_glvertexarrays_t, float_to_string( GL_COLOR_ARRAY_STRIDE ) );
  declareStandardConstant( pen_gl_index_array_type_t, "pen.gl_index_array_type", pen_glvertexarrays_t, float_to_string( GL_INDEX_ARRAY_TYPE ) );
  declareStandardConstant( pen_gl_index_array_stride_t, "pen.gl_index_array_stride", pen_glvertexarrays_t, float_to_string( GL_INDEX_ARRAY_STRIDE ) );
  declareStandardConstant( pen_gl_texture_coord_array_size_t, "pen.gl_texture_coord_array_size", pen_glvertexarrays_t, float_to_string( GL_TEXTURE_COORD_ARRAY_SIZE ) );
  declareStandardConstant( pen_gl_texture_coord_array_type_t, "pen.gl_texture_coord_array_type", pen_glvertexarrays_t, float_to_string( GL_TEXTURE_COORD_ARRAY_TYPE ) );
  declareStandardConstant( pen_gl_texture_coord_array_stride_t, "pen.gl_texture_coord_array_stride", pen_glvertexarrays_t, float_to_string( GL_TEXTURE_COORD_ARRAY_STRIDE ) );
  declareStandardConstant( pen_gl_edge_flag_array_stride_t, "pen.gl_edge_flag_array_stride", pen_glvertexarrays_t, float_to_string( GL_EDGE_FLAG_ARRAY_STRIDE ) );
  declareStandardConstant( pen_gl_vertex_array_pointer_t, "pen.gl_vertex_array_pointer", pen_glvertexarrays_t, float_to_string( GL_VERTEX_ARRAY_POINTER ) );
  declareStandardConstant( pen_gl_normal_array_pointer_t, "pen.gl_normal_array_pointer", pen_glvertexarrays_t, float_to_string( GL_NORMAL_ARRAY_POINTER ) );
  declareStandardConstant( pen_gl_color_array_pointer_t, "pen.gl_color_array_pointer", pen_glvertexarrays_t, float_to_string( GL_COLOR_ARRAY_POINTER ) );
  declareStandardConstant( pen_gl_index_array_pointer_t, "pen.gl_index_array_pointer", pen_glvertexarrays_t, float_to_string( GL_INDEX_ARRAY_POINTER ) );
  declareStandardConstant( pen_gl_texture_coord_array_pointer_t, "pen.gl_texture_coord_array_pointer", pen_glvertexarrays_t, float_to_string( GL_TEXTURE_COORD_ARRAY_POINTER ) );
  declareStandardConstant( pen_gl_edge_flag_array_pointer_t, "pen.gl_edge_flag_array_pointer", pen_glvertexarrays_t, float_to_string( GL_EDGE_FLAG_ARRAY_POINTER ) );
  declareStandardConstant( pen_gl_v2f_t, "pen.gl_v2f", pen_glvertexarrays_t, float_to_string( GL_V2F ) );
  declareStandardConstant( pen_gl_v3f_t, "pen.gl_v3f", pen_glvertexarrays_t, float_to_string( GL_V3F ) );
  declareStandardConstant( pen_gl_c4ub_v2f_t, "pen.gl_c4ub_v2f", pen_glvertexarrays_t, float_to_string( GL_C4UB_V2F ) );
  declareStandardConstant( pen_gl_c4ub_v3f_t, "pen.gl_c4ub_v3f", pen_glvertexarrays_t, float_to_string( GL_C4UB_V3F ) );
  declareStandardConstant( pen_gl_c3f_v3f_t, "pen.gl_c3f_v3f", pen_glvertexarrays_t, float_to_string( GL_C3F_V3F ) );
  declareStandardConstant( pen_gl_n3f_v3f_t, "pen.gl_n3f_v3f", pen_glvertexarrays_t, float_to_string( GL_N3F_V3F ) );
  declareStandardConstant( pen_gl_c4f_n3f_v3f_t, "pen.gl_c4f_n3f_v3f", pen_glvertexarrays_t, float_to_string( GL_C4F_N3F_V3F ) );
  declareStandardConstant( pen_gl_t2f_v3f_t, "pen.gl_t2f_v3f", pen_glvertexarrays_t, float_to_string( GL_T2F_V3F ) );
  declareStandardConstant( pen_gl_t4f_v4f_t, "pen.gl_t4f_v4f", pen_glvertexarrays_t, float_to_string( GL_T4F_V4F ) );
  declareStandardConstant( pen_gl_t2f_c4ub_v3f_t, "pen.gl_t2f_c4ub_v3f", pen_glvertexarrays_t, float_to_string( GL_T2F_C4UB_V3F ) );
  declareStandardConstant( pen_gl_t2f_c3f_v3f_t, "pen.gl_t2f_c3f_v3f", pen_glvertexarrays_t, float_to_string( GL_T2F_C3F_V3F ) );
  declareStandardConstant( pen_gl_t2f_n3f_v3f_t, "pen.gl_t2f_n3f_v3f", pen_glvertexarrays_t, float_to_string( GL_T2F_N3F_V3F ) );
  declareStandardConstant( pen_gl_t2f_c4f_n3f_v3f_t, "pen.gl_t2f_c4f_n3f_v3f", pen_glvertexarrays_t, float_to_string( GL_T2F_C4F_N3F_V3F ) );
  declareStandardConstant( pen_gl_t4f_c4f_n3f_v4f_t, "pen.gl_t4f_c4f_n3f_v4f", pen_glvertexarrays_t, float_to_string( GL_T4F_C4F_N3F_V4F ) );

  declareIdent( pen_glmodes_t, "pen.glmodes", integer_t, typeClass  );

  declareStandardConstant( pen_gl_matrix_mode_t, "pen.gl_matrix_mode", pen_glmodes_t, float_to_string( GL_MATRIX_MODE ) );
  declareStandardConstant( pen_gl_modelview_t, "pen.gl_modelview", pen_glmodes_t, float_to_string( GL_MODELVIEW ) );
  declareStandardConstant( pen_gl_projection_t, "pen.gl_projection", pen_glmodes_t, float_to_string( GL_PROJECTION ) );
  declareStandardConstant( pen_gl_texture_t, "pen.gl_texture", pen_glmodes_t, float_to_string( GL_TEXTURE ) );

  declareIdent( pen_glpoints_t, "pen.glpoints", integer_t, typeClass  );

  declareStandardConstant( pen_gl_point_smooth_t, "pen.gl_point_smooth", pen_glpoints_t, float_to_string( GL_POINT_SMOOTH ) );
  declareStandardConstant( pen_gl_point_size_t, "pen.gl_point_size", pen_glpoints_t, float_to_string( GL_POINT_SIZE ) );
  declareStandardConstant( pen_gl_point_size_granularity_t, "pen.gl_point_size_granularity", pen_glpoints_t, float_to_string( GL_POINT_SIZE_GRANULARITY ) );
  declareStandardConstant( pen_gl_point_size_range_t, "pen.gl_point_size_range", pen_glpoints_t, float_to_string( GL_POINT_SIZE_RANGE ) );

  declareIdent( pen_gllines_t, "pen.gllines", integer_t, typeClass  );

  declareStandardConstant( pen_gl_line_smooth_t, "pen.gl_line_smooth", pen_gllines_t, float_to_string( GL_LINE_SMOOTH ) );
  declareStandardConstant( pen_gl_line_stipple_t, "pen.gl_line_stipple", pen_gllines_t, float_to_string( GL_LINE_STIPPLE ) );
  declareStandardConstant( pen_gl_line_stipple_pattern_t, "pen.gl_line_stipple_pattern", pen_gllines_t, float_to_string( GL_LINE_STIPPLE_PATTERN ) );
  declareStandardConstant( pen_gl_line_stipple_repeat_t, "pen.gl_line_stipple_repeat", pen_gllines_t, float_to_string( GL_LINE_STIPPLE_REPEAT ) );
  declareStandardConstant( pen_gl_line_width_t, "pen.gl_line_width", pen_gllines_t, float_to_string( GL_LINE_WIDTH ) );
  declareStandardConstant( pen_gl_line_width_granularity_t, "pen.gl_line_width_granularity", pen_gllines_t, float_to_string( GL_LINE_WIDTH_GRANULARITY ) );
  declareStandardConstant( pen_gl_line_width_range_t, "pen.gl_line_width_range", pen_gllines_t, float_to_string( GL_LINE_WIDTH_RANGE ) );

  declareIdent( pen_glpolygons_t, "pen.glpolygons", integer_t, typeClass  );

  declareStandardConstant( pen_gl_point_t, "pen.gl_point", pen_glpolygons_t, float_to_string( GL_POINT ) );
  declareStandardConstant( pen_gl_line_t, "pen.gl_line", pen_glpolygons_t, float_to_string( GL_LINE ) );
  declareStandardConstant( pen_gl_fill_t, "pen.gl_fill", pen_glpolygons_t, float_to_string( GL_FILL ) );
  declareStandardConstant( pen_gl_cw_t, "pen.gl_cw", pen_glpolygons_t, float_to_string( GL_CW ) );
  declareStandardConstant( pen_gl_ccw_t, "pen.gl_ccw", pen_glpolygons_t, float_to_string( GL_CCW ) );
  declareStandardConstant( pen_gl_front_t, "pen.gl_front", pen_glpolygons_t, float_to_string( GL_FRONT ) );
  declareStandardConstant( pen_gl_back_t, "pen.gl_back", pen_glpolygons_t, float_to_string( GL_BACK ) );
  declareStandardConstant( pen_gl_polygon_mode_t, "pen.gl_polygon_mode", pen_glpolygons_t, float_to_string( GL_POLYGON_MODE ) );
  declareStandardConstant( pen_gl_polygon_smooth_t, "pen.gl_polygon_smooth", pen_glpolygons_t, float_to_string( GL_POLYGON_SMOOTH ) );
  declareStandardConstant( pen_gl_polygon_stipple_t, "pen.gl_polygon_stipple", pen_glpolygons_t, float_to_string( GL_POLYGON_STIPPLE ) );
  declareStandardConstant( pen_gl_edge_flag_t, "pen.gl_edge_flag", pen_glpolygons_t, float_to_string( GL_EDGE_FLAG ) );
  declareStandardConstant( pen_gl_cull_face_t, "pen.gl_cull_face", pen_glpolygons_t, float_to_string( GL_CULL_FACE ) );
  declareStandardConstant( pen_gl_cull_face_mode_t, "pen.gl_cull_face_mode", pen_glpolygons_t, float_to_string( GL_CULL_FACE_MODE ) );
  declareStandardConstant( pen_gl_front_face_t, "pen.gl_front_face", pen_glpolygons_t, float_to_string( GL_FRONT_FACE ) );
  declareStandardConstant( pen_gl_polygon_offset_factor_t, "pen.gl_polygon_offset_factor", pen_glpolygons_t, float_to_string( GL_POLYGON_OFFSET_FACTOR ) );
  declareStandardConstant( pen_gl_polygon_offset_units_t, "pen.gl_polygon_offset_units", pen_glpolygons_t, float_to_string( GL_POLYGON_OFFSET_UNITS ) );
  declareStandardConstant( pen_gl_polygon_offset_point_t, "pen.gl_polygon_offset_point", pen_glpolygons_t, float_to_string( GL_POLYGON_OFFSET_POINT ) );
  declareStandardConstant( pen_gl_polygon_offset_line_t, "pen.gl_polygon_offset_line", pen_glpolygons_t, float_to_string( GL_POLYGON_OFFSET_LINE ) );
  declareStandardConstant( pen_gl_polygon_offset_fill_t, "pen.gl_polygon_offset_fill", pen_glpolygons_t, float_to_string( GL_POLYGON_OFFSET_FILL ) );

  declareIdent( pen_gldlists_t, "pen.gldlists", integer_t, typeClass  );

  declareStandardConstant( pen_gl_compile_t, "pen.gl_compile", pen_gldlists_t, float_to_string( GL_COMPILE ) );
  declareStandardConstant( pen_gl_compile_and_execute_t, "pen.gl_compile_and_execute", pen_gldlists_t, float_to_string( GL_COMPILE_AND_EXECUTE ) );
  declareStandardConstant( pen_gl_list_base_t, "pen.gl_list_base", pen_gldlists_t, float_to_string( GL_LIST_BASE ) );
  declareStandardConstant( pen_gl_list_index_t, "pen.gl_list_index", pen_gldlists_t, float_to_string( GL_LIST_INDEX ) );
  declareStandardConstant( pen_gl_list_mode_t, "pen.gl_list_mode", pen_gldlists_t, float_to_string( GL_LIST_MODE ) );

  declareIdent( pen_glalphacompare_t, "pen.glalphacompare", integer_t, typeClass  );

  declareStandardConstant( pen_gl_never_t, "pen.gl_never", pen_glalphacompare_t, float_to_string( GL_NEVER ) );
  declareStandardConstant( pen_gl_less_t, "pen.gl_less", pen_glalphacompare_t, float_to_string( GL_LESS ) );
  declareStandardConstant( pen_gl_equal_t, "pen.gl_equal", pen_glalphacompare_t, float_to_string( GL_EQUAL ) );
  declareStandardConstant( pen_gl_lequal_t, "pen.gl_lequal", pen_glalphacompare_t, float_to_string( GL_LEQUAL ) );
  declareStandardConstant( pen_gl_greater_t, "pen.gl_greater", pen_glalphacompare_t, float_to_string( GL_GREATER ) );
  declareStandardConstant( pen_gl_notequal_t, "pen.gl_notequal", pen_glalphacompare_t, float_to_string( GL_NOTEQUAL ) );
  declareStandardConstant( pen_gl_gequal_t, "pen.gl_gequal", pen_glalphacompare_t, float_to_string( GL_GEQUAL ) );
  declareStandardConstant( pen_gl_always_t, "pen.gl_always", pen_glalphacompare_t, float_to_string( GL_ALWAYS ) );

  declareIdent( pen_gldbuffer_t, "pen.gldbuffer", integer_t, typeClass  );

  declareStandardConstant( pen_gl_depth_test_t, "pen.gl_depth_test", pen_gldbuffer_t, float_to_string( GL_DEPTH_TEST ) );
  declareStandardConstant( pen_gl_depth_bits_t, "pen.gl_depth_bits", pen_gldbuffer_t, float_to_string( GL_DEPTH_BITS ) );
  declareStandardConstant( pen_gl_depth_clear_value_t, "pen.gl_depth_clear_value", pen_gldbuffer_t, float_to_string( GL_DEPTH_CLEAR_VALUE ) );
  declareStandardConstant( pen_gl_depth_func_t, "pen.gl_depth_func", pen_gldbuffer_t, float_to_string( GL_DEPTH_FUNC ) );
  declareStandardConstant( pen_gl_depth_range_t, "pen.gl_depth_range", pen_gldbuffer_t, float_to_string( GL_DEPTH_RANGE ) );
  declareStandardConstant( pen_gl_depth_writemask_t, "pen.gl_depth_writemask", pen_gldbuffer_t, float_to_string( GL_DEPTH_WRITEMASK ) );
  declareStandardConstant( pen_gl_depth_component_t, "pen.gl_depth_component", pen_gldbuffer_t, float_to_string( GL_DEPTH_COMPONENT ) );

  declareIdent( pen_gllighting_t, "pen.gllighting", integer_t, typeClass  );

  declareStandardConstant( pen_gl_lighting_t, "pen.gl_lighting", pen_gllighting_t, float_to_string( GL_LIGHTING ) );
  declareStandardConstant( pen_gl_light0_t, "pen.gl_light0", pen_gllighting_t, float_to_string( GL_LIGHT0 ) );
  declareStandardConstant( pen_gl_light1_t, "pen.gl_light1", pen_gllighting_t, float_to_string( GL_LIGHT1 ) );
  declareStandardConstant( pen_gl_light2_t, "pen.gl_light2", pen_gllighting_t, float_to_string( GL_LIGHT2 ) );
  declareStandardConstant( pen_gl_light3_t, "pen.gl_light3", pen_gllighting_t, float_to_string( GL_LIGHT3 ) );
  declareStandardConstant( pen_gl_light4_t, "pen.gl_light4", pen_gllighting_t, float_to_string( GL_LIGHT4 ) );
  declareStandardConstant( pen_gl_light5_t, "pen.gl_light5", pen_gllighting_t, float_to_string( GL_LIGHT5 ) );
  declareStandardConstant( pen_gl_light6_t, "pen.gl_light6", pen_gllighting_t, float_to_string( GL_LIGHT6 ) );
  declareStandardConstant( pen_gl_light7_t, "pen.gl_light7", pen_gllighting_t, float_to_string( GL_LIGHT7 ) );
  declareStandardConstant( pen_gl_spot_exponent_t, "pen.gl_spot_exponent", pen_gllighting_t, float_to_string( GL_SPOT_EXPONENT ) );
  declareStandardConstant( pen_gl_spot_cutoff_t, "pen.gl_spot_cutoff", pen_gllighting_t, float_to_string( GL_SPOT_CUTOFF ) );
  declareStandardConstant( pen_gl_constant_attenuation_t, "pen.gl_constant_attenuation", pen_gllighting_t, float_to_string( GL_CONSTANT_ATTENUATION ) );
  declareStandardConstant( pen_gl_linear_attenuation_t, "pen.gl_linear_attenuation", pen_gllighting_t, float_to_string( GL_LINEAR_ATTENUATION ) );
  declareStandardConstant( pen_gl_quadratic_attenuation_t, "pen.gl_quadratic_attenuation", pen_gllighting_t, float_to_string( GL_QUADRATIC_ATTENUATION ) );
  declareStandardConstant( pen_gl_ambient_t, "pen.gl_ambient", pen_gllighting_t, float_to_string( GL_AMBIENT ) );
  declareStandardConstant( pen_gl_diffuse_t, "pen.gl_diffuse", pen_gllighting_t, float_to_string( GL_DIFFUSE ) );
  declareStandardConstant( pen_gl_specular_t, "pen.gl_specular", pen_gllighting_t, float_to_string( GL_SPECULAR ) );
  declareStandardConstant( pen_gl_shininess_t, "pen.gl_shininess", pen_gllighting_t, float_to_string( GL_SHININESS ) );
  declareStandardConstant( pen_gl_emission_t, "pen.gl_emission", pen_gllighting_t, float_to_string( GL_EMISSION ) );
  declareStandardConstant( pen_gl_position_t, "pen.gl_position", pen_gllighting_t, float_to_string( GL_POSITION ) );
  declareStandardConstant( pen_gl_spot_direction_t, "pen.gl_spot_direction", pen_gllighting_t, float_to_string( GL_SPOT_DIRECTION ) );
  declareStandardConstant( pen_gl_ambient_and_diffuse_t, "pen.gl_ambient_and_diffuse", pen_gllighting_t, float_to_string( GL_AMBIENT_AND_DIFFUSE ) );
  declareStandardConstant( pen_gl_color_indexes_t, "pen.gl_color_indexes", pen_gllighting_t, float_to_string( GL_COLOR_INDEXES ) );
  declareStandardConstant( pen_gl_light_model_two_side_t, "pen.gl_light_model_two_side", pen_gllighting_t, float_to_string( GL_LIGHT_MODEL_TWO_SIDE ) );
  declareStandardConstant( pen_gl_light_model_local_viewer_t, "pen.gl_light_model_local_viewer", pen_gllighting_t, float_to_string( GL_LIGHT_MODEL_LOCAL_VIEWER ) );
  declareStandardConstant( pen_gl_light_model_ambient_t, "pen.gl_light_model_ambient", pen_gllighting_t, float_to_string( GL_LIGHT_MODEL_AMBIENT ) );
  declareStandardConstant( pen_gl_front_and_back_t, "pen.gl_front_and_back", pen_gllighting_t, float_to_string( GL_FRONT_AND_BACK ) );
  declareStandardConstant( pen_gl_shade_model_t, "pen.gl_shade_model", pen_gllighting_t, float_to_string( GL_SHADE_MODEL ) );
  declareStandardConstant( pen_gl_flat_t, "pen.gl_flat", pen_gllighting_t, float_to_string( GL_FLAT ) );
  declareStandardConstant( pen_gl_smooth_t, "pen.gl_smooth", pen_gllighting_t, float_to_string( GL_SMOOTH ) );
  declareStandardConstant( pen_gl_color_material_t, "pen.gl_color_material", pen_gllighting_t, float_to_string( GL_COLOR_MATERIAL ) );
  declareStandardConstant( pen_gl_color_material_face_t, "pen.gl_color_material_face", pen_gllighting_t, float_to_string( GL_COLOR_MATERIAL_FACE ) );
  declareStandardConstant( pen_gl_color_material_parameter_t, "pen.gl_color_material_parameter", pen_gllighting_t, float_to_string( GL_COLOR_MATERIAL_PARAMETER ) );
  declareStandardConstant( pen_gl_normalize_t, "pen.gl_normalize", pen_gllighting_t, float_to_string( GL_NORMALIZE ) );

  declareIdent( pen_glusercplane_t, "pen.glusercplane", integer_t, typeClass  );

  declareStandardConstant( pen_gl_clip_plane0_t, "pen.gl_clip_plane0", pen_glusercplane_t, float_to_string( GL_CLIP_PLANE0 ) );
  declareStandardConstant( pen_gl_clip_plane1_t, "pen.gl_clip_plane1", pen_glusercplane_t, float_to_string( GL_CLIP_PLANE1 ) );
  declareStandardConstant( pen_gl_clip_plane2_t, "pen.gl_clip_plane2", pen_glusercplane_t, float_to_string( GL_CLIP_PLANE2 ) );
  declareStandardConstant( pen_gl_clip_plane3_t, "pen.gl_clip_plane3", pen_glusercplane_t, float_to_string( GL_CLIP_PLANE3 ) );
  declareStandardConstant( pen_gl_clip_plane4_t, "pen.gl_clip_plane4", pen_glusercplane_t, float_to_string( GL_CLIP_PLANE4 ) );
  declareStandardConstant( pen_gl_clip_plane5_t, "pen.gl_clip_plane5", pen_glusercplane_t, float_to_string( GL_CLIP_PLANE5 ) );

  declareIdent( pen_glabuffer_t, "pen.glabuffer", integer_t, typeClass  );

  declareStandardConstant( pen_gl_accum_red_bits_t, "pen.gl_accum_red_bits", pen_glabuffer_t, float_to_string( GL_ACCUM_RED_BITS ) );
  declareStandardConstant( pen_gl_accum_green_bits_t, "pen.gl_accum_green_bits", pen_glabuffer_t, float_to_string( GL_ACCUM_GREEN_BITS ) );
  declareStandardConstant( pen_gl_accum_blue_bits_t, "pen.gl_accum_blue_bits", pen_glabuffer_t, float_to_string( GL_ACCUM_BLUE_BITS ) );
  declareStandardConstant( pen_gl_accum_alpha_bits_t, "pen.gl_accum_alpha_bits", pen_glabuffer_t, float_to_string( GL_ACCUM_ALPHA_BITS ) );
  declareStandardConstant( pen_gl_accum_clear_value_t, "pen.gl_accum_clear_value", pen_glabuffer_t, float_to_string( GL_ACCUM_CLEAR_VALUE ) );
  declareStandardConstant( pen_gl_accum_t, "pen.gl_accum", pen_glabuffer_t, float_to_string( GL_ACCUM ) );
  declareStandardConstant( pen_gl_add_t, "pen.gl_add", pen_glabuffer_t, float_to_string( GL_ADD ) );
  declareStandardConstant( pen_gl_load_t, "pen.gl_load", pen_glabuffer_t, float_to_string( GL_LOAD ) );
  declareStandardConstant( pen_gl_mult_t, "pen.gl_mult", pen_glabuffer_t, float_to_string( GL_MULT ) );
  declareStandardConstant( pen_gl_return_t, "pen.gl_return", pen_glabuffer_t, float_to_string( GL_RETURN ) );

  declareIdent( pen_glatesting_t, "pen.glatesting", integer_t, typeClass  );

  declareStandardConstant( pen_gl_alpha_test_t, "pen.gl_alpha_test", pen_glatesting_t, float_to_string( GL_ALPHA_TEST ) );
  declareStandardConstant( pen_gl_alpha_test_ref_t, "pen.gl_alpha_test_ref", pen_glatesting_t, float_to_string( GL_ALPHA_TEST_REF ) );
  declareStandardConstant( pen_gl_alpha_test_func_t, "pen.gl_alpha_test_func", pen_glatesting_t, float_to_string( GL_ALPHA_TEST_FUNC ) );

  declareIdent( pen_glblending_t, "pen.glblending", integer_t, typeClass  );

  declareStandardConstant( pen_gl_blend_t, "pen.gl_blend", pen_glblending_t, float_to_string( GL_BLEND ) );
  declareStandardConstant( pen_gl_blend_src_t, "pen.gl_blend_src", pen_glblending_t, float_to_string( GL_BLEND_SRC ) );
  declareStandardConstant( pen_gl_blend_dst_t, "pen.gl_blend_dst", pen_glblending_t, float_to_string( GL_BLEND_DST ) );
  declareStandardConstant( pen_gl_zero_t, "pen.gl_zero", pen_glblending_t, float_to_string( GL_ZERO ) );
  declareStandardConstant( pen_gl_one_t, "pen.gl_one", pen_glblending_t, float_to_string( GL_ONE ) );
  declareStandardConstant( pen_gl_src_color_t, "pen.gl_src_color", pen_glblending_t, float_to_string( GL_SRC_COLOR ) );
  declareStandardConstant( pen_gl_one_minus_src_color_t, "pen.gl_one_minus_src_color", pen_glblending_t, float_to_string( GL_ONE_MINUS_SRC_COLOR ) );
  declareStandardConstant( pen_gl_src_alpha_t, "pen.gl_src_alpha", pen_glblending_t, float_to_string( GL_SRC_ALPHA ) );
  declareStandardConstant( pen_gl_one_minus_src_alpha_t, "pen.gl_one_minus_src_alpha", pen_glblending_t, float_to_string( GL_ONE_MINUS_SRC_ALPHA ) );
  declareStandardConstant( pen_gl_dst_alpha_t, "pen.gl_dst_alpha", pen_glblending_t, float_to_string( GL_DST_ALPHA ) );
  declareStandardConstant( pen_gl_one_minus_dst_alpha_t, "pen.gl_one_minus_dst_alpha", pen_glblending_t, float_to_string( GL_ONE_MINUS_DST_ALPHA ) );
  declareStandardConstant( pen_gl_dst_color_t, "pen.gl_dst_color", pen_glblending_t, float_to_string( GL_DST_COLOR ) );
  declareStandardConstant( pen_gl_one_minus_dst_color_t, "pen.gl_one_minus_dst_color", pen_glblending_t, float_to_string( GL_ONE_MINUS_DST_COLOR ) );
  declareStandardConstant( pen_gl_src_alpha_saturate_t, "pen.gl_src_alpha_saturate", pen_glblending_t, float_to_string( GL_SRC_ALPHA_SATURATE ) );

  declareIdent( pen_glrendermodes_t, "pen.glrendermodes", integer_t, typeClass  );

  declareStandardConstant( pen_gl_feedback_t, "pen.gl_feedback", pen_glrendermodes_t, float_to_string( GL_FEEDBACK ) );
  declareStandardConstant( pen_gl_render_t, "pen.gl_render", pen_glrendermodes_t, float_to_string( GL_RENDER ) );
  declareStandardConstant( pen_gl_select_t, "pen.gl_select", pen_glrendermodes_t, float_to_string( GL_SELECT ) );

  declareIdent( pen_glfeedback_t, "pen.glfeedback", integer_t, typeClass  );

  declareStandardConstant( pen_gl_2d_t, "pen.gl_2d", pen_glfeedback_t, float_to_string( GL_2D ) );
  declareStandardConstant( pen_gl_3d_t, "pen.gl_3d", pen_glfeedback_t, float_to_string( GL_3D ) );
  declareStandardConstant( pen_gl_3d_color_t, "pen.gl_3d_color", pen_glfeedback_t, float_to_string( GL_3D_COLOR ) );
  declareStandardConstant( pen_gl_3d_color_texture_t, "pen.gl_3d_color_texture", pen_glfeedback_t, float_to_string( GL_3D_COLOR_TEXTURE ) );
  declareStandardConstant( pen_gl_4d_color_texture_t, "pen.gl_4d_color_texture", pen_glfeedback_t, float_to_string( GL_4D_COLOR_TEXTURE ) );
  declareStandardConstant( pen_gl_point_token_t, "pen.gl_point_token", pen_glfeedback_t, float_to_string( GL_POINT_TOKEN ) );
  declareStandardConstant( pen_gl_line_token_t, "pen.gl_line_token", pen_glfeedback_t, float_to_string( GL_LINE_TOKEN ) );
  declareStandardConstant( pen_gl_line_reset_token_t, "pen.gl_line_reset_token", pen_glfeedback_t, float_to_string( GL_LINE_RESET_TOKEN ) );
  declareStandardConstant( pen_gl_polygon_token_t, "pen.gl_polygon_token", pen_glfeedback_t, float_to_string( GL_POLYGON_TOKEN ) );
  declareStandardConstant( pen_gl_bitmap_token_t, "pen.gl_bitmap_token", pen_glfeedback_t, float_to_string( GL_BITMAP_TOKEN ) );
  declareStandardConstant( pen_gl_draw_pixel_token_t, "pen.gl_draw_pixel_token", pen_glfeedback_t, float_to_string( GL_DRAW_PIXEL_TOKEN ) );
  declareStandardConstant( pen_gl_copy_pixel_token_t, "pen.gl_copy_pixel_token", pen_glfeedback_t, float_to_string( GL_COPY_PIXEL_TOKEN ) );
  declareStandardConstant( pen_gl_pass_through_token_t, "pen.gl_pass_through_token", pen_glfeedback_t, float_to_string( GL_PASS_THROUGH_TOKEN ) );
  declareStandardConstant( pen_gl_feedback_buffer_pointer_t, "pen.gl_feedback_buffer_pointer", pen_glfeedback_t, float_to_string( GL_FEEDBACK_BUFFER_POINTER ) );
  declareStandardConstant( pen_gl_feedback_buffer_size_t, "pen.gl_feedback_buffer_size", pen_glfeedback_t, float_to_string( GL_FEEDBACK_BUFFER_SIZE ) );
  declareStandardConstant( pen_gl_feedback_buffer_type_t, "pen.gl_feedback_buffer_type", pen_glfeedback_t, float_to_string( GL_FEEDBACK_BUFFER_TYPE ) );

  declareIdent( pen_glselection_t, "pen.glselection", integer_t, typeClass  );

  declareStandardConstant( pen_gl_selection_buffer_pointer_t, "pen.gl_selection_buffer_pointer", pen_glselection_t, float_to_string( GL_SELECTION_BUFFER_POINTER ) );
  declareStandardConstant( pen_gl_selection_buffer_size_t, "pen.gl_selection_buffer_size", pen_glselection_t, float_to_string( GL_SELECTION_BUFFER_SIZE ) );

  declareIdent( pen_glfog_t, "pen.glfog", integer_t, typeClass  );

  declareStandardConstant( pen_gl_fog_t, "pen.gl_fog", pen_glfog_t, float_to_string( GL_FOG ) );
  declareStandardConstant( pen_gl_fog_mode_t, "pen.gl_fog_mode", pen_glfog_t, float_to_string( GL_FOG_MODE ) );
  declareStandardConstant( pen_gl_fog_density_t, "pen.gl_fog_density", pen_glfog_t, float_to_string( GL_FOG_DENSITY ) );
  declareStandardConstant( pen_gl_fog_color_t, "pen.gl_fog_color", pen_glfog_t, float_to_string( GL_FOG_COLOR ) );
  declareStandardConstant( pen_gl_fog_index_t, "pen.gl_fog_index", pen_glfog_t, float_to_string( GL_FOG_INDEX ) );
  declareStandardConstant( pen_gl_fog_start_t, "pen.gl_fog_start", pen_glfog_t, float_to_string( GL_FOG_START ) );
  declareStandardConstant( pen_gl_fog_end_t, "pen.gl_fog_end", pen_glfog_t, float_to_string( GL_FOG_END ) );
  declareStandardConstant( pen_gl_linear_t, "pen.gl_linear", pen_glfog_t, float_to_string( GL_LINEAR ) );
  declareStandardConstant( pen_gl_exp_t, "pen.gl_exp", pen_glfog_t, float_to_string( GL_EXP ) );
  declareStandardConstant( pen_gl_exp2_t, "pen.gl_exp2", pen_glfog_t, float_to_string( GL_EXP2 ) );

  declareIdent( pen_gllogicops_t, "pen.gllogicops", integer_t, typeClass  );

  declareStandardConstant( pen_gl_logic_op_t, "pen.gl_logic_op", pen_gllogicops_t, float_to_string( GL_LOGIC_OP ) );
  declareStandardConstant( pen_gl_index_logic_op_t, "pen.gl_index_logic_op", pen_gllogicops_t, float_to_string( GL_INDEX_LOGIC_OP ) );
  declareStandardConstant( pen_gl_color_logic_op_t, "pen.gl_color_logic_op", pen_gllogicops_t, float_to_string( GL_COLOR_LOGIC_OP ) );
  declareStandardConstant( pen_gl_logic_op_mode_t, "pen.gl_logic_op_mode", pen_gllogicops_t, float_to_string( GL_LOGIC_OP_MODE ) );
  declareStandardConstant( pen_gl_clear_t, "pen.gl_clear", pen_gllogicops_t, float_to_string( GL_CLEAR ) );
  declareStandardConstant( pen_gl_set_t, "pen.gl_set", pen_gllogicops_t, float_to_string( GL_SET ) );
  declareStandardConstant( pen_gl_copy_t, "pen.gl_copy", pen_gllogicops_t, float_to_string( GL_COPY ) );
  declareStandardConstant( pen_gl_copy_inverted_t, "pen.gl_copy_inverted", pen_gllogicops_t, float_to_string( GL_COPY_INVERTED ) );
  declareStandardConstant( pen_gl_noop_t, "pen.gl_noop", pen_gllogicops_t, float_to_string( GL_NOOP ) );
  declareStandardConstant( pen_gl_invert_t, "pen.gl_invert", pen_gllogicops_t, float_to_string( GL_INVERT ) );
  declareStandardConstant( pen_gl_and_t, "pen.gl_and", pen_gllogicops_t, float_to_string( GL_AND ) );
  declareStandardConstant( pen_gl_nand_t, "pen.gl_nand", pen_gllogicops_t, float_to_string( GL_NAND ) );
  declareStandardConstant( pen_gl_or_t, "pen.gl_or", pen_gllogicops_t, float_to_string( GL_OR ) );
  declareStandardConstant( pen_gl_nor_t, "pen.gl_nor", pen_gllogicops_t, float_to_string( GL_NOR ) );
  declareStandardConstant( pen_gl_xor_t, "pen.gl_xor", pen_gllogicops_t, float_to_string( GL_XOR ) );
  declareStandardConstant( pen_gl_equiv_t, "pen.gl_equiv", pen_gllogicops_t, float_to_string( GL_EQUIV ) );
  declareStandardConstant( pen_gl_and_reverse_t, "pen.gl_and_reverse", pen_gllogicops_t, float_to_string( GL_AND_REVERSE ) );
  declareStandardConstant( pen_gl_and_inverted_t, "pen.gl_and_inverted", pen_gllogicops_t, float_to_string( GL_AND_INVERTED ) );
  declareStandardConstant( pen_gl_or_reverse_t, "pen.gl_or_reverse", pen_gllogicops_t, float_to_string( GL_OR_REVERSE ) );
  declareStandardConstant( pen_gl_or_inverted_t, "pen.gl_or_inverted", pen_gllogicops_t, float_to_string( GL_OR_INVERTED ) );

  declareIdent( pen_glstencil_t, "pen.glstencil", integer_t, typeClass  );

  declareStandardConstant( pen_gl_stencil_bits_t, "pen.gl_stencil_bits", pen_glstencil_t, float_to_string( GL_STENCIL_BITS ) );
  declareStandardConstant( pen_gl_stencil_test_t, "pen.gl_stencil_test", pen_glstencil_t, float_to_string( GL_STENCIL_TEST ) );
  declareStandardConstant( pen_gl_stencil_clear_value_t, "pen.gl_stencil_clear_value", pen_glstencil_t, float_to_string( GL_STENCIL_CLEAR_VALUE ) );
  declareStandardConstant( pen_gl_stencil_func_t, "pen.gl_stencil_func", pen_glstencil_t, float_to_string( GL_STENCIL_FUNC ) );
  declareStandardConstant( pen_gl_stencil_value_mask_t, "pen.gl_stencil_value_mask", pen_glstencil_t, float_to_string( GL_STENCIL_VALUE_MASK ) );
  declareStandardConstant( pen_gl_stencil_fail_t, "pen.gl_stencil_fail", pen_glstencil_t, float_to_string( GL_STENCIL_FAIL ) );
  declareStandardConstant( pen_gl_stencil_pass_depth_fail_t, "pen.gl_stencil_pass_depth_fail", pen_glstencil_t, float_to_string( GL_STENCIL_PASS_DEPTH_FAIL ) );
  declareStandardConstant( pen_gl_stencil_pass_depth_pass_t, "pen.gl_stencil_pass_depth_pass", pen_glstencil_t, float_to_string( GL_STENCIL_PASS_DEPTH_PASS ) );
  declareStandardConstant( pen_gl_stencil_ref_t, "pen.gl_stencil_ref", pen_glstencil_t, float_to_string( GL_STENCIL_REF ) );
  declareStandardConstant( pen_gl_stencil_writemask_t, "pen.gl_stencil_writemask", pen_glstencil_t, float_to_string( GL_STENCIL_WRITEMASK ) );
  declareStandardConstant( pen_gl_stencil_index_t, "pen.gl_stencil_index", pen_glstencil_t, float_to_string( GL_STENCIL_INDEX ) );
  declareStandardConstant( pen_gl_keep_t, "pen.gl_keep", pen_glstencil_t, float_to_string( GL_KEEP ) );
  declareStandardConstant( pen_gl_replace_t, "pen.gl_replace", pen_glstencil_t, float_to_string( GL_REPLACE ) );
  declareStandardConstant( pen_gl_incr_t, "pen.gl_incr", pen_glstencil_t, float_to_string( GL_INCR ) );
  declareStandardConstant( pen_gl_decr_t, "pen.gl_decr", pen_glstencil_t, float_to_string( GL_DECR ) );

  declareIdent( pen_glbuffers_t, "pen.glbuffers", integer_t, typeClass  );

  declareStandardConstant( pen_gl_none_t, "pen.gl_none", pen_glbuffers_t, float_to_string( GL_NONE ) );
  declareStandardConstant( pen_gl_left_t, "pen.gl_left", pen_glbuffers_t, float_to_string( GL_LEFT ) );
  declareStandardConstant( pen_gl_right_t, "pen.gl_right", pen_glbuffers_t, float_to_string( GL_RIGHT ) );
  declareStandardConstant( pen_gl_front_left_t, "pen.gl_front_left", pen_glbuffers_t, float_to_string( GL_FRONT_LEFT ) );
  declareStandardConstant( pen_gl_front_right_t, "pen.gl_front_right", pen_glbuffers_t, float_to_string( GL_FRONT_RIGHT ) );
  declareStandardConstant( pen_gl_back_left_t, "pen.gl_back_left", pen_glbuffers_t, float_to_string( GL_BACK_LEFT ) );
  declareStandardConstant( pen_gl_back_right_t, "pen.gl_back_right", pen_glbuffers_t, float_to_string( GL_BACK_RIGHT ) );
  declareStandardConstant( pen_gl_aux0_t, "pen.gl_aux0", pen_glbuffers_t, float_to_string( GL_AUX0 ) );
  declareStandardConstant( pen_gl_aux1_t, "pen.gl_aux1", pen_glbuffers_t, float_to_string( GL_AUX1 ) );
  declareStandardConstant( pen_gl_aux2_t, "pen.gl_aux2", pen_glbuffers_t, float_to_string( GL_AUX2 ) );
  declareStandardConstant( pen_gl_aux3_t, "pen.gl_aux3", pen_glbuffers_t, float_to_string( GL_AUX3 ) );
  declareStandardConstant( pen_gl_color_index_t, "pen.gl_color_index", pen_glbuffers_t, float_to_string( GL_COLOR_INDEX ) );
  declareStandardConstant( pen_gl_red_t, "pen.gl_red", pen_glbuffers_t, float_to_string( GL_RED ) );
  declareStandardConstant( pen_gl_green_t, "pen.gl_green", pen_glbuffers_t, float_to_string( GL_GREEN ) );
  declareStandardConstant( pen_gl_blue_t, "pen.gl_blue", pen_glbuffers_t, float_to_string( GL_BLUE ) );
  declareStandardConstant( pen_gl_alpha_t, "pen.gl_alpha", pen_glbuffers_t, float_to_string( GL_ALPHA ) );
  declareStandardConstant( pen_gl_luminance_t, "pen.gl_luminance", pen_glbuffers_t, float_to_string( GL_LUMINANCE ) );
  declareStandardConstant( pen_gl_luminance_alpha_t, "pen.gl_luminance_alpha", pen_glbuffers_t, float_to_string( GL_LUMINANCE_ALPHA ) );
  declareStandardConstant( pen_gl_alpha_bits_t, "pen.gl_alpha_bits", pen_glbuffers_t, float_to_string( GL_ALPHA_BITS ) );
  declareStandardConstant( pen_gl_red_bits_t, "pen.gl_red_bits", pen_glbuffers_t, float_to_string( GL_RED_BITS ) );
  declareStandardConstant( pen_gl_green_bits_t, "pen.gl_green_bits", pen_glbuffers_t, float_to_string( GL_GREEN_BITS ) );
  declareStandardConstant( pen_gl_blue_bits_t, "pen.gl_blue_bits", pen_glbuffers_t, float_to_string( GL_BLUE_BITS ) );
  declareStandardConstant( pen_gl_index_bits_t, "pen.gl_index_bits", pen_glbuffers_t, float_to_string( GL_INDEX_BITS ) );
  declareStandardConstant( pen_gl_subpixel_bits_t, "pen.gl_subpixel_bits", pen_glbuffers_t, float_to_string( GL_SUBPIXEL_BITS ) );
  declareStandardConstant( pen_gl_aux_buffers_t, "pen.gl_aux_buffers", pen_glbuffers_t, float_to_string( GL_AUX_BUFFERS ) );
  declareStandardConstant( pen_gl_read_buffer_t, "pen.gl_read_buffer", pen_glbuffers_t, float_to_string( GL_READ_BUFFER ) );
  declareStandardConstant( pen_gl_draw_buffer_t, "pen.gl_draw_buffer", pen_glbuffers_t, float_to_string( GL_DRAW_BUFFER ) );
  declareStandardConstant( pen_gl_doublebuffer_t, "pen.gl_doublebuffer", pen_glbuffers_t, float_to_string( GL_DOUBLEBUFFER ) );
  declareStandardConstant( pen_gl_stereo_t, "pen.gl_stereo", pen_glbuffers_t, float_to_string( GL_STEREO ) );
  declareStandardConstant( pen_gl_bitmap_t, "pen.gl_bitmap", pen_glbuffers_t, float_to_string( GL_BITMAP ) );
  declareStandardConstant( pen_gl_color_t, "pen.gl_color", pen_glbuffers_t, float_to_string( GL_COLOR ) );
  declareStandardConstant( pen_gl_depth_t, "pen.gl_depth", pen_glbuffers_t, float_to_string( GL_DEPTH ) );
  declareStandardConstant( pen_gl_stencil_t, "pen.gl_stencil", pen_glbuffers_t, float_to_string( GL_STENCIL ) );
  declareStandardConstant( pen_gl_dither_t, "pen.gl_dither", pen_glbuffers_t, float_to_string( GL_DITHER ) );
  declareStandardConstant( pen_gl_rgb_t, "pen.gl_rgb", pen_glbuffers_t, float_to_string( GL_RGB ) );
  declareStandardConstant( pen_gl_rgba_t, "pen.gl_rgba", pen_glbuffers_t, float_to_string( GL_RGBA ) );

  declareIdent( pen_gllimits_t, "pen.gllimits", integer_t, typeClass  );

  declareStandardConstant( pen_gl_max_list_nesting_t, "pen.gl_max_list_nesting", pen_gllimits_t, float_to_string( GL_MAX_LIST_NESTING ) );
  declareStandardConstant( pen_gl_max_eval_order_t, "pen.gl_max_eval_order", pen_gllimits_t, float_to_string( GL_MAX_EVAL_ORDER ) );
  declareStandardConstant( pen_gl_max_lights_t, "pen.gl_max_lights", pen_gllimits_t, float_to_string( GL_MAX_LIGHTS ) );
  declareStandardConstant( pen_gl_max_clip_planes_t, "pen.gl_max_clip_planes", pen_gllimits_t, float_to_string( GL_MAX_CLIP_PLANES ) );
  declareStandardConstant( pen_gl_max_texture_size_t, "pen.gl_max_texture_size", pen_gllimits_t, float_to_string( GL_MAX_TEXTURE_SIZE ) );
  declareStandardConstant( pen_gl_max_pixel_map_table_t, "pen.gl_max_pixel_map_table", pen_gllimits_t, float_to_string( GL_MAX_PIXEL_MAP_TABLE ) );
  declareStandardConstant( pen_gl_max_attrib_stack_depth_t, "pen.gl_max_attrib_stack_depth", pen_gllimits_t, float_to_string( GL_MAX_ATTRIB_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_max_modelview_stack_depth_t, "pen.gl_max_modelview_stack_depth", pen_gllimits_t, float_to_string( GL_MAX_MODELVIEW_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_max_name_stack_depth_t, "pen.gl_max_name_stack_depth", pen_gllimits_t, float_to_string( GL_MAX_NAME_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_max_projection_stack_depth_t, "pen.gl_max_projection_stack_depth", pen_gllimits_t, float_to_string( GL_MAX_PROJECTION_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_max_texture_stack_depth_t, "pen.gl_max_texture_stack_depth", pen_gllimits_t, float_to_string( GL_MAX_TEXTURE_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_max_viewport_dims_t, "pen.gl_max_viewport_dims", pen_gllimits_t, float_to_string( GL_MAX_VIEWPORT_DIMS ) );
  declareStandardConstant( pen_gl_max_client_attrib_stack_depth_t, "pen.gl_max_client_attrib_stack_depth", pen_gllimits_t, float_to_string( GL_MAX_CLIENT_ATTRIB_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_attrib_stack_depth_t, "pen.gl_attrib_stack_depth", pen_gllimits_t, float_to_string( GL_ATTRIB_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_client_attrib_stack_depth_t, "pen.gl_client_attrib_stack_depth", pen_gllimits_t, float_to_string( GL_CLIENT_ATTRIB_STACK_DEPTH ) );

  declareIdent( pen_glgets_t, "pen.glgets", integer_t, typeClass  );

  declareStandardConstant( pen_gl_color_clear_value_t, "pen.gl_color_clear_value", pen_glgets_t, float_to_string( GL_COLOR_CLEAR_VALUE ) );
  declareStandardConstant( pen_gl_color_writemask_t, "pen.gl_color_writemask", pen_glgets_t, float_to_string( GL_COLOR_WRITEMASK ) );
  declareStandardConstant( pen_gl_current_index_t, "pen.gl_current_index", pen_glgets_t, float_to_string( GL_CURRENT_INDEX ) );
  declareStandardConstant( pen_gl_current_color_t, "pen.gl_current_color", pen_glgets_t, float_to_string( GL_CURRENT_COLOR ) );
  declareStandardConstant( pen_gl_current_normal_t, "pen.gl_current_normal", pen_glgets_t, float_to_string( GL_CURRENT_NORMAL ) );
  declareStandardConstant( pen_gl_current_raster_color_t, "pen.gl_current_raster_color", pen_glgets_t, float_to_string( GL_CURRENT_RASTER_COLOR ) );
  declareStandardConstant( pen_gl_current_raster_distance_t, "pen.gl_current_raster_distance", pen_glgets_t, float_to_string( GL_CURRENT_RASTER_DISTANCE ) );
  declareStandardConstant( pen_gl_current_raster_index_t, "pen.gl_current_raster_index", pen_glgets_t, float_to_string( GL_CURRENT_RASTER_INDEX ) );
  declareStandardConstant( pen_gl_current_raster_position_t, "pen.gl_current_raster_position", pen_glgets_t, float_to_string( GL_CURRENT_RASTER_POSITION ) );
  declareStandardConstant( pen_gl_current_raster_texture_coords_t, "pen.gl_current_raster_texture_coords", pen_glgets_t, float_to_string( GL_CURRENT_RASTER_TEXTURE_COORDS ) );
  declareStandardConstant( pen_gl_current_raster_position_valid_t, "pen.gl_current_raster_position_valid", pen_glgets_t, float_to_string( GL_CURRENT_RASTER_POSITION_VALID ) );
  declareStandardConstant( pen_gl_current_texture_coords_t, "pen.gl_current_texture_coords", pen_glgets_t, float_to_string( GL_CURRENT_TEXTURE_COORDS ) );
  declareStandardConstant( pen_gl_index_clear_value_t, "pen.gl_index_clear_value", pen_glgets_t, float_to_string( GL_INDEX_CLEAR_VALUE ) );
  declareStandardConstant( pen_gl_index_mode_t, "pen.gl_index_mode", pen_glgets_t, float_to_string( GL_INDEX_MODE ) );
  declareStandardConstant( pen_gl_index_writemask_t, "pen.gl_index_writemask", pen_glgets_t, float_to_string( GL_INDEX_WRITEMASK ) );
  declareStandardConstant( pen_gl_modelview_matrix_t, "pen.gl_modelview_matrix", pen_glgets_t, float_to_string( GL_MODELVIEW_MATRIX ) );
  declareStandardConstant( pen_gl_modelview_stack_depth_t, "pen.gl_modelview_stack_depth", pen_glgets_t, float_to_string( GL_MODELVIEW_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_name_stack_depth_t, "pen.gl_name_stack_depth", pen_glgets_t, float_to_string( GL_NAME_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_projection_matrix_t, "pen.gl_projection_matrix", pen_glgets_t, float_to_string( GL_PROJECTION_MATRIX ) );
  declareStandardConstant( pen_gl_projection_stack_depth_t, "pen.gl_projection_stack_depth", pen_glgets_t, float_to_string( GL_PROJECTION_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_render_mode_t, "pen.gl_render_mode", pen_glgets_t, float_to_string( GL_RENDER_MODE ) );
  declareStandardConstant( pen_gl_rgba_mode_t, "pen.gl_rgba_mode", pen_glgets_t, float_to_string( GL_RGBA_MODE ) );
  declareStandardConstant( pen_gl_texture_matrix_t, "pen.gl_texture_matrix", pen_glgets_t, float_to_string( GL_TEXTURE_MATRIX ) );
  declareStandardConstant( pen_gl_texture_stack_depth_t, "pen.gl_texture_stack_depth", pen_glgets_t, float_to_string( GL_TEXTURE_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_viewport_t, "pen.gl_viewport", pen_glgets_t, float_to_string( GL_VIEWPORT ) );

  declareIdent( pen_glevaluators_t, "pen.glevaluators", integer_t, typeClass  );

  declareStandardConstant( pen_gl_auto_normal_t, "pen.gl_auto_normal", pen_glevaluators_t, float_to_string( GL_AUTO_NORMAL ) );
  declareStandardConstant( pen_gl_map1_color_4_t, "pen.gl_map1_color_4", pen_glevaluators_t, float_to_string( GL_MAP1_COLOR_4 ) );
  declareStandardConstant( pen_gl_map1_index_t, "pen.gl_map1_index", pen_glevaluators_t, float_to_string( GL_MAP1_INDEX ) );
  declareStandardConstant( pen_gl_map1_normal_t, "pen.gl_map1_normal", pen_glevaluators_t, float_to_string( GL_MAP1_NORMAL ) );
  declareStandardConstant( pen_gl_map1_texture_coord_1_t, "pen.gl_map1_texture_coord_1", pen_glevaluators_t, float_to_string( GL_MAP1_TEXTURE_COORD_1 ) );
  declareStandardConstant( pen_gl_map1_texture_coord_2_t, "pen.gl_map1_texture_coord_2", pen_glevaluators_t, float_to_string( GL_MAP1_TEXTURE_COORD_2 ) );
  declareStandardConstant( pen_gl_map1_texture_coord_3_t, "pen.gl_map1_texture_coord_3", pen_glevaluators_t, float_to_string( GL_MAP1_TEXTURE_COORD_3 ) );
  declareStandardConstant( pen_gl_map1_texture_coord_4_t, "pen.gl_map1_texture_coord_4", pen_glevaluators_t, float_to_string( GL_MAP1_TEXTURE_COORD_4 ) );
  declareStandardConstant( pen_gl_map1_vertex_3_t, "pen.gl_map1_vertex_3", pen_glevaluators_t, float_to_string( GL_MAP1_VERTEX_3 ) );
  declareStandardConstant( pen_gl_map1_vertex_4_t, "pen.gl_map1_vertex_4", pen_glevaluators_t, float_to_string( GL_MAP1_VERTEX_4 ) );
  declareStandardConstant( pen_gl_map2_color_4_t, "pen.gl_map2_color_4", pen_glevaluators_t, float_to_string( GL_MAP2_COLOR_4 ) );
  declareStandardConstant( pen_gl_map2_index_t, "pen.gl_map2_index", pen_glevaluators_t, float_to_string( GL_MAP2_INDEX ) );
  declareStandardConstant( pen_gl_map2_normal_t, "pen.gl_map2_normal", pen_glevaluators_t, float_to_string( GL_MAP2_NORMAL ) );
  declareStandardConstant( pen_gl_map2_texture_coord_1_t, "pen.gl_map2_texture_coord_1", pen_glevaluators_t, float_to_string( GL_MAP2_TEXTURE_COORD_1 ) );
  declareStandardConstant( pen_gl_map2_texture_coord_2_t, "pen.gl_map2_texture_coord_2", pen_glevaluators_t, float_to_string( GL_MAP2_TEXTURE_COORD_2 ) );
  declareStandardConstant( pen_gl_map2_texture_coord_3_t, "pen.gl_map2_texture_coord_3", pen_glevaluators_t, float_to_string( GL_MAP2_TEXTURE_COORD_3 ) );
  declareStandardConstant( pen_gl_map2_texture_coord_4_t, "pen.gl_map2_texture_coord_4", pen_glevaluators_t, float_to_string( GL_MAP2_TEXTURE_COORD_4 ) );
  declareStandardConstant( pen_gl_map2_vertex_3_t, "pen.gl_map2_vertex_3", pen_glevaluators_t, float_to_string( GL_MAP2_VERTEX_3 ) );
  declareStandardConstant( pen_gl_map2_vertex_4_t, "pen.gl_map2_vertex_4", pen_glevaluators_t, float_to_string( GL_MAP2_VERTEX_4 ) );
  declareStandardConstant( pen_gl_map1_grid_domain_t, "pen.gl_map1_grid_domain", pen_glevaluators_t, float_to_string( GL_MAP1_GRID_DOMAIN ) );
  declareStandardConstant( pen_gl_map1_grid_segments_t, "pen.gl_map1_grid_segments", pen_glevaluators_t, float_to_string( GL_MAP1_GRID_SEGMENTS ) );
  declareStandardConstant( pen_gl_map2_grid_domain_t, "pen.gl_map2_grid_domain", pen_glevaluators_t, float_to_string( GL_MAP2_GRID_DOMAIN ) );
  declareStandardConstant( pen_gl_map2_grid_segments_t, "pen.gl_map2_grid_segments", pen_glevaluators_t, float_to_string( GL_MAP2_GRID_SEGMENTS ) );
  declareStandardConstant( pen_gl_coeff_t, "pen.gl_coeff", pen_glevaluators_t, float_to_string( GL_COEFF ) );
  declareStandardConstant( pen_gl_order_t, "pen.gl_order", pen_glevaluators_t, float_to_string( GL_ORDER ) );
  declareStandardConstant( pen_gl_domain_t, "pen.gl_domain", pen_glevaluators_t, float_to_string( GL_DOMAIN ) );

  declareIdent( pen_glhints_t, "pen.glhints", integer_t, typeClass  );

  declareStandardConstant( pen_gl_perspective_correction_hint_t, "pen.gl_perspective_correction_hint", pen_glhints_t, float_to_string( GL_PERSPECTIVE_CORRECTION_HINT ) );
  declareStandardConstant( pen_gl_point_smooth_hint_t, "pen.gl_point_smooth_hint", pen_glhints_t, float_to_string( GL_POINT_SMOOTH_HINT ) );
  declareStandardConstant( pen_gl_line_smooth_hint_t, "pen.gl_line_smooth_hint", pen_glhints_t, float_to_string( GL_LINE_SMOOTH_HINT ) );
  declareStandardConstant( pen_gl_polygon_smooth_hint_t, "pen.gl_polygon_smooth_hint", pen_glhints_t, float_to_string( GL_POLYGON_SMOOTH_HINT ) );
  declareStandardConstant( pen_gl_fog_hint_t, "pen.gl_fog_hint", pen_glhints_t, float_to_string( GL_FOG_HINT ) );

  declareIdent( pen_glhintmodes_t, "pen.glhintmodes", integer_t, typeClass  );

  declareStandardConstant( pen_gl_dont_care_t, "pen.gl_dont_care", pen_glhintmodes_t, float_to_string( GL_DONT_CARE ) );
  declareStandardConstant( pen_gl_fastest_t, "pen.gl_fastest", pen_glhintmodes_t, float_to_string( GL_FASTEST ) );
  declareStandardConstant( pen_gl_nicest_t, "pen.gl_nicest", pen_glhintmodes_t, float_to_string( GL_NICEST ) );

  declareIdent( pen_glscissorbox_t, "pen.glscissorbox", integer_t, typeClass  );

  declareStandardConstant( pen_gl_scissor_box_t, "pen.gl_scissor_box", pen_glscissorbox_t, float_to_string( GL_SCISSOR_BOX ) );
  declareStandardConstant( pen_gl_scissor_test_t, "pen.gl_scissor_test", pen_glscissorbox_t, float_to_string( GL_SCISSOR_TEST ) );

  declareIdent( pen_glpixelmode_t, "pen.glpixelmode", integer_t, typeClass  );

  declareStandardConstant( pen_gl_map_color_t, "pen.gl_map_color", pen_glpixelmode_t, float_to_string( GL_MAP_COLOR ) );
  declareStandardConstant( pen_gl_map_stencil_t, "pen.gl_map_stencil", pen_glpixelmode_t, float_to_string( GL_MAP_STENCIL ) );
  declareStandardConstant( pen_gl_index_shift_t, "pen.gl_index_shift", pen_glpixelmode_t, float_to_string( GL_INDEX_SHIFT ) );
  declareStandardConstant( pen_gl_index_offset_t, "pen.gl_index_offset", pen_glpixelmode_t, float_to_string( GL_INDEX_OFFSET ) );
  declareStandardConstant( pen_gl_red_scale_t, "pen.gl_red_scale", pen_glpixelmode_t, float_to_string( GL_RED_SCALE ) );
  declareStandardConstant( pen_gl_red_bias_t, "pen.gl_red_bias", pen_glpixelmode_t, float_to_string( GL_RED_BIAS ) );
  declareStandardConstant( pen_gl_green_scale_t, "pen.gl_green_scale", pen_glpixelmode_t, float_to_string( GL_GREEN_SCALE ) );
  declareStandardConstant( pen_gl_green_bias_t, "pen.gl_green_bias", pen_glpixelmode_t, float_to_string( GL_GREEN_BIAS ) );
  declareStandardConstant( pen_gl_blue_scale_t, "pen.gl_blue_scale", pen_glpixelmode_t, float_to_string( GL_BLUE_SCALE ) );
  declareStandardConstant( pen_gl_blue_bias_t, "pen.gl_blue_bias", pen_glpixelmode_t, float_to_string( GL_BLUE_BIAS ) );
  declareStandardConstant( pen_gl_alpha_scale_t, "pen.gl_alpha_scale", pen_glpixelmode_t, float_to_string( GL_ALPHA_SCALE ) );
  declareStandardConstant( pen_gl_alpha_bias_t, "pen.gl_alpha_bias", pen_glpixelmode_t, float_to_string( GL_ALPHA_BIAS ) );
  declareStandardConstant( pen_gl_depth_scale_t, "pen.gl_depth_scale", pen_glpixelmode_t, float_to_string( GL_DEPTH_SCALE ) );
  declareStandardConstant( pen_gl_depth_bias_t, "pen.gl_depth_bias", pen_glpixelmode_t, float_to_string( GL_DEPTH_BIAS ) );
  declareStandardConstant( pen_gl_pixel_map_s_to_s_size_t, "pen.gl_pixel_map_s_to_s_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_S_TO_S_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_i_size_t, "pen.gl_pixel_map_i_to_i_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_I_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_r_size_t, "pen.gl_pixel_map_i_to_r_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_R_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_g_size_t, "pen.gl_pixel_map_i_to_g_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_G_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_b_size_t, "pen.gl_pixel_map_i_to_b_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_B_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_a_size_t, "pen.gl_pixel_map_i_to_a_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_A_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_r_to_r_size_t, "pen.gl_pixel_map_r_to_r_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_R_TO_R_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_g_to_g_size_t, "pen.gl_pixel_map_g_to_g_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_G_TO_G_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_b_to_b_size_t, "pen.gl_pixel_map_b_to_b_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_B_TO_B_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_a_to_a_size_t, "pen.gl_pixel_map_a_to_a_size", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_A_TO_A_SIZE ) );
  declareStandardConstant( pen_gl_pixel_map_s_to_s_t, "pen.gl_pixel_map_s_to_s", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_S_TO_S ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_i_t, "pen.gl_pixel_map_i_to_i", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_I ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_r_t, "pen.gl_pixel_map_i_to_r", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_R ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_g_t, "pen.gl_pixel_map_i_to_g", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_G ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_b_t, "pen.gl_pixel_map_i_to_b", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_B ) );
  declareStandardConstant( pen_gl_pixel_map_i_to_a_t, "pen.gl_pixel_map_i_to_a", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_I_TO_A ) );
  declareStandardConstant( pen_gl_pixel_map_r_to_r_t, "pen.gl_pixel_map_r_to_r", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_R_TO_R ) );
  declareStandardConstant( pen_gl_pixel_map_g_to_g_t, "pen.gl_pixel_map_g_to_g", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_G_TO_G ) );
  declareStandardConstant( pen_gl_pixel_map_b_to_b_t, "pen.gl_pixel_map_b_to_b", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_B_TO_B ) );
  declareStandardConstant( pen_gl_pixel_map_a_to_a_t, "pen.gl_pixel_map_a_to_a", pen_glpixelmode_t, float_to_string( GL_PIXEL_MAP_A_TO_A ) );
  declareStandardConstant( pen_gl_pack_alignment_t, "pen.gl_pack_alignment", pen_glpixelmode_t, float_to_string( GL_PACK_ALIGNMENT ) );
  declareStandardConstant( pen_gl_pack_lsb_first_t, "pen.gl_pack_lsb_first", pen_glpixelmode_t, float_to_string( GL_PACK_LSB_FIRST ) );
  declareStandardConstant( pen_gl_pack_row_length_t, "pen.gl_pack_row_length", pen_glpixelmode_t, float_to_string( GL_PACK_ROW_LENGTH ) );
  declareStandardConstant( pen_gl_pack_skip_pixels_t, "pen.gl_pack_skip_pixels", pen_glpixelmode_t, float_to_string( GL_PACK_SKIP_PIXELS ) );
  declareStandardConstant( pen_gl_pack_skip_rows_t, "pen.gl_pack_skip_rows", pen_glpixelmode_t, float_to_string( GL_PACK_SKIP_ROWS ) );
  declareStandardConstant( pen_gl_pack_swap_bytes_t, "pen.gl_pack_swap_bytes", pen_glpixelmode_t, float_to_string( GL_PACK_SWAP_BYTES ) );
  declareStandardConstant( pen_gl_unpack_alignment_t, "pen.gl_unpack_alignment", pen_glpixelmode_t, float_to_string( GL_UNPACK_ALIGNMENT ) );
  declareStandardConstant( pen_gl_unpack_lsb_first_t, "pen.gl_unpack_lsb_first", pen_glpixelmode_t, float_to_string( GL_UNPACK_LSB_FIRST ) );
  declareStandardConstant( pen_gl_unpack_row_length_t, "pen.gl_unpack_row_length", pen_glpixelmode_t, float_to_string( GL_UNPACK_ROW_LENGTH ) );
  declareStandardConstant( pen_gl_unpack_skip_pixels_t, "pen.gl_unpack_skip_pixels", pen_glpixelmode_t, float_to_string( GL_UNPACK_SKIP_PIXELS ) );
  declareStandardConstant( pen_gl_unpack_skip_rows_t, "pen.gl_unpack_skip_rows", pen_glpixelmode_t, float_to_string( GL_UNPACK_SKIP_ROWS ) );
  declareStandardConstant( pen_gl_unpack_swap_bytes_t, "pen.gl_unpack_swap_bytes", pen_glpixelmode_t, float_to_string( GL_UNPACK_SWAP_BYTES ) );
  declareStandardConstant( pen_gl_zoom_x_t, "pen.gl_zoom_x", pen_glpixelmode_t, float_to_string( GL_ZOOM_X ) );
  declareStandardConstant( pen_gl_zoom_y_t, "pen.gl_zoom_y", pen_glpixelmode_t, float_to_string( GL_ZOOM_Y ) );

  declareIdent( pen_gltexturemapping_t, "pen.gltexturemapping", integer_t, typeClass  );

  declareStandardConstant( pen_gl_texture_env_t, "pen.gl_texture_env", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_ENV ) );
  declareStandardConstant( pen_gl_texture_env_mode_t, "pen.gl_texture_env_mode", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_ENV_MODE ) );
  declareStandardConstant( pen_gl_texture_1d_t, "pen.gl_texture_1d", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_1D ) );
  declareStandardConstant( pen_gl_texture_2d_t, "pen.gl_texture_2d", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_2D ) );
  declareStandardConstant( pen_gl_texture_wrap_s_t, "pen.gl_texture_wrap_s", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_WRAP_S ) );
  declareStandardConstant( pen_gl_texture_wrap_t_t, "pen.gl_texture_wrap_t", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_WRAP_T ) );
  declareStandardConstant( pen_gl_texture_mag_filter_t, "pen.gl_texture_mag_filter", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_MAG_FILTER ) );
  declareStandardConstant( pen_gl_texture_min_filter_t, "pen.gl_texture_min_filter", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_MIN_FILTER ) );
  declareStandardConstant( pen_gl_texture_env_color_t, "pen.gl_texture_env_color", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_ENV_COLOR ) );
  declareStandardConstant( pen_gl_texture_gen_s_t, "pen.gl_texture_gen_s", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_GEN_S ) );
  declareStandardConstant( pen_gl_texture_gen_t_t, "pen.gl_texture_gen_t", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_GEN_T ) );
  declareStandardConstant( pen_gl_texture_gen_mode_t, "pen.gl_texture_gen_mode", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_GEN_MODE ) );
  declareStandardConstant( pen_gl_texture_border_color_t, "pen.gl_texture_border_color", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_BORDER_COLOR ) );
  declareStandardConstant( pen_gl_texture_width_t, "pen.gl_texture_width", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_WIDTH ) );
  declareStandardConstant( pen_gl_texture_height_t, "pen.gl_texture_height", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_HEIGHT ) );
  declareStandardConstant( pen_gl_texture_border_t, "pen.gl_texture_border", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_BORDER ) );
  declareStandardConstant( pen_gl_texture_components_t, "pen.gl_texture_components", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_COMPONENTS ) );
  declareStandardConstant( pen_gl_texture_red_size_t, "pen.gl_texture_red_size", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_RED_SIZE ) );
  declareStandardConstant( pen_gl_texture_green_size_t, "pen.gl_texture_green_size", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_GREEN_SIZE ) );
  declareStandardConstant( pen_gl_texture_blue_size_t, "pen.gl_texture_blue_size", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_BLUE_SIZE ) );
  declareStandardConstant( pen_gl_texture_alpha_size_t, "pen.gl_texture_alpha_size", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_ALPHA_SIZE ) );
  declareStandardConstant( pen_gl_texture_luminance_size_t, "pen.gl_texture_luminance_size", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_LUMINANCE_SIZE ) );
  declareStandardConstant( pen_gl_texture_intensity_size_t, "pen.gl_texture_intensity_size", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_INTENSITY_SIZE ) );
  declareStandardConstant( pen_gl_nearest_mipmap_nearest_t, "pen.gl_nearest_mipmap_nearest", pen_gltexturemapping_t, float_to_string( GL_NEAREST_MIPMAP_NEAREST ) );
  declareStandardConstant( pen_gl_nearest_mipmap_linear_t, "pen.gl_nearest_mipmap_linear", pen_gltexturemapping_t, float_to_string( GL_NEAREST_MIPMAP_LINEAR ) );
  declareStandardConstant( pen_gl_linear_mipmap_nearest_t, "pen.gl_linear_mipmap_nearest", pen_gltexturemapping_t, float_to_string( GL_LINEAR_MIPMAP_NEAREST ) );
  declareStandardConstant( pen_gl_linear_mipmap_linear_t, "pen.gl_linear_mipmap_linear", pen_gltexturemapping_t, float_to_string( GL_LINEAR_MIPMAP_LINEAR ) );
  declareStandardConstant( pen_gl_object_linear_t, "pen.gl_object_linear", pen_gltexturemapping_t, float_to_string( GL_OBJECT_LINEAR ) );
  declareStandardConstant( pen_gl_object_plane_t, "pen.gl_object_plane", pen_gltexturemapping_t, float_to_string( GL_OBJECT_PLANE ) );
  declareStandardConstant( pen_gl_eye_linear_t, "pen.gl_eye_linear", pen_gltexturemapping_t, float_to_string( GL_EYE_LINEAR ) );
  declareStandardConstant( pen_gl_eye_plane_t, "pen.gl_eye_plane", pen_gltexturemapping_t, float_to_string( GL_EYE_PLANE ) );
  declareStandardConstant( pen_gl_sphere_map_t, "pen.gl_sphere_map", pen_gltexturemapping_t, float_to_string( GL_SPHERE_MAP ) );
  declareStandardConstant( pen_gl_decal_t, "pen.gl_decal", pen_gltexturemapping_t, float_to_string( GL_DECAL ) );
  declareStandardConstant( pen_gl_modulate_t, "pen.gl_modulate", pen_gltexturemapping_t, float_to_string( GL_MODULATE ) );
  declareStandardConstant( pen_gl_nearest_t, "pen.gl_nearest", pen_gltexturemapping_t, float_to_string( GL_NEAREST ) );
  declareStandardConstant( pen_gl_repeat_t, "pen.gl_repeat", pen_gltexturemapping_t, float_to_string( GL_REPEAT ) );
  declareStandardConstant( pen_gl_clamp_t, "pen.gl_clamp", pen_gltexturemapping_t, float_to_string( GL_CLAMP ) );
  declareStandardConstant( pen_gl_s_t, "pen.gl_s", pen_gltexturemapping_t, float_to_string( GL_S ) );
  declareStandardConstant( pen_gl_t_t, "pen.gl_t", pen_gltexturemapping_t, float_to_string( GL_T ) );
  declareStandardConstant( pen_gl_r_t, "pen.gl_r", pen_gltexturemapping_t, float_to_string( GL_R ) );
  declareStandardConstant( pen_gl_q_t, "pen.gl_q", pen_gltexturemapping_t, float_to_string( GL_Q ) );
  declareStandardConstant( pen_gl_texture_gen_r_t, "pen.gl_texture_gen_r", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_GEN_R ) );
  declareStandardConstant( pen_gl_texture_gen_q_t, "pen.gl_texture_gen_q", pen_gltexturemapping_t, float_to_string( GL_TEXTURE_GEN_Q ) );

  declareIdent( pen_glutility_t, "pen.glutility", integer_t, typeClass  );

  declareStandardConstant( pen_gl_vendor_t, "pen.gl_vendor", pen_glutility_t, float_to_string( GL_VENDOR ) );
  declareStandardConstant( pen_gl_renderer_t, "pen.gl_renderer", pen_glutility_t, float_to_string( GL_RENDERER ) );
  declareStandardConstant( pen_gl_version_t, "pen.gl_version", pen_glutility_t, float_to_string( GL_VERSION ) );
  declareStandardConstant( pen_gl_extensions_t, "pen.gl_extensions", pen_glutility_t, float_to_string( GL_EXTENSIONS ) );

  declareIdent( pen_glerrors_t, "pen.glerrors", integer_t, typeClass  );

  declareStandardConstant( pen_gl_no_error_t, "pen.gl_no_error", pen_glerrors_t, float_to_string( GL_NO_ERROR ) );
  declareStandardConstant( pen_gl_invalid_enum_t, "pen.gl_invalid_enum", pen_glerrors_t, float_to_string( GL_INVALID_ENUM ) );
  declareStandardConstant( pen_gl_invalid_value_t, "pen.gl_invalid_value", pen_glerrors_t, float_to_string( GL_INVALID_VALUE ) );
  declareStandardConstant( pen_gl_invalid_operation_t, "pen.gl_invalid_operation", pen_glerrors_t, float_to_string( GL_INVALID_OPERATION ) );
  declareStandardConstant( pen_gl_stack_overflow_t, "pen.gl_stack_overflow", pen_glerrors_t, float_to_string( GL_STACK_OVERFLOW ) );
  declareStandardConstant( pen_gl_stack_underflow_t, "pen.gl_stack_underflow", pen_glerrors_t, float_to_string( GL_STACK_UNDERFLOW ) );
  declareStandardConstant( pen_gl_out_of_memory_t, "pen.gl_out_of_memory", pen_glerrors_t, float_to_string( GL_OUT_OF_MEMORY ) );

  -- bitfield is natural, not int, but bits are passed to glClear, etc. which takes a bitfield
  --declareIdent( pen_glpushbits_t, "pen.glpushbits", integer_t, typeClass  );
  declareIdent( pen_glpushbits_t, "pen.glpushbits", pen_glbitfield_t, subClass  );

  declareStandardConstant( pen_gl_current_bit_t, "pen.gl_current_bit", pen_glpushbits_t, float_to_string( GL_CURRENT_BIT ) );
  declareStandardConstant( pen_gl_point_bit_t, "pen.gl_point_bit", pen_glpushbits_t, float_to_string( GL_POINT_BIT ) );
  declareStandardConstant( pen_gl_line_bit_t, "pen.gl_line_bit", pen_glpushbits_t, float_to_string( GL_LINE_BIT ) );
  declareStandardConstant( pen_gl_polygon_bit_t, "pen.gl_polygon_bit", pen_glpushbits_t, float_to_string( GL_POLYGON_BIT ) );
  declareStandardConstant( pen_gl_polygon_stipple_bit_t, "pen.gl_polygon_stipple_bit", pen_glpushbits_t, float_to_string( GL_POLYGON_STIPPLE_BIT ) );
  declareStandardConstant( pen_gl_pixel_mode_bit_t, "pen.gl_pixel_mode_bit", pen_glpushbits_t, float_to_string( GL_PIXEL_MODE_BIT ) );
  declareStandardConstant( pen_gl_lighting_bit_t, "pen.gl_lighting_bit", pen_glpushbits_t, float_to_string( GL_LIGHTING_BIT ) );
  declareStandardConstant( pen_gl_fog_bit_t, "pen.gl_fog_bit", pen_glpushbits_t, float_to_string( GL_FOG_BIT ) );
  declareStandardConstant( pen_gl_depth_buffer_bit_t, "pen.gl_depth_buffer_bit", pen_glpushbits_t, float_to_string( GL_DEPTH_BUFFER_BIT ) );
  declareStandardConstant( pen_gl_accum_buffer_bit_t, "pen.gl_accum_buffer_bit", pen_glpushbits_t, float_to_string( GL_ACCUM_BUFFER_BIT ) );
  declareStandardConstant( pen_gl_stencil_buffer_bit_t, "pen.gl_stencil_buffer_bit", pen_glpushbits_t, float_to_string( GL_STENCIL_BUFFER_BIT ) );
  declareStandardConstant( pen_gl_viewport_bit_t, "pen.gl_viewport_bit", pen_glpushbits_t, float_to_string( GL_VIEWPORT_BIT ) );
  declareStandardConstant( pen_gl_transform_bit_t, "pen.gl_transform_bit", pen_glpushbits_t, float_to_string( GL_TRANSFORM_BIT ) );
  declareStandardConstant( pen_gl_enable_bit_t, "pen.gl_enable_bit", pen_glpushbits_t, float_to_string( GL_ENABLE_BIT ) );
  declareStandardConstant( pen_gl_color_buffer_bit_t, "pen.gl_color_buffer_bit", pen_glpushbits_t, float_to_string( GL_COLOR_BUFFER_BIT ) );
  declareStandardConstant( pen_gl_hint_bit_t, "pen.gl_hint_bit", pen_glpushbits_t, float_to_string( GL_HINT_BIT ) );
  declareStandardConstant( pen_gl_eval_bit_t, "pen.gl_eval_bit", pen_glpushbits_t, float_to_string( GL_EVAL_BIT ) );
  declareStandardConstant( pen_gl_list_bit_t, "pen.gl_list_bit", pen_glpushbits_t, float_to_string( GL_LIST_BIT ) );
  declareStandardConstant( pen_gl_texture_bit_t, "pen.gl_texture_bit", pen_glpushbits_t, float_to_string( GL_TEXTURE_BIT ) );
  declareStandardConstant( pen_gl_scissor_bit_t, "pen.gl_scissor_bit", pen_glpushbits_t, float_to_string( GL_SCISSOR_BIT ) );
  declareStandardConstant( pen_gl_all_attrib_bits_t, "pen.gl_all_attrib_bits", pen_glpushbits_t, float_to_string( GL_ALL_ATTRIB_BITS ) );
  declareStandardConstant( pen_gl_proxy_texture_1d_t, "pen.gl_proxy_texture_1d", pen_glenum_t, float_to_string( GL_PROXY_TEXTURE_1D ) );
  declareStandardConstant( pen_gl_proxy_texture_2d_t, "pen.gl_proxy_texture_2d", pen_glenum_t, float_to_string( GL_PROXY_TEXTURE_2D ) );
  declareStandardConstant( pen_gl_texture_priority_t, "pen.gl_texture_priority", pen_glenum_t, float_to_string( GL_TEXTURE_PRIORITY ) );
  declareStandardConstant( pen_gl_texture_resident_t, "pen.gl_texture_resident", pen_glenum_t, float_to_string( GL_TEXTURE_RESIDENT ) );
  declareStandardConstant( pen_gl_texture_binding_1d_t, "pen.gl_texture_binding_1d", pen_glenum_t, float_to_string( GL_TEXTURE_BINDING_1D ) );
  declareStandardConstant( pen_gl_texture_binding_2d_t, "pen.gl_texture_binding_2d", pen_glenum_t, float_to_string( GL_TEXTURE_BINDING_2D ) );
  declareStandardConstant( pen_gl_texture_internal_format_t, "pen.gl_texture_internal_format", pen_glenum_t, float_to_string( GL_TEXTURE_INTERNAL_FORMAT ) );
  declareStandardConstant( pen_gl_alpha4_t, "pen.gl_alpha4", pen_glenum_t, float_to_string( GL_ALPHA4 ) );
  declareStandardConstant( pen_gl_alpha8_t, "pen.gl_alpha8", pen_glenum_t, float_to_string( GL_ALPHA8 ) );
  declareStandardConstant( pen_gl_alpha12_t, "pen.gl_alpha12", pen_glenum_t, float_to_string( GL_ALPHA12 ) );
  declareStandardConstant( pen_gl_alpha16_t, "pen.gl_alpha16", pen_glenum_t, float_to_string( GL_ALPHA16 ) );
  declareStandardConstant( pen_gl_luminance4_t, "pen.gl_luminance4", pen_glenum_t, float_to_string( GL_LUMINANCE4 ) );
  declareStandardConstant( pen_gl_luminance8_t, "pen.gl_luminance8", pen_glenum_t, float_to_string( GL_LUMINANCE8 ) );
  declareStandardConstant( pen_gl_luminance12_t, "pen.gl_luminance12", pen_glenum_t, float_to_string( GL_LUMINANCE12 ) );
  declareStandardConstant( pen_gl_luminance16_t, "pen.gl_luminance16", pen_glenum_t, float_to_string( GL_LUMINANCE16 ) );
  declareStandardConstant( pen_gl_luminance4_alpha4_t, "pen.gl_luminance4_alpha4", pen_glenum_t, float_to_string( GL_LUMINANCE4_ALPHA4 ) );
  declareStandardConstant( pen_gl_luminance6_alpha2_t, "pen.gl_luminance6_alpha2", pen_glenum_t, float_to_string( GL_LUMINANCE6_ALPHA2 ) );
  declareStandardConstant( pen_gl_luminance8_alpha8_t, "pen.gl_luminance8_alpha8", pen_glenum_t, float_to_string( GL_LUMINANCE8_ALPHA8 ) );
  declareStandardConstant( pen_gl_luminance12_alpha4_t, "pen.gl_luminance12_alpha4", pen_glenum_t, float_to_string( GL_LUMINANCE12_ALPHA4 ) );
  declareStandardConstant( pen_gl_luminance12_alpha12_t, "pen.gl_luminance12_alpha12", pen_glenum_t, float_to_string( GL_LUMINANCE12_ALPHA12 ) );
  declareStandardConstant( pen_gl_luminance16_alpha16_t, "pen.gl_luminance16_alpha16", pen_glenum_t, float_to_string( GL_LUMINANCE16_ALPHA16 ) );
  declareStandardConstant( pen_gl_intensity_t, "pen.gl_intensity", pen_glenum_t, float_to_string( GL_INTENSITY ) );
  declareStandardConstant( pen_gl_intensity4_t, "pen.gl_intensity4", pen_glenum_t, float_to_string( GL_INTENSITY4 ) );
  declareStandardConstant( pen_gl_intensity8_t, "pen.gl_intensity8", pen_glenum_t, float_to_string( GL_INTENSITY8 ) );
  declareStandardConstant( pen_gl_intensity12_t, "pen.gl_intensity12", pen_glenum_t, float_to_string( GL_INTENSITY12 ) );
  declareStandardConstant( pen_gl_intensity16_t, "pen.gl_intensity16", pen_glenum_t, float_to_string( GL_INTENSITY16 ) );
  declareStandardConstant( pen_gl_r3_g3_b2_t, "pen.gl_r3_g3_b2", pen_glenum_t, float_to_string( GL_R3_G3_B2 ) );
  declareStandardConstant( pen_gl_rgb4_t, "pen.gl_rgb4", pen_glenum_t, float_to_string( GL_RGB4 ) );
  declareStandardConstant( pen_gl_rgb5_t, "pen.gl_rgb5", pen_glenum_t, float_to_string( GL_RGB5 ) );
  declareStandardConstant( pen_gl_rgb8_t, "pen.gl_rgb8", pen_glenum_t, float_to_string( GL_RGB8 ) );
  declareStandardConstant( pen_gl_rgb10_t, "pen.gl_rgb10", pen_glenum_t, float_to_string( GL_RGB10 ) );
  declareStandardConstant( pen_gl_rgb12_t, "pen.gl_rgb12", pen_glenum_t, float_to_string( GL_RGB12 ) );
  declareStandardConstant( pen_gl_rgb16_t, "pen.gl_rgb16", pen_glenum_t, float_to_string( GL_RGB16 ) );
  declareStandardConstant( pen_gl_rgba2_t, "pen.gl_rgba2", pen_glenum_t, float_to_string( GL_RGBA2 ) );
  declareStandardConstant( pen_gl_rgba4_t, "pen.gl_rgba4", pen_glenum_t, float_to_string( GL_RGBA4 ) );
  declareStandardConstant( pen_gl_rgb5_a1_t, "pen.gl_rgb5_a1", pen_glenum_t, float_to_string( GL_RGB5_A1 ) );
  declareStandardConstant( pen_gl_rgba8_t, "pen.gl_rgba8", pen_glenum_t, float_to_string( GL_RGBA8 ) );
  declareStandardConstant( pen_gl_rgb10_a2_t, "pen.gl_rgb10_a2", pen_glenum_t, float_to_string( GL_RGB10_A2 ) );
  declareStandardConstant( pen_gl_rgba12_t, "pen.gl_rgba12", pen_glenum_t, float_to_string( GL_RGBA12 ) );
  declareStandardConstant( pen_gl_rgba16_t, "pen.gl_rgba16", pen_glenum_t, float_to_string( GL_RGBA16 ) );
  declareStandardConstant( pen_gl_client_pixel_store_bit_t, "pen.gl_client_pixel_store_bit", pen_glenum_t, float_to_string( GL_CLIENT_PIXEL_STORE_BIT ) );
  declareStandardConstant( pen_gl_client_vertex_array_bit_t, "pen.gl_client_vertex_array_bit", pen_glenum_t, float_to_string( GL_CLIENT_VERTEX_ARRAY_BIT ) );
  declareStandardConstant( pen_gl_all_client_attrib_bits_t, "pen.gl_all_client_attrib_bits", pen_glenum_t, float_to_string( GL_ALL_CLIENT_ATTRIB_BITS ) );
  declareStandardConstant( pen_gl_client_all_attrib_bits_t, "pen.gl_client_all_attrib_bits", pen_glenum_t, float_to_string( GL_CLIENT_ALL_ATTRIB_BITS ) );
  declareStandardConstant( pen_gl_rescale_normal_t, "pen.gl_rescale_normal", pen_glenum_t, float_to_string( GL_RESCALE_NORMAL ) );
  declareStandardConstant( pen_gl_clamp_to_edge_t, "pen.gl_clamp_to_edge", pen_glenum_t, float_to_string( GL_CLAMP_TO_EDGE ) );
  declareStandardConstant( pen_gl_max_elements_vertices_t, "pen.gl_max_elements_vertices", pen_glenum_t, float_to_string( GL_MAX_ELEMENTS_VERTICES ) );
  declareStandardConstant( pen_gl_max_elements_indices_t, "pen.gl_max_elements_indices", pen_glenum_t, float_to_string( GL_MAX_ELEMENTS_INDICES ) );
  declareStandardConstant( pen_gl_bgr_t, "pen.gl_bgr", pen_glenum_t, float_to_string( GL_BGR ) );
  declareStandardConstant( pen_gl_bgra_t, "pen.gl_bgra", pen_glenum_t, float_to_string( GL_BGRA ) );
  declareStandardConstant( pen_gl_unsigned_byte_3_3_2_t, "pen.gl_unsigned_byte_3_3_2", pen_glenum_t, float_to_string( GL_UNSIGNED_BYTE_3_3_2 ) );
  declareStandardConstant( pen_gl_unsigned_byte_2_3_3_rev_t, "pen.gl_unsigned_byte_2_3_3_rev", pen_glenum_t, float_to_string( GL_UNSIGNED_BYTE_2_3_3_REV ) );
  declareStandardConstant( pen_gl_unsigned_short_5_6_5_t, "pen.gl_unsigned_short_5_6_5", pen_glenum_t, float_to_string( GL_UNSIGNED_SHORT_5_6_5 ) );
  declareStandardConstant( pen_gl_unsigned_short_5_6_5_rev_t, "pen.gl_unsigned_short_5_6_5_rev", pen_glenum_t, float_to_string( GL_UNSIGNED_SHORT_5_6_5_REV ) );
  declareStandardConstant( pen_gl_unsigned_short_4_4_4_4_t, "pen.gl_unsigned_short_4_4_4_4", pen_glenum_t, float_to_string( GL_UNSIGNED_SHORT_4_4_4_4 ) );
  declareStandardConstant( pen_gl_unsigned_short_4_4_4_4_rev_t, "pen.gl_unsigned_short_4_4_4_4_rev", pen_glenum_t, float_to_string( GL_UNSIGNED_SHORT_4_4_4_4_REV ) );
  declareStandardConstant( pen_gl_unsigned_short_5_5_5_1_t, "pen.gl_unsigned_short_5_5_5_1", pen_glenum_t, float_to_string( GL_UNSIGNED_SHORT_5_5_5_1 ) );
  declareStandardConstant( pen_gl_unsigned_short_1_5_5_5_rev_t, "pen.gl_unsigned_short_1_5_5_5_rev", pen_glenum_t, float_to_string( GL_UNSIGNED_SHORT_1_5_5_5_REV ) );
  declareStandardConstant( pen_gl_unsigned_int_8_8_8_8_t, "pen.gl_unsigned_int_8_8_8_8", pen_glenum_t, float_to_string( GL_UNSIGNED_INT_8_8_8_8 ) );
  declareStandardConstant( pen_gl_unsigned_int_8_8_8_8_rev_t, "pen.gl_unsigned_int_8_8_8_8_rev", pen_glenum_t, float_to_string( GL_UNSIGNED_INT_8_8_8_8_REV ) );
  declareStandardConstant( pen_gl_unsigned_int_10_10_10_2_t, "pen.gl_unsigned_int_10_10_10_2", pen_glenum_t, float_to_string( GL_UNSIGNED_INT_10_10_10_2 ) );
  declareStandardConstant( pen_gl_unsigned_int_2_10_10_10_rev_t, "pen.gl_unsigned_int_2_10_10_10_rev", pen_glenum_t, float_to_string( GL_UNSIGNED_INT_2_10_10_10_REV ) );
  declareStandardConstant( pen_gl_light_model_color_control_t, "pen.gl_light_model_color_control", pen_glenum_t, float_to_string( GL_LIGHT_MODEL_COLOR_CONTROL ) );
  declareStandardConstant( pen_gl_single_color_t, "pen.gl_single_color", pen_glenum_t, float_to_string( GL_SINGLE_COLOR ) );
  declareStandardConstant( pen_gl_separate_specular_color_t, "pen.gl_separate_specular_color", pen_glenum_t, float_to_string( GL_SEPARATE_SPECULAR_COLOR ) );
  declareStandardConstant( pen_gl_texture_min_lod_t, "pen.gl_texture_min_lod", pen_glenum_t, float_to_string( GL_TEXTURE_MIN_LOD ) );
  declareStandardConstant( pen_gl_texture_max_lod_t, "pen.gl_texture_max_lod", pen_glenum_t, float_to_string( GL_TEXTURE_MAX_LOD ) );
  declareStandardConstant( pen_gl_texture_base_level_t, "pen.gl_texture_base_level", pen_glenum_t, float_to_string( GL_TEXTURE_BASE_LEVEL ) );
  declareStandardConstant( pen_gl_texture_max_level_t, "pen.gl_texture_max_level", pen_glenum_t, float_to_string( GL_TEXTURE_MAX_LEVEL ) );
  declareStandardConstant( pen_gl_smooth_point_size_range_t, "pen.gl_smooth_point_size_range", pen_glenum_t, float_to_string( GL_SMOOTH_POINT_SIZE_RANGE ) );
  declareStandardConstant( pen_gl_smooth_point_size_granularity_t, "pen.gl_smooth_point_size_granularity", pen_glenum_t, float_to_string( GL_SMOOTH_POINT_SIZE_GRANULARITY ) );
  declareStandardConstant( pen_gl_smooth_line_width_range_t, "pen.gl_smooth_line_width_range", pen_glenum_t, float_to_string( GL_SMOOTH_LINE_WIDTH_RANGE ) );
  declareStandardConstant( pen_gl_smooth_line_width_granularity_t, "pen.gl_smooth_line_width_granularity", pen_glenum_t, float_to_string( GL_SMOOTH_LINE_WIDTH_GRANULARITY ) );
  declareStandardConstant( pen_gl_aliased_point_size_range_t, "pen.gl_aliased_point_size_range", pen_glenum_t, float_to_string( GL_ALIASED_POINT_SIZE_RANGE ) );
  declareStandardConstant( pen_gl_aliased_line_width_range_t, "pen.gl_aliased_line_width_range", pen_glenum_t, float_to_string( GL_ALIASED_LINE_WIDTH_RANGE ) );
  declareStandardConstant( pen_gl_pack_skip_images_t, "pen.gl_pack_skip_images", pen_glenum_t, float_to_string( GL_PACK_SKIP_IMAGES ) );
  declareStandardConstant( pen_gl_pack_image_height_t, "pen.gl_pack_image_height", pen_glenum_t, float_to_string( GL_PACK_IMAGE_HEIGHT ) );
  declareStandardConstant( pen_gl_unpack_skip_images_t, "pen.gl_unpack_skip_images", pen_glenum_t, float_to_string( GL_UNPACK_SKIP_IMAGES ) );
  declareStandardConstant( pen_gl_unpack_image_height_t, "pen.gl_unpack_image_height", pen_glenum_t, float_to_string( GL_UNPACK_IMAGE_HEIGHT ) );
  declareStandardConstant( pen_gl_texture_3d_t, "pen.gl_texture_3d", pen_glenum_t, float_to_string( GL_TEXTURE_3D ) );
  declareStandardConstant( pen_gl_proxy_texture_3d_t, "pen.gl_proxy_texture_3d", pen_glenum_t, float_to_string( GL_PROXY_TEXTURE_3D ) );
  declareStandardConstant( pen_gl_texture_depth_t, "pen.gl_texture_depth", pen_glenum_t, float_to_string( GL_TEXTURE_DEPTH ) );
  declareStandardConstant( pen_gl_texture_wrap_r_t, "pen.gl_texture_wrap_r", pen_glenum_t, float_to_string( GL_TEXTURE_WRAP_R ) );
  declareStandardConstant( pen_gl_max_3d_texture_size_t, "pen.gl_max_3d_texture_size", pen_glenum_t, float_to_string( GL_MAX_3D_TEXTURE_SIZE ) );
  declareStandardConstant( pen_gl_texture_binding_3d_t, "pen.gl_texture_binding_3d", pen_glenum_t, float_to_string( GL_TEXTURE_BINDING_3D ) );

  declareIdent( pen_glarbmapping_t, "pen.glarbmapping", integer_t, typeClass  );

  declareStandardConstant( pen_gl_constant_color_t, "pen.gl_constant_color", pen_glarbmapping_t, float_to_string( GL_CONSTANT_COLOR ) );
  declareStandardConstant( pen_gl_one_minus_constant_color_t, "pen.gl_one_minus_constant_color", pen_glarbmapping_t, float_to_string( GL_ONE_MINUS_CONSTANT_COLOR ) );
  declareStandardConstant( pen_gl_constant_alpha_t, "pen.gl_constant_alpha", pen_glarbmapping_t, float_to_string( GL_CONSTANT_ALPHA ) );
  declareStandardConstant( pen_gl_one_minus_constant_alpha_t, "pen.gl_one_minus_constant_alpha", pen_glarbmapping_t, float_to_string( GL_ONE_MINUS_CONSTANT_ALPHA ) );
  declareStandardConstant( pen_gl_color_table_t, "pen.gl_color_table", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE ) );
  declareStandardConstant( pen_gl_post_convolution_color_table_t, "pen.gl_post_convolution_color_table", pen_glarbmapping_t, float_to_string( GL_POST_CONVOLUTION_COLOR_TABLE ) );
  declareStandardConstant( pen_gl_post_color_matrix_color_table_t, "pen.gl_post_color_matrix_color_table", pen_glarbmapping_t, float_to_string( GL_POST_COLOR_MATRIX_COLOR_TABLE ) );
  declareStandardConstant( pen_gl_proxy_color_table_t, "pen.gl_proxy_color_table", pen_glarbmapping_t, float_to_string( GL_PROXY_COLOR_TABLE ) );
  declareStandardConstant( pen_gl_proxy_post_convolution_color_table_t, "pen.gl_proxy_post_convolution_color_table", pen_glarbmapping_t, float_to_string( GL_PROXY_POST_CONVOLUTION_COLOR_TABLE ) );
  declareStandardConstant( pen_gl_proxy_post_color_matrix_color_table_t, "pen.gl_proxy_post_color_matrix_color_table", pen_glarbmapping_t, float_to_string( GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE ) );
  declareStandardConstant( pen_gl_color_table_scale_t, "pen.gl_color_table_scale", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_SCALE ) );
  declareStandardConstant( pen_gl_color_table_bias_t, "pen.gl_color_table_bias", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_BIAS ) );
  declareStandardConstant( pen_gl_color_table_format_t, "pen.gl_color_table_format", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_FORMAT ) );
  declareStandardConstant( pen_gl_color_table_width_t, "pen.gl_color_table_width", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_WIDTH ) );
  declareStandardConstant( pen_gl_color_table_red_size_t, "pen.gl_color_table_red_size", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_RED_SIZE ) );
  declareStandardConstant( pen_gl_color_table_green_size_t, "pen.gl_color_table_green_size", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_GREEN_SIZE ) );
  declareStandardConstant( pen_gl_color_table_blue_size_t, "pen.gl_color_table_blue_size", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_BLUE_SIZE ) );
  declareStandardConstant( pen_gl_color_table_alpha_size_t, "pen.gl_color_table_alpha_size", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_ALPHA_SIZE ) );
  declareStandardConstant( pen_gl_color_table_luminance_size_t, "pen.gl_color_table_luminance_size", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_LUMINANCE_SIZE ) );
  declareStandardConstant( pen_gl_color_table_intensity_size_t, "pen.gl_color_table_intensity_size", pen_glarbmapping_t, float_to_string( GL_COLOR_TABLE_INTENSITY_SIZE ) );
  declareStandardConstant( pen_gl_convolution_1d_t, "pen.gl_convolution_1d", pen_glarbmapping_t, float_to_string( GL_CONVOLUTION_1D ) );
  declareStandardConstant( pen_gl_convolution_2d_t, "pen.gl_convolution_2d", pen_glarbmapping_t, float_to_string( GL_CONVOLUTION_2D ) );
  declareStandardConstant( pen_gl_separable_2d_t, "pen.gl_separable_2d", pen_glarbmapping_t, float_to_string( GL_SEPARABLE_2D ) );
  declareStandardConstant( pen_gl_convolution_border_mode_t, "pen.gl_convolution_border_mode", pen_glarbmapping_t, float_to_string( GL_CONVOLUTION_BORDER_MODE ) );
  declareStandardConstant( pen_gl_convolution_filter_scale_t, "pen.gl_convolution_filter_scale", pen_glarbmapping_t, float_to_string( GL_CONVOLUTION_FILTER_SCALE ) );
  declareStandardConstant( pen_gl_convolution_filter_bias_t, "pen.gl_convolution_filter_bias", pen_glarbmapping_t, float_to_string( GL_CONVOLUTION_FILTER_BIAS ) );
  declareStandardConstant( pen_gl_reduce_t, "pen.gl_reduce", pen_glarbmapping_t, float_to_string( GL_REDUCE ) );
  declareStandardConstant( pen_gl_convolution_format_t, "pen.gl_convolution_format", pen_glarbmapping_t, float_to_string( GL_CONVOLUTION_FORMAT ) );
  declareStandardConstant( pen_gl_convolution_width_t, "pen.gl_convolution_width", pen_glarbmapping_t, float_to_string( GL_CONVOLUTION_WIDTH ) );
  declareStandardConstant( pen_gl_convolution_height_t, "pen.gl_convolution_height", pen_glarbmapping_t, float_to_string( GL_CONVOLUTION_HEIGHT ) );
  declareStandardConstant( pen_gl_max_convolution_width_t, "pen.gl_max_convolution_width", pen_glarbmapping_t, float_to_string( GL_MAX_CONVOLUTION_WIDTH ) );
  declareStandardConstant( pen_gl_max_convolution_height_t, "pen.gl_max_convolution_height", pen_glarbmapping_t, float_to_string( GL_MAX_CONVOLUTION_HEIGHT ) );
  declareStandardConstant( pen_gl_post_convolution_red_scale_t, "pen.gl_post_convolution_red_scale", pen_glarbmapping_t, float_to_string( GL_POST_CONVOLUTION_RED_SCALE ) );
  declareStandardConstant( pen_gl_post_convolution_green_scale_t, "pen.gl_post_convolution_green_scale", pen_glarbmapping_t, float_to_string( GL_POST_CONVOLUTION_GREEN_SCALE ) );
  declareStandardConstant( pen_gl_post_convolution_blue_scale_t, "pen.gl_post_convolution_blue_scale", pen_glarbmapping_t, float_to_string( GL_POST_CONVOLUTION_BLUE_SCALE ) );
  declareStandardConstant( pen_gl_post_convolution_alpha_scale_t, "pen.gl_post_convolution_alpha_scale", pen_glarbmapping_t, float_to_string( GL_POST_CONVOLUTION_ALPHA_SCALE ) );
  declareStandardConstant( pen_gl_post_convolution_red_bias_t, "pen.gl_post_convolution_red_bias", pen_glarbmapping_t, float_to_string( GL_POST_CONVOLUTION_RED_BIAS ) );
  declareStandardConstant( pen_gl_post_convolution_green_bias_t, "pen.gl_post_convolution_green_bias", pen_glarbmapping_t, float_to_string( GL_POST_CONVOLUTION_GREEN_BIAS ) );
  declareStandardConstant( pen_gl_post_convolution_blue_bias_t, "pen.gl_post_convolution_blue_bias", pen_glarbmapping_t, float_to_string( GL_POST_CONVOLUTION_BLUE_BIAS ) );
  declareStandardConstant( pen_gl_post_convolution_alpha_bias_t, "pen.gl_post_convolution_alpha_bias", pen_glarbmapping_t, float_to_string( GL_POST_CONVOLUTION_ALPHA_BIAS ) );
  declareStandardConstant( pen_gl_constant_border_t, "pen.gl_constant_border", pen_glarbmapping_t, float_to_string( GL_CONSTANT_BORDER ) );
  declareStandardConstant( pen_gl_replicate_border_t, "pen.gl_replicate_border", pen_glarbmapping_t, float_to_string( GL_REPLICATE_BORDER ) );
  declareStandardConstant( pen_gl_convolution_border_color_t, "pen.gl_convolution_border_color", pen_glarbmapping_t, float_to_string( GL_CONVOLUTION_BORDER_COLOR ) );
  declareStandardConstant( pen_gl_color_matrix_t, "pen.gl_color_matrix", pen_glarbmapping_t, float_to_string( GL_COLOR_MATRIX ) );
  declareStandardConstant( pen_gl_color_matrix_stack_depth_t, "pen.gl_color_matrix_stack_depth", pen_glarbmapping_t, float_to_string( GL_COLOR_MATRIX_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_max_color_matrix_stack_depth_t, "pen.gl_max_color_matrix_stack_depth", pen_glarbmapping_t, float_to_string( GL_MAX_COLOR_MATRIX_STACK_DEPTH ) );
  declareStandardConstant( pen_gl_post_color_matrix_red_scale_t, "pen.gl_post_color_matrix_red_scale", pen_glarbmapping_t, float_to_string( GL_POST_COLOR_MATRIX_RED_SCALE ) );
  declareStandardConstant( pen_gl_post_color_matrix_green_scale_t, "pen.gl_post_color_matrix_green_scale", pen_glarbmapping_t, float_to_string( GL_POST_COLOR_MATRIX_GREEN_SCALE ) );
  declareStandardConstant( pen_gl_post_color_matrix_blue_scale_t, "pen.gl_post_color_matrix_blue_scale", pen_glarbmapping_t, float_to_string( GL_POST_COLOR_MATRIX_BLUE_SCALE ) );
  declareStandardConstant( pen_gl_post_color_matrix_alpha_scale_t, "pen.gl_post_color_matrix_alpha_scale", pen_glarbmapping_t, float_to_string( GL_POST_COLOR_MATRIX_ALPHA_SCALE ) );
  declareStandardConstant( pen_gl_post_color_matrix_red_bias_t, "pen.gl_post_color_matrix_red_bias", pen_glarbmapping_t, float_to_string( GL_POST_COLOR_MATRIX_RED_BIAS ) );
  declareStandardConstant( pen_gl_post_color_matrix_green_bias_t, "pen.gl_post_color_matrix_green_bias", pen_glarbmapping_t, float_to_string( GL_POST_COLOR_MATRIX_GREEN_BIAS ) );
  declareStandardConstant( pen_gl_post_color_matrix_blue_bias_t, "pen.gl_post_color_matrix_blue_bias", pen_glarbmapping_t, float_to_string( GL_POST_COLOR_MATRIX_BLUE_BIAS ) );
  declareStandardConstant( pen_gl_post_color_matrix_alpha_bias_t, "pen.gl_post_color_matrix_alpha_bias", pen_glarbmapping_t, float_to_string( GL_POST_COLOR_MATRIX_ALPHA_BIAS ) );
  declareStandardConstant( pen_gl_histogram_t, "pen.gl_histogram", pen_glarbmapping_t, float_to_string( GL_HISTOGRAM ) );
  declareStandardConstant( pen_gl_proxy_histogram_t, "pen.gl_proxy_histogram", pen_glarbmapping_t, float_to_string( GL_PROXY_HISTOGRAM ) );
  declareStandardConstant( pen_gl_histogram_width_t, "pen.gl_histogram_width", pen_glarbmapping_t, float_to_string( GL_HISTOGRAM_WIDTH ) );
  declareStandardConstant( pen_gl_histogram_format_t, "pen.gl_histogram_format", pen_glarbmapping_t, float_to_string( GL_HISTOGRAM_FORMAT ) );
  declareStandardConstant( pen_gl_histogram_red_size_t, "pen.gl_histogram_red_size", pen_glarbmapping_t, float_to_string( GL_HISTOGRAM_RED_SIZE ) );
  declareStandardConstant( pen_gl_histogram_green_size_t, "pen.gl_histogram_green_size", pen_glarbmapping_t, float_to_string( GL_HISTOGRAM_GREEN_SIZE ) );
  declareStandardConstant( pen_gl_histogram_blue_size_t, "pen.gl_histogram_blue_size", pen_glarbmapping_t, float_to_string( GL_HISTOGRAM_BLUE_SIZE ) );
  declareStandardConstant( pen_gl_histogram_alpha_size_t, "pen.gl_histogram_alpha_size", pen_glarbmapping_t, float_to_string( GL_HISTOGRAM_ALPHA_SIZE ) );
  declareStandardConstant( pen_gl_histogram_luminance_size_t, "pen.gl_histogram_luminance_size", pen_glarbmapping_t, float_to_string( GL_HISTOGRAM_LUMINANCE_SIZE ) );
  declareStandardConstant( pen_gl_histogram_sink_t, "pen.gl_histogram_sink", pen_glarbmapping_t, float_to_string( GL_HISTOGRAM_SINK ) );
  declareStandardConstant( pen_gl_minmax_t, "pen.gl_minmax", pen_glarbmapping_t, float_to_string( GL_MINMAX ) );
  declareStandardConstant( pen_gl_minmax_format_t, "pen.gl_minmax_format", pen_glarbmapping_t, float_to_string( GL_MINMAX_FORMAT ) );
  declareStandardConstant( pen_gl_minmax_sink_t, "pen.gl_minmax_sink", pen_glarbmapping_t, float_to_string( GL_MINMAX_SINK ) );
  declareStandardConstant( pen_gl_table_too_large_t, "pen.gl_table_too_large", pen_glarbmapping_t, float_to_string( GL_TABLE_TOO_LARGE ) );
  declareStandardConstant( pen_gl_blend_equation_t, "pen.gl_blend_equation", pen_glarbmapping_t, float_to_string( GL_BLEND_EQUATION ) );
  declareStandardConstant( pen_gl_min_t, "pen.gl_min", pen_glarbmapping_t, float_to_string( GL_MIN ) );
  declareStandardConstant( pen_gl_max_t, "pen.gl_max", pen_glarbmapping_t, float_to_string( GL_MAX ) );
  declareStandardConstant( pen_gl_func_add_t, "pen.gl_func_add", pen_glarbmapping_t, float_to_string( GL_FUNC_ADD ) );
  declareStandardConstant( pen_gl_func_subtract_t, "pen.gl_func_subtract", pen_glarbmapping_t, float_to_string( GL_FUNC_SUBTRACT ) );
  declareStandardConstant( pen_gl_func_reverse_subtract_t, "pen.gl_func_reverse_subtract", pen_glarbmapping_t, float_to_string( GL_FUNC_REVERSE_SUBTRACT ) );
  declareStandardConstant( pen_gl_blend_color_t, "pen.gl_blend_color", pen_glarbmapping_t, float_to_string( GL_BLEND_COLOR ) );

  declareIdent( pen_glmultitexture_t, "pen.glmultitexture", integer_t, typeClass  );

  declareStandardConstant( pen_gl_texture0_t, "pen.gl_texture0", pen_glmultitexture_t, float_to_string( GL_TEXTURE0 ) );
  declareStandardConstant( pen_gl_texture1_t, "pen.gl_texture1", pen_glmultitexture_t, float_to_string( GL_TEXTURE1 ) );
  declareStandardConstant( pen_gl_texture2_t, "pen.gl_texture2", pen_glmultitexture_t, float_to_string( GL_TEXTURE2 ) );
  declareStandardConstant( pen_gl_texture3_t, "pen.gl_texture3", pen_glmultitexture_t, float_to_string( GL_TEXTURE3 ) );
  declareStandardConstant( pen_gl_texture4_t, "pen.gl_texture4", pen_glmultitexture_t, float_to_string( GL_TEXTURE4 ) );
  declareStandardConstant( pen_gl_texture5_t, "pen.gl_texture5", pen_glmultitexture_t, float_to_string( GL_TEXTURE5 ) );
  declareStandardConstant( pen_gl_texture6_t, "pen.gl_texture6", pen_glmultitexture_t, float_to_string( GL_TEXTURE6 ) );
  declareStandardConstant( pen_gl_texture7_t, "pen.gl_texture7", pen_glmultitexture_t, float_to_string( GL_TEXTURE7 ) );
  declareStandardConstant( pen_gl_texture8_t, "pen.gl_texture8", pen_glmultitexture_t, float_to_string( GL_TEXTURE8 ) );
  declareStandardConstant( pen_gl_texture9_t, "pen.gl_texture9", pen_glmultitexture_t, float_to_string( GL_TEXTURE9 ) );
  declareStandardConstant( pen_gl_texture10_t, "pen.gl_texture10", pen_glmultitexture_t, float_to_string( GL_TEXTURE10 ) );
  declareStandardConstant( pen_gl_texture11_t, "pen.gl_texture11", pen_glmultitexture_t, float_to_string( GL_TEXTURE11 ) );
  declareStandardConstant( pen_gl_texture12_t, "pen.gl_texture12", pen_glmultitexture_t, float_to_string( GL_TEXTURE12 ) );
  declareStandardConstant( pen_gl_texture13_t, "pen.gl_texture13", pen_glmultitexture_t, float_to_string( GL_TEXTURE13 ) );
  declareStandardConstant( pen_gl_texture14_t, "pen.gl_texture14", pen_glmultitexture_t, float_to_string( GL_TEXTURE14 ) );
  declareStandardConstant( pen_gl_texture15_t, "pen.gl_texture15", pen_glmultitexture_t, float_to_string( GL_TEXTURE15 ) );
  declareStandardConstant( pen_gl_texture16_t, "pen.gl_texture16", pen_glmultitexture_t, float_to_string( GL_TEXTURE16 ) );
  declareStandardConstant( pen_gl_texture17_t, "pen.gl_texture17", pen_glmultitexture_t, float_to_string( GL_TEXTURE17 ) );
  declareStandardConstant( pen_gl_texture18_t, "pen.gl_texture18", pen_glmultitexture_t, float_to_string( GL_TEXTURE18 ) );
  declareStandardConstant( pen_gl_texture19_t, "pen.gl_texture19", pen_glmultitexture_t, float_to_string( GL_TEXTURE19 ) );
  declareStandardConstant( pen_gl_texture20_t, "pen.gl_texture20", pen_glmultitexture_t, float_to_string( GL_TEXTURE20 ) );
  declareStandardConstant( pen_gl_texture21_t, "pen.gl_texture21", pen_glmultitexture_t, float_to_string( GL_TEXTURE21 ) );
  declareStandardConstant( pen_gl_texture22_t, "pen.gl_texture22", pen_glmultitexture_t, float_to_string( GL_TEXTURE22 ) );
  declareStandardConstant( pen_gl_texture23_t, "pen.gl_texture23", pen_glmultitexture_t, float_to_string( GL_TEXTURE23 ) );
  declareStandardConstant( pen_gl_texture24_t, "pen.gl_texture24", pen_glmultitexture_t, float_to_string( GL_TEXTURE24 ) );
  declareStandardConstant( pen_gl_texture25_t, "pen.gl_texture25", pen_glmultitexture_t, float_to_string( GL_TEXTURE25 ) );
  declareStandardConstant( pen_gl_texture26_t, "pen.gl_texture26", pen_glmultitexture_t, float_to_string( GL_TEXTURE26 ) );
  declareStandardConstant( pen_gl_texture27_t, "pen.gl_texture27", pen_glmultitexture_t, float_to_string( GL_TEXTURE27 ) );
  declareStandardConstant( pen_gl_texture28_t, "pen.gl_texture28", pen_glmultitexture_t, float_to_string( GL_TEXTURE28 ) );
  declareStandardConstant( pen_gl_texture29_t, "pen.gl_texture29", pen_glmultitexture_t, float_to_string( GL_TEXTURE29 ) );
  declareStandardConstant( pen_gl_texture30_t, "pen.gl_texture30", pen_glmultitexture_t, float_to_string( GL_TEXTURE30 ) );
  declareStandardConstant( pen_gl_texture31_t, "pen.gl_texture31", pen_glmultitexture_t, float_to_string( GL_TEXTURE31 ) );
  declareStandardConstant( pen_gl_active_texture_t, "pen.gl_active_texture", pen_glmultitexture_t, float_to_string( GL_ACTIVE_TEXTURE ) );
  declareStandardConstant( pen_gl_client_active_texture_t, "pen.gl_client_active_texture", pen_glmultitexture_t, float_to_string( GL_CLIENT_ACTIVE_TEXTURE ) );
  declareStandardConstant( pen_gl_max_texture_units_t, "pen.gl_max_texture_units", pen_glmultitexture_t, float_to_string( GL_MAX_TEXTURE_UNITS ) );

  declareIdent( pen_gltexturecubemap_t, "pen.gltexturecubemap", integer_t, typeClass  );

  declareStandardConstant( pen_gl_normal_map_t, "pen.gl_normal_map", pen_gltexturecubemap_t, float_to_string( GL_NORMAL_MAP ) );
  declareStandardConstant( pen_gl_reflection_map_t, "pen.gl_reflection_map", pen_gltexturecubemap_t, float_to_string( GL_REFLECTION_MAP ) );
  declareStandardConstant( pen_gl_texture_cube_map_t, "pen.gl_texture_cube_map", pen_gltexturecubemap_t, float_to_string( GL_TEXTURE_CUBE_MAP ) );
  declareStandardConstant( pen_gl_texture_binding_cube_map_t, "pen.gl_texture_binding_cube_map", pen_gltexturecubemap_t, float_to_string( GL_TEXTURE_BINDING_CUBE_MAP ) );
  declareStandardConstant( pen_gl_texture_cube_map_positive_x_t, "pen.gl_texture_cube_map_positive_x", pen_gltexturecubemap_t, float_to_string( GL_TEXTURE_CUBE_MAP_POSITIVE_X ) );
  declareStandardConstant( pen_gl_texture_cube_map_negative_x_t, "pen.gl_texture_cube_map_negative_x", pen_gltexturecubemap_t, float_to_string( GL_TEXTURE_CUBE_MAP_NEGATIVE_X ) );
  declareStandardConstant( pen_gl_texture_cube_map_positive_y_t, "pen.gl_texture_cube_map_positive_y", pen_gltexturecubemap_t, float_to_string( GL_TEXTURE_CUBE_MAP_POSITIVE_Y ) );
  declareStandardConstant( pen_gl_texture_cube_map_negative_y_t, "pen.gl_texture_cube_map_negative_y", pen_gltexturecubemap_t, float_to_string( GL_TEXTURE_CUBE_MAP_NEGATIVE_Y ) );
  declareStandardConstant( pen_gl_texture_cube_map_positive_z_t, "pen.gl_texture_cube_map_positive_z", pen_gltexturecubemap_t, float_to_string( GL_TEXTURE_CUBE_MAP_POSITIVE_Z ) );
  declareStandardConstant( pen_gl_texture_cube_map_negative_z_t, "pen.gl_texture_cube_map_negative_z", pen_gltexturecubemap_t, float_to_string( GL_TEXTURE_CUBE_MAP_NEGATIVE_Z ) );
  declareStandardConstant( pen_gl_proxy_texture_cube_map_t, "pen.gl_proxy_texture_cube_map", pen_gltexturecubemap_t, float_to_string( GL_PROXY_TEXTURE_CUBE_MAP ) );
  declareStandardConstant( pen_gl_max_cube_map_texture_size_t, "pen.gl_max_cube_map_texture_size", pen_gltexturecubemap_t, float_to_string( GL_MAX_CUBE_MAP_TEXTURE_SIZE ) );

  declareIdent( pen_gltexturecomp_t, "pen.gltexturecomp", integer_t, typeClass  );

  declareStandardConstant( pen_gl_compressed_alpha_t, "pen.gl_compressed_alpha", pen_gltexturecomp_t, float_to_string( GL_COMPRESSED_ALPHA ) );
  declareStandardConstant( pen_gl_compressed_luminance_t, "pen.gl_compressed_luminance", pen_gltexturecomp_t, float_to_string( GL_COMPRESSED_LUMINANCE ) );
  declareStandardConstant( pen_gl_compressed_luminance_alpha_t, "pen.gl_compressed_luminance_alpha", pen_gltexturecomp_t, float_to_string( GL_COMPRESSED_LUMINANCE_ALPHA ) );
  declareStandardConstant( pen_gl_compressed_intensity_t, "pen.gl_compressed_intensity", pen_gltexturecomp_t, float_to_string( GL_COMPRESSED_INTENSITY ) );
  declareStandardConstant( pen_gl_compressed_rgb_t, "pen.gl_compressed_rgb", pen_gltexturecomp_t, float_to_string( GL_COMPRESSED_RGB ) );
  declareStandardConstant( pen_gl_compressed_rgba_t, "pen.gl_compressed_rgba", pen_gltexturecomp_t, float_to_string( GL_COMPRESSED_RGBA ) );
  declareStandardConstant( pen_gl_texture_compression_hint_t, "pen.gl_texture_compression_hint", pen_gltexturecomp_t, float_to_string( GL_TEXTURE_COMPRESSION_HINT ) );
  declareStandardConstant( pen_gl_texture_compressed_image_size_t, "pen.gl_texture_compressed_image_size", pen_gltexturecomp_t, float_to_string( GL_TEXTURE_COMPRESSED_IMAGE_SIZE ) );
  declareStandardConstant( pen_gl_texture_compressed_t, "pen.gl_texture_compressed", pen_gltexturecomp_t, float_to_string( GL_TEXTURE_COMPRESSED ) );
  declareStandardConstant( pen_gl_num_compressed_texture_formats_t, "pen.gl_num_compressed_texture_formats", pen_gltexturecomp_t, float_to_string( GL_NUM_COMPRESSED_TEXTURE_FORMATS ) );
  declareStandardConstant( pen_gl_compressed_texture_formats_t, "pen.gl_compressed_texture_formats", pen_gltexturecomp_t, float_to_string( GL_COMPRESSED_TEXTURE_FORMATS ) );

  declareIdent( pen_glmultisample_t, "pen.glmultisample", integer_t, typeClass  );

  declareStandardConstant( pen_gl_multisample_t, "pen.gl_multisample", pen_glmultisample_t, float_to_string( GL_MULTISAMPLE ) );
  declareStandardConstant( pen_gl_sample_alpha_to_coverage_t, "pen.gl_sample_alpha_to_coverage", pen_glmultisample_t, float_to_string( GL_SAMPLE_ALPHA_TO_COVERAGE ) );
  declareStandardConstant( pen_gl_sample_alpha_to_one_t, "pen.gl_sample_alpha_to_one", pen_glmultisample_t, float_to_string( GL_SAMPLE_ALPHA_TO_ONE ) );
  declareStandardConstant( pen_gl_sample_coverage_t, "pen.gl_sample_coverage", pen_glmultisample_t, float_to_string( GL_SAMPLE_COVERAGE ) );
  declareStandardConstant( pen_gl_sample_buffers_t, "pen.gl_sample_buffers", pen_glmultisample_t, float_to_string( GL_SAMPLE_BUFFERS ) );
  declareStandardConstant( pen_gl_samples_t, "pen.gl_samples", pen_glmultisample_t, float_to_string( GL_SAMPLES ) );
  declareStandardConstant( pen_gl_sample_coverage_value_t, "pen.gl_sample_coverage_value", pen_glmultisample_t, float_to_string( GL_SAMPLE_COVERAGE_VALUE ) );
  declareStandardConstant( pen_gl_sample_coverage_invert_t, "pen.gl_sample_coverage_invert", pen_glmultisample_t, float_to_string( GL_SAMPLE_COVERAGE_INVERT ) );
  declareStandardConstant( pen_gl_multisample_bit_t, "pen.gl_multisample_bit", pen_gluint_t, float_to_string( GL_MULTISAMPLE_BIT ) );

  declareIdent( pen_gltransposemat_t, "pen.gltransposemat", integer_t, typeClass  );

  declareStandardConstant( pen_gl_transpose_modelview_matrix_t, "pen.gl_transpose_modelview_matrix", pen_gltransposemat_t, float_to_string( GL_TRANSPOSE_MODELVIEW_MATRIX ) );
  declareStandardConstant( pen_gl_transpose_projection_matrix_t, "pen.gl_transpose_projection_matrix", pen_gltransposemat_t, float_to_string( GL_TRANSPOSE_PROJECTION_MATRIX ) );
  declareStandardConstant( pen_gl_transpose_texture_matrix_t, "pen.gl_transpose_texture_matrix", pen_gltransposemat_t, float_to_string( GL_TRANSPOSE_TEXTURE_MATRIX ) );
  declareStandardConstant( pen_gl_transpose_color_matrix_t, "pen.gl_transpose_color_matrix", pen_gltransposemat_t, float_to_string( GL_TRANSPOSE_COLOR_MATRIX ) );

  declareIdent( pen_gltextureenvcomb_t, "pen.gltextureenvcomb", integer_t, typeClass  );

  declareStandardConstant( pen_gl_combine_t, "pen.gl_combine", pen_gltextureenvcomb_t, float_to_string( GL_COMBINE ) );
  declareStandardConstant( pen_gl_combine_rgb_t, "pen.gl_combine_rgb", pen_gltextureenvcomb_t, float_to_string( GL_COMBINE_RGB ) );
  declareStandardConstant( pen_gl_combine_alpha_t, "pen.gl_combine_alpha", pen_gltextureenvcomb_t, float_to_string( GL_COMBINE_ALPHA ) );
  declareStandardConstant( pen_gl_source0_rgb_t, "pen.gl_source0_rgb", pen_gltextureenvcomb_t, float_to_string( GL_SOURCE0_RGB ) );
  declareStandardConstant( pen_gl_source1_rgb_t, "pen.gl_source1_rgb", pen_gltextureenvcomb_t, float_to_string( GL_SOURCE1_RGB ) );
  declareStandardConstant( pen_gl_source2_rgb_t, "pen.gl_source2_rgb", pen_gltextureenvcomb_t, float_to_string( GL_SOURCE2_RGB ) );
  declareStandardConstant( pen_gl_source0_alpha_t, "pen.gl_source0_alpha", pen_gltextureenvcomb_t, float_to_string( GL_SOURCE0_ALPHA ) );
  declareStandardConstant( pen_gl_source1_alpha_t, "pen.gl_source1_alpha", pen_gltextureenvcomb_t, float_to_string( GL_SOURCE1_ALPHA ) );
  declareStandardConstant( pen_gl_source2_alpha_t, "pen.gl_source2_alpha", pen_gltextureenvcomb_t, float_to_string( GL_SOURCE2_ALPHA ) );
  declareStandardConstant( pen_gl_operand0_rgb_t, "pen.gl_operand0_rgb", pen_gltextureenvcomb_t, float_to_string( GL_OPERAND0_RGB ) );
  declareStandardConstant( pen_gl_operand1_rgb_t, "pen.gl_operand1_rgb", pen_gltextureenvcomb_t, float_to_string( GL_OPERAND1_RGB ) );
  declareStandardConstant( pen_gl_operand2_rgb_t, "pen.gl_operand2_rgb", pen_gltextureenvcomb_t, float_to_string( GL_OPERAND2_RGB ) );
  declareStandardConstant( pen_gl_operand0_alpha_t, "pen.gl_operand0_alpha", pen_gltextureenvcomb_t, float_to_string( GL_OPERAND0_ALPHA ) );
  declareStandardConstant( pen_gl_operand1_alpha_t, "pen.gl_operand1_alpha", pen_gltextureenvcomb_t, float_to_string( GL_OPERAND1_ALPHA ) );
  declareStandardConstant( pen_gl_operand2_alpha_t, "pen.gl_operand2_alpha", pen_gltextureenvcomb_t, float_to_string( GL_OPERAND2_ALPHA ) );
  declareStandardConstant( pen_gl_rgb_scale_t, "pen.gl_rgb_scale", pen_gltextureenvcomb_t, float_to_string( GL_RGB_SCALE ) );
  declareStandardConstant( pen_gl_add_signed_t, "pen.gl_add_signed", pen_gltextureenvcomb_t, float_to_string( GL_ADD_SIGNED ) );
  declareStandardConstant( pen_gl_interpolate_t, "pen.gl_interpolate", pen_gltextureenvcomb_t, float_to_string( GL_INTERPOLATE ) );
  declareStandardConstant( pen_gl_subtract_t, "pen.gl_subtract", pen_gltextureenvcomb_t, float_to_string( GL_SUBTRACT ) );
  declareStandardConstant( pen_gl_constant_t, "pen.gl_constant", pen_gltextureenvcomb_t, float_to_string( GL_CONSTANT ) );
  declareStandardConstant( pen_gl_primary_color_t, "pen.gl_primary_color", pen_gltextureenvcomb_t, float_to_string( GL_PRIMARY_COLOR ) );
  declareStandardConstant( pen_gl_previous_t, "pen.gl_previous", pen_gltextureenvcomb_t, float_to_string( GL_PREVIOUS ) );

  declareIdent( pen_gltextureenvdot3_t, "pen.gltextureenvdot3", integer_t, typeClass  );

  declareStandardConstant( pen_gl_dot3_rgb_t, "pen.gl_dot3_rgb", pen_gltextureenvdot3_t, float_to_string( GL_DOT3_RGB ) );
  declareStandardConstant( pen_gl_dot3_rgba_t, "pen.gl_dot3_rgba", pen_gltextureenvdot3_t, float_to_string( GL_DOT3_RGBA ) );
  declareStandardConstant( pen_gl_clamp_to_border_t, "pen.gl_clamp_to_border", pen_glushort_t, float_to_string( GL_CLAMP_TO_BORDER ) );

  declareIdent( pen_glmultitexturearb_t, "pen.glmultitexturearb", integer_t, typeClass  );

  declareStandardConstant( pen_gl_texture0_arb_t, "pen.gl_texture0_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE0_ARB ) );
  declareStandardConstant( pen_gl_texture1_arb_t, "pen.gl_texture1_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE1_ARB ) );
  declareStandardConstant( pen_gl_texture2_arb_t, "pen.gl_texture2_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE2_ARB ) );
  declareStandardConstant( pen_gl_texture3_arb_t, "pen.gl_texture3_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE3_ARB ) );
  declareStandardConstant( pen_gl_texture4_arb_t, "pen.gl_texture4_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE4_ARB ) );
  declareStandardConstant( pen_gl_texture5_arb_t, "pen.gl_texture5_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE5_ARB ) );
  declareStandardConstant( pen_gl_texture6_arb_t, "pen.gl_texture6_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE6_ARB ) );
  declareStandardConstant( pen_gl_texture7_arb_t, "pen.gl_texture7_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE7_ARB ) );
  declareStandardConstant( pen_gl_texture8_arb_t, "pen.gl_texture8_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE8_ARB ) );
  declareStandardConstant( pen_gl_texture9_arb_t, "pen.gl_texture9_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE9_ARB ) );
  declareStandardConstant( pen_gl_texture10_arb_t, "pen.gl_texture10_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE10_ARB ) );
  declareStandardConstant( pen_gl_texture11_arb_t, "pen.gl_texture11_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE11_ARB ) );
  declareStandardConstant( pen_gl_texture12_arb_t, "pen.gl_texture12_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE12_ARB ) );
  declareStandardConstant( pen_gl_texture13_arb_t, "pen.gl_texture13_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE13_ARB ) );
  declareStandardConstant( pen_gl_texture14_arb_t, "pen.gl_texture14_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE14_ARB ) );
  declareStandardConstant( pen_gl_texture15_arb_t, "pen.gl_texture15_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE15_ARB ) );
  declareStandardConstant( pen_gl_texture16_arb_t, "pen.gl_texture16_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE16_ARB ) );
  declareStandardConstant( pen_gl_texture17_arb_t, "pen.gl_texture17_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE17_ARB ) );
  declareStandardConstant( pen_gl_texture18_arb_t, "pen.gl_texture18_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE18_ARB ) );
  declareStandardConstant( pen_gl_texture19_arb_t, "pen.gl_texture19_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE19_ARB ) );
  declareStandardConstant( pen_gl_texture20_arb_t, "pen.gl_texture20_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE20_ARB ) );
  declareStandardConstant( pen_gl_texture21_arb_t, "pen.gl_texture21_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE21_ARB ) );
  declareStandardConstant( pen_gl_texture22_arb_t, "pen.gl_texture22_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE22_ARB ) );
  declareStandardConstant( pen_gl_texture23_arb_t, "pen.gl_texture23_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE23_ARB ) );
  declareStandardConstant( pen_gl_texture24_arb_t, "pen.gl_texture24_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE24_ARB ) );
  declareStandardConstant( pen_gl_texture25_arb_t, "pen.gl_texture25_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE25_ARB ) );
  declareStandardConstant( pen_gl_texture26_arb_t, "pen.gl_texture26_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE26_ARB ) );
  declareStandardConstant( pen_gl_texture27_arb_t, "pen.gl_texture27_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE27_ARB ) );
  declareStandardConstant( pen_gl_texture28_arb_t, "pen.gl_texture28_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE28_ARB ) );
  declareStandardConstant( pen_gl_texture29_arb_t, "pen.gl_texture29_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE29_ARB ) );
  declareStandardConstant( pen_gl_texture30_arb_t, "pen.gl_texture30_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE30_ARB ) );
  declareStandardConstant( pen_gl_texture31_arb_t, "pen.gl_texture31_arb", pen_glmultitexturearb_t, float_to_string( GL_TEXTURE31_ARB ) );
  declareStandardConstant( pen_gl_active_texture_arb_t, "pen.gl_active_texture_arb", pen_glmultitexturearb_t, float_to_string( GL_ACTIVE_TEXTURE_ARB ) );
  declareStandardConstant( pen_gl_client_active_texture_arb_t, "pen.gl_client_active_texture_arb", pen_glmultitexturearb_t, float_to_string( GL_CLIENT_ACTIVE_TEXTURE_ARB ) );
  declareStandardConstant( pen_gl_max_texture_units_arb_t, "pen.gl_max_texture_units_arb", pen_glmultitexturearb_t, float_to_string( GL_MAX_TEXTURE_UNITS_ARB ) );

  declareIdent( pen_gldebugshaders_t, "pen.gldebugshaders", integer_t, typeClass  );

  declareStandardConstant( pen_gl_debug_object_mesa_t, "pen.gl_debug_object_mesa", pen_gldebugshaders_t, float_to_string( GL_DEBUG_OBJECT_MESA ) );
  declareStandardConstant( pen_gl_debug_print_mesa_t, "pen.gl_debug_print_mesa", pen_gldebugshaders_t, float_to_string( GL_DEBUG_PRINT_MESA ) );
  declareStandardConstant( pen_gl_debug_assert_mesa_t, "pen.gl_debug_assert_mesa", pen_gldebugshaders_t, float_to_string( GL_DEBUG_ASSERT_MESA ) );

  declareIdent( pen_globsolete_t, "pen.globsolete", integer_t, typeClass  );

  declareStandardConstant( pen_gl_depth_stencil_mesa_t, "pen.gl_depth_stencil_mesa", pen_globsolete_t, float_to_string( GL_DEPTH_STENCIL_MESA ) );
  declareStandardConstant( pen_gl_unsigned_int_24_8_mesa_t, "pen.gl_unsigned_int_24_8_mesa", pen_globsolete_t, float_to_string( GL_UNSIGNED_INT_24_8_MESA ) );
  declareStandardConstant( pen_gl_unsigned_int_8_24_rev_mesa_t, "pen.gl_unsigned_int_8_24_rev_mesa", pen_globsolete_t, float_to_string( GL_UNSIGNED_INT_8_24_REV_MESA ) );
  declareStandardConstant( pen_gl_unsigned_short_15_1_mesa_t, "pen.gl_unsigned_short_15_1_mesa", pen_globsolete_t, float_to_string( GL_UNSIGNED_SHORT_15_1_MESA ) );
  declareStandardConstant( pen_gl_unsigned_short_1_15_rev_mesa_t, "pen.gl_unsigned_short_1_15_rev_mesa", pen_globsolete_t, float_to_string( GL_UNSIGNED_SHORT_1_15_REV_MESA ) );
  declareStandardConstant( pen_gl_fragment_program_position_mesa_t, "pen.gl_fragment_program_position_mesa", pen_globsolete_t, float_to_string( GL_FRAGMENT_PROGRAM_POSITION_MESA ) );
  declareStandardConstant( pen_gl_fragment_program_callback_mesa_t, "pen.gl_fragment_program_callback_mesa", pen_globsolete_t, float_to_string( GL_FRAGMENT_PROGRAM_CALLBACK_MESA ) );
  declareStandardConstant( pen_gl_fragment_program_callback_func_mesa_t, "pen.gl_fragment_program_callback_func_mesa", pen_globsolete_t, float_to_string( GL_FRAGMENT_PROGRAM_CALLBACK_FUNC_MESA ) );
  declareStandardConstant( pen_gl_fragment_program_callback_data_mesa_t, "pen.gl_fragment_program_callback_data_mesa", pen_globsolete_t, float_to_string( GL_FRAGMENT_PROGRAM_CALLBACK_DATA_MESA ) );
  declareStandardConstant( pen_gl_vertex_program_position_mesa_t, "pen.gl_vertex_program_position_mesa", pen_globsolete_t, float_to_string( GL_VERTEX_PROGRAM_POSITION_MESA ) );
  declareStandardConstant( pen_gl_vertex_program_callback_mesa_t, "pen.gl_vertex_program_callback_mesa", pen_globsolete_t, float_to_string( GL_VERTEX_PROGRAM_CALLBACK_MESA ) );
  declareStandardConstant( pen_gl_vertex_program_callback_func_mesa_t, "pen.gl_vertex_program_callback_func_mesa", pen_globsolete_t, float_to_string( GL_VERTEX_PROGRAM_CALLBACK_FUNC_MESA ) );
  declareStandardConstant( pen_gl_vertex_program_callback_data_mesa_t, "pen.gl_vertex_program_callback_data_mesa", pen_globsolete_t, float_to_string( GL_VERTEX_PROGRAM_CALLBACK_DATA_MESA ) );
  declareStandardConstant( pen_gl_texture_1d_array_ext_t, "pen.gl_texture_1d_array_ext", pen_glushort_t, float_to_string( GL_TEXTURE_1D_ARRAY_EXT ) );
  declareStandardConstant( pen_gl_proxy_texture_1d_array_ext_t, "pen.gl_proxy_texture_1d_array_ext", pen_glushort_t, float_to_string( GL_PROXY_TEXTURE_1D_ARRAY_EXT ) );
  declareStandardConstant( pen_gl_texture_2d_array_ext_t, "pen.gl_texture_2d_array_ext", pen_glushort_t, float_to_string( GL_TEXTURE_2D_ARRAY_EXT ) );
  declareStandardConstant( pen_gl_proxy_texture_2d_array_ext_t, "pen.gl_proxy_texture_2d_array_ext", pen_glushort_t, float_to_string( GL_PROXY_TEXTURE_2D_ARRAY_EXT ) );
  declareStandardConstant( pen_gl_texture_binding_1d_array_ext_t, "pen.gl_texture_binding_1d_array_ext", pen_glushort_t, float_to_string( GL_TEXTURE_BINDING_1D_ARRAY_EXT ) );
  declareStandardConstant( pen_gl_texture_binding_2d_array_ext_t, "pen.gl_texture_binding_2d_array_ext", pen_glushort_t, float_to_string( GL_TEXTURE_BINDING_2D_ARRAY_EXT ) );
  declareStandardConstant( pen_gl_max_array_texture_layers_ext_t, "pen.gl_max_array_texture_layers_ext", pen_glushort_t, float_to_string( GL_MAX_ARRAY_TEXTURE_LAYERS_EXT ) );
  declareStandardConstant( pen_gl_framebuffer_attachment_texture_layer_ext_t, "pen.gl_framebuffer_attachment_texture_layer_ext", pen_glushort_t, float_to_string( GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT ) );
  declareStandardConstant( pen_gl_alpha_blend_equation_ati_t, "pen.gl_alpha_blend_equation_ati", pen_glushort_t, float_to_string( GL_ALPHA_BLEND_EQUATION_ATI ) );
  declareStandardConstant( pen_glu_ext_object_space_tess_t, "pen.glu_ext_object_space_tess", pen_glenum_t, float_to_string( GLU_EXT_object_space_tess ) );
  declareStandardConstant( pen_glu_ext_nurbs_tessellator_t, "pen.glu_ext_nurbs_tessellator", pen_glenum_t, float_to_string( GLU_EXT_nurbs_tessellator ) );
  declareStandardConstant( pen_glu_false_t, "pen.glu_false", pen_glenum_t, float_to_string( GLU_FALSE ) );
  declareStandardConstant( pen_glu_true_t, "pen.glu_true", pen_glenum_t, float_to_string( GLU_TRUE ) );
  declareStandardConstant( pen_glu_version_1_1_t, "pen.glu_version_1_1", pen_glenum_t, float_to_string( GLU_VERSION_1_1 ) );
  declareStandardConstant( pen_glu_version_1_2_t, "pen.glu_version_1_2", pen_glenum_t, float_to_string( GLU_VERSION_1_2 ) );
  declareStandardConstant( pen_glu_version_1_3_t, "pen.glu_version_1_3", pen_glenum_t, float_to_string( GLU_VERSION_1_3 ) );
  declareStandardConstant( pen_glu_version_t, "pen.glu_version", pen_glenum_t, float_to_string( GLU_VERSION ) );
  declareStandardConstant( pen_glu_extensions_t, "pen.glu_extensions", pen_glenum_t, float_to_string( GLU_EXTENSIONS ) );
  declareStandardConstant( pen_glu_invalid_enum_t, "pen.glu_invalid_enum", pen_glenum_t, float_to_string( GLU_INVALID_ENUM ) );
  declareStandardConstant( pen_glu_invalid_value_t, "pen.glu_invalid_value", pen_glenum_t, float_to_string( GLU_INVALID_VALUE ) );
  declareStandardConstant( pen_glu_out_of_memory_t, "pen.glu_out_of_memory", pen_glenum_t, float_to_string( GLU_OUT_OF_MEMORY ) );
  declareStandardConstant( pen_glu_incompatible_gl_version_t, "pen.glu_incompatible_gl_version", pen_glenum_t, float_to_string( GLU_INCOMPATIBLE_GL_VERSION ) );
  declareStandardConstant( pen_glu_invalid_operation_t, "pen.glu_invalid_operation", pen_glenum_t, float_to_string( GLU_INVALID_OPERATION ) );
  declareStandardConstant( pen_glu_outline_polygon_t, "pen.glu_outline_polygon", pen_glenum_t, float_to_string( GLU_OUTLINE_POLYGON ) );
  declareStandardConstant( pen_glu_outline_patch_t, "pen.glu_outline_patch", pen_glenum_t, float_to_string( GLU_OUTLINE_PATCH ) );

  declareIdent( pen_glunurbscallbacks_t, "pen.glunurbscallbacks", integer_t, typeClass  );

  declareStandardConstant( pen_glu_nurbs_error_t, "pen.glu_nurbs_error", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_ERROR ) );
  declareStandardConstant( pen_glu_error_t, "pen.glu_error", pen_glunurbscallbacks_t, float_to_string( GLU_ERROR ) );
  declareStandardConstant( pen_glu_nurbs_begin_t, "pen.glu_nurbs_begin", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_BEGIN ) );
  declareStandardConstant( pen_glu_nurbs_begin_ext_t, "pen.glu_nurbs_begin_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_BEGIN_EXT ) );
  declareStandardConstant( pen_glu_nurbs_vertex_t, "pen.glu_nurbs_vertex", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_VERTEX ) );
  declareStandardConstant( pen_glu_nurbs_vertex_ext_t, "pen.glu_nurbs_vertex_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_VERTEX_EXT ) );
  declareStandardConstant( pen_glu_nurbs_normal_t, "pen.glu_nurbs_normal", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_NORMAL ) );
  declareStandardConstant( pen_glu_nurbs_normal_ext_t, "pen.glu_nurbs_normal_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_NORMAL_EXT ) );
  declareStandardConstant( pen_glu_nurbs_color_t, "pen.glu_nurbs_color", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_COLOR ) );
  declareStandardConstant( pen_glu_nurbs_color_ext_t, "pen.glu_nurbs_color_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_COLOR_EXT ) );
  declareStandardConstant( pen_glu_nurbs_texture_coord_t, "pen.glu_nurbs_texture_coord", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_TEXTURE_COORD ) );
  declareStandardConstant( pen_glu_nurbs_tex_coord_ext_t, "pen.glu_nurbs_tex_coord_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_TEX_COORD_EXT ) );
  declareStandardConstant( pen_glu_nurbs_end_t, "pen.glu_nurbs_end", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_END ) );
  declareStandardConstant( pen_glu_nurbs_end_ext_t, "pen.glu_nurbs_end_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_END_EXT ) );
  declareStandardConstant( pen_glu_nurbs_begin_data_t, "pen.glu_nurbs_begin_data", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_BEGIN_DATA ) );
  declareStandardConstant( pen_glu_nurbs_begin_data_ext_t, "pen.glu_nurbs_begin_data_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_BEGIN_DATA_EXT ) );
  declareStandardConstant( pen_glu_nurbs_vertex_data_t, "pen.glu_nurbs_vertex_data", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_VERTEX_DATA ) );
  declareStandardConstant( pen_glu_nurbs_vertex_data_ext_t, "pen.glu_nurbs_vertex_data_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_VERTEX_DATA_EXT ) );
  declareStandardConstant( pen_glu_nurbs_normal_data_t, "pen.glu_nurbs_normal_data", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_NORMAL_DATA ) );
  declareStandardConstant( pen_glu_nurbs_normal_data_ext_t, "pen.glu_nurbs_normal_data_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_NORMAL_DATA_EXT ) );
  declareStandardConstant( pen_glu_nurbs_color_data_t, "pen.glu_nurbs_color_data", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_COLOR_DATA ) );
  declareStandardConstant( pen_glu_nurbs_color_data_ext_t, "pen.glu_nurbs_color_data_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_COLOR_DATA_EXT ) );
  declareStandardConstant( pen_glu_nurbs_texture_coord_data_t, "pen.glu_nurbs_texture_coord_data", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_TEXTURE_COORD_DATA ) );
  declareStandardConstant( pen_glu_nurbs_tex_coord_data_ext_t, "pen.glu_nurbs_tex_coord_data_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_TEX_COORD_DATA_EXT ) );
  declareStandardConstant( pen_glu_nurbs_end_data_t, "pen.glu_nurbs_end_data", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_END_DATA ) );
  declareStandardConstant( pen_glu_nurbs_end_data_ext_t, "pen.glu_nurbs_end_data_ext", pen_glunurbscallbacks_t, float_to_string( GLU_NURBS_END_DATA_EXT ) );
  declareStandardConstant( pen_glu_nurbs_error1_t, "pen.glu_nurbs_error1", pen_glenum_t, float_to_string( GLU_NURBS_ERROR1 ) );
  declareStandardConstant( pen_glu_nurbs_error2_t, "pen.glu_nurbs_error2", pen_glenum_t, float_to_string( GLU_NURBS_ERROR2 ) );
  declareStandardConstant( pen_glu_nurbs_error3_t, "pen.glu_nurbs_error3", pen_glenum_t, float_to_string( GLU_NURBS_ERROR3 ) );
  declareStandardConstant( pen_glu_nurbs_error4_t, "pen.glu_nurbs_error4", pen_glenum_t, float_to_string( GLU_NURBS_ERROR4 ) );
  declareStandardConstant( pen_glu_nurbs_error5_t, "pen.glu_nurbs_error5", pen_glenum_t, float_to_string( GLU_NURBS_ERROR5 ) );
  declareStandardConstant( pen_glu_nurbs_error6_t, "pen.glu_nurbs_error6", pen_glenum_t, float_to_string( GLU_NURBS_ERROR6 ) );
  declareStandardConstant( pen_glu_nurbs_error7_t, "pen.glu_nurbs_error7", pen_glenum_t, float_to_string( GLU_NURBS_ERROR7 ) );
  declareStandardConstant( pen_glu_nurbs_error8_t, "pen.glu_nurbs_error8", pen_glenum_t, float_to_string( GLU_NURBS_ERROR8 ) );
  declareStandardConstant( pen_glu_nurbs_error9_t, "pen.glu_nurbs_error9", pen_glenum_t, float_to_string( GLU_NURBS_ERROR9 ) );
  declareStandardConstant( pen_glu_nurbs_error10_t, "pen.glu_nurbs_error10", pen_glenum_t, float_to_string( GLU_NURBS_ERROR10 ) );
  declareStandardConstant( pen_glu_nurbs_error11_t, "pen.glu_nurbs_error11", pen_glenum_t, float_to_string( GLU_NURBS_ERROR11 ) );
  declareStandardConstant( pen_glu_nurbs_error12_t, "pen.glu_nurbs_error12", pen_glenum_t, float_to_string( GLU_NURBS_ERROR12 ) );
  declareStandardConstant( pen_glu_nurbs_error13_t, "pen.glu_nurbs_error13", pen_glenum_t, float_to_string( GLU_NURBS_ERROR13 ) );
  declareStandardConstant( pen_glu_nurbs_error14_t, "pen.glu_nurbs_error14", pen_glenum_t, float_to_string( GLU_NURBS_ERROR14 ) );
  declareStandardConstant( pen_glu_nurbs_error15_t, "pen.glu_nurbs_error15", pen_glenum_t, float_to_string( GLU_NURBS_ERROR15 ) );
  declareStandardConstant( pen_glu_nurbs_error16_t, "pen.glu_nurbs_error16", pen_glenum_t, float_to_string( GLU_NURBS_ERROR16 ) );
  declareStandardConstant( pen_glu_nurbs_error17_t, "pen.glu_nurbs_error17", pen_glenum_t, float_to_string( GLU_NURBS_ERROR17 ) );
  declareStandardConstant( pen_glu_nurbs_error18_t, "pen.glu_nurbs_error18", pen_glenum_t, float_to_string( GLU_NURBS_ERROR18 ) );
  declareStandardConstant( pen_glu_nurbs_error19_t, "pen.glu_nurbs_error19", pen_glenum_t, float_to_string( GLU_NURBS_ERROR19 ) );
  declareStandardConstant( pen_glu_nurbs_error20_t, "pen.glu_nurbs_error20", pen_glenum_t, float_to_string( GLU_NURBS_ERROR20 ) );
  declareStandardConstant( pen_glu_nurbs_error21_t, "pen.glu_nurbs_error21", pen_glenum_t, float_to_string( GLU_NURBS_ERROR21 ) );
  declareStandardConstant( pen_glu_nurbs_error22_t, "pen.glu_nurbs_error22", pen_glenum_t, float_to_string( GLU_NURBS_ERROR22 ) );
  declareStandardConstant( pen_glu_nurbs_error23_t, "pen.glu_nurbs_error23", pen_glenum_t, float_to_string( GLU_NURBS_ERROR23 ) );
  declareStandardConstant( pen_glu_nurbs_error24_t, "pen.glu_nurbs_error24", pen_glenum_t, float_to_string( GLU_NURBS_ERROR24 ) );
  declareStandardConstant( pen_glu_nurbs_error25_t, "pen.glu_nurbs_error25", pen_glenum_t, float_to_string( GLU_NURBS_ERROR25 ) );
  declareStandardConstant( pen_glu_nurbs_error26_t, "pen.glu_nurbs_error26", pen_glenum_t, float_to_string( GLU_NURBS_ERROR26 ) );
  declareStandardConstant( pen_glu_nurbs_error27_t, "pen.glu_nurbs_error27", pen_glenum_t, float_to_string( GLU_NURBS_ERROR27 ) );
  declareStandardConstant( pen_glu_nurbs_error28_t, "pen.glu_nurbs_error28", pen_glenum_t, float_to_string( GLU_NURBS_ERROR28 ) );
  declareStandardConstant( pen_glu_nurbs_error29_t, "pen.glu_nurbs_error29", pen_glenum_t, float_to_string( GLU_NURBS_ERROR29 ) );
  declareStandardConstant( pen_glu_nurbs_error30_t, "pen.glu_nurbs_error30", pen_glenum_t, float_to_string( GLU_NURBS_ERROR30 ) );
  declareStandardConstant( pen_glu_nurbs_error31_t, "pen.glu_nurbs_error31", pen_glenum_t, float_to_string( GLU_NURBS_ERROR31 ) );
  declareStandardConstant( pen_glu_nurbs_error32_t, "pen.glu_nurbs_error32", pen_glenum_t, float_to_string( GLU_NURBS_ERROR32 ) );
  declareStandardConstant( pen_glu_nurbs_error33_t, "pen.glu_nurbs_error33", pen_glenum_t, float_to_string( GLU_NURBS_ERROR33 ) );
  declareStandardConstant( pen_glu_nurbs_error34_t, "pen.glu_nurbs_error34", pen_glenum_t, float_to_string( GLU_NURBS_ERROR34 ) );
  declareStandardConstant( pen_glu_nurbs_error35_t, "pen.glu_nurbs_error35", pen_glenum_t, float_to_string( GLU_NURBS_ERROR35 ) );
  declareStandardConstant( pen_glu_nurbs_error36_t, "pen.glu_nurbs_error36", pen_glenum_t, float_to_string( GLU_NURBS_ERROR36 ) );
  declareStandardConstant( pen_glu_nurbs_error37_t, "pen.glu_nurbs_error37", pen_glenum_t, float_to_string( GLU_NURBS_ERROR37 ) );

  declareIdent( pen_glunurbsproperties_t, "pen.glunurbsproperties", integer_t, typeClass  );

  declareStandardConstant( pen_glu_auto_load_matrix_t, "pen.glu_auto_load_matrix", pen_glunurbsproperties_t, float_to_string( GLU_AUTO_LOAD_MATRIX ) );
  declareStandardConstant( pen_glu_culling_t, "pen.glu_culling", pen_glunurbsproperties_t, float_to_string( GLU_CULLING ) );
  declareStandardConstant( pen_glu_sampling_tolerance_t, "pen.glu_sampling_tolerance", pen_glunurbsproperties_t, float_to_string( GLU_SAMPLING_TOLERANCE ) );
  declareStandardConstant( pen_glu_display_mode_t, "pen.glu_display_mode", pen_glunurbsproperties_t, float_to_string( GLU_DISPLAY_MODE ) );
  declareStandardConstant( pen_glu_parametric_tolerance_t, "pen.glu_parametric_tolerance", pen_glunurbsproperties_t, float_to_string( GLU_PARAMETRIC_TOLERANCE ) );
  declareStandardConstant( pen_glu_sampling_method_t, "pen.glu_sampling_method", pen_glunurbsproperties_t, float_to_string( GLU_SAMPLING_METHOD ) );
  declareStandardConstant( pen_glu_u_step_t, "pen.glu_u_step", pen_glunurbsproperties_t, float_to_string( GLU_U_STEP ) );
  declareStandardConstant( pen_glu_v_step_t, "pen.glu_v_step", pen_glunurbsproperties_t, float_to_string( GLU_V_STEP ) );
  declareStandardConstant( pen_glu_nurbs_mode_t, "pen.glu_nurbs_mode", pen_glunurbsproperties_t, float_to_string( GLU_NURBS_MODE ) );
  declareStandardConstant( pen_glu_nurbs_mode_ext_t, "pen.glu_nurbs_mode_ext", pen_glunurbsproperties_t, float_to_string( GLU_NURBS_MODE_EXT ) );
  declareStandardConstant( pen_glu_nurbs_tessellator_t, "pen.glu_nurbs_tessellator", pen_glunurbsproperties_t, float_to_string( GLU_NURBS_TESSELLATOR ) );
  declareStandardConstant( pen_glu_nurbs_tessellator_ext_t, "pen.glu_nurbs_tessellator_ext", pen_glunurbsproperties_t, float_to_string( GLU_NURBS_TESSELLATOR_EXT ) );
  declareStandardConstant( pen_glu_nurbs_renderer_t, "pen.glu_nurbs_renderer", pen_glunurbsproperties_t, float_to_string( GLU_NURBS_RENDERER ) );
  declareStandardConstant( pen_glu_nurbs_renderer_ext_t, "pen.glu_nurbs_renderer_ext", pen_glunurbsproperties_t, float_to_string( GLU_NURBS_RENDERER_EXT ) );
  declareStandardConstant( pen_glu_object_parametric_error_t, "pen.glu_object_parametric_error:", pen_glenum_t, float_to_string( GLU_OBJECT_PARAMETRIC_ERROR ) );
  declareStandardConstant( pen_glu_object_parametric_error_ext_t, "pen.glu_object_parametric_error_ext", pen_glenum_t, float_to_string( GLU_OBJECT_PARAMETRIC_ERROR_EXT ) );
  declareStandardConstant( pen_glu_object_path_length_t, "pen.glu_object_path_length", pen_glenum_t, float_to_string( GLU_OBJECT_PATH_LENGTH ) );
  declareStandardConstant( pen_glu_object_path_length_ext_t, "pen.glu_object_path_length_ext", pen_glenum_t, float_to_string( GLU_OBJECT_PATH_LENGTH_EXT ) );
  declareStandardConstant( pen_glu_path_length_t, "pen.glu_path_length", pen_glenum_t, float_to_string( GLU_PATH_LENGTH ) );
  declareStandardConstant( pen_glu_parametric_error_t, "pen.glu_parametric_error", pen_glenum_t, float_to_string( GLU_PARAMETRIC_ERROR ) );
  declareStandardConstant( pen_glu_domain_distance_t, "pen.glu_domain_distance", pen_glenum_t, float_to_string( GLU_DOMAIN_DISTANCE ) );

  declareIdent( pen_glevaluators_t, "pen.glevaluators", integer_t, typeClass  );

  declareStandardConstant( pen_glu_map1_trim_2_t, "pen.glu_map1_trim_2", pen_glevaluators_t, float_to_string( GLU_MAP1_TRIM_2 ) );
  declareStandardConstant( pen_glu_map1_trim_3_t, "pen.glu_map1_trim_3", pen_glevaluators_t, float_to_string( GLU_MAP1_TRIM_3 ) );

  declareIdent( pen_gluquaddrawstyle_t, "pen.gluquaddrawstyle", integer_t, typeClass  );

  declareStandardConstant( pen_glu_point_t, "pen.glu_point", pen_gluquaddrawstyle_t, float_to_string( GLU_POINT ) );
  declareStandardConstant( pen_glu_line_t, "pen.glu_line", pen_gluquaddrawstyle_t, float_to_string( GLU_LINE ) );
  declareStandardConstant( pen_glu_fill_t, "pen.glu_fill", pen_gluquaddrawstyle_t, float_to_string( GLU_FILL ) );
  declareStandardConstant( pen_glu_silhouette_t, "pen.glu_silhouette", pen_gluquaddrawstyle_t, float_to_string( GLU_SILHOUETTE ) );

  declareIdent( pen_gluquadricnormal_t, "pen.gluquadricnormal", integer_t, typeClass  );

  declareStandardConstant( pen_glu_smooth_t, "pen.glu_smooth", pen_gluquadricnormal_t, float_to_string( GLU_SMOOTH ) );
  declareStandardConstant( pen_glu_flat_t, "pen.glu_flat", pen_gluquadricnormal_t, float_to_string( GLU_FLAT ) );
  declareStandardConstant( pen_glu_none_t, "pen.glu_none", pen_gluquadricnormal_t, float_to_string( GLU_NONE ) );

  declareIdent( pen_gluquadorientation_t, "pen.gluquadorientation", integer_t, typeClass  );

  declareStandardConstant( pen_glu_outside_t, "pen.glu_outside", pen_gluquadorientation_t, float_to_string( GLU_OUTSIDE ) );
  declareStandardConstant( pen_glu_inside_t, "pen.glu_inside", pen_gluquadorientation_t, float_to_string( GLU_INSIDE ) );

  declareIdent( pen_glutesscallbacks_t, "pen.glutesscallbacks", integer_t, typeClass  );

  declareStandardConstant( pen_glu_tess_begin_t, "pen.glu_tess_begin", pen_glutesscallbacks_t, float_to_string( GLU_TESS_BEGIN ) );
  declareStandardConstant( pen_glu_begin_t, "pen.glu_begin", pen_glutesscallbacks_t, float_to_string( GLU_BEGIN ) );
  declareStandardConstant( pen_glu_tess_vertex_t, "pen.glu_tess_vertex", pen_glutesscallbacks_t, float_to_string( GLU_TESS_VERTEX ) );
  declareStandardConstant( pen_glu_vertex_t, "pen.glu_vertex", pen_glutesscallbacks_t, float_to_string( GLU_VERTEX ) );
  declareStandardConstant( pen_glu_tess_end_t, "pen.glu_tess_end", pen_glutesscallbacks_t, float_to_string( GLU_TESS_END ) );
  declareStandardConstant( pen_glu_end_t, "pen.glu_end", pen_glutesscallbacks_t, float_to_string( GLU_END ) );
  declareStandardConstant( pen_glu_tess_error_t, "pen.glu_tess_error", pen_glutesscallbacks_t, float_to_string( GLU_TESS_ERROR ) );
  declareStandardConstant( pen_glu_tess_edge_flag_t, "pen.glu_tess_edge_flag", pen_glutesscallbacks_t, float_to_string( GLU_TESS_EDGE_FLAG ) );
  declareStandardConstant( pen_glu_edge_flag_t, "pen.glu_edge_flag", pen_glutesscallbacks_t, float_to_string( GLU_EDGE_FLAG ) );
  declareStandardConstant( pen_glu_tess_combine_t, "pen.glu_tess_combine", pen_glutesscallbacks_t, float_to_string( GLU_TESS_COMBINE ) );
  declareStandardConstant( pen_glu_tess_begin_data_t, "pen.glu_tess_begin_data", pen_glutesscallbacks_t, float_to_string( GLU_TESS_BEGIN_DATA ) );
  declareStandardConstant( pen_glu_tess_vertex_data_t, "pen.glu_tess_vertex_data", pen_glutesscallbacks_t, float_to_string( GLU_TESS_VERTEX_DATA ) );
  declareStandardConstant( pen_glu_tess_end_data_t, "pen.glu_tess_end_data", pen_glutesscallbacks_t, float_to_string( GLU_TESS_END_DATA ) );
  declareStandardConstant( pen_glu_tess_error_data_t, "pen.glu_tess_error_data", pen_glutesscallbacks_t, float_to_string( GLU_TESS_ERROR_DATA ) );
  declareStandardConstant( pen_glu_tess_edge_flag_data_t, "pen.glu_tess_edge_flag_data", pen_glutesscallbacks_t, float_to_string( GLU_TESS_EDGE_FLAG_DATA ) );
  declareStandardConstant( pen_glu_tess_combine_data_t, "pen.glu_tess_combine_data", pen_glutesscallbacks_t, float_to_string( GLU_TESS_COMBINE_DATA ) );

  declareIdent( pen_glutesscontour_t, "pen.glutesscontour", integer_t, typeClass  );

  declareStandardConstant( pen_glu_cw_t, "pen.glu_cw", pen_glutesscontour_t, float_to_string( GLU_CW ) );
  declareStandardConstant( pen_glu_ccw_t, "pen.glu_ccw", pen_glutesscontour_t, float_to_string( GLU_CCW ) );
  declareStandardConstant( pen_glu_interior_t, "pen.glu_interior", pen_glutesscontour_t, float_to_string( GLU_INTERIOR ) );
  declareStandardConstant( pen_glu_exterior_t, "pen.glu_exterior", pen_glutesscontour_t, float_to_string( GLU_EXTERIOR ) );
  declareStandardConstant( pen_glu_unknown_t, "pen.glu_unknown", pen_glutesscontour_t, float_to_string( GLU_UNKNOWN ) );

  declareIdent( pen_glutessproperties_t, "pen.glutessproperties", integer_t, typeClass  );

  declareStandardConstant( pen_glu_tess_winding_rule_t, "pen.glu_tess_winding_rule", pen_glutessproperties_t, float_to_string( GLU_TESS_WINDING_RULE ) );
  declareStandardConstant( pen_glu_tess_boundary_only_t, "pen.glu_tess_boundary_only", pen_glutessproperties_t, float_to_string( GLU_TESS_BOUNDARY_ONLY ) );
  declareStandardConstant( pen_glu_tess_tolerance_t, "pen.glu_tess_tolerance", pen_glutessproperties_t, float_to_string( GLU_TESS_TOLERANCE ) );
  declareStandardConstant( pen_glu_tess_error1_t, "pen.glu_tess_error1", pen_glenum_t, float_to_string( GLU_TESS_ERROR1 ) );
  declareStandardConstant( pen_glu_tess_error2_t, "pen.glu_tess_error2", pen_glenum_t, float_to_string( GLU_TESS_ERROR2 ) );
  declareStandardConstant( pen_glu_tess_error3_t, "pen.glu_tess_error3", pen_glenum_t, float_to_string( GLU_TESS_ERROR3 ) );
  declareStandardConstant( pen_glu_tess_error4_t, "pen.glu_tess_error4", pen_glenum_t, float_to_string( GLU_TESS_ERROR4 ) );
  declareStandardConstant( pen_glu_tess_error5_t, "pen.glu_tess_error5", pen_glenum_t, float_to_string( GLU_TESS_ERROR5 ) );
  declareStandardConstant( pen_glu_tess_error6_t, "pen.glu_tess_error6", pen_glenum_t, float_to_string( GLU_TESS_ERROR6 ) );
  declareStandardConstant( pen_glu_tess_error7_t, "pen.glu_tess_error7", pen_glenum_t, float_to_string( GLU_TESS_ERROR7 ) );
  declareStandardConstant( pen_glu_tess_error8_t, "pen.glu_tess_error8", pen_glenum_t, float_to_string( GLU_TESS_ERROR8 ) );
  declareStandardConstant( pen_glu_tess_missing_begin_polygon_t, "pen.glu_tess_missing_begin_polygon", pen_glenum_t, float_to_string( GLU_TESS_MISSING_BEGIN_POLYGON ) );
  declareStandardConstant( pen_glu_tess_missing_begin_contour_t, "pen.glu_tess_missing_begin_contour", pen_glenum_t, float_to_string( GLU_TESS_MISSING_BEGIN_CONTOUR ) );
  declareStandardConstant( pen_glu_tess_missing_end_polygon_t, "pen.glu_tess_missing_end_polygon", pen_glenum_t, float_to_string( GLU_TESS_MISSING_END_POLYGON ) );
  declareStandardConstant( pen_glu_tess_missing_end_contour_t, "pen.glu_tess_missing_end_contour", pen_glenum_t, float_to_string( GLU_TESS_MISSING_END_CONTOUR ) );
  declareStandardConstant( pen_glu_tess_coord_too_large_t, "pen.glu_tess_coord_too_large", pen_glenum_t, float_to_string( GLU_TESS_COORD_TOO_LARGE ) );
  declareStandardConstant( pen_glu_tess_need_combine_callback_t, "pen.glu_tess_need_combine_callback", pen_glenum_t, float_to_string( GLU_TESS_NEED_COMBINE_CALLBACK ) );
  declareStandardConstant( pen_glu_tess_winding_odd_t, "pen.glu_tess_winding_odd", pen_glenum_t, float_to_string( GLU_TESS_WINDING_ODD ) );
  declareStandardConstant( pen_glu_tess_winding_nonzero_t, "pen.glu_tess_winding_nonzero", pen_glenum_t, float_to_string( GLU_TESS_WINDING_NONZERO ) );
  declareStandardConstant( pen_glu_tess_winding_positive_t, "pen.glu_tess_winding_positive", pen_glenum_t, float_to_string( GLU_TESS_WINDING_POSITIVE ) );
  declareStandardConstant( pen_glu_tess_winding_negative_t, "pen.glu_tess_winding_negative", pen_glenum_t, float_to_string( GLU_TESS_WINDING_NEGATIVE ) );
  declareStandardConstant( pen_glu_tess_winding_abs_geq_two_t, "pen.glu_tess_winding_abs_geq_two", pen_glenum_t, float_to_string( GLU_TESS_WINDING_ABS_GEQ_TWO ) );
  declareStandardConstant( pen_glu_tess_max_coord_t, "pen.glu_tess_max_coord", long_float_t, float_to_string( GLU_TESS_MAX_COORD ) );
end declarePenGLConstants;


---  STARTUP PEN
--
-- Initialize the built-in Pen Package, declaring all identifiers.
-----------------------------------------------------------------------------

procedure StartupPen is
begin
  declareNamespace( "pen" );
  declarePenStandardTypes;

  -- declare pen package procedures and functions

  declareProcedure( pen_set_rect_t, "pen.set_rect", ParsePenSetRect'access );
  declareFunction( pen_is_empty_rect_t, "pen.is_empty_rect", ParsePenIsEmptyRect'access );
  declareProcedure( pen_offset_rect_t, "pen.offset_rect", ParsePenOffsetRect'access );
  declareProcedure( pen_inset_rect_t, "pen.inset_rect", ParsePenInsetRect'access );
  declareProcedure( pen_intersect_rect_t, "pen.intersect_rect", ParsePenIntersectRect'access );
  declareFunction( pen_inside_rect_t, "pen.inside_rect", ParsePenInsideRect'access );
  declareFunction( pen_in_rect_t, "pen.in_rect", ParsePenInRect'access );

  declareProcedure( pen_set_pen_mode_t, "pen.set_pen_mode", ParsePenSetPenMode'access );
  declareProcedure( pen_set_pen_ink_t, "pen.set_pen_ink", ParsePenSetPenInk'access );
  declareProcedure( pen_set_pen_brush_t, "pen.set_pen_brush", ParsePenSetPenBrush'access );
  declareProcedure( pen_set_pen_pattern_t, "pen.set_pen_pattern", ParsePenSetPenPattern'access );
  declareFunction( pen_get_pen_mode_t, "pen.get_pen_mode", ParsePenGetPenMode'access );
  declareProcedure( pen_get_pen_ink_t, "pen.get_pen_ink", ParsePenGetPenInk'access );
  declareFunction( pen_get_pen_brush_t, "pen.get_pen_brush", ParsePenGetPenBrush'access );
--  declareFunction( pen_get_pen_pattern_t, "pen.get_pen_pattern" );

  declareProcedure( pen_move_to_t, "pen.move_to", ParsePenMoveTo'access );
  declareProcedure( pen_move_t, "pen.move", ParsePenMove'access );
  declareProcedure( pen_line_to_t, "pen.line_to", ParsePenLineTo'access );
  declareProcedure( pen_line_t, "pen.line", ParsePenLine'access );
  declareProcedure( pen_hline_t, "pen.hline", ParsePenHLine'access );
  declareProcedure( pen_vline_t, "pen.vline", ParsePenVLine'access );

  declareProcedure( pen_frame_rect_t, "pen.frame_rect", ParsePenFrameRect'access );
  declareProcedure( pen_paint_rect_t, "pen.paint_rect", ParsePenPaintRect'access );
  declareProcedure( pen_fill_rect_t, "pen.fill_rect", ParsePenFillRect'access );
  declareProcedure( pen_frame_ellipse_t, "pen.frame_ellipse", ParsePenFrameEllipse'access );
  declareProcedure( pen_paint_ellipse_t, "pen.paint_ellipse", ParsePenPaintEllipse'access );
  declareProcedure( pen_fill_ellipse_t, "pen.fill_ellipse", ParsePenFillEllipse'access );

  declareProcedure( pen_clear_t, "pen.clear", ParsePenClear'access );

  declareProcedure( pen_new_screen_canvas_t, "pen.new_screen_canvas", ParsePenNewScreenCanvas'access );
  declareProcedure( pen_new_window_canvas_t, "pen.new_window_canvas", ParsePenNewWindowCanvas'access );
  declareProcedure( pen_new_gl_screen_canvas_t, "pen.new_gl_screen_canvas", ParsePenNewGLScreenCanvas'access );
  declareProcedure( pen_new_gl_window_canvas_t, "pen.new_gl_window_canvas", ParsePenNewGLWindowCanvas'access );
  declareProcedure( pen_new_canvas_t, "pen.new_canvas", ParsePenNewCanvas'access );
  declareProcedure( pen_save_canvas_t, "pen.save_canvas", ParsePenSaveCanvas'access );
  declareProcedure( pen_set_title_t, "pen.set_title", ParsePenSetTitle'access );
  declareProcedure( pen_close_canvas_t, "pen.close_canvas", ParsePenCloseCanvas'access );

  declareProcedure( pen_wait_to_reveal_t, "pen.wait_to_reveal", ParsePenWaitToReveal'access );
  declareProcedure( pen_reveal_t, "pen.reveal", ParsePenReveal'access );
  declareProcedure( pen_reveal_now_t, "pen.reveal_now", ParsePenRevealNow'access );

  -- declareProcedure( pen_clip_rect_t, "pen.clip_rect" );

  declareFunction( pen_greyscale_t, "pen.greyscale", ParsePenGreyscale'access );
  declareProcedure( pen_blend_t, "pen.blend", ParsePenBlend'access );
  declareProcedure( pen_fade_t, "pen.fade", ParsePenFade'access );

  declareProcedure( pen_plot_t, "pen.plot", ParsePenPlot'access );

  declareProcedure( pen_set_font_t, "pen.set_font" );
  declareProcedure( pen_put_t, "pen.put" );

  -- the declarations of pen color names have been broken out into separate
  -- procedures in order to reduce the frame size.  some machines give a
  -- warning.

  declarePenColorNames1;
  declarePenColorNames2;
  declarePenColorNames3;
  declarePenColorNames4;
  declarePenColorNames5;
  declarePenGLConstants;

-- record handling is limited in business shell
-- TODO: do these when record handling is improved
--  declareIdent( pen_a_display_info_rec_t, "pen.a_display_info_rec", root_record_t, typeClass );
--  identifiers( pen_a_display_info_rec_t ).value := to_unbounded_string( "6" );
 -- declareIdent( pen_a_display_info_rec_textbased_t, "pen.a_display_info_rec.textbased", boolean_t, subClass );
--  identifiers( pen_a_display_info_rec_textbased_t ).field_of := pen_a_display_info_rec_t;
--  identifiers( pen_a_display_info_rec_textbased_t ).value := to_unbounded_string( "1" );
--  declareIdent( pen_a_display_info_rec_h_res_t, "pen.a_display_info_rec.h_res", positive_t, subClass );
--  identifiers( pen_a_display_info_rec_h_res_t ).field_of := pen_a_display_info_rec_t;
--  identifiers( pen_a_display_info_rec_h_res_t ).value := to_unbounded_string( "2" );
--  declareIdent( pen_a_display_info_rec_v_res_t, "pen.a_display_info_rec.v_res", positive_t, subClass );
--  identifiers( pen_a_display_info_rec_v_res_t ).field_of := pen_a_display_info_rec_t;
--  identifiers( pen_a_display_info_rec_v_res_t ).value := to_unbounded_string( "3" );
--  declareIdent( pen_a_display_info_rec_c_res_t, "pen.a_display_info_rec.c_res", positive_t, subClass );
--  identifiers( pen_a_display_info_rec_c_res_t ).field_of := pen_a_display_info_rec_t;
--  identifiers( pen_a_display_info_rec_c_res_t ).value := to_unbounded_string( "4" );
---  declareIdent( pen_a_display_info_rec_p_res_t, "pen.a_display_info_rec.p_res", natural_t, subClass );
--  identifiers( pen_a_display_info_rec_p_res_t ).field_of := pen_a_display_info_rec_t;
--  identifiers( pen_a_display_info_rec_p_res_t ).value := to_unbounded_string( "5" );
--  declareIdent( pen_a_display_info_rec_d_res_t, "pen.a_display_info_rec.d_res", natural_t, subClass );
--  identifiers( pen_a_display_info_rec_d_res_t ).field_of := pen_a_display_info_rec_t;
--  identifiers( pen_a_display_info_rec_d_res_t ).value := to_unbounded_string( "6" );

  -- We don't start the actual pen package unless we need it.
  -- Pen.StartupPen;

  declareProcedure( pen_raise_gl_errors_t, "pen.raise_gl_errors", ParsePenRaiseGlErrors'access );
  declareProcedure( pen_glclearindex_t, "pen.glclearindex" );
  declareProcedure( pen_glclearcolor_t, "pen.glclearcolor", ParsePenglClearColor'access );
  declareProcedure( pen_glclear_t, "pen.glclear", ParsePenglClear'access );
  declareProcedure( pen_glindexmask_t, "pen.glindexmask" );
  declareProcedure( pen_glcolormask_t, "pen.glcolormask" );
  declareProcedure( pen_glalphafunc_t, "pen.glalphafunc" );
  declareProcedure( pen_glblendfunc_t, "pen.glblendfunc", ParsePenglBlendFunc'access );
  declareProcedure( pen_gllogicop_t, "pen.gllogicop" );
  declareProcedure( pen_glcullface_t, "pen.glcullface" );
  declareProcedure( pen_glfrontface_t, "pen.glfrontface" );
  declareProcedure( pen_glpointsize_t, "pen.glpointsize" );
  declareProcedure( pen_gllinewidth_t, "pen.gllinewidth" );
  declareProcedure( pen_gllinestipple_t, "pen.gllinestipple" );
  declareProcedure( pen_glpolygonmode_t, "pen.glpolygonmode" );
  declareProcedure( pen_glpolygonoffset_t, "pen.glpolygonoffset" );
  declareProcedure( pen_glpolygonstipple_t, "pen.glpolygonstipple" );
  declareProcedure( pen_glgetpolygonstipple_t, "pen.glgetpolygonstipple" );
  declareProcedure( pen_gledgeflag_t, "pen.gledgeflag" );
  declareProcedure( pen_gledgeflagv_t, "pen.gledgeflagv" );
  declareProcedure( pen_glscissor_t, "pen.glscissor" );
  declareProcedure( pen_glclipplane_t, "pen.glclipplane" );
  declareProcedure( pen_glgetclipplane_t, "pen.glgetclipplane" );
  declareProcedure( pen_gldrawbuffer_t, "pen.gldrawbuffer" );
  declareProcedure( pen_glreadbuffer_t, "pen.glreadbuffer" );
  declareProcedure( pen_glenable_t, "pen.glenable", ParsePenglEnable'access );
  declareProcedure( pen_gldisable_t, "pen.gldisable", ParsePenglDisable'access );
  declareFunction(  pen_glisenabled_t, "pen.glisenabled", ParsePenglIsEnabled'access );
  declareProcedure( pen_glenableclientstate_t, "pen.glenableclientstate" );
  declareProcedure( pen_gldisableclientstate_t, "pen.gldisableclientstate" );
  declareProcedure( pen_glgetbooleanv_t, "pen.glgetbooleanv" );
  declareProcedure( pen_glgetdoublev_t, "pen.glgetdoublev" );
  declareProcedure( pen_glgetfloatv_t, "pen.glgetfloatv" );
  declareProcedure( pen_glgetintegerv_t, "pen.glgetintegerv" );
  declareProcedure( pen_glpushattrib_t, "pen.glpushattrib" );
  declareProcedure( pen_glpopattrib_t, "pen.glpopattrib" );
  declareProcedure( pen_glpushclientattrib_t, "pen.glpushclientattrib" );
  declareProcedure( pen_glpopclientattrib_t, "pen.glpopclientattrib" );
  declareFunction(  pen_glrendermode_t, "pen.glrendermode" );
  declareFunction(  pen_glgeterror_t, "pen.glgeterror", ParsePenglGetError'access );
  declareFunction(  pen_glgetstring_t, "pen.glgetstring" );
  declareProcedure( pen_glfinish_t, "pen.glfinish", ParsePenglFinish'access );
  declareProcedure( pen_glflush_t, "pen.glflush", ParsePenglFlush'access );
  declareProcedure( pen_glhint_t, "pen.glhint", ParsePenglHint'access );
  declareProcedure( pen_glcleardepth_t, "pen.glcleardepth", ParsePenglClearDepth'access );
  declareProcedure( pen_gldepthfunc_t, "pen.gldepthfunc", ParsePenglDepthFunc'access );
  declareProcedure( pen_gldepthmask_t, "pen.gldepthmask" );
  declareProcedure( pen_gldepthrange_t, "pen.gldepthrange" );
  declareProcedure( pen_glclearaccum_t, "pen.glclearaccum" );
  declareProcedure( pen_glaccum_t, "pen.glaccum" );
  declareProcedure( pen_glmatrixmode_t, "pen.glmatrixmode", ParsePenglMatrixMode'access );
  declareProcedure( pen_glortho_t, "pen.glortho", ParsePenglOrtho'access );
  declareProcedure( pen_glfrustum_t, "pen.glfrustum" );
  declareProcedure( pen_glviewport_t, "pen.glviewport", ParsePenglViewport'access );
  declareProcedure( pen_glpushmatrix_t, "pen.glpushmatrix", ParsePenglPushMatrix'access );
  declareProcedure( pen_glpopmatrix_t, "pen.glpopmatrix", ParsePenglPopMatrix'access );
  declareProcedure( pen_glloadidentity_t, "pen.glloadidentity", ParsePenglLoadIdentity'access );
  declareProcedure( pen_glloadmatrixd_t, "pen.glloadmatrixd" );
  declareProcedure( pen_glloadmatrixf_t, "pen.glloadmatrixf" );
  declareProcedure( pen_glmultmatrixd_t, "pen.glmultmatrixd" );
  declareProcedure( pen_glmultmatrixf_t, "pen.glmultmatrixf" );
  declareProcedure( pen_glrotated_t, "pen.glrotated", ParsePenglRotated'access );
  declareProcedure( pen_glrotatef_t, "pen.glrotatef", ParsePenglRotatef'access );
  declareProcedure( pen_glscaled_t, "pen.glscaled", ParsePenglScaled'access );
  declareProcedure( pen_glscalef_t, "pen.glscalef", ParsePenglScalef'access );
  declareProcedure( pen_gltranslated_t, "pen.gltranslated", ParsePenglTranslated'access );
  declareProcedure( pen_gltranslatef_t, "pen.gltranslatef", ParsePenglTranslatef'access );
  declareFunction(  pen_glislist_t, "pen.glislist" );
  declareProcedure( pen_gldeletelists_t, "pen.gldeletelists" );
  declareFunction(  pen_glgenlists_t, "pen.glgenlists" );
  declareProcedure( pen_glnewlist_t, "pen.glnewlist" );
  declareProcedure( pen_glendlist_t, "pen.glendlist" );
  declareProcedure( pen_glcalllist_t, "pen.glcalllist" );
  declareProcedure( pen_glcalllists_t, "pen.glcalllists" );
  declareProcedure( pen_gllistbase_t, "pen.gllistbase" );
  declareProcedure( pen_glbegin_t, "pen.glbegin", ParsePenglBegin'access );
  declareProcedure( pen_glend_t, "pen.glend", ParsePenglEnd'access );
  declareProcedure( pen_glvertex2d_t, "pen.glvertex2d", ParsePenglVertex2d'access );
  declareProcedure( pen_glvertex2f_t, "pen.glvertex2f", ParsePenglVertex2f'access );
  declareProcedure( pen_glvertex2i_t, "pen.glvertex2i", ParsePenglVertex2i'access );
  declareProcedure( pen_glvertex2s_t, "pen.glvertex2s", ParsePenglVertex2s'access );
  declareProcedure( pen_glvertex3d_t, "pen.glvertex3d", ParsePenglVertex3d'access );
  declareProcedure( pen_glvertex3f_t, "pen.glvertex3f", ParsePenglVertex3f'access );
  declareProcedure( pen_glvertex3i_t, "pen.glvertex3i", ParsePenglVertex3i'access );
  declareProcedure( pen_glvertex3s_t, "pen.glvertex3s", ParsePenglVertex3s'access );
  declareProcedure( pen_glvertex4d_t, "pen.glvertex4d", ParsePenglVertex4d'access );
  declareProcedure( pen_glvertex4f_t, "pen.glvertex4f", ParsePenglVertex4f'access );
  declareProcedure( pen_glvertex4i_t, "pen.glvertex4i", ParsePenglVertex4i'access );
  declareProcedure( pen_glvertex4s_t, "pen.glvertex4s", ParsePenglVertex4s'access );
  declareProcedure( pen_glvertex2dv_t, "pen.glvertex2dv", ParsePenglVertex2dv'access );
  declareProcedure( pen_glvertex2fv_t, "pen.glvertex2fv", ParsePenglVertex2fv'access );
  declareProcedure( pen_glvertex2iv_t, "pen.glvertex2iv", ParsePenglVertex2iv'access );
  declareProcedure( pen_glvertex2sv_t, "pen.glvertex2sv", ParsePenglVertex2sv'access );
  declareProcedure( pen_glvertex3dv_t, "pen.glvertex3dv", ParsePenglVertex3dv'access );
  declareProcedure( pen_glvertex3fv_t, "pen.glvertex3fv", ParsePenglVertex3fv'access );
  declareProcedure( pen_glvertex3iv_t, "pen.glvertex3iv", ParsePenglVertex3iv'access );
  declareProcedure( pen_glvertex3sv_t, "pen.glvertex3sv", ParsePenglVertex3sv'access );
  declareProcedure( pen_glvertex4dv_t, "pen.glvertex4dv", ParsePenglVertex4dv'access );
  declareProcedure( pen_glvertex4fv_t, "pen.glvertex4fv", ParsePenglVertex4fv'access );
  declareProcedure( pen_glvertex4iv_t, "pen.glvertex4iv", ParsePenglVertex4iv'access );
  declareProcedure( pen_glvertex4sv_t, "pen.glvertex4sv", ParsePenglVertex4sv'access );
  declareProcedure( pen_glnormal3b_t, "pen.glnormal3b", ParsePenglNormal3b'access );
  declareProcedure( pen_glnormal3d_t, "pen.glnormal3d", ParsePenglNormal3d'access );
  declareProcedure( pen_glnormal3f_t, "pen.glnormal3f", ParsePenglNormal3f'access );
  declareProcedure( pen_glnormal3i_t, "pen.glnormal3i", ParsePenglNormal3i'access );
  declareProcedure( pen_glnormal3s_t, "pen.glnormal3s", ParsePenglNormal3s'access );
  declareProcedure( pen_glnormal3bv_t, "pen.glnormal3bv" );
  declareProcedure( pen_glnormal3dv_t, "pen.glnormal3dv" );
  declareProcedure( pen_glnormal3fv_t, "pen.glnormal3fv" );
  declareProcedure( pen_glnormal3iv_t, "pen.glnormal3iv" );
  declareProcedure( pen_glnormal3sv_t, "pen.glnormal3sv" );
  declareProcedure( pen_glindexd_t, "pen.glindexd", ParsePenglIndexd'access );
  declareProcedure( pen_glindexf_t, "pen.glindexf", ParsePenglIndexf'access );
  declareProcedure( pen_glindexi_t, "pen.glindexi", ParsePenglIndexi'access );
  declareProcedure( pen_glindexs_t, "pen.glindexs", ParsePenglIndexs'access );
  declareProcedure( pen_glindexub_t, "pen.glindexub" );
  declareProcedure( pen_glindexdv_t, "pen.glindexdv" );
  declareProcedure( pen_glindexfv_t, "pen.glindexfv" );
  declareProcedure( pen_glindexiv_t, "pen.glindexiv" );
  declareProcedure( pen_glindexsv_t, "pen.glindexsv" );
  declareProcedure( pen_glindexubv_t, "pen.glindexubv" );
  declareProcedure( pen_glcolor3_t, "pen.glcolor3", ParsePenglColor3'access );
  declareProcedure( pen_glcolor3b_t, "pen.glcolor3b", ParsePenglColor3b'access );
  declareProcedure( pen_glcolor3d_t, "pen.glcolor3d", ParsePenglColor3d'access );
  declareProcedure( pen_glcolor3f_t, "pen.glcolor3f", ParsePenglColor3f'access );
  declareProcedure( pen_glcolor3i_t, "pen.glcolor3i", ParsePenglColor3i'access );
  declareProcedure( pen_glcolor3s_t, "pen.glcolor3s", ParsePenglColor3s'access );
  declareProcedure( pen_glcolor3ub_t, "pen.glcolor3ub", ParsePenglColor3ub'access );
  declareProcedure( pen_glcolor3ui_t, "pen.glcolor3ui", ParsePenglColor3ui'access );
  declareProcedure( pen_glcolor3us_t, "pen.glcolor3us", ParsePenglColor3us'access );
  declareProcedure( pen_glcolor4_t, "pen.glcolor4", ParsePenglColor4'access );
  declareProcedure( pen_glcolor4b_t, "pen.glcolor4b", ParsePenglColor4b'access );
  declareProcedure( pen_glcolor4d_t, "pen.glcolor4d", ParsePenglColor4d'access );
  declareProcedure( pen_glcolor4f_t, "pen.glcolor4f", ParsePenglColor4f'access );
  declareProcedure( pen_glcolor4i_t, "pen.glcolor4i", ParsePenglColor4i'access );
  declareProcedure( pen_glcolor4s_t, "pen.glcolor4s", ParsePenglColor4s'access );
  declareProcedure( pen_glcolor4ub_t, "pen.glcolor4ub", ParsePenglColor4ub'access );
  declareProcedure( pen_glcolor4ui_t, "pen.glcolor4ui", ParsePenglColor4ui'access );
  declareProcedure( pen_glcolor4us_t, "pen.glcolor4us", ParsePenglColor4us'access );
  declareProcedure( pen_glcolor3bv_t, "pen.glcolor3bv" );
  declareProcedure( pen_glcolor3dv_t, "pen.glcolor3dv" );
  declareProcedure( pen_glcolor3fv_t, "pen.glcolor3fv" );
  declareProcedure( pen_glcolor3iv_t, "pen.glcolor3iv" );
  declareProcedure( pen_glcolor3sv_t, "pen.glcolor3sv" );
  declareProcedure( pen_glcolor3ubv_t, "pen.glcolor3ubv" );
  declareProcedure( pen_glcolor3uiv_t, "pen.glcolor3uiv" );
  declareProcedure( pen_glcolor3usv_t, "pen.glcolor3usv" );
  declareProcedure( pen_glcolor4bv_t, "pen.glcolor4bv" );
  declareProcedure( pen_glcolor4dv_t, "pen.glcolor4dv" );
  declareProcedure( pen_glcolor4fv_t, "pen.glcolor4fv" );
  declareProcedure( pen_glcolor4iv_t, "pen.glcolor4iv" );
  declareProcedure( pen_glcolor4sv_t, "pen.glcolor4sv" );
  declareProcedure( pen_glcolor4ubv_t, "pen.glcolor4ubv" );
  declareProcedure( pen_glcolor4uiv_t, "pen.glcolor4uiv" );
  declareProcedure( pen_glcolor4usv_t, "pen.glcolor4usv" );
  declareProcedure( pen_gltexcoord1d_t, "pen.gltexcoord1d", ParsePenGlTexCoord1d'access );
  declareProcedure( pen_gltexcoord1f_t, "pen.gltexcoord1f", ParsePenGlTexCoord1f'access );
  declareProcedure( pen_gltexcoord1i_t, "pen.gltexcoord1i", ParsePenGlTexCoord1i'access );
  declareProcedure( pen_gltexcoord1s_t, "pen.gltexcoord1s", ParsePenGlTexCoord1s'access );
  declareProcedure( pen_gltexcoord2d_t, "pen.gltexcoord2d", ParsePenGlTexCoord2d'access );
  declareProcedure( pen_gltexcoord2f_t, "pen.gltexcoord2f", ParsePenGlTexCoord2f'access );
  declareProcedure( pen_gltexcoord2i_t, "pen.gltexcoord2i", ParsePenGlTexCoord2i'access );
  declareProcedure( pen_gltexcoord2s_t, "pen.gltexcoord2s", ParsePenGlTexCoord2s'access );
  declareProcedure( pen_gltexcoord3d_t, "pen.gltexcoord3d", ParsePenGlTexCoord3d'access );
  declareProcedure( pen_gltexcoord3f_t, "pen.gltexcoord3f", ParsePenGlTexCoord3f'access );
  declareProcedure( pen_gltexcoord3i_t, "pen.gltexcoord3i", ParsePenGlTexCoord3i'access );
  declareProcedure( pen_gltexcoord3s_t, "pen.gltexcoord3s", ParsePenGlTexCoord3s'access );
  declareProcedure( pen_gltexcoord4d_t, "pen.gltexcoord4d", ParsePenGlTexCoord4d'access );
  declareProcedure( pen_gltexcoord4f_t, "pen.gltexcoord4f", ParsePenGlTexCoord4f'access );
  declareProcedure( pen_gltexcoord4i_t, "pen.gltexcoord4i", ParsePenGlTexCoord4i'access );
  declareProcedure( pen_gltexcoord4s_t, "pen.gltexcoord4s", ParsePenGlTexCoord4s'access );
  declareProcedure( pen_gltexcoord1dv_t, "pen.gltexcoord1dv" );
  declareProcedure( pen_gltexcoord1fv_t, "pen.gltexcoord1fv" );
  declareProcedure( pen_gltexcoord1iv_t, "pen.gltexcoord1iv" );
  declareProcedure( pen_gltexcoord1sv_t, "pen.gltexcoord1sv" );
  declareProcedure( pen_gltexcoord2dv_t, "pen.gltexcoord2dv" );
  declareProcedure( pen_gltexcoord2fv_t, "pen.gltexcoord2fv" );
  declareProcedure( pen_gltexcoord2iv_t, "pen.gltexcoord2iv" );
  declareProcedure( pen_gltexcoord2sv_t, "pen.gltexcoord2sv" );
  declareProcedure( pen_gltexcoord3dv_t, "pen.gltexcoord3dv" );
  declareProcedure( pen_gltexcoord3fv_t, "pen.gltexcoord3fv" );
  declareProcedure( pen_gltexcoord3iv_t, "pen.gltexcoord3iv" );
  declareProcedure( pen_gltexcoord3sv_t, "pen.gltexcoord3sv" );
  declareProcedure( pen_gltexcoord4dv_t, "pen.gltexcoord4dv" );
  declareProcedure( pen_gltexcoord4fv_t, "pen.gltexcoord4fv" );
  declareProcedure( pen_gltexcoord4iv_t, "pen.gltexcoord4iv" );
  declareProcedure( pen_gltexcoord4sv_t, "pen.gltexcoord4sv" );
  declareProcedure( pen_glrasterpos2d_t, "pen.glrasterpos2d", ParsePenglRasterPos2d'access );
  declareProcedure( pen_glrasterpos2f_t, "pen.glrasterpos2f", ParsePenglRasterPos2f'access );
  declareProcedure( pen_glrasterpos2i_t, "pen.glrasterpos2i", ParsePenglRasterPos2i'access );
  declareProcedure( pen_glrasterpos2s_t, "pen.glrasterpos2s", ParsePenglRasterPos2s'access );
  declareProcedure( pen_glrasterpos3d_t, "pen.glrasterpos3d", ParsePenglRasterPos3d'access );
  declareProcedure( pen_glrasterpos3f_t, "pen.glrasterpos3f", ParsePenglRasterPos3f'access );
  declareProcedure( pen_glrasterpos3i_t, "pen.glrasterpos3i", ParsePenglRasterPos3i'access );
  declareProcedure( pen_glrasterpos3s_t, "pen.glrasterpos3s", ParsePenglRasterPos3s'access );
  declareProcedure( pen_glrasterpos4d_t, "pen.glrasterpos4d", ParsePenglRasterPos4d'access );
  declareProcedure( pen_glrasterpos4f_t, "pen.glrasterpos4f", ParsePenglRasterPos4f'access );
  declareProcedure( pen_glrasterpos4i_t, "pen.glrasterpos4i", ParsePenglRasterPos4i'access );
  declareProcedure( pen_glrasterpos4s_t, "pen.glrasterpos4s", ParsePenglRasterPos4s'access );
  declareProcedure( pen_glrasterpos2dv_t, "pen.glrasterpos2dv" );
  declareProcedure( pen_glrasterpos2fv_t, "pen.glrasterpos2fv" );
  declareProcedure( pen_glrasterpos2iv_t, "pen.glrasterpos2iv" );
  declareProcedure( pen_glrasterpos2sv_t, "pen.glrasterpos2sv" );
  declareProcedure( pen_glrasterpos3dv_t, "pen.glrasterpos3dv" );
  declareProcedure( pen_glrasterpos3fv_t, "pen.glrasterpos3fv" );
  declareProcedure( pen_glrasterpos3iv_t, "pen.glrasterpos3iv" );
  declareProcedure( pen_glrasterpos3sv_t, "pen.glrasterpos3sv" );
  declareProcedure( pen_glrasterpos4dv_t, "pen.glrasterpos4dv" );
  declareProcedure( pen_glrasterpos4fv_t, "pen.glrasterpos4fv" );
  declareProcedure( pen_glrasterpos4iv_t, "pen.glrasterpos4iv" );
  declareProcedure( pen_glrasterpos4sv_t, "pen.glrasterpos4sv" );
  declareProcedure( pen_glrectd_t, "pen.glrectd", ParsePenglRectd'access );
  declareProcedure( pen_glrectf_t, "pen.glrectf", ParsePenglRectf'access );
  declareProcedure( pen_glrecti_t, "pen.glrecti", ParsePenglRecti'access );
  declareProcedure( pen_glrects_t, "pen.glrects", ParsePenglRects'access );
  declareProcedure( pen_glrectdv_t, "pen.glrectdv" );
  declareProcedure( pen_glrectfv_t, "pen.glrectfv" );
  declareProcedure( pen_glrectiv_t, "pen.glrectiv" );
  declareProcedure( pen_glrectsv_t, "pen.glrectsv" );
  declareProcedure( pen_glvertexpointer_t, "pen.glvertexpointer" );
  declareProcedure( pen_glnormalpointer_t, "pen.glnormalpointer" );
  declareProcedure( pen_glcolorpointer_t, "pen.glcolorpointer" );
  declareProcedure( pen_glindexpointer_t, "pen.glindexpointer" );
  declareProcedure( pen_gltexcoordpointer_t, "pen.gltexcoordpointer" );
  declareProcedure( pen_gledgeflagpointer_t, "pen.gledgeflagpointer" );
  declareProcedure( pen_glgetpointerv_t, "pen.glgetpointerv" );
  declareProcedure( pen_glarrayelement_t, "pen.glarrayelement" );
  declareProcedure( pen_gldrawarrays_t, "pen.gldrawarrays" );
  declareProcedure( pen_gldrawelements_t, "pen.gldrawelements" );
  declareProcedure( pen_glinterleavedarrays_t, "pen.glinterleavedarrays" );
  declareProcedure( pen_glshademodel_t, "pen.glshademodel", ParsePenglShadeModel'access );
  declareProcedure( pen_gllightf_t, "pen.gllightf", ParsePenglLightf'access );
  declareProcedure( pen_gllighti_t, "pen.gllighti", ParsePenglLighti'access );
  declareProcedure( pen_gllightfv_t, "pen.gllightfv" );
  declareProcedure( pen_gllightiv_t, "pen.gllightiv" );
  declareProcedure( pen_glgetlightfv_t, "pen.glgetlightfv" );
  declareProcedure( pen_glgetlightiv_t, "pen.glgetlightiv" );
  declareProcedure( pen_gllightmodelf_t, "pen.gllightmodelf" );
  declareProcedure( pen_gllightmodeli_t, "pen.gllightmodeli" );
  declareProcedure( pen_gllightmodelfv_t, "pen.gllightmodelfv" );
  declareProcedure( pen_gllightmodeliv_t, "pen.gllightmodeliv" );
  declareProcedure( pen_glmaterialf_t, "pen.glmaterialf" );
  declareProcedure( pen_glmateriali_t, "pen.glmateriali" );
  declareProcedure( pen_glmaterialfv_t, "pen.glmaterialfv" );
  declareProcedure( pen_glmaterialiv_t, "pen.glmaterialiv" );
  declareProcedure( pen_glgetmaterialfv_t, "pen.glgetmaterialfv" );
  declareProcedure( pen_glgetmaterialiv_t, "pen.glgetmaterialiv" );
  declareProcedure( pen_glcolormaterial_t, "pen.glcolormaterial" );
  declareProcedure( pen_glpixelzoom_t, "pen.glpixelzoom" );
  declareProcedure( pen_glpixelstoref_t, "pen.glpixelstoref" );
  declareProcedure( pen_glpixelstorei_t, "pen.glpixelstorei" );
  declareProcedure( pen_glpixeltransferf_t, "pen.glpixeltransferf" );
  declareProcedure( pen_glpixeltransferi_t, "pen.glpixeltransferi" );
  declareProcedure( pen_glpixelmapfv_t, "pen.glpixelmapfv" );
  declareProcedure( pen_glpixelmapuiv_t, "pen.glpixelmapuiv" );
  declareProcedure( pen_glpixelmapusv_t, "pen.glpixelmapusv" );
  declareProcedure( pen_glgetpixelmapfv_t, "pen.glgetpixelmapfv" );
  declareProcedure( pen_glgetpixelmapuiv_t, "pen.glgetpixelmapuiv" );
  declareProcedure( pen_glgetpixelmapusv_t, "pen.glgetpixelmapusv" );
  declareProcedure( pen_glbitmap_t, "pen.glbitmap" );
  declareProcedure( pen_glreadpixels_t, "pen.glreadpixels" );
  declareProcedure( pen_gldrawpixels_t, "pen.gldrawpixels" );
  declareProcedure( pen_glcopypixels_t, "pen.glcopypixels" );
  declareProcedure( pen_glstencilfunc_t, "pen.glstencilfunc" );
  declareProcedure( pen_glstencilmask_t, "pen.glstencilmask" );
  declareProcedure( pen_glstencilop_t, "pen.glstencilop" );
  declareProcedure( pen_glclearstencil_t, "pen.glclearstencil" );
  declareProcedure( pen_gltexgend_t, "pen.gltexgend" );
  declareProcedure( pen_gltexgenf_t, "pen.gltexgenf" );
  declareProcedure( pen_gltexgeni_t, "pen.gltexgeni" );
  declareProcedure( pen_gltexgendv_t, "pen.gltexgendv" );
  declareProcedure( pen_gltexgenfv_t, "pen.gltexgenfv" );
  declareProcedure( pen_gltexgeniv_t, "pen.gltexgeniv" );
  declareProcedure( pen_glgettexgendv_t, "pen.glgettexgendv" );
  declareProcedure( pen_glgettexgenfv_t, "pen.glgettexgenfv" );
  declareProcedure( pen_glgettexgeniv_t, "pen.glgettexgeniv" );
  declareProcedure( pen_gltexenvf_t, "pen.gltexenvf" );
  declareProcedure( pen_gltexenvi_t, "pen.gltexenvi" );
  declareProcedure( pen_gltexenvfv_t, "pen.gltexenvfv" );
  declareProcedure( pen_gltexenviv_t, "pen.gltexenviv" );
  declareProcedure( pen_glgettexenvfv_t, "pen.glgettexenvfv" );
  declareProcedure( pen_glgettexenviv_t, "pen.glgettexenviv" );
  declareProcedure( pen_gltexparameterf_t, "pen.gltexparameterf" );
  declareProcedure( pen_gltexparameteri_t, "pen.gltexparameteri" );
  declareProcedure( pen_gltexparameterfv_t, "pen.gltexparameterfv" );
  declareProcedure( pen_gltexparameteriv_t, "pen.gltexparameteriv" );
  declareProcedure( pen_glgettexparameterfv_t, "pen.glgettexparameterfv" );
  declareProcedure( pen_glgettexparameteriv_t, "pen.glgettexparameteriv" );
  declareProcedure( pen_glgettexlevelparameterfv_t, "pen.glgettexlevelparameterfv" );
  declareProcedure( pen_glgettexlevelparameteriv_t, "pen.glgettexlevelparameteriv" );
  declareProcedure( pen_glteximage1d_t, "pen.glteximage1d" );
  declareProcedure( pen_glteximage2d_t, "pen.glteximage2d" );
  declareProcedure( pen_glgetteximage_t, "pen.glgetteximage" );
  declareProcedure( pen_glgentextures_t, "pen.glgentextures" );
  declareProcedure( pen_gldeletetextures_t, "pen.gldeletetextures" );
  declareProcedure( pen_glbindtexture_t, "pen.glbindtexture" );
  declareProcedure( pen_glprioritizetextures_t, "pen.glprioritizetextures" );
  declareFunction(  pen_glaretexturesresident_t, "pen.glaretexturesresident" );
  declareFunction(  pen_glistexture_t, "pen.glistexture" );
  declareProcedure( pen_gltexsubimage1d_t, "pen.gltexsubimage1d" );
  declareProcedure( pen_gltexsubimage2d_t, "pen.gltexsubimage2d" );
  declareProcedure( pen_glcopyteximage1d_t, "pen.glcopyteximage1d" );
  declareProcedure( pen_glcopyteximage2d_t, "pen.glcopyteximage2d" );
  declareProcedure( pen_glcopytexsubimage1d_t, "pen.glcopytexsubimage1d" );
  declareProcedure( pen_glcopytexsubimage2d_t, "pen.glcopytexsubimage2d" );
  declareProcedure( pen_glmap1d_t, "pen.glmap1d" );
  declareProcedure( pen_glmap1f_t, "pen.glmap1f" );
  declareProcedure( pen_glmap2d_t, "pen.glmap2d" );
  declareProcedure( pen_glmap2f_t, "pen.glmap2f" );
  declareProcedure( pen_glgetmapdv_t, "pen.glgetmapdv" );
  declareProcedure( pen_glgetmapfv_t, "pen.glgetmapfv" );
  declareProcedure( pen_glgetmapiv_t, "pen.glgetmapiv" );
  declareProcedure( pen_glevalcoord1d_t, "pen.glevalcoord1d" );
  declareProcedure( pen_glevalcoord1f_t, "pen.glevalcoord1f" );
  declareProcedure( pen_glevalcoord1dv_t, "pen.glevalcoord1dv" );
  declareProcedure( pen_glevalcoord1fv_t, "pen.glevalcoord1fv" );
  declareProcedure( pen_glevalcoord2d_t, "pen.glevalcoord2d" );
  declareProcedure( pen_glevalcoord2f_t, "pen.glevalcoord2f" );
  declareProcedure( pen_glevalcoord2dv_t, "pen.glevalcoord2dv" );
  declareProcedure( pen_glevalcoord2fv_t, "pen.glevalcoord2fv" );
  declareProcedure( pen_glmapgrid1d_t, "pen.glmapgrid1d" );
  declareProcedure( pen_glmapgrid1f_t, "pen.glmapgrid1f" );
  declareProcedure( pen_glmapgrid2d_t, "pen.glmapgrid2d" );
  declareProcedure( pen_glmapgrid2f_t, "pen.glmapgrid2f" );
  declareProcedure( pen_glevalpoint1_t, "pen.glevalpoint1" );
  declareProcedure( pen_glevalpoint2_t, "pen.glevalpoint2" );
  declareProcedure( pen_glevalmesh1_t, "pen.glevalmesh1" );
  declareProcedure( pen_glevalmesh2_t, "pen.glevalmesh2" );
  declareProcedure( pen_glfogf_t, "pen.glfogf" );
  declareProcedure( pen_glfogi_t, "pen.glfogi" );
  declareProcedure( pen_glfogfv_t, "pen.glfogfv" );
  declareProcedure( pen_glfogiv_t, "pen.glfogiv" );
  declareProcedure( pen_glfeedbackbuffer_t, "pen.glfeedbackbuffer" );
  declareProcedure( pen_glpassthrough_t, "pen.glpassthrough" );
  declareProcedure( pen_glselectbuffer_t, "pen.glselectbuffer" );
  declareProcedure( pen_glinitnames_t, "pen.glinitnames" );
  declareProcedure( pen_glloadname_t, "pen.glloadname" );
  declareProcedure( pen_glpushname_t, "pen.glpushname" );
  declareProcedure( pen_glpopname_t, "pen.glpopname" );
  declareProcedure( pen_gldrawrangeelements_t, "pen.gldrawrangeelements" );
  declareProcedure( pen_glteximage3d_t, "pen.glteximage3d" );
  declareProcedure( pen_gltexsubimage3d_t, "pen.gltexsubimage3d" );
  declareProcedure( pen_glcopytexsubimage3d_t, "pen.glcopytexsubimage3d" );
  declareProcedure( pen_glcolortable_t, "pen.glcolortable" );
  declareProcedure( pen_glcolorsubtable_t, "pen.glcolorsubtable" );
  declareProcedure( pen_glcolortableparameteriv_t, "pen.glcolortableparameteriv" );
  declareProcedure( pen_glcolortableparameterfv_t, "pen.glcolortableparameterfv" );
  declareProcedure( pen_glcopycolorsubtable_t, "pen.glcopycolorsubtable" );
  declareProcedure( pen_glcopycolortable_t, "pen.glcopycolortable" );
  declareProcedure( pen_glgetcolortable_t, "pen.glgetcolortable" );
  declareProcedure( pen_glgetcolortableparameterfv_t, "pen.glgetcolortableparameterfv" );
  declareProcedure( pen_glgetcolortableparameteriv_t, "pen.glgetcolortableparameteriv" );
  declareProcedure( pen_glblendequation_t, "pen.glblendequation" );
  declareProcedure( pen_glblendcolor_t, "pen.glblendcolor" );
  declareProcedure( pen_glhistogram_t, "pen.glhistogram" );
  declareProcedure( pen_glresethistogram_t, "pen.glresethistogram" );
  declareProcedure( pen_glgethistogram_t, "pen.glgethistogram" );
  declareProcedure( pen_glgethistogramparameterfv_t, "pen.glgethistogramparameterfv" );
  declareProcedure( pen_glgethistogramparameteriv_t, "pen.glgethistogramparameteriv" );
  declareProcedure( pen_glminmax_t, "pen.glminmax" );
  declareProcedure( pen_glresetminmax_t, "pen.glresetminmax" );
  declareProcedure( pen_glgetminmax_t, "pen.glgetminmax" );
  declareProcedure( pen_glgetminmaxparameterfv_t, "pen.glgetminmaxparameterfv" );
  declareProcedure( pen_glgetminmaxparameteriv_t, "pen.glgetminmaxparameteriv" );
  declareProcedure( pen_glconvolutionfilter1d_t, "pen.glconvolutionfilter1d" );
  declareProcedure( pen_glconvolutionfilter2d_t, "pen.glconvolutionfilter2d" );
  declareProcedure( pen_glconvolutionparameterf_t, "pen.glconvolutionparameterf" );
  declareProcedure( pen_glconvolutionparameterfv_t, "pen.glconvolutionparameterfv" );
  declareProcedure( pen_glconvolutionparameterfv2_t, "pen.glconvolutionparameterfv2" );
  declareProcedure( pen_glconvolutionparameteri_t, "pen.glconvolutionparameteri" );
  declareProcedure( pen_glconvolutionparameteriv_t, "pen.glconvolutionparameteriv" );
  declareProcedure( pen_glcopyconvolutionfilter1d_t, "pen.glcopyconvolutionfilter1d" );
  declareProcedure( pen_glcopyconvolutionfilter2d_t, "pen.glcopyconvolutionfilter2d" );
  declareProcedure( pen_glgetconvolutionfilter_t, "pen.glgetconvolutionfilter" );
  declareProcedure( pen_glgetconvolutionparameterfv_t, "pen.glgetconvolutionparameterfv" );
  declareProcedure( pen_glgetconvolutionparameteriv_t, "pen.glgetconvolutionparameteriv" );
  declareProcedure( pen_glseparablefilter2d_t, "pen.glseparablefilter2d" );
  declareProcedure( pen_glgetseparablefilter_t, "pen.glgetseparablefilter" );
  declareProcedure( pen_glactivetexture_t, "pen.glactivetexture" );
  declareProcedure( pen_glclientactivetexture_t, "pen.glclientactivetexture" );
  declareProcedure( pen_glcompressedteximage1d_t, "pen.glcompressedteximage1d" );
  declareProcedure( pen_glcompressedteximage2d_t, "pen.glcompressedteximage2d" );
  declareProcedure( pen_glcompressedteximage3d_t, "pen.glcompressedteximage3d" );
  declareProcedure( pen_glcompressedtexsubimage1d_t, "pen.glcompressedtexsubimage1d" );
  declareProcedure( pen_glcompressedtexsubimage2d_t, "pen.glcompressedtexsubimage2d" );
  declareProcedure( pen_glcompressedtexsubimage3d_t, "pen.glcompressedtexsubimage3d" );
  declareProcedure( pen_glgetcompressedteximage_t, "pen.glgetcompressedteximage" );
  declareProcedure( pen_glmultitexcoord1d_t, "pen.glmultitexcoord1d" );
  declareProcedure( pen_glmultitexcoord1dv_t, "pen.glmultitexcoord1dv" );
  declareProcedure( pen_glmultitexcoord1f_t, "pen.glmultitexcoord1f" );
  declareProcedure( pen_glmultitexcoord1fv_t, "pen.glmultitexcoord1fv" );
  declareProcedure( pen_glmultitexcoord1i_t, "pen.glmultitexcoord1i" );
  declareProcedure( pen_glmultitexcoord1iv_t, "pen.glmultitexcoord1iv" );
  declareProcedure( pen_glmultitexcoord1s_t, "pen.glmultitexcoord1s" );
  declareProcedure( pen_glmultitexcoord1sv_t, "pen.glmultitexcoord1sv" );
  declareProcedure( pen_glmultitexcoord2d_t, "pen.glmultitexcoord2d" );
  declareProcedure( pen_glmultitexcoord2dv_t, "pen.glmultitexcoord2dv" );
  declareProcedure( pen_glmultitexcoord2f_t, "pen.glmultitexcoord2f" );
  declareProcedure( pen_glmultitexcoord2fv_t, "pen.glmultitexcoord2fv" );
  declareProcedure( pen_glmultitexcoord2i_t, "pen.glmultitexcoord2i" );
  declareProcedure( pen_glmultitexcoord2iv_t, "pen.glmultitexcoord2iv" );
  declareProcedure( pen_glmultitexcoord2s_t, "pen.glmultitexcoord2s" );
  declareProcedure( pen_glmultitexcoord2sv_t, "pen.glmultitexcoord2sv" );
  declareProcedure( pen_glmultitexcoord3d_t, "pen.glmultitexcoord3d" );
  declareProcedure( pen_glmultitexcoord3dv_t, "pen.glmultitexcoord3dv" );
  declareProcedure( pen_glmultitexcoord3f_t, "pen.glmultitexcoord3f" );
  declareProcedure( pen_glmultitexcoord3fv_t, "pen.glmultitexcoord3fv" );
  declareProcedure( pen_glmultitexcoord3i_t, "pen.glmultitexcoord3i" );
  declareProcedure( pen_glmultitexcoord3iv_t, "pen.glmultitexcoord3iv" );
  declareProcedure( pen_glmultitexcoord3s_t, "pen.glmultitexcoord3s" );
  declareProcedure( pen_glmultitexcoord3sv_t, "pen.glmultitexcoord3sv" );
  declareProcedure( pen_glmultitexcoord4d_t, "pen.glmultitexcoord4d" );
  declareProcedure( pen_glmultitexcoord4dv_t, "pen.glmultitexcoord4dv" );
  declareProcedure( pen_glmultitexcoord4f_t, "pen.glmultitexcoord4f" );
  declareProcedure( pen_glmultitexcoord4fv_t, "pen.glmultitexcoord4fv" );
  declareProcedure( pen_glmultitexcoord4i_t, "pen.glmultitexcoord4i" );
  declareProcedure( pen_glmultitexcoord4iv_t, "pen.glmultitexcoord4iv" );
  declareProcedure( pen_glmultitexcoord4s_t, "pen.glmultitexcoord4s" );
  declareProcedure( pen_glmultitexcoord4sv_t, "pen.glmultitexcoord4sv" );
  declareProcedure( pen_glloadtransposematrixd_t, "pen.glloadtransposematrixd" );
  declareProcedure( pen_glloadtransposematrixf_t, "pen.glloadtransposematrixf" );
  declareProcedure( pen_glmulttransposematrixd_t, "pen.glmulttransposematrixd" );
  declareProcedure( pen_glmulttransposematrixf_t, "pen.glmulttransposematrixf" );
  declareProcedure( pen_glsamplecoverage_t, "pen.glsamplecoverage" );
  declareProcedure( pen_glactivetexturearb_t, "pen.glactivetexturearb" );
  declareProcedure( pen_glclientactivetexturearb_t, "pen.glclientactivetexturearb" );
  declareProcedure( pen_glmultitexcoord1darb_t, "pen.glmultitexcoord1darb" );
  declareProcedure( pen_glmultitexcoord1dvarb_t, "pen.glmultitexcoord1dvarb" );
  declareProcedure( pen_glmultitexcoord1farb_t, "pen.glmultitexcoord1farb" );
  declareProcedure( pen_glmultitexcoord1fvarb_t, "pen.glmultitexcoord1fvarb" );
  declareProcedure( pen_glmultitexcoord1iarb_t, "pen.glmultitexcoord1iarb" );
  declareProcedure( pen_glmultitexcoord1ivarb_t, "pen.glmultitexcoord1ivarb" );
  declareProcedure( pen_glmultitexcoord1sarb_t, "pen.glmultitexcoord1sarb" );
  declareProcedure( pen_glmultitexcoord1svarb_t, "pen.glmultitexcoord1svarb" );
  declareProcedure( pen_glmultitexcoord2darb_t, "pen.glmultitexcoord2darb" );
  declareProcedure( pen_glmultitexcoord2dvarb_t, "pen.glmultitexcoord2dvarb" );
  declareProcedure( pen_glmultitexcoord2farb_t, "pen.glmultitexcoord2farb" );
  declareProcedure( pen_glmultitexcoord2fvarb_t, "pen.glmultitexcoord2fvarb" );
  declareProcedure( pen_glmultitexcoord2iarb_t, "pen.glmultitexcoord2iarb" );
  declareProcedure( pen_glmultitexcoord2ivarb_t, "pen.glmultitexcoord2ivarb" );
  declareProcedure( pen_glmultitexcoord2sarb_t, "pen.glmultitexcoord2sarb" );
  declareProcedure( pen_glmultitexcoord2svarb_t, "pen.glmultitexcoord2svarb" );
  declareProcedure( pen_glmultitexcoord3darb_t, "pen.glmultitexcoord3darb" );
  declareProcedure( pen_glmultitexcoord3dvarb_t, "pen.glmultitexcoord3dvarb" );
  declareProcedure( pen_glmultitexcoord3farb_t, "pen.glmultitexcoord3farb" );
  declareProcedure( pen_glmultitexcoord3fvarb_t, "pen.glmultitexcoord3fvarb" );
  declareProcedure( pen_glmultitexcoord3iarb_t, "pen.glmultitexcoord3iarb" );
  declareProcedure( pen_glmultitexcoord3ivarb_t, "pen.glmultitexcoord3ivarb" );
  declareProcedure( pen_glmultitexcoord3sarb_t, "pen.glmultitexcoord3sarb" );
  declareProcedure( pen_glmultitexcoord3svarb_t, "pen.glmultitexcoord3svarb" );
  declareProcedure( pen_glmultitexcoord4darb_t, "pen.glmultitexcoord4darb" );
  declareProcedure( pen_glmultitexcoord4dvarb_t, "pen.glmultitexcoord4dvarb" );
  declareProcedure( pen_glmultitexcoord4farb_t, "pen.glmultitexcoord4farb" );
  declareProcedure( pen_glmultitexcoord4fvarb_t, "pen.glmultitexcoord4fvarb" );
  declareProcedure( pen_glmultitexcoord4iarb_t, "pen.glmultitexcoord4iarb" );
  declareProcedure( pen_glmultitexcoord4ivarb_t, "pen.glmultitexcoord4ivarb" );
  declareProcedure( pen_glmultitexcoord4sarb_t, "pen.glmultitexcoord4sarb" );
  declareProcedure( pen_glmultitexcoord4svarb_t, "pen.glmultitexcoord4svarb" );
  declareFunction(  pen_glcreatedebugobjectmesa_t, "pen.glcreatedebugobjectmesa" );
  declareProcedure( pen_glcleardebuglogmesa_t, "pen.glcleardebuglogmesa" );
  declareProcedure( pen_glgetdebuglogmesa_t, "pen.glgetdebuglogmesa" );
  declareFunction(  pen_glgetdebugloglengthmesa_t, "pen.glgetdebugloglengthmesa" );
  declareProcedure( pen_glprogramcallbackmesa_t, "pen.glprogramcallbackmesa" );
  declareProcedure( pen_glgetprogramregisterfvmesa_t, "pen.glgetprogramregisterfvmesa" );
  declareProcedure( pen_glframebuffertexturelayerext_t, "pen.glframebuffertexturelayerext" );
  declareProcedure( pen_glblendequationseparateati_t, "pen.glblendequationseparateati" );
  declareProcedure( pen_gleglimagetargettexture2does_t, "pen.gleglimagetargettexture2does" );
  declareProcedure( pen_gleglimagetargetrenderbufferstorageoes_t, "pen.gleglimagetargetrenderbufferstorageoes" );
  declareProcedure( pen_glubegincurve_t, "pen.glubegincurve" );
  declareProcedure( pen_glubeginpolygon_t, "pen.glubeginpolygon" );
  declareProcedure( pen_glubeginsurface_t, "pen.glubeginsurface" );
  declareProcedure( pen_glubegintrim_t, "pen.glubegintrim" );
  declareFunction(  pen_glubuild1dmipmaplevels_t, "pen.glubuild1dmipmaplevels" );
  declareFunction(  pen_glubuild1dmipmaps_t, "pen.glubuild1dmipmaps" );
  declareFunction(  pen_glubuild2dmipmaplevels_t, "pen.glubuild2dmipmaplevels" );
  declareFunction(  pen_glubuild2dmipmaps_t, "pen.glubuild2dmipmaps" );
  declareFunction(  pen_glubuild3dmipmaplevels_t, "pen.glubuild3dmipmaplevels" );
  declareFunction(  pen_glubuild3dmipmaps_t, "pen.glubuild3dmipmaps" );
  declareFunction(  pen_glucheckextension_t, "pen.glucheckextension" );
  declareProcedure( pen_glucylinder_t, "pen.glucylinder" );
  declareProcedure( pen_gludeletenurbsrenderer_t, "pen.gludeletenurbsrenderer" );
  declareProcedure( pen_gludeletequadric_t, "pen.gludeletequadric" );
  declareProcedure( pen_gludeletetess_t, "pen.gludeletetess" );
  declareProcedure( pen_gludisk_t, "pen.gludisk" );
  declareProcedure( pen_gluendcurve_t, "pen.gluendcurve" );
  declareProcedure( pen_gluendpolygon_t, "pen.gluendpolygon" );
  declareProcedure( pen_gluendsurface_t, "pen.gluendsurface" );
  declareProcedure( pen_gluendtrim_t, "pen.gluendtrim" );
  declareFunction(  pen_gluerrorstring_t, "pen.gluerrorstring" );
  declareProcedure( pen_glugetnurbsproperty_t, "pen.glugetnurbsproperty" );
  declareFunction(  pen_glugetstring_t, "pen.glugetstring" );
  declareProcedure( pen_glugettessproperty_t, "pen.glugettessproperty" );
  declareProcedure( pen_gluloadsamplingmatrices_t, "pen.gluloadsamplingmatrices" );
  declareProcedure( pen_glulookat_t, "pen.glulookat" );
  declareFunction(  pen_glunewnurbsrenderer_t, "pen.glunewnurbsrenderer" );
  declareFunction(  pen_glunewquadric_t, "pen.glunewquadric" );
  declareFunction(  pen_glunewtess_t, "pen.glunewtess" );
  declareProcedure( pen_glunextcontour_t, "pen.glunextcontour" );
  declareProcedure( pen_glunurbscallback_t, "pen.glunurbscallback" );
  declareProcedure( pen_glunurbscallbackdata_t, "pen.glunurbscallbackdata" );
  declareProcedure( pen_glunurbscallbackdataext_t, "pen.glunurbscallbackdataext" );
  declareProcedure( pen_glunurbscurve_t, "pen.glunurbscurve" );
  declareProcedure( pen_glunurbsproperty_t, "pen.glunurbsproperty" );
  declareProcedure( pen_glunurbssurface_t, "pen.glunurbssurface" );
  declareProcedure( pen_gluortho2d_t, "pen.gluortho2d" );
  declareProcedure( pen_glupartialdisk_t, "pen.glupartialdisk" );
  declareProcedure( pen_gluperspective_t, "pen.gluperspective", ParsePengluPerspective'access );
  declareProcedure( pen_glupickmatrix_t, "pen.glupickmatrix" );
  declareFunction(  pen_gluproject_t, "pen.gluproject" );
  declareProcedure( pen_glupwlcurve_t, "pen.glupwlcurve" );
  declareProcedure( pen_gluquadriccallback_t, "pen.gluquadriccallback" );
  declareProcedure( pen_gluquadricdrawstyle_t, "pen.gluquadricdrawstyle" );
  declareProcedure( pen_gluquadricnormals_t, "pen.gluquadricnormals" );
  declareProcedure( pen_gluquadricorientation_t, "pen.gluquadricorientation" );
  declareProcedure( pen_gluquadrictexture_t, "pen.gluquadrictexture" );
  declareFunction(  pen_gluscaleimage_t, "pen.gluscaleimage" );
  declareProcedure( pen_glusphere_t, "pen.glusphere" );
  declareProcedure( pen_glutessbegincontour_t, "pen.glutessbegincontour" );
  declareProcedure( pen_glutessbeginpolygon_t, "pen.glutessbeginpolygon" );
  declareProcedure( pen_glutesscallback_t, "pen.glutesscallback" );
  declareProcedure( pen_glutessendcontour_t, "pen.glutessendcontour" );
  declareProcedure( pen_glutessendpolygon_t, "pen.glutessendpolygon" );
  declareProcedure( pen_glutessnormal_t, "pen.glutessnormal" );
  declareProcedure( pen_glutessproperty_t, "pen.glutessproperty" );
  declareProcedure( pen_glutessvertex_t, "pen.glutessvertex" );
  declareFunction(  pen_gluunproject_t, "pen.gluunproject" );
  declareFunction(  pen_gluunproject4_t, "pen.gluunproject4" );

  declareIdent( pen_gl_byte_array_t,  "pen.gl_byte_array", positive_t, typeClass );
  declareProcedure( pen_new_gl_byte_array_t, "pen.new_gl_byte_array", ParsePenNewGLByteArray'access );
  declareProcedure( pen_set_gl_byte_array_t, "pen.set_gl_byte_array", ParsePenSetGLByteArray'access );
  declareIdent( pen_gl_short_array_t,  "pen.gl_short_array", positive_t, typeClass );
  declareProcedure( pen_new_gl_short_array_t, "pen.new_gl_short_array", ParsePenNewGLShortArray'access );
  declareProcedure( pen_set_gl_short_array_t, "pen.set_gl_short_array", ParsePenSetGLShortArray'access );
  declareIdent( pen_gl_int_array_t,  "pen.gl_int_array", positive_t, typeClass );
  declareProcedure( pen_new_gl_int_array_t, "pen.new_gl_int_array", ParsePenNewGLIntArray'access );
  declareProcedure( pen_set_gl_int_array_t, "pen.set_gl_int_array", ParsePenSetGLIntArray'access );
  declareIdent( pen_gl_float_array_t,  "pen.gl_float_array", positive_t, typeClass );
  declareProcedure( pen_new_gl_float_array_t, "pen.new_gl_float_array", ParsePenNewGLFloatArray'access );
  declareProcedure( pen_set_gl_float_array_t, "pen.set_gl_float_array", ParsePenSetGLFloatArray'access );
  declareIdent( pen_gl_double_array_t,  "pen.gl_double_array", positive_t, typeClass );
  declareProcedure( pen_new_gl_double_array_t, "pen.new_gl_double_array", ParsePenNewGLDoubleArray'access );
  declareProcedure( pen_set_gl_double_array_t, "pen.set_gl_double_array", ParsePenSetGLDoubleArray'access );
  declareNamespaceClosed( "pen" );
end StartupPen;


---  SHUTDOWN PEN
--
-- Shutdown the pen package, shutting down the actual pen package if we
-- started it.
-----------------------------------------------------------------------------

procedure ShutdownPen is
begin
  if penRunning then
     Pen.ShutdownPen;
     penRunning := false;
  end if;
end ShutdownPen;

end parser_pen;

