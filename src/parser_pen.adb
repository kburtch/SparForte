------------------------------------------------------------------------------
-- Pen Package Parser                                                       --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2011 Free Software Foundation              --
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

with text_io;use text_io;
with interfaces.c,
    pen,
    bush_os.sdl,
    bush_os.opengl,
    user_io,
    world,
    scanner,
    string_util,
    scanner_arrays,
    parser_aux,
    parser;
use pen,
    bush_os,
    bush_os.sdl,
    bush_os.opengl,
    user_io,
    world,
    scanner,
    string_util,
    scanner_arrays,
    parser_aux,
    parser,
    bush_os;

package body parser_pen is

penRunning : boolean := false;
-- some versions of SDL take control of the screen if video is started so
-- we'll only start SDL when a canvas is created.

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

procedure ParsePenIsEmptyRect( result : out unbounded_string ) is
  -- Syntax: b := pen.is_empty_rect( rec );
  -- Source: Pen.isEmptyRect
  record_id : identifier;
begin
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

procedure ParsePenInsideRect( result : out unbounded_string ) is
  -- Syntax: b := pen.inside_rect( rect, rect2 );
  -- Source: Pen.insideRect
  record_id : identifier;
  record2_id : identifier;
  --x_val, y_val : unbounded_string;
  --x_type, y_type : identifier;
begin
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

procedure ParsePenInRect( result : out unbounded_string ) is
  -- Syntax: b := pen.in_rect( rect, x, y );
  -- Source: Pen.inRect
  record_id : identifier;
  x_val, y_val : unbounded_string;
  x_type, y_type : identifier;
begin
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

procedure ParsePenGetPenMode( result : out unbounded_string ) is
  -- Syntax: mode := pen.get_pen_mode( canvas_id );
  -- Source: Pen.getPenMode
  canvas_id : identifier;
begin
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

procedure ParsePenGetPenBrush( result : out unbounded_string ) is
  -- Syntax: brush := pen.get_pen_brush( canvas_id );
  -- Source: Pen.getPenBrush
  canvas_id : identifier;
begin
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

procedure ParsePenGreyscale( result : out unbounded_string ) is
  -- Syntax: rgb := pen.greyscale( r, g, b );
  -- Source: Pen.Greyscale
  rexpr_val : unbounded_string;
  rexpr_type: identifier;
  gexpr_val : unbounded_string;
  gexpr_type: identifier;
  bexpr_val : unbounded_string;
  bexpr_type: identifier;
begin
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
  canvas_id : identifier;
  array_var_id : identifier;
  first, last : long_integer;
  array_id : arrayID;
begin
  expect( pen_plot_t );
  expect( symbol_t, "(" );
  ParseIdentifier( canvas_id );
  if baseTypesOk( identifiers( canvas_id ).kind, pen_canvas_id_t ) then
     expect( symbol_t, "," );
     ParseIdentifier( array_var_id );
     if not (class_ok( array_var_id, otherClass ) and identifiers( array_var_id ).list) then
        err( "Array expected" );
     end if;
     if uniTypesOK( identifiers( array_var_id ).kind, uni_numeric_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;
  if isExecutingCommand then
     array_id := arrayID( to_numeric( identifiers( array_var_id ).value ) );
     first := firstBound( array_id );
     last  := lastBound( array_id );
     declare
        values : plotValues( first..last );
     begin
        -- copy values into an Ada array
        for i in first..last loop
            values(i) := to_numeric( arrayElement( array_id, i) );
        end loop;
        Plot( aCanvasID( to_numeric( identifiers( canvas_id ).value ) ), values );
     exception when others =>
        err( "exception raised" );
     end;
  end if;
end ParsePenPlot;

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

  declareIdent( pen_glenum_t, "pen.glenum", integer_t, typeClass  );
  declareIdent( pen_glboolean_t, "pen.glboolean", integer_t, typeClass  );

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

  declareIdent( pen_glpushbits_t, "pen.glpushbits", integer_t, typeClass  );

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

  declareIdent( pen_glenum_t, "pen.glenum", integer_t, typeClass  );

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

  declareIdent( pen_gluint_t, "pen.gluint", integer_t, typeClass  );

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

  declareIdent( pen_glushort_t, "pen.glushort", integer_t, typeClass  );

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

  declareStandardConstant( pen_glu_object_parametric_error_t, "pen.glu_object_parametric_error", pen_glenum_t, float_to_string( GLU_OBJECT_PARAMETRIC_ERROR ) );
  declareStandardConstant( pen_glu_object_parametric_error_ext_t, "pen.glu_object_parametric_error_ext", pen_glenum_t, float_to_string( GLU_OBJECT_PARAMETRIC_ERROR_EXT ) );
  declareStandardConstant( pen_glu_object_path_length_t, "pen.glu_object_path_length", pen_glenum_t, float_to_string( GLU_OBJECT_PATH_LENGTH ) );
  declareStandardConstant( pen_glu_object_path_length_ext_t, "pen.glu_object_path_length_ext", pen_glenum_t, float_to_string( GLU_OBJECT_PATH_LENGTH_EXT ) );
  declareStandardConstant( pen_glu_path_length_t, "pen.glu_path_length", pen_glenum_t, float_to_string( GLU_PATH_LENGTH ) );
  declareStandardConstant( pen_glu_parametric_error_t, "pen.glu_parametric_error", pen_glenum_t, float_to_string( GLU_PARAMETRIC_ERROR ) );
  declareStandardConstant( pen_glu_domain_distance_t, "pen.glu_domain_distance", pen_glenum_t, float_to_string( GLU_DOMAIN_DISTANCE ) );

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

  declareIdent( pen_long_float_t, "pen.long_float", integer_t, typeClass  );

  declareStandardConstant( pen_glu_tess_max_coord_t, "pen.glu_tess_max_coord", pen_long_float_t, float_to_string( GLU_TESS_MAX_COORD ) );
end declarePenGLConstants;


---  STARTUP PEN
--
-- Initialize the built-in Pen Package, declaring all identifiers.
-----------------------------------------------------------------------------

procedure StartupPen is
begin
  declarePenStandardTypes;

  -- declare pen package procedures and functions

  declareProcedure( pen_set_rect_t, "pen.set_rect" );
  declareFunction( pen_is_empty_rect_t, "pen.is_empty_rect" );
  declareProcedure( pen_offset_rect_t, "pen.offset_rect" );
  declareProcedure( pen_inset_rect_t, "pen.inset_rect" );
  declareProcedure( pen_intersect_rect_t, "pen.intersect_rect" );
  declareProcedure( pen_inside_rect_t, "pen.inside_rect" );
  declareProcedure( pen_in_rect_t, "pen.in_rect" );

  declareProcedure( pen_set_pen_mode_t, "pen.set_pen_mode" );
  declareProcedure( pen_set_pen_ink_t, "pen.set_pen_ink" );
  declareProcedure( pen_set_pen_brush_t, "pen.set_pen_brush" );
  declareProcedure( pen_set_pen_pattern_t, "pen.set_pen_pattern" );
  declareFunction( pen_get_pen_mode_t, "pen.get_pen_mode" );
  declareProcedure( pen_get_pen_ink_t, "pen.get_pen_ink" );
  declareFunction( pen_get_pen_brush_t, "pen.get_pen_brush" );
--  declareFunction( pen_get_pen_pattern_t, "pen.get_pen_pattern" );

  declareProcedure( pen_move_to_t, "pen.move_to" );
  declareProcedure( pen_move_t, "pen.move" );
  declareProcedure( pen_line_to_t, "pen.line_to" );
  declareProcedure( pen_line_t, "pen.line" );
  declareProcedure( pen_hline_t, "pen.hline" );
  declareProcedure( pen_vline_t, "pen.vline" );

  declareProcedure( pen_frame_rect_t, "pen.frame_rect" );
  declareProcedure( pen_paint_rect_t, "pen.paint_rect" );
  declareProcedure( pen_fill_rect_t, "pen.fill_rect" );
  declareProcedure( pen_frame_ellipse_t, "pen.frame_ellipse" );
  declareProcedure( pen_paint_ellipse_t, "pen.paint_ellipse" );
  declareProcedure( pen_fill_ellipse_t, "pen.fill_ellipse" );

  declareProcedure( pen_clear_t, "pen.clear" );

  declareProcedure( pen_new_screen_canvas_t, "pen.new_screen_canvas" );
  declareProcedure( pen_new_window_canvas_t, "pen.new_window_canvas" );
  declareProcedure( pen_new_gl_window_canvas_t, "pen.new_gl_window_canvas" );
  declareProcedure( pen_new_canvas_t, "pen.new_canvas" );
  declareProcedure( pen_save_canvas_t, "pen.save_canvas" );
  declareProcedure( pen_set_title_t, "pen.set_title" );
  declareProcedure( pen_close_canvas_t, "pen.close_canvas" );

  declareProcedure( pen_wait_to_reveal_t, "pen.wait_to_reveal" );
  declareProcedure( pen_reveal_t, "pen.reveal" );
  declareProcedure( pen_reveal_now_t, "pen.reveal_now" );

  declareProcedure( pen_clip_rect_t, "pen.clip_rect" );

  declareProcedure( pen_greyscale_t, "pen.greyscale" );
  declareProcedure( pen_blend_t, "pen.blend" );
  declareProcedure( pen_fade_t, "pen.fade" );

  declareProcedure( pen_plot_t, "pen.plot" );

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
