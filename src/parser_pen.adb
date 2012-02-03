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
with pen,
    bush_os.sdl,
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

procedure StartupPen is
begin

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

-- null rect needs to be declared

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

declareIdent( pen_pen_color_name_t, "pen.color_name", root_enumerated_t, typeClass );
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

-- record handling is limited in business shell
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

  -- Pen.StartupPen;
end StartupPen;

procedure ShutdownPen is
begin
  if penRunning then
     Pen.ShutdownPen;
     penRunning := false;
  end if;
end ShutdownPen;

end parser_pen;
