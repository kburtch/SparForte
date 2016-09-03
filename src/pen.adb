------------------------------------------------------------------------------
-- PEN                                                                      --
-- The Pen Graphics Package.                                                --
--                                                                          --
-- Part of SparForte                                                        --
-- Designed and Programmed by Ken O. Burtch                                 --
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

-- with system;
with system.address_to_access_conversions;
with ada.text_io; use ada.text_io;
-- with ada.strings.unbounded; use ada.strings.unbounded;
with bush_os.sdl; use bush_os.sdl;
with interfaces; use interfaces;
with interfaces.c; use interfaces.c;
use bush_os.sdl.SDL_Surface_Conv;
-- with gen_list;

package body pen is

---> Rectangles

procedure SetRect( r : out ARect; left, top, right, bottom : ACoordinate ) is
-- initialize a rectangle
begin
  r.left := left;
  r.top  := top;
  r.right := right;
  r.bottom := bottom;
end SetRect;

function IsEmptyRect( r : ARect ) return boolean is
begin
  return (r.left > r.right ) or (r.top > r.bottom );
end IsEmptyRect;

procedure OffsetRect( r : in out ARect; dx, dy : ACoordinateOffset ) is
-- shift a rectangle
begin
  r.left := r.left + dx;
  r.top := r.top + dy;
  r.right := r.right + dx;
  r.bottom := r.bottom + dy;
end OffsetRect;

function OffsetRect( r : in ARect; dx, dy : ACoordinateOffset ) return ARect is
-- shift a rectangle returning the resulting rectangle
  newRect : ARect;
begin
  newRect.left := r.left + dx;
  newRect.top := r.top + dy;
  newRect.right := r.right + dx;
  newRect.bottom := r.bottom + dy;
  return newRect;
end OffsetRect;

procedure InsetRect( r : in out ARect; dx, dy : ACoordinateOffset ) is
-- change the size of a rectangle
begin
  r.left := r.left + dx;
  r.top := r.top + dy;
  r.right := r.right - dx;
  r.bottom := r.bottom - dy;
end InsetRect;

function InsetRect( r : in ARect; dx, dy : ACoordinateOffset ) return ARect is
-- change the size of a rectangle returning the resulting rectangle
  newRect : ARect;
begin
  newRect.left := r.left + dx;
  newRect.top := r.top + dy;
  newRect.right := r.right - dx;
  newRect.bottom := r.bottom - dy;
  return newRect;
end InsetRect;

procedure IntersectRect( r : in out ARect; r1, r2 : ARect ) is
begin
  if r1.top > r2.top then
     r.top := r1.top;
  else
     r.top := r2.top;
  end if;
  if r1.bottom < r2.bottom then
     r.bottom := r1.bottom;
  else
     r.bottom := r2.bottom;
  end if;
  if r1.left > r2.left then
     r.left := r1.left;
  else
     r.left := r2.left;
  end if;
  if r1.right < r2.right then
     r.right := r1.right;
  else
     r.right := r2.right;
  end if;
  if IsEmptyRect( r ) then
     r := NullRect;
  end if;
end IntersectRect;

function IntersectRect( r1, r2 : in ARect ) return ARect is
  newRect : ARect;
begin
  IntersectRect( newRect, r1, r2 );
  return newRect;
end IntersectRect;

function InsideRect( Inner, Outer : in ARect ) return boolean is
-- test for one rectangle inside of another
begin
  return (Inner.left   >= Outer.left)   and then
         (Inner.top    >= Outer.top)    and then
         (Inner.right  <= Outer.right ) and then
         (Inner.bottom <= Outer.bottom );
end InsideRect;

function InRect( x, y : ACoordinate ; r : ARect ) return boolean is
-- test for a point inside of a rectangle
begin
  return (x >= r.left and x <= r.right) and then
         (y >= r.top and y <= r.bottom);
end InRect;


---> The Drawing Environment
--

function ">="( left, right : aCanvas ) return boolean is
begin
   return left.id >= right.id;
end ">=";

function "="( left, right : aCanvas ) return boolean is
begin
   return left.id = right.id;
end "=";


---> SET PEN INK
--
-- Change the drawing colour or pattern of a canvas pen.
-----------------------------------------------------------------------------

procedure setPenInk( canvas : in out aCanvas; c : AColourName ) is
begin
  SDL_MapRGB( canvas.pen.pixel,
              canvas.surface_ptr.format,
              -- SDL_RGB_Component( ARGBComponent(svga_maxcomponent-1) * ColourNames( c ).red / 100.0 ),
              -- SDL_RGB_Component( ARGBComponent(svga_maxcomponent-1) * ColourNames( c ).green / 100.0 ),
              -- SDL_RGB_Component( ARGBComponent(svga_maxcomponent-1) * ColourNames( c ).blue / 100.0 ) );
              SDL_RGB_Component( 255.0 * ColourNames( c ).red / 100.0 ),
              SDL_RGB_Component( 255.0 * ColourNames( c ).green / 100.0 ),
              SDL_RGB_Component( 255.0 * ColourNames( c ).blue / 100.0 ) );
end setPenInk;

procedure setPenInk( canvas_id : aCanvasID; c : aColourName ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     setPenInk( theCanvas, c );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
     setPenInk( theCanvas, c );
  end if;
end setPenInk;


-- procedure SetPenColour( screen_ptr : SDL_Surface_Ptr; r, g, b : ARGBComponent ) is
procedure setPenInk( canvas : in out aCanvas; r, g, b : ARGBComponent ) is
begin
  canvas.pen.ink_r := r;
  canvas.pen.ink_g := g;
  canvas.pen.ink_b := b;
--put_line( "Calling SDL_MapRGB" ); -- DEBUG
  -- canvas.surface_ptr := SDL_Surface_Conv.To_Pointer( canvas.surface ); -- VERIFYING
  SDL_MapRGB( canvas.pen.pixel,
              canvas.surface_ptr.format,
              SDL_RGB_Component( 255.0 * r / 100.0 ), -- DEBUG
              SDL_RGB_Component( 255.0 * g / 100.0 ),
              SDL_RGB_Component( 255.0 * b / 100.0 ) );
end setPenInk;

procedure setPenInk( canvas_id : aCanvasID; r, g, b : aRGBComponent ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     setPenInk( theCanvas, r, g, b );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end setPenInk;

procedure setPenInk( canvas : in out aCanvas; pattern : aCanvas ) is
begin
  canvas.pen.pattern := pattern.surface;
  canvas.pen.pattern_ptr := pattern.surface_ptr;
end setPenInk;

procedure setPenInk( canvas_id, pattern_id : aCanvasID ) is
  theCanvas : aCanvas;
  theBrush  : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
  brushIndex : canvasList.aListIndex := 0;
begin
  theBrush.id := pattern_id;
  canvasList.Find( canvas, theBrush, 1, brushIndex );
  if brushIndex = 0 then
     put_line( standard_error, "no such canvas id -" & pattern_id'img );
  else
     canvasList.Find( canvas, brushIndex, theBrush );
  end if;
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     setPenInk( theCanvas, theBrush );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end setPenInk;


---> GET PEN INK
--
-- Get the drawing ink of a canvas pen.
-----------------------------------------------------------------------------

procedure getPenInk( canvas : aCanvas; R, G, B : out aRGBComponent ) is
begin
  R := canvas.pen.ink_r;
  G := canvas.pen.ink_g;
  B := canvas.pen.ink_b;
end getPenInk;

procedure getPenInk( canvas_id : aCanvasID; R, G, B : out aRGBComponent ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     getPenInk( theCanvas, R, G, B );
  end if;
end getPenInk;


---> SET PEN BRUSH
--
-- Change the drawing pattern of a canvas pen.

procedure setPenBrush( canvas : in out aCanvas; newBrush : aPenBrush ) is
begin
  canvas.pen.brush := newBrush;
end setPenBrush;

procedure setPenBrush( canvas_id : aCanvasID; newBrush : aPenBrush ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     setPenBrush( theCanvas, newBrush );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end setPenBrush;

function getPenBrush( canvas_id : aCanvasID ) return aPenBrush is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, theCanvas, 1, canvasIndex );
     canvasList.Find( canvas, canvasIndex, theCanvas );
  end if;
  return theCanvas.pen.brush;
end getPenBrush;

function getPenBrush( canvas : aCanvas ) return aPenBrush is
begin
  return canvas.pen.brush;
end getPenBrush;


---> SET PEN PATTERN
--
-- Change the drawing pattern of a canvas pen.
-----------------------------------------------------------------------------

procedure setPenPattern( canvas : in out aCanvas; pattern : aCanvas ) is
begin
  canvas.pen.pattern := pattern.surface;
  canvas.pen.pattern_ptr := pattern.surface_ptr;
end setPenPattern;

procedure setPenPattern( canvas_id, pattern_id : aCanvasID ) is
  theCanvas, theBrush  : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
  brushIndex : canvasList.aListIndex := 0;
begin
  theBrush.id := pattern_id;
  canvasList.Find( canvas, theBrush, 1, brushIndex );
  if brushIndex = 0 then
     put_line( standard_error, "no such canvas id -" & pattern_id'img );
  else
     canvasList.Find( canvas, brushIndex, theBrush );
  end if;
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     setPenPattern( theCanvas, theBrush );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end setPenPattern;

--function getPenPattern( canvas_id : canvasList.aListIndex ) return canvasList.aListIndex is
--  theCanvas : aCanvas;
--begin
--  canvasList.Find( canvas, canvas_id, theCanvas );
--  return canvas.pen.pattern;
--end getPenPattern;
--
--function getPenPattern( canvas : aCanvas ) return aCanvas is
--begin
--
--end getPenPattern;


---> SET PEN MODE
--
-- Change the drawing mode of a canvas pen.
-----------------------------------------------------------------------------

procedure setPenMode( canvas : in out aCanvas; newMode : aPenMode ) is
begin
  canvas.pen.mode := newMode;
end setPenMode;

procedure setPenMode( canvas_id : aCanvasID; newMode : aPenMode ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     setPenMode( theCanvas, newMode );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end setPenMode;

function getPenMode( canvas : aCanvas ) return aPenMode is
begin
  return canvas.pen.mode;
end getPenMode;

function getPenMode( canvas_id : aCanvasID ) return aPenMode is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
  end if;
  return getPenMode( theCanvas );
end getPenMode;

procedure newPenState( ps : in out aPenState ) is
begin
  ps.X            := 0.0;
  ps.Y            := 0.0;
  ps.Angle        := 0.0;
  ps.ErrorX       := 0.0;
  ps.ErrorY       := 0.0;
  ps.Mode         := copy;
  ps.ink_r        := 100.0;
  ps.ink_g        := 100.0;
  ps.ink_b        := 100.0;
  ps.Brush        := pencil;
  ps.Pixel        := -1; -- white (usually)
  ps.pattern_ptr  := null;
  ps.revealCount  := 0;
end newPenState;


-----------------------------------------------------------------------------

---> DisplayInfo Conversion Functions

function svgah( canvas : aCanvas; ScreenPosition : ACoordinate ) return SDL_HCoordinate is
begin
  return SDL_HCoordinate( ACoordinate( canvas.displayInfo.H_Res ) * ScreenPosition / 100.0 );end svgah;

function svgav( canvas : aCanvas; ScreenPosition : ACoordinate ) return SDL_VCoordinate is
begin
  return SDL_VCoordinate( ACoordinate( canvas.displayInfo.V_Res ) * ScreenPosition / 100.0 );
end svgav;

function svga2comp( canvas : aCanvas; svga_value : Uint8 ) return ARGBComponent is
-- convert svga colour value to a standard RGB component
begin
  return ARGBComponent( 100.0 * float( svga_value ) /
     float( canvas.displayInfo.C_Res ) );
end svga2comp;

function svga2coord( canvas : aCanvas; x : Sint16 ) return ACoordinate is
begin
  return ACoordinate( float( x ) * 100.0 / float( canvas.displayInfo.C_Res - 1 ));
end svga2coord;


---> SVGA EFFECTIVE RECTANGLE
--
-- Convert rectangle r to a clipped pair of hardware pixel coordinates.  The
-- hardware coordinates are calculated.  Then the coordinates are constrained
-- to the intersection of the surface bounds rectangle and clipping rectangle.
-- If entire rectangle is clipped, svga_x1 and svga_x2 are 0.

procedure svgaEffectiveRectangle( theCanvas : aCanvas; r : aRect; svga_x1 : out SDL_HCoordinate; svga_y1 : out SDL_VCoordinate; svga_x2 : out SDL_HCoordinate; svga_y2 : out SDL_VCoordinate ) is
  clip_rect : SDL_Rect;
  clip_x : SDL_HCoordinate;
  clip_y : SDL_VCoordinate;
  clip_w : Uint16;
  clip_h : Uint16;
begin

  -- Convert to hardware coordinates

  svga_x1 := svgah( theCanvas, r.left );
  svga_y1 := svgav( theCanvas, r.top );
  svga_x2 := svgah( theCanvas, r.right )-1;
  svga_y2 := svgav( theCanvas, r.bottom )-1;

  -- Calculate Effective Clipping Rectangle
  --
  -- Surfaces always start at (0,0) but have w and h.

  SDL_GetClipRect( theCanvas.surface, clip_rect );
  clip_x := clip_rect.x;
  clip_y := clip_rect.y;
  if clip_rect.w > Uint16( theCanvas.surface_ptr.w ) then
     clip_w := theCanvas.surface_ptr.clip_rect.w;
  else
     clip_w := Uint16( theCanvas.surface_ptr.w ) - Uint16( clip_x ); -- surface width
  end if;
  if clip_rect.h > Uint16( theCanvas.surface_ptr.h ) then
     clip_h := clip_rect.h;
  else
     clip_h := Uint16( theCanvas.surface_ptr.h ) - Uint16( clip_y ); -- surface height
  end if;

  -- Apply Clipping

  -- put_line( "Original xy =" & svga_x1'img & "," & svga_y1'img );
  -- put_line( "Original  w  =" & svga_x2'img & " -" & svga_y2'img );
  -- put_line( "Clip xy =" & clip_x'img & "," & clip_y'img );
  -- put_line( "Clip wh =" & clip_w'img & " -" & clip_h'img );

  if svga_x2 < clip_x then
     svga_x1 := 0;
     svga_y1 := 0;
     svga_x2 := 0;
     svga_y2 := 0;
  elsif svga_y2 < clip_y then
     svga_x1 := 0;
     svga_y1 := 0;
     svga_x2 := 0;
     svga_y2 := 0;
  elsif svga_x1 > clip_x + SDL_HCoordinate( clip_w ) then
     svga_x1 := 0;
     svga_y1 := 0;
     svga_x2 := 0;
     svga_y2 := 0;
  elsif svga_y1 > clip_y + SDL_VCoordinate( clip_h ) then
     svga_x1 := 0;
     svga_y1 := 0;
     svga_x2 := 0;
     svga_y2 := 0;
  else
     if svga_x1 < clip_x then
        svga_x1 := clip_x;
     end if;
     if svga_y1 < clip_y then
        svga_y1 := clip_y;
     end if;
     if svga_x2 > clip_x + SDL_HCoordinate( clip_w ) then
        svga_x2 := clip_x + SDL_HCoordinate( clip_w );
     end if;
     if svga_y2 > clip_y + SDL_VCoordinate( clip_h ) then
        svga_y2 := clip_y + SDL_VCoordinate( clip_h );
     end if;
  end if;

end svgaEffectiveRectangle;


-----------------------------------------------------------------------------
-- COLOUR OPERATIONS
-----------------------------------------------------------------------------


---> GREY SCALE
--
-- Convert an RGB colour to greyscale weighted for the human eye.

function greyScale( redC, greenC, blueC : ARGBComponent ) return
  ARGBComponent is
begin
  return 0.53 * redC + 0.33 * greenC + 0.11 * blueC;
end greyScale;

function greyScale( colour : AColourName ) return ARGBComponent is
begin
  return 0.53 * ColourNames( colour ).red +
         0.33 * ColourNames( colour ).green +
         0.11 * ColourNames( colour ).blue;
end greyScale;


---> BLEND
--
-- Average two RGB colours to create a new colour.

procedure blend( redC1, greenC1, blueC1 : ARGBComponent;
                 redC2, greenC2, blueC2 : ARGBComponent;
                 redC3, greenC3, blueC3 : out ARGBComponent ) is
begin
  redC3   := ( redC1 + redC2 ) / 2.0;
  greenC3 := ( greenC1 + greenC2 ) / 2.0;
  blueC3  := ( blueC1 + blueC2 ) / 2.0;
end blend;


---> FADE
--
-- Brighten or darken a colour by a percentage.

procedure fade( redC1, greenC1, blueC1 : ARGBComponent;
                fadeamount : float;
                redC2, greenC2, blueC2 : out ARGBComponent ) is
  Temp : float;
begin
  Temp := (float( redC1 ) * ( 100.0 - fadeamount ))/100.0;
  if Temp < 0.0 then
     Temp := 0.0;
  end if;
  if Temp > 100.0 then
     Temp := 100.0;
  end if;
  redC2 := ARGBComponent( Temp );

  Temp := (float( greenC1 ) * ( 100.0 - fadeamount ))/100.0;
  if Temp < 0.0 then
     Temp := 0.0;
  end if;
  if Temp > 100.0 then
     Temp := 100.0;
  end if;
  greenC2 := ARGBComponent( Temp );

  Temp := (float( blueC1 ) * ( 100.0 - fadeamount ))/100.0;
  if Temp < 0.0 then
     Temp := 0.0;
  end if;
  if Temp > 100.0 then
     Temp := 100.0;
  end if;
  blueC2 := ARGBComponent( Temp );
end fade;

procedure fade( redC, greenC, blueC : in out ARGBComponent ;
                fadeamount : float ) is
begin
  fade( redC, greenC, blueC, fadeamount, redC, greenC, blueC );
end fade;


-----------------------------------------------------------------------------
-- HOUSEKEEPING
-----------------------------------------------------------------------------

moduleIsRunning : boolean := false;

---> Startup
--
-- Startup the pen package.
-----------------------------------------------------------------------------

procedure startupPen is
  res_int : integer;
  res     : SDL_success;
begin
  res_int := SDL_Init( SDL_INIT_VIDEO );
  if res_int < 0 then
     put_line( standard_error, "startupPen: sdl_init failed, SDL error = " & to_string( get_sdl_error ) );
  else
    res := TTF_Init;
    if res = SDL_Failed then
       put_line( standard_error, "startupPen: TTF_init failed, SDL error = " & to_string( get_sdl_error ) );
       SDL_Quit;
    else
      moduleIsRunning := true;
    end if;
  end if;

  -- LastButtons := 0; -- assume all mouse buttons up
end startupPen;


---> Shutdown
--
-- Shutdown the pen package.
-----------------------------------------------------------------------------

procedure shutdownPen is
begin
  if moduleIsRunning then
     SDL_Quit;
     moduleIsRunning := false;
  end if;
end shutdownPen;


---> isRunning
--
-----------------------------------------------------------------------------

function isPenRunning return boolean is
begin
  return moduleIsRunning;
end isPenRunning;

-----------------------------------------------------------------------------
-- PEN SPOOLING
--
-- This refers to drawing without revealing the results immediately.
-----------------------------------------------------------------------------


---> WAIT TO REVEAL
--
-- Start a series of drawing procedures and reveal the result only after all
-- drawing is completed with reveal.  waitToReveal can be nested with
-- other waitToReveal calls and the result will only be shown after all
-- have been completed with doneDrawing.  In many cases, drawing will be
-- performed faster: without this call, the results of each drawing procedure
-- will be shown immediately on the canvas.
-----------------------------------------------------------------------------

procedure waitToReveal( theCanvas : in out aCanvas ) is
begin
  if theCanvas.pen.revealCount  = 0 then
     if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
        put_line( standard_error, "SDL_LockSurface failed" );
     end if;
  end if;
  theCanvas.pen.revealCount := theCanvas.pen.revealCount + 1;
end waitToReveal;

procedure waitToReveal( canvas_id : aCanvasID ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     waitToReveal( theCanvas );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end waitToReveal;


---> REVEAL
--
-- Finish a series of drawing procedures started with beginDrawing and reveal
-- the result.  waitToReveal can be nested with other waitToReveal calls
-- and the result will only be shown after all have been completed with
-- doneDrawing.  Note this updates the entire canvas.
-----------------------------------------------------------------------------

procedure reveal( theCanvas : in out aCanvas ) is
begin
  case theCanvas.drawingType is
  when opengl =>
     if theCanvas.pen.revealCount > 0 then
        theCanvas.pen.revealCount := theCanvas.pen.revealCount - 1;
        if theCanvas.pen.revealCount  = 0 then
           SDL_GL_SwapBuffers;
        end if;
     end if;
  when raster =>
     if theCanvas.pen.revealCount > 0 then
        theCanvas.pen.revealCount := theCanvas.pen.revealCount - 1;
        if theCanvas.pen.revealCount  = 0 then
           SDL_UnlockSurface( theCanvas.surface  );
        SDL_UpdateRect( theCanvas.surface, 0, 0, 0, 0 );
        end if;
     end if;
  when others => null;
  end case;
end reveal;

procedure reveal( canvas_id : aCanvasID ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     reveal( theCanvas );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end reveal;


---> REVEAL NOW
--
-- If waitToReveal has been used, cancel the request, show the results so far
-- and continue drawing normally.    Note this updates the entire canvas.
-- Intended for debugging.
-----------------------------------------------------------------------------

procedure revealNow( theCanvas : in out aCanvas ) is
begin
  case theCanvas.drawingType is
  when opengl =>
     if theCanvas.pen.revealCount >= 0 then
        SDL_GL_SwapBuffers;
     end if;
     theCanvas.pen.revealCount := 0;
  when raster =>
     if theCanvas.pen.revealCount >= 0 then
        SDL_UnlockSurface( theCanvas.surface  );
        SDL_UpdateRect( theCanvas.surface, 0, 0, 0, 0 );
     end if;
     theCanvas.pen.revealCount := 0;
  when others => null;
  end case;
end revealNow;

procedure revealNow( canvas_id : aCanvasID ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     revealNow( theCanvas );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end revealNow;


---------------------------------------------------------------------------
-- CANVAS-WIDE DRAWING
-----------------------------------------------------------------------------


---> Clear
--
-- Clear the canvas to an arbitrary color. (From BASIC: Clear The Screen)
-----------------------------------------------------------------------------

procedure Clear( theCanvas : aCanvas ) is
begin
  paintRect( theCanvas, allRect );
end Clear;

procedure Clear( canvas_id : aCanvasID ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     Clear( theCanvas );
  end if;
end Clear;

procedure Clear( theCanvas : aCanvas; r, g, b : ARGBComponent ) is
begin
  fillRect( theCanvas, allRect, r, g, b );
end Clear;

procedure Clear( canvas_id : aCanvasID; r, g, b : ARGBComponent ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     Clear( theCanvas, r, g, b );
  end if;
end Clear;

procedure Clear( theCanvas : aCanvas; c : AColourName ) is
begin
  fillRect( theCanvas, allRect, ColourNames(c).red, ColourNames(c).green, ColourNames(c).blue );
end Clear;

procedure Clear( canvas_id : aCanvasID; c : AColourName ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     fillRect( theCanvas, allRect, ColourNames(c).red, ColourNames(c).green, ColourNames(c).blue );
  end if;
end Clear;


-----------------------------------------------------------------------------
-- RECTANGLE DRAWING
-----------------------------------------------------------------------------


---> FRAME RECT
--
-- Draw the outline of a rectangle with the current pen.
-----------------------------------------------------------------------------

procedure frameRect( theCanvas : aCanvas; r : ARect ) is
  svga_x1 : SDL_HCoordinate;
  svga_y1 : SDL_VCoordinate;
  svga_x2 : SDL_HCoordinate;
  svga_y2 : SDL_VCoordinate;
  -- res : SDL_Success;
begin

  svgaEffectiveRectangle( theCanvas, r, svga_x1, svga_y1, svga_x2, svga_y2 );

  if svga_x1 + svga_x2 = 0 then
     return;
  end if;

  case theCanvas.pen.brush is

  when stretch => -- not complete
     null;

  when tile =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1, svga_y1,
         svga_x1, svga_y2, 0, 0, theCanvas.pen.pattern, theCanvas.pen.mode );
     SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x2, svga_y1,
         svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern, theCanvas.pen.mode );
     SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y1, 0, 0, theCanvas.pen.pattern, theCanvas.pen.mode );
     SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1, svga_y2,
         svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when stamp =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1, svga_y1,
            svga_x1, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x2, svga_y1,
            svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1, svga_y1,
            svga_x2, svga_y1, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1, svga_y2,
            svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when smear => -- not done
       null;

  when others => -- including pencil
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     if theCanvas.clipRect = allRect then
        -- no clipping? used optimized drawing
        SDL_Ext_VLine( theCanvas.surface,
                       svgah( theCanvas, r.left ),
                       svgav( theCanvas, r.top ),
                       svgav( theCanvas, r.bottom )-1,
                       theCanvas.pen.pixel,
                       theCanvas.pen.mode );
        SDL_Ext_HLine( theCanvas.surface,
                       svgah( theCanvas, r.left ),
                       svgah( theCanvas, r.right )-1,
                       svgav( theCanvas, r.top ),
                       theCanvas.pen.pixel,
                       theCanvas.pen.mode );
        SDL_Ext_VLine( theCanvas.surface,
                       svgah( theCanvas, r.right )-1,
                       svgav( theCanvas, r.top ),
                       svgav( theCanvas, r.bottom )-1,
                       theCanvas.pen.pixel,
                       theCanvas.pen.mode );
        SDL_Ext_HLine( theCanvas.surface,
                       svgah( theCanvas, r.left ),
                       svgah( theCanvas, r.right )-1,
                       svgav( theCanvas, r.bottom )-1,
                       theCanvas.pen.pixel,
                       theCanvas.pen.mode );
     else
        -- clipping? use frame_rect
        SDL_EXT_Frame_Rect( theCanvas.surface,
           svgah( theCanvas, r.left ),
           svgav( theCanvas, r.top ),
           svgah( theCanvas, r.right )-1,
           svgav( theCanvas, r.bottom )-1,
           theCanvas.pen.pixel,
           theCanvas.pen.mode );
     end if;
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  end case;
  if theCanvas.pen.revealCount = 0 then
     -- res := SDL_Flip( theCanvas.surface );
     SDL_UpdateRect(
        theCanvas.surface,
        svgah( theCanvas, r.left ),
        svgav( theCanvas, r.top ),
        Uint32( svgah( theCanvas, r.right ) -
           svgah( theCanvas, r.left ) + 1 ),
        Uint32( svgav( theCanvas, r.bottom ) -
           svgav( theCanvas, r.top ) + 1 ) );
  end if;
end frameRect;

procedure frameRect( canvas_id : aCanvasID; r : ARect ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     frameRect( theCanvas, r );
  end if;
end frameRect;

---> FILL RECT
--
-- Fill in a rectangle with the current pen.

-- put_line( "Stamping" );
-- put_line( "CopyFillRect: x1/y1 = " & svga_x1'img & "," & svga_y1'img );
-- put_line( "CopyFillRect: x2/y2 = " & svga_x2'img & "," & svga_y2'img );
--         if SDL_LockSurface( theCanvas.surface  ) = SDL_OK then
--            SDL_EXT_Fill_Rect_Pattern( theCanvas.Surface, svga_x1,
--                svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
--            SDL_UnlockSurface( theCanvas.surface  );
--         end if;

procedure fillRect( theCanvas : aCanvas; theRect : ARect; r, g, b : ARGBComponent ) is
  svga_x1 : SDL_HCoordinate;
  svga_y1 : SDL_VCoordinate;
  svga_x2 : SDL_HCoordinate;
  svga_y2 : SDL_VCoordinate;
  pixel   : SDL_Generic_Pixel;
begin

  -- convert coordinates to pixels and clip

  svgaEffectiveRectangle( theCanvas, theRect, svga_x1, svga_y1, svga_x2, svga_y2 );
  if svga_x1 + svga_x2 = 0 then
     return;
  end if;

  -- mix the colour to fill with

  SDL_MapRGB( pixel,
              theCanvas.surface_ptr.format,
              SDL_RGB_Component( 255.0 * r / 100.0 ),
              SDL_RGB_Component( 255.0 * g / 100.0 ),
              SDL_RGB_Component( 255.0 * b / 100.0 ) );

  -- different fills with different brushes

  case theCanvas.pen.brush is

  when stretch => -- not complete
     if theCanvas.pen.mode /= copy then
        if theCanvas.pen.revealCount = 0 then
           if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
              put_line( standard_error, "unable to lock SDL surface" );
           end if;
        end if;
        SDL_EXT_Fill_Rect( theCanvas.surface, svga_x1, svga_y1,
            svga_x2, svga_y2, pixel, theCanvas.pen.mode );
        if theCanvas.pen.revealCount = 0 then
           SDL_UnlockSurface( theCanvas.surface  );
        end if;
     else
        SDL_EXT_Fill_Rect( theCanvas.surface, svga_x1, svga_y1,
            svga_x2, svga_y2, pixel, theCanvas.pen.mode );
     end if;

  when tile =>
     if theCanvas.pen.mode /= copy then
        if theCanvas.pen.revealCount = 0 then
           if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
              put_line( standard_error, "unable to lock SDL surface" );
           end if;
        end if;
        SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1, svga_y1,
            svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern, theCanvas.pen.mode );
        if theCanvas.pen.revealCount = 0 then
           SDL_UnlockSurface( theCanvas.surface  );
        end if;
     else
        -- Not sure if this is right...
        SDL_EXT_Copy_Fill_Rect_Pattern( theCanvas.surface, svga_x1,
             svga_y1, svga_x2, svga_y2, theCanvas.pen.pattern );
        --SDL_EXT_Fill_Rect_Pattern( theCanvas.Surface, svga_x1,
        --    svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     end if;

  when stamp =>
     -- if theCanvas.pen.mode /= copy then
     if theCanvas.pen.mode /= copy then
        if theCanvas.pen.revealCount = 0 then
           if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
              put_line( standard_error, "unable to lock SDL surface" );
           end if;
        end if;
        SDL_EXT_Fill_Rect_Pattern( theCanvas.Surface, svga_x1,
            svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
        if theCanvas.pen.revealCount = 0 then
           SDL_UnlockSurface( theCanvas.surface  );
        end if;
     else
        -- Copy but not-zero offset?  Fall back to regular Fill_Rect_Pattern.
        SDL_EXT_Fill_Rect_Pattern( theCanvas.Surface, svga_x1,
            svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     end if;

  when smear => -- not done
     if theCanvas.pen.mode /= copy then
        if theCanvas.pen.revealCount = 0 then
           if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
              put_line( standard_error, "unable to lock SDL surface" );
           end if;
        end if;
        SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1,
            svga_y1, svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern,
            theCanvas.pen.mode );
        if theCanvas.pen.revealCount = 0 then
           SDL_UnlockSurface( theCanvas.surface  );
        end if;
     else
        SDL_EXT_Copy_Fill_Rect_Pattern( theCanvas.surface, svga_x1,
            svga_y1, svga_x2, svga_y2, theCanvas.pen.pattern );
     end if;

  when others => -- including pencil
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Rect( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, pixel, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  end case;

  if theCanvas.pen.revealCount = 0 then
     SDL_UpdateRect( theCanvas.surface, svga_x1, svga_y1, Uint32( svga_x2 - svga_x1 ) + 1, Uint32( svga_y2 - svga_y1 ) + 1 );
  end if;

end fillRect;

procedure fillRect( canvas_id : aCanvasID; theRect : ARect; r, g, b : ARGBComponent ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     fillRect( theCanvas, theRect, r, g, b );
  end if;
end fillRect;

procedure fillRect( theCanvas : aCanvas; theRect : ARect; c : AColourName ) is
begin
  fillRect( theCanvas, theRect, ColourNames(c).red, ColourNames(c).green, ColourNames(c).blue );
end fillRect;

procedure fillRect( canvas_id : aCanvasID; theRect : ARect; c : AColourName ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     fillRect( theCanvas, theRect, ColourNames(c).red, ColourNames(c).green, ColourNames(c).blue );
  end if;
end fillRect;


---> PAINT RECT
--
-----------------------------------------------------------------------------

procedure paintRect( theCanvas : aCanvas; r : ARect ) is
  svga_x1 : SDL_HCoordinate;
  svga_y1 : SDL_VCoordinate;
  svga_x2 : SDL_HCoordinate;
  svga_y2 : SDL_VCoordinate;
  -- res : SDL_Success;
begin

  -- convert coordinates to pixels and clip

  svgaEffectiveRectangle( theCanvas, r, svga_x1, svga_y1, svga_x2, svga_y2 );
  if svga_x1 + svga_x2 = 0 then
     return;
  end if;

  -- different fills with different brushes

  case theCanvas.pen.brush is

  when stretch => -- not complete
     if theCanvas.pen.mode /= copy then
        if theCanvas.pen.revealCount = 0 then
           if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
              put_line( standard_error, "unable to lock SDL surface" );
           end if;
        end if;
        SDL_EXT_Fill_Rect( theCanvas.surface, svga_x1, svga_y1,
            svga_x2, svga_y2, theCanvas.pen.pixel, theCanvas.pen.mode );
        if theCanvas.pen.revealCount = 0 then
           SDL_UnlockSurface( theCanvas.surface  );
        end if;
     else
        SDL_EXT_Fill_Rect( theCanvas.surface, svga_x1, svga_y1,
            svga_x2, svga_y2, theCanvas.pen.pixel, theCanvas.pen.mode );
     end if;

  when tile =>
     if theCanvas.pen.mode /= copy then
        if theCanvas.pen.revealCount = 0 then
           if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
              put_line( standard_error, "unable to lock SDL surface" );
           end if;
        end if;
        SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1, svga_y1,
            svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern, theCanvas.pen.mode );
        if theCanvas.pen.revealCount = 0 then
           SDL_UnlockSurface( theCanvas.surface  );
        end if;
     else
        -- Not sure if this is right...
        SDL_EXT_Copy_Fill_Rect_Pattern( theCanvas.surface, svga_x1,
             svga_y1, svga_x2, svga_y2, theCanvas.pen.pattern );
        --SDL_EXT_Fill_Rect_Pattern( theCanvas.Surface, svga_x1,
        --    svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     end if;

  when stamp =>
     -- if theCanvas.pen.mode /= copy then
     if theCanvas.pen.mode /= copy then
        if theCanvas.pen.revealCount = 0 then
           if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
              put_line( standard_error, "unable to lock SDL surface" );
           end if;
        end if;
        SDL_EXT_Fill_Rect_Pattern( theCanvas.Surface, svga_x1,
            svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
        if theCanvas.pen.revealCount = 0 then
           SDL_UnlockSurface( theCanvas.surface  );
        end if;
     else
        -- Copy but not-zero offset?  Fall back to regular Fill_Rect_Pattern.
        SDL_EXT_Fill_Rect_Pattern( theCanvas.Surface, svga_x1,
            svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     end if;

  when smear => -- not done
     if theCanvas.pen.mode /= copy then
        if theCanvas.pen.revealCount = 0 then
           if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
              put_line( standard_error, "unable to lock SDL surface" );
           end if;
        end if;
        SDL_EXT_Fill_Rect_Pattern( theCanvas.surface, svga_x1,
            svga_y1, svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern,
            theCanvas.pen.mode );
        if theCanvas.pen.revealCount = 0 then
           SDL_UnlockSurface( theCanvas.surface  );
        end if;
     else
        SDL_EXT_Copy_Fill_Rect_Pattern( theCanvas.surface, svga_x1,
            svga_y1, svga_x2, svga_y2, theCanvas.pen.pattern );
     end if;

  when others => -- including pencil
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Rect( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, theCanvas.pen.pixel, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  end case;

  if theCanvas.pen.revealCount = 0 then
     SDL_UpdateRect( theCanvas.surface, svga_x1, svga_y1, Uint32( svga_x2 - svga_x1 ) + 1, Uint32( svga_y2 - svga_y1 ) + 1 );
     --if SDL_Flip( theCanvas.surface ) /= SDL_OK then
     --end if;
  end if;

end paintRect;

procedure paintRect( canvas_id : aCanvasID; r : ARect ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     paintRect( theCanvas, r );
  end if;
end paintRect;


---> FRAMED RECT
--
-- Fill in a rectangle with an arbitrary color and draw a frame with another
-- colour.
-----------------------------------------------------------------------------

procedure framedRect( theCanvas : aCanvas; r : ARect; fore_r, fore_g, fore_b, back_r, back_g, back_b : ARGBComponent ) is
begin
  fillRect( theCanvas, r, back_r, back_g, back_b );
  --frameRect( theCanvas, r, fore_r, fore_g, fore_b );
end framedRect;

procedure framedRect( canvas_id : aCanvasID; r : ARect; fore_r, fore_g, fore_b, back_r, back_g, back_b : ARGBComponent ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     framedRect( theCanvas, r, fore_r, fore_g, fore_b, back_r, back_g, back_b );
  end if;
end framedRect;


-----------------------------------------------------------------------------
-- ELLIPSE DRAWING
-----------------------------------------------------------------------------


---> FRAME ELLIPSE
--
-- Draw the outline of an ellipse with the current pen.

procedure frameEllipse( theCanvas : aCanvas; r : ARect ) is
  svga_x1 : SDL_HCoordinate;
  svga_y1 : SDL_VCoordinate;
  svga_x2 : SDL_HCoordinate;
  svga_y2 : SDL_VCoordinate;
  res : SDL_Success;
begin

  svgaEffectiveRectangle( theCanvas, r, svga_x1, svga_y1, svga_x2, svga_y2 );

  if svga_x1 + svga_x2 = 0 then
     return;
  end if;

  case theCanvas.pen.brush is

  when stretch => -- not complete
     null;

  when tile =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Frame_Ellipse_Pattern( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern, copy );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when stamp =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Frame_Ellipse_Pattern( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, copy );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when smear => -- not done
     null;

  when others => -- including pencil
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Frame_Ellipse( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, theCanvas.pen.pixel, theCanvas.pen.mode );
        -- adjust -1, adjust mode
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  end case;
  if theCanvas.pen.revealCount = 0 then
     res := SDL_Flip( theCanvas.surface );
     -- SDL_UpdateRect( theCanvas.surface, svga_x1, svga_y1, Uint32( svga_x2 - svga_x1 ) + 1, Uint32( svga_y2 - svga_y1 ) + 1 );
  end if;
end frameEllipse;

procedure frameEllipse( canvas_id : aCanvasID; r : ARect ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     frameEllipse( theCanvas, r );
  end if;
end frameEllipse;


---> FRAME ELLIPSE
--
-- Draw the outline of an ellipse with the current pen.

--procedure fillEllipse( theCanvas : aCanvas; r : ARect; pattern : system.address ) is
--  svga_x1 : SDL_HCoordinate;
--  svga_y1 : SDL_VCoordinate;
--  svga_x2 : SDL_HCoordinate;
--  svga_y2 : SDL_VCoordinate;
--  res : SDL_Success;
--begin
--
--  svgaEffectiveRectangle( theCanvas, r, svga_x1, svga_y1, svga_x2, svga_y2 );
--  if svga_x1 + svga_x2 = 0 then
--     return;
--  end if;
--
--  put_line( "Drawing filled ellipse on canvas: " & to_string( theCanvas.name ) );
--  put_line( "Pen brush =" & theCanvas.pen.brush'img );
--  put_line( "Pen mode  =" & theCanvas.pen.mode'img );
--
--  case theCanvas.pen.brush is
--
--  when stretch => -- not complete
--     if SDL_LockSurface( theCanvas.surface  ) = SDL_OK then
--        SDL_EXT_Fill_Ellipse( theCanvas.surface, svga_x1, svga_y1,
--            svga_x2, svga_y2, theCanvas.pen.pixel, theCanvas.pen.mode );
--        SDL_UnlockSurface( theCanvas.surface  );
--     end if;
--
--  when tile =>
--     if SDL_LockSurface( theCanvas.surface  ) = SDL_OK then
--        SDL_EXT_Fill_Ellipse_Pattern( theCanvas.surface, svga_x1, svga_y1,
--            svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern, theCanvas.pen.mode );
--        SDL_UnlockSurface( theCanvas.surface  );
--     end if;
--
--  when stamp =>
--     if SDL_LockSurface( theCanvas.surface  ) = SDL_OK then
--        SDL_EXT_Fill_Ellipse_Pattern( theCanvas.Surface, svga_x1,
--            svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
--        SDL_UnlockSurface( theCanvas.surface  );
--     end if;
--
--  when smear => -- not done
--     if SDL_LockSurface( theCanvas.surface  ) = SDL_OK then
--        SDL_EXT_Fill_Ellipse_Pattern( theCanvas.surface, svga_x1,
--          svga_y1, svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern,
--          theCanvas.pen.mode );
--        SDL_UnlockSurface( theCanvas.surface  );
--     end if;
--
--  when others => -- including pencil
--     if SDL_LockSurface( theCanvas.surface  ) = SDL_OK then
--        SDL_EXT_Fill_Ellipse( theCanvas.surface, svga_x1, svga_y1,
--            svga_x2, svga_y2, theCanvas.pen.pixel, theCanvas.pen.mode );
--        SDL_UnlockSurface( theCanvas.surface  );
--     end if;
--
--  end case;
--
--  if theCanvas.pen.revealCount = 0 then
--put_line( "Revealing!" );
--     SDL_UpdateRect( theCanvas.surface, svga_x1, svga_y1, Uint32( svga_x2 - svga_x1 ) + 1, Uint32( svga_y2 - svga_y1 ) + 1 );
--     --if SDL_Flip( theCanvas.surface ) /= SDL_OK then
     --end if;
--  end if;
--
--end fillEllipse;

procedure paintEllipse( theCanvas : aCanvas; r : ARect ) is
  svga_x1 : SDL_HCoordinate;
  svga_y1 : SDL_VCoordinate;
  svga_x2 : SDL_HCoordinate;
  svga_y2 : SDL_VCoordinate;
begin

  svgaEffectiveRectangle( theCanvas, r, svga_x1, svga_y1, svga_x2, svga_y2 );
  if svga_x1 + svga_x2 = 0 then
     return;
  end if;

  case theCanvas.pen.brush is

  when stretch => -- not complete
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, theCanvas.pen.pixel, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when tile =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse_Pattern( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when stamp =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse_Pattern( theCanvas.Surface, svga_x1,
         svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when smear => -- not done
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse_Pattern( theCanvas.surface, svga_x1,
         svga_y1, svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern,
         theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when others => -- including pencil
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, theCanvas.pen.pixel, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  end case;

  if theCanvas.pen.revealCount = 0 then
     SDL_UpdateRect( theCanvas.surface, svga_x1, svga_y1, Uint32( svga_x2 - svga_x1 ) + 1, Uint32( svga_y2 - svga_y1 ) + 1 );
  end if;

end paintEllipse;

procedure paintEllipse( canvas_id : aCanvasID; r : ARect ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     paintEllipse( theCanvas, r );
  end if;
end paintEllipse;


procedure fillEllipse( theCanvas : aCanvas; theRect : ARect; r, g, b : ARGBComponent ) is
  svga_x1 : SDL_HCoordinate;
  svga_y1 : SDL_VCoordinate;
  svga_x2 : SDL_HCoordinate;
  svga_y2 : SDL_VCoordinate;
  pixel   : SDL_Generic_Pixel;
begin

  svgaEffectiveRectangle( theCanvas, theRect, svga_x1, svga_y1, svga_x2, svga_y2 );
  if svga_x1 + svga_x2 = 0 then
     return;
  end if;

  -- mix the colour to fill with

  SDL_MapRGB( pixel,
              theCanvas.surface_ptr.format,
              SDL_RGB_Component( 255.0 * r / 100.0 ),
              SDL_RGB_Component( 255.0 * g / 100.0 ),
              SDL_RGB_Component( 255.0 * b / 100.0 ) );

  case theCanvas.pen.brush is

  when stretch => -- not complete
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, pixel, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when tile =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse_Pattern( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when stamp =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse_Pattern( theCanvas.Surface, svga_x1,
         svga_y1, svga_x2, svga_y2, svga_x1, svga_y1, theCanvas.pen.pattern, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when smear => -- not done
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse_Pattern( theCanvas.surface, svga_x1,
         svga_y1, svga_x2, svga_y2, 0, 0, theCanvas.pen.pattern,
         theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when others => -- including pencil
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_Fill_Ellipse( theCanvas.surface, svga_x1, svga_y1,
         svga_x2, svga_y2, pixel, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  end case;

  if theCanvas.pen.revealCount = 0 then
     SDL_UpdateRect( theCanvas.surface, svga_x1, svga_y1, Uint32( svga_x2 - svga_x1 ) + 1, Uint32( svga_y2 - svga_y1 ) + 1 );
  end if;

end fillEllipse;

procedure fillEllipse( canvas_id : aCanvasID; theRect : ARect; r, g, b : ARGBComponent ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     fillEllipse( theCanvas, theRect, r, g, b );
  end if;
end fillEllipse;

procedure fillEllipse( theCanvas : aCanvas; theRect : ARect; c : AColourName ) is
begin
  fillEllipse( theCanvas, theRect, ColourNames(c).red, ColourNames(c).green, ColourNames(c).blue );
end fillEllipse;

procedure fillEllipse( canvas_id : aCanvasID; theRect : ARect; c : AColourName ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     fillEllipse( theCanvas, theRect, ColourNames(c).red, ColourNames(c).green, ColourNames(c).blue );
  end if;
end fillEllipse;




-----------------------------------------------------------------------------
-- CANVAS
-----------------------------------------------------------------------------


---> NEW SCREEN CANVAS
--
-- Create a new on-screen canvas covering the whole screen.  The size and
-- resolution of the canvas is based on the canvas data structure.  There can
-- only be one screen canvas.

procedure newScreenCanvas( H_Res, V_Res, C_Res : positive; canvas_id : out aCanvasID ) is
  newcanvas : aCanvas;
  videoFlags : SDL_Surface_Flag;
begin
  newcanvas.id := canvasIdTop;
  canvasIdTop := canvasIdTop + 1;
  newPenState( newCanvas.pen );
  newcanvas.displayInfo.H_Res := H_Res;
  newcanvas.displayInfo.V_Res := V_Res;
  newcanvas.displayInfo.C_Res := C_Res;

  -- Check the hardware and set the options accordingly

  newcanvas.hardware := SDL_GetVideoInfo;
  newcanvas.hardware_ptr := SDL_VideoInfo_Conv.To_Pointer( newcanvas.hardware );
  videoFlags := SDL_FULLSCREEN + SDL_HWPALETTE;
  if newcanvas.hardware_ptr.hw_available then
     videoFlags := videoFlags + SDL_HWSURFACE;
  else
     videoFlags := videoFlags + SDL_SWSURFACE;
  end if;
  if newcanvas.hardware_ptr.blit_hw then
     videoFlags := videoFlags + SDL_HWACCEL;
  end if;

  -- Create the window

  newcanvas.surface := SDL_SetVideoMode( Interfaces.C.int( H_Res ), Interfaces.C.int( V_Res ), Interfaces.C.int( C_Res ), videoFlags );
  newcanvas.surface_ptr := SDL_Surface_Conv.To_Pointer( newcanvas.surface );
  if newcanvas.surface_ptr = null then
     put_line( standard_error, "newScreenCanvas: failed for " & H_Res'img & V_Res'img & C_Res'img & "SDL error = " & to_string( get_sdl_error ) );
  end if;
  newcanvas.kind := screen;
  newcanvas.drawingType := raster;
  newCanvas.name := to_unbounded_string( "Untitled SparForte Screen Canvas" );
  canvasList.Queue( canvas, newCanvas );
  canvas_id := newcanvas.id;
end newScreenCanvas;


---> NEW GL SCREEN CANVAS
--
-- Create a new OpenGL on-screen canvas covering the whole screen.  The size
-- and resolution of the canvas is based on the canvas data structure.  There
-- can only be one screen canvas.

procedure newGLScreenCanvas( H_Res, V_Res, C_Res : positive; canvas_id : out aCanvasID ) is
  newcanvas : aCanvas;
  res : SDL_success;
  videoFlags : SDL_Surface_Flag;
begin
  newcanvas.id := canvasIdTop;
  canvasIdTop := canvasIdTop + 1;
  newPenState( newCanvas.pen );
  newcanvas.displayInfo.H_Res := H_Res;
  newcanvas.displayInfo.V_Res := V_Res;
  newcanvas.displayInfo.C_Res := C_Res;

  -- Check the hardware and set the options accordingly

  newcanvas.hardware := SDL_GetVideoInfo;
  newcanvas.hardware_ptr := SDL_VideoInfo_Conv.To_Pointer( newcanvas.hardware );
  videoFlags := SDL_OPENGL + SDL_FULLSCREEN;
  if newcanvas.hardware_ptr.hw_available then
     videoFlags := videoFlags + SDL_HWSURFACE;
  else
     videoFlags := videoFlags + SDL_SWSURFACE;
  end if;
  if newcanvas.hardware_ptr.blit_hw then
     videoFlags := videoFlags + SDL_HWACCEL;
  end if;

  -- Create the window

  newcanvas.surface := SDL_SetVideoMode( Interfaces.C.int( H_Res ), Interfaces.C.int( V_Res ), Interfaces.C.int( C_Res ), videoFlags );
  res := SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  if res /= SDL_OK then
     put_line( standard_error, "newGLWindowCanvas: double buffer failed" );
  end if;
  if newcanvas.surface_ptr = null then
     put_line( standard_error, "newScreenCanvas: failed for " & H_Res'img & V_Res'img & C_Res'img & "SDL error = " & to_string( get_sdl_error ) );
  end if;
  newcanvas.surface_ptr := SDL_Surface_Conv.To_Pointer( newcanvas.surface );
  -- This is a kludge.  The first frame will not draw unless this is called
  -- once at the start.  Not sure why.  KB: 14/06/02
  SDL_GL_SwapBuffers;
  newcanvas.kind := screen;
  newcanvas.drawingType := opengl;
  newCanvas.name := to_unbounded_string( "Untitled SparForte Screen Canvas" );
  canvasList.Queue( canvas, newCanvas );
  canvas_id := newcanvas.id;
end newGLScreenCanvas;


---> NEW WINDOW CANVAS
--
-- Create a new on-screen canvas with its own window.  The size and resolution
-- of the canvas is based on the canvas data structure.

procedure newWindowCanvas( H_Res, V_Res, C_Res : positive; canvas_id : out aCanvasID ) is
  newCanvas : aCanvas;
  videoFlags : SDL_Surface_Flag;
begin
  newCanvas.id := canvasIdTop;
  canvasIdTop := canvasIdTop + 1;
  newPenState( newCanvas.pen );
  newCanvas.displayInfo.H_Res := H_Res;
  newCanvas.displayInfo.V_Res := V_Res;
  newCanvas.displayInfo.C_Res := C_Res;

  -- Check the hardware and set the options accordingly

  newcanvas.hardware := SDL_GetVideoInfo;
  newcanvas.hardware_ptr := SDL_VideoInfo_Conv.To_Pointer( newcanvas.hardware );
  videoFlags := 0;
  if newcanvas.hardware_ptr.hw_available then
     videoFlags := videoFlags + SDL_HWSURFACE;
  else
     videoFlags := videoFlags + SDL_SWSURFACE;
  end if;
  if newcanvas.hardware_ptr.blit_hw then
     videoFlags := videoFlags + SDL_HWACCEL;
  end if;

  -- Create the window

  newCanvas.surface := SDL_SetVideoMode( Interfaces.C.int( H_Res ), Interfaces.C.int( V_Res ), Interfaces.C.int( C_Res ), videoFlags );
  newCanvas.surface_ptr := SDL_Surface_Conv.To_Pointer( newcanvas.surface );
  if newCanvas.surface_ptr = null then
     put_line( standard_error, "newWindowCanvas: failed for " & H_Res'img & V_Res'img & C_Res'img & "SDL error = " & to_string( get_sdl_error ) );
  end if;
  newCanvas.kind := window;
  newcanvas.drawingType := raster;
  newCanvas.name := to_unbounded_string( "Untitled SparForte Window Canvas" );
  SDL_EXT_Window_Title( "Untitled SparForte Window Canvas" & ASCII.NUL );
  canvasList.Queue( canvas, newCanvas );
  canvas_id := newCanvas.id;
end newWindowCanvas;


---> GL NEW WINDOW CANVAS
--
-- Create a new on-screen canvas with its own window.  The size and resolution
-- of the canvas is based on the canvas data structure.

procedure newGLWindowCanvas( H_Res, V_Res, C_Res : positive; canvas_id : out aCanvasID ) is
  newCanvas : aCanvas;
  res : SDL_success;
  videoFlags : SDL_Surface_Flag;
begin
  newCanvas.id := canvasIdTop;
  canvasIdTop := canvasIdTop + 1;
  newPenState( newCanvas.pen );
  newCanvas.displayInfo.H_Res := H_Res;
  newCanvas.displayInfo.V_Res := V_Res;
  newCanvas.displayInfo.C_Res := C_Res;

  -- Check the hardware and set the options accordingly

  newcanvas.hardware := SDL_GetVideoInfo;
  newcanvas.hardware_ptr := SDL_VideoInfo_Conv.To_Pointer( newcanvas.hardware );
  videoFlags := SDL_OPENGL;
  if newcanvas.hardware_ptr.hw_available then
     videoFlags := videoFlags + SDL_HWSURFACE;
  else
     videoFlags := videoFlags + SDL_SWSURFACE;
  end if;
  if newcanvas.hardware_ptr.blit_hw then
     videoFlags := videoFlags + SDL_HWACCEL;
  end if;

  -- Create the window

  newCanvas.surface := SDL_SetVideoMode( Interfaces.C.int( H_Res ), Interfaces.C.int( V_Res ), Interfaces.C.int( C_Res ), videoFlags );
  res := SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  if res /= SDL_OK then
     put_line( standard_error, "newGLWindowCanvas: double buffer failed" );
  end if;
  newCanvas.surface_ptr := SDL_Surface_Conv.To_Pointer( newcanvas.surface );
  if newCanvas.surface_ptr = null then
     put_line( standard_error, "newWindowCanvas: failed for " & H_Res'img & V_Res'img & C_Res'img & "SDL error = " & to_string( get_sdl_error ) );
  end if;
  -- This is a kludge.  The first frame will not draw unless this is called
  -- once at the start.  Not sure why.  KB: 14/06/02
  SDL_GL_SwapBuffers;
  newCanvas.kind := window;
  newcanvas.drawingType := opengl;
  newCanvas.name := to_unbounded_string( "Untitled SparForte Window Canvas" );
  SDL_EXT_Window_Title( "Untitled SparForte Window Canvas" & ASCII.NUL );
  canvasList.Queue( canvas, newCanvas );
  canvas_id := newCanvas.id;
end newGLWindowCanvas;


---> NEW CANVAS
--
-- Create a new off-screen canvas based on the canvas data structure

procedure newCanvas( H_Res, V_Res : positive; old_canvas_id : aCanvasID; canvas_id : out aCanvasID ) is
  oldCanvas : aCanvas;
  newCanvas : aCanvas;
  oldCanvasIndex : canvasList.aListIndex := 0;
  oldCanvasSurface_ptr : SDL_Surface_Ptr;
  oldCanvasPixelFormat_ptr : SDL_PixelFormat_Ptr;
begin
  newcanvas.id := canvasIdTop;
  canvasIdTop := canvasIdTop + 1;
  -- We need the pixel format and color depth from the old surface
  oldCanvas.id := old_canvas_id;
  canvasList.Find( canvas, oldCanvas, 1, oldCanvasIndex );
  canvasList.Find( canvas, oldCanvasIndex, oldCanvas );
  oldCanvasSurface_ptr := SDL_Surface_Conv.To_Pointer( oldCanvas.surface );
  oldCanvasPixelFormat_ptr := SDL_PixelFormat_Conv.To_Pointer( oldCanvasSurface_ptr.format );
  newPenState( newCanvas.pen );
  newCanvas.displayInfo.H_Res := H_Res;
  newCanvas.displayInfo.V_Res := V_Res;
  newCanvas.displayInfo.C_Res := oldCanvas.displayInfo.C_Res;
  --newCanvas.surface := SDL_SetVideoMode( Interfaces.C.int( H_Res ), Interfaces.C.int( V_Res ), Interfaces.C.int( C_Res ), SDL_SWSURFACE );
  newCanvas.surface := SDL_CreateRGBSurface(
     SDL_SWSURFACE,
     Interfaces.C.int( H_Res ),
     Interfaces.C.int( V_res ),
     Interfaces.C.int( newCanvas.displayInfo.C_Res ),
     oldCanvasPixelFormat_ptr.Rmask,
     oldCanvasPixelFormat_ptr.Gmask,
     oldCanvasPixelFormat_ptr.Bmask,
     oldCanvasPixelFormat_ptr.Amask );
  newCanvas.surface_ptr := SDL_Surface_Conv.To_Pointer( newcanvas.surface );
  if newCanvas.surface_ptr = null then
     put_line( standard_error, "newCanvas: failed for " & H_Res'img & V_Res'img & "SDL error = " & to_string( get_sdl_error ) );
  end if;
  newCanvas.kind := offscreen;
  newcanvas.drawingType := raster;
  newCanvas.surface_ptr := SDL_Surface_Conv.To_Pointer( newcanvas.surface );
  newCanvas.name := to_unbounded_string( "Untitled SparForte Offscreen Canvas" );
  canvasList.Queue( canvas, newCanvas );
  canvas_id := newCanvas.id;
  -- NOTE: should use SDL_VideoInfo for additional information
end newCanvas;


procedure newCanvas( path : string; canvas_id : out aCanvasID ) is
  newcanvas : aCanvas;
  format_ptr : SDL_Pixelformat_Ptr;
begin
  newcanvas.id := canvasIdTop;
  canvasIdTop := canvasIdTop + 1;
  newPenState( newCanvas.pen );
  newCanvas.surface := IMG_Load( path & ASCII.NUL );
  newcanvas.surface_ptr := SDL_Surface_Conv.To_Pointer( newcanvas.surface );
  if newcanvas.surface_ptr /= null then
    newCanvas.name := to_unbounded_string( path );
  else
    put_line( standard_error, "Load failed, SDL error = " & to_string( get_sdl_error ) );
  end if;
  newcanvas.kind := offscreen;
  newcanvas.drawingType := raster;
  newcanvas.displayInfo.H_Res := integer( newCanvas.surface_ptr.w );
  newcanvas.displayInfo.V_Res := integer( newCanvas.surface_ptr.h );
  format_ptr := SDL_PixelFormat_Conv.To_Pointer( get_format_address( newcanvas.surface ) );
  newcanvas.displayInfo.C_Res := integer( format_ptr.BitsPerPixel );
  canvasList.Queue( canvas, newCanvas );
  canvas_id := newcanvas.id;
  -- should use SDL_VideoInfo for additional information
end newCanvas;


---> SAVE CANVAS
--
-- Save a canvas to a BMP image file.
-----------------------------------------------------------------------------

procedure saveCanvas( path : string; canvas_id : aCanvasID ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
  res : SDL_success;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
     if theCanvas.kind = window then
        res := SDL_EXT_Save_BMP( theCanvas.surface, path & ASCII.NUL );
        if res /= 0 then
           put_line( standard_error, "unable to save canvas id -" & canvas_id'img );
        end if;
     end if;
  end if;
end saveCanvas;


---> GET DISPLAY INFO
--
-- Return the display info from a canvas.
-----------------------------------------------------------------------------
-- can't really do this in bush 1.x because record types are limited.

--procedure getDisplayInfo( canvas_id : canvasList.aListIndex;  displayInfo : out aDisplayInfoRec ) is
--  theCanvas : aCanvas;
--begin
--  canvasList.Find( canvas, canvas_id, theCanvas );
--  displayinfo := theCanvas.displayInfo
--end getDisplayInfo;


procedure setTitle( canvas_id : aCanvasID; title :unbounded_string ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     theCanvas.name := title;
     canvasList.Replace( canvas, canvasIndex, theCanvas );
     if theCanvas.kind = window then
        SDL_EXT_Window_Title( to_string( title ) & ASCII.NUL );
     end if;
  end if;
end setTitle;


---> CLOSE CANVAS
--
-- Discard a canvas created with newCanvas, newScreenCanvas or newWindowCanvas.
-- (But it doesn't remove a window opened..you have to shut down).
-----------------------------------------------------------------------------

procedure closeCanvas( canvas_id : aCanvasID ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     SDL_FreeSurface( theCanvas.surface );
     if theCanvas.hasFont then
        TTF_CloseFont( theCanvas.font_ptr  );
     end if;
     canvasList.Clear( canvas, canvasIndex );
  end if;
end closeCanvas;


---> CLIP RECT
--
-- Set the clipping region to the given rectangle.

procedure clipRect( theCanvas : in out aCanvas; r : aRect ) is
  newClipRect : SDL_Rect;
begin
  -- null rect handling
  theCanvas.clipRect := r;
  newClipRect.x := svgah( theCanvas, r.left );
  newClipRect.y := svgav( theCanvas, r.top );
  newClipRect.w := Uint16( svgah( theCanvas, r.right ) - svgah( theCanvas, r.left ) );
  newClipRect.h := Uint16( svgav( theCanvas, r.bottom ) - svgav( theCanvas, r.top ) );
  SDL_SetClipRect( theCanvas.surface, newClipRect );
end clipRect;

procedure clipRect( canvas_id : aCanvasID; r : aRect ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     clipRect( theCanvas, r );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end clipRect;


---> MOVING
--
-- Moving the pen without drawing.
-----------------------------------------------------------------------------

procedure moveTo( theCanvas : in out aCanvas; x, y : aCoordinate ) is
begin
  theCanvas.pen.x := x;
  theCanvas.pen.y := y;
end moveTo;

procedure moveTo( canvas_id : aCanvasID; x, y : aCoordinate ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     moveTo( theCanvas, x, y );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end moveTo;

procedure move( theCanvas : in out aCanvas; dx, dy : aCoordinate ) is
begin
  theCanvas.pen.x := theCanvas.pen.x + dx;
  theCanvas.pen.y := theCanvas.pen.y + dy;
end move;

procedure move( canvas_id : aCanvasID; dx, dy : aCoordinate ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     move( theCanvas, dx, dy );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end move;

---> LINES
--
-- Moving the pen with drawing.
-----------------------------------------------------------------------------

procedure lineTo( theCanvas : in out aCanvas; x, y : aCoordinate ) is
begin
  SDL_EXT_line( theCanvas.surface,
    svgah( theCanvas, theCanvas.pen.x ), svgav( theCanvas, theCanvas.pen.y ),
    svgah( theCanvas, x ), svgav( theCanvas, y ), theCanvas.pen.pixel, theCanvas.pen.mode, theCanvas.pen.revealCount = 0 );
  theCanvas.pen.x := x;
  theCanvas.pen.y := y;
end lineTo;

procedure lineTo( canvas_id : aCanvasID; x, y : aCoordinate ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     lineTo( theCanvas, x, y );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end lineTo;

procedure line( theCanvas : in out aCanvas; dx, dy : aCoordinate ) is
begin
  SDL_EXT_line( theCanvas.surface,
    svgah( theCanvas, theCanvas.pen.x ), svgav( theCanvas, theCanvas.pen.y ),
    svgah( theCanvas, theCanvas.pen.x + dx ), svgav( theCanvas, theCanvas.pen.y + dy ),
    theCanvas.pen.pixel, theCanvas.pen.mode, theCanvas.pen.revealCount = 0 );
  theCanvas.pen.x := theCanvas.pen.x + dx;
  theCanvas.pen.y := theCanvas.pen.y + dy;
end line;

procedure line( canvas_id : aCanvasID; dx, dy : aCoordinate ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     line( theCanvas, dx, dy );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end line;

procedure hline( theCanvas : in out aCanvas; x1, x2, y : aCoordinate ) is
begin
  case theCanvas.pen.brush is

  when stretch => -- not complete
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_HLine_Pattern( theCanvas.surface, svgah( theCanvas, x1 ), svgah( theCanvas, x2 ), svgav( theCanvas, y ), theCanvas.pen.pattern, 0, 0, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when tile =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_HLine_Pattern( theCanvas.surface, svgah( theCanvas, x1 ), svgah( theCanvas, x2 ), svgav( theCanvas, y ), theCanvas.pen.pattern, 0, 0, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when stamp =>
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_HLine_Pattern( theCanvas.surface, svgah( theCanvas, x1 ), svgah( theCanvas, x2 ), svgav( theCanvas, y ), theCanvas.pen.pattern, svgah( theCanvas, x1 ), svgav( theCanvas, y ), theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when smear => -- not done
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_HLine_Pattern( theCanvas.surface, svgah( theCanvas, x1 ), svgah( theCanvas, x2 ), svgav( theCanvas, y ), theCanvas.pen.pattern, 0, 0, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when others => -- including pencil
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_HLine( theCanvas.surface, svgah( theCanvas, x1 ), svgah( theCanvas, x2 ), svgav( theCanvas, y ), theCanvas.pen.pixel, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;
  end case;
  if theCanvas.pen.revealCount = 0 then
     SDL_UpdateRect( theCanvas.surface, svgah( theCanvas, x1 ), svgav( theCanvas, y ), Uint32( svgav( theCanvas, x2 ) - svgav( theCanvas, x1 ) + 1 ), 1 );
  end if;
end hline;

procedure hline( canvas_id : aCanvasID; x1, x2, y : aCoordinate ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     hline( theCanvas, x1, x2, y );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
   end if;
end hline;

procedure vline( theCanvas : in out aCanvas; x, y1, y2 : aCoordinate ) is
begin
  case theCanvas.pen.brush is

  when stretch => -- not complete
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     put_line( standard_error, "not yet written" );
        --SDL_EXT_VLine_Pattern( theCanvas.surface, svgah( theCanvas, x ), svgav( theCanvas, y1 ), svgav( theCanvas, y2 ), theCanvas.pen.pattern, 0, 0, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when tile => -- not complete
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     put_line( standard_error, "not yet written" );
        --SDL_EXT_VLine_Pattern( theCanvas.surface, svgah( theCanvas, x ), svgav( theCanvas, y1 ), svgav( theCanvas, y2 ), theCanvas.pen.pattern, 0, 0, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when stamp => -- not complete
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
        --SDL_EXT_VLine_Pattern( theCanvas.surface, svgah( theCanvas, x ), svgav( theCanvas, y1 ), svgav( theCanvas, y2 ), theCanvas.pen.pattern, svgah( theCanvas, x1 ), svgav( theCanvas, y ), theCanvas.pen.mode );
     put_line( standard_error, "not yet written" );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when smear => -- not complete
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     put_line( standard_error, "not yet written" );
        --SDL_EXT_VLine_Pattern( theCanvas.surface, svgah( theCanvas, x ), svgav( theCanvas, y1 ), svgav( theCanvas, y2 ), theCanvas.pen.pattern, 0, 0, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;

  when others => -- including pencil
     if theCanvas.pen.revealCount = 0 then
        if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
           put_line( standard_error, "unable to lock SDL surface" );
        end if;
     end if;
     SDL_EXT_VLine( theCanvas.surface, svgah( theCanvas, x ), svgav( theCanvas, y1 ), svgav( theCanvas, y2 ), theCanvas.pen.pixel, theCanvas.pen.mode );
     if theCanvas.pen.revealCount = 0 then
        SDL_UnlockSurface( theCanvas.surface  );
     end if;
  end case;
  if theCanvas.pen.revealCount = 0 then
     SDL_UpdateRect( theCanvas.surface, svgah( theCanvas, x ), svgav( theCanvas, y1 ), 1, Uint32( svgav( theCanvas, y2 ) - svgav( theCanvas, y1 ) + 1 ) );
  end if;
end vline;

procedure vline( canvas_id : aCanvasID; x, y1, y2 : aCoordinate ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     vline( theCanvas, x, y1, y2 );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end vline;

procedure plot( theCanvas : aCanvas; values : plotValues ) is
begin
  put_line( "plot called" ); -- DEBUG
end plot;

procedure plot( canvas_id : aCanvasID; values : plotValues ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     plot( theCanvas, values );
     canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end plot;

procedure Stretch( sourceCanvas : aCanvas; targetCanvas : in out aCanvas; target_x, target_y : aCoordinate; newWidth, newHeight : aCoordinate ) is
  scaledSurface      : system.address;
  source_x1          : SDL_HCoordinate;
  source_y1          : SDL_VCoordinate;
  source_x2          : SDL_HCoordinate;
  source_y2          : SDL_VCoordinate;
  scaled_x1          : SDL_HCoordinate;
  scaled_y1          : SDL_VCoordinate;
  scaled_x2          : SDL_HCoordinate;
  scaled_y2          : SDL_VCoordinate;
  NoHorizontalChange : boolean;
  NoVerticalChange   : boolean;
  source_range       : SDL_Rect;
  target_range       : SDL_Rect;
  Rmask              : SDL_RGB_Mask;
  Gmask              : SDL_RGB_Mask;
  Bmask              : SDL_RGB_Mask;
  Amask              : SDL_RGB_Mask;
  res                : SDL_Success;
  surface_ptr        : SDL_Surface_Ptr;
begin

  -- Convert to SDL coordinates and compute the target surface bounds

  svgaEffectiveRectangle( sourceCanvas, sourceCanvas.bounds,
     source_x1, source_y1, source_x2, source_y2 );
  -- scaled_x2 := source_x2 + svgah( sourceCanvas, newWidth );
  -- scaled_y2 := source_y2 + svgav( sourceCanvas, newHeight );
  scaled_x2 := svgah( sourceCanvas, newWidth );
  scaled_y2 := svgav( sourceCanvas, newHeight );
  scaled_x1 := 0;
  scaled_y1 := 0;
  -- scaled_x2 := scaled_x2 - source_x1;
  -- scaled_y2 := scaled_y2 - source_y1;

  -- Create the temporary scaled surface and check surface sizes

  if source_x1 + source_x2 = 0 then
     return;
  end if;
  -- scaledSurface := SDL_SetVideoMode( scaled_x2, scaled_y2, Interfaces.C.int( theCanvas.displayInfo.C_Res ), SDL_SWSURFACE );
--put_line( "A" );
  SDL_EXT_Get_Pixel_Masks( Interfaces.C.int( sourceCanvas.displayInfo.C_Res ), Rmask, Gmask, Bmask, Amask );
  scaledSurface := SDL_CreateRGBSurface( SDL_SWSURFACE, Interfaces.C.int( scaled_x2 ), Interfaces.C.int( scaled_y2 ), Interfaces.C.int( sourceCanvas.displayInfo.C_Res ), Rmask, Gmask, Bmask, Amask );
  surface_ptr := SDL_Surface_Conv.To_Pointer( scaledSurface );
  if surface_ptr = null then
     put_line( standard_error, to_string( get_sdl_error ) );
     return;
  end if;
  NoVerticalChange  := (source_y2 = scaled_y2);
  NoHorizontalChange:= (source_x2 = scaled_x2);
--put_line( "B" ); -- DEBUG

  -- Copy the pixels

  if NoVerticalChange then
--put_line( "C1" );
     for y in 0..scaled_y2 loop
         for x in 0..scaled_x2 loop
             SDL_EXT_Plot( scaledSurface, x, y,
                SDL_EXT_Raw_Pixel( sourceCanvas.surface, scaledSurface,
                x * (scaled_x2/source_x2), y ), copy );
         end loop;
      end loop;
  elsif NoHorizontalChange then
--put_line( "C2" );
     for y in 0..scaled_y2 loop
         for x in 0..scaled_x2 loop
             SDL_EXT_Plot( scaledSurface, x, y,
                SDL_EXT_Raw_Pixel( sourceCanvas.surface, scaledSurface,
                x, y * (scaled_y2/source_y2) ), copy );
         end loop;
      end loop;
  else
--put_line( "C3 - right" );
     for y in 0..scaled_y2 loop
         for x in 0..scaled_x2 loop
             -- SDL_EXT_Plot( scaledSurface, x, y,
             --    SDL_EXT_Raw_Pixel( sourceCanvas.surface, scaledSurface,
             --    x * (scaled_x2/source_x2), y * (scaled_y2/source_y2) ), copy );
             SDL_EXT_Plot( scaledSurface, x, y, 10240, copy );
         end loop;
      end loop;
  end if;
--put_line( "D" );

  -- Draw stretched image

  source_range := SDL_Rect'( 0, 0, w => Uint16( scaled_x2 ), h => Uint16( scaled_y2 ) );
  -- target_range := SDL_Rect'( svgah( targetCanvas, target_x ), svgav( targetCanvas, target_y ) + SDL_VCoordinate( Uint16( scaled_y2 ) ), 0, 0 );
  target_range := SDL_Rect'( svgah( targetCanvas, target_x ), svgav( targetCanvas, target_y ), 0, 0 );
--put_line( "pattern source range = " & scaled_x2'img & scaled_y2'img );
--put_line( "screen target range = " & target_range.x'img & target_range.y'img & SDL_HCoordinate'image( target_range.x + scaled_x2 ) & SDL_VCoordinate'image(target_range.y + scaled_y2 ) );
SDL_EXT_Frame_Rect( targetcanvas.surface, target_range.x, target_range.y, target_range.x + scaled_x2, target_range.y + scaled_y2, -1, copy );
  -- res := SDL_UpperBlit( scaledSurface, source_range, targetCanvas.surface, target_range );
  res := SDL_UpperBlit( scaledSurface, source_range, targetCanvas.surface, target_range );
  if res < 0 then
     put( standard_error, "SDL_UpperBlit blit failed: " & to_string( get_sdl_error ) );
     new_line( standard_error );
  end if;

  -- free scaled surface here else memory leak
--put_line( "E" );

  -- SDL free's surfaces so we have a problem....
  -- SDL_FreeSurface( scaledSurface );
--put_line( "F" );
     if SDL_Flip( targetCanvas.surface ) /= SDL_OK then
        null;
     end if;

end Stretch;


---> SET FONT
--
-- Assign a true type font to a canvas
-----------------------------------------------------------------------------

procedure SetFont( canvas_id : aCanvasID; path : unbounded_string; points : natural ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
  path_str : string := to_string( path );
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
    canvasList.Find( canvas, canvasIndex, theCanvas );
    declare
      f : file_type;
    begin
      open( f, in_file, path_str );
      close( f );
      -- TODO: check result for null, handle bad path (though this
      -- should be mitigated by the open above
      if theCanvas.hasFont then
         TTF_CloseFont( theCanvas.font_ptr  );
      end if;
      theCanvas.font_ptr := TTF_OpenFont( path_str & ASCII.NUL, interfaces.c.int( points ) );
      -- TTF_SetFontStyle(font, renderstyle);
      -- TTF_SetFontOutline(font, outline);
      -- TTF_SetFontKerning(font, kerning);
      -- TTF_SetFontHinting(font, hinting);
      theCanvas.hasFont := true;
      canvasList.Replace( canvas, canvasIndex, theCanvas );
    end;
  end if;
end SetFont;


---> PUT
--
-- Print a message in the current font.
-----------------------------------------------------------------------------
-- TODO: this is not working

procedure Put( canvas_id : aCanvasID; str : unbounded_string ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
  textSurface : system.address;
  svga_x1 : SDL_HCoordinate;
  svga_y1 : SDL_VCoordinate;
  svga_x2 : SDL_HCoordinate;
  svga_y2 : SDL_VCoordinate;
  r : aRect;
  textSurface_ptr : SDL_Surface_Ptr;
begin
put_line( "Put on canvas " & canvas_id'img );
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
     canvasList.Find( canvas, canvasIndex, theCanvas );
     if theCanvas.hasFont then
put_line( "has font" );

       -- render the text on a separate off-screen surface.  (Alpha blending not yet implemented)

       clear( theCanvas, blue );
       setPenInk( theCanvas, white ); -- no effect?

       textSurface := TTF_RenderText_Solid( theCanvas.font_ptr, to_string( str ) & ASCII.NUL, theCanvas.pen.pixel );
       textSurface_ptr := SDL_Surface_Conv.To_Pointer( textSurface );
put_line( "rendered: " & textSurface_ptr.W'img & " x" & textSurface_ptr.H'img );

       -- TODO: handle null

       -- determine the current pen location (x1, y1).  Determine the number of pixels for
       -- ( x2, y2 ) in order to hold the text (based on the text surface).

       r.left := theCanvas.pen.X;
       r.top  := theCanvas.pen.Y;
       r.right := 100.0;
       r.bottom := 100.0;
       svgaEffectiveRectangle( theCanvas, r, svga_x1, svga_y1, svga_x2, svga_y2 );
       if svga_x1 + svga_x2 = 0 then
          return;
       end if;
       -- TODO: clipping not implemented
       svga_x1 := 10;
       svga_y1 := 10;
       svga_x2 := svga_x1 + SDL_HCoordinate( textSurface_ptr.w );
       svga_y2 := svga_y1 + SDL_VCoordinate( textSurface_ptr.h );
put_line( "(" & svga_x1'img & "," & svga_y1'img & ") (" & svga_x2'img & "," & svga_y2'img & ")" );
       -- draw the text as any other.  lock the surface if waitToReveal is not used.  Free
       -- the text surface when done.

put_line( "text" );
       setPenInk( theCanvas, grey );
       SDL_EXT_Copy_Fill_Rect_Pattern( theCanvas.surface, svga_x1, svga_y1, svga_x2, svga_y2, textSurface );
       -- SDL_BlitSurface(
       SDL_FreeSurface( textSurface );

       if theCanvas.pen.revealCount = 0 then
put_line( "lock" );
          if SDL_LockSurface( theCanvas.surface  ) /= SDL_OK then
             put_line( standard_error, "unable to lock SDL surface" );
          end if;
       end if;
put_line( "rect" );
       --SDL_Ext_Line( theCanvas.surface, svga_x1, svga_y1, svga_x2, svga_y1, theCanvas.pen.pixel, copy, true );
       --SDL_Ext_Line( theCanvas.surface, svga_x1, svga_y1, svga_x1, svga_y2, theCanvas.pen.pixel, copy, true );
       --SDL_Ext_Line( theCanvas.surface, svga_x1, svga_y2, svga_x2, svga_y1, theCanvas.pen.pixel, copy, true );
       --SDL_Ext_Line( theCanvas.surface, svga_x2, svga_y1, svga_x2, svga_y2, theCanvas.pen.pixel, copy, true );

       if theCanvas.pen.revealCount = 0 then
put_line( "unlock" );
          SDL_UnlockSurface( theCanvas.surface  );
       end if;

    end if;
    canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end Put;


---> CLOSE FONT
--
-- Close a font opened with set font.
-----------------------------------------------------------------------------

procedure CloseFont( canvas_id : aCanvasID ) is
  theCanvas : aCanvas;
  canvasIndex : canvasList.aListIndex := 0;
begin
  theCanvas.id := canvas_id;
  canvasList.Find( canvas, theCanvas, 1, canvasIndex );
  if canvasIndex = 0 then
     put_line( standard_error, "no such canvas id -" & canvas_id'img );
  else
    canvasList.Find( canvas, canvasIndex, theCanvas );
    if theCanvas.hasFont then
      TTF_CloseFont( theCanvas.font_ptr );
      theCanvas.hasFont := false;
    end if;
    canvasList.Replace( canvas, canvasIndex, theCanvas );
  end if;
end CloseFont;

end pen;

