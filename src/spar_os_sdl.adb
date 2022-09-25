------------------------------------------------------------------------------
-- Bindings to the SDL (Simple DirectMedia Layer) Library                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2017 Free Software Foundation              --
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

with ada.text_io; use ada.text_io; -- debug
package body spar_os.sdl is

use spar_os.sdl.SDL_Surface_Conv;

function debug_sdl_error_char( i : integer ) return character;
pragma import( C, debug_sdl_error_char, "debug_sdl_error_char" );

-----------------------------------------------------------------------------
-- GET SDL ERROR
--
-- Return the last SDL error message
-----------------------------------------------------------------------------

function get_sdl_error return unbounded_string is
  i  : integer := 0;
  ch : character;
  errorMessage : unbounded_string;
begin
  loop
    ch := debug_sdl_error_char( i );
    exit when ch = ASCII.NUL;
    errorMessage := errorMessage & ch;
    i := i + 1;
  end loop;
  return errorMessage;
end get_sdl_error;


-----------------------------------------------------------------------------
-- SDL EXT(ension) PLOT PATTERN
--
-- Set a single pixel in any SDL video mode.  There is no clipping and no
-- the surface must be locked.  Uses a pattern to get the pixel.
-----------------------------------------------------------------------------

procedure SDL_EXT_Plot_Pattern( screen : in system.address;
          pixel_x : SDL_HCoordinate; pixel_y : SDL_VCoordinate;
          start_offset_x : SDL_HCoordinate; start_offset_y : SDL_VCoordinate;
          pattern : system.address; mode : aPenMode ) is
  pattern_x : SDL_HCoordinate;
  pattern_y : SDL_VCoordinate;
  pattern_ptr : SDL_Surface_Ptr := SDL_Surface_Conv.To_Pointer( pattern );
  -- This is inefficient but it works.
begin
  if pattern_ptr /= null then
     pattern_x := (start_offset_x + pixel_x) mod SDL_HCoordinate( pattern_ptr.h );
     pattern_y := (start_offset_y + pixel_y) mod SDL_VCoordinate( pattern_ptr.w );
     SDL_EXT_Plot( screen, pixel_x, pixel_y,
                SDL_EXT_Raw_Pixel( pattern, screen, pattern_x, pattern_y ), mode );
  end if;
end SDL_EXT_Plot_Pattern;


-----------------------------------------------------------------------------
-- SDL EXT(ension) HLine Pattern
--
-- Draw a horizontal line using a pattern in any SDL video mode.
-----------------------------------------------------------------------------

procedure SDL_EXT_HLine_Pattern( screen : in system.address; x1, x2 : SDL_HCoordinate; y : SDL_VCoordinate; pattern : in system.address; start_offset_x : SDL_HCoordinate; start_offset_y : SDL_VCoordinate; mode : aPenMode ) is
  pattern_ptr : SDL_Surface_Ptr;
  pattern_x   : SDL_Hcoordinate;
  pattern_y   : SDL_Vcoordinate;
  pixel_colour: SDL_Generic_Pixel;
begin
  pattern_ptr := SDL_Surface_Conv.To_Pointer( pattern );
  if pattern_ptr /= null then
     -- WARNING: no clip check
     pattern_x := start_offset_x mod SDL_HCoordinate( pattern_ptr.w );
     pattern_y := start_offset_y mod SDL_VCoordinate( pattern_ptr.h );
     for x in x1..x2 loop
         pixel_colour := SDL_EXT_Raw_Pixel( pattern, screen, pattern_x, pattern_y );
         SDL_EXT_Plot( screen, x, y, pixel_colour, mode );
         pattern_x := (pattern_x + 1) mod SDL_HCoordinate( pattern_ptr.w );
     end loop;
  end if;
end SDL_EXT_HLine_Pattern;


-----------------------------------------------------------------------------
-- SDL EXT(ension) Line
--
-- Draw a line between two points.
-----------------------------------------------------------------------------

procedure SDL_Ext_Line( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                pixel_colour : SDL_Generic_Pixel; mode : aPenMode;
                mustReveal : boolean := true ) is
  x1 : SDL_HCoordinate := start_x;
  y1 : SDL_VCoordinate := start_y;
  x2 : SDL_HCoordinate := end_x;
  y2 : SDL_VCoordinate := end_y;
  dy : Sint32 := Sint32( y2 ) - Sint32( y1 );
  dx : Sint32 := Sint32( x2 ) - Sint32( x1 );

  G, DeltaG1, DeltaG2 : Sint32;
  inc : Sint32 := 1;

 pragma suppress( range_check );
 pragma suppress( index_check );

begin

    if abs(dy) < abs(dx) then                          -- x bigger? move by x
      if dx < 0 then                                   -- moving backwards?
        dx := -dx;                                     -- move forwards by
        dy := -dy;                                     -- reversing slope
        y2 := start_y;                                 -- swap start/stop
        y1 := end_y;
        x2 := start_x;
        x1 := end_x;
      end if;
      if dy < 0 then                                   -- moving up?
        dy := -dy+1;                                     -- move normally
        inc := -1;                                     -- with a neg incr
      end if;

      G := 2 * dy - dx;                                -- a little math voodoo
      DeltaG1 := 2 * (dy - dx);
      DeltaG2 := 2 * dy;

      loop
        SDL_Ext_Plot( screen, x1, y1, pixel_colour, mode );
        if mustReveal then
           SDL_UpdateRect( screen, x1, y1, 1, 1 );
        end if;
        x1 := x1 + 1;                                    -- moving by x
        exit when x1 > x2;                              -- quit when exceeded
        if G >= 0 then                                  -- error adjustment
           G := G + DeltaG1;
           y1 := y1 + SDL_VCoordinate( inc );
        else
           G := G + DeltaG2;
        end if;
      end loop;

  else -- /* ramp < -1 or ramp > 1 */

      if dy < 0 then
        dx := -dx;
        dy := -dy;
        y2 := start_y;
        y1 := end_y;
        x2 := start_x;
        x1 := end_x;
      end if;
      if dx < 0 then
        -- dx := -dx+1;
        dx := -dx;
        inc := -1;
      end if;

      G := 2 * dx - dy;
      DeltaG1 := 2 * (dx - dy);
      DeltaG2 := 2 * dx;

      loop
        SDL_Ext_Plot( screen, x1, y1, pixel_colour, mode );
        if mustReveal then
           SDL_UpdateRect( screen, x1, y1, 1, 1 );
        end if;
        y1 := y1 + 1;
        exit when y1 > y2;
        if G >= 0 then
           G := G + DeltaG1;
           x1 := x1 + SDL_HCoordinate( inc );
        else
           G := G + DeltaG2;
        end if;
      end loop;
  end if;

end SDL_Ext_Line;


-----------------------------------------------------------------------------
-- SDL EXT(ension) Frame Rect
--
-- Draw an outline of a rectangle between 2 corner points.
-----------------------------------------------------------------------------

procedure SDL_Ext_Frame_Rect( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                pixel_colour : SDL_Generic_Pixel;
                mode : aPenMode ) is
begin
  SDL_Ext_Line( screen, start_x, start_y, start_x, end_y, pixel_colour, mode );
  SDL_Ext_Line( screen, start_x, start_y, end_x, start_y, pixel_colour, Mode );
  SDL_Ext_Line( screen, end_x, start_y, end_x, end_y, pixel_colour, mode );
  SDL_Ext_Line( screen, start_x, end_y, end_x, end_y, pixel_colour, mode );
end SDL_Ext_Frame_Rect;


-----------------------------------------------------------------------------
-- SDL EXT(ension) Fill Rect
--
-- Draw a filled in rectangle.  Unlike SDL_FillRect, support pen modes.
-----------------------------------------------------------------------------

procedure SDL_EXT_Fill_Rect( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                pixel_colour : SDL_Generic_Pixel;
                mode : aPenMode ) is
  svga_rect : SDL_Rect;
  res : SDL_Success;
begin
  svga_rect.x := start_x;
  svga_rect.y := start_y;
  svga_rect.w := Uint16( end_x - start_x ) + 1;
  svga_rect.h := Uint16( end_y - start_y ) + 1;
  if mode = copy then
     res := SDL_FillRect( screen, svga_rect'address, pixel_colour );
  else
     for y in start_y..end_y loop
         SDL_Ext_HLine( screen, start_x, end_x, y, pixel_colour, Mode );
     end loop;
  end if;
end SDL_EXT_Fill_Rect;


-----------------------------------------------------------------------------
-- SDL EXT(ension) Copy Fill Rect Pattern
--
-- Draw a filled in rectangle with a pattern between 2 corner points.  No
-- clipping.  Surface must be unlocked.  Makes use of hardware blitting.
-----------------------------------------------------------------------------

procedure SDL_EXT_copy_fill_rect_pattern( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                pattern : system.address ) is
  svga_rect : SDL_Rect;
  source_range : SDL_Rect;
  target_range : SDL_Rect;
  pattern_ptr : SDL_Surface_Ptr;
  res : SDL_Success;
  x_repeat : Uint16;
  x_remainder : Uint16;
  y_repeat : Uint16;
  y_remainder : Uint16;
  screen_ptr : SDL_Surface_Ptr; -- debug
begin
    pattern_ptr := SDL_Surface_Conv.To_Pointer( pattern );
    screen_ptr := SDL_Surface_Conv.To_Pointer( screen );
    if pattern_ptr /= null then
       svga_rect.x := start_x;
       svga_rect.y := start_y;
       svga_rect.w := Uint16( end_x - start_x + 1);
       svga_rect.h := Uint16( end_y - start_y + 1);

       x_repeat    := svga_rect.w / Uint16( pattern_ptr.w );
       x_remainder := svga_rect.w - Uint16( pattern_ptr.w ) * x_repeat;
       y_repeat    := svga_rect.h / Uint16( pattern_ptr.h );
       y_remainder := svga_rect.h - Uint16( pattern_ptr.h ) * y_repeat;

       source_range := SDL_Rect'( 0, 0, w => Uint16( pattern_ptr.w ), h => Uint16( pattern_ptr.h ) );
       target_range := SDL_Rect'( svga_rect.x, svga_rect.y, 0, 0 );
-- put_line( "-BLIT---------------------------------------------------" );
-- put_line( "x_repeat = " & x_repeat'img );
-- put_line( "y_repeat = " & y_repeat'img );
-- put_line( "x_remainder = " & x_remainder'img );
-- put_line( "y_remainder = " & y_remainder'img );
-- put_line( "screen  w = " & screen_ptr.w'img );
-- put_line( "screen  h = " & screen_ptr.h'img );
-- put_line( "pattern w = " & pattern_ptr.w'img );
-- put_line( "pattern h = " & pattern_ptr.h'img );
-- put_line( "target  x = " & svga_rect.x'img );
-- put_line( "target  y = " & svga_rect.y'img );
-- put_line( "--------------------------------------------------------" );
       if y_repeat > 0 then -- GCC 3.4 - for loop executes on 0..-1
       for y in 0..y_repeat-1 loop
           source_range := SDL_Rect'( 0, 0, w => Uint16( pattern_ptr.w ), h => Uint16( pattern_ptr.h ) );
           target_range := SDL_Rect'( svga_rect.x, svga_rect.y + SDL_VCoordinate( source_range.h * y ), 0, 0 );
           for x in 1..x_repeat loop
               res := SDL_UpperBlit( pattern, source_range, screen, target_range );
               if res < 0 then
                  put( standard_error, "SDL_UpperBlit blit failed: SDL Error: " & to_string( get_sdl_error ) );
                  new_line( standard_error );
               end if;
               target_range.x := target_range.x + SDL_HCoordinate( source_range.w );
           end loop;
           if x_remainder > 0 then
               source_range.w := x_remainder;
               res := SDL_UpperBlit( pattern, source_range, screen, target_range );
           end if;
       end loop;
       end if;
       if y_remainder > 0 then
           source_range := SDL_Rect'( 0, 0, w => Uint16( pattern_ptr.w ), h => y_remainder );
           target_range := SDL_Rect'( svga_rect.x, svga_rect.y + SDL_VCoordinate( Uint16( pattern_ptr.h ) * y_repeat ), 0, 0 );
           -- x_remainder := 0;
           for x in 1..x_repeat loop
               res := SDL_UpperBlit( pattern, source_range, screen, target_range );
               if res < 0 then
                  put( standard_error, "SDL_UpperBlit blit (2) failed: SDL Error: " & to_string( get_sdl_error ) );
                  new_line( standard_error );
               end if;
               target_range.x := target_range.x + SDL_HCoordinate( source_range.w );
           end loop;
           if x_remainder > 0 then
               source_range.w := x_remainder;
               res := SDL_UpperBlit( pattern, source_range, screen, target_range );
               if res < 0 then
                  put( standard_error, "SDL_UpperBlit blit (3) failed: SDL Error: " & to_string( get_sdl_error ) );
                  new_line( standard_error );
               end if;
           end if;
       end if;
    end if;
end SDL_EXT_copy_fill_rect_pattern;


-----------------------------------------------------------------------------
-- SDL EXT(ension) Fill Rect Pattern
--
-- Draw a filled in rectangle with a pattern between 2 corner points.  No
-- clipping.  Surface must be locked unless mode copy.
-----------------------------------------------------------------------------

procedure SDL_EXT_fill_rect_pattern( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                start_offset_x : SDL_HCoordinate; start_offset_y : SDL_VCoordinate;
                pattern : system.address; mode : aPenMode ) is
  svga_rect : SDL_Rect;
begin
  svga_rect.x := start_x;
  svga_rect.y := start_y;
  svga_rect.w := Uint16( end_x - start_x ) + 1;
  svga_rect.h := Uint16( end_y - start_y ) + 1;
  if mode = copy and start_offset_x = 0 and start_offset_y = 0 then
     SDL_EXT_copy_fill_rect_pattern( screen, start_x, start_y, end_x, end_y, pattern );
  else
     for y in start_y..end_y loop
         SDL_Ext_HLine_Pattern( screen, start_x, end_x, y, pattern, start_offset_x, start_offset_y + y, mode );
     end loop;
  end if;
end SDL_EXT_fill_rect_pattern;


-----------------------------------------------------------------------------
-- SDL EXT(ension) Frame Ellipse
--
-- Draw an outline of a ellipse between 2 corner points.
-----------------------------------------------------------------------------
-- What happens when it doesn't divide evenly

procedure SDL_EXT_frame_ellipse( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                pixel_colour : SDL_Generic_Pixel; mode : aPenMode ) is

  Xradius : Uint16 := Uint16( end_x - start_x + 2 ) / 2;
  Yradius : Uint16 := Uint16( end_y - start_y + 2 ) / 2;
  x0 : SDL_HCoordinate := start_x + SDL_HCoordinate( Xradius ) - 1;
  y0 : SDL_VCoordinate := start_y + SDL_VCoordinate( Yradius ) - 1;
  x  : SDL_HCoordinate;
  y  : SDL_VCoordinate;
  Xchange : Sint32;
  Ychange : Sint32;
  EllipseError : Sint32;
  TwoASquare : Sint32;
  TwoBSquare : Sint32;
  StoppingX  : Sint32;
  StoppingY  : Sint32;

begin

  TwoASquare := 2 * Sint32( Xradius ) * Sint32( Xradius );
  TwoBSquare := 2 * Sint32( Yradius ) * Sint32( Yradius );

  -- 1st set of points
  x := SDL_HCoordinate( Xradius ) - 1;  -- /*radius zero == draw nothing*/
  y := 0;

  Xchange := Sint32( Yradius ) * Sint32( Yradius ) * (1-2*Sint32(Xradius));
  Ychange := Sint32( Xradius ) * Sint32( Xradius );

  EllipseError := 0;

  StoppingX := TwoBSquare*Sint32(Xradius);
  StoppingY := 0;
  -- /*Plot four ellipse points by iteration*/

-- put_line( "start_x = " & start_x'img );
-- put_line( "end_x = " & end_x'img );
-- put_line( "start_y = " & start_y'img );
-- put_line( "end_y = " & end_y'img );
-- put_line( "Xradius = " & Xradius'img );
-- put_line( "Yradius = " & Yradius'img );
-- put_line( "StoppingX = " & StoppingX'img );
-- put_line( "StoppingY = " & StoppingY'img );
-- put_line( "x0 = " & x0'img );
-- put_line( "y0 = " & y0'img );
-- put_line( "x  = " & x'img );
-- put_line( "y  = " & y'img );

  while StoppingX > StoppingY loop

    SDL_EXT_Plot( screen, x0+x, y0+y, pixel_colour, mode );
    SDL_EXT_Plot( screen, x0-x, y0+y, pixel_colour, mode );
    SDL_EXT_Plot( screen, x0+x, y0-y, pixel_colour, mode );
    SDL_EXT_Plot( screen, x0-x, y0-y, pixel_colour, mode );

    y := y + 1;
    StoppingY    := StoppingY + TwoASquare;
    EllipseError := EllipseError + Ychange;
    Ychange      := Ychange + TwoASquare;
    if 2 * EllipseError + Xchange > 0 then
      x := x - 1;
      StoppingX    := StoppingX - TwoBSquare;
      EllipseError := EllipseError + Xchange;
      Xchange      := Xchange + TwoBSquare;
    end if;
  end loop;

  -- /*2nd set of points*/
  x := 0;
  y := SDL_VCoordinate(Yradius)-1;  -- /*radius zero == draw nothing*/
  Xchange := Sint32( Yradius ) * Sint32( Yradius );
  Ychange := Sint32( Xradius ) * Sint32( Xradius ) * (1-2*Sint32(Yradius));
  EllipseError := 0;
  StoppingX := 0;
  StoppingY := TwoASquare * Sint32( Yradius );

  -- /*Plot four ellipse points by iteration*/
  while StoppingX < StoppingY loop

    SDL_EXT_Plot( screen, x0+x, y0+y, pixel_colour, mode );
    SDL_EXT_Plot( screen, x0-x, y0+y, pixel_colour, mode );
    SDL_EXT_Plot( screen, x0+x, y0-y, pixel_colour, mode );
    SDL_EXT_Plot( screen, x0-x, y0-y, pixel_colour, mode );

    x := x + 1;
    StoppingX    := StoppingX + TwoBSquare;
    EllipseError := EllipseError + Xchange;
    Xchange      := Xchange + TwoBSquare;
    if 2 * EllipseError + Ychange > 0 then
      y := y - 1;
      StoppingY    := StoppingY - TwoASquare;
      EllipseError := EllipseError + Ychange;
      Ychange      := Ychange + TwoASquare;
    end if;
  end loop;

end SDL_EXT_frame_ellipse;


-----------------------------------------------------------------------------
-- SDL EXT(ension) Frame Ellipse Pattern
--
-- Draw an outline of a ellipse between 2 corner points.
-----------------------------------------------------------------------------

procedure SDL_EXT_Frame_Ellipse_Pattern( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                start_offset_x : SDL_HCoordinate; start_offset_y : SDL_VCoordinate;
                pattern : system.address; mode : aPenMode ) is

  Xradius : Uint16 := Uint16( end_x - start_x ) / 2;
  Yradius : Uint16 := Uint16( end_y - start_y ) / 2;
  x0 : SDL_HCoordinate := start_x + SDL_HCoordinate( Xradius );
  y0 : SDL_VCoordinate := start_y + SDL_VCoordinate( Yradius );
  x  : SDL_HCoordinate;
  y  : SDL_VCoordinate;
  Xchange : Sint32;
  Ychange : Sint32;
  EllipseError : Sint32;
  TwoASquare : Sint32;
  TwoBSquare : Sint32;
  StoppingX  : Sint32;
  StoppingY  : Sint32;

  procedure patternPlot( screen : in system.address;
            pixel_x : SDL_HCoordinate; pixel_y : SDL_VCoordinate;
            pattern : system.address; mode : aPenMode ) is
    pattern_x : SDL_HCoordinate;
    pattern_y : SDL_VCoordinate;
    pattern_ptr : SDL_Surface_Ptr := SDL_Surface_Conv.To_Pointer( pattern );
    -- This is inefficient but it works.
  begin
    if pattern_ptr /= null then
       pattern_x := (start_offset_x + pixel_x) mod SDL_HCoordinate( pattern_ptr.h );
       pattern_y := (start_offset_y + pixel_y) mod SDL_VCoordinate( pattern_ptr.w );
       SDL_EXT_Plot( screen, pixel_x, pixel_y,
                  SDL_EXT_Raw_Pixel( pattern, screen, pattern_x, pattern_y ), mode );
    end if;
  end patternPlot;

begin
put_line( "Frame ellipse pattern" );

  TwoASquare := 2 * Sint32( Xradius ) * Sint32( Xradius );
  TwoBSquare := 2 * Sint32( Yradius ) * Sint32( Yradius );

  -- 1st set of points
  x := SDL_HCoordinate( Xradius ) - 1;  -- /*radius zero == draw nothing*/
  y := 0;

  Xchange := Sint32( Yradius ) * Sint32( Yradius ) * (1-2*Sint32(Xradius));
  Ychange := Sint32( Xradius ) * Sint32( Xradius );

  EllipseError := 0;

  StoppingX := TwoBSquare*Sint32(Xradius);
  StoppingY := 0;
  -- /*Plot four ellipse points by iteration*/

-- put_line( "start_x = " & start_x'img );
-- put_line( "end_x = " & end_x'img );
-- put_line( "start_y = " & start_y'img );
-- put_line( "end_y = " & end_y'img );
-- put_line( "Xradius = " & Xradius'img );
-- put_line( "Yradius = " & Yradius'img );
-- put_line( "StoppingX = " & StoppingX'img );
-- put_line( "StoppingY = " & StoppingY'img );
-- put_line( "x0 = " & x0'img );
-- put_line( "y0 = " & y0'img );
-- put_line( "x  = " & x'img );
-- put_line( "y  = " & y'img );

  while StoppingX > StoppingY loop

    SDL_EXT_Plot_Pattern( screen, x0+x, y0+y, start_offset_x, start_offset_y, pattern, mode );
    SDL_EXT_Plot_Pattern( screen, x0-x, y0+y, start_offset_x, start_offset_y, pattern, mode );
    SDL_EXT_Plot_Pattern( screen, x0+x, y0-y, start_offset_x, start_offset_y, pattern, mode );
    SDL_EXT_Plot_Pattern( screen, x0-x, y0-y, start_offset_x, start_offset_y, pattern, mode );

    y := y + 1;
    StoppingY    := StoppingY + TwoASquare;
    EllipseError := EllipseError + Ychange;
    Ychange      := Ychange + TwoASquare;
    if 2 * EllipseError + Xchange > 0 then
      x := x - 1;
      StoppingX    := StoppingX - TwoBSquare;
      EllipseError := EllipseError + Xchange;
      Xchange      := Xchange + TwoBSquare;
    end if;
  end loop;

  -- /*2nd set of points*/
  x := 0;
  y := SDL_VCoordinate(Yradius)-1;  -- /*radius zero == draw nothing*/
  Xchange := Sint32( Yradius ) * Sint32( Yradius );
  Ychange := Sint32( Xradius ) * Sint32( Xradius ) * (1-2*Sint32(Yradius));
  EllipseError := 0;
  StoppingX := 0;
  StoppingY := TwoASquare * Sint32( Yradius );

  -- /*Plot four ellipse points by iteration*/
  while StoppingX < StoppingY loop

    SDL_EXT_Plot_Pattern( screen, x0+x, y0+y, start_offset_x, start_offset_y, pattern, mode );
    SDL_EXT_Plot_Pattern( screen, x0-x, y0+y, start_offset_x, start_offset_y, pattern, mode );
    SDL_EXT_Plot_Pattern( screen, x0+x, y0-y, start_offset_x, start_offset_y, pattern, mode );
    SDL_EXT_Plot_Pattern( screen, x0-x, y0-y, start_offset_x, start_offset_y, pattern, mode );

    x := x + 1;
    StoppingX    := StoppingX + TwoBSquare;
    EllipseError := EllipseError + Xchange;
    Xchange      := Xchange + TwoBSquare;
    if 2 * EllipseError + Ychange > 0 then
      y := y - 1;
      StoppingY    := StoppingY - TwoASquare;
      EllipseError := EllipseError + Ychange;
      Ychange      := Ychange + TwoASquare;
    end if;
  end loop;

end SDL_EXT_Frame_Ellipse_Pattern;


-----------------------------------------------------------------------------
-- SDL EXT(ension) Fill Ellipse
--
-- Draw a filled in ellipse between 2 corner points.
-----------------------------------------------------------------------------

procedure SDL_EXT_fill_ellipse( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                pixel_colour : SDL_Generic_Pixel; mode : aPenMode ) is

  Xradius : Uint16 := Uint16( end_x - start_x + 2 ) / 2;
  Yradius : Uint16 := Uint16( end_y - start_y + 2 ) / 2;
  x0 : SDL_HCoordinate := start_x + SDL_HCoordinate( Xradius ) - 1;
  y0 : SDL_VCoordinate := start_y + SDL_VCoordinate( Yradius ) - 1;
  x  : SDL_HCoordinate;
  y  : SDL_VCoordinate;
  Xchange : Sint32;
  Ychange : Sint32;
  EllipseError : Sint32;
  TwoASquare : Sint32;
  TwoBSquare : Sint32;
  StoppingX  : Sint32;
  StoppingY  : Sint32;

  new_top_y    : SDL_VCoordinate;
  new_bottom_y : SDL_VCoordinate;
  old_top_y    : SDL_VCoordinate;
  old_bottom_y : SDL_VCoordinate;
begin

  TwoASquare := 2 * Sint32( Xradius ) * Sint32( Xradius );
  TwoBSquare := 2 * Sint32( Yradius ) * Sint32( Yradius );

  -- 1st set of points
  x := SDL_HCoordinate( Xradius );
  y := 0;

  if x = 0 then -- /*radius zero == draw nothing*/
     Xchange := 0;
     Ychange := 0;
  else
     Xchange := Sint32( Yradius ) * Sint32( Yradius ) * (1-2*Sint32(Xradius));
     Ychange := Sint32( Xradius ) * Sint32( Xradius );
  end if;

  EllipseError := 0;

  StoppingX := TwoBSquare*Sint32(Xradius);
  StoppingY := 0;

  -- This loop draws the middle (vertical) 50% of the ellipse
  -- /*Plot four ellipse points by iteration*/

  old_top_y := 0;
  old_bottom_y := 0;

  while StoppingX > StoppingY loop

    new_top_y := y0 - y;
    new_bottom_y := y0 + y;
    if old_top_y /= new_top_y then
       SDL_Ext_HLine( screen, x0-x, x0+x, new_top_y, pixel_colour, mode );
       if new_top_y /= new_bottom_y then
          SDL_Ext_HLine( screen, x0-x, x0+x, new_bottom_y, pixel_colour, mode );
       end if;
    end if;
    old_top_y := new_top_y;
    old_bottom_y := new_bottom_y;

    y := y + 1;
    StoppingY    := StoppingY + TwoASquare;
    EllipseError := EllipseError + Ychange;
    Ychange      := Ychange + TwoASquare;
    if 2 * EllipseError + Xchange > 0 then
      x := x - 1;
      StoppingX    := StoppingX - TwoBSquare;
      EllipseError := EllipseError + Xchange;
      Xchange      := Xchange + TwoBSquare;
    end if;
  end loop;

  if old_top_y /= new_top_y then
     SDL_Ext_HLine( screen, x0-x, x0+x, new_top_y, pixel_colour, mode );
     if new_top_y /= new_bottom_y then
        SDL_Ext_HLine( screen, x0-x, x0+x, new_bottom_y, pixel_colour, mode );
     end if;
  end if;

  -- This loop draws the outer (vertical) top/bottom 50% of the ellipse
  -- /*2nd set of points*/

  x := 0;
  y := SDL_VCoordinate(Yradius);
  if y = 0 then -- /*radius zero == draw nothing*/
     Xchange := 0;
     Ychange := 0;
  else
     Xchange := Sint32( Yradius ) * Sint32( Yradius );
     Ychange := Sint32( Xradius ) * Sint32( Xradius ) * (1-2*Sint32(Yradius));
  end if;
  EllipseError := 0;
  StoppingX := 0;
  StoppingY := TwoASquare * Sint32( Yradius );

  -- /*Plot four ellipse points by iteration*/

  old_top_y := 0;
  old_bottom_y := 0;

  while StoppingX < StoppingY loop

    -- SDL_Ext_HLine( screen, x0-x-1, x0+x+1, y0-y-1, pixel_colour, mode );
    -- SDL_Ext_HLine( screen, x0-x-1, x0+x+1, y0+y+1, pixel_colour, mode );

    -- Some modes (like invert) in a y-narrow ellipse will cause multiple
    -- invert lines to be drawn.  So check for duplicate y's and only do the
    -- last one.

    new_top_y := y0 - y;
    new_bottom_y := y0 + y;
    if old_top_y /= new_top_y then
       SDL_Ext_HLine( screen, x0-x, x0+x, new_top_y, pixel_colour, mode );
       if new_top_y /= new_bottom_y then
          SDL_Ext_HLine( screen, x0-x, x0+x, new_bottom_y, pixel_colour, mode );
       end if;
    end if;
    old_top_y := new_top_y;
    old_bottom_y := new_bottom_y;

    x := x + 1;
    StoppingX    := StoppingX + TwoBSquare;
    EllipseError := EllipseError + Xchange;
    Xchange      := Xchange + TwoBSquare;
    if 2 * EllipseError + Ychange > 0 then
      y := y - 1;
      StoppingY    := StoppingY - TwoASquare;
      EllipseError := EllipseError + Ychange;
      Ychange      := Ychange + TwoASquare;
    end if;
  end loop;

  if old_top_y /= new_top_y then
     SDL_Ext_HLine( screen, x0-x, x0+x, new_top_y, pixel_colour, mode );
     if new_top_y /= new_bottom_y then
        SDL_Ext_HLine( screen, x0-x, x0+x, new_bottom_y, pixel_colour, mode );
     end if;
  end if;

end SDL_EXT_fill_ellipse;

-----------------------------------------------------------------------------
-- SDL EXT(ension) Fill Ellipse Pattern
--
-- Draw a filled in ellipse with a pattern between 2 corner points.
-----------------------------------------------------------------------------

procedure SDL_EXT_Fill_Ellipse_Pattern( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                start_offset_x : SDL_HCoordinate; start_offset_y : SDL_VCoordinate;
                pattern : system.address; mode : aPenMode ) is

  Xradius : Uint16 := Uint16( end_x - start_x + 2 ) / 2;
  Yradius : Uint16 := Uint16( end_y - start_y + 2 ) / 2;
  x0 : SDL_HCoordinate := start_x + SDL_HCoordinate( Xradius ) - 1;
  y0 : SDL_VCoordinate := start_y + SDL_VCoordinate( Yradius ) - 1;
  x  : SDL_HCoordinate;
  y  : SDL_VCoordinate;
  Xchange : Sint32;
  Ychange : Sint32;
  EllipseError : Sint32;
  TwoASquare : Sint32;
  TwoBSquare : Sint32;
  StoppingX  : Sint32;
  StoppingY  : Sint32;

  new_top_y    : SDL_VCoordinate;
  new_bottom_y : SDL_VCoordinate;
  old_top_y    : SDL_VCoordinate;
  old_bottom_y : SDL_VCoordinate;

begin

  TwoASquare := 2 * Sint32( Xradius ) * Sint32( Xradius );
  TwoBSquare := 2 * Sint32( Yradius ) * Sint32( Yradius );

  -- 1st set of points
  x := SDL_HCoordinate( Xradius );
  y := 0;

  if x = 0 then -- /*radius zero == draw nothing*/
     Xchange := 0;
     Ychange := 0;
  else
     Xchange := Sint32( Yradius ) * Sint32( Yradius ) * (1-2*Sint32(Xradius));
     Ychange := Sint32( Xradius ) * Sint32( Xradius );
  end if;

  EllipseError := 0;

  StoppingX := TwoBSquare*Sint32(Xradius);
  StoppingY := 0;
  -- /*Plot four ellipse points by iteration*/

  old_top_y := 0;
  old_bottom_y := 0;

  while StoppingX > StoppingY loop

    new_top_y := y0 - y;
    new_bottom_y := y0 + y;
    if old_top_y /= new_top_y then
       SDL_Ext_HLine_Pattern( screen, x0-x, x0+x, new_top_y, pattern, start_offset_x+x0-x, start_offset_y+y, mode );
       if new_top_y /= new_bottom_y then
          SDL_Ext_HLine_Pattern( screen, x0-x, x0+x, new_bottom_y, pattern, start_offset_x+x0-x, start_offset_y+y, mode );
       end if;
    end if;

    y := y + 1;
    StoppingY    := StoppingY + TwoASquare;
    EllipseError := EllipseError + Ychange;
    Ychange      := Ychange + TwoASquare;
    if 2 * EllipseError + Xchange > 0 then
      x := x - 1;
      StoppingX    := StoppingX - TwoBSquare;
      EllipseError := EllipseError + Xchange;
      Xchange      := Xchange + TwoBSquare;
    end if;
  end loop;

  if old_top_y /= new_top_y then
     SDL_Ext_HLine_Pattern( screen, x0-x, x0+x, new_top_y, pattern, start_offset_x+x0-x, start_offset_y+y, mode );
     if new_top_y /= new_bottom_y then
        SDL_Ext_HLine_Pattern( screen, x0-x, x0+x, new_bottom_y, pattern, start_offset_x+x0-x, start_offset_y+y, mode );
     end if;
  end if;

  -- /*2nd set of points*/

  old_top_y := 0;
  old_bottom_y := 0;

  x := 0;
  y := SDL_VCoordinate(Yradius);  -- /*radius zero == draw nothing*/
  if y = 0 then -- /*radius zero == draw nothing*/
     Xchange := 0;
     Ychange := 0;
  else
     Xchange := Sint32( Yradius ) * Sint32( Yradius );
     Ychange := Sint32( Xradius ) * Sint32( Xradius ) * (1-2*Sint32(Yradius));
  end if;

  EllipseError := 0;
  StoppingX := 0;
  StoppingY := TwoASquare * Sint32( Yradius );

  -- /*Plot four ellipse points by iteration*/
  while StoppingX < StoppingY loop

    new_top_y := y0 - y;
    new_bottom_y := y0 + y;
    if old_top_y /= new_top_y then
       SDL_Ext_HLine_Pattern( screen, x0-x, x0+x, new_top_y, pattern, 0+x0-x, 0+y, mode );
       if new_top_y /= new_bottom_y then
          SDL_Ext_HLine_Pattern( screen, x0-x, x0+x, new_bottom_y, pattern, 0+x0-x, 0+y, mode );
       end if;
    end if;

    x := x + 1;
    StoppingX    := StoppingX + TwoBSquare;
    EllipseError := EllipseError + Xchange;
    Xchange      := Xchange + TwoBSquare;
    if 2 * EllipseError + Ychange > 0 then
      y := y - 1;
      StoppingY    := StoppingY - TwoASquare;
      EllipseError := EllipseError + Ychange;
      Ychange      := Ychange + TwoASquare;
    end if;
  end loop;

  if old_top_y /= new_top_y then
     SDL_Ext_HLine_Pattern( screen, x0-x, x0+x, new_top_y, pattern, 0+x0-x, 0+y, mode );
     if new_top_y /= new_bottom_y then
        SDL_Ext_HLine_Pattern( screen, x0-x, x0+x, new_bottom_y, pattern, 0+x0-x, 0+y, mode );
     end if;
  end if;

end SDL_EXT_Fill_Ellipse_Pattern;

end spar_os.sdl;

