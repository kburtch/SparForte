------------------------------------------------------------------------------
-- PEN                                                                      --
-- The Pen Graphics Package.                                                --
--                                                                          --
-- Part of SparForte                                                        --
-- Designed and Programmed by Ken O. Burtch                                 --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2019 Free Software Foundation              --
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
-- This is loosely based on my Texttools' userio package.

with system,
     ada.strings.unbounded,
     spar_os.sdl;
with gen_list;

use ada.strings.unbounded,
    spar_os.sdl;

package pen is


---> Coordinates
--
-- Coordinates are floating point values.  (0.0, 0.0) is the top-left corner
-- of the screen; (100.0, 100.0) is the bottom-right corner of the screen.
-- Values below 0% and greater than 100% represent areas off the screen.
--
-- Coordinates and Rectangles are defined in the Common package.
------------------------------------------------------------------------------

type ACoordinate is new float;
subtype ACoordinateOffset is ACoordinate;


---> Definitions of Basic Colours
--
-- A shade of colour is represented as a percentage of red, green and blue
-- values above 100% or below 0% are not allowed.
------------------------------------------------------------------------------

type ARGBComponent is new float range 0.0 .. 100.0;


---> Basic Rectangles
--
-- Rectangles are used all over.
--
-- A coordinate could be constraint to avoid errors with multiplying
-- to get the area of a rectangle--this would improve stability.
------------------------------------------------------------------------------

type ARect is record
   left, top, right, bottom : ACoordinate;
end record;

nullRect : constant ARect := (0.0, 0.0, -1.0, -1.0);
allRect  : constant ARect := (0.0, 0.0, 100.0, 100.0);

---> SET RECT
--
-- Set the coordinates of the rectangle, moving the rectangle to that
-- position.  Or initializes the rect, whichever you prefer.
-----------------------------------------------------------------------------

procedure SetRect( r : out ARect; left, top, right, bottom : ACoordinate );

---> IS EMPTY RECT
--
-- True if the rectangle is infinitely small.  That is, a null rect.
-----------------------------------------------------------------------------

function IsEmptyRect( r : ARect ) return boolean;

---> OFFSET RECT
--
-- Shift the rectangle position horizontal by dx, vertical by dy, maintaining
-- the rectangle's shape.
-----------------------------------------------------------------------------

procedure OffsetRect( r : in out ARect; dx, dy : ACoordinateOffset );
function OffsetRect( r : in ARect; dx, dy : ACoordinateOffset ) return ARect;

---> INSET RECT
--
-- Change the size of the rectangle horizontal by dx, vertical by dy.  The
-- the center of the rectangle does not move.
-----------------------------------------------------------------------------

procedure InsetRect( r : in out ARect; dx, dy : ACoordinateOffset );
function InsetRect( r : in ARect; dx, dy : ACoordinateOffset ) return ARect;

---> INTERSECT RECT
--
-- Return the rectangle formed by the intersection of two rectangles.
-----------------------------------------------------------------------------

procedure IntersectRect( r : in out ARect; r1, r2 : ARect );
function IntersectRect( r1, r2 : in ARect ) return ARect;

---> INSIDE RECT
--
-- True if one rectangle is inside of another.
-----------------------------------------------------------------------------

function InsideRect( Inner, Outer : in ARect ) return boolean;

---> IN RECT
--
-- True if the coordinates are inside of the rectangle.
-----------------------------------------------------------------------------

function InRect( x, y : ACoordinate ; r : ARect ) return boolean;


---> The Pen
--
-- The pen is the drawing environment on a canvas.  This includes the current
-- drawing position, colour, brush and so forth.
------------------------------------------------------------------------------

type aPenBrush is ( undefined, pencil, stretch, tile, stamp, smear );

type aPenState is record
    X            : ACoordinate;                    -- location of the pen
    Y            : ACoordinate;
    Angle        : float;                          -- pen orientation
    ErrorX       : ACoordinate;                    -- where next DrawErr goes
    ErrorY       : ACoordinate;
    Mode         : aPenMode;                       -- source/destination mixing
    Brush        : aPenBrush;                      -- how the ink is applied
    Ink_R        : aRGBComponent;                  -- user's RGB for pixel
    Ink_G        : aRGBComponent;
    Ink_B        : aRGBComponent;
    Pixel        : SDL_Generic_Pixel;              -- Pixel for a pencil brush
    pattern_ptr  : SDL_Surface_Ptr;                -- pattern for other brushes
    pattern      : system.address;                 -- same as a C pointer
    revealCount  : integer := 0;                   -- drawing spooling
end record;

pen          : aPenState;


---> The Drawing Environment
--
------------------------------------------------------------------------------

type ADisplayInfoRec is record
     TextBased : boolean;            -- true if screen can't do graphics
     H_Res     : positive;           -- horizontal size of screen (in pixels)
     V_Res     : positive;           -- vertical size of screen (in pixels)
     C_Res     : positive;           -- size of pixel (in bits, 0=N/A)
     P_Len     : natural;            -- size of the palette (in entries, 0=N/A)
     D_Buf     : natural;            -- number of display buffers
     -- S_Res     : natural;  -- sound resolution (0=no sound)
     -- Y_Res     : natural;  -- sound voices/channels (0=no sound)
end record;

type aCanvasType is ( unknown, screen, window, offscreen, closed );
type aDrawingType is ( unknown, raster, opengl );

subtype aCanvasID is natural;

type aCanvas is record
    id          : aCanvasID := 0;
    pen         : aPenState;
    displayInfo : aDisplayInfoRec;                     -- the canvas properties
    bounds      : ARect := (0.0, 0.0, 100.0, 100.0);   -- canvas bounds
    clipRect    : ARect := (0.0, 0.0, 100.0, 100.0);   -- clipping bounds
    surface_ptr : SDL_Surface_Ptr;                     -- SDL Surface
    surface     : system.address;                      -- same as a C pointer
    hardware_ptr: SDL_VideoInfo_Ptr;                   -- SDL video info
    hardware    : system.address;                      -- same as a C pointer
    name        : unbounded_string;                    -- name of canvas
    kind        : aCanvasType := unknown;
    drawingType : aDrawingType := unknown;
    hasFont     : boolean := false;                    -- true if font loaded
    font_ptr     : TTF_font;                           -- TTF font
end record;

function ">="( left, right : aCanvas ) return boolean;
function "="( left, right : aCanvas ) return boolean;
package canvasList is new gen_list( aCanvas, "=", ">=" );

canvas : canvasList.List;                              -- list of canvases
canvasIdTop : natural := 1;                            -- id counter

screen_canvas : canvasList.aListIndex;


---> Predefined Colours
--
-- The predefined colours are stored in the ColourNames array.
------------------------------------------------------------------------------

type AColourDefinition is record
  name : string(1..19);             -- name of the colour
  red, green, blue : ARGBComponent; -- red, gren, blue percentages
end record;
pragma Pack( AColourDefinition );

type AColourName is (
  AliceBlue, AntiqueWhite, AntiqueWhite1, AntiqueWhite2, AntiqueWhite3,
  AntiqueWhite4, Aquamarine, Aquamarine1, Aquamarine2, Aquamarine3,
  Aquamarine4, Azure, Azure1, Azure2, Azure3, Azure4, Beige, Bisque,
  Bisque1, Bisque2, Bisque3, Bisque4, Black, BlanchedAlmond, Blue,
  Blue1, Blue2, Blue3, Blue4, BlueViolet, Brown, Brown1, Brown2, Brown3,
  Brown4, Burlywood, Burlywood1, Burlywood2, Burlywood3, Burlywood4,
  CadetBlue, CadetBlue1, CadetBlue2, CadetBlue3, CadetBlue4, Chartreuse,
  Chartreuse1, Chartreuse2, Chartreuse3, Chartreuse4, Chocolate,
  Chocolate1, Chocolate2, Chocolate3, Chocolate4, Coral, Coral1, Coral2,
  Coral3, Coral4, CornflowerBlue, Cornsilk, Cornsilk1, Cornsilk2,
  Cornsilk3, Cornsilk4, Cyan, Cyan1, Cyan2, Cyan3, Cyan4, DarkBlue,
  DarkCyan, DarkGoldenrod, DarkGoldenrod1, DarkGoldenrod2, DarkGoldenrod3,
  DarkGoldenrod4, DarkGreen, DarkGrey, DarkKhaki, DarkMagenta,
  DarkOliveGreen, DarkOliveGreen1, DarkOliveGreen2, DarkOliveGreen3,
  DarkOliveGreen4, DarkOrange, DarkOrange1, DarkOrange2, DarkOrange3,
  DarkOrange4, DarkOrchid, DarkOrchid1, DarkOrchid2, DarkOrchid3,
  DarkOrchid4, DarkRed, DarkSalmon, DarkSeaGreen, DarkSeaGreen1,
  DarkSeaGreen2, DarkSeaGreen3, DarkSeaGreen4, DarkSlateBlue,
  DarkSlateGrey, DarkSlateGrey1, DarkSlateGrey2, DarkSlateGrey3,
  DarkSlateGrey4, DarkTurquoise, DarkViolet, DeepPink, DeepPink1,
  DeepPink2, DeepPink3, DeepPink4, DeepSkyBlue, DeepSkyBlue1, DeepSkyBlue2,
  DeepSkyBlue3, DeepSkyBlue4, DimGrey, DodgerBlue, DodgerBlue1,
  DodgerBlue2, DodgerBlue3, DodgerBlue4 , Firebrick, Firebrick1,
  Firebrick2, Firebrick3, Firebrick4, FloralWhite, ForestGreen, Gainsboro,
  GhostWhite, Gold, Gold1, Gold2, Gold3, Gold4, Goldenrod, Goldenrod1,
  Goldenrod2, Goldenrod3, Goldenrod4, Green, Green1, Green2, Green3,
  Green4, GreenYellow, Grey, Honeydew, Honeydew1, Honeydew2, Honeydew3,
  Honeydew4, HotPink, HotPink1, HotPink2, HotPink3, HotPink4, IndianRed,
  IndianRed1, IndianRed2, IndianRed3, IndianRed4, Ivory, Ivory1, Ivory2,
  Ivory3, Ivory4, Khaki, Khaki1, Khaki2, Khaki3, Khaki4, Lavender,
  LavenderBlush, LavenderBlush1, LavenderBlush2, LavenderBlush3,
  LavenderBlush4, LawnGreen, LemonChiffon, LemonChiffon1, LemonChiffon2,
  LemonChiffon3, LemonChiffon4, LightBlue, LightBlue1, LightBlue2,
  LightBlue3, LightBlue4, LightCoral, LightCyan, LightCyan1, LightCyan2,
  LightCyan3, LightCyan4, LightGoldenrod, LightGoldenrod1,
  LightGoldenrod2, LightGoldenrod3, LightGoldenrod4,
  LightGoldenrodYellow, LightGreen, LightGrey, LightPink, LightPink1,
  LightPink2, LightPink3, LightPink4, LightSalmon, LightSalmon1,
  LightSalmon2, LightSalmon3, LightSalmon4, LightSeaGreen, LightSkyBlue,
  LightSkyBlue1, LightSkyBlue2, LightSkyBlue3, LightSkyBlue4,
  LightSlateBlue, LightSlateGrey, LightSteelBlue, LightSteelBlue1,
  LightSteelBlue2, LightSteelBlue3, LightSteelBlue4, LightYellow,
  LightYellow1, LightYellow2, LightYellow3, LightYellow4, LimeGreen,
  Linen, Magenta, Magenta1, Magenta2, Magenta3, Magenta4, Maroon,
  Maroon1, Maroon2, Maroon3, Maroon4, MediumAquamarine, MediumBlue,
  MediumOrchid, MediumOrchid1, MediumOrchid2, MediumOrchid3,
  MediumOrchid4, MediumPurple, MediumPurple1, MediumPurple2,
  MediumPurple3, MediumPurple4, MediumSeaGreen, MediumSlateBlue,
  MediumSpringGreen, MediumTurquoise, MediumVioletRed, MidnightBlue,
  MintCream, MistyRose, MistyRose1, MistyRose2, MistyRose3, MistyRose4,
  Moccasin, NavajoWhite, NavajoWhite1, NavajoWhite2, NavajoWhite3,
  NavajoWhite4, NavyBlue, OldLace, OliveDrab, OliveDrab1, OliveDrab2,
  OliveDrab3, OliveDrab4, Orange, Orange1, Orange2, Orange3, Orange4,
  OrangeRed, OrangeRed1, OrangeRed2, OrangeRed3, OrangeRed4, Orchid,
  Orchid1, Orchid2, Orchid3, Orchid4, PaleGoldenrod, PaleGreen,
  PaleGreen1, PaleGreen2, PaleGreen3, PaleGreen4, PaleTurquoise,
  PaleTurquoise1, PaleTurquoise2, PaleTurquoise3, PaleTurquoise4,
  PaleVioletRed, PaleVioletRed1, PaleVioletRed2, PaleVioletRed3,
  PaleVioletRed4, PapayaWhip, PeachPuff, PeachPuff1, PeachPuff2,
  PeachPuff3, PeachPuff4, Peru, Pink, Pink1, Pink2, Pink3, Pink4,
  Plum, Plum1, Plum2, Plum3, Plum4, PowderBlue, Purple, Purple1,
  Purple2, Purple3, Purple4, Red, Red1, Red2, Red3, Red4, RosyBrown,
  RosyBrown1, RosyBrown2, RosyBrown3, RosyBrown4, RoyalBlue,
  RoyalBlue1, RoyalBlue2, RoyalBlue3, RoyalBlue4, SaddleBrown,
  Salmon, Salmon1, Salmon2, Salmon3, Salmon4, SandyBrown, SeaGreen,
  SeaGreen1, SeaGreen2, SeaGreen3, SeaGreen4, Seashell, Seashell1,
  Seashell2, Seashell3, Seashell4, Sienna, Sienna1, Sienna2, Sienna3,
  Sienna4, SkyBlue, SkyBlue1, SkyBlue2, SkyBlue3, SkyBlue4, SlateBlue,
  SlateBlue1, SlateBlue2, SlateBlue3, SlateBlue4, SlateGray1,
  SlateGray2, SlateGray3, SlateGray4, SlateGrey, Snow, Snow1, Snow2,
  Snow3, Snow4, SpringGreen, SpringGreen1, SpringGreen2, SpringGreen3,
  SpringGreen4, SteelBlue, SteelBlue1, SteelBlue2, SteelBlue3,
  SteelBlue4, Tan, Tan1, Tan2, Tan3, Tan4, Thistle, Thistle1, Thistle2,
  Thistle3, Thistle4, Tomato, Tomato1, Tomato2, Tomato3, Tomato4,
  Turquoise, Turquoise1, Turquoise2, Turquoise3, Turquoise4, Violet,
  VioletRed, VioletRed1, VioletRed2, VioletRed3, VioletRed4, Wheat,
  Wheat1, Wheat2, Wheat3, Wheat4, White, WhiteSmoke, Yellow, Yellow1,
  Yellow2, Yellow3, Yellow4, YellowGreen );

subtype AColorName is AColourName; -- for the Americans who can't spell

type AColourDefinitionList is array(AColourName) of AColourDefinition;

ColourNames : constant AColourDefinitionList := (
AliceBlue     => (name=>"Alice Blue         ", red=>94.12, green=>97.25, blue=>100.00),
AntiqueWhite  => (name=>"Antique White      ", red=>98.04, green=>92.16, blue=>84.31),
AntiqueWhite1 => (name=>"Antique White 1    ", red=>100.00, green=>93.73, blue=>85.88),
AntiqueWhite2 => (name=>"Antique White 2    ", red=>93.33, green=>87.45, blue=>80.00),
AntiqueWhite3 => (name=>"Antique White 3    ", red=>80.39, green=>75.29, blue=>69.02),
AntiqueWhite4 => (name=>"Antique White 4    ", red=>54.51, green=>51.37, blue=>47.06),
Aquamarine    => (name=>"aquamarine         ", red=>49.80, green=>100.00, blue=>83.14),
Aquamarine1   => (name=>"aquamarine 1       ", red=>49.80, green=>100.00, blue=>83.14),
Aquamarine2   => (name=>"aquamarine 2       ", red=>46.27, green=>93.33, blue=>77.65),
Aquamarine3   => (name=>"aquamarine 3       ", red=>40.00, green=>80.39, blue=>66.67),
Aquamarine4   => (name=>"aquamarine 4       ", red=>27.06, green=>54.51, blue=>45.49),
Azure         => (name=>"azure              ", red=>94.12, green=>100.00, blue=>100.00),
Azure1        => (name=>"azure 1            ", red=>94.12, green=>100.00, blue=>100.00),
Azure2        => (name=>"azure 2            ", red=>87.84, green=>93.33, blue=>93.33),
Azure3        => (name=>"azure 3            ", red=>75.69, green=>80.39, blue=>80.39),
Azure4        => (name=>"azure 4            ", red=>51.37, green=>54.51, blue=>54.51),
Beige         => (name=>"beige              ", red=>96.08, green=>96.08, blue=>86.27),
Bisque        => (name=>"bisque             ", red=>100.00, green=>89.41, blue=>76.86),
Bisque1       => (name=>"bisque 1           ", red=>100.00, green=>89.41, blue=>76.86),
Bisque2       => (name=>"bisque 2           ", red=>93.33, green=>83.53, blue=>71.76),
Bisque3       => (name=>"bisque 3           ", red=>80.39, green=>71.76, blue=>61.96),
Bisque4       => (name=>"bisque 4           ", red=>54.51, green=>49.02, blue=>41.96),
Black         => (name=>"black              ", red=>0.00, green=>0.00, blue=>0.00),
BlanchedAlmond => (name=>"Blanched Almond    ", red=>100.00, green=>92.16, blue=>80.39),
Blue          => (name=>"blue               ", red=>0.00, green=>0.00, blue=>100.00),
Blue1         => (name=>"blue 1             ", red=>0.00, green=>0.00, blue=>100.00),
Blue2         => (name=>"blue 2             ", red=>0.00, green=>0.00, blue=>93.33),
Blue3         => (name=>"blue 3             ", red=>0.00, green=>0.00, blue=>80.39),
Blue4         => (name=>"blue 4             ", red=>0.00, green=>0.00, blue=>54.51),
BlueViolet    => (name=>"Blue Violet        ", red=>54.12, green=>16.86, blue=>88.63),
Brown         => (name=>"brown              ", red=>64.71, green=>16.47, blue=>16.47),
Brown1        => (name=>"brown 1            ", red=>100.00, green=>25.10, blue=>25.10),
Brown2        => (name=>"brown 2            ", red=>93.33, green=>23.14, blue=>23.14),
Brown3        => (name=>"brown 3            ", red=>80.39, green=>20.00, blue=>20.00),
Brown4        => (name=>"brown 4            ", red=>54.51, green=>13.73, blue=>13.73),
Burlywood     => (name=>"burlywood          ", red=>87.06, green=>72.16, blue=>52.94),
Burlywood1    => (name=>"burlywood 1        ", red=>100.00, green=>82.75, blue=>60.78),
Burlywood2    => (name=>"burlywood 2        ", red=>93.33, green=>77.25, blue=>56.86),
Burlywood3    => (name=>"burlywood 3        ", red=>80.39, green=>66.67, blue=>49.02),
Burlywood4    => (name=>"burlywood 4        ", red=>54.51, green=>45.10, blue=>33.33),
CadetBlue     => (name=>"Cadet Blue         ", red=>37.25, green=>61.96, blue=>62.75),
CadetBlue1    => (name=>"Cadet Blue 1       ", red=>59.61, green=>96.08, blue=>100.00),
CadetBlue2    => (name=>"Cadet Blue 2       ", red=>55.69, green=>89.80, blue=>93.33),
CadetBlue3    => (name=>"Cadet Blue 3       ", red=>47.84, green=>77.25, blue=>80.39),
CadetBlue4    => (name=>"Cadet Blue 4       ", red=>32.55, green=>52.55, blue=>54.51),
Chartreuse    => (name=>"chartreuse         ", red=>49.80, green=>100.00, blue=>0.00),
Chartreuse1   => (name=>"chartreuse 1       ", red=>49.80, green=>100.00, blue=>0.00),
Chartreuse2   => (name=>"chartreuse 2       ", red=>46.27, green=>93.33, blue=>0.00),
Chartreuse3   => (name=>"chartreuse 3       ", red=>40.00, green=>80.39, blue=>0.00),
Chartreuse4   => (name=>"chartreuse 4       ", red=>27.06, green=>54.51, blue=>0.00),
Chocolate     => (name=>"chocolate          ", red=>82.35, green=>41.18, blue=>11.76),
Chocolate1    => (name=>"chocolate 1        ", red=>100.00, green=>49.80, blue=>14.12),
Chocolate2    => (name=>"chocolate 2        ", red=>93.33, green=>46.27, blue=>12.94),
Chocolate3    => (name=>"chocolate 3        ", red=>80.39, green=>40.00, blue=>11.37),
Chocolate4    => (name=>"chocolate 4        ", red=>54.51, green=>27.06, blue=>7.45),
Coral         => (name=>"coral              ", red=>100.00, green=>49.80, blue=>31.37),
Coral1        => (name=>"coral 1            ", red=>100.00, green=>44.71, blue=>33.73),
Coral2        => (name=>"coral 2            ", red=>93.33, green=>41.57, blue=>31.37),
Coral3        => (name=>"coral 3            ", red=>80.39, green=>35.69, blue=>27.06),
Coral4        => (name=>"coral 4            ", red=>54.51, green=>24.31, blue=>18.43),
CornflowerBlue => (name=>"Cornflower Blue    ", red=>39.22, green=>58.43, blue=>92.94),
Cornsilk      => (name=>"cornsilk           ", red=>100.00, green=>97.25, blue=>86.27),
Cornsilk1     => (name=>"cornsilk 1         ", red=>100.00, green=>97.25, blue=>86.27),
Cornsilk2     => (name=>"cornsilk 2         ", red=>93.33, green=>90.98, blue=>80.39),
Cornsilk3     => (name=>"cornsilk 3         ", red=>80.39, green=>78.43, blue=>69.41),
Cornsilk4     => (name=>"cornsilk 4         ", red=>54.51, green=>53.33, blue=>47.06),
Cyan          => (name=>"cyan               ", red=>0.00, green=>100.00, blue=>100.00),
Cyan1         => (name=>"cyan 1             ", red=>0.00, green=>100.00, blue=>100.00),
Cyan2         => (name=>"cyan 2             ", red=>0.00, green=>93.33, blue=>93.33),
Cyan3         => (name=>"cyan 3             ", red=>0.00, green=>80.39, blue=>80.39),
Cyan4         => (name=>"cyan 4             ", red=>0.00, green=>54.51, blue=>54.51),
DarkBlue      => (name=>"Dark Blue          ", red=>0.00, green=>0.00, blue=>54.51),
DarkCyan      => (name=>"Dark Cyan          ", red=>0.00, green=>54.51, blue=>54.51),
DarkGoldenrod => (name=>"Dark Goldenrod     ", red=>72.16, green=>52.55, blue=>4.31),
DarkGoldenrod1 => (name=>"Dark Goldenrod 1   ", red=>100.00, green=>72.55, blue=>5.88),
DarkGoldenrod2 => (name=>"Dark Goldenrod 2   ", red=>93.33, green=>67.84, blue=>5.49),
DarkGoldenrod3 => (name=>"Dark Goldenrod 3   ", red=>80.39, green=>58.43, blue=>4.71),
DarkGoldenrod4 => (name=>"Dark Goldenrod 4   ", red=>54.51, green=>39.61, blue=>3.14),
DarkGreen     => (name=>"Dark Green         ", red=>0.00, green=>39.22, blue=>0.00),
DarkGrey      => (name=>"Dark Grey          ", red=>66.27, green=>66.27, blue=>66.27),
DarkKhaki     => (name=>"Dark Khaki         ", red=>74.12, green=>71.76, blue=>41.96),
DarkMagenta   => (name=>"Dark Magenta       ", red=>54.51, green=>0.00, blue=>54.51),
DarkOliveGreen => (name=>"Dark Olive Green   ", red=>33.33, green=>41.96, blue=>18.43),
DarkOliveGreen1 => (name=>"Dark Olive Green 1 ", red=>79.22, green=>100.00, blue=>43.92),
DarkOliveGreen2 => (name=>"Dark Olive Green 2 ", red=>73.73, green=>93.33, blue=>40.78),
DarkOliveGreen3 => (name=>"Dark Olive Green 3 ", red=>63.53, green=>80.39, blue=>35.29),
DarkOliveGreen4 => (name=>"Dark Olive Green 4 ", red=>43.14, green=>54.51, blue=>23.92),
DarkOrange    => (name=>"Dark Orange        ", red=>100.00, green=>54.90, blue=>0.00),
DarkOrange1   => (name=>"Dark Orange 1      ", red=>100.00, green=>49.80, blue=>0.00),
DarkOrange2   => (name=>"Dark Orange 2      ", red=>93.33, green=>46.27, blue=>0.00),
DarkOrange3   => (name=>"Dark Orange 3      ", red=>80.39, green=>40.00, blue=>0.00),
DarkOrange4   => (name=>"Dark Orange 4      ", red=>54.51, green=>27.06, blue=>0.00),
DarkOrchid    => (name=>"Dark Orchid        ", red=>60.00, green=>19.61, blue=>80.00),
DarkOrchid1   => (name=>"Dark Orchid 1      ", red=>74.90, green=>24.31, blue=>100.00),
DarkOrchid2   => (name=>"Dark Orchid 2      ", red=>69.80, green=>22.75, blue=>93.33),
DarkOrchid3   => (name=>"Dark Orchid 3      ", red=>60.39, green=>19.61, blue=>80.39),
DarkOrchid4   => (name=>"Dark Orchid 4      ", red=>40.78, green=>13.33, blue=>54.51),
DarkRed       => (name=>"Dark Red           ", red=>54.51, green=>0.00, blue=>0.00),
DarkSalmon    => (name=>"Dark Salmon        ", red=>91.37, green=>58.82, blue=>47.84),
DarkSeaGreen  => (name=>"Dark Sea Green     ", red=>56.08, green=>73.73, blue=>56.08),
DarkSeaGreen1 => (name=>"Dark Sea Green 1   ", red=>75.69, green=>100.00, blue=>75.69),
DarkSeaGreen2 => (name=>"Dark Sea Green 2   ", red=>70.59, green=>93.33, blue=>70.59),
DarkSeaGreen3 => (name=>"Dark Sea Green 3   ", red=>60.78, green=>80.39, blue=>60.78),
DarkSeaGreen4 => (name=>"Dark Sea Green 4   ", red=>41.18, green=>54.51, blue=>41.18),
DarkSlateBlue => (name=>"Dark Slate Blue    ", red=>28.24, green=>23.92, blue=>54.51),
DarkSlateGrey => (name=>"Dark Slate Grey    ", red=>18.43, green=>30.98, blue=>30.98),
DarkSlateGrey1 => (name=>"Dark Slate Grey 1  ", red=>59.22, green=>100.00, blue=>100.00),
DarkSlateGrey2 => (name=>"Dark Slate Grey 2  ", red=>55.29, green=>93.33, blue=>93.33),
DarkSlateGrey3 => (name=>"Dark Slate Grey 3  ", red=>47.45, green=>80.39, blue=>80.39),
DarkSlateGrey4 => (name=>"Dark Slate Grey 4  ", red=>32.16, green=>54.51, blue=>54.51),
DarkTurquoise => (name=>"Dark Turquoise     ", red=>0.00, green=>80.78, blue=>81.96),
DarkViolet    => (name=>"Dark Violet        ", red=>58.04, green=>0.00, blue=>82.75),
DeepPink      => (name=>"Deep Pink          ", red=>100.00, green=>7.84, blue=>57.65),
DeepPink1     => (name=>"Deep Pink 1        ", red=>100.00, green=>7.84, blue=>57.65),
DeepPink2     => (name=>"Deep Pink 2        ", red=>93.33, green=>7.06, blue=>53.73),
DeepPink3     => (name=>"Deep Pink 3        ", red=>80.39, green=>6.27, blue=>46.27),
DeepPink4     => (name=>"Deep Pink 4        ", red=>54.51, green=>3.92, blue=>31.37),
DeepSkyBlue   => (name=>"Deep Sky Blue      ", red=>0.00, green=>74.90, blue=>100.00),
DeepSkyBlue1  => (name=>"Deep Sky Blue 1    ", red=>0.00, green=>74.90, blue=>100.00),
DeepSkyBlue2  => (name=>"Deep Sky Blue 2    ", red=>0.00, green=>69.80, blue=>93.33),
DeepSkyBlue3  => (name=>"Deep Sky Blue 3    ", red=>0.00, green=>60.39, blue=>80.39),
DeepSkyBlue4  => (name=>"Deep Sky Blue 4    ", red=>0.00, green=>40.78, blue=>54.51),
DimGrey       => (name=>"Dim Grey           ", red=>41.18, green=>41.18, blue=>41.18),
DodgerBlue    => (name=>"Dodger Blue        ", red=>11.76, green=>56.47, blue=>100.00),
DodgerBlue1   => (name=>"Dodger Blue 1      ", red=>11.76, green=>56.47, blue=>100.00),
DodgerBlue2   => (name=>"Dodger Blue 2      ", red=>10.98, green=>52.55, blue=>93.33),
DodgerBlue3   => (name=>"Dodger Blue 3      ", red=>9.41, green=>45.49, blue=>80.39),
DodgerBlue4   => (name=>"Dodger Blue 4      ", red=>6.27, green=>30.59, blue=>54.51),
Firebrick     => (name=>"firebrick          ", red=>69.80, green=>13.33, blue=>13.33),
Firebrick1    => (name=>"firebrick 1        ", red=>100.00, green=>18.82, blue=>18.82),
Firebrick2    => (name=>"firebrick 2        ", red=>93.33, green=>17.25, blue=>17.25),
Firebrick3    => (name=>"firebrick 3        ", red=>80.39, green=>14.90, blue=>14.90),
Firebrick4    => (name=>"firebrick 4        ", red=>54.51, green=>10.20, blue=>10.20),
FloralWhite   => (name=>"Floral White       ", red=>100.00, green=>98.04, blue=>94.12),
ForestGreen   => (name=>"Forest Green       ", red=>13.33, green=>54.51, blue=>13.33),
Gainsboro     => (name=>"gainsboro          ", red=>86.27, green=>86.27, blue=>86.27),
GhostWhite    => (name=>"Ghost White        ", red=>97.25, green=>97.25, blue=>100.00),
Gold          => (name=>"gold               ", red=>100.00, green=>84.31, blue=>0.00),
Gold1         => (name=>"gold 1             ", red=>100.00, green=>84.31, blue=>0.00),
Gold2         => (name=>"gold 2             ", red=>93.33, green=>78.82, blue=>0.00),
Gold3         => (name=>"gold 3             ", red=>80.39, green=>67.84, blue=>0.00),
Gold4         => (name=>"gold 4             ", red=>54.51, green=>45.88, blue=>0.00),
Goldenrod     => (name=>"goldenrod          ", red=>85.49, green=>64.71, blue=>12.55),
Goldenrod1    => (name=>"goldenrod 1        ", red=>100.00, green=>75.69, blue=>14.51),
Goldenrod2    => (name=>"goldenrod 2        ", red=>93.33, green=>70.59, blue=>13.33),
Goldenrod3    => (name=>"goldenrod 3        ", red=>80.39, green=>60.78, blue=>11.37),
Goldenrod4    => (name=>"goldenrod 4        ", red=>54.51, green=>41.18, blue=>7.84),
Green         => (name=>"green              ", red=>0.00, green=>100.00, blue=>0.00),
Green1        => (name=>"green 1            ", red=>0.00, green=>100.00, blue=>0.00),
Green2        => (name=>"green 2            ", red=>0.00, green=>93.33, blue=>0.00),
Green3        => (name=>"green 3            ", red=>0.00, green=>80.39, blue=>0.00),
Green4        => (name=>"green 4            ", red=>0.00, green=>54.51, blue=>0.00),
GreenYellow   => (name=>"Green Yellow       ", red=>67.84, green=>100.00, blue=>18.43),
Grey          => (name=>"grey               ", red=>74.51, green=>74.51, blue=>74.51),
Honeydew      => (name=>"honeydew           ", red=>94.12, green=>100.00, blue=>94.12),
Honeydew1     => (name=>"honeydew 1         ", red=>94.12, green=>100.00, blue=>94.12),
Honeydew2     => (name=>"honeydew 2         ", red=>87.84, green=>93.33, blue=>87.84),
Honeydew3     => (name=>"honeydew 3         ", red=>75.69, green=>80.39, blue=>75.69),
Honeydew4     => (name=>"honeydew 4         ", red=>51.37, green=>54.51, blue=>51.37),
HotPink       => (name=>"Hot Pink           ", red=>100.00, green=>41.18, blue=>70.59),
HotPink1      => (name=>"Hot Pink 1         ", red=>100.00, green=>43.14, blue=>70.59),
HotPink2      => (name=>"Hot Pink 2         ", red=>93.33, green=>41.57, blue=>65.49),
HotPink3      => (name=>"Hot Pink 3         ", red=>80.39, green=>37.65, blue=>56.47),
HotPink4      => (name=>"Hot Pink 4         ", red=>54.51, green=>22.75, blue=>38.43),
IndianRed     => (name=>"Indian Red         ", red=>80.39, green=>36.08, blue=>36.08),
IndianRed1    => (name=>"Indian Red 1       ", red=>100.00, green=>41.57, blue=>41.57),
IndianRed2    => (name=>"Indian Red 2       ", red=>93.33, green=>38.82, blue=>38.82),
IndianRed3    => (name=>"Indian Red 3       ", red=>80.39, green=>33.33, blue=>33.33),
IndianRed4    => (name=>"Indian Red 4       ", red=>54.51, green=>22.75, blue=>22.75),
Ivory         => (name=>"ivory              ", red=>100.00, green=>100.00, blue=>94.12),
Ivory1        => (name=>"ivory 1            ", red=>100.00, green=>100.00, blue=>94.12),
Ivory2        => (name=>"ivory 2            ", red=>93.33, green=>93.33, blue=>87.84),
Ivory3        => (name=>"ivory 3            ", red=>80.39, green=>80.39, blue=>75.69),
Ivory4        => (name=>"ivory 4            ", red=>54.51, green=>54.51, blue=>51.37),
Khaki         => (name=>"khaki              ", red=>94.12, green=>90.20, blue=>54.90),
Khaki1        => (name=>"khaki 1            ", red=>100.00, green=>96.47, blue=>56.08),
Khaki2        => (name=>"khaki 2            ", red=>93.33, green=>90.20, blue=>52.16),
Khaki3        => (name=>"khaki 3            ", red=>80.39, green=>77.65, blue=>45.10),
Khaki4        => (name=>"khaki 4            ", red=>54.51, green=>52.55, blue=>30.59),
Lavender      => (name=>"lavender           ", red=>90.20, green=>90.20, blue=>98.04),
LavenderBlush => (name=>"Lavender Blush     ", red=>100.00, green=>94.12, blue=>96.08),
LavenderBlush1 => (name=>"Lavender Blush 1   ", red=>100.00, green=>94.12, blue=>96.08),
LavenderBlush2 => (name=>"Lavender Blush 2   ", red=>93.33, green=>87.84, blue=>89.80),
LavenderBlush3 => (name=>"Lavender Blush 3   ", red=>80.39, green=>75.69, blue=>77.25),
LavenderBlush4 => (name=>"Lavender Blush 4   ", red=>54.51, green=>51.37, blue=>52.55),
LawnGreen     => (name=>"Lawn Green         ", red=>48.63, green=>98.82, blue=>0.00),
LemonChiffon  => (name=>"Lemon Chiffon      ", red=>100.00, green=>98.04, blue=>80.39),
LemonChiffon1 => (name=>"Lemon Chiffon 1    ", red=>100.00, green=>98.04, blue=>80.39),
LemonChiffon2 => (name=>"Lemon Chiffon 2    ", red=>93.33, green=>91.37, blue=>74.90),
LemonChiffon3 => (name=>"Lemon Chiffon 3    ", red=>80.39, green=>78.82, blue=>64.71),
LemonChiffon4 => (name=>"Lemon Chiffon 4    ", red=>54.51, green=>53.73, blue=>43.92),
LightBlue     => (name=>"Light Blue         ", red=>67.84, green=>84.71, blue=>90.20),
LightBlue1    => (name=>"Light Blue1        ", red=>74.90, green=>93.73, blue=>100.00),
LightBlue2    => (name=>"Light Blue2        ", red=>69.80, green=>87.45, blue=>93.33),
LightBlue3    => (name=>"Light Blue3        ", red=>60.39, green=>75.29, blue=>80.39),
LightBlue4    => (name=>"Light Blue4        ", red=>40.78, green=>51.37, blue=>54.51),
LightCoral    => (name=>"Light Coral        ", red=>94.12, green=>50.20, blue=>50.20),
LightCyan     => (name=>"Light Cyan         ", red=>87.84, green=>100.00, blue=>100.00),
LightCyan1    => (name=>"Light Cyan1        ", red=>87.84, green=>100.00, blue=>100.00),
LightCyan2    => (name=>"Light Cyan2        ", red=>81.96, green=>93.33, blue=>93.33),
LightCyan3    => (name=>"Light Cyan3        ", red=>70.59, green=>80.39, blue=>80.39),
LightCyan4    => (name=>"Light Cyan4        ", red=>47.84, green=>54.51, blue=>54.51),
LightGoldenrod => (name=>"Light Goldenrod    ", red=>93.33, green=>86.67, blue=>50.98),
LightGoldenrod1 => (name=>"Light Goldenrod1   ", red=>100.00, green=>92.55, blue=>54.51),
LightGoldenrod2 => (name=>"Light Goldenrod2   ", red=>93.33, green=>86.27, blue=>50.98),
LightGoldenrod3 => (name=>"Light Goldenrod3   ", red=>80.39, green=>74.51, blue=>43.92),
LightGoldenrod4 => (name=>"Light Goldenrod4   ", red=>54.51, green=>50.59, blue=>29.80),
LightGoldenrodYellow => (name=>"LightGoldrodYellow ", red=>98.04, green=>98.04, blue=>82.35),
LightGreen    => (name=>"Light Green        ", red=>56.47, green=>93.33, blue=>56.47),
LightGrey     => (name=>"Light Grey         ", red=>82.75, green=>82.75, blue=>82.75),
LightPink     => (name=>"Light Pink         ", red=>100.00, green=>71.37, blue=>75.69),
LightPink1    => (name=>"Light Pink 1       ", red=>100.00, green=>68.24, blue=>72.55),
LightPink2    => (name=>"Light Pink 2       ", red=>93.33, green=>63.53, blue=>67.84),
LightPink3    => (name=>"Light Pink 3       ", red=>80.39, green=>54.90, blue=>58.43),
LightPink4    => (name=>"Light Pink 4       ", red=>54.51, green=>37.25, blue=>39.61),
LightSalmon   => (name=>"Light Salmon       ", red=>100.00, green=>62.75, blue=>47.84),
LightSalmon1  => (name=>"Light Salmon 1     ", red=>100.00, green=>62.75, blue=>47.84),
LightSalmon2  => (name=>"Light Salmon 2     ", red=>93.33, green=>58.43, blue=>44.71),
LightSalmon3  => (name=>"Light Salmon 3     ", red=>80.39, green=>50.59, blue=>38.43),
LightSalmon4  => (name=>"Light Salmon 4     ", red=>54.51, green=>34.12, blue=>25.88),
LightSeaGreen => (name=>"Light Sea Green    ", red=>12.55, green=>69.80, blue=>66.67),
LightSkyBlue  => (name=>"Light Sky Blue     ", red=>52.94, green=>80.78, blue=>98.04),
LightSkyBlue1 => (name=>"Light Sky Blue 1   ", red=>69.02, green=>88.63, blue=>100.00),
LightSkyBlue2 => (name=>"Light Sky Blue 2   ", red=>64.31, green=>82.75, blue=>93.33),
LightSkyBlue3 => (name=>"Light Sky Blue 3   ", red=>55.29, green=>71.37, blue=>80.39),
LightSkyBlue4 => (name=>"Light Sky Blue 4   ", red=>37.65, green=>48.24, blue=>54.51),
LightSlateBlue => (name=>"Light Slate Blue   ", red=>51.76, green=>43.92, blue=>100.00),
LightSlateGrey => (name=>"Light Slate Grey   ", red=>46.67, green=>53.33, blue=>60.00),
LightSteelBlue => (name=>"Light Steel Blue   ", red=>69.02, green=>76.86, blue=>87.06),
LightSteelBlue1 => (name=>"Light Steel Blue 1 ", red=>79.22, green=>88.24, blue=>100.00),
LightSteelBlue2 => (name=>"Light Steel Blue 2 ", red=>73.73, green=>82.35, blue=>93.33),
LightSteelBlue3 => (name=>"Light Steel Blue 3 ", red=>63.53, green=>70.98, blue=>80.39),
LightSteelBlue4 => (name=>"Light Steel Blue 4 ", red=>43.14, green=>48.24, blue=>54.51),
LightYellow   => (name=>"Light Yellow       ", red=>100.00, green=>100.00, blue=>87.84),
LightYellow1  => (name=>"Light Yellow 1     ", red=>100.00, green=>100.00, blue=>87.84),
LightYellow2  => (name=>"Light Yellow 2     ", red=>93.33, green=>93.33, blue=>81.96),
LightYellow3  => (name=>"Light Yellow 3     ", red=>80.39, green=>80.39, blue=>70.59),
LightYellow4  => (name=>"Light Yellow 4     ", red=>54.51, green=>54.51, blue=>47.84),
LimeGreen     => (name=>"Lime Green         ", red=>19.61, green=>80.39, blue=>19.61),
Linen         => (name=>"linen              ", red=>98.04, green=>94.12, blue=>90.20),
Magenta       => (name=>"magenta            ", red=>100.00, green=>0.00, blue=>100.00),
Magenta1      => (name=>"magenta 1          ", red=>100.00, green=>0.00, blue=>100.00),
Magenta2      => (name=>"magenta 2          ", red=>93.33, green=>0.00, blue=>93.33),
Magenta3      => (name=>"magenta 3          ", red=>80.39, green=>0.00, blue=>80.39),
Magenta4      => (name=>"magenta 4          ", red=>54.51, green=>0.00, blue=>54.51),
Maroon        => (name=>"maroon             ", red=>69.02, green=>18.82, blue=>37.65),
Maroon1       => (name=>"maroon 1           ", red=>100.00, green=>20.39, blue=>70.20),
Maroon2       => (name=>"maroon 2           ", red=>93.33, green=>18.82, blue=>65.49),
Maroon3       => (name=>"maroon 3           ", red=>80.39, green=>16.08, blue=>56.47),
Maroon4       => (name=>"maroon 4           ", red=>54.51, green=>10.98, blue=>38.43),
MediumAquamarine => (name=>"Medium Aquamarine  ", red=>40.00, green=>80.39, blue=>66.67),
MediumBlue    => (name=>"Medium Blue        ", red=>0.00, green=>0.00, blue=>80.39),
MediumOrchid  => (name=>"Medium Orchid      ", red=>72.94, green=>33.33, blue=>82.75),
MediumOrchid1 => (name=>"Medium Orchid 1    ", red=>87.84, green=>40.00, blue=>100.00),
MediumOrchid2 => (name=>"Medium Orchid 2    ", red=>81.96, green=>37.25, blue=>93.33),
MediumOrchid3 => (name=>"Medium Orchid 3    ", red=>70.59, green=>32.16, blue=>80.39),
MediumOrchid4 => (name=>"Medium Orchid 4    ", red=>47.84, green=>21.57, blue=>54.51),
MediumPurple  => (name=>"Medium Purple      ", red=>57.65, green=>43.92, blue=>85.88),
MediumPurple1 => (name=>"Medium Purple 1    ", red=>67.06, green=>50.98, blue=>100.00),
MediumPurple2 => (name=>"Medium Purple 2    ", red=>62.35, green=>47.45, blue=>93.33),
MediumPurple3 => (name=>"Medium Purple 3    ", red=>53.73, green=>40.78, blue=>80.39),
MediumPurple4 => (name=>"Medium Purple 4    ", red=>36.47, green=>27.84, blue=>54.51),
MediumSeaGreen => (name=>"Medium Sea Green   ", red=>23.53, green=>70.20, blue=>44.31),
MediumSlateBlue => (name=>"Medium Slate Blue  ", red=>48.24, green=>40.78, blue=>93.33),
MediumSpringGreen => (name=>"Medium Spring Green", red=>0.00, green=>98.04, blue=>60.39),
MediumTurquoise => (name=>"Medium Turquoise   ", red=>28.24, green=>81.96, blue=>80.00),
MediumVioletRed => (name=>"Medium Violet Red  ", red=>78.04, green=>8.24, blue=>52.16),
MidnightBlue  => (name=>"Midnight Blue      ", red=>9.80, green=>9.80, blue=>43.92),
MintCream     => (name=>"Mint Cream         ", red=>96.08, green=>100.00, blue=>98.04),
MistyRose     => (name=>"Misty Rose         ", red=>100.00, green=>89.41, blue=>88.24),
MistyRose1    => (name=>"Misty Rose 1       ", red=>100.00, green=>89.41, blue=>88.24),
MistyRose2    => (name=>"Misty Rose 2       ", red=>93.33, green=>83.53, blue=>82.35),
MistyRose3    => (name=>"Misty Rose 3       ", red=>80.39, green=>71.76, blue=>70.98),
MistyRose4    => (name=>"Misty Rose 4       ", red=>54.51, green=>49.02, blue=>48.24),
Moccasin      => (name=>"moccasin           ", red=>100.00, green=>89.41, blue=>70.98),
NavajoWhite   => (name=>"Navajo White       ", red=>100.00, green=>87.06, blue=>67.84),
NavajoWhite1  => (name=>"Navajo White 1     ", red=>100.00, green=>87.06, blue=>67.84),
NavajoWhite2  => (name=>"Navajo White 2     ", red=>93.33, green=>81.18, blue=>63.14),
NavajoWhite3  => (name=>"Navajo White 3     ", red=>80.39, green=>70.20, blue=>54.51),
NavajoWhite4  => (name=>"Navajo White 4     ", red=>54.51, green=>47.45, blue=>36.86),
NavyBlue      => (name=>"Navy Blue          ", red=>0.00, green=>0.00, blue=>50.20),
OldLace       => (name=>"Old Lace           ", red=>99.22, green=>96.08, blue=>90.20),
OliveDrab     => (name=>"Olive Drab         ", red=>41.96, green=>55.69, blue=>13.73),
OliveDrab1    => (name=>"Olive Drab 1       ", red=>75.29, green=>100.00, blue=>24.31),
OliveDrab2    => (name=>"Olive Drab 2       ", red=>70.20, green=>93.33, blue=>22.75),
OliveDrab3    => (name=>"Olive Drab 3       ", red=>60.39, green=>80.39, blue=>19.61),
OliveDrab4    => (name=>"Olive Drab 4       ", red=>41.18, green=>54.51, blue=>13.33),
Orange        => (name=>"orange             ", red=>100.00, green=>64.71, blue=>0.00),
Orange1       => (name=>"orange 1           ", red=>100.00, green=>64.71, blue=>0.00),
Orange2       => (name=>"orange 2           ", red=>93.33, green=>60.39, blue=>0.00),
Orange3       => (name=>"orange 3           ", red=>80.39, green=>52.16, blue=>0.00),
Orange4       => (name=>"orange 4           ", red=>54.51, green=>35.29, blue=>0.00),
OrangeRed     => (name=>"Orange Red         ", red=>100.00, green=>27.06, blue=>0.00),
OrangeRed1    => (name=>"Orange Red 1       ", red=>100.00, green=>27.06, blue=>0.00),
OrangeRed2    => (name=>"Orange Red 2       ", red=>93.33, green=>25.10, blue=>0.00),
OrangeRed3    => (name=>"Orange Red 3       ", red=>80.39, green=>21.57, blue=>0.00),
OrangeRed4    => (name=>"Orange Red 4       ", red=>54.51, green=>14.51, blue=>0.00),
Orchid        => (name=>"orchid             ", red=>85.49, green=>43.92, blue=>83.92),
Orchid1       => (name=>"orchid 1           ", red=>100.00, green=>51.37, blue=>98.04),
Orchid2       => (name=>"orchid 2           ", red=>93.33, green=>47.84, blue=>91.37),
Orchid3       => (name=>"orchid 3           ", red=>80.39, green=>41.18, blue=>78.82),
Orchid4       => (name=>"orchid 4           ", red=>54.51, green=>27.84, blue=>53.73),
PaleGoldenrod => (name=>"Pale Goldenrod     ", red=>93.33, green=>90.98, blue=>66.67),
PaleGreen     => (name=>"Pale Green         ", red=>59.61, green=>98.43, blue=>59.61),
PaleGreen1    => (name=>"Pale Green 1       ", red=>60.39, green=>100.00, blue=>60.39),
PaleGreen2    => (name=>"Pale Green 2       ", red=>56.47, green=>93.33, blue=>56.47),
PaleGreen3    => (name=>"Pale Green 3       ", red=>48.63, green=>80.39, blue=>48.63),
PaleGreen4    => (name=>"Pale Green 4       ", red=>32.94, green=>54.51, blue=>32.94),
PaleTurquoise => (name=>"Pale Turquoise     ", red=>68.63, green=>93.33, blue=>93.33),
PaleTurquoise1 => (name=>"Pale Turquoise 1   ", red=>73.33, green=>100.00, blue=>100.00),
PaleTurquoise2 => (name=>"Pale Turquoise 2   ", red=>68.24, green=>93.33, blue=>93.33),
PaleTurquoise3 => (name=>"Pale Turquoise 3   ", red=>58.82, green=>80.39, blue=>80.39),
PaleTurquoise4 => (name=>"Pale Turquoise 4   ", red=>40.00, green=>54.51, blue=>54.51),
PaleVioletRed => (name=>"Pale Violet Red    ", red=>85.88, green=>43.92, blue=>57.65),
PaleVioletRed1 => (name=>"Pale Violet Red 1  ", red=>100.00, green=>50.98, blue=>67.06),
PaleVioletRed2 => (name=>"Pale Violet Red 2  ", red=>93.33, green=>47.45, blue=>62.35),
PaleVioletRed3 => (name=>"Pale Violet Red 3  ", red=>80.39, green=>40.78, blue=>53.73),
PaleVioletRed4 => (name=>"Pale Violet Red 4  ", red=>54.51, green=>27.84, blue=>36.47),
PapayaWhip    => (name=>"Papaya Whip        ", red=>100.00, green=>93.73, blue=>83.53),
PeachPuff     => (name=>"Peach Puff         ", red=>100.00, green=>85.49, blue=>72.55),
PeachPuff1    => (name=>"Peach Puff 1       ", red=>100.00, green=>85.49, blue=>72.55),
PeachPuff2    => (name=>"Peach Puff 2       ", red=>93.33, green=>79.61, blue=>67.84),
PeachPuff3    => (name=>"Peach Puff 3       ", red=>80.39, green=>68.63, blue=>58.43),
PeachPuff4    => (name=>"Peach Puff 4       ", red=>54.51, green=>46.67, blue=>39.61),
Peru          => (name=>"peru               ", red=>80.39, green=>52.16, blue=>24.71),
Pink          => (name=>"pink               ", red=>100.00, green=>75.29, blue=>79.61),
Pink1         => (name=>"pink 1             ", red=>100.00, green=>70.98, blue=>77.25),
Pink2         => (name=>"pink 2             ", red=>93.33, green=>66.27, blue=>72.16),
Pink3         => (name=>"pink 3             ", red=>80.39, green=>56.86, blue=>61.96),
Pink4         => (name=>"pink 4             ", red=>54.51, green=>38.82, blue=>42.35),
Plum          => (name=>"plum               ", red=>86.67, green=>62.75, blue=>86.67),
Plum1         => (name=>"plum 1             ", red=>100.00, green=>73.33, blue=>100.00),
Plum2         => (name=>"plum 2             ", red=>93.33, green=>68.24, blue=>93.33),
Plum3         => (name=>"plum 3             ", red=>80.39, green=>58.82, blue=>80.39),
Plum4         => (name=>"plum 4             ", red=>54.51, green=>40.00, blue=>54.51),
PowderBlue    => (name=>"Powder Blue        ", red=>69.02, green=>87.84, blue=>90.20),
Purple        => (name=>"purple             ", red=>62.75, green=>12.55, blue=>94.12),
Purple1       => (name=>"purple 1           ", red=>60.78, green=>18.82, blue=>100.00),
Purple2       => (name=>"purple 2           ", red=>56.86, green=>17.25, blue=>93.33),
Purple3       => (name=>"purple 3           ", red=>49.02, green=>14.90, blue=>80.39),
Purple4       => (name=>"purple 4           ", red=>33.33, green=>10.20, blue=>54.51),
Red           => (name=>"red                ", red=>100.00, green=>0.00, blue=>0.00),
Red1          => (name=>"red 1              ", red=>100.00, green=>0.00, blue=>0.00),
Red2          => (name=>"red 2              ", red=>93.33, green=>0.00, blue=>0.00),
Red3          => (name=>"red 3              ", red=>80.39, green=>0.00, blue=>0.00),
Red4          => (name=>"red 4              ", red=>54.51, green=>0.00, blue=>0.00),
RosyBrown     => (name=>"Rosy Brown         ", red=>73.73, green=>56.08, blue=>56.08),
RosyBrown1    => (name=>"Rosy Brown 1       ", red=>100.00, green=>75.69, blue=>75.69),
RosyBrown2    => (name=>"Rosy Brown 2       ", red=>93.33, green=>70.59, blue=>70.59),
RosyBrown3    => (name=>"Rosy Brown 3       ", red=>80.39, green=>60.78, blue=>60.78),
RosyBrown4    => (name=>"Rosy Brown 4       ", red=>54.51, green=>41.18, blue=>41.18),
RoyalBlue     => (name=>"Royal Blue         ", red=>25.49, green=>41.18, blue=>88.24),
RoyalBlue1    => (name=>"Royal Blue 1       ", red=>28.24, green=>46.27, blue=>100.00),
RoyalBlue2    => (name=>"Royal Blue 2       ", red=>26.27, green=>43.14, blue=>93.33),
RoyalBlue3    => (name=>"Royal Blue 3       ", red=>22.75, green=>37.25, blue=>80.39),
RoyalBlue4    => (name=>"Royal Blue 4       ", red=>15.29, green=>25.10, blue=>54.51),
SaddleBrown   => (name=>"Saddle Brown       ", red=>54.51, green=>27.06, blue=>7.45),
Salmon        => (name=>"salmon             ", red=>98.04, green=>50.20, blue=>44.71),
Salmon1       => (name=>"salmon 1           ", red=>100.00, green=>54.90, blue=>41.18),
Salmon2       => (name=>"salmon 2           ", red=>93.33, green=>50.98, blue=>38.43),
Salmon3       => (name=>"salmon 3           ", red=>80.39, green=>43.92, blue=>32.94),
Salmon4       => (name=>"salmon 4           ", red=>54.51, green=>29.80, blue=>22.35),
SandyBrown    => (name=>"Sandy Brown        ", red=>95.69, green=>64.31, blue=>37.65),
SeaGreen      => (name=>"Sea Green          ", red=>18.04, green=>54.51, blue=>34.12),
SeaGreen1     => (name=>"Sea Green 1        ", red=>32.94, green=>100.00, blue=>62.35),
SeaGreen2     => (name=>"Sea Green 2        ", red=>30.59, green=>93.33, blue=>58.04),
SeaGreen3     => (name=>"Sea Green 3        ", red=>26.27, green=>80.39, blue=>50.20),
SeaGreen4     => (name=>"Sea Green 4        ", red=>18.04, green=>54.51, blue=>34.12),
Seashell      => (name=>"seashell           ", red=>100.00, green=>96.08, blue=>93.33),
Seashell1     => (name=>"seashell 1         ", red=>100.00, green=>96.08, blue=>93.33),
Seashell2     => (name=>"seashell 2         ", red=>93.33, green=>89.80, blue=>87.06),
Seashell3     => (name=>"seashell 3         ", red=>80.39, green=>77.25, blue=>74.90),
Seashell4     => (name=>"seashell 4         ", red=>54.51, green=>52.55, blue=>50.98),
Sienna        => (name=>"sienna             ", red=>62.75, green=>32.16, blue=>17.65),
Sienna1       => (name=>"sienna 1           ", red=>100.00, green=>50.98, blue=>27.84),
Sienna2       => (name=>"sienna 2           ", red=>93.33, green=>47.45, blue=>25.88),
Sienna3       => (name=>"sienna 3           ", red=>80.39, green=>40.78, blue=>22.35),
Sienna4       => (name=>"sienna 4           ", red=>54.51, green=>27.84, blue=>14.90),
SkyBlue       => (name=>"Sky Blue           ", red=>52.94, green=>80.78, blue=>92.16),
SkyBlue1      => (name=>"Sky Blue 1         ", red=>52.94, green=>80.78, blue=>100.00),
SkyBlue2      => (name=>"Sky Blue 2         ", red=>49.41, green=>75.29, blue=>93.33),
SkyBlue3      => (name=>"Sky Blue 3         ", red=>42.35, green=>65.10, blue=>80.39),
SkyBlue4      => (name=>"Sky Blue 4         ", red=>29.02, green=>43.92, blue=>54.51),
SlateBlue     => (name=>"Slate Blue         ", red=>41.57, green=>35.29, blue=>80.39),
SlateBlue1    => (name=>"Slate Blue 1       ", red=>51.37, green=>43.53, blue=>100.00),
SlateBlue2    => (name=>"Slate Blue 2       ", red=>47.84, green=>40.39, blue=>93.33),
SlateBlue3    => (name=>"Slate Blue 3       ", red=>41.18, green=>34.90, blue=>80.39),
SlateBlue4    => (name=>"Slate Blue 4       ", red=>27.84, green=>23.53, blue=>54.51),
SlateGray1    => (name=>"Slate Grey 1       ", red=>77.65, green=>88.63, blue=>100.00),
SlateGray2    => (name=>"Slate Grey 2       ", red=>72.55, green=>82.75, blue=>93.33),
SlateGray3    => (name=>"Slate Grey 3       ", red=>62.35, green=>71.37, blue=>80.39),
SlateGray4    => (name=>"Slate Grey 4       ", red=>42.35, green=>48.24, blue=>54.51),
SlateGrey     => (name=>"SlateGrey          ", red=>43.92, green=>50.20, blue=>56.47),
Snow          => (name=>"snow               ", red=>100.00, green=>98.04, blue=>98.04),
Snow1         => (name=>"snow 1             ", red=>100.00, green=>98.04, blue=>98.04),
Snow2         => (name=>"snow 2             ", red=>93.33, green=>91.37, blue=>91.37),
Snow3         => (name=>"snow 3             ", red=>80.39, green=>78.82, blue=>78.82),
Snow4         => (name=>"snow 4             ", red=>54.51, green=>53.73, blue=>53.73),
SpringGreen   => (name=>"Spring Green       ", red=>0.00, green=>100.00, blue=>49.80),
SpringGreen1  => (name=>"Spring Green 1     ", red=>0.00, green=>100.00, blue=>49.80),
SpringGreen2  => (name=>"Spring Green 2     ", red=>0.00, green=>93.33, blue=>46.27),
SpringGreen3  => (name=>"Spring Green 3     ", red=>0.00, green=>80.39, blue=>40.00),
SpringGreen4  => (name=>"Spring Green 4     ", red=>0.00, green=>54.51, blue=>27.06),
SteelBlue     => (name=>"Steel Blue         ", red=>27.45, green=>50.98, blue=>70.59),
SteelBlue1    => (name=>"Steel Blue 1       ", red=>38.82, green=>72.16, blue=>100.00),
SteelBlue2    => (name=>"Steel Blue 2       ", red=>36.08, green=>67.45, blue=>93.33),
SteelBlue3    => (name=>"Steel Blue 3       ", red=>30.98, green=>58.04, blue=>80.39),
SteelBlue4    => (name=>"Steel Blue 4       ", red=>21.18, green=>39.22, blue=>54.51),
Tan           => (name=>"tan                ", red=>82.35, green=>70.59, blue=>54.90),
Tan1          => (name=>"tan 1              ", red=>100.00, green=>64.71, blue=>30.98),
Tan2          => (name=>"tan 2              ", red=>93.33, green=>60.39, blue=>28.63),
Tan3          => (name=>"tan 3              ", red=>80.39, green=>52.16, blue=>24.71),
Tan4          => (name=>"tan 4              ", red=>54.51, green=>35.29, blue=>16.86),
Thistle       => (name=>"thistle            ", red=>84.71, green=>74.90, blue=>84.71),
Thistle1      => (name=>"thistle 1          ", red=>100.00, green=>88.24, blue=>100.00),
Thistle2      => (name=>"thistle 2          ", red=>93.33, green=>82.35, blue=>93.33),
Thistle3      => (name=>"thistle 3          ", red=>80.39, green=>70.98, blue=>80.39),
Thistle4      => (name=>"thistle 4          ", red=>54.51, green=>48.24, blue=>54.51),
Tomato        => (name=>"tomato             ", red=>100.00, green=>38.82, blue=>27.84),
Tomato1       => (name=>"tomato 1           ", red=>100.00, green=>38.82, blue=>27.84),
Tomato2       => (name=>"tomato 2           ", red=>93.33, green=>36.08, blue=>25.88),
Tomato3       => (name=>"tomato 3           ", red=>80.39, green=>30.98, blue=>22.35),
Tomato4       => (name=>"tomato 4           ", red=>54.51, green=>21.18, blue=>14.90),
Turquoise     => (name=>"turquoise          ", red=>25.10, green=>87.84, blue=>81.57),
Turquoise1    => (name=>"turquoise 1        ", red=>0.00, green=>96.08, blue=>100.00),
Turquoise2    => (name=>"turquoise 2        ", red=>0.00, green=>89.80, blue=>93.33),
Turquoise3    => (name=>"turquoise 3        ", red=>0.00, green=>77.25, blue=>80.39),
Turquoise4    => (name=>"turquoise 4        ", red=>0.00, green=>52.55, blue=>54.51),
Violet        => (name=>"violet             ", red=>93.33, green=>50.98, blue=>93.33),
VioletRed     => (name=>"Violet Red         ", red=>81.57, green=>12.55, blue=>56.47),
VioletRed1    => (name=>"Violet Red 1       ", red=>100.00, green=>24.31, blue=>58.82),
VioletRed2    => (name=>"Violet Red 2       ", red=>93.33, green=>22.75, blue=>54.90),
VioletRed3    => (name=>"Violet Red 3       ", red=>80.39, green=>19.61, blue=>47.06),
VioletRed4    => (name=>"Violet Red 4       ", red=>54.51, green=>13.33, blue=>32.16),
Wheat         => (name=>"wheat              ", red=>96.08, green=>87.06, blue=>70.20),
Wheat1        => (name=>"wheat 1            ", red=>100.00, green=>90.59, blue=>72.94),
Wheat2        => (name=>"wheat 2            ", red=>93.33, green=>84.71, blue=>68.24),
Wheat3        => (name=>"wheat 3            ", red=>80.39, green=>72.94, blue=>58.82),
Wheat4        => (name=>"wheat 4            ", red=>54.51, green=>49.41, blue=>40.00),
White         => (name=>"white              ", red=>100.00, green=>100.00, blue=>100.00),
WhiteSmoke    => (name=>"White Smoke        ", red=>96.08, green=>96.08, blue=>96.08),
Yellow        => (name=>"yellow             ", red=>100.00, green=>100.00, blue=>0.00),
Yellow1       => (name=>"yellow 1           ", red=>100.00, green=>100.00, blue=>0.00),
Yellow2       => (name=>"yellow 2           ", red=>93.33, green=>93.33, blue=>0.00),
Yellow3       => (name=>"yellow 3           ", red=>80.39, green=>80.39, blue=>0.00),
Yellow4       => (name=>"yellow 4           ", red=>54.51, green=>54.51, blue=>0.00),
YellowGreen   => (name=>"Yellow Green       ", red=>60.39, green=>80.39, blue=>19.61)
);


---> SET PEN INK
--
-- Change the drawing colour or pattern of a canvas pen.
-----------------------------------------------------------------------------

procedure setPenInk( canvas : in out aCanvas; c : AColourName );
procedure setPenInk( canvas_id : aCanvasID; c : aColourName );
procedure setPenInk( canvas : in out aCanvas; r, g, b : ARGBComponent );
procedure setPenInk( canvas_id : aCanvasID; r, g, b : aRGBComponent );
procedure setPenInk( canvas : in out aCanvas; pattern : aCanvas );
procedure setPenInk( canvas_id, pattern_id : aCanvasID );


---> GET PEN INK
--
-- Get the drawing ink of a canvas pen.
-----------------------------------------------------------------------------

procedure getPenInk( canvas : aCanvas; R, G, B : out aRGBComponent );
procedure getPenInk( canvas_id : aCanvasID; R, G, B : out aRGBComponent );


---> SET PEN BRUSH
--
-- Change the drawing pattern of a canvas pen.
------------------------------------------------------------------------------

procedure setPenBrush( canvas_id : aCanvasID; newBrush : aPenBrush );
procedure setPenBrush( canvas : in out aCanvas; newBrush : aPenBrush );

function getPenBrush( canvas_id : aCanvasID ) return aPenBrush;
function getPenBrush( canvas : aCanvas ) return aPenBrush;


---> SET PEN PATTERN
--
-- Change the drawing pattern of a canvas pen.
-----------------------------------------------------------------------------

procedure setPenPattern( canvas : in out aCanvas; pattern : aCanvas );
procedure setPenPattern( canvas_id, pattern_id : aCanvasID );

-- function getPenPattern( canvas_id : canvasList.aListIndex ) return canvasList.aListIndex;
-- function getPenPattern( canvas : aCanvas ) return aCanvas;

---> SET PEN MODE
--
-- Change the drawing mode of a canvas pen.
-----------------------------------------------------------------------------

procedure setPenMode( canvas : in out aCanvas; newMode : aPenMode );
procedure setPenMode( canvas_id : aCanvasID; newMode : aPenMode );


---> GET PEN MODE
--
-- Get the drawing mode of a canvas pen.
-----------------------------------------------------------------------------

function getPenMode( canvas : aCanvas ) return aPenMode;
function getPenMode( canvas_id : aCanvasID ) return aPenMode;


---> NEW PEN STATE
--
-- Initialize a pen to the default state.  The default state is zero for
-- position, angle, pixel.  Mode is copy.  Brush is a pencil.  Does not
-- free the pattern canvas.
-----------------------------------------------------------------------------

procedure newPenState( ps : in out aPenState );


-----------------------------------------------------------------------------
-- COLOUR OPERATIONS
-----------------------------------------------------------------------------


---> GREY SCALE
--
-- Convert an RGB colour to greyscale weighted for the human eye.
------------------------------------------------------------------------------

function greyScale( redC, greenC, blueC : ARGBComponent ) return
  ARGBComponent;
function greyScale( colour : AColourName ) return ARGBComponent;


---> BLEND
--
-- Average two RGB colours to create a new colour.
------------------------------------------------------------------------------

procedure blend( redC1, greenC1, blueC1 : ARGBComponent;
                 redC2, greenC2, blueC2 : ARGBComponent;
                 redC3, greenC3, blueC3 : out ARGBComponent );


---> FADE
--
-- Brighten or darken a colour by a percentage.
------------------------------------------------------------------------------

procedure fade( redC1, greenC1, blueC1 : ARGBComponent;
                fadeamount : float;
                redC2, greenC2, blueC2 : out ARGBComponent );
procedure fade( redC, greenC, blueC : in out ARGBComponent ;
                fadeamount : float );


-----------------------------------------------------------------------------
-- HOUSEKEEPING
-----------------------------------------------------------------------------


---> Startup
--
-- Startup the pen package.
-----------------------------------------------------------------------------

procedure startupPen;


---> Shutdown
--
-- Shutdown the pen package.
-----------------------------------------------------------------------------

procedure shutdownPen;


---> isRunning
--
-- True if pen package is running.
-----------------------------------------------------------------------------

function isPenRunning return boolean;


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

procedure waitToReveal( theCanvas : in out aCanvas );
procedure waitToReveal( canvas_id : aCanvasID );


---> REVEAL
--
-- Finish a series of drawing procedures started with beginDrawing and reveal
-- the result.  waitToReveal can be nested with other waitToReveal calls
-- and the result will only be shown after all have been completed with
-- doneDrawing.  Note this updates the entire canvas.
-----------------------------------------------------------------------------

procedure reveal( theCanvas : in out aCanvas );
procedure reveal( canvas_id : aCanvasID );


---> REVEAL NOW
--
-- If beginDrawing has been used, cancel the request, show the results so far
-- and continue drawing normally.    Note this updates the entire canvas.
-- Intended for debugging.
-----------------------------------------------------------------------------

procedure revealNow( theCanvas : in out aCanvas );
procedure revealNow( canvas_id : aCanvasID );


-----------------------------------------------------------------------------
-- CANVAS-WIDE DRAWING
-----------------------------------------------------------------------------

---> Clear
--
-- Clear the canvas to the current color or an arbitrary color.
-----------------------------------------------------------------------------

procedure Clear( theCanvas : aCanvas );
procedure Clear( canvas_id : aCanvasID );
procedure Clear( theCanvas : aCanvas; r, g, b : ARGBComponent );
procedure Clear( canvas_id : aCanvasID; r, g, b : ARGBComponent );
procedure Clear( theCanvas : aCanvas; c : AColorName );
procedure Clear( canvas_id : aCanvasID; c : AColorName );


-----------------------------------------------------------------------------
-- RECTANGLE DRAWING
-----------------------------------------------------------------------------


---> FRAME RECT
--
-- Draw the outline of a rectangle with the current pen.
-----------------------------------------------------------------------------

procedure frameRect( theCanvas : aCanvas; r : ARect );
procedure frameRect( canvas_id : aCanvasID; r : ARect );


---> FILL RECT
--
-- Fill in a rectangle with an arbitrary color.
-----------------------------------------------------------------------------

procedure fillRect( theCanvas : aCanvas; theRect : ARect; r, g, b : ARGBComponent );
procedure fillRect( canvas_id : aCanvasID; theRect : ARect; r, g, b : ARGBComponent );

procedure fillRect( theCanvas : aCanvas; theRect : ARect; c : AColourName );
procedure fillRect( canvas_id : aCanvasID; theRect : ARect; c : AColourName );


---> PAINT RECT
--
-- Fill in a rectangle with the current pen.
-----------------------------------------------------------------------------

procedure paintRect( theCanvas : aCanvas; r : ARect );
procedure paintRect( canvas_id : aCanvasID; r : ARect );


---> FRAMED RECT
--
-- Fill in a rectangle with an arbitrary color and draw a frame with another
-- colour.
-----------------------------------------------------------------------------

procedure framedRect( canvas_id : aCanvasID; r : ARect; fore_r, fore_g, fore_b, back_r, back_g, back_b : ARGBComponent );
-- should support color names
procedure framedRect( theCanvas : aCanvas; r : ARect; fore_r, fore_g, fore_b, back_r, back_g, back_b : ARGBComponent );


-----------------------------------------------------------------------------
-- ELLIPSE DRAWING
-----------------------------------------------------------------------------


---> FRAME ELLIPSE
--
-- Draw the outline of an ellipse with the current pen.
-----------------------------------------------------------------------------

procedure frameEllipse( theCanvas : aCanvas; r : ARect );
procedure frameEllipse( canvas_id : aCanvasID; r : ARect );


---> PAINT ELLIPSE
--
-- Fill in an ellipse with the current pen.
-----------------------------------------------------------------------------

procedure paintEllipse( theCanvas : aCanvas; r : ARect );
procedure paintEllipse( canvas_id : aCanvasID; r : ARect );


---> FILL ELLIPSE
--
-- Fill in an ellipse with the current pen.
-----------------------------------------------------------------------------

procedure fillEllipse( theCanvas : aCanvas; theRect : ARect; r, g, b : ARGBComponent ) ;
procedure fillEllipse( canvas_id : aCanvasID; theRect : ARect; r, g, b : ARGBComponent );

procedure fillEllipse( theCanvas : aCanvas; theRect : ARect; c : AColourName );
procedure fillEllipse( canvas_id : aCanvasID; theRect : ARect; c : AColourName );


-----------------------------------------------------------------------------
-- CANVAS
-----------------------------------------------------------------------------


---> NEW SCREEN CANVAS
--
-- Create a new on-screen canvas covering the whole screen.  The size and
-- resolution of the canvas is based on the canvas data structure.  There can
-- only be one screen canvas.
-----------------------------------------------------------------------------

procedure newScreenCanvas( H_Res, V_Res, C_Res : positive; canvas_id : out aCanvasID );

procedure newGLScreenCanvas( H_Res, V_Res, C_Res : positive; canvas_id : out aCanvasID );


---> NEW WINDOW CANVAS
--
-- Create a new on-screen canvas with its own window.  The size and resolution
-- of the canvas is based on the canvas data structure.
-----------------------------------------------------------------------------

procedure newWindowCanvas( H_Res, V_Res, C_Res : positive; canvas_id : out aCanvasID );
procedure newGLWindowCanvas( H_Res, V_Res, C_Res : positive; canvas_id : out aCanvasID );


---> NEW CANVAS
--
-- Create a new off-screen canvas based on the canvas data structure
-----------------------------------------------------------------------------

procedure newCanvas( H_Res, V_Res : positive; old_canvas_id : aCanvasID; canvas_id : out aCanvasID );
procedure newCanvas( path : string; canvas_id : out aCanvasID );


---> SAVE CANVAS
--
-- Save a canvas to a BMP image file.
-----------------------------------------------------------------------------

procedure saveCanvas( path : string; canvas_id : aCanvasID );


---> SET TITLE NAME
--
-- Set the name of the canvas.  For a window canvas, change window title.
-----------------------------------------------------------------------------

procedure setTitle( canvas_id : aCanvasID; title :unbounded_string );


---> CLOSE CANVAS
--
-- Discard a canvas created with newCanvas, newScreenCanvas or newWindowCanvas.
-- (But it doesn't remove a window opened..you have to shut down).
-----------------------------------------------------------------------------

procedure closeCanvas( canvas_id : aCanvasID ) ;


---> GET DISPLAY INFO
--
-- Return the display info from a canvas.
-----------------------------------------------------------------------------

--procedure getDisplayInfo( canvas_id : canvasList.aListIndex;  display : out aDisplayInfoRec );


---> CLIP RECT
--
-- Set the clipping region to the given rectangle.
-----------------------------------------------------------------------------

procedure clipRect( theCanvas : in out aCanvas; r : aRect );
procedure clipRect( canvas_id : aCanvasID; r : aRect );


---> MOVING
--
-- Moving the pen without drawing.
-----------------------------------------------------------------------------

procedure moveTo( theCanvas : in out aCanvas; x, y : aCoordinate );
procedure moveTo( canvas_id : aCanvasID; x, y : aCoordinate );

procedure move( theCanvas : in out aCanvas; dx, dy : aCoordinate );
procedure move( canvas_id : aCanvasID; dx, dy : aCoordinate );


---> LINES
--
-- Moving the pen with drawing.
-----------------------------------------------------------------------------

procedure lineTo( theCanvas : in out aCanvas; x, y : aCoordinate );
procedure lineTo( canvas_id : aCanvasID; x, y : aCoordinate );

procedure line( theCanvas : in out aCanvas; dx, dy : aCoordinate );
procedure line( canvas_id : aCanvasID; dx, dy : aCoordinate );

procedure hline( theCanvas : in out aCanvas; x1, x2, y : aCoordinate );
procedure hline( canvas_id : aCanvasID; x1, x2, y : aCoordinate );

procedure vline( theCanvas : in out aCanvas; x, y1, y2 : aCoordinate );
procedure vline( canvas_id : aCanvasID; x, y1, y2 : aCoordinate );

procedure Stretch( sourceCanvas : aCanvas; targetCanvas : in out aCanvas; target_x, target_y : aCoordinate; newWidth, newHeight : aCoordinate );

---> Truetype Fonts TEXT
--
-- Drawing strings.
-----------------------------------------------------------------------------

procedure SetFont( canvas_id : aCanvasID; path : unbounded_string; points : natural );
-- assign a true type font to a canvas

procedure Put( canvas_id : aCanvasID; str : unbounded_string );

procedure CloseFont( canvas_id : aCanvasID );
-- TODO: make cleaner to use

---> GRAPHING
--
-- Drawing graphs.
-----------------------------------------------------------------------------

-- - considering a simple plot (graph) function?
-- compare R langauge http://en.wikipedia.org/wiki/R_%28programming_language%29

type plotValues is array(long_integer range <>) of long_float;

procedure plot( theCanvas : aCanvas; values : plotValues );
procedure plot( canvas_id : aCanvasID; values : plotValues );

end pen;

