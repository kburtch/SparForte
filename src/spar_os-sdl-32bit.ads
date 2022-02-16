------------------------------------------------------------------------------
-- Bindings to the SDL (Simple DirectMedia Layer) Library                   --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2022 Free Software Foundation              --
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

with Ada.Strings.Unbounded, Interfaces.C, System.Address_To_Access_Conversions;
use  Ada.Strings.Unbounded, Interfaces.C;

package spar_os.sdl is

------------------------------------------------------------------------------
-- Standard SDL Types ( from /usr/include/SDL/SDL_types.h)
------------------------------------------------------------------------------

type C_string is new char_array(0..1024);
-- length arbitrarily chosen.  fixed bounds required for C interfacing

--subtype SDL_bool is boolean;
-- SDL bool is the same as an Ada boolean

type SDL_success is new int;
SDL_OK     : constant SDL_success := 0;
SDL_Failed : constant SDL_success := -1;

type Uint8  is new unsigned_char;   for Uint8'size  use 8;
type Sint8  is new signed_char;     for Sint8'size  use 8;
type Uint16 is new unsigned_short; for Uint16'size use 16;
type Sint16 is new short;          for Sint16'size use 16;
type Uint32 is new unsigned;       for Uint32'size use 32;
type Sint32 is new int;            for Sint32'size use 32;
-- constrain the proper number of bits

type private_ptr is new SYSTEM.ADDRESS;
-- generic pointer not to be dereferenced

------------------------------------------------------------------------------
-- Housekeeping (from /usr/include/SDL/SDL.h)
------------------------------------------------------------------------------

type SDL_Init_Flags is new Uint32;
SDL_INIT_TIMER       : constant SDL_Init_Flags := 16#00000001#;
SDL_INIT_AUDIO       : constant SDL_Init_Flags := 16#00000010#;
SDL_INIT_VIDEO       : constant SDL_Init_Flags := 16#00000020#;
SDL_INIT_CDROM       : constant SDL_Init_Flags := 16#00000100#;
SDL_INIT_JOYSTICK    : constant SDL_Init_Flags := 16#00000200#;
SDL_INIT_NOPARACHUTE : constant SDL_Init_Flags := 16#00100000#;
SDL_INIT_EVENTTHREAD : constant SDL_Init_Flags := 16#01000000#;
SDL_INIT_EVERYTHING  : constant SDL_Init_Flags := 16#0000FFFF#;

function SDL_Init( flags : SDL_Init_Flags ) return integer;
pragma import( C, SDL_Init, "SDL_Init" );

function SDL_WasInit( flags : SDL_Init_Flags) return SDL_Init_Flags;
pragma import( C, SDL_WasInit, "SDL_WasInit" );

procedure SDL_Quit;
pragma import( C, SDL_Quit, "SDL_Quit" );

type SDL_version is record
     major : Uint8;
     minor : Uint8;
     patch : Uint8;
end record;
-- SDL_version is a C preprocessor macro so we can't call it from Ada anyway


------------------------------------------------------------------------------
-- Video Subprograms (from /usr/include/SDL/SDL_video.h)
------------------------------------------------------------------------------

type SDL_Coordinate is new Sint16;
type SDL_HCoordinate is new SDL_Coordinate;
type SDL_VCoordinate is new SDL_Coordinate;

type SDL_Generic_Pixel is new Uint32;
type SDL_RGB_Component is new Uint8;
type SDL_RGB_Mask is new Uint32;

type alpha_transparency is new Uint8;
SDL_ALPHA_OPAQUE      : constant alpha_transparency := 255;
SDL_ALPHA_TRANSPARENT : constant alpha_transparency := 0;

type SDL_Rect is record
     x : SDL_HCoordinate;
     y : SDL_VCoordinate;
     w, h : Uint16;
end record;
pragma pack( SDL_Rect );

package SDL_Rect_Conv is new
     system.address_to_access_conversions( SDL_Rect );
subtype SDL_Rect_Ptr is SDL_Rect_Conv.object_pointer;
--package SDL_Rect_Ptr_Conv is new
--     system.address_to_access_conversions( SDL_Rect_Ptr );
--subtype SDL_Rect_Handle is SDL_Rect_Ptr_Conv.object_pointer;

type SDL_Color is record
     r, g, b, unused : SDL_RGB_Component;
end record;
pragma pack( SDL_Color );
subtype SDL_Colour is SDL_Color;

package SDL_Color_Conv is new
     system.address_to_access_conversions( SDL_Color );
subtype SDL_Color_Ptr is SDL_Color_Conv.object_pointer;

type SDL_Palette is record
     ncolors : int;
     colors  : SDL_Color_Ptr; -- actually, an array of colours
end record;
pragma pack( SDL_Palette );

package SDL_Palette_Conv is new
     system.address_to_access_conversions( SDL_Palette );
subtype SDL_Palette_Ptr is SDL_Palette_Conv.object_pointer;

type SDL_PixelFormat is record
        palette : SDL_Palette_Ptr;
        BitsPerPixel : Uint8;
        BytesPerPixel : Uint8;
        Rloss : Uint8;
        Gloss : Uint8;
        Bloss : Uint8;
        Aloss : Uint8;
        Rshift: Uint8;
        Gshift: Uint8;
        Bshift: Uint8;
        Ashift: Uint8;
        Rmask : SDL_RGB_Mask;
        Gmask : SDL_RGB_Mask;
        Bmask : SDL_RGB_Mask;
        Amask : SDL_RGB_Mask;
        colorkey : SDL_Generic_Pixel;
        alpha : alpha_transparency;
end record;
pragma pack( SDL_PixelFormat );

package SDL_PixelFormat_Conv is new
     system.address_to_access_conversions( SDL_PixelFormat );
subtype SDL_PixelFormat_Ptr is SDL_PixelFormat_Conv.object_pointer;

type SDL_Surface_Flag is new Uint32;
SDL_SWSURFACE   : constant SDL_Surface_Flag := 16#00000000#;
SDL_HWSURFACE   : constant SDL_Surface_Flag := 16#00000001#;
SDL_ASYNCBLIT   : constant SDL_Surface_Flag := 16#00000004#;
SDL_ANYFORMAT   : constant SDL_Surface_Flag := 16#10000000#;
SDL_HWPALETTE   : constant SDL_Surface_Flag := 16#20000000#;
SDL_DOUBLEBUF   : constant SDL_Surface_Flag := 16#40000000#;
SDL_FULLSCREEN  : constant SDL_Surface_Flag := 16#80000000#;
SDL_OPENGL      : constant SDL_Surface_Flag := 16#00000002#;
SDL_OPENGLBLIT  : constant SDL_Surface_Flag := 16#0000000A#;
SDL_RESIZABLE   : constant SDL_Surface_Flag := 16#00000010#;
SDL_NOFRAME     : constant SDL_Surface_Flag := 16#00000020#;
SDL_HWACCEL     : constant SDL_Surface_Flag := 16#00000100#;
SDL_SRCCOLORKEY : constant SDL_Surface_Flag := 16#00001000#;
SDL_RLEACCELOK  : constant SDL_Surface_Flag := 16#00002000#;
SDL_RLEACCEL    : constant SDL_Surface_Flag := 16#00004000#;
SDL_SRCALPHA    : constant SDL_Surface_Flag := 16#00010000#;
SDL_PREALLOC    : constant SDL_Surface_Flag := 16#01000000#;

pragma warnings( off );
type SDL_Pixels is array( natural range <> ) of Uint8;
package SDL_Pixels_Conv is new
     system.address_to_access_conversions( SDL_Pixels );
subtype SDL_Pixels_Ptr is SDL_Pixels_Conv.object_pointer;
package SDL_Pixels_Ptr_Conv is new
     system.address_to_access_conversions( SDL_Pixels_Ptr );
subtype SDL_Pixels_Handle is SDL_Pixels_Ptr_Conv.object_pointer;
-- we don't know that the pixels are 8-bits but use Uint8--it's the smallest.
-- use unchecked_conversion, etc to access the pixels
pragma warnings( on );

type SDL_Surface is record
        flags     : SDL_Surface_Flag;
        format    : System.Address;
        w, h      : int;
        pitch     : Uint16;
        pixels    : System.Address;
        offset    : int;
        hwdata    : System.Address;
        clip_rect : SDL_Rect;
        unused1   : Uint32;
        locked    : Uint32;
        map       : Private_Ptr;
        format_version : unsigned;
        refcount  : int;
end record;
pragma pack( SDL_Surface );

package SDL_Surface_Conv is new
     system.address_to_access_conversions( SDL_Surface );
subtype SDL_Surface_Ptr is SDL_Surface_Conv.object_pointer;

type SDL_VideoInfo is record
        hw_available  : boolean;
        wm_available  : boolean;
        UnusedBits1_1 : boolean;
        UnusedBits1_2 : boolean;
        UnusedBits1_3 : boolean;
        UnusedBits1_4 : boolean;
        UnusedBits1_5 : boolean;
        UnusedBits1_6 : boolean;
        UnusedBits2   : boolean;
        blit_hw       : boolean;
        blit_hw_CC    : boolean;
        blit_hw_A     : boolean;
        blit_sw       : boolean;
        blit_sw_CC    : boolean;
        blit_sw_A     : boolean;
        blit_fill     : boolean;
        UnusedBits3   : Uint16;
        video_mem     : Uint32;
        vfmt          : System.Address;
end record;
pragma pack( SDL_VideoInfo );
-- this may only pack booleans to bytes, not bits.  investigate.

package SDL_VideoInfo_Conv is new
     system.address_to_access_conversions( SDL_VideoInfo );
subtype SDL_VideoInfo_Ptr is SDL_VideoInfo_Conv.object_pointer;

type SDL_Overlay_Type is new Uint32;
SDL_YV12_OVERLAY : constant SDL_Overlay_type := 16#32315659#;
SDL_IYUV_OVERLAY : constant SDL_Overlay_type := 16#56555949#;
SDL_YUY2_OVERLAY : constant SDL_Overlay_type := 16#32595559#;
SDL_UYVY_OVERLAY : constant SDL_Overlay_type := 16#59565955#;
SDL_YVYU_OVERLAY : constant SDL_Overlay_type := 16#55595659#;

pragma warnings( off );
type SDL_Pitches is array( natural range <> ) of Uint16;
package SDL_Pitches_Conv is new
     system.address_to_access_conversions( SDL_Pitches );
subtype SDL_Pitches_Ptr is SDL_Pitches_Conv.object_pointer;
pragma warnings( on );
-- may not be Uint8 but we'll use this as the base.

type SDL_Overlay is record
        format : SDL_Overlay_Type;
        w, h   : int;
        planes : int;
        pitches: System.Address;
        pixels : System.Address;
        hwfuncs: Private_Ptr;
        hwdata : Private_Ptr;
        hw_overlay : Uint32; -- only bit 1 used
end record;
pragma pack( SDL_Overlay );

package SDL_Overlay_Conv is new
     system.address_to_access_conversions( SDL_Overlay );
subtype SDL_Overlay_Ptr is SDL_Overlay_Conv.object_pointer;

type SDL_GLattr is (
    SDL_GL_RED_SIZE,
    SDL_GL_GREEN_SIZE,
    SDL_GL_BLUE_SIZE,
    SDL_GL_ALPHA_SIZE,
    SDL_GL_BUFFER_SIZE,
    SDL_GL_DOUBLEBUFFER,
    SDL_GL_DEPTH_SIZE,
    SDL_GL_STENCIL_SIZE,
    SDL_GL_ACCUM_RED_SIZE,
    SDL_GL_ACCUM_GREEN_SIZE,
    SDL_GL_ACCUM_BLUE_SIZE,
    SDL_GL_ACCUM_ALPHA_SIZE,
    SDL_GL_STEREO );
pragma convention( C, SDL_GLattr );

type SDL_Palette_Type is new Uint32;
SDL_LOGPAL  : constant SDL_Palette_Type := 16#01#;
SDL_PHYSPAL : constant SDL_Palette_Type := 16#02#;

procedure SDL_VideoDriverName( result : out private_ptr; nameBuff : in out C_string; maxLen : int );
pragma import( C, SDL_VideoDriverName, "SDL_VideoDriverName" );
pragma import_valued_procedure( SDL_VideoDriverName );

function SDL_GetVideoSurface return System.Address;
pragma import( C, SDL_GetVideoSurface, "SDL_GetVideoSurface" );

function SDL_GetVideoInfo return System.Address;
pragma import( C, SDL_GetVideoInfo, "SDL_GetVideoInfo" );

function SDL_VideoModeOK( width, height, bpp : int; flags : System.Address) return int;
pragma import( C, SDL_VideoModeOK, "SDL_VideoModeOK" );

procedure SDL_ListModes( result : out System.Address; format : in out SDL_PixelFormat; flags : SDL_Surface_Flag );
pragma import( C, SDL_ListModes, "SDL_ListModes" );
pragma import_valued_procedure( SDL_ListModes );
-- result maybe should be a handle, not a pointer

function SDL_SetVideoMode( width, height, bpp : int; flags : SDL_Surface_Flag) return System.Address;
pragma import( C, SDL_SetVideoMode, "SDL_SetVideoMode" );

procedure SDL_UpdateRects( screen : System.Address;  numrects : int; rects : System.Address );
pragma import( C, SDL_UpdateRects, "SDL_UpdateRects" );
-- really, a pointer to a rect array

procedure SDL_UpdateRect( screen : System.Address; x : SDL_HCoordinate; y : SDL_VCoordinate; w, h : Uint32);
pragma import( C, SDL_UpdateRect, "SDL_UpdateRect" );

function SDL_Flip( screen : System.Address ) return SDL_success;
pragma import( C, SDL_Flip, "SDL_Flip" );

function SDL_SetGamma( red, green, blue : float ) return SDL_success;
pragma import( C, SDL_SetGamma, "SDL_SetGamma" );

type SDL_Gamma_Ramp is new Uint16;
package SDL_Gamma_Conv is new
     system.address_to_access_conversions( SDL_Gamma_Ramp );
subtype SDL_Gamma_Ramp_Ptr is SDL_Gamma_Conv.object_pointer;
SDL_Gamma_Ramp_Null : constant SYSTEM.Address := SDL_Gamma_Conv.To_Address( null );

procedure SDL_SetGammaRamp( result : out SDL_success; red, green, blue : System.Address );
pragma import( C, SDL_SetGammaRamp, "SDL_SetGammaRamp" );
pragma import_valued_procedure( SDL_SetGammaRamp );

procedure SDL_GetGammaRamp( result : out SDL_success; red, green, blue : System.Address );
pragma import( C, SDL_GetGammaRamp, "SDL_GetGammaRamp" );
pragma import_valued_procedure( SDL_GetGammaRamp );

function SDL_SetColors( surface : System.Address; colors : System.Address; firstcolor, ncolors : int ) return SDL_success;
pragma import( C, SDL_SetColors, "SDL_SetColors" );

function SDL_SetPalette( surface : System.Address; flags : SDL_Palette_Type; colors : System.Address; firstcolor, ncolors : int) return SDL_success;
pragma import( C, SDL_SetPalette, "SDL_SetPalette" );

procedure SDL_MapRGB( result : out SDL_Generic_Pixel; format : System.Address; r, g, b : SDL_RGB_Component );
pragma import( C, SDL_MapRGB, "SDL_MapRGB" );
pragma import_valued_procedure( SDL_MapRGB );

procedure SDL_MapRGBA( result : out SDL_Generic_Pixel; format : System.Address; r, g, b, a : SDL_RGB_Component );
pragma import( C, SDL_MapRGBA, "SDL_MapRGBA" );

procedure SDL_GetRGB( pixel : SDL_Generic_Pixel; fmt : System.Address; r, g, b : in out SDL_RGB_Component );
pragma import( C, SDL_GetRGB, "SDL_GetRGB" );

procedure SDL_GetRGBA( pixel : SDL_Generic_Pixel; fmt : System.Address; r, g, b, a : in out SDL_RGB_Component );
pragma import( C, SDL_GetRGBA, "SDL_GetRGBA" );

function SDL_CreateRGBSurface( flags : SDL_Surface_Flag; width, height, depth : int; Rmask, Gmask, Bmask, Amask : SDL_RGB_Mask ) return System.Address;
pragma import( C, SDL_CreateRGBSurface, "SDL_CreateRGBSurface" );

function SDL_CreateRGBSurfaceFrom( pixels : System.Address; width, height, depth, pitch : int; Rmask, Gmask, Bmask, Amask : SDL_RGB_Mask ) return System.Address;
pragma import( C, SDL_CreateRGBSurfaceFrom, "SDL_CreateRGBSurfaceFrom" );

procedure SDL_FreeSurface( surface : System.Address );
pragma import( C, SDL_FreeSurface, "SDL_FreeSurface" );

function SDL_LockSurface( surface : System.Address ) return SDL_success;
pragma import( C, SDL_LockSurface, "SDL_LockSurface" );

procedure SDL_UnlockSurface( surface : System.Address );
pragma import( C, SDL_UnlockSurface, "SDL_UnlockSurface" );

function SDL_LoadBMP_RW( src : System.Address; freesrc : int ) return System.Address;
pragma import( C, SDL_LoadBMP_RW, "SDL_LoadBMP_RW" );

function SDL_SaveBMP_RW( surface : System.Address; dst : System.Address; freedst : SDL_Success ) return SDL_Success;
pragma import( C, SDL_SaveBMP_RW, "SDL_SaveBMP_RW" );

function SDL_SetColorKey( surface : System.Address; flag : SDL_Surface_Flag; key : SDL_Generic_Pixel ) return SDL_Success;
pragma import( C, SDL_SetColorKey, "SDL_SetColorKey" );

function SDL_SetAlpha( surface : System.Address; flag : SDL_Surface_Flag; alpha : SDL_RGB_Component ) return SDL_Success;
pragma import( C, SDL_SetAlpha, "SDL_SetAlpha" );

--procedure SDL_SetClipRect( res : out SDL_Bool; surface : System.Address; rect : in out SDL_Rect );
procedure SDL_SetClipRect( surface : System.Address; rect : in out SDL_Rect );
pragma import( C, SDL_SetClipRect, "SDL_SetClipRect" );
--pragma import_valued_procedure( SDL_SetClipRect );

procedure SDL_GetClipRect( surface : System.Address; rect : in out SDL_Rect);
pragma import( C, SDL_GetClipRect, "SDL_GetClipRect" );

function SDL_ConvertSurface( src : System.Address; fmt : System.Address; flags : SDL_Surface_Flag ) return System.Address;
pragma import( C, SDL_ConvertSurface, "SDL_ConvertSurface" );

-- function SDL_UpperBlit( src : System.Address; srcrect : System.Address; dst : System.Address; dstrect : System.Address ) return SDL_Success;
function SDL_UpperBlit( src : System.Address; srcrect : SDL_Rect; dst : System.Address; dstrect : SDL_Rect ) return SDL_Success;
pragma import( C, SDL_UpperBlit, "SDL_UpperBlit" );

function SDL_LowerBlit( src : System.Address; srcrect : System.Address; dst : System.Address; dstrect : System.Address ) return SDL_Success;
pragma import( C, SDL_LowerBlit, "SDL_LowerBlit" );

function SDL_FillRect( dst : System.Address; dstrect : System.Address; color : SDL_Generic_Pixel ) return SDL_Success;
pragma import( C, SDL_FillRect, "SDL_FillRect" );

function SDL_DisplayFormat( surface : System.Address ) return System.Address;
pragma import( C, SDL_DisplayFormat, "SDL_DisplayFormat" );

function SDL_DisplayFormatAlpha( surface : System.Address ) return System.Address;
pragma import( C, SDL_DisplayFormatAlpha, "SDL_DisplayFormatAlpha" );

-- YUV Overlays

function SDL_CreateYUVOverlay( width, height : int; format : SDL_Overlay_Type; display : System.Address ) return System.Address;
pragma import( C, SDL_CreateYUVOverlay, "SDL_CreateYUVOverlay" );

function SDL_LockYUVOverlay( Overlay : System.Address) return SDL_Success;
pragma import( C, SDL_LockYUVOverlay, "SDL_LockYUVOverlay" );

procedure SDL_UnlockYUVOverlay( Overall : System.Address);
pragma import( C, SDL_UnlockYUVOverlay, "SDL_UnlockYUVOverlay" );

function SDL_DisplayYUVOverlay( Overlay : System.Address; DstRect : System.Address ) return SDL_success;
pragma import( C, SDL_DisplayYUVOverlay, "SDL_DisplayYUVOverlay" );

procedure SDL_FreeYUVOverlay( Overlay : System.Address );
pragma import( C, SDL_FreeYUVOverlay, "SDL_FreeYUVOverlay" );

-- Open/GL Interface

function SDL_GL_LoadLibrary( path : C_string ) return SDL_Success;
pragma import( C, SDL_GL_LoadLibrary, "SDL_GL_LoadLibrary" );

function SDL_GL_GetProcAddress( proc : C_string) return Private_Ptr;
pragma import( C, SDL_GL_GetProcAddress, "SDL_GL_GetProcAddress" );

function SDL_GL_SetAttribute( attr : SDL_GLattr; value : int) return SDL_Success;
pragma import( C, SDL_GL_SetAttribute, "SDL_GL_SetAttribute" );

procedure SDL_GL_GetAttribute( result : out SDL_Success; attr : SDL_GLattr; value : in out int);
pragma import( C, SDL_GL_GetAttribute, "SDL_GL_GetAttribute" );
pragma import_valued_procedure( SDL_GL_GetAttribute );

procedure SDL_GL_SwapBuffers;
pragma import( C, SDL_GL_SwapBuffers, "SDL_GL_SwapBuffers" );

procedure SDL_GL_UpdateRects( numrects : int; rects : System.Address );
pragma import( C, SDL_GL_UpdateRects, "SDL_GL_UpdateRects" );

procedure SDL_GL_Lock;
pragma import( C, SDL_GL_Lock, "SDL_GL_Lock" );

procedure SDL_GL_Unlock;
pragma import( C,  SDL_GL_Unlock, "SDL_GL_Unlock" );

-- Window Manager Functions

procedure SDL_WM_SetCaption( title : C_String; icon : Private_Ptr);
pragma import( C, SDL_WM_SetCaption, "SDL_WM_SetCaption" );

procedure SDL_WM_GetCaption( title : in out C_String; icon : in out Private_Ptr );
pragma import( C, SDL_WM_GetCaption, "SDL_WM_GetCaption" );

procedure SDL_WM_SetIcon(icon : System.Address; mask : SDL_RGB_Component );
pragma import( C, SDL_WM_SetIcon );

function SDL_WM_IconifyWindow return SDL_success;
pragma import( C, SDL_WM_IconifyWindow, "SDL_WM_IconifyWindow" );

function SDL_WM_ToggleFullScreen( surface : System.Address ) return SDL_success;
pragma import( C, SDL_WM_ToggleFullScreen, "SDL_WM_ToggleFullScreen" );

type SDL_GrabMode is new int;
SDL_GRAB_QUERY      : constant SDL_GrabMode := -1;
SDL_GRAB_OFF        : constant SDL_GrabMode := 0;
SDL_GRAB_ON         : constant SDL_GrabMode := 1;
SDL_GRAB_FULLSCREEN : constant SDL_GrabMode := 2;

function SDL_WM_GrabInput(mode : SDL_GrabMode) return SDL_GrabMode;
pragma import( C, SDL_WM_GrabInput, "SDL_WM_GrabInput" );

-----------------------------------------------------------------------------
-- SDL_rwops.h
-----------------------------------------------------------------------------

type SDL_RWops_data is null record;

-- type SDL_RWops_data is record
--      case Uint32 is -- THIS IS NOT RIGHT
--      when 0 => autoclose : integer;
--                c_file : system_address;
--      when 1 => base : system.address;
--                here : system.address;
--                stop : system.address;
--      when others => data1 : system.address;
--      end case;
-- end record;

type SDL_RWops is record
     seek_callback : system.address;
     read_callback : system.address;
     write_callback : system.address;
     close_callback : system.address;
     file_type : Uint32;
     data : SDL_RWops_data;
end record;

-----------------------------------------------------------------------------

--function SDL_GetError return string;
--pragma import( C, SDL_GetError, "SDL_GetError" );

-----------------------------------------------------------------------------
-- GET SDL ERROR
--
-- Return the last SDL error message
-----------------------------------------------------------------------------

function get_sdl_error return unbounded_string;

-----------------------------------------------------------------------------
-- SDL_image.h
-----------------------------------------------------------------------------

function IMG_LoadTyped_RW( src : SDL_RWops; freesrc : int; filetype : string ) return system.address;
pragma import( C, IMG_LoadTyped_RW, "IMG_LoadTyped_RW" );

function IMG_Load( fname : string ) return system.address;
pragma import( C, IMG_Load, "IMG_Load" );

function IMG_Load_RW( src : SDL_RWops; freesrc : int ) return system.address;
pragma import( C, IMG_Load_RW, "IMG_Load_RW" );

function IMG_isBMP(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isBMP, "IMG_isBMP" );
function IMG_isPNM(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isPNM, "IMG_isPNM" );
function IMG_isXPM(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isXPM, "IMG_isXPM" );
function IMG_isXCF(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isXCF, "IMG_isXCF" );
function IMG_isPCX(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isPCX, "IMG_isPCX" );
function IMG_isGIF(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isGIF, "IMG_isGIF" );
function IMG_isJPG(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isJPG, "IMG_isJPG" );
function IMG_isTIF(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isTIF, "IMG_isTIF" );
function IMG_isPNG(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isPNG, "IMG_isPNG" );
function IMG_isLBM(src : SDL_RWops) return SDL_Success;
pragma import( C, IMG_isLBM, "IMG_isLBM" );

-----------------------------------------------------------------------------
-- SDL EXT(ensions)
--
-- These extensions are intended as drawing primitives used to created a
-- more sophisticated drawing package.
-----------------------------------------------------------------------------

-- I had problems getting system addresses out of dereferenced system
-- addresses.  These are utility function to grab system addresses from
-- SDL_Surface.

function get_pixels_address( screen : in system.address ) return
   system.address;
pragma import( C, get_pixels_address, "get_pixels_address" );

function get_format_address( screen : in system.address ) return
   system.address;
pragma import( C, get_format_address, "get_format_address" );

---> SDL Extension Data Structures

--->  PEN MODES
--
-- Not all modes are available for SDL_EXT_ routines.  Some modes are not
-- available in all pixel formats.

type aPenMode is (
     invert,                                          -- bit-wise XOR
     add,                                             -- add components
     subtract,                                        -- subtract components
     average,                                         -- average components
     copy,                                            -- overwrite (normal)
     off                                              -- invisible
);
pragma convention( C, aPenMode );
-- may remove add and subtract and do it at a higher level


---> SDL EXT Plot
--
-- Set a single pixel in any SDL video mode.  There is no clipping and no
-- pixel format conversion.  Surface must be locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_Plot(
   screen : in system.address;                       -- target SDL surface
   x      : SDL_HCoordinate;                         -- horizontal
   y      : SDL_VCoordinate;                         -- vertical
   colour : SDL_Generic_Pixel;                       -- the raw pixel
   mode   : aPenMode );                              -- special effects
pragma import( C, SDL_EXT_plot, "SDL_EXT_plot" );    -- in c_os.c


---> SDL EXT(ension) Raw Pixel
--
-- Get a single raw pixel in any SDL video mode.  This pixel is converted
-- to the target surface pixel format.  There is no clipping.  Surface must
-- be locked.
-----------------------------------------------------------------------------

function SDL_EXT_Raw_Pixel(
   screen,                                           -- source SDL surface
   target : in system.address;                       -- target (for format)
   x : SDL_HCoordinate;                              -- horizontal
   y : SDL_VCoordinate )                             -- vertical
   return SDL_Generic_Pixel;                         -- the raw pixel
pragma import( C, SDL_EXT_Raw_Pixel, "SDL_EXT_raw_pixel" );  -- in c_os.c


---> SDL EXT(ension) PLOT PATTERN
--
-- Set a single pixel in any SDL video mode.  There is no clipping and no
-- the surface must be locked.  Use a pattern to get the pixel.
-----------------------------------------------------------------------------

procedure SDL_EXT_Plot_Pattern( screen : in system.address;
          pixel_x : SDL_HCoordinate; pixel_y : SDL_VCoordinate;
          start_offset_x : SDL_HCoordinate; start_offset_y : SDL_VCoordinate;
          pattern : system.address; mode : aPenMode );


---> SDL EXT(ension) Pixel
--
-- Get the RGB values for a pixel in any SDL video mode.  There is no
-- clipping.  Surface must be locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_Pixel(
   screen : in system.address;                       -- target SDL surface
   x      : SDL_HCoordinate;                         -- horizontal
   y      : SDL_VCoordinate;                         -- vertical
   Red    : in out SDL_RGB_Component;                -- red 0..255
   Green  : in out SDL_RGB_Component;                -- green 0..255
   Blue   : in out SDL_RGB_Component );              -- blue 0..255
pragma import( C, SDL_EXT_Pixel, "SDL_EXT_pixel" );  -- in c_os.c


---> SDL EXT(ension) HLine
--
-- Draw a horizontal line in any SDL video mode.  There is no clipping and no
-- pixel format conversion.  x2 must be greater than x.  Surface must be
-- locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_HLine(
   screen : in system.address;                       -- target SDL surface
   x, x2  : SDL_HCoordinate;                         -- horizontal range
   y      : SDL_VCoordinate;                         -- vertical
   colour : SDL_Generic_Pixel;                       -- the raw pixel
   mode   : aPenMode );                              -- special effects
pragma import( C, SDL_EXT_HLine, "SDL_EXT_hline" );  -- in c_os.c


---> SDL EXT(ension) HLine Pattern
--
-- Draw a horizontal line using a pattern in any SDL video mode.  There is
-- no clipping.  Surface must be locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_HLine_Pattern(
   screen  : in system.address;                      -- target SDL surface
   x1, x2  : SDL_HCoordinate;                        -- horizontal range
   y       : SDL_VCoordinate;                        -- vertical
   pattern : in system.address;                      -- pattern surface
   start_offset_x : SDL_HCoordinate;                 -- pattern x offset
   start_offset_y : SDL_VCoordinate;                 -- pattern y offset
   mode    : aPenMode );


---> SDL EXT(ension) VLine
--
-- Draw a horizontal line in any SDL video mode.  There is no clipping and no
-- pixel format conversion.  x2 must be greater than x.  Surface must be
-- locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_VLine(
   screen : in system.address;                       -- target SDL surface
   x      : SDL_HCoordinate;                         -- horizontal range
   y1, y2 : SDL_VCoordinate;                         -- vertical
   colour : SDL_Generic_Pixel;                       -- the raw pixel
   mode   : aPenMode );                              -- special effects
pragma import( C, SDL_EXT_VLine, "SDL_EXT_vline" );  -- in c_os.c


---> SDL EXT(ension) Line
--
-- Draw a line between two points.  There is no clipping.  Surface must be
-- locked.
-----------------------------------------------------------------------------

procedure SDL_Ext_Line(
   screen  : in system.address;                      -- target SDL surface
   start_x : SDL_HCoordinate;                        -- start point (x,y)
   start_y : SDL_VCoordinate;
   end_x   : SDL_HCoordinate;                        -- end point (x,y)
   end_y   : SDL_VCoordinate;
   pixel_colour : SDL_Generic_Pixel;                 -- the raw pixel
   mode    : aPenMode;                               -- special effects
   mustReveal : boolean := true                      -- call UpdateRect
 );


---> SDL EXT(ension) Frame Rect
--
-- Draw an outline of a rectangle between 2 corner points.  There is no
-- clipping.  Surface must be locked.
-----------------------------------------------------------------------------

procedure SDL_Ext_Frame_Rect(
   screen  : in system.address;                      -- target SDL surface
   start_x : SDL_HCoordinate;                        -- left
   start_y : SDL_VCoordinate;                        -- top
   end_x   : SDL_HCoordinate;                        -- right
   end_y   : SDL_VCoordinate;                        -- bottom
   pixel_colour : SDL_Generic_Pixel;                 -- the raw pixel
   mode    : aPenMode );                             -- special effects


---> SDL EXT(ension) Fill Rect
--
-- Draw a filled in rectangle.  Unlike SDL_FillRect, support pen modes.
-- There is no clipping.  Surface must be locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_Fill_Rect(
   screen  : in system.address;                      -- target SDL surface
   start_x : SDL_HCoordinate;                        -- left
   start_y : SDL_VCoordinate;                        -- top
   end_x   : SDL_HCoordinate;                        -- right
   end_y   : SDL_VCoordinate;                        -- bottom
   pixel_colour : SDL_Generic_Pixel;                 -- the raw pixel
   mode   : aPenMode );                              -- special effects


---> SDL EXT(ension) Copy Fill Rect Pattern
--
-- Draw a filled in rectangle with a pattern between 2 corner points.  No
-- clipping.  Surface must be unlocked.  Makes use of hardware blitting.
-----------------------------------------------------------------------------

procedure SDL_EXT_Copy_Fill_Rect_Pattern(
   screen : in system.address;                       -- target SDL surface
   start_x : SDL_HCoordinate;                        -- left
   start_y : SDL_VCoordinate;                        -- top
   end_x : SDL_HCoordinate;                          -- right
   end_y : SDL_VCoordinate;                          -- bottom
   pattern : system.address );                       -- pattern surface


---> SDL EXT(ension) Fill Rect Pattern
--
-- Draw a filled in rectangle with a pattern between 2 corner points.  No
-- clipping.  Surface must be locked unless mode copy.
-----------------------------------------------------------------------------

procedure SDL_EXT_Fill_Rect_Pattern(
   screen   : in system.address;                     -- target SDL surface
   start_x  : SDL_HCoordinate;                       -- left
   start_y  : SDL_VCoordinate;                       -- top
   end_x    : SDL_HCoordinate;                       -- right
   end_y    : SDL_VCoordinate;                       -- bottom
   start_offset_x : SDL_HCoordinate;                 -- pattern x offset
   start_offset_y : SDL_VCoordinate;                 -- pattern y offset
   pattern  : system.address;                        -- pattern surface
   mode : aPenMode );                                -- special effects


---> SDL EXT(ension) Frame Ellipse
--
-- Draw an outline of a ellipse between 2 corner points.  There is no
-- clipping.  Surface must be locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_Frame_Ellipse(
   screen  : in system.address;                      -- target SDL surface
   start_x : SDL_HCoordinate;                        -- left
   start_y : SDL_VCoordinate;                        -- top
   end_x   : SDL_HCoordinate;                        -- right
   end_y   : SDL_VCoordinate;                        -- bottom
   pixel_colour : SDL_Generic_Pixel;                 -- the raw pixel
   mode    : aPenMode );                             -- special effects


---> SDL EXT(ension) Frame Ellipse with Pattern
--
-- Draw an outline of a ellipse between 2 corner points.  There is no
-- clipping.  Surface must be locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_Frame_Ellipse_Pattern( screen : in system.address;
                start_x : SDL_HCoordinate; start_y : SDL_VCoordinate;
                end_x : SDL_HCoordinate; end_y : SDL_VCoordinate;
                start_offset_x : SDL_HCoordinate; start_offset_y : SDL_VCoordinate;
                pattern : system.address; mode : aPenMode );


---> SDL EXT(ension) Fill Ellipse
--
-- Draw a filled in ellipse between 2 corner points.  There is no clipping.
-- Surface must be locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_Fill_Ellipse(
   screen  : in system.address;                      -- target SDL surface
   start_x : SDL_HCoordinate;                        -- left
   start_y : SDL_VCoordinate;                        -- top
   end_x   : SDL_HCoordinate;                        -- right
   end_y   : SDL_VCoordinate;                        -- bottom
   pixel_colour : SDL_Generic_Pixel;                 -- the raw pixel
   mode    : aPenMode );                             -- special effects


---> SDL EXT(ension) Fill Ellipse Pattern
--
-- Draw a filled in ellipse with a pattern between 2 corner points.  There
-- is no clipping.  Surface must be locked.
-----------------------------------------------------------------------------

procedure SDL_EXT_Fill_Ellipse_Pattern(
  screen  : in system.address;                       -- target SDL surface
  start_x : SDL_HCoordinate;                         -- left
  start_y : SDL_VCoordinate;                         -- top
  end_x   : SDL_HCoordinate;                         -- right
  end_y   : SDL_VCoordinate;                         -- bottom
  start_offset_x : SDL_HCoordinate;                  -- pattern x offset
  start_offset_y : SDL_VCoordinate;                  -- pattern y offset
  pattern : system.address;                          -- pattern surface
  mode    : aPenMode );                              -- special effects

---> SDL EXT(ension) WINDOW TITLE
--
-- Set window title without an icon.  Easier to do this from C than Ada
-- since we need a null C pointer.  s is a C string (use a ASCII.NUL)
-----------------------------------------------------------------------------

procedure SDL_EXT_Window_Title( s : string );
pragma import( C, SDL_EXT_Window_Title, "SDL_EXT_window_title" );

procedure SDL_EXT_Get_Pixel_Masks( res : Interfaces.C.int; Rmask, Gmask, Bmask, Amask : out SDL_RGB_Mask );
pragma import( C, SDL_EXT_Get_Pixel_Masks, "SDL_EXT_get_pixel_masks" );

function SDL_EXT_Save_BMP( s : system.address; path : string ) return SDL_Success;
pragma import( C, SDL_EXT_Save_BMP, "SDL_EXT_save_bmp" );


-----------------------------------------------------------------------------
-- SDL TTF
-----------------------------------------------------------------------------


type TTF_STYLE is new int;
TTF_STYLE_NORMAL    : TTF_STYLE := 0;
TTF_STYLE_BOLD      : TTF_STYLE := 1;
TTF_STYLE_ITALIC    : TTF_STYLE := 2;
TTF_STYLE_UNDERLINE : TTF_STYLE := 4;

type TTF_font is new system.address;

function TTF_Init return SDL_success;
pragma import( C, TTF_Init, "TTF_Init" );

function TTF_OpenFont( path : string; point_size : int ) return TTF_Font;
pragma import( C, TTF_OpenFont, "TTF_OpenFont" );

procedure TTF_CloseFont( font : TTF_Font );
pragma import( C, TTF_CloseFont, "TTF_CloseFont" );

function TTF_RenderText_Solid(font : TTF_Font; text : string; fg : SDL_Generic_Pixel ) return system.address; -- returns a surface
pragma import( C, TTF_RenderText_Solid, "TTF_RenderText_Solid" );

end spar_os.sdl;

