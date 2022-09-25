pragma Ada_2012;
package body spar_os.sdl is

   --------------
   -- SDL_Init --
   --------------

   function SDL_Init (flags : SDL_Init_Flags) return integer is
   begin
      pragma Compile_Time_Warning (Standard.True, "SDL_Init unimplemented");
      return raise Program_Error with "Unimplemented function SDL_Init";
   end SDL_Init;

   -----------------
   -- SDL_WasInit --
   -----------------

   function SDL_WasInit (flags : SDL_Init_Flags) return SDL_Init_Flags is
   begin
      pragma Compile_Time_Warning (Standard.True, "SDL_WasInit unimplemented");
      return raise Program_Error with "Unimplemented function SDL_WasInit";
   end SDL_WasInit;

   --------------
   -- SDL_Quit --
   --------------

   procedure SDL_Quit is
   begin
      pragma Compile_Time_Warning (Standard.True, "SDL_Quit unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_Quit";
   end SDL_Quit;

   -------------------------
   -- SDL_VideoDriverName --
   -------------------------

   procedure SDL_VideoDriverName
     (result : out private_ptr; nameBuff : in out C_string; maxLen : int)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_VideoDriverName unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_VideoDriverName";
   end SDL_VideoDriverName;

   -------------------------
   -- SDL_GetVideoSurface --
   -------------------------

   function SDL_GetVideoSurface return System.Address is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GetVideoSurface unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_GetVideoSurface";
   end SDL_GetVideoSurface;

   ----------------------
   -- SDL_GetVideoInfo --
   ----------------------

   function SDL_GetVideoInfo return System.Address is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GetVideoInfo unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_GetVideoInfo";
   end SDL_GetVideoInfo;

   ---------------------
   -- SDL_VideoModeOK --
   ---------------------

   function SDL_VideoModeOK
     (width, height, bpp : int; flags : System.Address) return int
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_VideoModeOK unimplemented");
      return raise Program_Error with "Unimplemented function SDL_VideoModeOK";
   end SDL_VideoModeOK;

   -------------------
   -- SDL_ListModes --
   -------------------

   procedure SDL_ListModes
     (result : out System.Address; format : in out SDL_PixelFormat;
      flags  :     SDL_Surface_Flag)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_ListModes unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_ListModes";
   end SDL_ListModes;

   ----------------------
   -- SDL_SetVideoMode --
   ----------------------

   function SDL_SetVideoMode
     (width, height, bpp : int; flags : SDL_Surface_Flag) return System.Address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_SetVideoMode unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_SetVideoMode";
   end SDL_SetVideoMode;

   ---------------------
   -- SDL_UpdateRects --
   ---------------------

   procedure SDL_UpdateRects
     (screen : System.Address; numrects : int; rects : System.Address)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_UpdateRects unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_UpdateRects";
   end SDL_UpdateRects;

   --------------------
   -- SDL_UpdateRect --
   --------------------

   procedure SDL_UpdateRect
     (screen : System.Address; x : SDL_HCoordinate; y : SDL_VCoordinate;
      w, h   : Uint32)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_UpdateRect unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_UpdateRect";
   end SDL_UpdateRect;

   --------------
   -- SDL_Flip --
   --------------

   function SDL_Flip (screen : System.Address) return SDL_success is
   begin
      pragma Compile_Time_Warning (Standard.True, "SDL_Flip unimplemented");
      return raise Program_Error with "Unimplemented function SDL_Flip";
   end SDL_Flip;

   ------------------
   -- SDL_SetGamma --
   ------------------

   function SDL_SetGamma (red, green, blue : float) return SDL_success is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_SetGamma unimplemented");
      return raise Program_Error with "Unimplemented function SDL_SetGamma";
   end SDL_SetGamma;

   ----------------------
   -- SDL_SetGammaRamp --
   ----------------------

   procedure SDL_SetGammaRamp
     (result : out SDL_success; red, green, blue : System.Address)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_SetGammaRamp unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_SetGammaRamp";
   end SDL_SetGammaRamp;

   ----------------------
   -- SDL_GetGammaRamp --
   ----------------------

   procedure SDL_GetGammaRamp
     (result : out SDL_success; red, green, blue : System.Address)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GetGammaRamp unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_GetGammaRamp";
   end SDL_GetGammaRamp;

   -------------------
   -- SDL_SetColors --
   -------------------

   function SDL_SetColors
     (surface             : System.Address; colors : System.Address;
      firstcolor, ncolors : int) return SDL_success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_SetColors unimplemented");
      return raise Program_Error with "Unimplemented function SDL_SetColors";
   end SDL_SetColors;

   --------------------
   -- SDL_SetPalette --
   --------------------

   function SDL_SetPalette
     (surface : System.Address; flags : SDL_Palette_Type;
      colors  : System.Address; firstcolor, ncolors : int) return SDL_success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_SetPalette unimplemented");
      return raise Program_Error with "Unimplemented function SDL_SetPalette";
   end SDL_SetPalette;

   ----------------
   -- SDL_MapRGB --
   ----------------

   procedure SDL_MapRGB
     (result  : out SDL_Generic_Pixel; format : System.Address;
      r, g, b :     SDL_RGB_Component)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "SDL_MapRGB unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_MapRGB";
   end SDL_MapRGB;

   -----------------
   -- SDL_MapRGBA --
   -----------------

   procedure SDL_MapRGBA
     (result     : out SDL_Generic_Pixel; format : System.Address;
      r, g, b, a :     SDL_RGB_Component)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "SDL_MapRGBA unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_MapRGBA";
   end SDL_MapRGBA;

   ----------------
   -- SDL_GetRGB --
   ----------------

   procedure SDL_GetRGB
     (pixel   :        SDL_Generic_Pixel; fmt : System.Address;
      r, g, b : in out SDL_RGB_Component)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "SDL_GetRGB unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_GetRGB";
   end SDL_GetRGB;

   -----------------
   -- SDL_GetRGBA --
   -----------------

   procedure SDL_GetRGBA
     (pixel      :        SDL_Generic_Pixel; fmt : System.Address;
      r, g, b, a : in out SDL_RGB_Component)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "SDL_GetRGBA unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_GetRGBA";
   end SDL_GetRGBA;

   --------------------------
   -- SDL_CreateRGBSurface --
   --------------------------

   function SDL_CreateRGBSurface
     (flags : SDL_Surface_Flag; width, height, depth : int;
      Rmask, Gmask, Bmask, Amask : SDL_RGB_Mask) return System.Address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_CreateRGBSurface unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_CreateRGBSurface";
   end SDL_CreateRGBSurface;

   ------------------------------
   -- SDL_CreateRGBSurfaceFrom --
   ------------------------------

   function SDL_CreateRGBSurfaceFrom
     (pixels : System.Address; width, height, depth, pitch : int;
      Rmask, Gmask, Bmask, Amask : SDL_RGB_Mask) return System.Address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_CreateRGBSurfaceFrom unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_CreateRGBSurfaceFrom";
   end SDL_CreateRGBSurfaceFrom;

   ---------------------
   -- SDL_FreeSurface --
   ---------------------

   procedure SDL_FreeSurface (surface : System.Address) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_FreeSurface unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_FreeSurface";
   end SDL_FreeSurface;

   ---------------------
   -- SDL_LockSurface --
   ---------------------

   function SDL_LockSurface (surface : System.Address) return SDL_success is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_LockSurface unimplemented");
      return raise Program_Error with "Unimplemented function SDL_LockSurface";
   end SDL_LockSurface;

   -----------------------
   -- SDL_UnlockSurface --
   -----------------------

   procedure SDL_UnlockSurface (surface : System.Address) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_UnlockSurface unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_UnlockSurface";
   end SDL_UnlockSurface;

   --------------------
   -- SDL_LoadBMP_RW --
   --------------------

   function SDL_LoadBMP_RW
     (src : System.Address; freesrc : int) return System.Address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_LoadBMP_RW unimplemented");
      return raise Program_Error with "Unimplemented function SDL_LoadBMP_RW";
   end SDL_LoadBMP_RW;

   --------------------
   -- SDL_SaveBMP_RW --
   --------------------

   function SDL_SaveBMP_RW
     (surface : System.Address; dst : System.Address; freedst : SDL_Success)
      return SDL_Success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_SaveBMP_RW unimplemented");
      return raise Program_Error with "Unimplemented function SDL_SaveBMP_RW";
   end SDL_SaveBMP_RW;

   ---------------------
   -- SDL_SetColorKey --
   ---------------------

   function SDL_SetColorKey
     (surface : System.Address; flag : SDL_Surface_Flag;
      key     : SDL_Generic_Pixel) return SDL_Success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_SetColorKey unimplemented");
      return raise Program_Error with "Unimplemented function SDL_SetColorKey";
   end SDL_SetColorKey;

   ------------------
   -- SDL_SetAlpha --
   ------------------

   function SDL_SetAlpha
     (surface : System.Address; flag : SDL_Surface_Flag;
      alpha   : SDL_RGB_Component) return SDL_Success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_SetAlpha unimplemented");
      return raise Program_Error with "Unimplemented function SDL_SetAlpha";
   end SDL_SetAlpha;

   ---------------------
   -- SDL_SetClipRect --
   ---------------------

   procedure SDL_SetClipRect (surface : System.Address; rect : in out SDL_Rect)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_SetClipRect unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_SetClipRect";
   end SDL_SetClipRect;

   ---------------------
   -- SDL_GetClipRect --
   ---------------------

   procedure SDL_GetClipRect (surface : System.Address; rect : in out SDL_Rect)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GetClipRect unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_GetClipRect";
   end SDL_GetClipRect;

   ------------------------
   -- SDL_ConvertSurface --
   ------------------------

   function SDL_ConvertSurface
     (src : System.Address; fmt : System.Address; flags : SDL_Surface_Flag)
      return System.Address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_ConvertSurface unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_ConvertSurface";
   end SDL_ConvertSurface;

   -------------------
   -- SDL_UpperBlit --
   -------------------

   function SDL_UpperBlit
     (src     : System.Address; srcrect : SDL_Rect; dst : System.Address;
      dstrect : SDL_Rect) return SDL_Success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_UpperBlit unimplemented");
      return raise Program_Error with "Unimplemented function SDL_UpperBlit";
   end SDL_UpperBlit;

   -------------------
   -- SDL_LowerBlit --
   -------------------

   function SDL_LowerBlit
     (src     : System.Address; srcrect : System.Address; dst : System.Address;
      dstrect : System.Address) return SDL_Success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_LowerBlit unimplemented");
      return raise Program_Error with "Unimplemented function SDL_LowerBlit";
   end SDL_LowerBlit;

   ------------------
   -- SDL_FillRect --
   ------------------

   function SDL_FillRect
     (dst   : System.Address; dstrect : System.Address;
      color : SDL_Generic_Pixel) return SDL_Success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_FillRect unimplemented");
      return raise Program_Error with "Unimplemented function SDL_FillRect";
   end SDL_FillRect;

   -----------------------
   -- SDL_DisplayFormat --
   -----------------------

   function SDL_DisplayFormat (surface : System.Address) return System.Address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_DisplayFormat unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_DisplayFormat";
   end SDL_DisplayFormat;

   ----------------------------
   -- SDL_DisplayFormatAlpha --
   ----------------------------

   function SDL_DisplayFormatAlpha
     (surface : System.Address) return System.Address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_DisplayFormatAlpha unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_DisplayFormatAlpha";
   end SDL_DisplayFormatAlpha;

   --------------------------
   -- SDL_CreateYUVOverlay --
   --------------------------

   function SDL_CreateYUVOverlay
     (width, height : int; format : SDL_Overlay_Type; display : System.Address)
      return System.Address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_CreateYUVOverlay unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_CreateYUVOverlay";
   end SDL_CreateYUVOverlay;

   ------------------------
   -- SDL_LockYUVOverlay --
   ------------------------

   function SDL_LockYUVOverlay (Overlay : System.Address) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_LockYUVOverlay unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_LockYUVOverlay";
   end SDL_LockYUVOverlay;

   --------------------------
   -- SDL_UnlockYUVOverlay --
   --------------------------

   procedure SDL_UnlockYUVOverlay (Overall : System.Address) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_UnlockYUVOverlay unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_UnlockYUVOverlay";
   end SDL_UnlockYUVOverlay;

   ---------------------------
   -- SDL_DisplayYUVOverlay --
   ---------------------------

   function SDL_DisplayYUVOverlay
     (Overlay : System.Address; DstRect : System.Address) return SDL_success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_DisplayYUVOverlay unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_DisplayYUVOverlay";
   end SDL_DisplayYUVOverlay;

   ------------------------
   -- SDL_FreeYUVOverlay --
   ------------------------

   procedure SDL_FreeYUVOverlay (Overlay : System.Address) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_FreeYUVOverlay unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_FreeYUVOverlay";
   end SDL_FreeYUVOverlay;

   ------------------------
   -- SDL_GL_LoadLibrary --
   ------------------------

   function SDL_GL_LoadLibrary (path : C_string) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GL_LoadLibrary unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_GL_LoadLibrary";
   end SDL_GL_LoadLibrary;

   ---------------------------
   -- SDL_GL_GetProcAddress --
   ---------------------------

   function SDL_GL_GetProcAddress (proc : C_string) return Private_Ptr is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GL_GetProcAddress unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_GL_GetProcAddress";
   end SDL_GL_GetProcAddress;

   -------------------------
   -- SDL_GL_SetAttribute --
   -------------------------

   function SDL_GL_SetAttribute
     (attr : SDL_GLattr; value : int) return SDL_Success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GL_SetAttribute unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_GL_SetAttribute";
   end SDL_GL_SetAttribute;

   -------------------------
   -- SDL_GL_GetAttribute --
   -------------------------

   procedure SDL_GL_GetAttribute
     (result : out SDL_Success; attr : SDL_GLattr; value : in out int)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GL_GetAttribute unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_GL_GetAttribute";
   end SDL_GL_GetAttribute;

   ------------------------
   -- SDL_GL_SwapBuffers --
   ------------------------

   procedure SDL_GL_SwapBuffers is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GL_SwapBuffers unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_GL_SwapBuffers";
   end SDL_GL_SwapBuffers;

   ------------------------
   -- SDL_GL_UpdateRects --
   ------------------------

   procedure SDL_GL_UpdateRects (numrects : int; rects : System.Address) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GL_UpdateRects unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_GL_UpdateRects";
   end SDL_GL_UpdateRects;

   -----------------
   -- SDL_GL_Lock --
   -----------------

   procedure SDL_GL_Lock is
   begin
      pragma Compile_Time_Warning (Standard.True, "SDL_GL_Lock unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_GL_Lock";
   end SDL_GL_Lock;

   -------------------
   -- SDL_GL_Unlock --
   -------------------

   procedure SDL_GL_Unlock is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_GL_Unlock unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_GL_Unlock";
   end SDL_GL_Unlock;

   -----------------------
   -- SDL_WM_SetCaption --
   -----------------------

   procedure SDL_WM_SetCaption (title : C_String; icon : Private_Ptr) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_WM_SetCaption unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_WM_SetCaption";
   end SDL_WM_SetCaption;

   -----------------------
   -- SDL_WM_GetCaption --
   -----------------------

   procedure SDL_WM_GetCaption
     (title : in out C_String; icon : in out Private_Ptr)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_WM_GetCaption unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_WM_GetCaption";
   end SDL_WM_GetCaption;

   --------------------
   -- SDL_WM_SetIcon --
   --------------------

   procedure SDL_WM_SetIcon (icon : System.Address; mask : SDL_RGB_Component)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_WM_SetIcon unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_WM_SetIcon";
   end SDL_WM_SetIcon;

   --------------------------
   -- SDL_WM_IconifyWindow --
   --------------------------

   function SDL_WM_IconifyWindow return SDL_success is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_WM_IconifyWindow unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_WM_IconifyWindow";
   end SDL_WM_IconifyWindow;

   -----------------------------
   -- SDL_WM_ToggleFullScreen --
   -----------------------------

   function SDL_WM_ToggleFullScreen
     (surface : System.Address) return SDL_success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_WM_ToggleFullScreen unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_WM_ToggleFullScreen";
   end SDL_WM_ToggleFullScreen;

   ----------------------
   -- SDL_WM_GrabInput --
   ----------------------

   function SDL_WM_GrabInput (mode : SDL_GrabMode) return SDL_GrabMode is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_WM_GrabInput unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_WM_GrabInput";
   end SDL_WM_GrabInput;

   -------------------
   -- get_sdl_error --
   -------------------

   function get_sdl_error return unbounded_string is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "get_sdl_error unimplemented");
      return raise Program_Error with "Unimplemented function get_sdl_error";
   end get_sdl_error;

   ----------------------
   -- IMG_LoadTyped_RW --
   ----------------------

   function IMG_LoadTyped_RW
     (src : SDL_RWops; freesrc : int; filetype : string) return system.address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "IMG_LoadTyped_RW unimplemented");
      return raise Program_Error
          with "Unimplemented function IMG_LoadTyped_RW";
   end IMG_LoadTyped_RW;

   --------------
   -- IMG_Load --
   --------------

   function IMG_Load (fname : string) return system.address is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_Load unimplemented");
      return raise Program_Error with "Unimplemented function IMG_Load";
   end IMG_Load;

   -----------------
   -- IMG_Load_RW --
   -----------------

   function IMG_Load_RW (src : SDL_RWops; freesrc : int) return system.address
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_Load_RW unimplemented");
      return raise Program_Error with "Unimplemented function IMG_Load_RW";
   end IMG_Load_RW;

   ---------------
   -- IMG_isBMP --
   ---------------

   function IMG_isBMP (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isBMP unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isBMP";
   end IMG_isBMP;

   ---------------
   -- IMG_isPNM --
   ---------------

   function IMG_isPNM (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isPNM unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isPNM";
   end IMG_isPNM;

   ---------------
   -- IMG_isXPM --
   ---------------

   function IMG_isXPM (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isXPM unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isXPM";
   end IMG_isXPM;

   ---------------
   -- IMG_isXCF --
   ---------------

   function IMG_isXCF (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isXCF unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isXCF";
   end IMG_isXCF;

   ---------------
   -- IMG_isPCX --
   ---------------

   function IMG_isPCX (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isPCX unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isPCX";
   end IMG_isPCX;

   ---------------
   -- IMG_isGIF --
   ---------------

   function IMG_isGIF (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isGIF unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isGIF";
   end IMG_isGIF;

   ---------------
   -- IMG_isJPG --
   ---------------

   function IMG_isJPG (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isJPG unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isJPG";
   end IMG_isJPG;

   ---------------
   -- IMG_isTIF --
   ---------------

   function IMG_isTIF (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isTIF unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isTIF";
   end IMG_isTIF;

   ---------------
   -- IMG_isPNG --
   ---------------

   function IMG_isPNG (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isPNG unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isPNG";
   end IMG_isPNG;

   ---------------
   -- IMG_isLBM --
   ---------------

   function IMG_isLBM (src : SDL_RWops) return SDL_Success is
   begin
      pragma Compile_Time_Warning (Standard.True, "IMG_isLBM unimplemented");
      return raise Program_Error with "Unimplemented function IMG_isLBM";
   end IMG_isLBM;

   ------------------------
   -- get_pixels_address --
   ------------------------

   function get_pixels_address
     (screen : in system.address) return system.address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "get_pixels_address unimplemented");
      return raise Program_Error
          with "Unimplemented function get_pixels_address";
   end get_pixels_address;

   ------------------------
   -- get_format_address --
   ------------------------

   function get_format_address
     (screen : in system.address) return system.address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "get_format_address unimplemented");
      return raise Program_Error
          with "Unimplemented function get_format_address";
   end get_format_address;

   ------------------
   -- SDL_EXT_Plot --
   ------------------

   procedure SDL_EXT_Plot
     (screen : in system.address; x : SDL_HCoordinate; y : SDL_VCoordinate;
      colour :    SDL_Generic_Pixel; mode : aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Plot unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_Plot";
   end SDL_EXT_Plot;

   -----------------------
   -- SDL_EXT_Raw_Pixel --
   -----------------------

   function SDL_EXT_Raw_Pixel
     (screen, target : in system.address; x : SDL_HCoordinate;
      y              :    SDL_VCoordinate) return SDL_Generic_Pixel
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Raw_Pixel unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_EXT_Raw_Pixel";
   end SDL_EXT_Raw_Pixel;

   --------------------------
   -- SDL_EXT_Plot_Pattern --
   --------------------------

   procedure SDL_EXT_Plot_Pattern
     (screen         : in system.address; pixel_x : SDL_HCoordinate;
      pixel_y        :    SDL_VCoordinate; start_offset_x : SDL_HCoordinate;
      start_offset_y :    SDL_VCoordinate; pattern : system.address;
      mode           :    aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Plot_Pattern unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_Plot_Pattern";
   end SDL_EXT_Plot_Pattern;

   -------------------
   -- SDL_EXT_Pixel --
   -------------------

   procedure SDL_EXT_Pixel
     (screen : in     system.address; x : SDL_HCoordinate; y : SDL_VCoordinate;
      Red    : in out SDL_RGB_Component; Green : in out SDL_RGB_Component;
      Blue   : in out SDL_RGB_Component)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Pixel unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_Pixel";
   end SDL_EXT_Pixel;

   -------------------
   -- SDL_EXT_HLine --
   -------------------

   procedure SDL_EXT_HLine
     (screen : in system.address; x, x2 : SDL_HCoordinate; y : SDL_VCoordinate;
      colour :    SDL_Generic_Pixel; mode : aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_HLine unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_HLine";
   end SDL_EXT_HLine;

   ---------------------------
   -- SDL_EXT_HLine_Pattern --
   ---------------------------

   procedure SDL_EXT_HLine_Pattern
     (screen         : in system.address; x1, x2 : SDL_HCoordinate;
      y              :    SDL_VCoordinate; pattern : in system.address;
      start_offset_x :    SDL_HCoordinate; start_offset_y : SDL_VCoordinate;
      mode           :    aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_HLine_Pattern unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_HLine_Pattern";
   end SDL_EXT_HLine_Pattern;

   -------------------
   -- SDL_EXT_VLine --
   -------------------

   procedure SDL_EXT_VLine
     (screen : in system.address; x : SDL_HCoordinate;
      y1, y2 :    SDL_VCoordinate; colour : SDL_Generic_Pixel; mode : aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_VLine unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_VLine";
   end SDL_EXT_VLine;

   ------------------
   -- SDL_Ext_Line --
   ------------------

   procedure SDL_Ext_Line
     (screen  : in system.address; start_x : SDL_HCoordinate;
      start_y :    SDL_VCoordinate; end_x : SDL_HCoordinate;
      end_y   :    SDL_VCoordinate; pixel_colour : SDL_Generic_Pixel;
      mode    :    aPenMode; mustReveal : boolean := true)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_Ext_Line unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_Ext_Line";
   end SDL_Ext_Line;

   ------------------------
   -- SDL_Ext_Frame_Rect --
   ------------------------

   procedure SDL_Ext_Frame_Rect
     (screen  : in system.address; start_x : SDL_HCoordinate;
      start_y :    SDL_VCoordinate; end_x : SDL_HCoordinate;
      end_y   :    SDL_VCoordinate; pixel_colour : SDL_Generic_Pixel;
      mode    :    aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_Ext_Frame_Rect unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_Ext_Frame_Rect";
   end SDL_Ext_Frame_Rect;

   -----------------------
   -- SDL_EXT_Fill_Rect --
   -----------------------

   procedure SDL_EXT_Fill_Rect
     (screen  : in system.address; start_x : SDL_HCoordinate;
      start_y :    SDL_VCoordinate; end_x : SDL_HCoordinate;
      end_y   :    SDL_VCoordinate; pixel_colour : SDL_Generic_Pixel;
      mode    :    aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Fill_Rect unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_Fill_Rect";
   end SDL_EXT_Fill_Rect;

   ------------------------------------
   -- SDL_EXT_Copy_Fill_Rect_Pattern --
   ------------------------------------

   procedure SDL_EXT_Copy_Fill_Rect_Pattern
     (screen  : in system.address; start_x : SDL_HCoordinate;
      start_y :    SDL_VCoordinate; end_x : SDL_HCoordinate;
      end_y   :    SDL_VCoordinate; pattern : system.address)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Copy_Fill_Rect_Pattern unimplemented");
      raise Program_Error
        with "Unimplemented procedure SDL_EXT_Copy_Fill_Rect_Pattern";
   end SDL_EXT_Copy_Fill_Rect_Pattern;

   -------------------------------
   -- SDL_EXT_Fill_Rect_Pattern --
   -------------------------------

   procedure SDL_EXT_Fill_Rect_Pattern
     (screen         : in system.address; start_x : SDL_HCoordinate;
      start_y        :    SDL_VCoordinate; end_x : SDL_HCoordinate;
      end_y          :    SDL_VCoordinate; start_offset_x : SDL_HCoordinate;
      start_offset_y :    SDL_VCoordinate; pattern : system.address;
      mode           :    aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Fill_Rect_Pattern unimplemented");
      raise Program_Error
        with "Unimplemented procedure SDL_EXT_Fill_Rect_Pattern";
   end SDL_EXT_Fill_Rect_Pattern;

   ---------------------------
   -- SDL_EXT_Frame_Ellipse --
   ---------------------------

   procedure SDL_EXT_Frame_Ellipse
     (screen  : in system.address; start_x : SDL_HCoordinate;
      start_y :    SDL_VCoordinate; end_x : SDL_HCoordinate;
      end_y   :    SDL_VCoordinate; pixel_colour : SDL_Generic_Pixel;
      mode    :    aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Frame_Ellipse unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_Frame_Ellipse";
   end SDL_EXT_Frame_Ellipse;

   -----------------------------------
   -- SDL_EXT_Frame_Ellipse_Pattern --
   -----------------------------------

   procedure SDL_EXT_Frame_Ellipse_Pattern
     (screen         : in system.address; start_x : SDL_HCoordinate;
      start_y        :    SDL_VCoordinate; end_x : SDL_HCoordinate;
      end_y          :    SDL_VCoordinate; start_offset_x : SDL_HCoordinate;
      start_offset_y :    SDL_VCoordinate; pattern : system.address;
      mode           :    aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Frame_Ellipse_Pattern unimplemented");
      raise Program_Error
        with "Unimplemented procedure SDL_EXT_Frame_Ellipse_Pattern";
   end SDL_EXT_Frame_Ellipse_Pattern;

   --------------------------
   -- SDL_EXT_Fill_Ellipse --
   --------------------------

   procedure SDL_EXT_Fill_Ellipse
     (screen  : in system.address; start_x : SDL_HCoordinate;
      start_y :    SDL_VCoordinate; end_x : SDL_HCoordinate;
      end_y   :    SDL_VCoordinate; pixel_colour : SDL_Generic_Pixel;
      mode    :    aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Fill_Ellipse unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_Fill_Ellipse";
   end SDL_EXT_Fill_Ellipse;

   ----------------------------------
   -- SDL_EXT_Fill_Ellipse_Pattern --
   ----------------------------------

   procedure SDL_EXT_Fill_Ellipse_Pattern
     (screen         : in system.address; start_x : SDL_HCoordinate;
      start_y        :    SDL_VCoordinate; end_x : SDL_HCoordinate;
      end_y          :    SDL_VCoordinate; start_offset_x : SDL_HCoordinate;
      start_offset_y :    SDL_VCoordinate; pattern : system.address;
      mode           :    aPenMode)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Fill_Ellipse_Pattern unimplemented");
      raise Program_Error
        with "Unimplemented procedure SDL_EXT_Fill_Ellipse_Pattern";
   end SDL_EXT_Fill_Ellipse_Pattern;

   --------------------------
   -- SDL_EXT_Window_Title --
   --------------------------

   procedure SDL_EXT_Window_Title (s : string) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Window_Title unimplemented");
      raise Program_Error with "Unimplemented procedure SDL_EXT_Window_Title";
   end SDL_EXT_Window_Title;

   -----------------------------
   -- SDL_EXT_Get_Pixel_Masks --
   -----------------------------

   procedure SDL_EXT_Get_Pixel_Masks
     (res : Interfaces.C.int; Rmask, Gmask, Bmask, Amask : out SDL_RGB_Mask)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Get_Pixel_Masks unimplemented");
      raise Program_Error
        with "Unimplemented procedure SDL_EXT_Get_Pixel_Masks";
   end SDL_EXT_Get_Pixel_Masks;

   ----------------------
   -- SDL_EXT_Save_BMP --
   ----------------------

   function SDL_EXT_Save_BMP
     (s : system.address; path : string) return SDL_Success
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "SDL_EXT_Save_BMP unimplemented");
      return raise Program_Error
          with "Unimplemented function SDL_EXT_Save_BMP";
   end SDL_EXT_Save_BMP;

   --------------
   -- TTF_Init --
   --------------

   function TTF_Init return SDL_success is
   begin
      pragma Compile_Time_Warning (Standard.True, "TTF_Init unimplemented");
      return raise Program_Error with "Unimplemented function TTF_Init";
   end TTF_Init;

   ------------------
   -- TTF_OpenFont --
   ------------------

   function TTF_OpenFont (path : string; point_size : int) return TTF_Font is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "TTF_OpenFont unimplemented");
      return raise Program_Error with "Unimplemented function TTF_OpenFont";
   end TTF_OpenFont;

   -------------------
   -- TTF_CloseFont --
   -------------------

   procedure TTF_CloseFont (font : TTF_Font) is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "TTF_CloseFont unimplemented");
      raise Program_Error with "Unimplemented procedure TTF_CloseFont";
   end TTF_CloseFont;

   --------------------------
   -- TTF_RenderText_Solid --
   --------------------------

   function TTF_RenderText_Solid
     (font : TTF_Font; text : string; fg : SDL_Generic_Pixel) return system
     .address
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "TTF_RenderText_Solid unimplemented");
      return raise Program_Error
          with "Unimplemented function TTF_RenderText_Solid";
   end TTF_RenderText_Solid;

end spar_os.sdl;
