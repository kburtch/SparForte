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

with ada.strings.unbounded, world;
use ada.strings.unbounded, world;

package parser_pen is

------------------------------------------------------------------------------
-- Numerics package identifiers
------------------------------------------------------------------------------

pen_rect_t          : identifier;
pen_coordinate_t    : identifier;
pen_rgbcomponent_t  : identifier;
pen_rect_left_t     : identifier;
pen_rect_top_t      : identifier;
pen_rect_right_t    : identifier;
pen_rect_bottom_t   : identifier;
pen_canvas_id_t     : identifier;

pen_pen_mode_t      : identifier;
pen_mode_invert_t   : identifier;
pen_mode_add_t      : identifier;
pen_mode_subtract_t : identifier;
pen_mode_average_t  : identifier;
pen_mode_copy_t     : identifier;
pen_mode_off_t      : identifier;

pen_pen_brush_t     : identifier;
pen_brush_undefined_t : identifier;
pen_brush_pencil_t  : identifier;
pen_brush_stretch_t : identifier;
pen_brush_tile_t    : identifier;
pen_brush_stamp_t   : identifier;
pen_brush_smear_t   : identifier;

pen_set_rect_t       : identifier;
pen_is_empty_rect_t  : identifier;
pen_offset_rect_t    : identifier;
pen_inset_rect_t     : identifier;
pen_intersect_rect_t : identifier;
pen_inside_rect_t    : identifier;
pen_in_rect_t        : identifier;

pen_set_pen_mode_t   : identifier;
pen_set_pen_ink_t    : identifier;
pen_set_pen_brush_t  : identifier;
pen_set_pen_pattern_t: identifier;
pen_get_pen_mode_t   : identifier;
pen_get_pen_ink_t    : identifier;
pen_get_pen_brush_t  : identifier;
-- pen_get_pen_pattern_t: identifier;

pen_move_to_t        : identifier;
pen_move_t           : identifier;
pen_line_to_t        : identifier;
pen_line_t           : identifier;
pen_hline_t          : identifier;
pen_vline_t          : identifier;

pen_frame_rect_t     : identifier;
pen_paint_rect_t     : identifier;
pen_fill_rect_t      : identifier;
pen_frame_ellipse_t  : identifier;
pen_fill_ellipse_t   : identifier;
pen_clear_t          : identifier;

pen_new_screen_canvas_t : identifier;
pen_new_gl_screen_canvas_t : identifier;
pen_new_window_canvas_t : identifier;
pen_new_gl_window_canvas_t : identifier;
pen_new_canvas_t     : identifier;
pen_save_canvas_t    : identifier;
pen_set_title_t      : identifier;
pen_close_canvas_t   : identifier;

pen_wait_to_reveal_t : identifier;
pen_reveal_t         : identifier;
pen_reveal_now_t     : identifier;

pen_clip_rect_t      : identifier;

pen_greyscale_t      : identifier;
pen_blend_t          : identifier;
pen_fade_t           : identifier;

pen_plot_t           : identifier;

pen_pen_color_name_t : identifier;
color_name_aliceblue_t : identifier;
color_name_antiquewhite_t : identifier;
color_name_antiquewhite1_t : identifier;
color_name_antiquewhite2_t : identifier;
color_name_antiquewhite3_t : identifier;
color_name_antiquewhite4_t : identifier;
color_name_aquamarine_t : identifier;
color_name_aquamarine1_t : identifier;
color_name_aquamarine2_t : identifier;
color_name_aquamarine3_t : identifier;
color_name_aquamarine4_t : identifier;
color_name_azure_t : identifier;
color_name_azure1_t : identifier;
color_name_azure2_t : identifier;
color_name_azure3_t : identifier;
color_name_azure4_t : identifier;
color_name_beige_t : identifier;
color_name_bisque_t : identifier;
color_name_bisque1_t : identifier;
color_name_bisque2_t : identifier;
color_name_bisque3_t : identifier;
color_name_bisque4_t : identifier;
color_name_black_t : identifier;
color_name_blanchedalmond_t : identifier;
color_name_blue_t : identifier;
color_name_blue1_t : identifier;
color_name_blue2_t : identifier;
color_name_blue3_t : identifier;
color_name_blue4_t : identifier;
color_name_blueviolet_t : identifier;
color_name_brown_t : identifier;
color_name_brown1_t : identifier;
color_name_brown2_t : identifier;
color_name_brown3_t : identifier;
color_name_brown4_t : identifier;
color_name_burlywood_t : identifier;
color_name_burlywood1_t : identifier;
color_name_burlywood2_t : identifier;
color_name_burlywood3_t : identifier;
color_name_burlywood4_t : identifier;
color_name_cadetblue_t : identifier;
color_name_cadetblue1_t : identifier;
color_name_cadetblue2_t : identifier;
color_name_cadetblue3_t : identifier;
color_name_cadetblue4_t : identifier;
color_name_chartreuse_t : identifier;
color_name_chartreuse1_t : identifier;
color_name_chartreuse2_t : identifier;
color_name_chartreuse3_t : identifier;
color_name_chartreuse4_t : identifier;
color_name_chocolate_t : identifier;
color_name_chocolate1_t : identifier;
color_name_chocolate2_t : identifier;
color_name_chocolate3_t : identifier;
color_name_chocolate4_t : identifier;
color_name_coral_t : identifier;
color_name_coral1_t : identifier;
color_name_coral2_t : identifier;
color_name_coral3_t : identifier;
color_name_coral4_t : identifier;
color_name_cornflowerblue_t : identifier;
color_name_cornsilk_t : identifier;
color_name_cornsilk1_t : identifier;
color_name_cornsilk2_t : identifier;
color_name_cornsilk3_t : identifier;
color_name_cornsilk4_t : identifier;
color_name_cyan_t : identifier;
color_name_cyan1_t : identifier;
color_name_cyan2_t : identifier;
color_name_cyan3_t : identifier;
color_name_cyan4_t : identifier;
color_name_darkblue_t : identifier;
color_name_darkcyan_t : identifier;
color_name_darkgoldenrod_t : identifier;
color_name_darkgoldenrod1_t : identifier;
color_name_darkgoldenrod2_t : identifier;
color_name_darkgoldenrod3_t : identifier;
color_name_darkgoldenrod4_t : identifier;
color_name_darkgreen_t : identifier;
color_name_darkgrey_t : identifier;
color_name_darkkhaki_t : identifier;
color_name_darkmagenta_t : identifier;
color_name_darkolivegreen_t : identifier;
color_name_darkolivegreen1_t : identifier;
color_name_darkolivegreen2_t : identifier;
color_name_darkolivegreen3_t : identifier;
color_name_darkolivegreen4_t : identifier;
color_name_darkorange_t : identifier;
color_name_darkorange1_t : identifier;
color_name_darkorange2_t : identifier;
color_name_darkorange3_t : identifier;
color_name_darkorange4_t : identifier;
color_name_darkorchid_t : identifier;
color_name_darkorchid1_t : identifier;
color_name_darkorchid2_t : identifier;
color_name_darkorchid3_t : identifier;
color_name_darkorchid4_t : identifier;
color_name_darkred_t : identifier;
color_name_darksalmon_t : identifier;
color_name_darkseagreen_t : identifier;
color_name_darkseagreen1_t : identifier;
color_name_darkseagreen2_t : identifier;
color_name_darkseagreen3_t : identifier;
color_name_darkseagreen4_t : identifier;
color_name_darkslateblue_t : identifier;
color_name_darkslategrey_t : identifier;
color_name_darkslategrey1_t : identifier;
color_name_darkslategrey2_t : identifier;
color_name_darkslategrey3_t : identifier;
color_name_darkslategrey4_t : identifier;
color_name_darkturquoise_t : identifier;
color_name_darkviolet_t : identifier;
color_name_deeppink_t : identifier;
color_name_deeppink1_t : identifier;
color_name_deeppink2_t : identifier;
color_name_deeppink3_t : identifier;
color_name_deeppink4_t : identifier;
color_name_deepskyblue_t : identifier;
color_name_deepskyblue1_t : identifier;
color_name_deepskyblue2_t : identifier;
color_name_deepskyblue3_t : identifier;
color_name_deepskyblue4_t : identifier;
color_name_dimgrey_t : identifier;
color_name_dodgerblue_t : identifier;
color_name_dodgerblue1_t : identifier;
color_name_dodgerblue2_t : identifier;
color_name_dodgerblue3_t : identifier;
color_name_dodgerblue4_t : identifier;
color_name_firebrick_t : identifier;
color_name_firebrick1_t : identifier;
color_name_firebrick2_t : identifier;
color_name_firebrick3_t : identifier;
color_name_firebrick4_t : identifier;
color_name_floralwhite_t : identifier;
color_name_forestgreen_t : identifier;
color_name_gainsboro_t : identifier;
color_name_ghostwhite_t : identifier;
color_name_gold_t : identifier;
color_name_gold1_t : identifier;
color_name_gold2_t : identifier;
color_name_gold3_t : identifier;
color_name_gold4_t : identifier;
color_name_goldenrod_t : identifier;
color_name_goldenrod1_t : identifier;
color_name_goldenrod2_t : identifier;
color_name_goldenrod3_t : identifier;
color_name_goldenrod4_t : identifier;
color_name_green_t : identifier;
color_name_green1_t : identifier;
color_name_green2_t : identifier;
color_name_green3_t : identifier;
color_name_green4_t : identifier;
color_name_greenyellow_t : identifier;
color_name_grey_t : identifier;
color_name_honeydew_t : identifier;
color_name_honeydew1_t : identifier;
color_name_honeydew2_t : identifier;
color_name_honeydew3_t : identifier;
color_name_honeydew4_t : identifier;
color_name_hotpink_t : identifier;
color_name_hotpink1_t : identifier;
color_name_hotpink2_t : identifier;
color_name_hotpink3_t : identifier;
color_name_hotpink4_t : identifier;
color_name_indianred_t : identifier;
color_name_indianred1_t : identifier;
color_name_indianred2_t : identifier;
color_name_indianred3_t : identifier;
color_name_indianred4_t : identifier;
color_name_ivory_t : identifier;
color_name_ivory1_t : identifier;
color_name_ivory2_t : identifier;
color_name_ivory3_t : identifier;
color_name_ivory4_t : identifier;
color_name_khaki_t : identifier;
color_name_khaki1_t : identifier;
color_name_khaki2_t : identifier;
color_name_khaki3_t : identifier;
color_name_khaki4_t : identifier;
color_name_lavender_t : identifier;
color_name_lavenderblush_t : identifier;
color_name_lavenderblush1_t : identifier;
color_name_lavenderblush2_t : identifier;
color_name_lavenderblush3_t : identifier;
color_name_lavenderblush4_t : identifier;
color_name_lawngreen_t : identifier;
color_name_lemonchiffon_t : identifier;
color_name_lemonchiffon1_t : identifier;
color_name_lemonchiffon2_t : identifier;
color_name_lemonchiffon3_t : identifier;
color_name_lemonchiffon4_t : identifier;
color_name_lightblue_t : identifier;
color_name_lightblue1_t : identifier;
color_name_lightblue2_t : identifier;
color_name_lightblue3_t : identifier;
color_name_lightblue4_t : identifier;
color_name_lightcoral_t : identifier;
color_name_lightcyan_t : identifier;
color_name_lightcyan1_t : identifier;
color_name_lightcyan2_t : identifier;
color_name_lightcyan3_t : identifier;
color_name_lightcyan4_t : identifier;
color_name_lightgoldenrod_t : identifier;
color_name_lightgoldenrod1_t : identifier;
color_name_lightgoldenrod2_t : identifier;
color_name_lightgoldenrod3_t : identifier;
color_name_lightgoldenrod4_t : identifier;
color_name_lightgoldenrodyellow_t : identifier;
color_name_lightgreen_t : identifier;
color_name_lightgrey_t : identifier;
color_name_lightpink_t : identifier;
color_name_lightpink1_t : identifier;
color_name_lightpink2_t : identifier;
color_name_lightpink3_t : identifier;
color_name_lightpink4_t : identifier;
color_name_lightsalmon_t : identifier;
color_name_lightsalmon1_t : identifier;
color_name_lightsalmon2_t : identifier;
color_name_lightsalmon3_t : identifier;
color_name_lightsalmon4_t : identifier;
color_name_lightseagreen_t : identifier;
color_name_lightskyblue_t : identifier;
color_name_lightskyblue1_t : identifier;
color_name_lightskyblue2_t : identifier;
color_name_lightskyblue3_t : identifier;
color_name_lightskyblue4_t : identifier;
color_name_lightslateblue_t : identifier;
color_name_lightslategrey_t : identifier;
color_name_lightsteelblue_t : identifier;
color_name_lightsteelblue1_t : identifier;
color_name_lightsteelblue2_t : identifier;
color_name_lightsteelblue3_t : identifier;
color_name_lightsteelblue4_t : identifier;
color_name_lightyellow_t : identifier;
color_name_lightyellow1_t : identifier;
color_name_lightyellow2_t : identifier;
color_name_lightyellow3_t : identifier;
color_name_lightyellow4_t : identifier;
color_name_limegreen_t : identifier;
color_name_linen_t : identifier;
color_name_magenta_t : identifier;
color_name_magenta1_t : identifier;
color_name_magenta2_t : identifier;
color_name_magenta3_t : identifier;
color_name_magenta4_t : identifier;
color_name_maroon_t : identifier;
color_name_maroon1_t : identifier;
color_name_maroon2_t : identifier;
color_name_maroon3_t : identifier;
color_name_maroon4_t : identifier;
color_name_mediumaquamarine_t : identifier;
color_name_mediumblue_t : identifier;
color_name_mediumorchid_t : identifier;
color_name_mediumorchid1_t : identifier;
color_name_mediumorchid2_t : identifier;
color_name_mediumorchid3_t : identifier;
color_name_mediumorchid4_t : identifier;
color_name_mediumpurple_t : identifier;
color_name_mediumpurple1_t : identifier;
color_name_mediumpurple2_t : identifier;
color_name_mediumpurple3_t : identifier;
color_name_mediumpurple4_t : identifier;
color_name_mediumseagreen_t : identifier;
color_name_mediumslateblue_t : identifier;
color_name_mediumspringgreen_t : identifier;
color_name_mediumturquoise_t : identifier;
color_name_mediumvioletred_t : identifier;
color_name_midnightblue_t : identifier;
color_name_mintcream_t : identifier;
color_name_mistyrose_t : identifier;
color_name_mistyrose1_t : identifier;
color_name_mistyrose2_t : identifier;
color_name_mistyrose3_t : identifier;
color_name_mistyrose4_t : identifier;
color_name_moccasin_t : identifier;
color_name_navajowhite_t : identifier;
color_name_navajowhite1_t : identifier;
color_name_navajowhite2_t : identifier;
color_name_navajowhite3_t : identifier;
color_name_navajowhite4_t : identifier;
color_name_navyblue_t : identifier;
color_name_oldlace_t : identifier;
color_name_olivedrab_t : identifier;
color_name_olivedrab1_t : identifier;
color_name_olivedrab2_t : identifier;
color_name_olivedrab3_t : identifier;
color_name_olivedrab4_t : identifier;
color_name_orange_t : identifier;
color_name_orange1_t : identifier;
color_name_orange2_t : identifier;
color_name_orange3_t : identifier;
color_name_orange4_t : identifier;
color_name_orangered_t : identifier;
color_name_orangered1_t : identifier;
color_name_orangered2_t : identifier;
color_name_orangered3_t : identifier;
color_name_orangered4_t : identifier;
color_name_orchid_t : identifier;
color_name_orchid1_t : identifier;
color_name_orchid2_t : identifier;
color_name_orchid3_t : identifier;
color_name_orchid4_t : identifier;
color_name_palegoldenrod_t : identifier;
color_name_palegreen_t : identifier;
color_name_palegreen1_t : identifier;
color_name_palegreen2_t : identifier;
color_name_palegreen3_t : identifier;
color_name_palegreen4_t : identifier;
color_name_paleturquoise_t : identifier;
color_name_paleturquoise1_t : identifier;
color_name_paleturquoise2_t : identifier;
color_name_paleturquoise3_t : identifier;
color_name_paleturquoise4_t : identifier;
color_name_palevioletred_t : identifier;
color_name_palevioletred1_t : identifier;
color_name_palevioletred2_t : identifier;
color_name_palevioletred3_t : identifier;
color_name_palevioletred4_t : identifier;
color_name_papayawhip_t : identifier;
color_name_peachpuff_t : identifier;
color_name_peachpuff1_t : identifier;
color_name_peachpuff2_t : identifier;
color_name_peachpuff3_t : identifier;
color_name_peachpuff4_t : identifier;
color_name_peru_t : identifier;
color_name_pink_t : identifier;
color_name_pink1_t : identifier;
color_name_pink2_t : identifier;
color_name_pink3_t : identifier;
color_name_pink4_t : identifier;
color_name_plum_t : identifier;
color_name_plum1_t : identifier;
color_name_plum2_t : identifier;
color_name_plum3_t : identifier;
color_name_plum4_t : identifier;
color_name_powderblue_t : identifier;
color_name_purple_t : identifier;
color_name_purple1_t : identifier;
color_name_purple2_t : identifier;
color_name_purple3_t : identifier;
color_name_purple4_t : identifier;
color_name_red_t : identifier;
color_name_red1_t : identifier;
color_name_red2_t : identifier;
color_name_red3_t : identifier;
color_name_red4_t : identifier;
color_name_rosybrown_t : identifier;
color_name_rosybrown1_t : identifier;
color_name_rosybrown2_t : identifier;
color_name_rosybrown3_t : identifier;
color_name_rosybrown4_t : identifier;
color_name_royalblue_t : identifier;
color_name_royalblue1_t : identifier;
color_name_royalblue2_t : identifier;
color_name_royalblue3_t : identifier;
color_name_royalblue4_t : identifier;
color_name_saddlebrown_t : identifier;
color_name_salmon_t : identifier;
color_name_salmon1_t : identifier;
color_name_salmon2_t : identifier;
color_name_salmon3_t : identifier;
color_name_salmon4_t : identifier;
color_name_sandybrown_t : identifier;
color_name_seagreen_t : identifier;
color_name_seagreen1_t : identifier;
color_name_seagreen2_t : identifier;
color_name_seagreen3_t : identifier;
color_name_seagreen4_t : identifier;
color_name_seashell_t : identifier;
color_name_seashell1_t : identifier;
color_name_seashell2_t : identifier;
color_name_seashell3_t : identifier;
color_name_seashell4_t : identifier;
color_name_sienna_t : identifier;
color_name_sienna1_t : identifier;
color_name_sienna2_t : identifier;
color_name_sienna3_t : identifier;
color_name_sienna4_t : identifier;
color_name_skyblue_t : identifier;
color_name_skyblue1_t : identifier;
color_name_skyblue2_t : identifier;
color_name_skyblue3_t : identifier;
color_name_skyblue4_t : identifier;
color_name_slateblue_t : identifier;
color_name_slateblue1_t : identifier;
color_name_slateblue2_t : identifier;
color_name_slateblue3_t : identifier;
color_name_slateblue4_t : identifier;
color_name_slategray1_t : identifier;
color_name_slategray2_t : identifier;
color_name_slategray3_t : identifier;
color_name_slategray4_t : identifier;
color_name_slategrey_t : identifier;
color_name_snow_t : identifier;
color_name_snow1_t : identifier;
color_name_snow2_t : identifier;
color_name_snow3_t : identifier;
color_name_snow4_t : identifier;
color_name_springgreen_t : identifier;
color_name_springgreen1_t : identifier;
color_name_springgreen2_t : identifier;
color_name_springgreen3_t : identifier;
color_name_springgreen4_t : identifier;
color_name_steelblue_t : identifier;
color_name_steelblue1_t : identifier;
color_name_steelblue2_t : identifier;
color_name_steelblue3_t : identifier;
color_name_steelblue4_t : identifier;
color_name_tan_t : identifier;
color_name_tan1_t : identifier;
color_name_tan2_t : identifier;
color_name_tan3_t : identifier;
color_name_tan4_t : identifier;
color_name_thistle_t : identifier;
color_name_thistle1_t : identifier;
color_name_thistle2_t : identifier;
color_name_thistle3_t : identifier;
color_name_thistle4_t : identifier;
color_name_tomato_t : identifier;
color_name_tomato1_t : identifier;
color_name_tomato2_t : identifier;
color_name_tomato3_t : identifier;
color_name_tomato4_t : identifier;
color_name_turquoise_t : identifier;
color_name_turquoise1_t : identifier;
color_name_turquoise2_t : identifier;
color_name_turquoise3_t : identifier;
color_name_turquoise4_t : identifier;
color_name_violet_t : identifier;
color_name_violetred_t : identifier;
color_name_violetred1_t : identifier;
color_name_violetred2_t : identifier;
color_name_violetred3_t : identifier;
color_name_violetred4_t : identifier;
color_name_wheat_t : identifier;
color_name_wheat1_t : identifier;
color_name_wheat2_t : identifier;
color_name_wheat3_t : identifier;
color_name_wheat4_t : identifier;
color_name_white_t : identifier;
color_name_whitesmoke_t : identifier;
color_name_yellow_t : identifier;
color_name_yellow1_t : identifier;
color_name_yellow2_t : identifier;
color_name_yellow3_t : identifier;
color_name_yellow4_t : identifier;
color_name_yellowgreen_t : identifier;

pen_a_display_info_rec_t : identifier;
pen_a_display_info_rec_textbased_t : identifier;
pen_a_display_info_rec_h_res_t : identifier;
pen_a_display_info_rec_v_res_t : identifier;
pen_a_display_info_rec_c_res_t : identifier;
pen_a_display_info_rec_p_res_t : identifier;
pen_a_display_info_rec_d_res_t : identifier;

------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupPen;
procedure ShutdownPen;

------------------------------------------------------------------------------
-- PARSE THE PEN PACKAGE
------------------------------------------------------------------------------

----> Rects

procedure ParsePenSetRect;
procedure ParsePenIsEmptyRect( result : out unbounded_string );
procedure ParsePenOffsetRect;
procedure ParsePenInsetRect;
procedure ParsePenIntersectRect;
procedure ParsePenInsideRect( result : out unbounded_string );
procedure ParsePenInRect( result : out unbounded_string );

procedure ParsePenSetPenMode;
procedure ParsePenSetPenBrush;
procedure ParsePenSetPenInk;
procedure ParsePenSetPenPattern;
procedure ParsePenGetPenMode( result : out unbounded_string );
procedure ParsePenGetPenInk;
procedure ParsePenGetPenBrush( result : out unbounded_string );
-- procedure ParsePenGetPenPattern( result : out unbounded_string );

procedure ParsePenMoveTo;
procedure ParsePenMove;
procedure ParsePenLineTo;
procedure ParsePenLine;
procedure ParsePenHLine;
procedure ParsePenVLine;

procedure ParsePenFrameRect;
procedure ParsePenPaintRect;
procedure ParsePenFillRect;
procedure ParsePenFrameEllipse;
procedure ParsePenFillEllipse;
procedure ParsePenClear;

procedure ParsePenNewScreenCanvas;
procedure ParsePenNewGLScreenCanvas;
procedure ParsePenNewWindowCanvas;
procedure ParsePenNewGLWindowCanvas;
procedure ParsePenNewCanvas;
procedure ParsePenSaveCanvas;
procedure ParsePenSetTitle;
procedure ParsePenCloseCanvas;

procedure ParsePenWaitToReveal;
procedure ParsePenReveal;
procedure ParsePenRevealNow;

procedure ParsePenGreyscale( result : out unbounded_string );
procedure ParsePenBlend;
procedure ParsePenFade;

procedure ParsePenPlot;

end parser_pen;
