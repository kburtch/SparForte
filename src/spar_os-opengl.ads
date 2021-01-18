------------------------------------------------------------------------------
-- Bindings to OpenGL (e.g. Mesa)                                           --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2021 Free Software Foundation              --
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

with Ada.Strings.Unbounded,
     Ada.Unchecked_Deallocation,
     Interfaces.C,
     System.Address_To_Access_Conversions;
use  Ada.Strings.Unbounded,
     Interfaces.C;

package spar_os.opengl is

------------------------------------------------------------------------------
-- Standard OpenGL Types
------------------------------------------------------------------------------

type GLenum is new unsigned;           -- typedef unsigned int    GLenum;
type GLboolean is new unsigned_char;   -- typedef unsigned char   GLboolean;
type GLbitfield is new unsigned;       -- typedef unsigned int    GLbitfield;
--type GLvoid is new System.address;     -- typedef void            GLvoid;
subtype GLbyte is signed_char;         -- typedef signed char     GLbyte;
                                       --   /* 1-byte signed */
subtype GLshort is short;              -- typedef short           GLshort;
                                       --   /* 2-byte signed */
subtype GLint is int;                  -- typedef int             GLint;
                                       --   /* 4-byte signed */
subtype GLubyte is unsigned_char;      -- typedef unsigned char   GLubyte;
                                       --   /* 1-byte unsigned */
subtype GLushort is unsigned_short;    -- typedef unsigned short  GLushort;
                                       --   /* 2-byte unsigned */
type GLuint is new unsigned;           -- typedef unsigned int    GLuint;
                                       --   /* 4-byte unsigned */
type GLsizei is new int;               -- typedef int             GLsizei;
                                       --   /* 4-byte signed */
subtype GLfloat is float;              -- typedef float           GLfloat;
                                       --   /* single precision float */
type GLclampf is new float;            -- typedef float           GLclampf;
                                       --   /* single precision float in [0,1] */
subtype GLdouble is double;            -- typedef double          GLdouble;
                                       --   /* double precision float */
type GLclampd is new double;           -- typedef double          GLclampd;

------------------------------------------------------------------------------
-- C arrays and their Pointers
------------------------------------------------------------------------------

type char_array_ptr is new System.address;

type double_array is array( size_t range <> ) of aliased GLdouble;
type double_array_matrix is new double_array( 0..15 );
pragma warnings( off );
package GL_Double_Array_Conv is new
     system.address_to_access_conversions( double_array );
subtype GL_Double_Array_Ptr is System.Address;
subtype GL_Double_Array_Access is GL_Double_Array_Conv.object_pointer;
procedure free is new Ada.Unchecked_Deallocation( double_array, GL_Double_Array_Access );
pragma warnings( on );

type double_vertex_2d is new double_array( 0..1 );
type double_vertex_3d is new double_array( 0..2 );
type double_vertex_4d is new double_array( 0..3 );

type clamp_float_array is array( size_t range <> ) of aliased GLclampf;
pragma warnings( off );
package GL_Clampf_Array_Conv is new
     system.address_to_access_conversions( clamp_float_array );
     subtype GL_Clampf_Array_Ptr is GL_Clampf_Array_Conv.object_pointer;
pragma warnings( on );

type float_array is array( size_t range <> ) of aliased GLfloat;
type float_array_matrix is new float_array( 0..15 );
pragma warnings( off );
package GL_Float_Array_Conv is new
     system.address_to_access_conversions( float_array );
subtype GL_Float_Array_Ptr is System.Address;
subtype GL_Float_Array_Access is GL_Float_Array_Conv.object_pointer;
procedure free is new Ada.Unchecked_Deallocation( float_array, GL_Float_Array_Access );
pragma warnings( on );

type float_vertex_2d is new float_array( 0..1 );
type float_vertex_3d is new float_array( 0..2 );
type float_vertex_4d is new float_array( 0..3 );

type int_array is array( size_t range <> ) of aliased GLint;
type int_array_matrix is new double_array( 0..15 );
pragma warnings( off );
package GL_Int_Array_Conv is new
     system.address_to_access_conversions( int_array );
subtype GL_Int_Array_Ptr is System.Address;
subtype GL_Int_Array_Access is GL_Int_Array_Conv.object_pointer;
procedure free is new Ada.Unchecked_Deallocation( int_array, GL_Int_Array_Access );
pragma warnings( on );

type int_vertex_2d is new int_array( 0..1 );
type int_vertex_3d is new int_array( 0..2 );
type int_vertex_4d is new int_array( 0..3 );

type short_array is array( size_t range <> ) of aliased GLshort;
type short_array_matrix is new short_array( 0..15 );
pragma warnings( off );
package GL_Short_Array_Conv is new
     system.address_to_access_conversions( short_array );
subtype GL_Short_Array_Ptr is System.Address;
subtype GL_Short_Array_Access is GL_Short_Array_Conv.object_pointer;
procedure free is new Ada.Unchecked_Deallocation( short_array, GL_Short_Array_Access );
pragma warnings( on );

type short_vertex_2d is new short_array( 0..1 );
type short_vertex_3d is new short_array( 0..2 );
type short_vertex_4d is new short_array( 0..3 );

type byte_array is array( size_t range <> ) of aliased GLbyte;
pragma warnings( off );
package GL_Byte_Array_Conv is new
     system.address_to_access_conversions( byte_array );
subtype GL_Byte_Array_Ptr is System.Address;
subtype GL_Byte_Array_Access is GL_Byte_Array_Conv.object_pointer;
procedure free is new Ada.Unchecked_Deallocation( byte_array, GL_Byte_Array_Access );
pragma warnings( on );

type byte_vertex_2d is new byte_array( 0..1 );
type byte_vertex_3d is new byte_array( 0..2 );
type byte_vertex_4d is new byte_array( 0..3 );

type uint_array is array( size_t range <> ) of aliased GLuint;
pragma warnings( off );
package GL_UInt_Array_Conv is new
     system.address_to_access_conversions( uint_array );
subtype GL_UInt_Array_Ptr is System.Address;
subtype GL_UInt_Array_Access is GL_UInt_Array_Conv.object_pointer;
procedure free is new Ada.Unchecked_Deallocation( uint_array, GL_UInt_Array_Access );
pragma warnings( on );

type ushort_array is array( size_t range <> ) of aliased GLushort;
pragma warnings( off );
package GL_UShort_Array_Conv is new
     system.address_to_access_conversions( ushort_array );
subtype GL_UShort_Array_Ptr is System.Address;
subtype GL_UShort_Array_Access is GL_UShort_Array_Conv.object_pointer;
procedure free is new Ada.Unchecked_Deallocation( ushort_array, GL_UShort_Array_Access );
pragma warnings( on );

type ubyte_array is array( size_t range <> ) of aliased GLubyte;
pragma warnings( off );
package GL_UByte_Array_Conv is new
     system.address_to_access_conversions( ubyte_array );
subtype GL_UByte_Array_Ptr is System.Address;
subtype GL_UByte_Array_Access is GL_UByte_Array_Conv.object_pointer;
procedure free is new Ada.Unchecked_Deallocation( ubyte_array, GL_UByte_Array_Access );
pragma warnings( on );

type bool_array is array( size_t range <> ) of aliased GLboolean;
pragma warnings( off );
package GL_Boolean_Array_Conv is new
     system.address_to_access_conversions( bool_array );
subtype GL_Boolean_Array_Ptr is System.Address;
subtype GL_Boolean_Array_Access is GL_Boolean_Array_Conv.object_pointer;
procedure free is new Ada.Unchecked_Deallocation( bool_array, GL_Boolean_Array_Access );
pragma warnings( on );

------------------------------------------------------------------------------
-- OpenGL Core
------------------------------------------------------------------------------

GL_FALSE : constant GLboolean := 16#0#; -- #define GL_FALSE  0x0
GL_TRUE  : constant GLboolean := 16#1#; -- #define GL_TRUE   0x1

type GLtypes is new GLenum;

GL_BYTE          : constant GLtypes := 16#1400#; -- #define GL_BYTE 0x1400
GL_UNSIGNED_BYTE : constant GLtypes := 16#1401#; -- #define GL_UNSIGNED_BYTE 0x1401
GL_SHORT         : constant GLtypes := 16#1402#; -- #define GL_SHORT 0x1402
GL_UNSIGNED_SHORT : constant GLtypes := 16#1403#; -- #define GL_UNSIGNED_SHORT 0x1403
GL_INT           : constant GLtypes := 16#1404#; -- #define GL_INT 0x1404
GL_UNSIGNED_INT  : constant GLtypes := 16#1405#; -- #define GL_UNSIGNED_INT 0x1405
GL_FLOAT         : constant GLtypes := 16#1406#; -- #define GL_FLOAT 0x1406
GL_2_BYTES       : constant GLtypes := 16#1407#; -- #define GL_2_BYTES 0x1407
GL_3_BYTES       : constant GLtypes := 16#1408#; -- #define GL_3_BYTES 0x1408
GL_4_BYTES       : constant GLtypes := 16#1409#; -- #define GL_4_BYTES 0x1409
GL_DOUBLE        : constant GLtypes := 16#140A#; -- #define GL_DOUBLE 0x140A

------------------------------------------------------------------------------
-- Primitives
------------------------------------------------------------------------------

type GLprimitives is new GLenum;

GL_POINTS        : constant GLprimitives := 16#0000#; -- #define GL_POINTS 0x0000
GL_LINES         : constant GLprimitives := 16#0001#; -- #define GL_LINES  0x0001
GL_LINE_LOOP     : constant GLprimitives := 16#0002#; -- #define GL_LINE_LOOP 0x0002
GL_LINE_STRIP    : constant GLprimitives := 16#0003#; -- #define GL_LINE_STRIP 0x0003
GL_TRIANGLES     : constant GLprimitives := 16#0004#; -- #define GL_TRIANGLES 0x0004
GL_TRIANGLE_STRIP : constant GLprimitives := 16#0005#; -- #define GL_TRIANGLE_STRIP 0x0005
GL_TRIANGLE_FAN  : constant GLprimitives := 16#0006#; -- #define GL_TRIANGLE_FAN 0x0006
GL_QUADS         : constant GLprimitives := 16#0007#; -- #define GL_QUADS 0x0007
GL_QUAD_STRIP    : constant GLprimitives := 16#0008#; -- #define GL_QUAD_STRIP 0x0008
GL_POLYGON       : constant GLprimitives := 16#0009#; -- #define GL_POLYGON 0x0009

------------------------------------------------------------------------------
-- Vertex Arrays
------------------------------------------------------------------------------
--
type GLvertexarrays is new GLenum;

GL_VERTEX_ARRAY            : constant GLvertexarrays := 16#8074#; -- #define GL_VERTEX_ARRAY                         0x8074
GL_NORMAL_ARRAY            : constant GLvertexarrays := 16#8075#; -- #define GL_NORMAL_ARRAY                         0x8075
GL_COLOR_ARRAY             : constant GLvertexarrays := 16#8076#; -- #define GL_COLOR_ARRAY                          0x8076
GL_INDEX_ARRAY             : constant GLvertexarrays := 16#8077#; -- #define GL_INDEX_ARRAY                          0x8077
GL_TEXTURE_COORD_ARRAY     : constant GLvertexarrays := 16#8078#; -- #define GL_TEXTURE_COORD_ARRAY                  0x8078
GL_EDGE_FLAG_ARRAY         : constant GLvertexarrays := 16#8079#; -- #define GL_EDGE_FLAG_ARRAY                      0x8079
GL_VERTEX_ARRAY_SIZE       : constant GLvertexarrays := 16#807A#; -- #define GL_VERTEX_ARRAY_SIZE                    0x807A
GL_VERTEX_ARRAY_TYPE       : constant GLvertexarrays := 16#807B#; -- #define GL_VERTEX_ARRAY_TYPE                    0x807B
GL_VERTEX_ARRAY_STRIDE     : constant GLvertexarrays := 16#807C#; -- #define GL_VERTEX_ARRAY_STRIDE                  0x807C
GL_NORMAL_ARRAY_TYPE       : constant GLvertexarrays := 16#807E#; -- #define GL_NORMAL_ARRAY_TYPE                    0x807E
GL_NORMAL_ARRAY_STRIDE     : constant GLvertexarrays := 16#807F#; -- #define GL_NORMAL_ARRAY_STRIDE                  0x807F
GL_COLOR_ARRAY_SIZE        : constant GLvertexarrays := 16#8081#; -- #define GL_COLOR_ARRAY_SIZE                     0x8081
GL_COLOR_ARRAY_TYPE        : constant GLvertexarrays := 16#8082#; -- #define GL_COLOR_ARRAY_TYPE                     0x8082
GL_COLOR_ARRAY_STRIDE      : constant GLvertexarrays := 16#8083#; -- #define GL_COLOR_ARRAY_STRIDE                   0x8083
GL_INDEX_ARRAY_TYPE        : constant GLvertexarrays := 16#8085#; -- #define GL_INDEX_ARRAY_TYPE                     0x8085
GL_INDEX_ARRAY_STRIDE      : constant GLvertexarrays := 16#8086#; -- #define GL_INDEX_ARRAY_STRIDE                   0x8086
GL_TEXTURE_COORD_ARRAY_SIZE : constant GLvertexarrays := 16#8088#; -- #define GL_TEXTURE_COORD_ARRAY_SIZE             0x8088
GL_TEXTURE_COORD_ARRAY_TYPE : constant GLvertexarrays := 16#8089#; -- #define GL_TEXTURE_COORD_ARRAY_TYPE             0x8089
GL_TEXTURE_COORD_ARRAY_STRIDE : constant GLvertexarrays := 16#808A#; -- #define GL_TEXTURE_COORD_ARRAY_STRIDE           0x808A
GL_EDGE_FLAG_ARRAY_STRIDE  : constant GLvertexarrays := 16#808C#; -- #define GL_EDGE_FLAG_ARRAY_STRIDE               0x808C
GL_VERTEX_ARRAY_POINTER    : constant GLvertexarrays := 16#808E#; -- #define GL_VERTEX_ARRAY_POINTER                 0x808E
GL_NORMAL_ARRAY_POINTER : constant GLvertexarrays := 16#808F#; -- #define GL_NORMAL_ARRAY_POINTER                 0x808F
GL_COLOR_ARRAY_POINTER : constant GLvertexarrays := 16#8090#; -- #define GL_COLOR_ARRAY_POINTER                  0x8090
GL_INDEX_ARRAY_POINTER : constant GLvertexarrays := 16#8091#; -- #define GL_INDEX_ARRAY_POINTER                  0x8091
GL_TEXTURE_COORD_ARRAY_POINTER : constant GLvertexarrays := 16#8092#; -- #define GL_TEXTURE_COORD_ARRAY_POINTER          0x8092
GL_EDGE_FLAG_ARRAY_POINTER : constant GLvertexarrays := 16#8093#; -- #define GL_EDGE_FLAG_ARRAY_POINTER              0x8093
GL_V2F             : constant GLvertexarrays := 16#2A20#; -- #define GL_V2F 0x2A20
GL_V3F             : constant GLvertexarrays := 16#2A21#; -- #define GL_V3F 0x2A21
GL_C4UB_V2F        : constant GLvertexarrays := 16#2A22#; -- #define GL_C4UB_V2F 0x2A22
GL_C4UB_V3F        : constant GLvertexarrays := 16#2A23#; -- #define GL_C4UB_V3F 0x2A23
GL_C3F_V3F         : constant GLvertexarrays := 16#2A24#; -- #define GL_C3F_V3F 0x2A24
GL_N3F_V3F         : constant GLvertexarrays := 16#2A25#; -- #define GL_N3F_V3F 0x2A25
GL_C4F_N3F_V3F     : constant GLvertexarrays := 16#2A26#; -- #define GL_C4F_N3F_V3F 0x2A26
GL_T2F_V3F         : constant GLvertexarrays := 16#2A27#; -- #define GL_T2F_V3F 0x2A27
GL_T4F_V4F         : constant GLvertexarrays := 16#2A28#; -- #define GL_T4F_V4F 0x2A28
GL_T2F_C4UB_V3F    : constant GLvertexarrays := 16#2A29#; -- #define GL_T2F_C4UB_V3F 0x2A29
GL_T2F_C3F_V3F     : constant GLvertexarrays := 16#2A2A#; -- #define GL_T2F_C3F_V3F 0x2A2A
GL_T2F_N3F_V3F     : constant GLvertexarrays := 16#2A2B#; -- #define GL_T2F_N3F_V3F 0x2A2B
GL_T2F_C4F_N3F_V3F : constant GLvertexarrays := 16#2A2C#; -- #define GL_T2F_C4F_N3F_V3F 0x2A2C
GL_T4F_C4F_N3F_V4F : constant GLvertexarrays := 16#2A2D#; -- #define GL_T4F_C4F_N3F_V4F 0x2A2D

------------------------------------------------------------------------------
-- Matrix Mode
------------------------------------------------------------------------------

type GLmodes is new GLenum;

GL_MATRIX_MODE : constant GLmodes := 16#0BA0#; -- #define GL_MATRIX_MODE 0x0BA0
GL_MODELVIEW   : constant GLmodes := 16#1700#; -- #define GL_MODELVIEW 0x1700
GL_PROJECTION  : constant GLmodes := 16#1701#; -- #define GL_PROJECTION 0x1701
GL_TEXTURE     : constant GLmodes := 16#1702#; -- #define GL_TEXTURE 0x1702

------------------------------------------------------------------------------
-- Points
------------------------------------------------------------------------------

type GLpoints is new GLenum;

GL_POINT_SMOOTH : constant GLpoints :=  16#0B10#; -- #define GL_POINT_SMOOTH 0x0B10
GL_POINT_SIZE   : constant GLpoints :=  16#0B11#; -- #define GL_POINT_SIZE 0x0B11
GL_POINT_SIZE_GRANULARITY : constant GLpoints := 16#0B13#; -- #define GL_POINT_SIZE_GRANULARITY 0x0B13
GL_POINT_SIZE_RANGE : constant GLpoints := 16#0B12#; -- #define GL_POINT_SIZE_RANGE 0x0B12

------------------------------------------------------------------------------
-- Lines
------------------------------------------------------------------------------

type GLlines is new GLenum;

GL_LINE_SMOOTH : constant GLlines := 16#0B20#; -- #define GL_LINE_SMOOTH 0x0B20
GL_LINE_STIPPLE : constant GLlines := 16#0B24#; -- #define GL_LINE_STIPPLE 0x0B24
GL_LINE_STIPPLE_PATTERN : constant GLlines := 16#0B25#; -- #define GL_LINE_STIPPLE_PATTERN 0x0B25
GL_LINE_STIPPLE_REPEAT : constant GLlines := 16#0B26#; -- #define GL_LINE_STIPPLE_REPEAT 0x0B26
GL_LINE_WIDTH : constant GLlines := 16#0B21#; -- #define GL_LINE_WIDTH 0x0B21
GL_LINE_WIDTH_GRANULARITY : constant GLlines := 16#0B23#; -- #define GL_LINE_WIDTH_GRANULARITY 0x0B23
GL_LINE_WIDTH_RANGE : constant GLlines := 16#0B22#; -- #define GL_LINE_WIDTH_RANGE 0x0B22

------------------------------------------------------------------------------
-- Polygons
------------------------------------------------------------------------------

type GLpolygons is new GLenum;

GL_POINT           : constant GLpolygons := 16#1B00#; -- GL_POINT 0x1B00
GL_LINE            : constant GLpolygons := 16#1B01#; -- GL_LINE 0x1B01
GL_FILL            : constant GLpolygons := 16#1B02#; -- GL_FILL 0x1B02
GL_CW              : constant GLpolygons := 16#0900#; -- GL_CW 0x0900
GL_CCW             : constant GLpolygons := 16#0901#; -- GL_CCW 0x0901
GL_FRONT           : constant GLpolygons := 16#0404#; -- GL_FRONT 0x0404
GL_BACK            : constant GLpolygons := 16#0405#; -- GL_BACK 0x0405
GL_POLYGON_MODE    : constant GLpolygons := 16#0B40#; -- GL_POLYGON_MODE 0x0B40
GL_POLYGON_SMOOTH  : constant GLpolygons := 16#0B41#; -- GL_POLYGON_SMOOTH 0x0B41
GL_POLYGON_STIPPLE : constant GLpolygons := 16#0B42#; -- GL_POLYGON_STIPPLE 0x0B42
GL_EDGE_FLAG       : constant GLpolygons := 16#0B43#; -- GL_EDGE_FLAG 0x0B43
GL_CULL_FACE       : constant GLpolygons := 16#0B44#; -- GL_CULL_FACE 0x0B44
GL_CULL_FACE_MODE  : constant GLpolygons := 16#0B45#; -- GL_CULL_FACE_MODE 0x0B45
GL_FRONT_FACE      : constant GLpolygons := 16#0B46#; -- GL_FRONT_FACE 0x0B46
GL_POLYGON_OFFSET_FACTOR : constant GLpolygons := 16#8038#; -- GL_POLYGON_OFFSET_FACTOR 0x8038
GL_POLYGON_OFFSET_UNITS : constant GLpolygons := 16#2A00#; -- GL_POLYGON_OFFSET_UNITS 0x2A00
GL_POLYGON_OFFSET_POINT : constant GLpolygons := 16#2A01#; -- GL_POLYGON_OFFSET_POINT 0x2A01
GL_POLYGON_OFFSET_LINE : constant GLpolygons := 16#2A02#; -- GL_POLYGON_OFFSET_LINE 0x2A02
GL_POLYGON_OFFSET_FILL : constant GLpolygons := 16#8037#; -- GL_POLYGON_OFFSET_FILL 0x8037

------------------------------------------------------------------------------
-- Display Lists
------------------------------------------------------------------------------
--
type GLdlists is new GLenum;

GL_COMPILE     : constant GLdlists := 16#1300#; -- GL_COMPILE 0x1300
GL_COMPILE_AND_EXECUTE : constant GLdlists := 16#1301#; -- GL_COMPILE_AND_EXECUTE 0x1301
GL_LIST_BASE   : constant GLdlists := 16#0B32#; -- GL_LIST_BASE 0x0B32
GL_LIST_INDEX  : constant GLdlists := 16#0B33#; -- GL_LIST_INDEX 0x0B33
GL_LIST_MODE   : constant GLdlists := 16#0B30#; -- GL_LIST_MODE 0x0B30

------------------------------------------------------------------------------
-- Depth Buffer
------------------------------------------------------------------------------

type GLalphacompare is new GLenum;

GL_NEVER      : constant GLalphacompare := 16#0200#; -- GL_NEVER 0x0200
GL_LESS       : constant GLalphacompare := 16#0201#; -- GL_LESS 0x0201
GL_EQUAL      : constant GLalphacompare := 16#0202#; -- GL_EQUAL 0x0202
GL_LEQUAL     : constant GLalphacompare := 16#0203#; -- GL_LEQUAL 0x0203
GL_GREATER    : constant GLalphacompare := 16#0204#; -- GL_GREATER 0x0204
GL_NOTEQUAL   : constant GLalphacompare := 16#0205#; -- GL_NOTEQUAL 0x0205
GL_GEQUAL     : constant GLalphacompare := 16#0206#; -- GL_GEQUAL 0x0206
GL_ALWAYS     : constant GLalphacompare := 16#0207#; -- GL_ALWAYS 0x0207

type GLdbuffer is new GLenum;

GL_DEPTH_TEST : constant GLdbuffer := 16#0B71#; -- GL_DEPTH_TEST 0x0B71
GL_DEPTH_BITS : constant GLdbuffer := 16#0D56#; -- GL_DEPTH_BITS 0x0D56
GL_DEPTH_CLEAR_VALUE : constant GLdbuffer := 16#0B73#; -- GL_DEPTH_CLEAR_VALUE 0x0B73
GL_DEPTH_FUNC : constant GLdbuffer := 16#0B74#; -- GL_DEPTH_FUNC 0x0B74
GL_DEPTH_RANGE : constant GLdbuffer := 16#0B70#; -- GL_DEPTH_RANGE 0x0B70
GL_DEPTH_WRITEMASK : constant GLdbuffer := 16#0B72#; -- GL_DEPTH_WRITEMASK 0x0B72
GL_DEPTH_COMPONENT : constant GLdbuffer := 16#1902#; -- GL_DEPTH_COMPONENT 0x1902

------------------------------------------------------------------------------
-- Lighting
------------------------------------------------------------------------------

type GLlighting is new GLenum;

GL_LIGHTING : constant GLlighting := 16#0B50#; -- GL_LIGHTING 0x0B50
GL_LIGHT0   : constant GLlighting := 16#4000#; -- GL_LIGHT0 0x4000
GL_LIGHT1   : constant GLlighting := 16#4001#; -- GL_LIGHT1 0x4001
GL_LIGHT2   : constant GLlighting := 16#4002#; -- GL_LIGHT2 0x4002
GL_LIGHT3   : constant GLlighting := 16#4003#; -- GL_LIGHT3 0x4003
GL_LIGHT4   : constant GLlighting := 16#4004#; -- GL_LIGHT4 0x4004
GL_LIGHT5   : constant GLlighting := 16#4005#; -- GL_LIGHT5 0x4005
GL_LIGHT6   : constant GLlighting := 16#4006#; -- GL_LIGHT6 0x4006
GL_LIGHT7   : constant GLlighting := 16#4007#; -- GL_LIGHT7 0x4007
GL_SPOT_EXPONENT : constant GLlighting := 16#1205#; -- GL_SPOT_EXPONENT 0x1205
GL_SPOT_CUTOFF : constant GLlighting := 16#1206#; -- GL_SPOT_CUTOFF 0x1206
GL_CONSTANT_ATTENUATION : constant GLlighting := 16#1207#; -- GL_CONSTANT_ATTENUATION 0x1207
GL_LINEAR_ATTENUATION : constant GLlighting := 16#1208#; -- GL_LINEAR_ATTENUATION 0x1208
GL_QUADRATIC_ATTENUATION : constant GLlighting := 16#1209#; -- GL_QUADRATIC_ATTENUATION 0x1209
GL_AMBIENT  : constant GLlighting :=    16#1200#; -- GL_AMBIENT 0x1200
GL_DIFFUSE  : constant GLlighting :=    16#1201#; -- GL_DIFFUSE 0x1201
GL_SPECULAR : constant GLlighting :=    16#1202#; -- GL_SPECULAR 0x1202
GL_SHININESS : constant GLlighting :=   16#1601#; -- GL_SHININESS 0x1601
GL_EMISSION : constant GLlighting :=    16#1600#; -- GL_EMISSION 0x1600
GL_POSITION : constant GLlighting :=    16#1203#; -- GL_POSITION 0x1203
GL_SPOT_DIRECTION : constant GLlighting := 16#1204#; -- GL_SPOT_DIRECTION 0x1204
GL_AMBIENT_AND_DIFFUSE : constant GLlighting := 16#1602#; -- GL_AMBIENT_AND_DIFFUSE 0x1602
GL_COLOR_INDEXES : constant GLlighting := 16#1603#; -- GL_COLOR_INDEXES 0x1603
GL_LIGHT_MODEL_TWO_SIDE : constant GLlighting := 16#0B52#; -- GL_LIGHT_MODEL_TWO_SIDE 0x0B52
GL_LIGHT_MODEL_LOCAL_VIEWER : constant GLlighting := 16#0B51#; -- GL_LIGHT_MODEL_LOCAL_VIEWER 0x0B51
GL_LIGHT_MODEL_AMBIENT : constant GLlighting := 16#0B53#; -- GL_LIGHT_MODEL_AMBIENT 0x0B53
GL_FRONT_AND_BACK : constant GLlighting := 16#0408#; -- GL_FRONT_AND_BACK 0x0408
GL_SHADE_MODEL : constant GLlighting := 16#0B54#; -- GL_SHADE_MODEL 0x0B54
GL_FLAT : constant GLlighting := 16#1D00#; -- GL_FLAT 0x1D00
GL_SMOOTH : constant GLlighting := 16#1D01#; -- GL_SMOOTH 0x1D01
GL_COLOR_MATERIAL : constant GLlighting := 16#0B57#; -- GL_COLOR_MATERIAL 0x0B57
GL_COLOR_MATERIAL_FACE : constant GLlighting := 16#0B55#; -- GL_COLOR_MATERIAL_FACE 0x0B55
GL_COLOR_MATERIAL_PARAMETER : constant GLlighting := 16#0B56#; -- GL_COLOR_MATERIAL_PARAMETER 0x0B56
GL_NORMALIZE : constant GLlighting := 16#0BA1#; -- GL_NORMALIZE 0x0BA1

------------------------------------------------------------------------------
-- User Clip Plane
------------------------------------------------------------------------------

type GLusercplane is new GLenum;

GL_CLIP_PLANE0 : constant GLusercplane := 16#3000#; -- GL_CLIP_PLANE0 0x3000
GL_CLIP_PLANE1 : constant GLusercplane := 16#3001#; -- GL_CLIP_PLANE1 0x3001
GL_CLIP_PLANE2 : constant GLusercplane := 16#3002#; -- GL_CLIP_PLANE2 0x3002
GL_CLIP_PLANE3 : constant GLusercplane := 16#3003#; -- GL_CLIP_PLANE3 0x3003
GL_CLIP_PLANE4 : constant GLusercplane := 16#3004#; -- GL_CLIP_PLANE4 0x3004
GL_CLIP_PLANE5 : constant GLusercplane := 16#3005#; -- GL_CLIP_PLANE5 0x3005

------------------------------------------------------------------------------
-- Accumulation buffer
------------------------------------------------------------------------------

type GLabuffer is new GLenum;

GL_ACCUM_RED_BITS    : constant GLabuffer := 16#0D58#; -- GL_ACCUM_RED_BITS 0x0D58
GL_ACCUM_GREEN_BITS  : constant GLabuffer := 16#0D59#; -- GL_ACCUM_GREEN_BITS 0x0D59
GL_ACCUM_BLUE_BITS   : constant GLabuffer := 16#0D5A#; -- GL_ACCUM_BLUE_BITS 0x0D5A
GL_ACCUM_ALPHA_BITS  : constant GLabuffer := 16#0D5B#; -- GL_ACCUM_ALPHA_BITS 0x0D5B
GL_ACCUM_CLEAR_VALUE : constant GLabuffer := 16#0B80#; -- GL_ACCUM_CLEAR_VALUE 0x0B80
GL_ACCUM             : constant GLabuffer := 16#0100#; -- GL_ACCUM 0x0100
GL_ADD               : constant GLabuffer := 16#0104#; -- GL_ADD 0x0104
GL_LOAD              : constant GLabuffer := 16#0101#; -- GL_LOAD 0x0101
GL_MULT              : constant GLabuffer := 16#0103#; -- GL_MULT 0x0103
GL_RETURN            : constant GLabuffer := 16#0102#; -- GL_RETURN 0x0102

------------------------------------------------------------------------------
-- Alpha testing
------------------------------------------------------------------------------

type GLatesting is new GLenum;

GL_ALPHA_TEST      : constant GLatesting := 16#0BC0#; -- GL_ALPHA_TEST 0x0BC0
GL_ALPHA_TEST_REF  : constant GLatesting := 16#0BC2#; -- GL_ALPHA_TEST_REF 0x0BC2
GL_ALPHA_TEST_FUNC : constant GLatesting := 16#0BC1#; -- GL_ALPHA_TEST_FUNC 0x0BC1

------------------------------------------------------------------------------
-- Blending
------------------------------------------------------------------------------

type GLblending is new GLenum;

GL_BLEND               : constant GLblending := 16#0BE2#; -- GL_BLEND 0x0BE2
GL_BLEND_SRC           : constant GLblending := 16#0BE1#; -- GL_BLEND_SRC 0x0BE1
GL_BLEND_DST           : constant GLblending := 16#0BE0#; -- GL_BLEND_DST 0x0BE0
GL_ZERO                : constant GLblending := 16#0#; -- GL_ZERO 0x0
GL_ONE                 : constant GLblending := 16#1#; -- GL_ONE 0x1
GL_SRC_COLOR           : constant GLblending := 16#0300#; -- GL_SRC_COLOR 0x0300
GL_ONE_MINUS_SRC_COLOR : constant GLblending := 16#0301#; -- GL_ONE_MINUS_SRC_COLOR 0x0301
GL_SRC_ALPHA           : constant GLblending := 16#0302#; -- GL_SRC_ALPHA 0x0302
GL_ONE_MINUS_SRC_ALPHA : constant GLblending := 16#0303#; -- GL_ONE_MINUS_SRC_ALPHA 0x0303
GL_DST_ALPHA           : constant GLblending := 16#0304#; -- GL_DST_ALPHA 0x0304
GL_ONE_MINUS_DST_ALPHA : constant GLblending := 16#0305#; -- GL_ONE_MINUS_DST_ALPHA 0x0305
GL_DST_COLOR           : constant GLblending := 16#0306#; -- GL_DST_COLOR 0x0306
GL_ONE_MINUS_DST_COLOR : constant GLblending := 16#0307#; -- GL_ONE_MINUS_DST_COLOR 0x0307
GL_SRC_ALPHA_SATURATE  : constant GLblending := 16#0308#; -- GL_SRC_ALPHA_SATURATE 0x0308

------------------------------------------------------------------------------
-- Render Mode
------------------------------------------------------------------------------

type GLrendermodes is new GLenum;

GL_FEEDBACK : constant GLrendermodes := 16#1C01#; -- GL_FEEDBACK 0x1C01
GL_RENDER   : constant GLrendermodes := 16#1C00#; -- GL_RENDER 0x1C00
GL_SELECT   : constant GLrendermodes := 16#1C02#; -- GL_SELECT 0x1C02

------------------------------------------------------------------------------
-- Feedback
------------------------------------------------------------------------------

type GLfeedback is new GLenum;

GL_2D               : constant GLfeedback := 16#0600#; -- GL_2D                                   0x0600
GL_3D               : constant GLfeedback := 16#0601#; -- GL_3D                                   0x0601
GL_3D_COLOR         : constant GLfeedback := 16#0602#; -- GL_3D_COLOR                             0x0602
GL_3D_COLOR_TEXTURE : constant GLfeedback := 16#0603#; -- GL_3D_COLOR_TEXTURE                     0x0603
GL_4D_COLOR_TEXTURE : constant GLfeedback := 16#0604#; -- GL_4D_COLOR_TEXTURE                     0x0604
GL_POINT_TOKEN      : constant GLfeedback := 16#0701#; -- GL_POINT_TOKEN                          0x0701
GL_LINE_TOKEN       : constant GLfeedback := 16#0702#; -- GL_LINE_TOKEN                           0x0702
GL_LINE_RESET_TOKEN : constant GLfeedback := 16#0707#; -- GL_LINE_RESET_TOKEN                     0x0707
GL_POLYGON_TOKEN    : constant GLfeedback := 16#0703#; -- GL_POLYGON_TOKEN                        0x0703
GL_BITMAP_TOKEN     : constant GLfeedback := 16#0704#; -- GL_BITMAP_TOKEN                         0x0704
GL_DRAW_PIXEL_TOKEN : constant GLfeedback := 16#0705#; -- GL_DRAW_PIXEL_TOKEN                     0x0705
GL_COPY_PIXEL_TOKEN : constant GLfeedback := 16#0706#; -- GL_COPY_PIXEL_TOKEN                     0x0706
GL_PASS_THROUGH_TOKEN : constant GLfeedback := 16#0700#; -- GL_PASS_THROUGH_TOKEN                   0x0700
GL_FEEDBACK_BUFFER_POINTER : constant GLfeedback := 16#0DF0#; -- GL_FEEDBACK_BUFFER_POINTER              0x0DF0
GL_FEEDBACK_BUFFER_SIZE : constant GLfeedback := 16#0DF1#; -- GL_FEEDBACK_BUFFER_SIZE                 0x0DF1
GL_FEEDBACK_BUFFER_TYPE : constant GLfeedback := 16#0DF2#; -- GL_FEEDBACK_BUFFER_TYPE                 0x0DF2

------------------------------------------------------------------------------
-- Selection
------------------------------------------------------------------------------

type GLselection is new GLenum;

GL_SELECTION_BUFFER_POINTER : constant Glselection := 16#0DF3#; -- GL_SELECTION_BUFFER_POINTER             0x0DF3
GL_SELECTION_BUFFER_SIZE : constant Glselection := 16#0DF4#; -- GL_SELECTION_BUFFER_SIZE                0x0DF4

------------------------------------------------------------------------------
-- Fog
------------------------------------------------------------------------------

type GLfog is new GLenum;

GL_FOG         : constant GLfog := 16#0B60#; -- GL_FOG 0x0B60
GL_FOG_MODE    : constant GLfog := 16#0B65#; -- GL_FOG_MODE 0x0B65
GL_FOG_DENSITY : constant GLfog := 16#0B62#; -- GL_FOG_DENSITY 0x0B62
GL_FOG_COLOR   : constant GLfog := 16#0B66#; -- GL_FOG_COLOR 0x0B66
GL_FOG_INDEX   : constant GLfog := 16#0B61#; -- GL_FOG_INDEX 0x0B61
GL_FOG_START   : constant GLfog := 16#0B63#; -- GL_FOG_START 0x0B63
GL_FOG_END     : constant GLfog := 16#0B64#; -- GL_FOG_END 0x0B64
GL_LINEAR      : constant GLfog := 16#2601#; -- GL_LINEAR 0x2601
GL_EXP         : constant GLfog := 16#0800#; -- GL_EXP 0x0800
GL_EXP2        : constant GLfog := 16#0801#; -- GL_EXP2 0x0801

------------------------------------------------------------------------------
-- Logic Ops
------------------------------------------------------------------------------

type GLlogicops is new GLenum;

GL_LOGIC_OP       : constant GLlogicops := 16#0BF1#; -- GL_LOGIC_OP 0x0BF1
GL_INDEX_LOGIC_OP : constant GLlogicops := 16#0BF1#; -- GL_INDEX_LOGIC_OP 0x0BF1
GL_COLOR_LOGIC_OP : constant GLlogicops := 16#0BF2#; -- GL_COLOR_LOGIC_OP 0x0BF2
GL_LOGIC_OP_MODE  : constant GLlogicops := 16#0BF0#; -- GL_LOGIC_OP_MODE 0x0BF0
GL_CLEAR          : constant GLlogicops := 16#1500#; -- GL_CLEAR 0x1500
GL_SET            : constant GLlogicops := 16#150F#; -- GL_SET 0x150F
GL_COPY           : constant GLlogicops := 16#1503#; -- GL_COPY 0x1503
GL_COPY_INVERTED  : constant GLlogicops := 16#150C#; -- GL_COPY_INVERTED 0x150C
GL_NOOP           : constant GLlogicops := 16#1505#; -- GL_NOOP 0x1505
GL_INVERT         : constant GLlogicops := 16#150A#; -- GL_INVERT 0x150A
GL_AND            : constant GLlogicops := 16#1501#; -- GL_AND 0x1501
GL_NAND           : constant GLlogicops := 16#150E#; -- GL_NAND 0x150E
GL_OR             : constant GLlogicops := 16#1507#; -- GL_OR 0x1507
GL_NOR            : constant GLlogicops := 16#1508#; -- GL_NOR 0x1508
GL_XOR            : constant GLlogicops := 16#1506#; -- GL_XOR 0x1506
GL_EQUIV          : constant GLlogicops := 16#1509#; -- GL_EQUIV 0x1509
GL_AND_REVERSE    : constant GLlogicops := 16#1502#; -- GL_AND_REVERSE 0x1502
GL_AND_INVERTED   : constant GLlogicops := 16#1504#; -- GL_AND_INVERTED 0x1504
GL_OR_REVERSE     : constant GLlogicops := 16#150B#; -- GL_OR_REVERSE 0x150B
GL_OR_INVERTED    : constant GLlogicops := 16#150D#; -- GL_OR_INVERTED 0x150D

------------------------------------------------------------------------------
-- Stencil
------------------------------------------------------------------------------

type GLstencil is new GLenum;

GL_STENCIL_BITS            : constant GLstencil := 16#0D57#; -- GL_STENCIL_BITS 0x0D57
GL_STENCIL_TEST            : constant GLstencil := 16#0B90#; -- GL_STENCIL_TEST 0x0B90
GL_STENCIL_CLEAR_VALUE     : constant GLstencil := 16#0B91#; -- GL_STENCIL_CLEAR_VALUE 0x0B91
GL_STENCIL_FUNC            : constant GLstencil := 16#0B92#; -- GL_STENCIL_FUNC 0x0B92
GL_STENCIL_VALUE_MASK      : constant GLstencil := 16#0B93#; -- GL_STENCIL_VALUE_MASK 0x0B93
GL_STENCIL_FAIL            : constant GLstencil := 16#0B94#; -- GL_STENCIL_FAIL 0x0B94
GL_STENCIL_PASS_DEPTH_FAIL : constant GLstencil := 16#0B95#; -- GL_STENCIL_PASS_DEPTH_FAIL 0x0B95
GL_STENCIL_PASS_DEPTH_PASS : constant GLstencil := 16#0B96#; -- GL_STENCIL_PASS_DEPTH_PASS 0x0B96
GL_STENCIL_REF             : constant GLstencil := 16#0B97#; -- GL_STENCIL_REF 0x0B97
GL_STENCIL_WRITEMASK       : constant GLstencil := 16#0B98#; -- GL_STENCIL_WRITEMASK 0x0B98
GL_STENCIL_INDEX           : constant GLstencil := 16#1901#; -- GL_STENCIL_INDEX 0x1901
GL_KEEP                    : constant GLstencil := 16#1E00#; -- GL_KEEP 0x1E00
GL_REPLACE                 : constant GLstencil := 16#1E01#; -- GL_REPLACE 0x1E01
GL_INCR                    : constant GLstencil := 16#1E02#; -- GL_INCR 0x1E02
GL_DECR                    : constant GLstencil := 16#1E03#; -- GL_DECR 0x1E03

------------------------------------------------------------------------------
-- Buffers, Pixel Drawing/Reading
------------------------------------------------------------------------------

type GLbuffers is new GLenum;

GL_NONE             : constant GLbuffers := 16#0#; -- GL_NONE 0x0
GL_LEFT             : constant GLbuffers := 16#0406#; -- GL_LEFT 0x0406
GL_RIGHT            : constant GLbuffers := 16#0407#; -- GL_RIGHT 0x0407
--/*GL_FRONT                                      0x0404 */
--/*GL_BACK                                       0x0405 */
--/*GL_FRONT_AND_BACK                            0x0408 */
GL_FRONT_LEFT       : constant GLbuffers := 16#0400#; -- GL_FRONT_LEFT 0x0400
GL_FRONT_RIGHT      : constant GLbuffers := 16#0401#; -- GL_FRONT_RIGHT 0x0401
GL_BACK_LEFT        : constant GLbuffers := 16#0402#; -- GL_BACK_LEFT 0x0402
GL_BACK_RIGHT       : constant GLbuffers := 16#0403#; -- GL_BACK_RIGHT 0x0403
GL_AUX0             : constant GLbuffers := 16#0409#; -- GL_AUX0 0x0409
GL_AUX1             : constant GLbuffers := 16#040A#; -- GL_AUX1 0x040A
GL_AUX2             : constant GLbuffers := 16#040B#; -- GL_AUX2 0x040B
GL_AUX3             : constant GLbuffers := 16#040C#; -- GL_AUX3 0x040C
GL_COLOR_INDEX      : constant GLbuffers := 16#1900#; -- GL_COLOR_INDEX 0x1900
GL_RED              : constant GLbuffers := 16#1903#; -- GL_RED 0x1903
GL_GREEN            : constant GLbuffers := 16#1904#; -- GL_GREEN 0x1904
GL_BLUE             : constant GLbuffers := 16#1905#; -- GL_BLUE 0x1905
GL_ALPHA            : constant GLbuffers := 16#1906#; -- GL_ALPHA 0x1906
GL_LUMINANCE        : constant GLbuffers := 16#1909#; -- GL_LUMINANCE 0x1909
GL_LUMINANCE_ALPHA  : constant GLbuffers := 16#190A#; -- GL_LUMINANCE_ALPHA 0x190A
GL_ALPHA_BITS       : constant GLbuffers := 16#0D55#; -- GL_ALPHA_BITS 0x0D55
GL_RED_BITS         : constant GLbuffers := 16#0D52#; -- GL_RED_BITS 0x0D52
GL_GREEN_BITS       : constant GLbuffers := 16#0D53#; -- GL_GREEN_BITS 0x0D53
GL_BLUE_BITS        : constant GLbuffers := 16#0D54#; -- GL_BLUE_BITS 0x0D54
GL_INDEX_BITS       : constant GLbuffers := 16#0D51#; -- GL_INDEX_BITS 0x0D51
GL_SUBPIXEL_BITS    : constant GLbuffers := 16#0D50#; -- GL_SUBPIXEL_BITS 0x0D50
GL_AUX_BUFFERS      : constant GLbuffers := 16#0C00#; -- GL_AUX_BUFFERS 0x0C00
GL_READ_BUFFER      : constant GLbuffers := 16#0C02#; -- GL_READ_BUFFER 0x0C02
GL_DRAW_BUFFER      : constant GLbuffers := 16#0C01#; -- GL_DRAW_BUFFER 0x0C01
GL_DOUBLEBUFFER     : constant GLbuffers := 16#0C32#; -- GL_DOUBLEBUFFER 0x0C32
GL_STEREO           : constant GLbuffers := 16#0C33#; -- GL_STEREO 0x0C33
GL_BITMAP           : constant GLbuffers := 16#1A00#; -- GL_BITMAP 0x1A00
GL_COLOR            : constant GLbuffers := 16#1800#; -- GL_COLOR 0x1800
GL_DEPTH            : constant GLbuffers := 16#1801#; -- GL_DEPTH 0x1801
GL_STENCIL          : constant GLbuffers := 16#1802#; -- GL_STENCIL 0x1802
GL_DITHER           : constant GLbuffers := 16#0BD0#; -- GL_DITHER 0x0BD0
GL_RGB              : constant GLbuffers := 16#1907#; -- GL_RGB 0x1907
GL_RGBA             : constant GLbuffers := 16#1908#; -- GL_RGBA 0x1908

------------------------------------------------------------------------------
-- Implementation limits
------------------------------------------------------------------------------

type GLlimits is new GLenum;

GL_MAX_LIST_NESTING : constant GLlimits := 16#0B31#;
GL_MAX_EVAL_ORDER : constant GLlimits := 16#0D30#;
GL_MAX_LIGHTS : constant GLlimits := 16#0D31#;
GL_MAX_CLIP_PLANES : constant GLlimits := 16#0D32#;
GL_MAX_TEXTURE_SIZE : constant GLlimits := 16#0D33#;
GL_MAX_PIXEL_MAP_TABLE : constant GLlimits := 16#0D34#;
GL_MAX_ATTRIB_STACK_DEPTH : constant GLlimits := 16#0D35#;
GL_MAX_MODELVIEW_STACK_DEPTH : constant GLlimits := 16#0D36#;
GL_MAX_NAME_STACK_DEPTH : constant GLlimits := 16#0D37#;
GL_MAX_PROJECTION_STACK_DEPTH : constant GLlimits := 16#0D38#;
GL_MAX_TEXTURE_STACK_DEPTH : constant GLlimits := 16#0D39#;
GL_MAX_VIEWPORT_DIMS : constant GLlimits := 16#0D3A#;
GL_MAX_CLIENT_ATTRIB_STACK_DEPTH : constant GLlimits:= 16#0D3B#;
GL_ATTRIB_STACK_DEPTH : constant GLlimits := 16#0BB0#;
GL_CLIENT_ATTRIB_STACK_DEPTH : constant GLlimits := 16#0BB1#;

------------------------------------------------------------------------------
-- Gets
------------------------------------------------------------------------------

type GLgets is new GLenum;

GL_COLOR_CLEAR_VALUE : constant GLgets := 16#0C22#;
GL_COLOR_WRITEMASK : constant GLgets := 16#0C23#;
GL_CURRENT_INDEX : constant GLgets := 16#0B01#;
GL_CURRENT_COLOR : constant GLgets := 16#0B00#;
GL_CURRENT_NORMAL : constant GLgets := 16#0B02#;
GL_CURRENT_RASTER_COLOR : constant GLgets := 16#0B04#;
GL_CURRENT_RASTER_DISTANCE : constant GLgets := 16#0B09#;
GL_CURRENT_RASTER_INDEX : constant GLgets := 16#0B05#;
GL_CURRENT_RASTER_POSITION : constant GLgets := 16#0B07#;
GL_CURRENT_RASTER_TEXTURE_COORDS : constant GLgets := 16#0B06#;
GL_CURRENT_RASTER_POSITION_VALID : constant GLgets := 16#0B08#;
GL_CURRENT_TEXTURE_COORDS : constant GLgets := 16#0B03#;
GL_INDEX_CLEAR_VALUE : constant GLgets := 16#0C20#;
GL_INDEX_MODE : constant GLgets := 16#0C30#;
GL_INDEX_WRITEMASK : constant GLgets := 16#0C21#;
GL_MODELVIEW_MATRIX : constant GLgets := 16#0BA6#;
GL_MODELVIEW_STACK_DEPTH : constant GLgets := 16#0BA3#;
GL_NAME_STACK_DEPTH : constant GLgets := 16#0D70#;
GL_PROJECTION_MATRIX : constant GLgets := 16#0BA7#;
GL_PROJECTION_STACK_DEPTH : constant GLgets := 16#0BA4#;
GL_RENDER_MODE : constant GLgets := 16#0C40#;
GL_RGBA_MODE : constant GLgets := 16#0C31#;
GL_TEXTURE_MATRIX : constant GLgets := 16#0BA8#;
GL_TEXTURE_STACK_DEPTH : constant GLgets := 16#0BA5#;
GL_VIEWPORT : constant GLgets := 16#0BA2#;

------------------------------------------------------------------------------
-- Evaluators
------------------------------------------------------------------------------

type GLevaluators is new GLenum;

GL_AUTO_NORMAL : constant GLevaluators := 16#0D80#;
GL_MAP1_COLOR_4 : constant GLevaluators := 16#0D90#;
GL_MAP1_INDEX : constant GLevaluators := 16#0D91#;
GL_MAP1_NORMAL : constant GLevaluators := 16#0D92#;
GL_MAP1_TEXTURE_COORD_1 : constant GLevaluators := 16#0D93#;
GL_MAP1_TEXTURE_COORD_2 : constant GLevaluators := 16#0D94#;
GL_MAP1_TEXTURE_COORD_3 : constant GLevaluators := 16#0D95#;
GL_MAP1_TEXTURE_COORD_4 : constant GLevaluators := 16#0D96#;
GL_MAP1_VERTEX_3 : constant GLevaluators := 16#0D97#;
GL_MAP1_VERTEX_4 : constant GLevaluators := 16#0D98#;
GL_MAP2_COLOR_4 : constant GLevaluators := 16#0DB0#;
GL_MAP2_INDEX : constant GLevaluators := 16#0DB1#;
GL_MAP2_NORMAL : constant GLevaluators := 16#0DB2#;
GL_MAP2_TEXTURE_COORD_1 : constant GLevaluators := 16#0DB3#;
GL_MAP2_TEXTURE_COORD_2 : constant GLevaluators := 16#0DB4#;
GL_MAP2_TEXTURE_COORD_3 : constant GLevaluators := 16#0DB5#;
GL_MAP2_TEXTURE_COORD_4 : constant GLevaluators := 16#0DB6#;
GL_MAP2_VERTEX_3 : constant GLevaluators := 16#0DB7#;
GL_MAP2_VERTEX_4 : constant GLevaluators := 16#0DB8#;
GL_MAP1_GRID_DOMAIN : constant GLevaluators := 16#0DD0#;
GL_MAP1_GRID_SEGMENTS : constant GLevaluators := 16#0DD1#;
GL_MAP2_GRID_DOMAIN : constant GLevaluators := 16#0DD2#;
GL_MAP2_GRID_SEGMENTS : constant GLevaluators := 16#0DD3#;
GL_COEFF : constant GLevaluators := 16#0A00#;
GL_ORDER : constant GLevaluators := 16#0A01#;
GL_DOMAIN : constant GLevaluators := 16#0A02#;

------------------------------------------------------------------------------
-- Hints
------------------------------------------------------------------------------

type GLhints is new GLenum;

GL_PERSPECTIVE_CORRECTION_HINT : constant GLhints := 16#0C50#;
GL_POINT_SMOOTH_HINT : constant GLhints := 16#0C51#;
GL_LINE_SMOOTH_HINT : constant GLhints := 16#0C52#;
GL_POLYGON_SMOOTH_HINT : constant GLhints := 16#0C53#;
GL_FOG_HINT : constant GLhints := 16#0C54#;

type GLhintmodes is new GLenum;

GL_DONT_CARE : constant GLhintmodes := 16#1100#;
GL_FASTEST : constant GLhintmodes := 16#1101#;
GL_NICEST : constant GLhintmodes := 16#1102#;

------------------------------------------------------------------------------
-- Scissor Box
------------------------------------------------------------------------------

type GLscissorbox is new GLenum;

GL_SCISSOR_BOX : constant GLscissorbox := 16#0C10#;
GL_SCISSOR_TEST : constant GLscissorbox := 16#0C11#;

------------------------------------------------------------------------------
-- Pixel Mode / Transfer
------------------------------------------------------------------------------

type GLpixelmode is new GLenum;

GL_MAP_COLOR : constant GLpixelmode := 16#0D10#;
GL_MAP_STENCIL : constant GLpixelmode := 16#0D11#;
GL_INDEX_SHIFT : constant GLpixelmode := 16#0D12#;
GL_INDEX_OFFSET : constant GLpixelmode := 16#0D13#;
GL_RED_SCALE : constant GLpixelmode := 16#0D14#;
GL_RED_BIAS : constant GLpixelmode := 16#0D15#;
GL_GREEN_SCALE : constant GLpixelmode := 16#0D18#;
GL_GREEN_BIAS : constant GLpixelmode := 16#0D19#;
GL_BLUE_SCALE : constant GLpixelmode := 16#0D1A#;
GL_BLUE_BIAS : constant GLpixelmode := 16#0D1B#;
GL_ALPHA_SCALE : constant GLpixelmode := 16#0D1C#;
GL_ALPHA_BIAS : constant GLpixelmode := 16#0D1D#;
GL_DEPTH_SCALE : constant GLpixelmode := 16#0D1E#;
GL_DEPTH_BIAS : constant GLpixelmode := 16#0D1F#;
GL_PIXEL_MAP_S_TO_S_SIZE : constant GLpixelmode := 16#0CB1#;
GL_PIXEL_MAP_I_TO_I_SIZE : constant GLpixelmode := 16#0CB0#;
GL_PIXEL_MAP_I_TO_R_SIZE : constant GLpixelmode := 16#0CB2#;
GL_PIXEL_MAP_I_TO_G_SIZE : constant GLpixelmode := 16#0CB3#;
GL_PIXEL_MAP_I_TO_B_SIZE : constant GLpixelmode := 16#0CB4#;
GL_PIXEL_MAP_I_TO_A_SIZE : constant GLpixelmode := 16#0CB5#;
GL_PIXEL_MAP_R_TO_R_SIZE : constant GLpixelmode := 16#0CB6#;
GL_PIXEL_MAP_G_TO_G_SIZE : constant GLpixelmode := 16#0CB7#;
GL_PIXEL_MAP_B_TO_B_SIZE : constant GLpixelmode := 16#0CB8#;
GL_PIXEL_MAP_A_TO_A_SIZE : constant GLpixelmode := 16#0CB9#;
GL_PIXEL_MAP_S_TO_S : constant GLpixelmode := 16#0C71#;
GL_PIXEL_MAP_I_TO_I : constant GLpixelmode := 16#0C70#;
GL_PIXEL_MAP_I_TO_R : constant GLpixelmode := 16#0C72#;
GL_PIXEL_MAP_I_TO_G : constant GLpixelmode := 16#0C73#;
GL_PIXEL_MAP_I_TO_B : constant GLpixelmode := 16#0C74#;
GL_PIXEL_MAP_I_TO_A : constant GLpixelmode := 16#0C75#;
GL_PIXEL_MAP_R_TO_R : constant GLpixelmode := 16#0C76#;
GL_PIXEL_MAP_G_TO_G : constant GLpixelmode := 16#0C77#;
GL_PIXEL_MAP_B_TO_B : constant GLpixelmode := 16#0C78#;
GL_PIXEL_MAP_A_TO_A : constant GLpixelmode := 16#0C79#;
GL_PACK_ALIGNMENT : constant GLpixelmode := 16#0D05#;
GL_PACK_LSB_FIRST : constant GLpixelmode := 16#0D01#;
GL_PACK_ROW_LENGTH : constant GLpixelmode := 16#0D02#;
GL_PACK_SKIP_PIXELS : constant GLpixelmode := 16#0D04#;
GL_PACK_SKIP_ROWS : constant GLpixelmode := 16#0D03#;
GL_PACK_SWAP_BYTES : constant GLpixelmode := 16#0D00#;
GL_UNPACK_ALIGNMENT : constant GLpixelmode := 16#0CF5#;
GL_UNPACK_LSB_FIRST : constant GLpixelmode := 16#0CF1#;
GL_UNPACK_ROW_LENGTH : constant GLpixelmode := 16#0CF2#;
GL_UNPACK_SKIP_PIXELS : constant GLpixelmode := 16#0CF4#;
GL_UNPACK_SKIP_ROWS : constant GLpixelmode := 16#0CF3#;
GL_UNPACK_SWAP_BYTES : constant GLpixelmode := 16#0CF0#;
GL_ZOOM_X : constant GLpixelmode := 16#0D16#;
GL_ZOOM_Y : constant GLpixelmode := 16#0D17#;

------------------------------------------------------------------------------
-- Texture Mapping
------------------------------------------------------------------------------

type GLtexturemapping is new GLenum;

GL_TEXTURE_ENV : constant GLtexturemapping:= 16#2300#;
GL_TEXTURE_ENV_MODE : constant GLtexturemapping:= 16#2200#;
GL_TEXTURE_1D : constant GLtexturemapping:= 16#0DE0#;
GL_TEXTURE_2D : constant GLtexturemapping:= 16#0DE1#;
GL_TEXTURE_WRAP_S : constant GLtexturemapping:= 16#2802#;
GL_TEXTURE_WRAP_T : constant GLtexturemapping:= 16#2803#;
GL_TEXTURE_MAG_FILTER : constant GLtexturemapping:= 16#2800#;
GL_TEXTURE_MIN_FILTER : constant GLtexturemapping:= 16#2801#;
GL_TEXTURE_ENV_COLOR : constant GLtexturemapping:= 16#2201#;
GL_TEXTURE_GEN_S : constant GLtexturemapping:= 16#0C60#;
GL_TEXTURE_GEN_T : constant GLtexturemapping:= 16#0C61#;
GL_TEXTURE_GEN_MODE : constant GLtexturemapping:= 16#2500#;
GL_TEXTURE_BORDER_COLOR : constant GLtexturemapping:= 16#1004#;
GL_TEXTURE_WIDTH : constant GLtexturemapping:= 16#1000#;
GL_TEXTURE_HEIGHT : constant GLtexturemapping:= 16#1001#;
GL_TEXTURE_BORDER : constant GLtexturemapping:= 16#1005#;
GL_TEXTURE_COMPONENTS : constant GLtexturemapping:= 16#1003#;
GL_TEXTURE_RED_SIZE : constant GLtexturemapping:= 16#805C#;
GL_TEXTURE_GREEN_SIZE : constant GLtexturemapping:= 16#805D#;
GL_TEXTURE_BLUE_SIZE : constant GLtexturemapping:= 16#805E#;
GL_TEXTURE_ALPHA_SIZE : constant GLtexturemapping:= 16#805F#;
GL_TEXTURE_LUMINANCE_SIZE : constant GLtexturemapping:= 16#8060#;
GL_TEXTURE_INTENSITY_SIZE : constant GLtexturemapping:= 16#8061#;
GL_NEAREST_MIPMAP_NEAREST : constant GLtexturemapping:= 16#2700#;
GL_NEAREST_MIPMAP_LINEAR : constant GLtexturemapping:= 16#2702#;
GL_LINEAR_MIPMAP_NEAREST : constant GLtexturemapping:= 16#2701#;
GL_LINEAR_MIPMAP_LINEAR : constant GLtexturemapping:= 16#2703#;
GL_OBJECT_LINEAR : constant GLtexturemapping:= 16#2401#;
GL_OBJECT_PLANE : constant GLtexturemapping:= 16#2501#;
GL_EYE_LINEAR : constant GLtexturemapping:= 16#2400#;
GL_EYE_PLANE : constant GLtexturemapping:= 16#2502#;
GL_SPHERE_MAP : constant GLtexturemapping:= 16#2402#;
GL_DECAL : constant GLtexturemapping:= 16#2101#;
GL_MODULATE : constant GLtexturemapping:= 16#2100#;
GL_NEAREST : constant GLtexturemapping:= 16#2600#;
GL_REPEAT : constant GLtexturemapping:= 16#2901#;
GL_CLAMP : constant GLtexturemapping:= 16#2900#;
GL_S : constant GLtexturemapping:= 16#2000#;
GL_T : constant GLtexturemapping:= 16#2001#;
GL_R : constant GLtexturemapping:= 16#2002#;
GL_Q : constant GLtexturemapping:= 16#2003#;
GL_TEXTURE_GEN_R : constant GLtexturemapping:= 16#0C62#;
GL_TEXTURE_GEN_Q : constant GLtexturemapping:= 16#0C63#;

------------------------------------------------------------------------------
-- Utility
------------------------------------------------------------------------------

type GLutility is new GLenum;

GL_VENDOR : constant GLutility := 16#1F00#;
GL_RENDERER : constant GLutility := 16#1F01#;
GL_VERSION : constant GLutility := 16#1F02#;
GL_EXTENSIONS : constant GLutility := 16#1F03#;

------------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------------

type GLerrors is new GLenum;

GL_NO_ERROR  : constant GLerrors := 16#0#;
GL_INVALID_ENUM : constant GLerrors := 16#0500#;
GL_INVALID_VALUE : constant GLerrors := 16#0501#;
GL_INVALID_OPERATION : constant GLerrors := 16#0502#;
GL_STACK_OVERFLOW : constant GLerrors := 16#0503#;
GL_STACK_UNDERFLOW : constant GLerrors := 16#0504#;
GL_OUT_OF_MEMORY : constant GLerrors := 16#0505#;

------------------------------------------------------------------------------
-- glPush/PopAttrib bits
------------------------------------------------------------------------------

type GLpushbits is new GLenum;

GL_CURRENT_BIT : constant GLpushbits := 16#00000001#;
GL_POINT_BIT : constant GLpushbits := 16#00000002#;
GL_LINE_BIT : constant GLpushbits := 16#00000004#;
GL_POLYGON_BIT : constant GLpushbits := 16#00000008#;
GL_POLYGON_STIPPLE_BIT : constant GLpushbits := 16#00000010#;
GL_PIXEL_MODE_BIT : constant GLpushbits := 16#00000020#;
GL_LIGHTING_BIT : constant GLpushbits := 16#00000040#;
GL_FOG_BIT : constant GLpushbits := 16#00000080#;
GL_DEPTH_BUFFER_BIT : constant GLpushbits := 16#00000100#;
GL_ACCUM_BUFFER_BIT : constant GLpushbits := 16#00000200#;
GL_STENCIL_BUFFER_BIT : constant GLpushbits := 16#00000400#;
GL_VIEWPORT_BIT : constant GLpushbits := 16#00000800#;
GL_TRANSFORM_BIT : constant GLpushbits := 16#00001000#;
GL_ENABLE_BIT : constant GLpushbits := 16#00002000#;
GL_COLOR_BUFFER_BIT : constant GLpushbits := 16#00004000#;
GL_HINT_BIT : constant GLpushbits := 16#00008000#;
GL_EVAL_BIT : constant GLpushbits := 16#00010000#;
GL_LIST_BIT : constant GLpushbits := 16#00020000#;
GL_TEXTURE_BIT : constant GLpushbits := 16#00040000#;
GL_SCISSOR_BIT : constant GLpushbits := 16#00080000#;
GL_ALL_ATTRIB_BITS : constant GLpushbits := 16#000FFFFF#;

------------------------------------------------------------------------------
-- OpenGL 1.1
------------------------------------------------------------------------------

GL_PROXY_TEXTURE_1D : constant GLenum := 16#8063#;
GL_PROXY_TEXTURE_2D : constant GLenum := 16#8064#;
GL_TEXTURE_PRIORITY : constant GLenum := 16#8066#;
GL_TEXTURE_RESIDENT : constant GLenum := 16#8067#;
GL_TEXTURE_BINDING_1D : constant GLenum := 16#8068#;
GL_TEXTURE_BINDING_2D : constant GLenum := 16#8069#;
GL_TEXTURE_INTERNAL_FORMAT : constant GLenum := 16#1003#;
GL_ALPHA4 : constant GLenum := 16#803B#;
GL_ALPHA8 : constant GLenum := 16#803C#;
GL_ALPHA12 : constant GLenum := 16#803D#;
GL_ALPHA16 : constant GLenum := 16#803E#;
GL_LUMINANCE4 : constant GLenum := 16#803F#;
GL_LUMINANCE8 : constant GLenum := 16#8040#;
GL_LUMINANCE12 : constant GLenum := 16#8041#;
GL_LUMINANCE16 : constant GLenum := 16#8042#;
GL_LUMINANCE4_ALPHA4 : constant GLenum := 16#8043#;
GL_LUMINANCE6_ALPHA2 : constant GLenum := 16#8044#;
GL_LUMINANCE8_ALPHA8 : constant GLenum := 16#8045#;
GL_LUMINANCE12_ALPHA4 : constant GLenum := 16#8046#;
GL_LUMINANCE12_ALPHA12 : constant GLenum := 16#8047#;
GL_LUMINANCE16_ALPHA16 : constant GLenum := 16#8048#;
GL_INTENSITY : constant GLenum := 16#8049#;
GL_INTENSITY4 : constant GLenum := 16#804A#;
GL_INTENSITY8 : constant GLenum := 16#804B#;
GL_INTENSITY12 : constant GLenum := 16#804C#;
GL_INTENSITY16 : constant GLenum := 16#804D#;
GL_R3_G3_B2 : constant GLenum := 16#2A10#;
GL_RGB4 : constant GLenum := 16#804F#;
GL_RGB5 : constant GLenum := 16#8050#;
GL_RGB8 : constant GLenum := 16#8051#;
GL_RGB10 : constant GLenum := 16#8052#;
GL_RGB12 : constant GLenum := 16#8053#;
GL_RGB16 : constant GLenum := 16#8054#;
GL_RGBA2 : constant GLenum := 16#8055#;
GL_RGBA4 : constant GLenum := 16#8056#;
GL_RGB5_A1 : constant GLenum := 16#8057#;
GL_RGBA8 : constant GLenum := 16#8058#;
GL_RGB10_A2 : constant GLenum := 16#8059#;
GL_RGBA12 : constant GLenum := 16#805A#;
GL_RGBA16 : constant GLenum := 16#805B#;
GL_CLIENT_PIXEL_STORE_BIT : constant GLenum := 16#00000001#;
GL_CLIENT_VERTEX_ARRAY_BIT : constant GLenum := 16#00000002#;
GL_ALL_CLIENT_ATTRIB_BITS  : constant GLenum := 16#FFFFFFFF#;
GL_CLIENT_ALL_ATTRIB_BITS  : constant GLenum := 16#FFFFFFFF#;

------------------------------------------------------------------------------
-- OpenGL 1.2
------------------------------------------------------------------------------

GL_RESCALE_NORMAL : constant GLenum := 16#803A#;
GL_CLAMP_TO_EDGE : constant GLenum := 16#812F#;
GL_MAX_ELEMENTS_VERTICES : constant GLenum := 16#80E8#;
GL_MAX_ELEMENTS_INDICES : constant GLenum := 16#80E9#;
GL_BGR : constant GLenum := 16#80E0#;
GL_BGRA : constant GLenum := 16#80E1#;
GL_UNSIGNED_BYTE_3_3_2 : constant GLenum := 16#8032#;
GL_UNSIGNED_BYTE_2_3_3_REV : constant GLenum := 16#8362#;
GL_UNSIGNED_SHORT_5_6_5 : constant GLenum := 16#8363#;
GL_UNSIGNED_SHORT_5_6_5_REV : constant GLenum := 16#8364#;
GL_UNSIGNED_SHORT_4_4_4_4 : constant GLenum := 16#8033#;
GL_UNSIGNED_SHORT_4_4_4_4_REV : constant GLenum := 16#8365#;
GL_UNSIGNED_SHORT_5_5_5_1 : constant GLenum := 16#8034#;
GL_UNSIGNED_SHORT_1_5_5_5_REV : constant GLenum := 16#8366#;
GL_UNSIGNED_INT_8_8_8_8 : constant GLenum := 16#8035#;
GL_UNSIGNED_INT_8_8_8_8_REV : constant GLenum := 16#8367#;
GL_UNSIGNED_INT_10_10_10_2 : constant GLenum := 16#8036#;
GL_UNSIGNED_INT_2_10_10_10_REV : constant GLenum := 16#8368#;
GL_LIGHT_MODEL_COLOR_CONTROL : constant GLenum := 16#81F8#;
GL_SINGLE_COLOR : constant GLenum := 16#81F9#;
GL_SEPARATE_SPECULAR_COLOR : constant GLenum := 16#81FA#;
GL_TEXTURE_MIN_LOD : constant GLenum := 16#813A#;
GL_TEXTURE_MAX_LOD : constant GLenum := 16#813B#;
GL_TEXTURE_BASE_LEVEL : constant GLenum := 16#813C#;
GL_TEXTURE_MAX_LEVEL : constant GLenum := 16#813D#;
GL_SMOOTH_POINT_SIZE_RANGE : constant GLenum := 16#0B12#;
GL_SMOOTH_POINT_SIZE_GRANULARITY : constant GLenum := 16#0B13#;
GL_SMOOTH_LINE_WIDTH_RANGE : constant GLenum := 16#0B22#;
GL_SMOOTH_LINE_WIDTH_GRANULARITY : constant GLenum := 16#0B23#;
GL_ALIASED_POINT_SIZE_RANGE : constant GLenum := 16#846D#;
GL_ALIASED_LINE_WIDTH_RANGE : constant GLenum := 16#846E#;
GL_PACK_SKIP_IMAGES : constant GLenum := 16#806B#;
GL_PACK_IMAGE_HEIGHT : constant GLenum := 16#806C#;
GL_UNPACK_SKIP_IMAGES : constant GLenum := 16#806D#;
GL_UNPACK_IMAGE_HEIGHT : constant GLenum := 16#806E#;
GL_TEXTURE_3D : constant GLenum := 16#806F#;
GL_PROXY_TEXTURE_3D : constant GLenum := 16#8070#;
GL_TEXTURE_DEPTH : constant GLenum := 16#8071#;
GL_TEXTURE_WRAP_R : constant GLenum := 16#8072#;
GL_MAX_3D_TEXTURE_SIZE : constant GLenum := 16#8073#;
GL_TEXTURE_BINDING_3D : constant GLenum := 16#806A#;

------------------------------------------------------------------------------
-- GL_ARB_imaging
------------------------------------------------------------------------------

type GLarbmapping is new GLenum;

GL_CONSTANT_COLOR : constant GLarbmapping := 16#8001#;
GL_ONE_MINUS_CONSTANT_COLOR : constant GLarbmapping := 16#8002#;
GL_CONSTANT_ALPHA : constant GLarbmapping := 16#8003#;
GL_ONE_MINUS_CONSTANT_ALPHA : constant GLarbmapping := 16#8004#;
GL_COLOR_TABLE : constant GLarbmapping := 16#80D0#;
GL_POST_CONVOLUTION_COLOR_TABLE : constant GLarbmapping := 16#80D1#;
GL_POST_COLOR_MATRIX_COLOR_TABLE : constant GLarbmapping := 16#80D2#;
GL_PROXY_COLOR_TABLE : constant GLarbmapping := 16#80D3#;
GL_PROXY_POST_CONVOLUTION_COLOR_TABLE : constant GLarbmapping := 16#80D4#;
GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE : constant GLarbmapping := 16#80D5#;
GL_COLOR_TABLE_SCALE : constant GLarbmapping := 16#80D6#;
GL_COLOR_TABLE_BIAS : constant GLarbmapping := 16#80D7#;
GL_COLOR_TABLE_FORMAT : constant GLarbmapping := 16#80D8#;
GL_COLOR_TABLE_WIDTH : constant GLarbmapping := 16#80D9#;
GL_COLOR_TABLE_RED_SIZE : constant GLarbmapping := 16#80DA#;
GL_COLOR_TABLE_GREEN_SIZE : constant GLarbmapping := 16#80DB#;
GL_COLOR_TABLE_BLUE_SIZE : constant GLarbmapping := 16#80DC#;
GL_COLOR_TABLE_ALPHA_SIZE : constant GLarbmapping := 16#80DD#;
GL_COLOR_TABLE_LUMINANCE_SIZE : constant GLarbmapping := 16#80DE#;
GL_COLOR_TABLE_INTENSITY_SIZE : constant GLarbmapping := 16#80DF#;
GL_CONVOLUTION_1D : constant GLarbmapping := 16#8010#;
GL_CONVOLUTION_2D : constant GLarbmapping := 16#8011#;
GL_SEPARABLE_2D : constant GLarbmapping := 16#8012#;
GL_CONVOLUTION_BORDER_MODE : constant GLarbmapping := 16#8013#;
GL_CONVOLUTION_FILTER_SCALE : constant GLarbmapping := 16#8014#;
GL_CONVOLUTION_FILTER_BIAS : constant GLarbmapping := 16#8015#;
GL_REDUCE : constant GLarbmapping := 16#8016#;
GL_CONVOLUTION_FORMAT : constant GLarbmapping := 16#8017#;
GL_CONVOLUTION_WIDTH : constant GLarbmapping := 16#8018#;
GL_CONVOLUTION_HEIGHT : constant GLarbmapping := 16#8019#;
GL_MAX_CONVOLUTION_WIDTH : constant GLarbmapping := 16#801A#;
GL_MAX_CONVOLUTION_HEIGHT : constant GLarbmapping := 16#801B#;
GL_POST_CONVOLUTION_RED_SCALE : constant GLarbmapping := 16#801C#;
GL_POST_CONVOLUTION_GREEN_SCALE : constant GLarbmapping := 16#801D#;
GL_POST_CONVOLUTION_BLUE_SCALE : constant GLarbmapping := 16#801E#;
GL_POST_CONVOLUTION_ALPHA_SCALE : constant GLarbmapping := 16#801F#;
GL_POST_CONVOLUTION_RED_BIAS : constant GLarbmapping := 16#8020#;
GL_POST_CONVOLUTION_GREEN_BIAS : constant GLarbmapping := 16#8021#;
GL_POST_CONVOLUTION_BLUE_BIAS : constant GLarbmapping := 16#8022#;
GL_POST_CONVOLUTION_ALPHA_BIAS : constant GLarbmapping := 16#8023#;
GL_CONSTANT_BORDER : constant GLarbmapping := 16#8151#;
GL_REPLICATE_BORDER : constant GLarbmapping := 16#8153#;
GL_CONVOLUTION_BORDER_COLOR : constant GLarbmapping := 16#8154#;
GL_COLOR_MATRIX : constant GLarbmapping := 16#80B1#;
GL_COLOR_MATRIX_STACK_DEPTH : constant GLarbmapping := 16#80B2#;
GL_MAX_COLOR_MATRIX_STACK_DEPTH : constant GLarbmapping := 16#80B3#;
GL_POST_COLOR_MATRIX_RED_SCALE : constant GLarbmapping := 16#80B4#;
GL_POST_COLOR_MATRIX_GREEN_SCALE : constant GLarbmapping := 16#80B5#;
GL_POST_COLOR_MATRIX_BLUE_SCALE : constant GLarbmapping := 16#80B6#;
GL_POST_COLOR_MATRIX_ALPHA_SCALE : constant GLarbmapping := 16#80B7#;
GL_POST_COLOR_MATRIX_RED_BIAS : constant GLarbmapping := 16#80B8#;
GL_POST_COLOR_MATRIX_GREEN_BIAS : constant GLarbmapping := 16#80B9#;
GL_POST_COLOR_MATRIX_BLUE_BIAS : constant GLarbmapping := 16#80BA#;
GL_POST_COLOR_MATRIX_ALPHA_BIAS : constant GLarbmapping := 16#80BB#;
GL_HISTOGRAM : constant GLarbmapping := 16#8024#;
GL_PROXY_HISTOGRAM : constant GLarbmapping := 16#8025#;
GL_HISTOGRAM_WIDTH : constant GLarbmapping := 16#8026#;
GL_HISTOGRAM_FORMAT : constant GLarbmapping := 16#8027#;
GL_HISTOGRAM_RED_SIZE : constant GLarbmapping := 16#8028#;
GL_HISTOGRAM_GREEN_SIZE : constant GLarbmapping := 16#8029#;
GL_HISTOGRAM_BLUE_SIZE : constant GLarbmapping := 16#802A#;
GL_HISTOGRAM_ALPHA_SIZE : constant GLarbmapping := 16#802B#;
GL_HISTOGRAM_LUMINANCE_SIZE : constant GLarbmapping := 16#802C#;
GL_HISTOGRAM_SINK : constant GLarbmapping := 16#802D#;
GL_MINMAX : constant GLarbmapping := 16#802E#;
GL_MINMAX_FORMAT : constant GLarbmapping := 16#802F#;
GL_MINMAX_SINK : constant GLarbmapping := 16#8030#;
GL_TABLE_TOO_LARGE : constant GLarbmapping := 16#8031#;
GL_BLEND_EQUATION : constant GLarbmapping := 16#8009#;
GL_MIN : constant GLarbmapping := 16#8007#;
GL_MAX : constant GLarbmapping := 16#8008#;
GL_FUNC_ADD : constant GLarbmapping := 16#8006#;
GL_FUNC_SUBTRACT : constant GLarbmapping := 16#800A#;
GL_FUNC_REVERSE_SUBTRACT : constant GLarbmapping := 16#800B#;
GL_BLEND_COLOR : constant GLarbmapping := 16#8005#;

------------------------------------------------------------------------------
-- OpenGL 1.3 mulit-texture
------------------------------------------------------------------------------

type GLmultitexture is new GLenum;

GL_TEXTURE0 : constant GLmultitexture := 16#84C0#;
GL_TEXTURE1 : constant GLmultitexture := 16#84C1#;
GL_TEXTURE2 : constant GLmultitexture := 16#84C2#;
GL_TEXTURE3 : constant GLmultitexture := 16#84C3#;
GL_TEXTURE4 : constant GLmultitexture := 16#84C4#;
GL_TEXTURE5 : constant GLmultitexture := 16#84C5#;
GL_TEXTURE6 : constant GLmultitexture := 16#84C6#;
GL_TEXTURE7 : constant GLmultitexture := 16#84C7#;
GL_TEXTURE8 : constant GLmultitexture := 16#84C8#;
GL_TEXTURE9 : constant GLmultitexture := 16#84C9#;
GL_TEXTURE10 : constant GLmultitexture := 16#84CA#;
GL_TEXTURE11 : constant GLmultitexture := 16#84CB#;
GL_TEXTURE12 : constant GLmultitexture := 16#84CC#;
GL_TEXTURE13 : constant GLmultitexture := 16#84CD#;
GL_TEXTURE14 : constant GLmultitexture := 16#84CE#;
GL_TEXTURE15 : constant GLmultitexture := 16#84CF#;
GL_TEXTURE16 : constant GLmultitexture := 16#84D0#;
GL_TEXTURE17 : constant GLmultitexture := 16#84D1#;
GL_TEXTURE18 : constant GLmultitexture := 16#84D2#;
GL_TEXTURE19 : constant GLmultitexture := 16#84D3#;
GL_TEXTURE20 : constant GLmultitexture := 16#84D4#;
GL_TEXTURE21 : constant GLmultitexture := 16#84D5#;
GL_TEXTURE22 : constant GLmultitexture := 16#84D6#;
GL_TEXTURE23 : constant GLmultitexture := 16#84D7#;
GL_TEXTURE24 : constant GLmultitexture := 16#84D8#;
GL_TEXTURE25 : constant GLmultitexture := 16#84D9#;
GL_TEXTURE26 : constant GLmultitexture := 16#84DA#;
GL_TEXTURE27 : constant GLmultitexture := 16#84DB#;
GL_TEXTURE28 : constant GLmultitexture := 16#84DC#;
GL_TEXTURE29 : constant GLmultitexture := 16#84DD#;
GL_TEXTURE30 : constant GLmultitexture := 16#84DE#;
GL_TEXTURE31 : constant GLmultitexture := 16#84DF#;
GL_ACTIVE_TEXTURE : constant GLmultitexture := 16#84E0#;
GL_CLIENT_ACTIVE_TEXTURE : constant GLmultitexture := 16#84E1#;
GL_MAX_TEXTURE_UNITS : constant GLmultitexture := 16#84E2#;

------------------------------------------------------------------------------
-- OpenGL 1.3 texture cube map
------------------------------------------------------------------------------

type GLtexturecubemap is new GLenum;

GL_NORMAL_MAP : constant GLtexturecubemap := 16#8511#;
GL_REFLECTION_MAP : constant GLtexturecubemap := 16#8512#;
GL_TEXTURE_CUBE_MAP : constant GLtexturecubemap := 16#8513#;
GL_TEXTURE_BINDING_CUBE_MAP : constant GLtexturecubemap := 16#8514#;
GL_TEXTURE_CUBE_MAP_POSITIVE_X : constant GLtexturecubemap := 16#8515#;
GL_TEXTURE_CUBE_MAP_NEGATIVE_X : constant GLtexturecubemap := 16#8516#;
GL_TEXTURE_CUBE_MAP_POSITIVE_Y : constant GLtexturecubemap := 16#8517#;
GL_TEXTURE_CUBE_MAP_NEGATIVE_Y : constant GLtexturecubemap := 16#8518#;
GL_TEXTURE_CUBE_MAP_POSITIVE_Z : constant GLtexturecubemap := 16#8519#;
GL_TEXTURE_CUBE_MAP_NEGATIVE_Z : constant GLtexturecubemap := 16#851A#;
GL_PROXY_TEXTURE_CUBE_MAP : constant GLtexturecubemap := 16#851B#;
GL_MAX_CUBE_MAP_TEXTURE_SIZE : constant GLtexturecubemap := 16#851C#;

------------------------------------------------------------------------------
-- OpenGL 1.3 texture compression
------------------------------------------------------------------------------

type GLtexturecomp is new GLenum;

GL_COMPRESSED_ALPHA : constant GLtexturecomp := 16#84E9#;
GL_COMPRESSED_LUMINANCE : constant GLtexturecomp := 16#84EA#;
GL_COMPRESSED_LUMINANCE_ALPHA : constant GLtexturecomp := 16#84EB#;
GL_COMPRESSED_INTENSITY : constant GLtexturecomp := 16#84EC#;
GL_COMPRESSED_RGB : constant GLtexturecomp := 16#84ED#;
GL_COMPRESSED_RGBA : constant GLtexturecomp := 16#84EE#;
GL_TEXTURE_COMPRESSION_HINT : constant GLtexturecomp := 16#84EF#;
GL_TEXTURE_COMPRESSED_IMAGE_SIZE : constant GLtexturecomp := 16#86A0#;
GL_TEXTURE_COMPRESSED : constant GLtexturecomp := 16#86A1#;
GL_NUM_COMPRESSED_TEXTURE_FORMATS : constant GLtexturecomp := 16#86A2#;
GL_COMPRESSED_TEXTURE_FORMATS : constant GLtexturecomp := 16#86A3#;

------------------------------------------------------------------------------
-- OpenGL 1.3 multisample
------------------------------------------------------------------------------

type GLmultisample is new GLenum;

GL_MULTISAMPLE : constant GLmultisample := 16#809D#;
GL_SAMPLE_ALPHA_TO_COVERAGE : constant GLmultisample := 16#809E#;
GL_SAMPLE_ALPHA_TO_ONE : constant GLmultisample := 16#809F#;
GL_SAMPLE_COVERAGE : constant GLmultisample := 16#80A0#;
GL_SAMPLE_BUFFERS : constant GLmultisample := 16#80A8#;
GL_SAMPLES : constant GLmultisample := 16#80A9#;
GL_SAMPLE_COVERAGE_VALUE : constant GLmultisample := 16#80AA#;
GL_SAMPLE_COVERAGE_INVERT : constant GLmultisample := 16#80AB#;
GL_MULTISAMPLE_BIT : constant GLuint := 16#20000000#;

------------------------------------------------------------------------------
-- OpenGL 1.3 transpose matrix
------------------------------------------------------------------------------

type GLtransposemat is new GLenum;

GL_TRANSPOSE_MODELVIEW_MATRIX : constant GLtransposemat := 16#84E3#;
GL_TRANSPOSE_PROJECTION_MATRIX : constant GLtransposemat := 16#84E4#;
GL_TRANSPOSE_TEXTURE_MATRIX : constant GLtransposemat := 16#84E5#;
GL_TRANSPOSE_COLOR_MATRIX : constant GLtransposemat := 16#84E6#;

------------------------------------------------------------------------------
-- OpenGL 1.3 texture env combine
------------------------------------------------------------------------------

type GLtextureenvcomb is new GLenum;

GL_COMBINE : constant GLtextureenvcomb := 16#8570#;
GL_COMBINE_RGB : constant GLtextureenvcomb := 16#8571#;
GL_COMBINE_ALPHA : constant GLtextureenvcomb := 16#8572#;
GL_SOURCE0_RGB : constant GLtextureenvcomb := 16#8580#;
GL_SOURCE1_RGB : constant GLtextureenvcomb := 16#8581#;
GL_SOURCE2_RGB : constant GLtextureenvcomb := 16#8582#;
GL_SOURCE0_ALPHA : constant GLtextureenvcomb := 16#8588#;
GL_SOURCE1_ALPHA : constant GLtextureenvcomb := 16#8589#;
GL_SOURCE2_ALPHA : constant GLtextureenvcomb := 16#858A#;
GL_OPERAND0_RGB : constant GLtextureenvcomb := 16#8590#;
GL_OPERAND1_RGB : constant GLtextureenvcomb := 16#8591#;
GL_OPERAND2_RGB : constant GLtextureenvcomb := 16#8592#;
GL_OPERAND0_ALPHA : constant GLtextureenvcomb := 16#8598#;
GL_OPERAND1_ALPHA : constant GLtextureenvcomb := 16#8599#;
GL_OPERAND2_ALPHA : constant GLtextureenvcomb := 16#859A#;
GL_RGB_SCALE : constant GLtextureenvcomb := 16#8573#;
GL_ADD_SIGNED : constant GLtextureenvcomb := 16#8574#;
GL_INTERPOLATE : constant GLtextureenvcomb := 16#8575#;
GL_SUBTRACT : constant GLtextureenvcomb := 16#84E7#;
GL_CONSTANT : constant GLtextureenvcomb := 16#8576#;
GL_PRIMARY_COLOR : constant GLtextureenvcomb := 16#8577#;
GL_PREVIOUS : constant GLtextureenvcomb := 16#8578#;

------------------------------------------------------------------------------
-- OpenGL 1.3 texture_env_dot3
------------------------------------------------------------------------------

type GLtextureenvdot3 is new GLenum;

GL_DOT3_RGB : constant GLtextureenvdot3 := 16#86AE#;
GL_DOT3_RGBA : constant GLtextureenvdot3 := 16#86AF#;

GL_CLAMP_TO_BORDER : constant GLushort := 16#812D#;

------------------------------------------------------------------------------
-- GL_ARB_multitexture (ARB extension 1 and OpenGL 1.2.1)
------------------------------------------------------------------------------

type GLmultitexturearb is new GLenum;

GL_TEXTURE0_ARB : constant GLmultitexturearb := 16#84C0#;
GL_TEXTURE1_ARB : constant GLmultitexturearb := 16#84C1#;
GL_TEXTURE2_ARB : constant GLmultitexturearb := 16#84C2#;
GL_TEXTURE3_ARB : constant GLmultitexturearb := 16#84C3#;
GL_TEXTURE4_ARB : constant GLmultitexturearb := 16#84C4#;
GL_TEXTURE5_ARB : constant GLmultitexturearb := 16#84C5#;
GL_TEXTURE6_ARB : constant GLmultitexturearb := 16#84C6#;
GL_TEXTURE7_ARB : constant GLmultitexturearb := 16#84C7#;
GL_TEXTURE8_ARB : constant GLmultitexturearb := 16#84C8#;
GL_TEXTURE9_ARB : constant GLmultitexturearb := 16#84C9#;
GL_TEXTURE10_ARB : constant GLmultitexturearb := 16#84CA#;
GL_TEXTURE11_ARB : constant GLmultitexturearb := 16#84CB#;
GL_TEXTURE12_ARB : constant GLmultitexturearb := 16#84CC#;
GL_TEXTURE13_ARB : constant GLmultitexturearb := 16#84CD#;
GL_TEXTURE14_ARB : constant GLmultitexturearb := 16#84CE#;
GL_TEXTURE15_ARB : constant GLmultitexturearb := 16#84CF#;
GL_TEXTURE16_ARB : constant GLmultitexturearb := 16#84D0#;
GL_TEXTURE17_ARB : constant GLmultitexturearb := 16#84D1#;
GL_TEXTURE18_ARB : constant GLmultitexturearb := 16#84D2#;
GL_TEXTURE19_ARB : constant GLmultitexturearb := 16#84D3#;
GL_TEXTURE20_ARB : constant GLmultitexturearb := 16#84D4#;
GL_TEXTURE21_ARB : constant GLmultitexturearb := 16#84D5#;
GL_TEXTURE22_ARB : constant GLmultitexturearb := 16#84D6#;
GL_TEXTURE23_ARB : constant GLmultitexturearb := 16#84D7#;
GL_TEXTURE24_ARB : constant GLmultitexturearb := 16#84D8#;
GL_TEXTURE25_ARB : constant GLmultitexturearb := 16#84D9#;
GL_TEXTURE26_ARB : constant GLmultitexturearb := 16#84DA#;
GL_TEXTURE27_ARB : constant GLmultitexturearb := 16#84DB#;
GL_TEXTURE28_ARB : constant GLmultitexturearb := 16#84DC#;
GL_TEXTURE29_ARB : constant GLmultitexturearb := 16#84DD#;
GL_TEXTURE30_ARB : constant GLmultitexturearb := 16#84DE#;
GL_TEXTURE31_ARB : constant GLmultitexturearb := 16#84DF#;
GL_ACTIVE_TEXTURE_ARB : constant GLmultitexturearb := 16#84E0#;
GL_CLIENT_ACTIVE_TEXTURE_ARB : constant GLmultitexturearb := 16#84E1#;
GL_MAX_TEXTURE_UNITS_ARB : constant GLmultitexturearb := 16#84E2#;

------------------------------------------------------------------------------
-- Debug Shaders
------------------------------------------------------------------------------

type GLdebugshaders is new GLenum;

GL_DEBUG_OBJECT_MESA : constant GLdebugshaders := 16#8759#;
GL_DEBUG_PRINT_MESA : constant GLdebugshaders := 16#875A#;
GL_DEBUG_ASSERT_MESA : constant GLdebugshaders := 16#875B#;

------------------------------------------------------------------------------
-- Debug Shaders
------------------------------------------------------------------------------

type GLobsolete is new GLenum;

GL_DEPTH_STENCIL_MESA : constant GLobsolete := 16#8750#;
GL_UNSIGNED_INT_24_8_MESA : constant GLobsolete := 16#8751#;
GL_UNSIGNED_INT_8_24_REV_MESA : constant GLobsolete := 16#8752#;
GL_UNSIGNED_SHORT_15_1_MESA : constant GLobsolete := 16#8753#;
GL_UNSIGNED_SHORT_1_15_REV_MESA : constant GLobsolete := 16#8754#;

------------------------------------------------------------------------------
-- Program Debuggers
------------------------------------------------------------------------------

GL_FRAGMENT_PROGRAM_POSITION_MESA : constant GLobsolete := 16#8bb0#;
GL_FRAGMENT_PROGRAM_CALLBACK_MESA : constant GLobsolete := 16#8bb1#;
GL_FRAGMENT_PROGRAM_CALLBACK_FUNC_MESA : constant GLobsolete := 16#8bb2#;
GL_FRAGMENT_PROGRAM_CALLBACK_DATA_MESA : constant GLobsolete := 16#8bb3#;
GL_VERTEX_PROGRAM_POSITION_MESA : constant GLobsolete := 16#8bb4#;
GL_VERTEX_PROGRAM_CALLBACK_MESA : constant GLobsolete := 16#8bb5#;
GL_VERTEX_PROGRAM_CALLBACK_FUNC_MESA : constant GLobsolete := 16#8bb6#;
GL_VERTEX_PROGRAM_CALLBACK_DATA_MESA : constant GLobsolete := 16#8bb7#;

GL_TEXTURE_1D_ARRAY_EXT : constant GLushort :=        16#8C18#;
GL_PROXY_TEXTURE_1D_ARRAY_EXT : constant GLushort :=  16#8C19#;
GL_TEXTURE_2D_ARRAY_EXT : constant GLushort :=        16#8C1A#;
GL_PROXY_TEXTURE_2D_ARRAY_EXT : constant GLushort :=  16#8C1B#;
GL_TEXTURE_BINDING_1D_ARRAY_EXT : constant GLushort := 16#8C1C#;
GL_TEXTURE_BINDING_2D_ARRAY_EXT : constant GLushort := 16#8C1D#;
GL_MAX_ARRAY_TEXTURE_LAYERS_EXT : constant GLushort := 16#88FF#;
GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT : constant GLushort := 16#8CD4#;

GL_ALPHA_BLEND_EQUATION_ATI : constant GLushort := 16#883D#;

------------------------------------------------------------------------------

procedure glClearIndex( c : GLfloat );
procedure glClearColor( red, green, blue, alpha: GLclampf );
procedure glClear( mask: GLbitfield );
procedure glIndexMask( mask : GLuint );
procedure glColorMask( red, green, blue, alpha : GLboolean );
procedure glAlphaFunc( func : GLalphacompare; ref : GLclampf );
procedure glBlendFunc( sfactor, dfactor : GLblending );
procedure glLogicOp( opcode : GLlogicops );
procedure glCullFace( mode : GLbuffers );
procedure glFrontFace( mode : GLpolygons );
procedure glPointSize( size : GLfloat );
procedure glLineWidth( width : GLfloat );
procedure glLineStipple( factor : GLint; pattern : GLushort );
procedure glPolygonMode( face : GLbuffers; mode : GLpolygons );
procedure glPolygonOffset( factor : GLfloat; units : GLfloat );
procedure glPolygonStipple( mask : in out GLubyte );
procedure glGetPolygonStipple( mask : GLubyte );
procedure glEdgeFlag( flag : GLboolean );
procedure glEdgeFlagv( flag : in out GLboolean );
procedure glScissor( x, y : GLint; width, height : GLsizei );
procedure glClipPlane( plane : GLusercplane; equation : in out GLdouble );
procedure glGetClipPlane( plane : GLusercplane; equation : in out GLdouble );
procedure glDrawBuffer( mode : GLbuffers );
procedure glReadBuffer( mode : GLbuffers );
procedure glEnable( cap : GLenum ); -- left as GLenum
procedure glDisable( cap : GLenum ); -- left as GLenum
function  glIsEnabled( cap : GLenum ) return GLboolean; -- left as GLenum
procedure glEnableClientState( cap : GLenum );  -- /* 1.1 */
procedure glDisableClientState( cap : GLenum );  -- /* 1.1 */
procedure glGetBooleanv( pname : GLenum; params : in out GLboolean );
procedure glGetDoublev( pname : GLenum; params : in out GLdouble );
procedure glGetFloatv( pname : GLenum; params : in out GLfloat );
procedure glGetIntegerv( pname : GLenum; params : in out GLint );
procedure glPushAttrib( mask : GLbitfield );
procedure glPopAttrib;
procedure glPushClientAttrib( mask : GLbitfield );  -- /* 1.1 */
procedure glPopClientAttrib;  -- /* 1.1 */
function  glRenderMode( mode : GLrendermodes ) return GLint;
function  glGetError return GLerrors;
function  glGetString( name : GLenum ) return system.address; -- GLUbyte*
procedure glFinish;
procedure glFlush;
procedure glHint( target : GLhints; mode : GLhintmodes );
procedure glClearDepth( depth : GLclampd );
procedure glDepthFunc( func : GLalphacompare );
procedure glDepthMask( flag : GLboolean );
procedure glDepthRange( near_val, far_val : GLclampd );
procedure glClearAccum( red, green, blue, alpha : GLfloat );
procedure glAccum( op : GLenum; value : GLfloat );
procedure glMatrixMode( mode : GLmodes );
procedure glOrtho( left, right, bottom, top, near_val, far_val : GLdouble );
procedure glFrustum( left, right, bottom, top, near_val, far_val : GLdouble );
procedure glViewport( x, y : GLint; width, height : GLsizei );
procedure glPushMatrix;
procedure glPopMatrix;
procedure glLoadIdentity;
procedure glLoadMatrixd( m : GL_Double_Array_Ptr );
procedure glLoadMatrixf( m : GL_Float_Array_Ptr );
procedure glMultMatrixd( m : GL_Double_Array_Ptr );
procedure glMultMatrixf( m : GL_Float_Array_Ptr );
procedure glRotated( angle, x, y, z : GLdouble );
procedure glRotatef( angle, x, y, z : GLfloat );
procedure glScaled( x, y, z : GLdouble );
procedure glScalef( x, y, z : GLfloat );
procedure glTranslated( x, y, z : GLdouble );
procedure glTranslatef( x, y,z : GLfloat );
function  glIsList( list : GLuint ) return GLboolean;
procedure glDeleteLists( list : GLuint; rng : GLsizei );
function  glGenLists( rng : GLsizei ) return GLuint;
procedure glNewList( list : GLuint; mode : GLdlists );
procedure glEndList;
procedure glCallList( list : GLuint );
procedure glCallLists( n : GLsizei; kind : GLtypes; lists : System.address );
procedure glListBase( base : GLuint );
procedure glBegin( mode : GLprimitives );
procedure glEnd;
procedure glVertex2d( x, y : GLdouble );
procedure glVertex2f( x, y : GLfloat );
procedure glVertex2i( x, y : GLint );
procedure glVertex2s( x, y : GLshort );
procedure glVertex3d( x, y, z : GLdouble );
procedure glVertex3f( x, y, z : GLfloat );
procedure glVertex3i( x, y, z : GLint );
procedure glVertex3s( x, y, z : GLshort );
procedure glVertex4d( x, y, z, w : GLdouble );
procedure glVertex4f( x, y, z, w : GLfloat );
procedure glVertex4i( x, y, z, w : GLint );
procedure glVertex4s( x, y, z, w : GLshort );
procedure glVertex2dv( v : GL_Double_Array_Ptr );
procedure glVertex2fv( v : GL_Float_Array_Ptr );
procedure glVertex2iv( v : GL_Int_Array_Ptr );
procedure glVertex2sv( v : GL_Short_Array_Ptr );
procedure glVertex3dv( v : GL_Double_Array_Ptr );
procedure glVertex3fv( v : GL_Float_Array_Ptr );
procedure glVertex3iv( v : GL_Int_Array_Ptr );
procedure glVertex3sv( v : GL_Short_Array_Ptr );
procedure glVertex4dv( v : GL_Double_Array_Ptr );
procedure glVertex4fv( v : GL_Float_Array_Ptr );
procedure glVertex4iv( v : GL_Int_Array_Ptr );
procedure glVertex4sv( v : GL_Short_Array_Ptr );
procedure glNormal3b( nx, ny, nz : GLbyte );
procedure glNormal3d( nx, ny, nz : GLdouble );
procedure glNormal3f( nx, ny, nz : GLfloat );
procedure glNormal3i( nx, ny, nz : GLint );
procedure glNormal3s( nx, ny, nz : GLshort );
procedure glNormal3bv( v : GL_Byte_Array_Ptr );
procedure glNormal3dv( v : GL_Double_Array_Ptr );
procedure glNormal3fv( v : GL_Float_Array_Ptr );
procedure glNormal3iv( v : GL_Int_Array_Ptr );
procedure glNormal3sv( v : GL_Short_Array_Ptr );
procedure glIndexd( c : GLdouble );
procedure glIndexf( c : GLfloat );
procedure glIndexi( c : GLint );
procedure glIndexs( c : GLshort );
procedure glIndexub( c : GLubyte );  -- /* 1.1 */
procedure glIndexdv( c : GL_Double_Array_Ptr );
procedure glIndexfv( c : GL_Float_Array_Ptr );
procedure glIndexiv( c : GL_Int_Array_Ptr );
procedure glIndexsv( c : GL_Short_Array_Ptr );
procedure glIndexubv( c : GL_UByte_Array_Ptr );  -- /* 1.1 */
procedure glColor3b( red, green, blue : GLbyte );
procedure glColor3d( red, green, blue : GLdouble );
procedure glColor3f( red, green, blue : GLfloat );
procedure glColor3i( red, green, blue : GLint );
procedure glColor3s( red, green, blue : GLshort );
procedure glColor3ub( red, green, blue : GLubyte );
procedure glColor3ui( red, green, blue : GLuint );
procedure glColor3us( red, green, blue : GLushort );
procedure glColor4b( red, green, blue, alpha : GLbyte );
procedure glColor4d( red, green, blue, alpha : GLdouble );
procedure glColor4f( red, green, blue, alpha : GLfloat );
procedure glColor4i( red, green, blue, alpha : GLint );
procedure glColor4s( red, green, blue, alpha : GLshort );
procedure glColor4ub( red, green, blue, alpha : GLubyte );
procedure glColor4ui( red, green, blue, alpha : GLuint );
procedure glColor4us( red, green, blue, alpha : GLushort );
procedure glColor3bv( v : GL_Byte_Array_Ptr );
procedure glColor3dv( v : GL_Double_Array_Ptr );
procedure glColor3fv( v : GL_Float_Array_Ptr );
procedure glColor3iv( v : GL_Int_Array_Ptr );
procedure glColor3sv( v : GL_Short_Array_Ptr );
procedure glColor3ubv( v : GL_UByte_Array_Ptr );
procedure glColor3uiv( v : GL_UInt_Array_Ptr );
procedure glColor3usv( v : GL_UShort_Array_Ptr );
procedure glColor4bv( v : GL_Byte_Array_Ptr );
procedure glColor4dv( v : GL_Double_Array_Ptr );
procedure glColor4fv( v : GL_Float_Array_Ptr );
procedure glColor4iv( v : GL_Int_Array_Ptr );
procedure glColor4sv( v : GL_Short_Array_Ptr );
procedure glColor4ubv( v : GL_UByte_Array_Ptr );
procedure glColor4uiv( v : GL_UInt_Array_Ptr );
procedure glColor4usv( v : GL_UShort_Array_Ptr );
procedure glTexCoord1d( s : GLdouble );
procedure glTexCoord1f( s : GLfloat );
procedure glTexCoord1i( s : GLint );
procedure glTexCoord1s( s : GLshort );
procedure glTexCoord2d( s, t : GLdouble );
procedure glTexCoord2f( s, t : GLfloat );
procedure glTexCoord2i( s, t : GLint );
procedure glTexCoord2s( s, t : GLshort );
procedure glTexCoord3d( s, t, r : GLdouble );
procedure glTexCoord3f( s, t, r : GLfloat );
procedure glTexCoord3i( s, t, r : GLint );
procedure glTexCoord3s( s, t, r : GLshort );
procedure glTexCoord4d( s, t, r, q : GLdouble );
procedure glTexCoord4f( s, t, r, q : GLfloat );
procedure glTexCoord4i( s, t, r, q : GLint );
procedure glTexCoord4s( s, t, r, q : GLshort );
procedure glTexCoord1dv( v : GL_Double_Array_Ptr );
procedure glTexCoord1fv( v : GL_Float_Array_Ptr );
procedure glTexCoord1iv( v : GL_Int_Array_Ptr );
procedure glTexCoord1sv( v : GL_Short_Array_Ptr );
procedure glTexCoord2dv( v : GL_Double_Array_Ptr );
procedure glTexCoord2fv( v : GL_Float_Array_Ptr );
procedure glTexCoord2iv( v : GL_Int_Array_Ptr );
procedure glTexCoord2sv( v : GL_Short_Array_Ptr );
procedure glTexCoord3dv( v : GL_Double_Array_Ptr );
procedure glTexCoord3fv( v : GL_Float_Array_Ptr );
procedure glTexCoord3iv( v : GL_Int_Array_Ptr );
procedure glTexCoord3sv( v : GL_Short_Array_Ptr );
procedure glTexCoord4dv( v : GL_Double_Array_Ptr );
procedure glTexCoord4fv( v : GL_Float_Array_Ptr );
procedure glTexCoord4iv( v : GL_Int_Array_Ptr );
procedure glTexCoord4sv( v : GL_Short_Array_Ptr );
procedure glRasterPos2d( x, y : GLdouble );
procedure glRasterPos2f( x, y : GLfloat );
procedure glRasterPos2i( x, y : GLint );
procedure glRasterPos2s( x, y : GLshort );
procedure glRasterPos3d( x, y, z : GLdouble );
procedure glRasterPos3f( x, y, z : GLfloat );
procedure glRasterPos3i( x, y, z : GLint );
procedure glRasterPos3s( x, y, z : GLshort );
procedure glRasterPos4d( x, y, z, w : GLdouble );
procedure glRasterPos4f( x, y, z, w : GLfloat );
procedure glRasterPos4i( x, y, z, w : GLint );
procedure glRasterPos4s( x, y, z, w : GLshort );
procedure glRasterPos2dv( v : GL_Double_Array_Ptr );
procedure glRasterPos2fv( v : GL_Float_Array_Ptr );
procedure glRasterPos2iv( v : GL_Int_Array_Ptr );
procedure glRasterPos2sv( v : GL_Short_Array_Ptr );
procedure glRasterPos3dv( v : GL_Double_Array_Ptr );
procedure glRasterPos3fv( v : GL_Float_Array_Ptr );
procedure glRasterPos3iv( v : GL_Int_Array_Ptr );
procedure glRasterPos3sv( v : GL_Short_Array_Ptr );
procedure glRasterPos4dv( v : GL_Double_Array_Ptr );
procedure glRasterPos4fv( v : GL_Float_Array_Ptr );
procedure glRasterPos4iv( v : GL_Int_Array_Ptr );
procedure glRasterPos4sv( v : GL_Short_Array_Ptr );
procedure glRectd( x1, y1, x2, y2 : GLdouble );
procedure glRectf( x1, y1, x2, y2 : GLfloat );
procedure glRecti( x1, y1, x2, y2 : GLint );
procedure glRects( x1, y1, x2, y2 : GLshort );
procedure glRectdv( v1, v2 : GL_Double_Array_Ptr );
procedure glRectfv( v1, v2 : GL_Float_Array_Ptr );
procedure glRectiv( v1, v2 : GL_Int_Array_Ptr );
procedure glRectsv( v1, v2 : GL_Short_Array_Ptr );
procedure glVertexPointer( size : GLint; kind : GLtypes; stride : GLsizei; ptr : System.address );
procedure glNormalPointer( kind : GLtypes; stride : GLsizei; ptr : System.address );
procedure glColorPointer( size : GLint; kind : GLtypes; stride : GLsizei; ptr : System.address );
procedure glIndexPointer( kind : GLtypes; stride : GLsizei; ptr : System.address );
procedure glTexCoordPointer( size : GLint; kind : GLtypes; stride : GLsizei; ptr : System.address );
procedure glEdgeFlagPointer( stride : GLsizei; ptr : System.address );
procedure glGetPointerv( pname : GLvertexarrays; params : in out System.address );
procedure glArrayElement( i : GLint );
procedure glDrawArrays( mode : GLprimitives; first : GLint; count : GLsizei );
procedure glDrawElements( mode : GLtypes; count : GLsizei; kind : GLprimitives; indices : System.address );
procedure glInterleavedArrays( format : GLvertexarrays; stride : GLsizei; pointer : System.address );
procedure glShadeModel( mode : GLlighting );
procedure glLightf( light : GLenum; pname : GLlighting; param : GLfloat );
procedure glLighti( light : GLenum; pname : GLlighting; param : GLint );
procedure glLightfv( light : GLenum; pname : GLlighting; params : GL_Float_Array_Ptr );
procedure glLightiv( light : GLenum; pname : GLlighting; params : GL_Int_Array_Ptr );
procedure glGetLightfv( light : GLenum; pname : GLlighting; params : GL_Float_Array_Ptr );
procedure glGetLightiv( light : GLenum; pname : GLlighting; params : GL_Int_Array_Ptr );
procedure glLightModelf( pname : GLenum; param : GLfloat );
procedure glLightModeli( pname : GLenum; param : GLint );
procedure glLightModelfv( pname : GLenum; params : GL_Float_Array_Ptr );
procedure glLightModeliv( pname : GLenum; params : GL_Int_Array_Ptr );
procedure glMaterialf( face : GLbuffers; pname : GLlighting; param : GLfloat );
procedure glMateriali( face : GLbuffers; pname : GLlighting; param : GLint );
procedure glMaterialfv( face : GLbuffers; pname : GLlighting; param : GL_Float_Array_Ptr );
procedure glMaterialiv( face : GLbuffers; pname : GLlighting; param : GL_Int_Array_Ptr );
procedure glGetMaterialfv( face : GLbuffers; pname : GLlighting; param : GL_Float_Array_Ptr );
procedure glGetMaterialiv( face : GLbuffers; pname : GLlighting; param : GL_Int_Array_Ptr );
procedure glColorMaterial( face : GLbuffers; mode : GLlighting );
procedure glPixelZoom( xfactor, yfactor : GLfloat );
procedure glPixelStoref( pname : GLpixelmode; param : GLfloat );
procedure glPixelStorei( pname : GLpixelmode; param : GLint );
procedure glPixelTransferf( pname : GLpixelmode; param : GLfloat );
procedure glPixelTransferi( pname : GLpixelmode; param : GLint );
procedure glPixelMapfv( map : GLpixelmode; mapsize : GLsizei; values : GL_Float_Array_Ptr );
procedure glPixelMapuiv( map : GLpixelmode; mapsize : GLsizei; values : GL_UInt_Array_Ptr );
procedure glPixelMapusv( map : GLpixelmode; mapsize : GLsizei; values : GL_UShort_Array_Ptr );
procedure glGetPixelMapfv( map : GLpixelmode; values : GL_Float_Array_Ptr );
procedure glGetPixelMapuiv( map : GLpixelmode; values : GL_UInt_Array_Ptr );
procedure glGetPixelMapusv( map : GLpixelmode; values : GL_UShort_Array_Ptr );
procedure glBitmap( width, height : GLsizei; xorig, yorig : GLfloat; xmove, ymove : GLfloat; bitmap : GL_UByte_Array_Ptr );
procedure glReadPixels( x, y : GLint; width, height : GLsizei; format : GLenum; kind : GLtypes; pixels : System.address );
procedure glDrawPixels( width, height : GLsizei; format : GLbuffers; kind : GLtypes; pixels : System.address );
procedure glCopyPixels( x, y : GLint; width, height : GLsizei; kind : GLbuffers );
procedure glStencilFunc( func : GLenum; ref : GLint; mask : GLuint );
procedure glStencilMask( mask : GLuint );
procedure glStencilOp( fail : GLstencil; zfail : GLstencil; zpass : GLstencil );
procedure glClearStencil( s : GLint );
procedure glTexGend( coord : GLtexturemapping; pname : GLtexturemapping; param : GLdouble );
procedure glTexGenf( coord : GLtexturemapping; pname : GLtexturemapping; param : GLfloat );
procedure glTexGeni( coord : GLtexturemapping; pname : GLtexturemapping; param : GLint );
procedure glTexGendv( coord : GLtexturemapping; pname : GLtexturemapping; params : GL_Double_Array_Ptr );
procedure glTexGenfv( coord : GLtexturemapping; pname : GLtexturemapping; params : GL_Float_Array_Ptr );
procedure glTexGeniv( coord : GLtexturemapping; pname : GLtexturemapping; params : GL_Int_Array_Ptr );
procedure glGetTexGendv( coord : GLtexturemapping; pname : GLtexturemapping; params : GL_Double_Array_Ptr );
procedure glGetTexGenfv( coord : GLtexturemapping; pname : GLtexturemapping; params : GL_Float_Array_Ptr );
procedure glGetTexGeniv( coord : GLtexturemapping; pname : GLtexturemapping; params : in out GL_Int_Array_Ptr );
procedure glTexEnvf( target : GLtexturemapping; pname : GLtexturemapping; param : GLfloat );
procedure glTexEnvi( target : GLtexturemapping; pname : GLtexturemapping; param : GLint );
procedure glTexEnvfv( target : GLtexturemapping; pname : GLtexturemapping; param : GL_Float_Array_Ptr );
procedure glTexEnviv( target : GLtexturemapping; pname : GLtexturemapping; param : GL_Int_Array_Ptr );
procedure glGetTexEnvfv( target : GLtexturemapping; pname : GLtexturemapping; param : GL_Float_Array_Ptr );
procedure glGetTexEnviv( target : GLtexturemapping; pname : GLtexturemapping; param : GL_Int_Array_Ptr );
procedure glTexParameterf( target : GLtexturemapping; pname : GLtexturemapping; param : GLfloat );
procedure glTexParameteri( target : GLtexturemapping; pname : GLtexturemapping; param : GLint );
procedure glTexParameterfv( target : GLtexturemapping; pname : GLtexturemapping; params : GL_Float_Array_Ptr );
procedure glTexParameteriv( target : GLtexturemapping; pname : GLtexturemapping; params : GL_Int_Array_Ptr );
procedure glGetTexParameterfv( target : GLtexturemapping; pname : GLtexturemapping; params : GL_Float_Array_Ptr );
procedure glGetTexParameteriv( target : GLtexturemapping; pname : GLtexturemapping; params : GL_Int_Array_Ptr );
procedure glGetTexLevelParameterfv( target : GLtexturemapping; level : GLint; pname : GLenum; params : GL_Float_Array_Ptr );
procedure glGetTexLevelParameteriv( target : GLtexturemapping; level : GLint; pname : GLenum; params : GL_Int_Array_Ptr );
procedure glTexImage1D( target : GLtexturemapping; level : GLint; internalFormat : GLint; width : GLsizei; border : GLint; format : GLbuffers; kind : GLtypes; pixels : System.address );
procedure glTexImage2D( target : GLenum; level : GLint; internalFormat : GLint; width : GLsizei; height : GLsizei; border : GLint; format : GLenum; kind : GLenum; pixels : System.address );
procedure glGetTexImage( target : GLtexturemapping; level : GLint; format : GLbuffers; kind : GLtypes; pixels : System.address );
procedure glGenTextures( n : GLsizei; textures : GL_UInt_Array_Ptr );
procedure glDeleteTextures( n : GLsizei; textures : GL_UInt_Array_Ptr );
procedure glBindTexture( target : GLtexturemapping; texture : GLuint );
procedure glPrioritizeTextures( n : GLsizei; textures : GL_UInt_Array_Ptr; priorities : GL_Clampf_Array_Ptr );
function  glAreTexturesResident( n : GLsizei; textures : GL_UInt_Array_Ptr; residences : GL_Boolean_Array_Ptr ) return GLboolean;
function  glIsTexture( texture : GLuint ) return GLboolean;
procedure glTexSubImage1D( target : GLtexturemapping; level : GLint; xoffset : GLint; width : GLsizei; format : GLbuffers; kind : GLtypes; pixels : System.address );
procedure glTexSubImage2D( target : GLtexturemapping; level : GLint; xoffset, yoffset : GLint; width, height : GLsizei; format : GLbuffers; kind : GLtypes; pixels : System.address );
procedure glCopyTexImage1D( target : GLtexturemapping; level : GLint; internalformat : GLenum; x, y : GLint; width : GLsizei; border : GLint );
procedure glCopyTexImage2D( target : GLtexturemapping; level : GLint; internalformat : GLenum; x, y : GLint; width, height : GLsizei; border : GLint );
procedure glCopyTexSubImage1D( target : GLtexturemapping; level : GLint; xoffset : GLint; x, y : GLint; width : GLsizei );
procedure glCopyTexSubImage2D( target : GLtexturemapping; level : GLint; xoffset, yoffset : GLint; x, y : GLint; width, height : GLsizei );
procedure glMap1d( target : GLevaluators; u1, u2 : GLdouble; stride : GLint; order : GLint; points : GL_Double_Array_Ptr );
procedure glMap1f( target : GLevaluators; u1, u2 : GLfloat; stride : GLint; order : GLint; points : GL_Float_Array_Ptr );
procedure glMap2d( target : GLevaluators; u1, u2 : GLdouble; ustride : GLint; uorder : GLint; v1, v2 : GLdouble; vstride : GLint; vorder : GLint; points : GL_Double_Array_Ptr );
procedure glMap2f( target : GLevaluators; u1, u2 : GLfloat; ustride : GLint; uorder : GLint; v1, v2 : GLfloat; vstride : GLint; vorder : GLint; points : GL_Float_Array_Ptr );
procedure glGetMapdv( target, query : GLevaluators; v : GL_Double_Array_Ptr );
procedure glGetMapfv( target, query : GLevaluators; v : GL_Float_Array_Ptr );
procedure glGetMapiv( target, query : GLevaluators; v : GL_Int_Array_Ptr );
procedure glEvalCoord1d( u : GL_Double_Array_Ptr );
procedure glEvalCoord1f( u : GL_Float_Array_Ptr );
procedure glEvalCoord1dv( u : GL_Double_Array_Ptr );
procedure glEvalCoord1fv( u : GL_Float_Array_Ptr );
procedure glEvalCoord2d( u, v : GLdouble );
procedure glEvalCoord2f( u, v : GLfloat );
procedure glEvalCoord2dv( u : GL_Double_Array_Ptr );
procedure glEvalCoord2fv( u : GL_Float_Array_Ptr );
procedure glMapGrid1d( un : GLint; u1, u2 : GLdouble );
procedure glMapGrid1f( un : GLint; u1, u2 : GLfloat );
procedure glMapGrid2d( un : GLint; u1, u2 : GLdouble; vn : GLint; v1, v2 : GLdouble );
procedure glMapGrid2f( un : GLint; u1, u2 : GLfloat; vn : GLint; v1, v2 : GLfloat );
procedure glEvalPoint1( i : GLint );
procedure glEvalPoint2( i, j : GLint );
procedure glEvalMesh1( mode : GLpolygons; i1, i2 : GLint );
procedure glEvalMesh2( mode : GLpolygons; i1, i2  :GLint; j1, j2 : GLint );
procedure glFogf( pname : GLfog; param : GLfloat );
procedure glFogi( pname : GLfog; param : GLint );
procedure glFogfv( pname : GLfog; param : GL_Float_Array_Ptr );
procedure glFogiv( pname : GLfog; param : GL_Int_Array_Ptr );
procedure glFeedbackBuffer( size : GLsizei; kind : GLfeedback; buffer : GL_Float_Array_Ptr );
procedure glPassThrough( token : GLfloat );
procedure glSelectBuffer( kind : GLsizei; buffer : GL_UInt_Array_Ptr );
procedure glInitNames;
procedure glLoadName( name : GLuint );
procedure glPushName( name : GLuint );
procedure glPopName;
procedure glDrawRangeElements( mode : GLprimitives; start : GLuint; done : GLuint; count : GLsizei; kind : GLtypes; indices : System.address );
procedure glTexImage3D( target : GLenum; level : GLint; internalFormat : GLint; width, height, depth : GLsizei; border : GLint; format : GLbuffers; kind : GLtypes; pixels : System.address );
procedure glTexSubImage3D( target : GLenum; level : GLint; xoffset, yoffset, zoffset : GLint; width, height, depth : GLsizei; format : GLbuffers; kind : GLtypes; pixels : System.address );
procedure glCopyTexSubImage3D( target : GLenum; level : GLint; xoffset, yoffset, zoffset : GLint; x, y : GLint; width, height : GLsizei );
procedure glColorTable( target : GLenum; internalformat : GLenum; width : GLsizei; format : GLenum; kind : GLenum; table : System.address );
procedure glColorSubTable( target : GLarbmapping; start : GLsizei; count : GLsizei; format : GLbuffers; kind : GLtypes; data : System.address );
procedure glColorTableParameteriv( target : GLarbmapping; pname : GLarbmapping; params : GL_Int_Array_Ptr );
procedure glColorTableParameterfv( target : GLarbmapping; pname : GLarbmapping; params : GL_Float_Array_Ptr );
procedure glCopyColorSubTable( target : GLarbmapping; start : GLsizei; x, y : GLint; width : GLsizei );
procedure glCopyColorTable( target : GLarbmapping; internalformat : GLenum; x, y : GLint; width : GLsizei );
procedure glGetColorTable( target : GLarbmapping; format : GLbuffers; kind : GLtypes; table : System.address );
procedure glGetColorTableParameterfv( target : GLarbmapping; pname : GLarbmapping; params : GL_Float_Array_Ptr );
procedure glGetColorTableParameteriv( target : GLarbmapping; pname : GLarbmapping; params : GL_Int_Array_Ptr );
procedure glBlendEquation( mode : GLarbmapping );
procedure glBlendColor( red, green, blue, alpha : GLclampf );
procedure glHistogram( target : GLarbmapping; width : GLsizei; internalformat : GLenum; sink : GLboolean );
procedure glResetHistogram( target : GLarbmapping );
procedure glGetHistogram( target : GLarbmapping; reset : GLboolean; format : GLbuffers; kind : GLtypes; values : System.address );
procedure glGetHistogramParameterfv( target : GLarbmapping; pname : GLarbmapping; params : in out GLfloat );
procedure glGetHistogramParameteriv( target : GLarbmapping; pname : GLarbmapping; params : in out GLint );
procedure glMinmax( target : GLarbmapping; internalformat : GLenum; sink : GLboolean );
procedure glResetMinmax( target : GLarbmapping );
procedure glGetMinmax( target : GLarbmapping; reset : GLboolean; format : GLbuffers; kind : GLtypes; values : System.address );
procedure glGetMinmaxParameterfv( target : GLarbmapping; pname : GLarbmapping; params : GL_Float_Array_Ptr );
procedure glGetMinmaxParameteriv( target : GLarbmapping; pname : GLarbmapping; params : GL_Int_Array_Ptr );
procedure glConvolutionFilter1D( target : GLbuffers; internalformat : GLbuffers; width : GLsizei; format : GLbuffers; kind : GLtypes; image : System.address );
procedure glConvolutionFilter2D( target : GLbuffers; internalformat : GLbuffers; width, height : GLsizei; format : GLbuffers; kind : GLtypes; image : System.address );
procedure glConvolutionParameterf( target : GLarbmapping; pname : GLarbmapping; params : GLfloat );
procedure glConvolutionParameterfv( target : GLarbmapping; pname : GLarbmapping; params : in out GLfloat );
procedure glConvolutionParameterfv( target : GLarbmapping; pname : GLarbmapping; params :  GL_Float_Array_Ptr );
procedure glConvolutionParameteri( target : GLarbmapping; pname : GLarbmapping; params : GLint );
procedure glConvolutionParameteriv( target : GLarbmapping; pname : GLarbmapping; params :  GL_Int_Array_Ptr  );
procedure glCopyConvolutionFilter1D( target : GLarbmapping; internalformat : GLarbmapping; x, y : GLint; width : GLsizei );
procedure glCopyConvolutionFilter2D( target : GLarbmapping; internalformat : GLarbmapping; x, y : GLint; width, height : GLsizei );
procedure glGetConvolutionFilter( target : GLarbmapping; format : GLbuffers; kind : GLtypes; image : System.address );
procedure glGetConvolutionParameterfv( target : GLarbmapping; pname : GLarbmapping; params : in out GLfloat );
procedure glGetConvolutionParameteriv( target : GLarbmapping; pname : GLarbmapping; params : in out GLint );
procedure glSeparableFilter2D( target : GLarbmapping; internalformat : GLbuffers; width, height : GLsizei; format : GLbuffers; kind : GLtypes; row, column : System.address );
procedure glGetSeparableFilter( target : GLarbmapping; format : GLbuffers; kind : GLtypes; row, column, span : System.address );

-- No man pages for what these do

procedure glActiveTexture( texture : GLenum );
procedure glClientActiveTexture( texture : GLenum );
procedure glCompressedTexImage1D( target : GLenum; level : GLint; internalformat : GLbuffers; width : GLsizei; border : GLint; imageSize : GLsizei; data : System.address );
procedure glCompressedTexImage2D( target : GLenum; level : GLint; internalformat : GLbuffers; width, height : GLsizei; border : GLint; imageSize : GLsizei; data : System.address );
procedure glCompressedTexImage3D( target : GLenum; level : GLint; internalformat : GLbuffers; width, height, depth : GLsizei; border : GLint; imageSize : GLsizei; data : System.address );
procedure glCompressedTexSubImage1D( target : GLenum; level : GLint; xoffset : GLint; width : GLsizei; format : GLbuffers; imageSize : GLsizei; data : System.address );
procedure glCompressedTexSubImage2D( target : GLenum; level : GLint; xoffset, yoffset : GLint; width, height : GLsizei; format : GLbuffers; imageSize : GLsizei; data : System.address );
procedure glCompressedTexSubImage3D( target : GLenum; level : GLint; xoffset, yoffset, zoffset : GLint; width, height, depth : GLsizei; format : GLbuffers; imageSize : GLsizei; data : System.address );
procedure glGetCompressedTexImage( target : GLenum; lod : GLint; img : System.address );
procedure glMultiTexCoord1d( target : GLenum; s : GLdouble );
procedure glMultiTexCoord1dv( target : GLenum; v : in out GLdouble );
procedure glMultiTexCoord1f( target : GLenum; s : GLfloat );
procedure glMultiTexCoord1fv( target : GLenum; v : in out GLfloat );
procedure glMultiTexCoord1i( target : GLenum; s : GLint );
procedure glMultiTexCoord1iv( target : GLenum; v : in out GLint );
procedure glMultiTexCoord1s( target : GLenum; s : GLshort );
procedure glMultiTexCoord1sv( target : GLenum; v : in out GLshort );
procedure glMultiTexCoord2d( target : GLenum; s, t : GLdouble );
procedure glMultiTexCoord2dv( target : GLenum; v : in out GLdouble );
procedure glMultiTexCoord2f( target : GLenum; s, t : GLfloat );
procedure glMultiTexCoord2fv( target : GLenum; v : in out GLfloat );
procedure glMultiTexCoord2i( target : GLenum; s, t : GLint );
procedure glMultiTexCoord2iv( target : GLenum; v : in out GLint );
procedure glMultiTexCoord2s( target : GLenum; s, t : GLshort );
procedure glMultiTexCoord2sv( target : GLenum; v : in out GLshort );
procedure glMultiTexCoord3d( target : GLenum; s, t, r : GLdouble );
procedure glMultiTexCoord3dv( target : GLenum; v : in out GLdouble );
procedure glMultiTexCoord3f( target : GLenum; s, t, r : GLfloat );
procedure glMultiTexCoord3fv( target : GLenum; v : in out GLfloat );
procedure glMultiTexCoord3i( target : GLenum; s, t, r : GLint );
procedure glMultiTexCoord3iv( target : GLenum; v : in out GLint );
procedure glMultiTexCoord3s( target : GLenum; s, t, r : GLshort );
procedure glMultiTexCoord3sv( target : GLenum; v : in out GLshort );
procedure glMultiTexCoord4d( target : GLenum; s, t, r, q : GLdouble );
procedure glMultiTexCoord4dv( target : GLenum; v : in out GLdouble );
procedure glMultiTexCoord4f( target : GLenum; s, t, r, q : GLfloat );
procedure glMultiTexCoord4fv( target : GLenum; v : in out GLfloat );
procedure glMultiTexCoord4i( target : GLenum; s, t, r, q : GLint );
procedure glMultiTexCoord4iv( target : GLenum; v : in out GLint );
procedure glMultiTexCoord4s( target : GLenum; s, t, r, q : GLshort );
procedure glMultiTexCoord4sv( target : GLenum; v : in out GLshort );
--procedure glLoadTransposeMatrixd( GLdouble m[16] );
--procedure glLoadTransposeMatrixf( GLfloat m[16] );
--procedure glMultTransposeMatrixd( GLdouble m[16] );
--procedure glMultTransposeMatrixf( GLfloat m[16] );
procedure glSampleCoverage( value : GLclampf; invert : GLboolean );
procedure glActiveTextureARB( texture : GLenum );
procedure glClientActiveTextureARB( texture : GLenum );
procedure glMultiTexCoord1dARB( target : GLenum; s : GLdouble );
procedure glMultiTexCoord1dvARB( target : GLenum; v : in out GLdouble );
procedure glMultiTexCoord1fARB( target : GLenum; s : GLfloat );
procedure glMultiTexCoord1fvARB( target : GLenum; v : in out GLfloat );
procedure glMultiTexCoord1iARB( target : GLenum; s : GLint );
procedure glMultiTexCoord1ivARB( target : GLenum; v : in out GLint );
procedure glMultiTexCoord1sARB( target : GLenum; s : GLshort );
procedure glMultiTexCoord1svARB( target : GLenum; v : in out GLshort );
procedure glMultiTexCoord2dARB( target : GLenum; s, t : GLdouble );
procedure glMultiTexCoord2dvARB( target : GLenum; v : in out GLdouble );
procedure glMultiTexCoord2fARB( target : GLenum; s, t : GLfloat );
procedure glMultiTexCoord2fvARB( target : GLenum; v : in out GLfloat );
procedure glMultiTexCoord2iARB( target : GLenum; s, t : GLint );
procedure glMultiTexCoord2ivARB( target : GLenum; v : in out GLint );
procedure glMultiTexCoord2sARB( target : GLenum; s, t : GLshort );
procedure glMultiTexCoord2svARB( target : GLenum; v : in out GLshort );
procedure glMultiTexCoord3dARB( target : GLenum; s, t, r : GLdouble );
procedure glMultiTexCoord3dvARB( target : GLenum; v : in out GLdouble );
procedure glMultiTexCoord3fARB( target : GLenum; s, t, r : GLfloat );
procedure glMultiTexCoord3fvARB( target : GLenum; v : in out GLfloat );
procedure glMultiTexCoord3iARB( target : GLenum; s, t, r : GLint );
procedure glMultiTexCoord3ivARB( target : GLenum; v : in out GLint );
procedure glMultiTexCoord3sARB( target : GLenum; s, t, r : GLshort );
procedure glMultiTexCoord3svARB( target : GLenum; v : in out GLshort );
procedure glMultiTexCoord4dARB( target : GLenum; s, t, r, q : GLdouble );
procedure glMultiTexCoord4dvARB( target : GLenum; v : in out GLdouble );
procedure glMultiTexCoord4fARB( target : GLenum; s, t, r, q : GLfloat );
procedure glMultiTexCoord4fvARB( target : GLenum; v : in out GLfloat );
procedure glMultiTexCoord4iARB( target : GLenum; s, t, r, q : GLint );
procedure glMultiTexCoord4ivARB( target : GLenum; v : in out GLint );
procedure glMultiTexCoord4sARB( target : GLenum; s, t, r, q : GLshort );
procedure glMultiTexCoord4svARB( target : GLenum; v : in out GLshort );

-- MESA specific

--function  glCreateDebugObjectMESA return GLhandleARB;
--procedure glClearDebugLogMESA( obj : GLhandleARB; logType : GLenum; shaderType : GLenum);
--procedure glGetDebugLogMESA( obj : GLhandleARB; logType : GLenum; shaderType : GLenum; maxLength : GLsizei; length : in out GLsizei; debugLog : in out GLcharARB );
--function  glGetDebugLogLengthMESA ( obj : GLhandleARB; logType : GLenum; shaderType : GLenum ) return GLsizei;
--procedure glProgramCallbackMESA( target : GLenum; callback : GLprogramcallbackMESA; data : System.address );
--procedure glGetProgramRegisterfvMESA( target : GLenum; len : GLsizei; name : in out GLubyte; v : in out GLfloat );
--procedure glFramebufferTextureLayerEXT( target : GLenum; attachment : GLenum; texture : GLuint; level : GLint; layer : GLint );
--procedure glBlendEquationSeparateATI( modeRGB, modeA : GLenum );
--procedure glEGLImageTargetTexture2DOES ( target : GLenum; image : GLeglImageOES );
--procedure glEGLImageTargetRenderbufferStorageOES (target : GLenum; image : GLeglImageOES );


-----------------------------------------------------------------------------
-- GLU - OpenGL Utility Library
-----------------------------------------------------------------------------

-- Extensions

GLU_EXT_object_space_tess  : constant GLenum := 1;
GLU_EXT_nurbs_tessellator  : constant GLenum := 1;

-- Boolean

GLU_FALSE                  : constant GLenum := 0;
GLU_TRUE                   : constant GLenum := 1;

-- Version

GLU_VERSION_1_1            : constant GLenum := 1;
GLU_VERSION_1_2            : constant GLenum := 1;
GLU_VERSION_1_3            : constant GLenum := 1;

-- String Name

GLU_VERSION                : constant GLenum := 100800;
GLU_EXTENSIONS             : constant GLenum := 100801;

-- Error Code

GLU_INVALID_ENUM           : constant GLenum := 100900;
GLU_INVALID_VALUE          : constant GLenum := 100901;
GLU_OUT_OF_MEMORY          : constant GLenum := 100902;
GLU_INCOMPATIBLE_GL_VERSION : constant GLenum := 100903;
GLU_INVALID_OPERATION      : constant GLenum := 100904;

-- Nurbs Display

GLU_OUTLINE_POLYGON        : constant GLenum := 100240;
GLU_OUTLINE_PATCH          : constant GLenum := 100241;

-- Nurbs Callback

type GLUnurbscallbacks is new GLenum;

GLU_NURBS_ERROR            : constant GLUnurbscallbacks := 100103;
GLU_ERROR                  : constant GLUnurbscallbacks := 100103;
GLU_NURBS_BEGIN            : constant GLUnurbscallbacks := 100164;
GLU_NURBS_BEGIN_EXT        : constant GLUnurbscallbacks := 100164;
GLU_NURBS_VERTEX           : constant GLUnurbscallbacks := 100165;
GLU_NURBS_VERTEX_EXT       : constant GLUnurbscallbacks := 100165;
GLU_NURBS_NORMAL           : constant GLUnurbscallbacks := 100166;
GLU_NURBS_NORMAL_EXT       : constant GLUnurbscallbacks := 100166;
GLU_NURBS_COLOR            : constant GLUnurbscallbacks := 100167;
GLU_NURBS_COLOR_EXT        : constant GLUnurbscallbacks := 100167;
GLU_NURBS_TEXTURE_COORD    : constant GLUnurbscallbacks := 100168;
GLU_NURBS_TEX_COORD_EXT    : constant GLUnurbscallbacks := 100168;
GLU_NURBS_END              : constant GLUnurbscallbacks := 100169;
GLU_NURBS_END_EXT          : constant GLUnurbscallbacks := 100169;
GLU_NURBS_BEGIN_DATA       : constant GLUnurbscallbacks := 100170;
GLU_NURBS_BEGIN_DATA_EXT   : constant GLUnurbscallbacks := 100170;
GLU_NURBS_VERTEX_DATA      : constant GLUnurbscallbacks := 100171;
GLU_NURBS_VERTEX_DATA_EXT  : constant GLUnurbscallbacks := 100171;
GLU_NURBS_NORMAL_DATA      : constant GLUnurbscallbacks := 100172;
GLU_NURBS_NORMAL_DATA_EXT  : constant GLUnurbscallbacks := 100172;
GLU_NURBS_COLOR_DATA       : constant GLUnurbscallbacks := 100173;
GLU_NURBS_COLOR_DATA_EXT   : constant GLUnurbscallbacks := 100173;
GLU_NURBS_TEXTURE_COORD_DATA : constant GLUnurbscallbacks := 100174;
GLU_NURBS_TEX_COORD_DATA_EXT : constant GLUnurbscallbacks := 100174;
GLU_NURBS_END_DATA         : constant GLUnurbscallbacks := 100175;
GLU_NURBS_END_DATA_EXT     : constant GLUnurbscallbacks := 100175;

-- Nurbs Error

GLU_NURBS_ERROR1           : constant GLenum := 100251;
GLU_NURBS_ERROR2           : constant GLenum := 100252;
GLU_NURBS_ERROR3           : constant GLenum := 100253;
GLU_NURBS_ERROR4           : constant GLenum := 100254;
GLU_NURBS_ERROR5           : constant GLenum := 100255;
GLU_NURBS_ERROR6           : constant GLenum := 100256;
GLU_NURBS_ERROR7           : constant GLenum := 100257;
GLU_NURBS_ERROR8           : constant GLenum := 100258;
GLU_NURBS_ERROR9           : constant GLenum := 100259;
GLU_NURBS_ERROR10          : constant GLenum := 100260;
GLU_NURBS_ERROR11          : constant GLenum := 100261;
GLU_NURBS_ERROR12          : constant GLenum := 100262;
GLU_NURBS_ERROR13          : constant GLenum := 100263;
GLU_NURBS_ERROR14          : constant GLenum := 100264;
GLU_NURBS_ERROR15          : constant GLenum := 100265;
GLU_NURBS_ERROR16          : constant GLenum := 100266;
GLU_NURBS_ERROR17          : constant GLenum := 100267;
GLU_NURBS_ERROR18          : constant GLenum := 100268;
GLU_NURBS_ERROR19          : constant GLenum := 100269;
GLU_NURBS_ERROR20          : constant GLenum := 100270;
GLU_NURBS_ERROR21          : constant GLenum := 100271;
GLU_NURBS_ERROR22          : constant GLenum := 100272;
GLU_NURBS_ERROR23          : constant GLenum := 100273;
GLU_NURBS_ERROR24          : constant GLenum := 100274;
GLU_NURBS_ERROR25          : constant GLenum := 100275;
GLU_NURBS_ERROR26          : constant GLenum := 100276;
GLU_NURBS_ERROR27          : constant GLenum := 100277;
GLU_NURBS_ERROR28          : constant GLenum := 100278;
GLU_NURBS_ERROR29          : constant GLenum := 100279;
GLU_NURBS_ERROR30          : constant GLenum := 100280;
GLU_NURBS_ERROR31          : constant GLenum := 100281;
GLU_NURBS_ERROR32          : constant GLenum := 100282;
GLU_NURBS_ERROR33          : constant GLenum := 100283;
GLU_NURBS_ERROR34          : constant GLenum := 100284;
GLU_NURBS_ERROR35          : constant GLenum := 100285;
GLU_NURBS_ERROR36          : constant GLenum := 100286;
GLU_NURBS_ERROR37          : constant GLenum := 100287;

-- Nurbs Property

type GLUnurbsproperties is new GLenum;

GLU_AUTO_LOAD_MATRIX       : constant GLUnurbsproperties := 100200;
GLU_CULLING                : constant GLUnurbsproperties := 100201;
GLU_SAMPLING_TOLERANCE     : constant GLUnurbsproperties := 100203;
GLU_DISPLAY_MODE           : constant GLUnurbsproperties := 100204;
GLU_PARAMETRIC_TOLERANCE   : constant GLUnurbsproperties := 100202;
GLU_SAMPLING_METHOD        : constant GLUnurbsproperties := 100205;
GLU_U_STEP                 : constant GLUnurbsproperties := 100206;
GLU_V_STEP                 : constant GLUnurbsproperties := 100207;
GLU_NURBS_MODE             : constant GLUnurbsproperties := 100160;
GLU_NURBS_MODE_EXT         : constant GLUnurbsproperties := 100160;
GLU_NURBS_TESSELLATOR      : constant GLUnurbsproperties := 100161;
GLU_NURBS_TESSELLATOR_EXT  : constant GLUnurbsproperties := 100161;
GLU_NURBS_RENDERER         : constant GLUnurbsproperties := 100162;
GLU_NURBS_RENDERER_EXT     : constant GLUnurbsproperties := 100162;

-- Nurbs Sampling

GLU_OBJECT_PARAMETRIC_ERROR: constant GLenum := 100208;
GLU_OBJECT_PARAMETRIC_ERROR_EXT : constant GLenum := 100208;
GLU_OBJECT_PATH_LENGTH     : constant GLenum := 100209;
GLU_OBJECT_PATH_LENGTH_EXT : constant GLenum := 100209;
GLU_PATH_LENGTH            : constant GLenum := 100215;
GLU_PARAMETRIC_ERROR       : constant GLenum := 100216;
GLU_DOMAIN_DISTANCE        : constant GLenum := 100217;

-- Nurbs Trim

GLU_MAP1_TRIM_2            : constant GLevaluators := 100210;
GLU_MAP1_TRIM_3            : constant GLevaluators := 100211;

-- Quadric Draw Style

type GLUquaddrawstyle is new GLenum;

GLU_POINT                  : constant GLUquaddrawstyle := 100010;
GLU_LINE                   : constant GLUquaddrawstyle := 100011;
GLU_FILL                   : constant GLUquaddrawstyle := 100012;
GLU_SILHOUETTE             : constant GLUquaddrawstyle := 100013;

-- Quadric Normal

type GLUquadricnormal is new GLenum;

GLU_SMOOTH                 : constant GLUquadricnormal := 100000;
GLU_FLAT                   : constant GLUquadricnormal := 100001;
GLU_NONE                   : constant GLUquadricnormal := 100002;

-- Quadric Orientation

type GLUquadorientation is new GLenum;

GLU_OUTSIDE                : constant GLUquadorientation := 100020;
GLU_INSIDE                 : constant GLUquadorientation := 100021;

-- Tess Callback

type GLUtesscallbacks is new GLenum;

GLU_TESS_BEGIN             : constant GLUtesscallbacks := 100100;
GLU_BEGIN                  : constant GLUtesscallbacks := 100100;
GLU_TESS_VERTEX            : constant GLUtesscallbacks := 100101;
GLU_VERTEX                 : constant GLUtesscallbacks := 100101;
GLU_TESS_END               : constant GLUtesscallbacks := 100102;
GLU_END                    : constant GLUtesscallbacks := 100102;
GLU_TESS_ERROR             : constant GLUtesscallbacks := 100103;
GLU_TESS_EDGE_FLAG         : constant GLUtesscallbacks := 100104;
GLU_EDGE_FLAG              : constant GLUtesscallbacks := 100104;
GLU_TESS_COMBINE           : constant GLUtesscallbacks := 100105;
GLU_TESS_BEGIN_DATA        : constant GLUtesscallbacks := 100106;
GLU_TESS_VERTEX_DATA       : constant GLUtesscallbacks := 100107;
GLU_TESS_END_DATA          : constant GLUtesscallbacks := 100108;
GLU_TESS_ERROR_DATA        : constant GLUtesscallbacks := 100109;
GLU_TESS_EDGE_FLAG_DATA    : constant GLUtesscallbacks := 100110;
GLU_TESS_COMBINE_DATA      : constant GLUtesscallbacks := 100111;

-- Tess Contour

type GLUtesscontour is new GLenum;

GLU_CW                     : constant GLUtesscontour := 100120;
GLU_CCW                    : constant GLUtesscontour := 100121;
GLU_INTERIOR               : constant GLUtesscontour := 100122;
GLU_EXTERIOR               : constant GLUtesscontour := 100123;
GLU_UNKNOWN                : constant GLUtesscontour := 100124;

-- Tess Property

type GLUtessproperties is new GLenum;

GLU_TESS_WINDING_RULE      : constant GLUtessproperties := 100140;
GLU_TESS_BOUNDARY_ONLY     : constant GLUtessproperties := 100141;
GLU_TESS_TOLERANCE         : constant GLUtessproperties := 100142;

-- Tess Error

GLU_TESS_ERROR1            : constant GLenum := 100151;
GLU_TESS_ERROR2            : constant GLenum := 100152;
GLU_TESS_ERROR3            : constant GLenum := 100153;
GLU_TESS_ERROR4            : constant GLenum := 100154;
GLU_TESS_ERROR5            : constant GLenum := 100155;
GLU_TESS_ERROR6            : constant GLenum := 100156;
GLU_TESS_ERROR7            : constant GLenum := 100157;
GLU_TESS_ERROR8            : constant GLenum := 100158;
GLU_TESS_MISSING_BEGIN_POLYGON : constant GLenum := 100151;
GLU_TESS_MISSING_BEGIN_CONTOUR : constant GLenum := 100152;
GLU_TESS_MISSING_END_POLYGON : constant GLenum := 100153;
GLU_TESS_MISSING_END_CONTOUR : constant GLenum := 100154;
GLU_TESS_COORD_TOO_LARGE   : constant GLenum := 100155;
GLU_TESS_NEED_COMBINE_CALLBACK : constant GLenum := 100156;

-- Tess Winding

GLU_TESS_WINDING_ODD       : constant GLenum := 100130;
GLU_TESS_WINDING_NONZERO   : constant GLenum := 100131;
GLU_TESS_WINDING_POSITIVE  : constant GLenum := 100132;
GLU_TESS_WINDING_NEGATIVE  : constant GLenum := 100133;
GLU_TESS_WINDING_ABS_GEQ_TWO : constant GLenum := 100134;

GLU_TESS_MAX_COORD : constant long_float := 1.0e150;

type GLUnurbs_Ptr is new System.Address;
type GLUtessellator_Ptr is new System.Address;
type GLUquadratic_Ptr is new System.Address;

procedure gluBeginCurve ( nurb : GLUnurbs_Ptr );
procedure gluBeginPolygon ( tess : GLUtessellator_Ptr);
procedure gluBeginSurface ( nurb : GLUnurbs_Ptr );
procedure gluBeginTrim ( nurb : GLUnurbs_Ptr);
function  gluBuild1DMipmapLevels ( target : GLenum; internalFormat : GLbuffers; width : GLsizei; format : GLbuffers; kind : GLtypes; level, base, max : GLint; data : System.address) return GLint;
function  gluBuild1DMipmaps ( target : GLenum; internalFormat : GLbuffers; width : GLsizei; format : GLbuffers; kind : GLtypes; data : System.address) return GLint;
function  gluBuild2DMipmapLevels ( target : GLenum; internalFormat : GLbuffers;  width, height : GLsizei; format : GLbuffers; kind : GLtypes; level, base, max : GLint; data : System.address) return GLint;
function  gluBuild2DMipmaps ( target : GLenum; internalFormat : GLbuffers; width, height : GLsizei; format : GLbuffers; kind : GLtypes; data : System.address) return GLint;
function  gluBuild3DMipmapLevels ( taraget : GLenum; internalFormat : GLbuffers; width, height, depth : GLsizei; format : GLbuffers;  kind : GLtypes; level, base, max : GLint; data : System.address) return GLint;
function  gluBuild3DMipmaps ( target : GLenum; internalFormat : GLbuffers; width, height, depth : GLsizei; format : GLbuffers; kind : GLtypes; data : System.address ) return GLint;
function gluCheckExtension ( extName, extString : char_array) return GLboolean;
procedure gluCylinder (quad : GLUquadratic_Ptr; base, top, height : GLdouble; slices : GLint; stacks : GLint);
procedure gluDeleteNurbsRenderer ( nurb : GLUnurbs_Ptr );
procedure gluDeleteQuadric ( quad : GLUquadratic_Ptr );
procedure gluDeleteTess ( tess : GLUtessellator_Ptr );
procedure gluDisk ( quad : GLUquadratic_Ptr; inner, outer : GLdouble; slices : GLint; loops : GLint );
procedure gluEndCurve ( nurb : GLUnurbs_Ptr );
procedure gluEndPolygon ( tess : GLUtessellator_Ptr );
procedure gluEndSurface ( nurb : GLUnurbs_Ptr );
procedure gluEndTrim ( nurb : GLUnurbs_Ptr );
function  gluErrorString ( error : GLenum ) return char_array_ptr;
procedure gluGetNurbsProperty ( nurb : GLUnurbs_Ptr; property : GLUnurbsproperties; data : GL_Float_Array_Ptr );
function  gluGetString ( name : GLenum ) return char_array_ptr;
procedure gluGetTessProperty (tess : GLUtessellator_Ptr; which : GLUtessproperties; data : GL_Float_Array_Ptr );
procedure gluLoadSamplingMatrices ( nurb : GLUnurbs_Ptr; model : GL_Float_Array_Ptr; perspective : GL_Float_Array_Ptr; view : GL_Int_Array_Ptr );
procedure gluLookAt ( eyeX, eyeY, eyeZ, centerX, centerY, centerZ, upX, upY, upZ : GLdouble );
function  gluNewNurbsRenderer return GLUnurbs_Ptr;
function  gluNewQuadric return GLUquadratic_Ptr;
function  gluNewTess return GLUtessellator_Ptr;
procedure gluNextContour ( tess : GLUtessellator_Ptr; kind : GLUtesscontour );
procedure gluNurbsCallback ( nurb : GLUnurbs_Ptr; which : GLUnurbscallbacks; CallBackFunc : System.address );
procedure gluNurbsCallbackData ( nurb : GLUnurbs_Ptr; userData : System.address );
procedure gluNurbsCallbackDataEXT ( nurb : GLUnurbs_Ptr; userData : System.address );
procedure gluNurbsCurve ( nurb : GLUnurbs_Ptr; knotCount : GLint; knots : GL_Float_Array_Ptr; stride : GLint; control : GL_Float_Array_Ptr; order : GLint; kind : GLevaluators );
procedure gluNurbsProperty ( nurb : GLUnurbs_Ptr; property : GLUnurbsproperties; value : GLfloat );
procedure gluNurbsSurface ( nurb : GLUnurbs_Ptr; sKnotCount : GLint;  sKnots : GL_Float_Array_Ptr; tKnotCount : GLint;  tKnots : GL_Float_Array_Ptr; sStride, tStride : GLint; control : GL_Float_Array_Ptr; sOrder, tOrder : GLint; kind : GLenum );
procedure gluOrtho2D ( left, right, bottom, top : GLdouble );
procedure gluPartialDisk ( quad : GLUquadratic_Ptr; inner, outer : GLdouble; slices : GLint; loops : GLint; start : GLdouble; sweep : GLdouble );
procedure gluPerspective ( fovy, aspect, zNear, zFar : GLdouble );
procedure gluPickMatrix ( x, y : GLdouble; delX, delY : GLdouble; viewport : in out GLint );
function  gluProject ( objX, objY, objZ : GLdouble; model, proj : GL_Double_Array_Ptr; view : GL_Int_Array_Ptr; winX, winY, winZ : GL_Double_Array_Ptr ) return GLint;
procedure gluPwlCurve ( nurb : GLUnurbs_Ptr; count : GLint; data : GL_Float_Array_Ptr; stride : GLint; kind : GLevaluators );
procedure gluQuadricCallback ( quad : GLUquadratic_Ptr; which : GLUnurbscallbacks; CallBackFunc : System.address );
procedure gluQuadricDrawStyle ( quad : GLUquadratic_Ptr; draw : GLUquaddrawstyle );
procedure gluQuadricNormals ( quad : GLUquadratic_Ptr; normal : GLUquadricnormal );
procedure gluQuadricOrientation ( quad : GLUquadratic_Ptr; orientation : GLUquadorientation );
procedure gluQuadricTexture ( quad : GLUquadratic_Ptr; texture : GLboolean );
function  gluScaleImage ( format : GLbuffers; wIn, hIn : GLsizei; typeIn : GLtypes; dataIn : System.address; wOut, hOut : GLsizei; typeOut : GLtypes; dataOut : System.address ) return GLint;
procedure gluSphere ( quad : GLUquadratic_Ptr; radius : GLdouble; slices : GLint; stacks : GLint );
procedure gluTessBeginContour ( tess : GLUtessellator_Ptr );
procedure gluTessBeginPolygon ( tess : GLUtessellator_Ptr; data : System.address );
procedure gluTessCallback (tess : GLUtessellator_Ptr; which : GLUtesscallbacks; CallBackFunc : System.address );
procedure gluTessEndContour ( tess : GLUtessellator_Ptr );
procedure gluTessEndPolygon ( tess : GLUtessellator_Ptr );
procedure gluTessNormal ( tess : GLUtessellator_Ptr; valueX, valueY, valueZ : GLdouble );
procedure gluTessProperty ( tess : GLUtessellator_Ptr; which : GLUtessproperties; data : GLdouble );
procedure gluTessVertex ( tess : GLUtessellator_Ptr; location : in out GLdouble; data : System.address );
function  gluUnProject ( winX, winY, winZ : GLdouble; model : GL_Double_Array_Ptr; proj : GL_Double_Array_Ptr; view : GL_Int_Array_Ptr; objX, objY, objZ : GL_Double_Array_Ptr) return GLint;

function  gluUnProject4 ( winX, winY, winZ : GLdouble; clipW : GLdouble; model : GL_Double_Array_Ptr; proj : GL_Double_Array_Ptr; view : GL_Int_Array_Ptr; nearVal, farVal : GLdouble; objX, objY, objZ, objW : GL_Double_Array_Ptr ) return GLint;


-----------------------------------------------------------------------------
-- Import from C
-----------------------------------------------------------------------------


pragma import( C, glAccum, "glAccum" );
pragma import( C, glActiveTexture, "glActiveTexture" );
pragma import( C, glActiveTextureARB, "glActiveTextureARB" );
pragma import( C, glAlphaFunc, "glAlphaFunc" );
pragma import( C, glAreTexturesResident, "glAreTexturesResident" );
pragma import( C, glArrayElement, "glArrayElement" );
pragma import( C, glBegin, "glBegin" );
pragma import( C, glBindTexture, "glBindTexture" );
pragma import( C, glBitmap, "glBitmap" );
pragma import( C, glBlendColor, "glBlendColor" );
pragma import( C, glBlendEquation, "glBlendEquation" );
pragma import( C, glBlendFunc, "glBlendFunc" );
pragma import( C, glCallList, "glCallList" );
pragma import( C, glCallLists, "glCallLists" );
pragma import( C, glClear, "glClear" );
pragma import( C, glClearAccum, "glClearAccum" );
pragma import( C, glClearColor, "glClearColor" );
pragma import( C, glClearDepth, "glClearDepth" );
pragma import( C, glClearIndex, "glClearIndex" );
pragma import( C, glClearStencil, "glClearStencil" );
pragma import( C, glClientActiveTexture, "glClientActiveTexture" );
pragma import( C, glClientActiveTextureARB, "glClientActiveTextureARB" );
pragma import( C, glClipPlane, "glClipPlane" );
pragma import( C, glColor3b, "glColor3b" );
pragma import( C, glColor3bv, "glColor3bv" );
pragma import( C, glColor3d, "glColor3d" );
pragma import( C, glColor3dv, "glColor3dv" );
pragma import( C, glColor3f, "glColor3f" );
pragma import( C, glColor3fv, "glColor3fv" );
pragma import( C, glColor3i, "glColor3i" );
pragma import( C, glColor3iv, "glColor3iv" );
pragma import( C, glColor3s, "glColor3s" );
pragma import( C, glColor3sv, "glColor3sv" );
pragma import( C, glColor3ub, "glColor3ub" );
pragma import( C, glColor3ubv, "glColor3ubv" );
pragma import( C, glColor3ui, "glColor3ui" );
pragma import( C, glColor3uiv, "glColor3uiv" );
pragma import( C, glColor3us, "glColor3us" );
pragma import( C, glColor3usv, "glColor3usv" );
pragma import( C, glColor4b, "glColor4b" );
pragma import( C, glColor4bv, "glColor4bv" );
pragma import( C, glColor4d, "glColor4d" );
pragma import( C, glColor4dv, "glColor4dv" );
pragma import( C, glColor4f, "glColor4f" );
pragma import( C, glColor4fv, "glColor4fv" );
pragma import( C, glColor4i, "glColor4i" );
pragma import( C, glColor4iv, "glColor4iv" );
pragma import( C, glColor4s, "glColor4s" );
pragma import( C, glColor4sv, "glColor4sv" );
pragma import( C, glColor4ub, "glColor4ub" );
pragma import( C, glColor4ubv, "glColor4ubv" );
pragma import( C, glColor4ui, "glColor4ui" );
pragma import( C, glColor4uiv, "glColor4uiv" );
pragma import( C, glColor4us, "glColor4us" );
pragma import( C, glColor4usv, "glColor4usv" );
pragma import( C, glColorMask, "glColorMask" );
pragma import( C, glColorMaterial, "glColorMaterial" );
pragma import( C, glColorPointer, "glColorPointer" );
pragma import( C, glColorSubTable, "glColorSubTable" );
pragma import( C, glColorTable, "glColorTable" );
pragma import( C, glColorTableParameterfv, "glColorTableParameterfv" );
pragma import( C, glColorTableParameteriv, "glColorTableParameteriv" );
pragma import( C, glCompressedTexImage1D, "glCompressedTexImage1D" );
pragma import( C, glCompressedTexImage2D, "glCompressedTexImage2D" );
pragma import( C, glCompressedTexImage3D, "glCompressedTexImage3D" );
pragma import( C, glCompressedTexSubImage1D, "glCompressedTexSubImage1D" );
pragma import( C, glCompressedTexSubImage2D, "glCompressedTexSubImage2D" );
pragma import( C, glCompressedTexSubImage3D, "glCompressedTexSubImage3D" );
pragma import( C, glConvolutionFilter1D, "glConvolutionFilter1D" );
pragma import( C, glConvolutionFilter2D, "glConvolutionFilter2D" );
pragma import( C, glConvolutionParameterf, "glConvolutionParameterf" );
pragma import( C, glConvolutionParameterfv, "glConvolutionParameterfv" );
pragma import( C, glConvolutionParameteri, "glConvolutionParameteri" );
pragma import( C, glConvolutionParameteriv, "glConvolutionParameteriv" );
pragma import( C, glCopyColorSubTable, "glCopyColorSubTable" );
pragma import( C, glCopyColorTable, "glCopyColorTable" );
pragma import( C, glCopyConvolutionFilter1D, "glCopyConvolutionFilter1D" );
pragma import( C, glCopyConvolutionFilter2D, "glCopyConvolutionFilter2D" );
pragma import( C, glCopyPixels, "glCopyPixels" );
pragma import( C, glCopyTexImage1D, "glCopyTexImage1D" );
pragma import( C, glCopyTexImage2D, "glCopyTexImage2D" );
pragma import( C, glCopyTexSubImage1D, "glCopyTexSubImage1D" );
pragma import( C, glCopyTexSubImage2D, "glCopyTexSubImage2D" );
pragma import( C, glCopyTexSubImage3D, "glCopyTexSubImage3D" );
pragma import( C, glCullFace, "glCullFace" );
pragma import( C, glDeleteLists, "glDeleteLists" );
pragma import( C, glDeleteTextures, "glDeleteTextures" );
pragma import( C, glDepthFunc, "glDepthFunc" );
pragma import( C, glDepthMask, "glDepthMask" );
pragma import( C, glDepthRange, "glDepthRange" );
pragma import( C, glDisable, "glDisable" );
pragma import( C, glDisableClientState, "glDisableClientState" );
pragma import( C, glDrawArrays, "glDrawArrays" );
pragma import( C, glDrawBuffer, "glDrawBuffer" );
pragma import( C, glDrawElements, "glDrawElements" );
pragma import( C, glDrawPixels, "glDrawPixels" );
pragma import( C, glDrawRangeElements, "glDrawRangeElements" );
pragma import( C, glEdgeFlag, "glEdgeFlag" );
pragma import( C, glEdgeFlagPointer, "glEdgeFlagPointer" );
pragma import( C, glEdgeFlagv, "glEdgeFlagv" );
pragma import( C, glEnable, "glEnable" );
pragma import( C, glEnableClientState, "glEnableClientState" );
pragma import( C, glEnd, "glEnd" );
pragma import( C, glEndList, "glEndList" );
pragma import( C, glEvalCoord1d, "glEvalCoord1d" );
pragma import( C, glEvalCoord1dv, "glEvalCoord1dv" );
pragma import( C, glEvalCoord1f, "glEvalCoord1f" );
pragma import( C, glEvalCoord1fv, "glEvalCoord1fv" );
pragma import( C, glEvalCoord2d, "glEvalCoord2d" );
pragma import( C, glEvalCoord2dv, "glEvalCoord2dv" );
pragma import( C, glEvalCoord2f, "glEvalCoord2f" );
pragma import( C, glEvalCoord2fv, "glEvalCoord2fv" );
pragma import( C, glEvalMesh1, "glEvalMesh1" );
pragma import( C, glEvalMesh2, "glEvalMesh2" );
pragma import( C, glEvalPoint1, "glEvalPoint1" );
pragma import( C, glEvalPoint2, "glEvalPoint2" );
pragma import( C, glFeedbackBuffer, "glFeedbackBuffer" );
pragma import( C, glFinish, "glFinish" );
pragma import( C, glFlush, "glFlush" );
pragma import( C, glFogf, "glFogf" );
pragma import( C, glFogfv, "glFogfv" );
pragma import( C, glFogi, "glFogi" );
pragma import( C, glFogiv, "glFogiv" );
pragma import( C, glFrontFace, "glFrontFace" );
pragma import( C, glFrustum, "glFrustum" );
pragma import( C, glGenLists, "glGenLists" );
pragma import( C, glGenTextures, "glGenTextures" );
pragma import( C, glGetBooleanv, "glGetBooleanv" );
pragma import( C, glGetClipPlane, "glGetClipPlane" );
pragma import( C, glGetColorTable, "glGetColorTable" );
pragma import( C, glGetColorTableParameterfv, "glGetColorTableParameterfv" );
pragma import( C, glGetColorTableParameteriv, "glGetColorTableParameteriv" );
pragma import( C, glGetCompressedTexImage, "glGetCompressedTexImage" );
pragma import( C, glGetConvolutionFilter, "glGetConvolutionFilter" );
pragma import( C, glGetConvolutionParameterfv, "glGetConvolutionParameterfv" );
pragma import( C, glGetConvolutionParameteriv, "glGetConvolutionParameteriv" );
pragma import( C, glGetDoublev, "glGetDoublev" );
pragma import( C, glGetError, "glGetError" );
pragma import( C, glGetFloatv, "glGetFloatv" );
pragma import( C, glGetHistogram, "glGetHistogram" );
pragma import( C, glGetHistogramParameterfv, "glGetHistogramParameterfv" );
pragma import( C, glGetHistogramParameteriv, "glGetHistogramParameteriv" );
pragma import( C, glGetIntegerv, "glGetIntegerv" );
pragma import( C, glGetLightfv, "glGetLightfv" );
pragma import( C, glGetLightiv, "glGetLightiv" );
pragma import( C, glGetMapdv, "glGetMapdv" );
pragma import( C, glGetMapfv, "glGetMapfv" );
pragma import( C, glGetMapiv, "glGetMapiv" );
pragma import( C, glGetMaterialfv, "glGetMaterialfv" );
pragma import( C, glGetMaterialiv, "glGetMaterialiv" );
pragma import( C, glGetMinmax, "glGetMinmax" );
pragma import( C, glGetMinmaxParameterfv, "glGetMinmaxParameterfv" );
pragma import( C, glGetMinmaxParameteriv, "glGetMinmaxParameteriv" );
pragma import( C, glGetPixelMapfv, "glGetPixelMapfv" );
pragma import( C, glGetPixelMapuiv, "glGetPixelMapuiv" );
pragma import( C, glGetPixelMapusv, "glGetPixelMapusv" );
pragma import( C, glGetPointerv, "glGetPointerv" );
pragma import( C, glGetPolygonStipple, "glGetPolygonStipple" );
pragma import( C, glGetSeparableFilter, "glGetSeparableFilter" );
pragma import( C, glGetString, "glGetString" );
pragma import( C, glGetTexEnvfv, "glGetTexEnvfv" );
pragma import( C, glGetTexEnviv, "glGetTexEnviv" );
pragma import( C, glGetTexGendv, "glGetTexGendv" );
pragma import( C, glGetTexGenfv, "glGetTexGenfv" );
pragma import( C, glGetTexGeniv, "glGetTexGeniv" );
pragma import( C, glGetTexImage, "glGetTexImage" );
pragma import( C, glGetTexLevelParameterfv, "glGetTexLevelParameterfv" );
pragma import( C, glGetTexLevelParameteriv, "glGetTexLevelParameteriv" );
pragma import( C, glGetTexParameterfv, "glGetTexParameterfv" );
pragma import( C, glGetTexParameteriv, "glGetTexParameteriv" );
pragma import( C, glHint, "glHint" );
pragma import( C, glHistogram, "glHistogram" );
pragma import( C, glIndexd, "glIndexd" );
pragma import( C, glIndexdv, "glIndexdv" );
pragma import( C, glIndexf, "glIndexf" );
pragma import( C, glIndexfv, "glIndexfv" );
pragma import( C, glIndexi, "glIndexi" );
pragma import( C, glIndexiv, "glIndexiv" );
pragma import( C, glIndexMask, "glIndexMask" );
pragma import( C, glIndexPointer, "glIndexPointer" );
pragma import( C, glIndexs, "glIndexs" );
pragma import( C, glIndexsv, "glIndexsv" );
pragma import( C, glIndexub, "glIndexub" );
pragma import( C, glIndexubv, "glIndexubv" );
pragma import( C, glInitNames, "glInitNames" );
pragma import( C, glInterleavedArrays, "glInterleavedArrays" );
pragma import( C, glIsEnabled, "glIsEnabled" );
pragma import( C, glIsList, "glIsList" );
pragma import( C, glIsTexture, "glIsTexture" );
pragma import( C, glLightf, "glLightf" );
pragma import( C, glLightfv, "glLightfv" );
pragma import( C, glLighti, "glLighti" );
pragma import( C, glLightiv, "glLightiv" );
pragma import( C, glLightModelf, "glLightModelf" );
pragma import( C, glLightModelfv, "glLightModelfv" );
pragma import( C, glLightModeli, "glLightModeli" );
pragma import( C, glLightModeliv, "glLightModeliv" );
pragma import( C, glLineStipple, "glLineStipple" );
pragma import( C, glLineWidth, "glLineWidth" );
pragma import( C, glListBase, "glListBase" );
pragma import( C, glLoadIdentity, "glLoadIdentity" );
pragma import( C, glLoadMatrixd, "glLoadMatrixd" );
pragma import( C, glLoadMatrixf, "glLoadMatrixf" );
pragma import( C, glLoadName, "glLoadName" );
pragma import( C, glLogicOp, "glLogicOp" );
pragma import( C, glMap1d, "glMap1d" );
pragma import( C, glMap1f, "glMap1f" );
pragma import( C, glMap2d, "glMap2d" );
pragma import( C, glMap2f, "glMap2f" );
pragma import( C, glMapGrid1d, "glMapGrid1d" );
pragma import( C, glMapGrid1f, "glMapGrid1f" );
pragma import( C, glMapGrid2d, "glMapGrid2d" );
pragma import( C, glMapGrid2f, "glMapGrid2f" );
pragma import( C, glMaterialf, "glMaterialf" );
pragma import( C, glMaterialfv, "glMaterialfv" );
pragma import( C, glMateriali, "glMateriali" );
pragma import( C, glMaterialiv, "glMaterialiv" );
pragma import( C, glMatrixMode, "glMatrixMode" );
pragma import( C, glMinmax, "glMinmax" );
pragma import( C, glMultiTexCoord1d, "glMultiTexCoord1d" );
pragma import( C, glMultiTexCoord1dARB, "glMultiTexCoord1dARB" );
pragma import( C, glMultiTexCoord1dv, "glMultiTexCoord1dv" );
pragma import( C, glMultiTexCoord1dvARB, "glMultiTexCoord1dvARB" );
pragma import( C, glMultiTexCoord1f, "glMultiTexCoord1f" );
pragma import( C, glMultiTexCoord1fARB, "glMultiTexCoord1fARB" );
pragma import( C, glMultiTexCoord1fv, "glMultiTexCoord1fv" );
pragma import( C, glMultiTexCoord1fvARB, "glMultiTexCoord1fvARB" );
pragma import( C, glMultiTexCoord1i, "glMultiTexCoord1i" );
pragma import( C, glMultiTexCoord1iARB, "glMultiTexCoord1iARB" );
pragma import( C, glMultiTexCoord1iv, "glMultiTexCoord1iv" );
pragma import( C, glMultiTexCoord1ivARB, "glMultiTexCoord1ivARB" );
pragma import( C, glMultiTexCoord1s, "glMultiTexCoord1s" );
pragma import( C, glMultiTexCoord1sARB, "glMultiTexCoord1sARB" );
pragma import( C, glMultiTexCoord1sv, "glMultiTexCoord1sv" );
pragma import( C, glMultiTexCoord1svARB, "glMultiTexCoord1svARB" );
pragma import( C, glMultiTexCoord2d, "glMultiTexCoord2d" );
pragma import( C, glMultiTexCoord2dARB, "glMultiTexCoord2dARB" );
pragma import( C, glMultiTexCoord2dv, "glMultiTexCoord2dv" );
pragma import( C, glMultiTexCoord2dvARB, "glMultiTexCoord2dvARB" );
pragma import( C, glMultiTexCoord2f, "glMultiTexCoord2f" );
pragma import( C, glMultiTexCoord2fARB, "glMultiTexCoord2fARB" );
pragma import( C, glMultiTexCoord2fv, "glMultiTexCoord2fv" );
pragma import( C, glMultiTexCoord2fvARB, "glMultiTexCoord2fvARB" );
pragma import( C, glMultiTexCoord2i, "glMultiTexCoord2i" );
pragma import( C, glMultiTexCoord2iARB, "glMultiTexCoord2iARB" );
pragma import( C, glMultiTexCoord2iv, "glMultiTexCoord2iv" );
pragma import( C, glMultiTexCoord2ivARB, "glMultiTexCoord2ivARB" );
pragma import( C, glMultiTexCoord2s, "glMultiTexCoord2s" );
pragma import( C, glMultiTexCoord2sARB, "glMultiTexCoord2sARB" );
pragma import( C, glMultiTexCoord2sv, "glMultiTexCoord2sv" );
pragma import( C, glMultiTexCoord2svARB, "glMultiTexCoord2svARB" );
pragma import( C, glMultiTexCoord3d, "glMultiTexCoord3d" );
pragma import( C, glMultiTexCoord3dARB, "glMultiTexCoord3dARB" );
pragma import( C, glMultiTexCoord3dv, "glMultiTexCoord3dv" );
pragma import( C, glMultiTexCoord3dvARB, "glMultiTexCoord3dvARB" );
pragma import( C, glMultiTexCoord3f, "glMultiTexCoord3f" );
pragma import( C, glMultiTexCoord3fARB, "glMultiTexCoord3fARB" );
pragma import( C, glMultiTexCoord3fv, "glMultiTexCoord3fv" );
pragma import( C, glMultiTexCoord3fvARB, "glMultiTexCoord3fvARB" );
pragma import( C, glMultiTexCoord3i, "glMultiTexCoord3i" );
pragma import( C, glMultiTexCoord3iARB, "glMultiTexCoord3iARB" );
pragma import( C, glMultiTexCoord3iv, "glMultiTexCoord3iv" );
pragma import( C, glMultiTexCoord3ivARB, "glMultiTexCoord3ivARB" );
pragma import( C, glMultiTexCoord3s, "glMultiTexCoord3s" );
pragma import( C, glMultiTexCoord3sARB, "glMultiTexCoord3sARB" );
pragma import( C, glMultiTexCoord3sv, "glMultiTexCoord3sv" );
pragma import( C, glMultiTexCoord3svARB, "glMultiTexCoord3svARB" );
pragma import( C, glMultiTexCoord4d, "glMultiTexCoord4d" );
pragma import( C, glMultiTexCoord4dARB, "glMultiTexCoord4dARB" );
pragma import( C, glMultiTexCoord4dv, "glMultiTexCoord4dv" );
pragma import( C, glMultiTexCoord4dvARB, "glMultiTexCoord4dvARB" );
pragma import( C, glMultiTexCoord4f, "glMultiTexCoord4f" );
pragma import( C, glMultiTexCoord4fARB, "glMultiTexCoord4fARB" );
pragma import( C, glMultiTexCoord4fv, "glMultiTexCoord4fv" );
pragma import( C, glMultiTexCoord4fvARB, "glMultiTexCoord4fvARB" );
pragma import( C, glMultiTexCoord4i, "glMultiTexCoord4i" );
pragma import( C, glMultiTexCoord4iARB, "glMultiTexCoord4iARB" );
pragma import( C, glMultiTexCoord4iv, "glMultiTexCoord4iv" );
pragma import( C, glMultiTexCoord4ivARB, "glMultiTexCoord4ivARB" );
pragma import( C, glMultiTexCoord4s, "glMultiTexCoord4s" );
pragma import( C, glMultiTexCoord4sARB, "glMultiTexCoord4sARB" );
pragma import( C, glMultiTexCoord4sv, "glMultiTexCoord4sv" );
pragma import( C, glMultiTexCoord4svARB, "glMultiTexCoord4svARB" );
pragma import( C, glMultMatrixd, "glMultMatrixd" );
pragma import( C, glMultMatrixf, "glMultMatrixf" );
pragma import( C, glNewList, "glNewList" );
pragma import( C, glNormal3b, "glNormal3b" );
pragma import( C, glNormal3bv, "glNormal3bv" );
pragma import( C, glNormal3d, "glNormal3d" );
pragma import( C, glNormal3dv, "glNormal3dv" );
pragma import( C, glNormal3f, "glNormal3f" );
pragma import( C, glNormal3fv, "glNormal3fv" );
pragma import( C, glNormal3i, "glNormal3i" );
pragma import( C, glNormal3iv, "glNormal3iv" );
pragma import( C, glNormal3s, "glNormal3s" );
pragma import( C, glNormal3sv, "glNormal3sv" );
pragma import( C, glNormalPointer, "glNormalPointer" );
pragma import( C, glOrtho, "glOrtho" );
pragma import( C, glPassThrough, "glPassThrough" );
pragma import( C, glPixelMapfv, "glPixelMapfv" );
pragma import( C, glPixelMapuiv, "glPixelMapuiv" );
pragma import( C, glPixelMapusv, "glPixelMapusv" );
pragma import( C, glPixelStoref, "glPixelStoref" );
pragma import( C, glPixelStorei, "glPixelStorei" );
pragma import( C, glPixelTransferf, "glPixelTransferf" );
pragma import( C, glPixelTransferi, "glPixelTransferi" );
pragma import( C, glPixelZoom, "glPixelZoom" );
pragma import( C, glPointSize, "glPointSize" );
pragma import( C, glPolygonMode, "glPolygonMode" );
pragma import( C, glPolygonOffset, "glPolygonOffset" );
pragma import( C, glPolygonStipple, "glPolygonStipple" );
pragma import( C, glPopAttrib, "glPopAttrib" );
pragma import( C, glPopClientAttrib, "glPopClientAttrib" );
pragma import( C, glPopMatrix, "glPopMatrix" );
pragma import( C, glPopName, "glPopName" );
pragma import( C, glPrioritizeTextures, "glPrioritizeTextures" );
pragma import( C, glPushAttrib, "glPushAttrib" );
pragma import( C, glPushClientAttrib, "glPushClientAttrib" );
pragma import( C, glPushMatrix, "glPushMatrix" );
pragma import( C, glPushName, "glPushName" );
pragma import( C, glRasterPos2d, "glRasterPos2d" );
pragma import( C, glRasterPos2dv, "glRasterPos2dv" );
pragma import( C, glRasterPos2f, "glRasterPos2f" );
pragma import( C, glRasterPos2fv, "glRasterPos2fv" );
pragma import( C, glRasterPos2i, "glRasterPos2i" );
pragma import( C, glRasterPos2iv, "glRasterPos2iv" );
pragma import( C, glRasterPos2s, "glRasterPos2s" );
pragma import( C, glRasterPos2sv, "glRasterPos2sv" );
pragma import( C, glRasterPos3d, "glRasterPos3d" );
pragma import( C, glRasterPos3dv, "glRasterPos3dv" );
pragma import( C, glRasterPos3f, "glRasterPos3f" );
pragma import( C, glRasterPos3fv, "glRasterPos3fv" );
pragma import( C, glRasterPos3i, "glRasterPos3i" );
pragma import( C, glRasterPos3iv, "glRasterPos3iv" );
pragma import( C, glRasterPos3s, "glRasterPos3s" );
pragma import( C, glRasterPos3sv, "glRasterPos3sv" );
pragma import( C, glRasterPos4d, "glRasterPos4d" );
pragma import( C, glRasterPos4dv, "glRasterPos4dv" );
pragma import( C, glRasterPos4f, "glRasterPos4f" );
pragma import( C, glRasterPos4fv, "glRasterPos4fv" );
pragma import( C, glRasterPos4i, "glRasterPos4i" );
pragma import( C, glRasterPos4iv, "glRasterPos4iv" );
pragma import( C, glRasterPos4s, "glRasterPos4s" );
pragma import( C, glRasterPos4sv, "glRasterPos4sv" );
pragma import( C, glReadBuffer, "glReadBuffer" );
pragma import( C, glReadPixels, "glReadPixels" );
pragma import( C, glRectd, "glRectd" );
pragma import( C, glRectdv, "glRectdv" );
pragma import( C, glRectf, "glRectf" );
pragma import( C, glRectfv, "glRectfv" );
pragma import( C, glRecti, "glRecti" );
pragma import( C, glRectiv, "glRectiv" );
pragma import( C, glRects, "glRects" );
pragma import( C, glRectsv, "glRectsv" );
pragma import( C, glRenderMode, "glRenderMode" );
pragma import( C, glResetHistogram, "glResetHistogram" );
pragma import( C, glResetMinmax, "glResetMinmax" );
pragma import( C, glRotated, "glRotated" );
pragma import( C, glRotatef, "glRotatef" );
pragma import( C, glSampleCoverage, "glSampleCoverage" );
pragma import( C, glScaled, "glScaled" );
pragma import( C, glScalef, "glScalef" );
pragma import( C, glScissor, "glScissor" );
pragma import( C, glSelectBuffer, "glSelectBuffer" );
pragma import( C, glSeparableFilter2D, "glSeparableFilter2D" );
pragma import( C, glShadeModel, "glShadeModel" );
pragma import( C, glStencilFunc, "glStencilFunc" );
pragma import( C, glStencilMask, "glStencilMask" );
pragma import( C, glStencilOp, "glStencilOp" );
pragma import( C, glTexCoord1d, "glTexCoord1d" );
pragma import( C, glTexCoord1dv, "glTexCoord1dv" );
pragma import( C, glTexCoord1f, "glTexCoord1f" );
pragma import( C, glTexCoord1fv, "glTexCoord1fv" );
pragma import( C, glTexCoord1i, "glTexCoord1i" );
pragma import( C, glTexCoord1iv, "glTexCoord1iv" );
pragma import( C, glTexCoord1s, "glTexCoord1s" );
pragma import( C, glTexCoord1sv, "glTexCoord1sv" );
pragma import( C, glTexCoord2d, "glTexCoord2d" );
pragma import( C, glTexCoord2dv, "glTexCoord2dv" );
pragma import( C, glTexCoord2f, "glTexCoord2f" );
pragma import( C, glTexCoord2fv, "glTexCoord2fv" );
pragma import( C, glTexCoord2i, "glTexCoord2i" );
pragma import( C, glTexCoord2iv, "glTexCoord2iv" );
pragma import( C, glTexCoord2s, "glTexCoord2s" );
pragma import( C, glTexCoord2sv, "glTexCoord2sv" );
pragma import( C, glTexCoord3d, "glTexCoord3d" );
pragma import( C, glTexCoord3dv, "glTexCoord3dv" );
pragma import( C, glTexCoord3f, "glTexCoord3f" );
pragma import( C, glTexCoord3fv, "glTexCoord3fv" );
pragma import( C, glTexCoord3i, "glTexCoord3i" );
pragma import( C, glTexCoord3iv, "glTexCoord3iv" );
pragma import( C, glTexCoord3s, "glTexCoord3s" );
pragma import( C, glTexCoord3sv, "glTexCoord3sv" );
pragma import( C, glTexCoord4d, "glTexCoord4d" );
pragma import( C, glTexCoord4dv, "glTexCoord4dv" );
pragma import( C, glTexCoord4f, "glTexCoord4f" );
pragma import( C, glTexCoord4fv, "glTexCoord4fv" );
pragma import( C, glTexCoord4i, "glTexCoord4i" );
pragma import( C, glTexCoord4iv, "glTexCoord4iv" );
pragma import( C, glTexCoord4s, "glTexCoord4s" );
pragma import( C, glTexCoord4sv, "glTexCoord4sv" );
pragma import( C, glTexCoordPointer, "glTexCoordPointer" );
pragma import( C, glTexEnvf, "glTexEnvf" );
pragma import( C, glTexEnvfv, "glTexEnvfv" );
pragma import( C, glTexEnvi, "glTexEnvi" );
pragma import( C, glTexEnviv, "glTexEnviv" );
pragma import( C, glTexGend, "glTexGend" );
pragma import( C, glTexGendv, "glTexGendv" );
pragma import( C, glTexGenf, "glTexGenf" );
pragma import( C, glTexGenfv, "glTexGenfv" );
pragma import( C, glTexGeni, "glTexGeni" );
pragma import( C, glTexGeniv, "glTexGeniv" );
pragma import( C, glTexImage1D, "glTexImage1D" );
pragma import( C, glTexImage2D, "glTexImage2D" );
pragma import( C, glTexImage3D, "glTexImage3D" );
pragma import( C, glTexParameterf, "glTexParameterf" );
pragma import( C, glTexParameterfv, "glTexParameterfv" );
pragma import( C, glTexParameteri, "glTexParameteri" );
pragma import( C, glTexParameteriv, "glTexParameteriv" );
pragma import( C, glTexSubImage1D, "glTexSubImage1D" );
pragma import( C, glTexSubImage2D, "glTexSubImage2D" );
pragma import( C, glTexSubImage3D, "glTexSubImage3D" );
pragma import( C, glTranslated, "glTranslated" );
pragma import( C, glTranslatef, "glTranslatef" );
pragma import( C, gluBeginCurve, "gluBeginCurve" );
pragma import( C, gluBeginPolygon, "gluBeginPolygon" );
pragma import( C, gluBeginSurface, "gluBeginSurface" );
pragma import( C, gluBeginTrim, "gluBeginTrim" );
pragma import( C, gluBuild1DMipmapLevels, "gluBuild1DMipmapLevels" );
pragma import( C, gluBuild1DMipmaps, "gluBuild1DMipmaps" );
pragma import( C, gluBuild2DMipmapLevels, "gluBuild2DMipmapLevels" );
pragma import( C, gluBuild2DMipmaps, "gluBuild2DMipmaps" );
pragma import( C, gluBuild3DMipmapLevels, "gluBuild3DMipmapLevels" );
pragma import( C, gluBuild3DMipmaps, "gluBuild3DMipmaps" );
pragma import( C, gluCheckExtension, "gluCheckExtension" );
pragma import( C, gluCylinder, "gluCylinder" );
pragma import( C, gluDeleteNurbsRenderer, "gluDeleteNurbsRenderer" );
pragma import( C, gluDeleteQuadric, "gluDeleteQuadric" );
pragma import( C, gluDeleteTess, "gluDeleteTess" );
pragma import( C, gluDisk, "gluDisk" );
pragma import( C, gluEndCurve, "gluEndCurve" );
pragma import( C, gluEndPolygon, "gluEndPolygon" );
pragma import( C, gluEndSurface, "gluEndSurface" );
pragma import( C, gluEndTrim, "gluEndTrim" );
pragma import( C, gluErrorString, "gluErrorString" );
pragma import( C, gluGetNurbsProperty, "gluGetNurbsProperty" );
pragma import( C, gluGetString, "gluGetString" );
pragma import( C, gluGetTessProperty, "gluGetTessProperty" );
pragma import( C, gluLoadSamplingMatrices, "gluLoadSamplingMatrices" );
pragma import( C, gluLookAt, "gluLookAt" );
pragma import( C, gluNewNurbsRenderer, "gluNewNurbsRendererreturnGLUnurbs_Ptr" );
pragma import( C, gluNewQuadric, "gluNewQuadricreturnGLUquadratic_Ptr" );
pragma import( C, gluNewTess, "gluNewTessreturnGLUtessellator_Ptr" );
pragma import( C, gluNextContour, "gluNextContour" );
pragma import( C, gluNurbsCallback, "gluNurbsCallback" );
pragma import( C, gluNurbsCallbackData, "gluNurbsCallbackData" );
pragma import( C, gluNurbsCallbackDataEXT, "gluNurbsCallbackDataEXT" );
pragma import( C, gluNurbsCurve, "gluNurbsCurve" );
pragma import( C, gluNurbsProperty, "gluNurbsProperty" );
pragma import( C, gluNurbsSurface, "gluNurbsSurface" );
pragma import( C, gluOrtho2D, "gluOrtho2D" );
pragma import( C, gluPartialDisk, "gluPartialDisk" );
pragma import( C, gluPerspective, "gluPerspective" );
pragma import( C, gluPickMatrix, "gluPickMatrix" );
pragma import( C, gluProject, "gluProject" );
pragma import( C, gluPwlCurve, "gluPwlCurve" );
pragma import( C, gluQuadricCallback, "gluQuadricCallback" );
pragma import( C, gluQuadricDrawStyle, "gluQuadricDrawStyle" );
pragma import( C, gluQuadricNormals, "gluQuadricNormals" );
pragma import( C, gluQuadricOrientation, "gluQuadricOrientation" );
pragma import( C, gluQuadricTexture, "gluQuadricTexture" );
pragma import( C, gluScaleImage, "gluScaleImage" );
pragma import( C, gluSphere, "gluSphere" );
pragma import( C, gluTessBeginContour, "gluTessBeginContour" );
pragma import( C, gluTessBeginPolygon, "gluTessBeginPolygon" );
pragma import( C, gluTessCallback, "gluTessCallback" );
pragma import( C, gluTessEndContour, "gluTessEndContour" );
pragma import( C, gluTessEndPolygon, "gluTessEndPolygon" );
pragma import( C, gluTessNormal, "gluTessNormal" );
pragma import( C, gluTessProperty, "gluTessProperty" );
pragma import( C, gluTessVertex, "gluTessVertex" );
pragma import( C, gluUnProject, "gluUnProject" );
pragma import( C, gluUnProject4, "gluUnProject4" );
pragma import( C, glVertex2d, "glVertex2d" );
pragma import( C, glVertex2dv, "glVertex2dv" );
pragma import( C, glVertex2f, "glVertex2f" );
pragma import( C, glVertex2fv, "glVertex2fv" );
pragma import( C, glVertex2i, "glVertex2i" );
pragma import( C, glVertex2iv, "glVertex2iv" );
pragma import( C, glVertex2s, "glVertex2s" );
pragma import( C, glVertex2sv, "glVertex2sv" );
pragma import( C, glVertex3d, "glVertex3d" );
pragma import( C, glVertex3dv, "glVertex3dv" );
pragma import( C, glVertex3f, "glVertex3f" );
pragma import( C, glVertex3fv, "glVertex3fv" );
pragma import( C, glVertex3i, "glVertex3i" );
pragma import( C, glVertex3iv, "glVertex3iv" );
pragma import( C, glVertex3s, "glVertex3s" );
pragma import( C, glVertex3sv, "glVertex3sv" );
pragma import( C, glVertex4d, "glVertex4d" );
pragma import( C, glVertex4dv, "glVertex4dv" );
pragma import( C, glVertex4f, "glVertex4f" );
pragma import( C, glVertex4fv, "glVertex4fv" );
pragma import( C, glVertex4i, "glVertex4i" );
pragma import( C, glVertex4iv, "glVertex4iv" );
pragma import( C, glVertex4s, "glVertex4s" );
pragma import( C, glVertex4sv, "glVertex4sv" );
pragma import( C, glVertexPointer, "glVertexPointer" );
pragma import( C, glViewport, "glViewport" );

end spar_os.opengl;
