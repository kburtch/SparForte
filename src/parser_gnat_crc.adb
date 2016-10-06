------------------------------------------------------------------------------
-- BUSH GNAT CRC Package Parser                                             --
--                                                                          --
-- Part of BUSH                                                             --
------------------------------------------------------------------------------
--                                                                          --
--              Copyright (C) 2001-2016 Ken O. Burtch & FSF                 --
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
-- CVS: $Id$

with ada.text_io; use ada.text_io;

with interfaces,
    gnat.crc32,
    world,
    scanner,
    parser,
    parser_aux,
    parser_params;
use interfaces,
    world,
    scanner,
    parser,
    parser_aux,
    parser_params;

package body parser_gnat_crc is

------------------------------------------------------------------------------
-- CGI package identifiers
------------------------------------------------------------------------------

gnat_crc32_crc32_t : identifier;

gnat_crc32_initialize_t : identifier;
gnat_crc32_update_t : identifier;
gnat_crc32_get_value_t : identifier;

------------------------------------------------------------------------------
-- PARSE THE CGI PACKAGE
------------------------------------------------------------------------------

procedure ParseGnatCRC32Initialize is
  -- gnat.crc32.initialize( crc32 )
  record_ref : reference;
  record_type : identifier := gnat_crc32_crc32_t;
begin
  expect( gnat_crc32_initialize_t );
  expect( symbol_t, "(" );
  ParseOutParameter( record_ref, record_type );
  if baseTypesOk( record_type, gnat_crc32_crc32_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     declare
       c : Gnat.CRC32.CRC32;
     begin
       Gnat.CRC32.Initialize( C );
       --identifiers( record_ref.id ).value := to_unbounded_string( long_float( 16#FFFF_FFFF# XOR Gnat.CRC32.Get_Value( C ) ) );
       identifiers( record_ref.id ).value.all := to_unbounded_string( long_float( C ) );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseGnatCRC32Initialize;

procedure ParseGnatCRC32Update is
  -- gnat.crc32.update( crc32 )
  var_id  : identifier;
  expr_val  : unbounded_string;
  expr_type : identifier;
begin
  expect( gnat_crc32_update_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if baseTypesOk( identifiers( var_id ).kind, gnat_crc32_crc32_t ) then
     expect( symbol_t, "," );
     ParseExpression( expr_val, expr_type );
     --if uniTypesOk( identifiers( expr_type ).kind, uni_string_t ) then
     if uniTypesOk( expr_type, uni_string_t ) then
        expect( symbol_t, ")" );
     end if;
  end if;

  if isExecutingCommand then
     declare
       c : Gnat.CRC32.CRC32 := Gnat.CRC32.CRC32'value( to_string(
           identifiers( var_id ).value.all ) );
     begin
       Gnat.CRC32.Update( C, to_string( expr_val ) );
       --identifiers( var_id ).value := to_unbounded_string( long_float( 16#FFFF_FFFF# XOR Gnat.CRC32.Get_Value( C ) ) );
       identifiers( var_id ).value.all := to_unbounded_string( long_float( C ) );
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseGnatCRC32Update;

procedure ParseGnatCRC32GetValue( result : out unbounded_string; kind : out identifier ) is
  -- integer := gnat.crc32.update( crc32 )  -- really, unsigned 32
  var_id  : identifier;
begin
  kind := integer_t;
  expect( gnat_crc32_get_value_t );
  expect( symbol_t, "(" );
  ParseIdentifier( var_id );
  if baseTypesOk( identifiers( var_id ).kind, gnat_crc32_crc32_t ) then
     expect( symbol_t, ")" );
  end if;

  if isExecutingCommand then
     begin
       result := identifiers( var_id ).value.all;
     exception when others =>
       err_exception_raised;
     end;
  end if;
end ParseGnatCRC32GetValue;


------------------------------------------------------------------------------
-- HOUSEKEEPING
------------------------------------------------------------------------------

procedure StartupGnatCRC is
begin
  declareNamespace( "gnat.crc32" );
  declareIdent( gnat_crc32_crc32_t, "gnat.crc32.crc32", uni_numeric_t, typeClass );
  declareProcedure( gnat_crc32_initialize_t, "gnat.crc32.initialize", ParseGnatCRC32Initialize'access );
  declareProcedure( gnat_crc32_update_t, "gnat.crc32.update", ParseGnatCRC32Update'access );
  declareFunction( gnat_crc32_get_value_t, "gnat.crc32.get_value",ParseGnatCRC32GetValue'access );
  declareNamespaceClosed( "gnat.crc32" );
end StartupGnatCRC;

procedure ShutdownGnatCRC is
begin
  null;
end ShutdownGnatCRC;

end parser_gnat_crc;
