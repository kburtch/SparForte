#!/usr/local/bin/spar

pragma annotate( summary, "chomp" );
pragma annotate( description, "Program to convert one kind of text file to another" );
pragma annotate( description, "Like chomp2, but without praga ada_95" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

-- Error codes

error_syntax   : constant short_short_integer := 192;
error_no_file  : constant short_short_integer := 193;
error_internal : constant short_short_integer := 194;

-- Script Flags

type text_types is ( unknown, dos, mac, unix, broken, binary, c, strip );
-- conversions we can do

target_type : text_types := unknown;
verbose : boolean := false;

-- Files

inputfile  : file_type;
sourcepath : string;
outputfile : file_type;
targetpath : string;

-- Misc

textline   : string;
len        : natural;
ch         : character;
out_cnt    : natural := 0;

-- Usage

if $# < 3 then
   if $# > 0 then
      if $1 = "-h" or $1 = "--help" then
         put( "usage: " )
           @( source_info.file );
         put_line( " -cdmsuv sourcefile targetfile" );
         new_line;
         put_line( "  Morph text files into different formats." );
         new_line;
         put_line( "  -d convert to MS-DOS or Windows (CR+LF)" )
                @( "  -m convert to Apple or Mac (CR)" )
                @( "  -u convert to UNIX or Linux (LF)" )
                @( "  -c convert to C or C++ (NUL)" )
                @( "  -s strip CR and LF" )
                @( "  -v verbose" );
         command_line.set_exit_status( 0 );
         return;
      end if;
   end if;
   put( standard_error, source_info.file );
   put_line( standard_error, ": use -h for help" );
   command_line.set_exit_status( error_syntax );
   return;
end if;

-- Process arguments

for i in 1..$#-2 loop
   if command_line.argument(i) = "-d" then
      if target_type /= unknown then
         put( standard_error, source_info.file )
           @( standard_error, ":" )
           @( standard_error, source_info.line );
         put_line( standard_error, ": more than 1 conversion" );
         command_line.set_exit_status( error_syntax );
         return;
      end if;
      target_type := dos;
   elsif command_line.argument(i) = "-m" then
      if target_type /= unknown then
         put( standard_error, source_info.file )
           @( standard_error, ":" )
           @( standard_error, source_info.line );
         put_line( standard_error, ": more than 1 conversion" );
         command_line.set_exit_status( error_syntax );
         return;
      end if;
      target_type := mac;
   elsif command_line.argument(i) = "-u" then
      if target_type /= unknown then
         put( standard_error, source_info.file )
           @( standard_error, ":" )
           @( standard_error, source_info.line );
         put_line( standard_error, ": more than 1 conversion" );
         command_line.set_exit_status( error_syntax );
         return;
      end if;
      target_type := unix;
   elsif command_line.argument(i) = "-c" then
      if target_type /= unknown then
         put( standard_error, source_info.file )
           @( standard_error, ":" )
           @( standard_error, source_info.line );
         put_line( standard_error, ": more than 1 conversion" );
         command_line.set_exit_status( error_syntax );
         return;
      end if;
      target_type := c;
   elsif command_line.argument(i) = "-s" then
      if target_type /= unknown then
         put( standard_error, source_info.file )
           @( standard_error, ":" )
           @( standard_error, source_info.line );
         put_line( standard_error, ": more than 1 conversion" );
         command_line.set_exit_status( error_syntax );
         return;
      end if;
      target_type := strip;
   elsif command_line.argument(i) = "-v" then
      verbose;
   else
      put( standard_error, source_info.file )
        @( standard_error, ":" )
        @( standard_error, source_info.line );
      put_line( standard_error, ": unknown option: " & command_line.argument(i) );
      command_line.set_exit_status( error_syntax );
      return;
   end if;
end loop;

-- Check files

sourcepath := command_line.argument(positive(command_line.argument_count)-1);
targetpath := command_line.argument(positive(command_line.argument_count));
if not files.is_readable_file( sourcepath ) then
   put( standard_error, source_info.file )
     @( standard_error, ":" )
     @( standard_error, source_info.line );
   put_line( standard_error, ": " & sourcepath & " doesn't exist or is not readable" );
   command_line.set_exit_status( error_no_file );
   return;
end if;

-- Do it

create( outputfile, out_file, targetpath );
open( inputfile, in_file, sourcepath );
while not end_of_file( inputfile ) loop
   -- strip end-of-line terminators
   -- Note: SparForte should do this automatically.
   textline := get_line( inputfile );
   if verbose then
      put( standard_error, "<< " );
      put_line( standard_error, strings.to_escaped( textline ) );
   end if;
   len := strings.length( textline );
   ch := strings.element( textline, positive( len ) );
   if ch = ASCII.LF then
      textline := strings.delete( textline, positive(len), len );
      len := len - 1;
      ch := strings.element( textline, positive( len ) );
      if ch = ASCII.CR then
         textline := strings.delete( textline, positive( len ), len );
      end if;
  elsif ch = ASCII.CR then
      textline := strings.delete( textline, positive( len ), len );
  end if;
  -- now add the correct ones
  case target_type is
  when dos =>
       textline := @ & ASCII.CR & ASCII.LF;
  when mac =>
       textline := @ & ASCII.CR;
  when unix =>
       textline := @ & ASCII.LF;
  when c =>
       textline := @ & ASCII.NUL;
  when strip =>
       null;
  when others =>
       put( standard_error, source_info.file )
         @( standard_error, ":" )
         @( standard_error, source_info.line );
       put_line( standard_error,  ": internal error: target type in case" );

       command_line.set_exit_status( error_internal );
       return;
  end case;
  if verbose then
     put( standard_error, ">> " );
     put_line( standard_error, strings.to_escaped( textline ) );
  end if;
  len := strings.length( textline );
  out_cnt := @+len;
  put( outputfile, textline ); -- NOT put_line
end loop;
close( inputfile ) @ ( outputfile );

if verbose then
   put( standard_error, "Wrote" ) @ ( standard_error, out_cnt );
   put_line( standard_error, " chars" );
end if;
command_line.set_exit_status( 0 );

-- VIM editor formatting instructions
-- vim: ft=spar

