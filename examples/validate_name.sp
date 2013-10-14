#!/usr/local/bin/spar

pragma annotate( summary, "validate_name string" );
pragma annotate( description, "Check a name for suspicious characters" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure validate_name is

procedure usage is
begin
  put( "usage: " )
   @( source_info.file );
  put_line( " string" );
  new_line;
  put_line( "  Check a name for suspicious characters." );
  new_line;
  command_line.set_exit_status( 0 );
end usage;

begin

-- Usage

if $# /= 1 then
   usage;
   return;
elsif $1 = "-h" or $1 = "--help" then
   usage;
   return;
end if;

-- Name tests

declare
  name : string := $1;
  ch   : character;
  lastWasComma  : boolean := false;
  lastWasPeriod : boolean := false;
  lastWasMinus  : boolean := false;
  lastWasApostrophe : boolean := false;
begin
  command_line.set_exit_status( 192 );
  name := strings.trim( name, trim_end.both );
  if name = "" then
     put( standard_error, source_info.file )
         @( standard_error, ": name is missing" );
     new_line;
  elsif strings.length( name ) > 63 then
     put( standard_error, source_info.file )
         @( standard_error, ": name is unusually long" );
     new_line;
  elsif strings.length( name ) < 5 then
     put( standard_error, source_info.file )
         @( standard_error, ": name is unusually short" );
     new_line;
  end if;

  -- Name should start with a letter

  ch := strings.element( name, 1 );
  if not strings.is_letter( ch ) then
     put( standard_error, source_info.file )
         @( standard_error, ": name starts with an unusual letter - " )
         @( standard_error, ch );
     new_line;
  end if;

  -- Name should contain letters, commas or periods

  for i in 1..strings.length( name )-1 loop
      ch := strings.element( name, i );
      if ch = ' ' then
         null;
         lastWasComma := false;
         lastWasPeriod := false;
         lastWasMinus := false;
         lastWasApostrophe := false;
      elsif ch = '0' then
         put( standard_error, source_info.file )
             @( standard_error, ": zero in name should probably be a O" );
         new_line;
      elsif ch = '1' then
         put( standard_error, source_info.file )
             @( standard_error, ": one in name should probably be a l or i" );
         new_line;
      elsif ch = '<' then
         put( standard_error, source_info.file )
             @( standard_error, ": < in name should probably be a comma" );
         new_line;
      elsif ch = '>' then
         put( standard_error, source_info.file )
             @( standard_error, ": > in name should probably be a period" );
         new_line;
      elsif ch = '_' then
         put( standard_error, source_info.file )
             @( standard_error, ": _ in name should probably be a dash" );
         new_line;
      elsif ch = '"' then
         put( standard_error, source_info.file )
             @( standard_error, ": double quote in name should probably be an apostraphe" );
         new_line;
      elsif strings.is_letter( ch ) then
         lastWasComma := false;
         lastWasPeriod := false;
         lastWasMinus := false;
         lastWasApostrophe := false;
      elsif ch = "," then
         lastWasPeriod := false;
         lastWasMinus := false;
         lastWasApostrophe := false;
         if lastWasComma then
            put( standard_error, source_info.file )
                @( standard_error, ": name contains a repeated comma" );
            new_line;
         else
            lastWasComma;
         end if;
      elsif ch = '-' then
         lastWasComma := false;
         lastWasPeriod := false;
         lastWasApostrophe := false;
         if lastWasMinus then
            put( standard_error, source_info.file )
                @( standard_error, ": name contains a repeated dash" );
            new_line;
         else
            lastWasMinus;
         end if;
      elsif ch = "'" then
         lastWasComma := false;
         lastWasMinus := false;
         lastWasPeriod := false;
         if lastWasApostrophe then
            put( standard_error, source_info.file )
                @( standard_error, ": name contains a repeated apostraphe" );
            new_line;
         else
            lastWasApostrophe;
         end if;
      elsif ch = "." then
         lastWasComma := false;
         lastWasMinus := false;
         lastWasApostrophe := false;
         if lastWasPeriod then
            put( standard_error, source_info.file )
                @( standard_error, ": name contains a repeated period" );
            new_line;
         else
            lastWasPeriod;
         end if;
      else
         lastWasComma := false;
         lastWasPeriod := false;
         lastWasMinus := false;
         lastWasApostrophe := false;
         put( standard_error, source_info.file )
             @( standard_error, ": name contains an unusual letter - " )
             @( standard_error, ch );
         new_line;
     end if;
  end loop;

  -- Name should end with a letter

  ch := strings.element( name, positive( strings.length( name ) ) );
  if not strings.is_letter( ch ) then
     put( standard_error, source_info.file )
         @( standard_error, ": name ends with an unusual letter - " )
         @( standard_error, ch );
     new_line;
  end if;
end;

  command_line.set_exit_status( 0 );
end validate_name;

-- VIM editor formatting instructions
-- vim: ft=spar

