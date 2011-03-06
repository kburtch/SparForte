#!/usr/local/bin/bush

-------------------------------------------------------------------------------
-- CONVERT                                                                   --
--                                                                           --
-- Descripton: Convert CSV comma separated value files to Postgres friendly  --
-- CSV files that can be imported using Postgres' COPY command               --
--                                                                           --
-- Written by Ken O. Burtch <ken@pegasoft.ca>                                --
-------------------------------------------------------------------------------

procedure postgres_csv is
  convert_source : string;
  fieldpos   : natural;
  firstpos   : natural;
  quote_flag : boolean;
  another_field_expected : boolean; -- to handle trailing ,
  field      : string;
  s          : string;
  converted_field : string;
  f          : file_type;
  ch         : character;
  first_comma : boolean;
  record_count : natural := 0;
begin
  -- Usage
  if command_line.argument_count = 0 then
     put_line( standard_error, "Convert normal CSV to Postgres CSV file" );
     put( standard_error, "usage: " );
     put( standard_error, command_line.command_name );
     put( standard_error, " original.csv > postgres.csv" );
     new_line;
     return;
  end if;
  convert_source := command_line.argument(1);
  open( f, in_file, convert_source );

  loop
    s := get_line( f );
    exit when end_of_file( f );

    -- Read a field

    put( strings.image( record_count+1 ) & ',' );
--    put( "," );

    fieldpos := 1;
    first_comma := true;

    while fieldpos <= strings.length( s ) loop
      quote_flag := false;
      firstpos := fieldpos;
      another_field_expected := false;

      while fieldpos <= strings.length( s ) loop
        ch := strings.element( s, positive(fieldpos) );
        if ch = ASCII.Quotation then
           quote_flag := not quote_flag;
        elsif ch = ',' and not quote_flag then
           another_field_expected := true;
           exit;
        end if;
        fieldpos := @+1;
      end loop;

      field := strings.slice( s, positive(firstpos), fieldpos-1 );
      if strings.length( field ) > 0 then
         if strings.element( field, 1 ) = ASCII.Quotation and
            strings.element( field, positive(strings.length( field )) )
             = ASCII.Quotation then
               field := strings.slice( field, 2, strings.length(field)-1 );
         end if;
      end if;
      -- Backslash escapes for Postgres' COPY
      converted_field := "";

      for i in 1..strings.length( field ) loop
          ch := strings.element( field, i );
          if ch = ',' then
             converted_field := @ & "\054"; -- comma
          elsif ch = '\' then
             converted_field := @ & "\134"; -- backslash
          elsif ch = ASCII.BS then
             converted_field := @ & "\b"; -- backspace
          elsif ch = ASCII.HT then
             converted_field := @ & "\t"; -- tab
          elsif ch = ASCII.FF then
             converted_field := @ & "\f"; -- form feed
          elsif ch = ASCII.LF then
             converted_field := @ & "\n"; -- line feed
          elsif ch = ASCII.VT then
             converted_field := @ & "\v"; -- vertical tab
          elsif ch = ASCII.CR then
             converted_field := @ & "\r"; -- carriage return
          elsif ch < ' ' or ch > '~' then
             converted_field := converted_field & "[bad ?]";
          else
             converted_field := @ & ch;
          end if;
      end loop;

      if first_comma then
         first_comma := false;
      else
         put( "," );
      end if;
      put( converted_field );
      fieldpos := @+1;
    end loop;

    if another_field_expected then
       put( "," );
    end if;
    new_line;
    record_count := @+1;
    if record_count mod 20 = 0 then
       put( standard_error, "." );
    end if;
    if record_count mod 1000 = 0 then
       put_line( standard_error, strings.image( record_count) );
    end if;
  end loop;

  close( f );
  new_line( standard_error );
  put_line( standard_error, "Converted" & strings.image( record_count ) &
    " rows" );
-- $Log: postgres_csv.bush,v $
-- Revision 1.2  2005/08/23 12:27:13  ken
-- Bush 1.0.2 release
--
-- Revision 1.1.1.1  2003/10/16 03:08:34  ken
-- imported by TIA
--
end postgres_csv;

