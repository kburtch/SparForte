#!/usr/local/bin/spar

pragma annotate( summary, "aligncols" )
              @( description, "Given a text file of many lines, where fields within a line are delineated ")
              @( description, "by a single 'dollar' character, write a program that aligns each column of" )
              @( description, "fields by ensuring that words in each column are separated by at least one" )
              @( description, "space. Further, allow for each word in a column to be either left justified," )
              @( description, "right justified, or center justified within its column. " )
              @( description, "A modified version of the Ada solution from Rosetta Code" )
              @( category, "tutorials" )
              @( author, "Ken O. Burtch" )
              @( see_also, "http://rosettacode.org/wiki/Align_columns" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure aligncols is
   Text : constant string :=
      "Given$a$text$file$of$many$lines,$where$fields$within$a$line$" & ASCII.NUL &
      "are$delineated$by$a$single$'dollar'$character,$write$a$program" & ASCII.NUL &
      "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$" & ASCII.NUL &
      "column$are$separated$by$at$least$one$space." & ASCII.NUL &
      "Further,$allow$for$each$word$in$a$column$to$be$either$left$" & ASCII.NUL &
      "justified,$right$justified,$or$center$justified$within$its$column." & ASCII.NUL;
   File : file_type;
   Width : array(1..1000) of natural;
   ch : character;
   Column : positive := 1;
   Start : positive := 1;
   type Alignment is ( Left, Center, Right );
   s : string;
   padding : natural;
begin
   -- Zero Widths
   for I in arrays.first( Width )..arrays.last( Width ) loop
       Width(I) := 0;
   end loop;
   -- Determining the widths of columns
   for I in 1..strings.length(Text) loop
      ch := strings.element( Text, I );
      case ch is
         when '$' | ASCII.NUL =>
            Width (Column) := numerics.max(Width (Column), I - Start + 1);
            Start  := I + 1;
            if strings.element( Text, I ) = ASCII.NUL then
               Column := 1;
            else
               Column := @+1;
            end if;
         when others =>
            null;
      end case;
   end loop;
   create( File, out_file, "columned.txt" );
   -- Formatting
   for Align in Left..Right loop
       Column := 1;
       Start := 1;
       for I in 1..strings.length(Text) loop
          ch := strings.element( Text, I );
          case ch is
             when '$' | ASCII.NUL =>
                s := strings.slice( Text, Start, I-1 );
                padding := (Width( Column ) - strings.length(s));
                case Align is
                when Left =>
                  s := @ & (padding * ' ');
                when Center =>
                  declare
                     left_padding : constant natural := padding/2;
                     right_padding : constant natural := padding - left_padding;
                  begin
                     s := (left_padding * ' ') & @ & (right_padding * ' ');
                  end;
                when Right =>
                  s := (padding * ' ') & @;
                when others =>
                    null;
                end case;
                put( File, s );
                Start := I+1;
                if ch = ASCII.NUL then
                   new_line( File );
                   Column := 1;
                else
                   Column := @+1;
                end if;
             when others =>
                null;
          end case;
       end loop;
       new_line( File );
   end loop;
   close( File );
end aligncols;


-- VIM editor formatting instructions
-- vim: ft=spar

