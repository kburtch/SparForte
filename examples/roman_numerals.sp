#!/usr/local/bin/bush

procedure Roman is

  pragma annotate( "Roman Numerals" );
  pragma annotate( "" );
  pragma annotate( "Roman numeral arithmetic adapted from an Ada to SparForte" );
  pragma annotate( "adapted by Ken O. Burtch" );
  pragma ada_95;
  pragma restriction( no_external_commands );

  -- Declare variables which will be used in main program out here

  MaxRomanLength : constant positive := 20;
  subtype RomanNumeralType is string;

--------------------------------------------------------------------------------
  
   -- Function for checking Valid Input from the user
   -- This function returns a boolean value

   function ValidRomanNumeral (PossibleNumeral : RomanNumeralType) return
       boolean is

         Position : positive := 1; -- position we are checking in the input

   begin -- ValidRomanNumeral
      if strings.length( PossibleNumeral ) > natural( MaxRomanLength ) then
         return false;
      end if;
      if strings.length( PossibleNumeral ) = 0 then
         return false;
      end if;
      loop
         if Position = MaxRomanLength then
            return true;   -- we have reached the maximum length without problems
         elsif Position > positive( strings.length( PossibleNumeral ) ) then
            return true;   -- we have reached the actual end without problems
         elsif strings.element( PossibleNumeral, Position) = 'I'
            or strings.element( PossibleNumeral, Position) = 'V'
            or strings.element( PossibleNumeral, Position) = 'X'
            or strings.element( PossibleNumeral, Position) = 'L'
            or strings.element( PossibleNumeral, Position) = 'C'
            or strings.element( PossibleNumeral, Position) = 'D'
            or strings.element( PossibleNumeral, Position) = 'M' then
               Position := Position + 1;
         else
            return false;  -- we have a problem, so return false
         end if;
      end loop;
      -- This will never happen
      return false;
   end ValidRomanNumeral;
   
--------------------------------------------------------------------------------

   function RomanToNatural (RomanNumeral : RomanNumeralType) return natural is
      Result : natural := 0;  -- result variable
      CurrentPosition : positive := 1; -- the current position in RomanNumeral being examined.
      Value : integer:=0; -- Value of the corresponding roman numeral
      First  : integer:=0;
      Next : integer:=0;

     
   begin --RomanToNatural
      
      -- Loop to process the appropriate value from the user
      loop

         if CurrentPosition > positive( strings.length( RomanNumeral ) ) then
            exit;
         elsif strings.element( RomanNumeral, CurrentPosition) = 'M' then
            Value := 1000;      
         elsif strings.element( RomanNumeral, CurrentPosition) = 'D' then
            Value := 500;
         elsif strings.element( RomanNumeral, CurrentPosition) = 'C' then
            Value := 100;
         elsif strings.element( RomanNumeral, CurrentPosition) = 'L' then    
            Value := 50;
         elsif strings.element( RomanNumeral, CurrentPosition) = 'X' then
            Value := 10;
         elsif strings.element( RomanNumeral, CurrentPosition) = 'V' then
            Value := 5;
         elsif strings.element( RomanNumeral, CurrentPosition) = 'I' then
            Value := 1;
         end if;
                       
         -- If this is the first character then store the value
         -- of it in the variable 'First'

         if CurrentPosition = 1 then
            First := Value;        
         end if;   
         
         Next := Value; -- Store the second value in this variable(2nd iteration)
         
         -- This is for the second Iteration. Check if the second value
         -- is greater than the first value and perform the nescassary
         -- steps. 
         if First < Next then
            Result := natural(Next - First) +Result - natural(First);
         else
            Result := Result + natural(Next);
      end if;   
      
     -- Assign the value of first to next
     -- for comparison in later cases 
     First := Next;      
      
     CurrentPosition := CurrentPosition + 1; -- Increment the counter Variable

     --exit when CurrentPosition = (LEN + 1); -- Exit the loop after the last character is read
      
    
    end loop; -- End the loop here  
    
    return Result; -- Return the final result to where function is being called
    
   end RomanToNatural; -- End the function

--------------------------------------------------------------------------------

   -- This Function Gets a valid choice from the user
   -- The user can chooose to perform arithmetic operations on the 
   -- Roman numerals. The choices are 1 through 4. 
   -- Will keep looping till the user gives correct Choice

   function GetValidChoice return integer is
    ValidChoice : integer;
    Choice : integer:=1; 
       
    begin
       
      loop
         new_line;
         put("Please choose one of the following operations to be performed:");   
         new_line;
         put("1) Add the two Roman Numerals.");
         new_line;
         put("2) Subtract the two Roman Numerals.");
         new_line;
         put("3) Multiply the two Roman Numerals.");
         new_line;
         put("4) Divide the two Roman Numerals.");
         new_line;
         put("----------------------------------------------------");
         new_line;
         put("Please enter choice: ");
         Choice := numerics.value( get_line );         
         if (Choice>0) or (Choice<=4) then
            exit;
         else 
            new_line;         
            put("Please enter Valid Choice.");   
            new_line;
         end if;      
      
      end loop;
      
    ValidChoice := Choice;
    return ValidChoice;
     
   end GetValidChoice;

--------------------------------------------------------------------------------
   
   -- This procedure process the valid choice and performs the
   -- nescassary calculations on the Two user inputs
   function ProcessChoice(UserInput1 : natural;
                           UserInput2 : natural;
                           Choice     : integer ) return natural is
     Output : natural;
-- Write this procedure in the morning
-- what this essentially does is 
-- it takes in userinput1 and userinput2 and Choice as IN Variables
-- According to the user's specified choice we can then compute the possible
-- values and return Output
-- This value of output will the in turn be stored in a variable called 
-- FinalInteger
      
      -- declare variables here
      
      Plus     : natural:=0;
      Minus    : natural:=0;
      Multiply : natural:=0;
      Divide   : natural:=0;
      
      
      begin -- ProcessChoice
      
      -- Start checking for user choice and perform nescassary action
            
      if (Choice = 1) then
      
         Plus := UserInput1 + UserInput2;
         Output := Plus;
         
      elsif (Choice = 2) then
         
         Minus := UserInput1 - UserInput2;
         Output := Minus;
         
      elsif (Choice = 3) then
         
         Multiply := UserInput1 * UserInput2;
         Output := Multiply;
         
      elsif (Choice = 4) then
         
         Divide := UserInput1 / UserInput2;
         Output := Divide;
      else
         put("SOMETHING HAS GONE WRONG");       
         
      end if;   
 
      return Output;
   end ProcessChoice;

-------------------------------------------------------------------------

   procedure ConvertBack(ConvertWhat : natural) is
         
     Value : natural;
     
    begin  
        
     Value := ConvertWhat;
     for I in 1..20 loop
        if Value<=4000 and Value>=1 then
        
               while Value>=1000 loop
               
                  put( "M" );             
                  Value:=Value - 1000;
                  
               end loop;
               
               while Value>=500 loop
               
                  put( "D" );
                  Value:=Value - 500;  
               end loop;
               
               while Value>=100 loop
                  put( "C" );
                  Value:=Value - 100;
               end loop;
               
               while Value>=50 loop
                  put( "L" );
                  Value:=Value - 50;
               end loop;
               
               while Value>=10 loop
                  put( "X" );
                  Value:=Value - 10;
               end loop;
               
               while Value>=5 loop
                  put( "V" );
                  Value:=Value - 5;
               end loop;
               
               while Value>=1 loop
                  put( "I" );
                  Value:=Value - 1;
               end loop;                                 
               
            end if;
         end loop;
         new_line;
   end ConvertBack;

--------------------------------------------------------------------------------

  RomanNumeral1 : RomanNumeralType;
  RomanNumeral2 : RomanNumeralType;
  
  FinalInteger : natural:=0;

  Input1 : natural:=0;
  Input2 : natural:=0;
 
  UserChoice : integer:=0;

begin -- Procedure Roman
  
  put("Welcome to the Roman Numeral Calculator program:");
  new_line;
  new_line;
  
  -- Get the valid roman numeral from the user
  -- This function returns two integer values in RomanInteger1 and RomanInteger2
  -- These values are the corresponding integer values of the entered roman numerals 

  loop 
     put( "Please enter the FIRST Roman numeral: ");
     RomanNumeral1 := get_line;

     put( "Please enter the SECOND Roman numeral: ");
     RomanNumeral2 := get_line;

     if ValidRomanNumeral(RomanNumeral1) then
        if ValidRomanNumeral(RomanNumeral2) then
           exit;
        end if;
     end if;      

    put( "Please make sure BOTH the Roman Numerals are valid and try again");
    new_line;
  end loop;

  Input1 := RomanToNatural( RomanNumeral1 );
  Input2 := RomanToNatural( RomanNumeral2 );

  UserChoice := GetValidChoice;

  FinalInteger := ProcessChoice(Input1,Input2,UserChoice);

  put( "Final Result is: " );

  ConvertBack(FinalInteger);   

end Roman; 

-- VIM editor formatting instructions -- vim: ft=bush

