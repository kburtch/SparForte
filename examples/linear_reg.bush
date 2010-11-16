#!/usr/local/bin/bush

-- A simple linear regression script

pragma annotate( "linear reg" );
pragma annotate( "" );
pragma annotate( "Based on an Ada program by Dr. David G. Simpson, 2002" );
pragma annotate( "" );
pragma annotate( "This program performs a linear regression analysis for a set of data given" );
pragma annotate( "as (x,y) pairs.  The output from the program is the slope and y-intercept" );
pragma annotate( "of the least-squares best fit straight line through the data points." );
-----------------------------------------------------------------------------

pragma ada_95;                     -- enforce ada_95 restrictions

procedure linear_reg is

--  Variable declarations

   b     : float;                  -- y-intercpt of least-squares best fit line
   m     : float;                  -- slope of least-squares best fit line
   n     : float := 0.0;           -- number of data points
   r     : float;                  -- squared correlation coefficient
   sumx  : float := 0.0;           -- sum of x
   sumx2 : float := 0.0;           -- sum of x**2
   sumxy : float := 0.0;           -- sum of x * y
   sumy  : float := 0.0;           -- sum of y
   sumy2 : float := 0.0;           -- sum of y**2
   x     : float;                  -- input x data
   y     : float;                  -- input y data


--  Main program code

begin

   -- print introductory message

   put_line ("LINREG - Perform linear regression");
   put_line (" (Enter X=-9999 to stop data entry and compute linear regression.)");

   loop                           -- loop for all data points
      new_line;
      put ("Enter x:  ");         -- prompt for x
      x := numerics.value( get_line ); -- read x
      if x = -9999.0 then         -- if no more data..
         exit;                    -- ..then exit loop
      end if;                     -- else prompt for y
      put ("Enter y:  ");         -- prompt for y
      y := numerics.value( get_line ); -- read y

      n := n + 1.0;               -- increment number of data points by 1
      sumx  := sumx + x;          -- compute sum of x
      sumx2 := sumx2 + x * x;     -- compute sum of x**2
      sumxy := sumxy + x * y;     -- compute sum of x * y
      sumy  := sumy + y;          -- compute sum of y
      sumy2 := sumy2 + y * y;     -- compute sum of y**2
   end loop;

   m := (n * sumxy  -  sumx * sumy) /
     (n * sumx2 - sumx**2);                  -- compute slope
   b := (sumy * sumx2  -  sumx * sumxy) /
     (n * sumx2  -  sumx**2);                -- compute y-intercept
   r := (sumxy - sumx * sumy / n) /          -- compute correlation coefficient
            numerics.sqrt((sumx2 - sumx**2/n) * (sumy2 - sumy**2/n));

   new_line;

   put ("Slope        m = ");                -- print results: slope
   put (m ); --, Fore => 2, Aft => 6, Exp => 3);

   new_line;

   put ("y-intercept  b = ");                -- print y-intercept
   put (b); --, Fore => 2, Aft => 6, Exp => 3);

   new_line;

   put ("Correlation  r = ");                -- print correlation coefficient
   put (r) ; --, Fore => 2, Aft => 6, Exp => 3);

   new_line;

   command_line.set_exit_status( 0 );        -- success
end linear_reg;

