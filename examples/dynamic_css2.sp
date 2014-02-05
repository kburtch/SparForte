#!/usr/local/bin/spar

procedure dynamic_css2 is

   pragma annotate( summary, "dynamic_css" )
                 @( description, "A simple example of dynamically generating CSS." )
                 @( description, "One way to run this script is to include it in" )
                 @( description, "a link tag." )
                 @( author, "Ken O. Burtch" );
   pragma license( public_domain );

   -- Example usage:
   --
   -- <html>
   -- <head>
   -- <link rel="stylesheet" href="cgi-bin/dynamic_css.sp">
   -- </head>
   -- <body>
   -- <p>This is a test</p>
   -- </body>
   -- </html>

   pragma template( css );
   pragma restriction( no_external_commands );

   -- CSS Substitution Variables

   color       : constant string := "#FFFFFF";
   normal_font : constant string := "12px arial";

begin
   null;
end dynamic_css2;

-- vim: ft=spar


