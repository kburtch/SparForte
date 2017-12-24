------------------------------------------------------------------------------
-- Opening and Reading script files.                                        --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2018 Free Software Foundation              --
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

with interfaces.c, ada.text_io, ada.strings.unbounded.text_io,
     Gnat.Source_Info;
use  interfaces.c, ada.text_io, ada.strings.unbounded.text_io;


package body script_io is

type aLineReadBuffer is new string(1..80);

function LineRead( lineptr : access unbounded_string ) return boolean is
-- read a line from the current script file.  return false on eof
  buffer     : aLineReadBuffer;
  bufpos     : positive;
  amountRead : size_t;
  ch         : character := ASCII.NUL;
begin
   buffer(1) := ' '; -- suppress GNAT warning about buffer having no value
<<next>> lineptr.all := null_unbounded_string;
  scriptLineStart := lseek( scriptFile, 0, 1 );
-- strictly speaking, the start of the current block should not reference
-- an entire line, but this will do for now.  It should really be the
-- start of the current token!
   bufpos := buffer'first;
   loop
<<reread>> readchar( amountRead, scriptFile, ch, 1 );
 -- KB: 2012/02/15: see spar_os-tty for an explaination of this kludge
     if (amountRead < 0 or amountRead = size_t'last)
         and (C_errno = EAGAIN or C_errno = EINTR) then
        goto reread;
     end if;
     exit when amountRead /= 1 or ch = ASCII.LF;
     if ch = ASCII.CR then -- ignore carriage returns (not Mac friendly)
        goto reread;
     end if;
     if bufpos > buffer'last then
        lineptr.all := lineptr.all & string( buffer );
        bufpos := 1;
     end if;
     buffer( bufpos ) := ch;
     bufpos := bufpos + 1;
  end loop;
  if bufpos > 1 then
     lineptr.all := lineptr.all & string( buffer(1..bufpos-1 ) );
  end if;
  if amountRead < 0 then
     put_line( standard_error, Gnat.Source_Info.Source_Location & ": error reading script file: errno " & C_errno'img );
     return false;
  elsif amountRead = 0 and length( lineptr.all ) = 0 then
     return false;
  else
     return true;
  end if;
end LineRead;

end script_io;
