------------------------------------------------------------------------------
-- Built-in Shell Commands                                                  --
--                                                                          --
-- Part of SparForte                                                        --
------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2001-2020 Free Software Foundation              --
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

with ada.strings.unbounded,
     world,
     scanner,
     spar_os.exec;
use  ada.strings.unbounded,
     world,
     scanner,
     spar_os,
     spar_os.exec;

package builtins is

-- PWD
-- loosely modelled on bash/builtins/cd.def

current_working_directory : unbounded_string := null_unbounded_string;
-- NULL if not known

procedure findpwd;
-- run this on startup to initialize current_working_directory

procedure alter( ap : argumentListPtr );
-- alter : SQL alter

procedure old_cd( s : unbounded_string );
-- cd: change directory

procedure cd( ap : argumentListPtr );
-- cd: change directory

procedure clear( ap : argumentListPtr );
-- clear : clear the screen

procedure delete( ap : argumentListPtr );
-- delete : SQL delete

--procedure env( id : identifier := eof_t );
-- env - list environment to standard output

-- help is in a separate package

procedure do_history( ap : argumentListPtr );
-- history: shell history control

procedure insert( ap : argumentListPtr );
-- insert : SQL insert

procedure jobs( ap : argumentListPtr );
-- jobs: list running jobs

procedure pwd( ap : argumentListPtr );
-- pwd: present working directory (also updated current_working_directory)

procedure SQLselect( ap : argumentListPtr );
-- SQLselect : SQL select

procedure step( ap : argumentListPtr );
-- step: debugger step

procedure do_trace( ap : argumentListPtr );
-- trace: command tracing

procedure unset( ap : argumentListPtr );
-- unset: remove an identifier

procedure env( ap : argumentListPtr );
-- env: command update

procedure update( ap : argumentListPtr );
-- update: command update

procedure wait( ap : argumentListPtr );
-- wait: wait for background jobs

procedure vm( regtype, regnum : unbounded_string );
-- vm - show virtual machine status

end builtins;

