#!/usr/local/bin/spar

--              TITLE : Gather RPM files.
--           LANGUAGE : Bush (AdaScript)
--             MODULE : Gazelle Utils
--       COMPONENT OF : BuZco Software (Gazelle)
-- PARENT ORGANIZATION : BuzCo Systems
--      LATEST AUTHOR : Buz Cory
--             $RCSfile: de_RPM,v $
--            $Revision: 0.1 $
--                $Date: 2003/11/29 19:42:01 $
-- $Id: de_RPM,v 0.1 2003/11/29 19:42:01 buzCo_as_toolmakr Exp $
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- PURPOSE :
-- collect all the files from an RPM into a single package.
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- OVERVIEW:
-- Queries for an installed RPM, gets all the filenames that are part
-- of the RPM, and mirrors them inside a package with the same name.
-- syntax:
-- gather-RPM [options] <rpm-name>
-- where options are:
-- --version or -V
-- put version # on Std Output and exit success.
-- --help or -[hH]
-- print usage info and exit success.
-- -[dp]<dir>
-- directory prefix for package dir, need not exist.
--
-- The <rpm-name> can be the complete name or the base name of the
-- RPM (without version info) to be gathered. It will become the
-- package name w/ the first "-" converted to a slash. (EG:
-- gather-RPM Some_RPM, where "rpm -q Some_RPM" returns
-- "Some_RPM-1.3-5" will become ./Some_RPM/1.3-5). All paths from the
-- original RPM will be kept inside the package, so /usr/bin/junk
-- becomes Some_RPM/<version>/usr/bin/junk.
--     If invoked by root, ownerships will be set to man.man for all
-- man or doc directories and their contents, everything else will
-- become owned by bin.bin except for SUID executables, which will
-- keep their original ownerships. Otherwise, of course, everything
-- will be owned by the invoking user.
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- NOTICES, LICENSE, follow; HISTORY at end
-- --------------------------------------------------------------------
-- This software is part of the Gazelle suite of useful,
-- lightweight, fast tools, developed and distributed by BuzCo
-- Systems.
--
-- UNREGISTERED COPIES OF GAZELLE SOFTWARE COME WITH NO WARRANTY,
-- EXPRESSED OR IMPLIED, AND EXPRESSLY WITHOUT THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
-- PURPOSE.
--
-- Registered users may have additional rights, depending on the terms
-- under which they choose to register. See the file
-- GAZELLE.REGISTER in the root directory of the distribution.
--
-- IF THE COUNTRY, STATE, OR PROVINCE IN WHICH YOU ARE USING THIS
-- SOFTWARE DOES NOT ALLOW SUCH LIMITATIONS, YOU HAVE NO LEGAL RIGHT
-- TO USE AN UNREGISTERED COPY.
--
-- GAZELLE software is distributed under the GNU General Public License.
-- It may be freely copied, modified, redistributed under the terms
-- of that license.
--
-- You should have gotten a copy of that license in the file COPYING. If
-- not, a copy may be obtained by sending E-mail to BuzCo Systems or
-- by writing: the Free Software Foundation 59 Temple Place - Suite
-- 330 Boston, MA 02111-1307, USA.
-- --------------------------------------------------------------------
    procedure
de_RPM                          is
    arg_cnt                     : integer ;
    Arg                         : string ;

    Pkg_Nm      : string ;      -- actually used as a renaming for "Arg".
    Pkg_FN                      : string ;      -- for full package name
    Pkg_Vrsn                    : string ;
    Me                          : constant string
                                := command_line.command_name
                                ;

begin

-- First process arguments
    arg_cnt := command_line.argument_count ;
    if arg_cnt = 0 then
        put_line( standard_error, Me & ": no args!" ) ;
        command_line.set_exit_status( 1 ) ; return ;
        end if ;
    for i in 1 .. arg_cnt loop
        Arg := command_line.argument( i ) ;
        -- must be -h, -V or Pkg_FN name.
        if Arg = "-h" or strings.glob( "--h*", Arg )
        then
            put_line( "help needed" ) ;
            command_line.set_exit_status( 0 ) ; return ;
        elsif Arg = "-V" or strings.glob( "--[vV]*", Arg )
        then
            put_line( "version needed!" ) ;
            command_line.set_exit_status( 0 ) ; return ;
        elsif strings.glob( "-*", Arg )
        then
            put_line( standard_error, "Other option not recognized!" ) ;
            command_line.set_exit_status( 1 ) ; return ;
        else                    -- must be package name.
            if i /= arg_cnt then
                put_line( standard_error, "Too many argumentsj" ) ;
                command_line.set_exit_status( 1 ) ; return ;
                end if ;
        end if ;
        --| put_line( "file_nm given is " & Arg ) ;
        end loop ;

    Pkg_Nm := Arg ;
    -- Pkg_FN := $( rpm -q $Pkg_Nm 2>&1 )
    Pkg_FN := `set_error( standard_output ) ; rpm( "-q", Pkg_Nm ) ;` ;
        -- does not catch error output inside backquotes!
    --| put_line( "RPM said " & Pkg_FN ) ;

    if strings.glob( "*not installed", Pkg_FN ) or Pkg_FN = "" then
        put_line( standard_error, Pkg_FN ) ;
        command_line.set_exit_status( 1 ) ; return ;
    elsif strings.glob( "*", Pkg_FN ) then      -- Pkg_FN is full Pkg_FN name.
        --| put_line( "full name is " & Pkg_FN ) ;
        null ;
    end if ;

-- Next determine package path
    if Pkg_FN = Pkg_Nm then             -- full name was given on command line.
        Pkg_Vrsn := Pkg_FN ;
        for i in 1 .. 5 loop
            Pkg_Vrsn :=
                strings.delete
                    ( Pkg_Vrsn
                    , 1
                    , strings.index( Pkg_Vrsn, "-" )
                    )
                ;
            --| put_line( "Version is " & Pkg_Vrsn ) ;
            exit when strings.glob( "[0-9]*", Pkg_Vrsn ) ;
            end loop ;
        Pkg_Nm := strings.head
            ( Pkg_FN
            , strings.index( Pkg_FN, "-" & Pkg_Vrsn ) -1
            )
        ;
       --| put_line( "Pkg_Nm = " & Pkg_Nm ) ;
    else
        --| put_line( "No version given." ) ;
        Pkg_Vrsn := strings.tail
            ( Pkg_FN
            , strings.length( Pkg_Nm ) -1
            ) ;
    end if ;
    --| put_line( Pkg_Nm & "/" & Pkg_Vrsn ) ;

-- Now do the work.
    declare
        rpm_files               : constant string
                                := `rpm( "-ql", Pkg_Nm) ;` ;
        path                    : string
                                := strings.field( rpm_files, 1, ASCII.LF )
                                ;
        dest_dir                : string ;
        path_len                : constant natural
                                := strings.length( path )
                                ;
        EOL                     : constant string
            := strings.slice( rpm_files, positive( path_len ) + 1, path_len + 1 )
            ;
        file_cnt                : natural
            ;
    begin
        file_cnt := strings.count( rpm_files, EOL ) +1 ;
        for i in 1 .. file_cnt loop
            path := strings.field( rpm_files, i, ASCII.LF ) ;
            dest_dir :=  Pkg_Nm & "/" & Pkg_Vrsn ;
            mkdir( "-p", dest_dir ) ;
            --| put_line( "dest dir is " & dest_dir ) ;
            put_line
                ( Me & ": copying "
                & path
                & " -> " & dest_dir
                ) ;
            cp( "--preserve"
                , "--no-dereference", "--parents"
                , path
                , dest_dir
                ) ;
            end loop ;
    end ; -- declare

    return ;

end de_RPM ;

-- --------------------------------------------------------------------
-- NOTES, HISTORY follow:
-- ----------------------------------------------------------------
-- NOTE: The coding and comment style were chosen to provide nice
-- indent folding until the ViM syntax folding works well.
-- ----------------------------------------------------------------
-- $Log: de_RPM,v $
-- Revision 0.1  2003/11/29 19:42:01  buzCo_as_toolmakr
-- Changed args for "cp" and related vars.
--
-- Revision 0.0  2003/11/29 14:20:53  buzCo_as_toolmakr
-- First Cut
--
-- vim: ts=8 sw=4 tw=72 fdm=indent ft=ada smarttab noexpandtab

