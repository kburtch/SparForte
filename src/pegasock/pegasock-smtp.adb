
with ada.text_io;
use  ada.text_io;
with Ada.Calendar,
     Gnat.Source_Info,
     Gnat.Calendar.Time_IO;
use  Ada.Calendar,
     Gnat.Calendar.Time_IO;
package body pegasock.smtp is

--  ESTABLISH
--
--  Open a connection to an email server.  Default port is 25.  An
--  SMTP_ERROR is raised on a bad mail server reply.
-----------------------------------------------------------------------------

procedure establish( mysmtp : out aSMTPSocket; host : unbounded_string; port : integer := 25 ) is
  status : integer;
  s : unbounded_string;
begin
  establish( mysmtp.socket, host, port );
  setEOL( mysmtp.socket, CRLF );
  get( mysmtp.socket, s );
  status := integer'value( slice(s, 1, 3 ) );
  if status /= 220 then
     raise SMTP_ERROR with to_string( s );
  end if;
end establish;


--  EMAIL
--
--  Send an email from one address to one recipient.  From and To are
--  the sender and recipient.  Content is
--  the body of the email, including subject and date (From and To are
--  filled in using the parameters.  Usual caveats for content: CRLF for
--  end of line, include all headers followed by a blank line, no blank
--  lines with only a dot as this terminates the email.
--  No formatting (e.g. MIME) is done.  Error codes from the server raise a
--  SMTP_ERROR exception.
-----------------------------------------------------------------------------

procedure email( mysmtp : in out aSMTPSocket; serverDomain, from, to, content : unbounded_string ) is
  status : integer;
  s : unbounded_string;
  sanitized_content : unbounded_string := content;
begin
  if not isOpen( mysmtp.socket ) then
     return;
  end if;
  put_line( mysmtp.socket, "HELO " & serverDomain );

  get( mysmtp.socket, s );
  status := integer'value( slice(s, 1, 3 ) );

  if status = 250 then
     put_line( mysmtp.socket, "MAIL FROM: " & from );
     get( mysmtp.socket, s );
     status := integer'value( slice(s, 1, 3 ) );

     if status = 250 then
	-- NOTE: multiple RCPT's are for multiple recipients but are not
	-- supported here.
        put_line( mysmtp.socket, "RCPT TO: " & to );
        get( mysmtp.socket, s );
        status := integer'value( slice(s, 1, 3 ) );
        if status = 250 then
           put_line( mysmtp.socket, "DATA" );
           get( mysmtp.socket, s );
           status := integer'value( slice(s, 1, 3 ) );
           if status = 354 then
              put_line( mysmtp.socket, "From: " & from );
              put_line( mysmtp.socket, "To: " & to );
              -- TODO: timezone handling
              put_line( mysmtp.socket, "Date: " & Image(Clock, "%a, %d %b %Y %T -0500") );
              declare
                 -- sanitize input: "." at the start of lines must be
                 -- escaped
                 start_of_line : boolean := false;
                 i  : positive := 1;
                 ch : character;
              begin
                 while i < length( sanitized_content ) loop
                     ch := element( sanitized_content, i );
                     if start_of_line and ch = '.' then
                        insert( sanitized_content, i, "." );
                     end if;
                     if ch = ASCII.LF then
                        start_of_line := true;
                     else
                        start_of_line := false;
                     end if;
                 end loop;
              end;
              put_line( mysmtp.socket, sanitized_content );
              put_line( mysmtp.socket, "." );

              get( mysmtp.socket, s );
              status := integer'value( slice(s, 1, 3 ) );
              if status = 250 then
                 null; -- success
              end if;
           end if;
        end if;
     end if;
  end if;
  if status /= 250 then
    pragma debug( put_line( standard_error, Gnat.Source_Info.Source_Location &
    ": Error: mail server says '" & to_string( s ) & "'" ) );
     raise SMTP_ERROR with to_string( s );
  end if;
end email;


--  CLOSE
--
--  Close a connection to an email server.  Raises SMTP_ERROR if the server
--  gives an unexpected response.
-----------------------------------------------------------------------------

procedure close( mysmtp : in out aSMTPSocket ) is
  s : unbounded_string;
  status : integer;
begin
  put_line( mysmtp.socket, "QUIT" );
  get( mysmtp.socket, s );
  status := integer'value( slice(s, 1, 3 ) );
  if status /= 221 then
     raise SMTP_ERROR with to_string( s );
  end if;
  close ( mysmtp.socket );
end close;

end pegasock.smtp;

