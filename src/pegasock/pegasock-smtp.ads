-- Basic support for the Simple Mail Transport Protocol (SMTP)
-- Ken O. Burtch, April 2010
-----------------------------------------------------------------------------

package pegasock.smtp is

-- Example
--
--mysmtp : aSMTPSocket;
--
--begin
--  mysmtp := establish( to_unbounded_string( "mailserver") );
--  email( mysmtp,
--         to_unbounded_string( "mymailserver.domain" ),
--         to_unbounded_string( "foo@pegasoft.ca" ),
--         to_unbounded_string( "bar@pegasoft.ca" ),
--  	 to_unbounded_string( "Subject: test email" & ASCII.CR & ASCII.LF &
--	                     ASCII.CR & ASCII.LF &
--			     "This is a test" & ASCII.CR & ASCII.LF
--        ) );
--  close( mysmtp );

type aSMTPSocket is tagged private;

SMTP_ERROR : exception;

--  ESTABLISH
-- 
-- Open a connection to an email server.  Default port is 25.  An
-- SMTP_ERROR is raised on a bad mail server reply.  NAME_ERROR is raised
-- on a socket error.
-----------------------------------------------------------------------------

procedure establish( mysmtp : out aSMTPSocket; host : unbounded_string; port : integer := 25 );

--  EMAIL
--
-- Send an email from one address to one recipient.  From and To are
-- the sender and recipient.  Content is
-- the body of the email, including subject and date (From and To are
-- filled in using the parameters.  Usual caveats for content: CRLF for
-- end of line, include all headers followed by a blank line, no blank
-- lines with only a dot as this terminates the email.
-- No formatting (e.g. MIME) is done.  Error codes from the server raise a
-- SMTP_ERROR exception.  DATA_ERROR is raised on a socket error.
-----------------------------------------------------------------------------

procedure email( mysmtp : in out aSMTPSocket; serverDomain, from, to, content : unbounded_string );

--  CLOSE
--
-- Close a connection to an email server.  Raises SMTP_ERROR if the server
-- gives an unexpected response.  NAME_ERROR is raised on a socket error.
-----------------------------------------------------------------------------
--
procedure close( mysmtp : in out aSMTPSocket );

-- is_valid_content not yet written

-----------------------------------------------------------------------------
private
-----------------------------------------------------------------------------

type aSMTPSocket is tagged record
     socket : aBufferedSocket;
end record;

end pegasock.smtp;

