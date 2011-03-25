-- Basic support for the Syslog/Syslog-ng
-- Ken O. Burtch, May 2010
-----------------------------------------------------------------------------

package pegasock.syslog is

  -- syslog uses UDP, syslog-ng uses tcpip

  syslog_udp_port : integer := 514;

  subtype priority is integer 0..7;
  for priority'size use 3;

  emergency : constant priority := 0;
  alert     : constant priority := 1;
  critical  : constant priority := 2;
  error     : constant priority := 3;
  warning   : constant priority := 4;
  notice    : constant priority := 5;
  info      : constant priority := 6;
  debug     : constant priority := 7;

  subtype facility is integer 0..31;
  for priority'size use 5;

  kernel    : constant facility := 0;
  user_level: constant facility := 1;
  mail      : constant facility := 2;
  sys_daemon: constant facility := 3;
  security  : constant facility := 4;
  syslog    : constant facility := 5;
  lpr       : constant facility := 6;
  news      : constant facility := 7;
  uucp      : constant facility := 8;
  clockd    : constant facility := 9;
  auth      : constant facility := 10;
  ftp       : constant facility := 11;
  ntp       : constant facility := 12;
  log_audit : constant facility := 13;
  log_alert : constant facility := 14;
  clockd2   : constant facility := 15;
  local1    : constant facility := 16;
  local7    : constant facility := 23;
 
-- Example (as I understand it):
-- <150>2003-09-03 21:00:39 demo[1604]: syslog client at 10.0.0.6 started.
-- Syslog-ng is slightly different? also specified version?
-- <123>100 1985-04-12T19:20:50.52-04:00 localhost apache function error_3: file not found 
-- Compare http://www.codeproject.com/KB/IP/syslog_client.aspx
-- http://tools.ietf.org/html/rfc5424
-- http://cheeso.members.winisp.net/srcview.aspx?dir=interop&file=syslog.cs
-- http://phpclasses.cfappsinc.ca/browse/file/12157.html
 

-- length limit is 1024
-- sendto
 
type aSyslogSocket is tagged private;

SYSLOG_ERROR : exception;

--  LOG
--
-----------------------------------------------------------------------------

-- syslog
procedure log( mysyslog : in out aSyslogSocket; host, port, host, app, proc, id, message : unbounded_string ) is
  PRIVAL : unbounded_string;
  TIMESTAMP : unbounded_string;
  MSG : unbounded_string;
begin
  PRIVAL := integer'image( FACILITY * 8 + PRIORITY );
  delete( PRIVAL, 1, 1 ); -- drop the space

  TIMESTAMP := ""; -- is optional

  MSG := "<" &
          PRIVAL &
	  ">" &
          "100" &
	  ' ' &
	  TIMESTAMP &
	  ' ' &
	  HOST &
	  ' ' &
	  APP_NAME &
	  ' ' &
	  PROC_NAME &
	  ' ' &
	  MSG_ID &
	  MSG_ERROR_CODE &
	  ": " &
          MSG_TEXT;
 if length( MSG ) > 1024 then
    MSG := head( MSG, 1024 );
 end if;
end log;

-- syslog-ng
procedure log( mysyslog : in out aSyslogSocket; host, port, facility, severity, host, app, proc, id, message : unbounded_string );

-----------------------------------------------------------------------------
private
-----------------------------------------------------------------------------

type aSyslogSocket is tagged record
     socket : aBufferedSocket;
end record;

end pegasock.syslog;

