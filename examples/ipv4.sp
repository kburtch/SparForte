#!/usr/local/bin/spar

pragma annotate( summary, "ipv4" )
       @( description, "Demonstrate some computations with IP address and " )
       @( description, "subnet mask pairs, using strong typing and " )
       @( description, "programming-by-contract." )
       @( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure ipv4 is

  ----------------------------------------------------------------------------
  --
  -- numeric ip number and subnet mask
  --
  ----------------------------------------------------------------------------

  -- a type of long integer where an error occurs if the value is not 32-bits

  unsigned32bits_error : exception;

  type unsigned32bits is new long_integer
  affirm
    raise unsigned32bits_error when unsigned32bits not in 0..4294967295;
  end affirm;

  -- a type of long integer where an error occurs if the value is not 1 byte

  unsignedbyte_error : exception;

  subtype unsignedbyte is unsigned32bits
  affirm
    raise unsignedbyte_error when unsignedbyte not in 0..255;
  end affirm;

  -- distinct types for IP address and subnet mask

  type ip_address is new unsigned32bits;
  type ip_subnet_mask is new unsigned32bits;


  --  PUT UNSIGNED32BITS
  --
  -- Display a 32-bit integer in dotted octet format (AAA.BBB.CCC.DDD).
  ----------------------------------------------------------------------------

  procedure put_unsigned32bits( num : unsigned32bits ) is
     octet     : unsignedbyte;
     remainder : unsigned32bits;
  begin
     remainder := num;
     octet := numerics.floor( remainder / (256*256*256) );
     remainder := @ - octet * (256 * 256 * 256);
     put( octet, "999" ) @ (".");
     octet := numerics.floor( remainder / (256*256) );
     remainder := @ - octet * (256 * 256);
     put( octet, "999" ) @ (".");
     octet := numerics.floor( remainder / 256 );
     remainder := @ - octet * 256;
     put( octet, "999" ) @ (".");
     octet := remainder;
     put( octet, "999" );
  end put_unsigned32bits;


  ----------------------------------------------------------------------------
  --
  -- hostnames
  --
  ----------------------------------------------------------------------------


  hostname_error : exception;

  -- a string for a valid Internet hostname

  type hostname_string is new string
  affirm
    raise hostname_error when strings.length( hostname_string ) not in 1..63;
    raise hostname_error when not strings.is_alphanumeric( strings.head( hostname_string, 1 ) );
    raise hostname_error when strings.tail( hostname_string, 1 ) = '-' or
                              strings.tail( hostname_string, 1 ) = '_';
    for i in 2..strings.length( hostname_string )-1 loop
        if not strings.is_alphanumeric( strings.element( hostname_string, i ) ) and
           not ( strings.element( hostname_string, i ) = '-' ) and
           not ( strings.element( hostname_string, i ) = '_' ) then
           raise hostname_error;
        end if;
    end loop;
  end affirm;


  ----------------------------------------------------------------------------
  --
  -- human-readable ip number string
  --
  ----------------------------------------------------------------------------


  format_error : exception;

  -- an octet string where an error is raised if the format is wrong

  type ip_string is new string
  affirm
    raise format_error when strings.length( ip_string ) /= 15;
    raise format_error when strings.element( ip_string, 4 ) /= ".";
    raise format_error when strings.element( ip_string, 8 ) /= ".";
    raise format_error when strings.element( ip_string, 12 ) /= ".";
    -- check octets
    raise format_error when numerics.value( strings.slice( ip_string, 1, 3 ) ) < 0;
    raise format_error when numerics.value( strings.slice( ip_string, 1, 3 ) ) > 255;
    raise format_error when numerics.value( strings.slice( ip_string, 5, 7 ) ) < 0;
    raise format_error when numerics.value( strings.slice( ip_string, 5, 7 ) ) > 255;
    raise format_error when numerics.value( strings.slice( ip_string, 9, 11 ) ) < 0;
    raise format_error when numerics.value( strings.slice( ip_string, 9, 11 ) ) > 255;
    raise format_error when numerics.value( strings.slice( ip_string, 11, 15 ) ) < 0;
    raise format_error when numerics.value( strings.slice( ip_string, 13, 15 ) ) > 255;
  end affirm;

  -- some standard constants

  network_ip_string   : constant ip_string := "000.000.000.000";
  broadcast_ip_string : constant ip_string := "255.255.255.255";

  -- a type of long integer where an error occurs if the value is out of range

  cidr_error : exception;

  subtype cidr_mask is unsigned32bits
  affirm
    raise cidr_error when cidr_mask not in 0..31;
  end affirm;


  --  CIDR TO MASK
  --
  -- Convert 0-32 CIDR notation to a subnet mask.
  ----------------------------------------------------------------------------

  function cidr_to_mask( bits : cidr_mask ) return ip_subnet_mask is
    tmp : unsigned32bits := bits;
    res : ip_subnet_mask := 0;
  begin
    for i in 1..32 loop
        tmp := @ - 1;
        if tmp >= 0 then
           res := numerics.shift_left( @, 1 ) + 1;
        else
           res := numerics.shift_left( @, 1 );
        end if;
    end loop;
    return res;
  end cidr_to_mask;


  ----------------------------------------------------------------------------
  --
  -- numeric ip number and subnet mask
  -- ip number and subnet mask pair
  --
  ----------------------------------------------------------------------------


  type ip_pair is record
     host : hostname_string;
     ip   : ip_address;
     mask : ip_subnet_mask;
  end record;


  --  ASSIGN PAIR
  --
  -- Create an IP pair from an IP addresss and a subnet mask strings.
  ----------------------------------------------------------------------------

  procedure assign_pair( pair : in out ip_pair; host : hostname_string; ip : ip_string; ip_mask : ip_string ) is
  begin
     pair.host := host;
     pair.ip :=
         numerics.value( strings.slice( ip, 13, 15 ) ) +
         numerics.value( strings.slice( ip, 9, 11 ) ) * 256 +
         numerics.value( strings.slice( ip, 5, 7 ) ) * 256 * 256 +
         numerics.value( strings.slice( ip, 1, 3 ) ) * 256 * 256 * 256;
     pair.mask :=
         numerics.value( strings.slice( ip_mask, 13, 15 ) ) +
         numerics.value( strings.slice( ip_mask, 9, 11 ) ) * 256 +
         numerics.value( strings.slice( ip_mask, 5, 7 ) ) * 256 * 256 +
         numerics.value( strings.slice( ip_mask, 1, 3 ) ) * 256 * 256 * 256;
  end assign_pair;


  --  ASSIGN CIDR
  --
  -- Create an IP pair from an IP addresss and a CIDR subnet mask.
  ----------------------------------------------------------------------------

  procedure assign_cidr( pair : in out ip_pair; host : hostname_string; ip : ip_string; bits : cidr_mask ) is
  begin
     pair.host := host;
     pair.ip :=
         numerics.value( strings.slice( ip, 13, 15 ) ) +
         numerics.value( strings.slice( ip, 9, 11 ) ) * 256 +
         numerics.value( strings.slice( ip, 5, 7 ) ) * 256 * 256 +
         numerics.value( strings.slice( ip, 1, 3 ) ) * 256 * 256 * 256;
    pair.mask := cidr_to_mask( bits );
  end assign_cidr;


  --  PUT PAIR
  --
  -- Display the ip address and subnet mask as octets strings.
  ----------------------------------------------------------------------------

  procedure put_pair( pair : in out ip_pair ) is
     pragma assumption( written, pair );
     -- when this was written, a record cannot be an in parameter.
     -- assume it was written, even though it was not
  begin
     put( pair.host ) @ (": ");
     put_unsigned32bits( unsigned32bits( pair.ip ) );
     put( " / " );
     put_unsigned32bits( unsigned32bits( pair.mask ) );
  end put_pair;


  --  PUT SUBNET
  --
  -- Display the subnet as octets strings based on an ip address and mask.
  ----------------------------------------------------------------------------

  procedure put_subnet( pair : in out ip_pair ) is
     pragma assumption( written, pair );
     -- when this was written, a record cannot be an in parameter.
     -- assume it was written, even though it was not
    subnet : unsigned32bits;
  begin
    subnet := unsigned32bits( pair.ip ) and unsigned32bits( pair.mask );
    put_unsigned32bits( subnet );
  end put_subnet;

  pair : ip_pair;

begin
  assign_pair( pair, "broadcast", broadcast_ip_string, broadcast_ip_string );
  put_pair( pair );
  new_line;

  assign_pair( pair, "network", network_ip_string, network_ip_string );
  put_pair( pair );
  new_line;

  assign_pair( pair, "host1", "192.168.001.255", "255.255.255.000" );
  put_pair( pair );
  new_line;

  put( "   subnet is " ); put_subnet( pair );
  new_line;

  assign_cidr( pair, "host2", "192.168.001.002", 16 );
  put_pair( pair );
  new_line;

  put( "   subnet is " ); put_subnet( pair );
  new_line;

end ipv4;

-- VIM editor formatting instructions
-- vim: ft=spar

