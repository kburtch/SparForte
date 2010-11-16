-- Copyright (C) 1994-2001 Grady Booch, David Weller and Simon Wright.
-- All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

-- $Id: bc.ads,v 1.2 2005/02/11 02:59:35 ken Exp $

package BC is

  -- This is the top level package in the Booch Component Hierarchy.

  -- The following exceptions may be raised by improper use of the Components.

  Container_Error : exception;
  Duplicate : exception;
  Is_Null : exception;
  Not_Found : exception;
  Not_Null : exception;
  Not_Root : exception;
  Overflow : exception;
  Range_Error : exception;
  Referenced : exception;
  Storage_Error : exception;
  Synchronization_Error : exception;
  Underflow : exception;

  Should_Have_Been_Overridden : exception;
  -- Raised if the Components have failed to override a primitive
  -- subprogram that should have been overridden for a derived type.
  -- Used only where the subprogram is private (and therefore can't be
  -- abstract).

  Not_Yet_Implemented : exception;
  -- Raised when a feature hasn't yet been implemented.

end BC;

