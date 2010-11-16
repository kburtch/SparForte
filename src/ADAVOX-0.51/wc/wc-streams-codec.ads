-- $Id: wc-streams-codec.ads,v 1.2 2005/02/11 02:59:38 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Interfaces; use Interfaces;
with Ada.Streams; use Ada.Streams;

with WC.Streams.Audio;
use WC.Streams.Audio;

package WC.Streams.Codec is

   Unsupported_Format : exception;

   procedure Pump(In_Str, Out_Str : Audio_Stream);

private

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-streams-codec.ads,v 1.2 2005/02/11 02:59:38 ken Exp $";

end WC.Streams.Codec;
