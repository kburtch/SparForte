-- $Id: wc-global_heap.ads,v 1.2 2005/02/11 02:59:37 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with BC.Support.Managed_Storage;

package WC.Global_Heap is

   subtype Pool is BC.Support.Managed_Storage.Pool;
   Storage :  Pool (Chunk_Size => 2048);

private

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-global_heap.ads,v 1.2 2005/02/11 02:59:37 ken Exp $";

end WC.Global_Heap;
    
