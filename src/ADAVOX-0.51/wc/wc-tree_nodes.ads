-- $Id: wc-tree_nodes.ads,v 1.2 2005/02/11 02:59:40 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;

generic
   type Item_Type is private;
   type Free_Item is access procedure(Item : in out Item_Type);
package WC.Tree_Nodes is

   Traversal_Error :    exception;     -- Could not traverse, because no more nodes exist
   Attach_Error :       exception;     -- Raised when adding a node, where a node already exists
   Locate_Error :       exception;     -- Item not found in tree

   type Traversal_Type is ( Depth_First );
   type Tree_Context_Type is private;
   type Tree_Type(Free_Proc : Free_Item := null) is limited private;
   type Tree_Type_Ptr is access all Tree_Type;

   type Trav_Proc is access procedure(Item : in out Item_Type; Level : Natural);
    
   --------------------------------------------------
   -- Traverse the Tree :
   --------------------------------------------------
   procedure Traverse_Tree(Tree : Tree_Type_Ptr; Method : Traversal_Type; Proc : Trav_Proc);

   --------------------------------------------------
   -- Open and Close a Tree Context :
   --------------------------------------------------
   procedure Open_Context(Tree : Tree_Type_Ptr; Context : out Tree_Context_Type);
   procedure Close_Context(Context : in out Tree_Context_Type);

   --------------------------------------------------
   -- Context Driven Attaching :
   --------------------------------------------------
   type Attach_Type is (Right, Left, Child, Parent);
   procedure Attach_Next(Context : in out Tree_Context_Type; Where : Attach_Type);
   procedure Attach(Context : in out Tree_Context_Type; Item : Item_Type);

   --------------------------------------------------
   -- Attach new nodes to the tree :
   --------------------------------------------------
   procedure Attach_Right(Context : in out Tree_Context_Type; Item : Item_Type);
   procedure Attach_Left(Context : in out Tree_Context_Type; Item : Item_Type);
   procedure Attach_Child(Context : in out Tree_Context_Type; Item : Item_Type);
   procedure Attach_Parent(Context : in out Tree_Context_Type; Item : Item_Type);

   --------------------------------------------------
   -- Context Changing Procedures ;
   --------------------------------------------------
   procedure Root(Context : in out Tree_Context_Type);          -- Move to root node
   procedure Right(Context : in out Tree_Context_Type);         -- Move right
   procedure Left(Context : in out Tree_Context_Type);          -- Move left
   procedure Ascend(Context : in out Tree_Context_Type);        -- Move up
   procedure Descend(Context : in out Tree_Context_Type);       -- Move down
   procedure Left_Most(Context : in out Tree_Context_Type);     -- Move to left most
   procedure Right_Most(Context : in out Tree_Context_Type);    -- Move to right most
   procedure No_Node(Context : in out Tree_Context_Type);       -- Set selection to "no node"
   procedure Locate(Context : in out Tree_Context_Type; Item : Item_Type);

   --------------------------------------------------
   -- Retrieve the Item at the current node :
   --------------------------------------------------
   function Current(Context : Tree_Context_Type) return Item_Type;

   --------------------------------------------------
   -- Context Testing Functions :
   --------------------------------------------------
   function Is_Empty(Context : Tree_Context_Type) return Boolean;
   function Is_Current(Context : Tree_Context_Type) return Boolean;     -- Can return FALSE if tree not empty (see No_Node())
   function Is_Right(Context : Tree_Context_Type) return Boolean;
   function Is_Left(Context : Tree_Context_Type) return Boolean;
   function Is_Parent(Context : Tree_Context_Type) return Boolean;
   function Is_Child(Context : Tree_Context_Type) return Boolean;

private

   type Tree_Node;
   type Tree_Node_Ptr is access all Tree_Node;
   type Tree_Node_Ptr_Ptr is access all Tree_Node_Ptr;

   --------------------------------------------------
   -- The Tree Node Itself :
   --------------------------------------------------
   type Tree_Node is
      record
         Left :       Tree_Node_Ptr := null;      -- Pointer to prior node, if any
         Right :      Tree_Node_Ptr := null;      -- Pointer to next node, if any
         Parent :     Tree_Node_Ptr := null;      -- Pointer to parent node, if any
         Child :      Tree_Node_Ptr := null;      -- Pointer to child node, if any
         Item :       Item_Type;                  -- Item
      end record;

   procedure Finalize(Tree : in out Tree_Type);

   --------------------------------------------------
   -- The Tree Type :
   --------------------------------------------------
   type Tree_Type(Free_Proc : Free_Item := null) is new controlled with
      record
         Root :       Tree_Node_Ptr := null;      -- The root node of the tree
      end record;

   --------------------------------------------------
   -- The Tree Context :
   --------------------------------------------------
   type Tree_Context_Type is
      record
         Tree :          Tree_Type_Ptr   := null;    -- Points to the underlying tree
         Current :       Tree_Node_Ptr   := null;    -- Points to the current node, if any
         Next_Attach :   Attach_Type     := Right;   -- Sets attach mode for next Attach() call
      end record;

   CVSID_Spec : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-tree_nodes.ads,v 1.2 2005/02/11 02:59:40 ken Exp $";

end WC.Tree_Nodes;
