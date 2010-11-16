-- $Id: wc-tree_nodes.adb,v 1.2 2005/02/11 02:59:40 ken Exp $
--
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Unchecked_Deallocation;

package body WC.Tree_Nodes is

   CVSID : constant String := "$Header: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-tree_nodes.adb,v 1.2 2005/02/11 02:59:40 ken Exp $";

   --------------------------------------------------
   -- INTERNAL : Create a new tree node :
   --------------------------------------------------
   procedure New_Node(Context : in out Tree_Context_Type; Item : Item_Type; Node : out Tree_Node_Ptr) is
   begin

      if Context.Current = null then
         Context.Current := Context.Tree.Root;
      end if;

      Node := new Tree_Node;                          -- Allocate new node

      if Context.Current = null then
         Node.Parent := null;                        -- This is the first node
         Context.Tree.Root := Node;                  -- Point the Root to this new node
      else
         Node.Parent := Context.Current.Parent;      -- Use the current node's parent
      end if;

      Node.Item := New_Node.Item;

   end New_Node;

   --------------------------------------------------
   -- INTERNAL : Re-Parent all nodes at the current level
   --------------------------------------------------
   procedure Re_Parent(Context : Tree_Context_Type; New_Parent : Tree_Node_Ptr)
   is
      Node :   Tree_Node_Ptr := Context.Current;
   begin

      -- Find left most node
      loop
         exit when Node = null or else Node.Left = null;
         Node := Node.Left;
      end loop;

      -- Now reparent all nodes, processing from left to right
      loop
         exit when Node = null;
         Node.Parent := New_Parent;
         Node := Node.Right;
      end loop;

   end Re_Parent;

   --------------------------------------------------
   -- Free all nodes at this level, and below :
   --------------------------------------------------
   procedure Free(Level : in out Tree_Node_Ptr; Free_Proc : Free_Item)
   is
      Node :   Tree_Node_Ptr := Level;
      N :      Tree_Node_Ptr;
      procedure Rele is new Ada.Unchecked_Deallocation(Tree_Node,Tree_Node_Ptr);
   begin

      loop    -- Start at the left most node at this level
         exit when Node = null or else Node.Left = null;
         Node := Node.Left;
      end loop;

      loop    -- Process all nodes at this level
         exit when Node = null;
         if Node.Child /= null then
            Free(Node.Child,Free_Proc);   -- Free nodes at lower levels first
         end if;

         N := Node.Right;        -- Next node pointer

         if Free_Proc /= null then
            Free_Proc(Node.Item); -- Free this item
         end if;
         Rele(Node);             -- Free this node
         Node := N;              -- Proceed to next node
      end loop;

   end Free;

   --------------------------------------------------
   -- Finalization for a Tree_Type :
   --------------------------------------------------
   procedure Finalize(Tree : in out Tree_Type) is
      Parent : Tree_Node_Ptr := null;
      Node :   Tree_Node_Ptr := Tree.Root;
   begin

      Free(Tree.Root,Tree.Free_Proc);
      Tree.Root := null;

   end Finalize;

   --------------------------------------------------
   -- Open a new Tree Context :
   --------------------------------------------------
   procedure Open_Context(Tree : Tree_Type_Ptr; Context : out Tree_Context_Type) is
   begin

      Context.Tree := Tree;                           -- Point to the tree being used
      Context.Current := Tree.Root;                   -- This may be null if no nodes exist yet in the tree

   end Open_Context;

   --------------------------------------------------
   -- Close a Tree Context :
   --------------------------------------------------
   procedure Close_Context(Context : in out Tree_Context_Type) is
   begin

      Context.Tree := null;                           -- forget pointer to the tree being used
      Context.Current := null;                        -- and there is no current node now

   end Close_Context;

   --------------------------------------------------
   -- Set Next Attach Context :
   --------------------------------------------------
   procedure Attach_Next(Context : in out Tree_Context_Type; Where : Attach_Type) is
   begin

      Context.Next_Attach := Where;

   end Attach_Next;

   --------------------------------------------------
   -- Attach in current Context :
   --------------------------------------------------
   procedure Attach(Context : in out Tree_Context_Type; Item : Item_Type) is
   begin

      case Context.Next_Attach is
         when Right =>
            Attach_Right(Context,Item);
         when Left =>
            Attach_Left(Context,Item);
         when Child =>
            Attach_Child(Context,Item);
         when Parent =>
            Attach_Parent(Context,Item);
      end case;

   end Attach;

   --------------------------------------------------
   -- Attach a node to the right of the current :
   --------------------------------------------------
   procedure Attach_Right(Context : in out Tree_Context_Type; Item : Item_Type)
   is
      Node :       Tree_Node_Ptr := null;
   begin

      if Context.Current = null then
         Context.Current := Context.Tree.Root;
      end if;
        
      if Context.Current /= null and then Context.Current.Right /= null then
         raise Attach_Error;
      end if;

      New_Node(Context,Item,Node);

      if Context.Tree.Root /= Node then
         Context.Current.Right := Node;
         Node.Left             := Context.Current;
      end if;
      Context.Current := Node;

   end Attach_Right;

   --------------------------------------------------
   -- Attach a node to the left of the current :
   --------------------------------------------------
   procedure Attach_Left(Context : in out Tree_Context_Type; Item : Item_Type)
   is
      Node :   Tree_Node_Ptr := null;
   begin

      if Context.Current = null then
         Context.Current := Context.Tree.Root;
      end if;
        
      if Context.Current /= null and then Context.Current.Left /= null then
         raise Attach_Error;
      end if;

      New_Node(Context,Item,Node);

      if Context.Tree.Root /= Node then
         Context.Current.Left := Node;
         Node.Right           := Context.Current;
      end if;
      Context.Current := Node;

   end Attach_Left;

   --------------------------------------------------
   -- Attach a node to the right of the current :
   --------------------------------------------------
   procedure Attach_Child(Context : in out Tree_Context_Type; Item : Item_Type)
   is
      Node :   Tree_Node_Ptr := null;
   begin

      if Context.Current = null then
         Context.Current := Context.Tree.Root;
      end if;
        
      if Context.Current /= null and then Context.Current.Child /= null then
         raise Attach_Error;
      end if;

      New_Node(Context,Item,Node);

      if Context.Tree.Root /= Node then
         Context.Current.Child := Node;
         Node.Parent           := Context.Current;
      end if;
      Context.Current := Node;

   end Attach_Child;

   --------------------------------------------------
   -- Attach a new node above the current :
   --------------------------------------------------
   procedure Attach_Parent(Context : in out Tree_Context_Type; Item : Item_Type)
   is
      Node :   Tree_Node_Ptr := null;
   begin

      if Context.Current = null then
         Context.Current := Context.Tree.Root;
      end if;

      if Context.Current /= null and then Context.Current.Parent /= null then
         raise Attach_Error;
      end if;

      New_Node(Context,Item,Node);

      Node.Child          := Context.Current;
      if Context.Tree.Root /= Node then
         Re_Parent(Context,Node);                    -- All nodes at this level now point up to new node
         Context.Tree.Root   := Node;                -- The new node is now the root node
      end if;
      Context.Current := Node;                -- New current node

   end Attach_Parent;

   --------------------------------------------------
   -- Return the Item at the current node :
   --------------------------------------------------
   function Current(Context : Tree_Context_Type) return Item_Type is
   begin

      return Context.Current.Item;

   end Current;

   --------------------------------------------------
   -- Change the context to the next Right node :
   --------------------------------------------------
   procedure Right(Context : in out Tree_Context_Type) is
   begin

      if Context.Current /= null then
         Context.Current := Context.Current.Right;
      else
         raise Traversal_Error;
      end if;

   end Right;

   --------------------------------------------------
   -- Change the context to the next Left node :
   --------------------------------------------------
   procedure Left(Context : in out Tree_Context_Type) is
   begin

      if Context.Current /= null then
         Context.Current := Context.Current.Left;
      else
         raise Traversal_Error;
      end if;

   end Left;

   --------------------------------------------------
   -- Change the context to the next upper node :
   --------------------------------------------------
   procedure Ascend(Context : in out Tree_Context_Type) is
   begin

      if Context.Current /= null then
         Context.Current := Context.Current.Parent;
      else
         raise Traversal_Error;
      end if;

   end Ascend;

   --------------------------------------------------
   -- Change the context to the next lower node :
   --------------------------------------------------
   procedure Descend(Context : in out Tree_Context_Type) is
   begin

      if Context.Current /= null then
         Context.Current := Context.Current.Child;
      else
         raise Traversal_Error;
      end if;

   end Descend;

   --------------------------------------------------
   -- Change the context to the Root node :
   --------------------------------------------------
   procedure Root(Context : in out Tree_Context_Type) is
   begin

      Context.Current := Context.Tree.Root;

   end Root;

   --------------------------------------------------
   -- Change the context to the right most node :
   --------------------------------------------------
   procedure Right_Most(Context : in out Tree_Context_Type) is
   begin

      if Context.Current = null then
         Context.Current := Context.Tree.Root;
      end if;

      loop
         exit when Context.Current = null or else Context.Current.Right = null;
         Context.Current := Context.Current.Right;
      end loop;

   end Right_Most;

   --------------------------------------------------
   -- Set Context to "No current node"
   --------------------------------------------------
   procedure No_Node(Context : in out Tree_Context_Type) is
   begin

      Context.Current := null;

   end No_Node;

   --------------------------------------------------
   -- Change the context to the left most node :
   --------------------------------------------------
   procedure Left_Most(Context : in out Tree_Context_Type) is
   begin

      if Context.Current = null then
         Context.Current := Context.Tree.Root;
      end if;

      loop
         exit when Context.Current = null or else Context.Current.Left = null;
         Context.Current := Context.Current.Left;
      end loop;

   end Left_Most;

   --------------------------------------------------
   -- Return True if there is a Right Node :
   --------------------------------------------------
   function Is_Right(Context : Tree_Context_type) return Boolean
   is
      Current :    Tree_Node_Ptr := Context.Current;
   begin

      if Current = null then
         Current := Context.Tree.Root;
      end if;

      if Current = null then
         return False;
      end if;

      return Current.Right /= null;

   end Is_Right;

   --------------------------------------------------
   -- Return True if there is a Left Node :
   --------------------------------------------------
   function Is_Left(Context : Tree_Context_type) return Boolean
   is
      Current :    Tree_Node_Ptr := Context.Current;
   begin

      if Current = null then
         Current := Context.Tree.Root;
      end if;

      if Current = null then
         return False;
      end if;

      return Current.Left /= null;

   end Is_Left;

   --------------------------------------------------
   -- Return True if there is a Parent Node :
   --------------------------------------------------
   function Is_Parent(Context : Tree_Context_type) return Boolean
   is
      Current :    Tree_Node_Ptr := Context.Current;
   begin

      if Current = null then
         Current := Context.Tree.Root;
      end if;

      if Current = null then
         return False;
      end if;

      return Current.Parent /= null;

   end Is_Parent;

   --------------------------------------------------
   -- Return True if there is a Child Node :
   --------------------------------------------------
   function Is_Child(Context : Tree_Context_type) return Boolean
   is
      Current :    Tree_Node_Ptr := Context.Current;
   begin

      if Current = null then
         Current := Context.Tree.Root;
      end if;

      if Current = null then
         return False;
      end if;

      return Current.Child /= null;

   end Is_Child;

   --------------------------------------------------
   -- Return TRUE if the Tree is Empty :
   --------------------------------------------------
   function Is_Empty(Context : Tree_Context_Type) return Boolean is
   begin

      return Context.Tree.Root = null;

   end Is_Empty;

   --------------------------------------------------
   -- Test if there is a Current Node (ie. any nodes)
   --------------------------------------------------
   function Is_Current(Context : Tree_Context_Type) return Boolean
   is
      Current :    Tree_Node_Ptr := Context.Current;
   begin

      return Current /= null;

   end Is_Current;
    
   --------------------------------------------------
   -- Traverse a Tree at a Particular Level :
   --------------------------------------------------
   procedure Traverse_Tree(Context : in out Tree_Context_Type; Method : Traversal_Type; Proc : Trav_Proc; Level : Natural)
   is
      Save_Node : Tree_Node_Ptr := null;
   begin

      case Method is
         when Depth_First =>
            loop
               exit when not Is_Current(Context);
      
               declare
                  Item :   Item_Type := Current(Context);
               begin
                  Proc(Item,Level);
               end;

               if Is_Child(Context) then
                  Save_Node := Context.Current;                       -- Save current context
                  Descend(Context);                                   -- Descend to Child Node
                  Traverse_Tree(Context,Method,Proc,Level+1);         -- Traverse at the Next Level(s)
                  Context.Current := Save_Node;                       -- Restore Context
               end if;
      
               Right(Context);                                         -- Move to next Right node
            end loop;
         when others =>
            raise Program_Error;
      end case;

   end Traverse_Tree;

   --------------------------------------------------
   -- Traverse a Tree :
   --------------------------------------------------
   procedure Traverse_Tree(Tree : Tree_Type_Ptr; Method : Traversal_Type; Proc : Trav_Proc)
   is
      Context :    Tree_Context_Type;
      Level :      Natural := 0;
   begin

      Open_Context(Tree,Context);
      Traverse_Tree(Context,Method,Proc,0);
      Close_Context(Context);

   end Traverse_Tree;

   --------------------------------------------------
   -- Internal : Locate a Node 
   --------------------------------------------------
   function Locate(Root : Tree_Node_Ptr; Item : Item_Type) return Tree_Node_Ptr is
      Node :   Tree_Node_Ptr := Root;
      R :      Tree_Node_Ptr := null;
   begin
        
      loop    -- Start at left most node, for this level
         exit when Node.Left = null;
         Node := Node.Left;
      end loop;

      loop
         exit when Node = null;
         if Node.Item = Locate.Item then
            return Node;
         end if;

         if Node.Child /= null then
            R := Locate(Node.Child,Item);
            if R /= null then
               return R;
            end if;
         end if;
      
         Node := Node.Right;
      end loop;

      return Node;

   end Locate;

   --------------------------------------------------
   -- Locate a Node by Item :
   --------------------------------------------------
   procedure Locate(Context : in out Tree_Context_Type; Item : Item_Type) is
   begin

      Context.Current := Locate(Context.Tree.Root,Item);

   end Locate;

end WC.Tree_Nodes;

-- $Source: /home/cvsroot/bush/src/ADAVOX-0.51/wc/wc-tree_nodes.adb,v $
