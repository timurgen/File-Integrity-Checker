with Unchecked_Deallocation;
package body Binary_Search_Tree is

   procedure Free is new Unchecked_Deallocation (Node_Type, Node_Ptr);

   function Empty (Tree: Tree_Type) return Boolean is
   begin
      return Tree = null;
   end Empty;

   function Size (Tree: Tree_Type) return Natural is
   begin
      if Tree = null then
         return (0);
      else
         return (1 + Size (Tree.Left) + Size (Tree.Right));
      end if;
   end Size;

   procedure Clear (Tree : in out Tree_Type) is
   begin
      if Tree /= null then
         Clear (Tree.Left);   -- Delete all nodes in the left subtree
         Clear (Tree.Right);  -- Delete all nodes in the right subtree
         Free (Tree);         -- Delete this node
      end if;
   end Clear;

   procedure Insert (Tree: in out Tree_Type; Item: in Element_Type) is
   begin
      if Tree = null then
         Tree := new Node_Type'(Info=>Item, Left=>null, Right=>null);
      elsif Key_Of (Item) = Key_Of (Tree.Info) then
         raise Duplicate_Key;
      elsif Key_of (Item) < Key_Of (Tree.Info) then
         Insert (Tree => Tree.Left, Item => Item);
      else
         Insert (Tree => Tree.Right, Item => Item);
      end if;
   end Insert;


   procedure Modify (Tree    : in out Tree_Type;
                     Element : in     Element_Type) is
   begin
      if Tree = null then
         raise Key_Error;
      elsif Key_Of (Element) = Key_Of (Tree.Info) then
         Tree.Info := Element;
      elsif Key_Of (Element) < Key_Of (Tree.Info) then
         Modify (Tree => Tree.Left, Element => Element);
      else
         Modify (Tree => Tree.Right, Element => Element);
      end if;
   end Modify;

   function Retrieve (Tree: Tree_Type;
                      Key :  Key_Type) return Element_Type is
   begin
      if Tree = null then
         raise Key_Error;
      elsif Key = Key_Of (Tree.Info) then
         return (Tree.Info);
      elsif Key < Key_Of (Tree.Info) then
         return (Retrieve (Tree => Tree.Left, Key => Key));
      else
         return (Retrieve (Tree => Tree.Right, Key => Key));
      end if;
   end Retrieve;


   -- Finds and unlinks the node with the maximum key from non-empty tree
   procedure Find_And_Unlink_Max (Tree: in out Node_Ptr;
                                  Max :    out Element_Type) is
   begin
      if Tree.Right = null then  -- Is there a right child?
         -- Root contains the maximum key in Tree
         Max  := Tree.Info;
         Free (Tree);
         Tree := Tree.Left;   -- Replace root with left subtree
      else
         -- Keep looking in the right subtree
         Find_And_Unlink_Max (Tree => Tree.Right, Max => Max);
      end if;
   end Find_And_Unlink_Max;


   -- This procedure deletes the root node from the given tree.
   procedure Delete_Root (Tree : in out Tree_Type) is
      To_Recycle : Node_Ptr;  -- For recycling nodes
   begin
      if Tree.Left = null  and Tree.Right = null then
         Free (Tree);                    -- Entire tree now empty
      elsif Tree.Left = null then
         -- Root node has only a right child
         To_Recycle := Tree;             -- Save for later deallocation
         Tree := Tree.Right;             -- Unlink the root node
         Free (To_Recycle);              -- Deallocate former root node
      elsif Tree.Right = null  then
         -- Root node has a left child
         To_Recycle := Tree;             -- Save for later deallocation
         Tree := Tree.Left;              -- Unlink the root node
         Free (To_Recycle);              -- Deallocate former root node
      else
         -- Find and unlink the logical predecessor
         Find_And_Unlink_Max (Tree => Tree.Left, Max => Tree.Info);
      end if;
   end Delete_Root;


   procedure Delete (Tree : in out Tree_Type;
                     Key  : in     Key_Type) is
   begin
      if Tree = null then
         raise Key_Error;
      elsif Key = Key_Of (Tree.Info) then
         Delete_Root (Tree);
      elsif Key < Key_Of (Tree.Info) then
         Delete (Tree => Tree.Left,  Key => Key);
      else
         Delete (Tree => Tree.Right, Key => Key);
      end if;
   end Delete;

   procedure Traverse (Tree  : in out Tree_Type;
                       Order : in Traversal_Order) is

      procedure Checked_Process (Element: in out Element_Type) is
         New_Element: Element_Type := Element;
      begin
         Process (New_Element);
         if Key_Of (Element) /= Key_Of (New_Element) then
            raise Key_Error;
         else
            Element := New_Element;
         end if;
      end Checked_Process;


      procedure Inorder (Tree : in Tree_Type) is
      begin
         if Tree /= null then
            Inorder (Tree => Tree.Left);    -- Traverse Left subtree
            Checked_Process (Tree.Info);
            Inorder (Tree => Tree.Right);   -- Traverse Right subtree
         end if;
      end Inorder;

      procedure Preorder (Tree : in Tree_Type) is
      begin
         if Tree /= null then
            Checked_Process (Tree.Info);
            Preorder (Tree => Tree.Left);    -- Traverse Left subtree
            Preorder (Tree => Tree.Right);   -- Traverse Right subtree
         end if;
      end Preorder;

      procedure Postorder (Tree : in Tree_Type) is
      begin
         if Tree /= null then
            Postorder (Tree => Tree.Left);    -- Traverse Left subtree
            Postorder (Tree => Tree.Right);   -- Traverse Right subtree
            Checked_Process (Tree.Info);
         end if;
      end Postorder;

   begin -- Traverse
      case Order is
         when Inorder   => Inorder (Tree);
         when Preorder  => Preorder (Tree);
         when Postorder => Postorder (Tree);
      end case;
   end Traverse;

end Binary_Search_Tree;
