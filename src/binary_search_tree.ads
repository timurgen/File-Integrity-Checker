-- The elements in the tree are ordered from smallest to largest using
-- the "<" function supplied as a generic parameter.  The placement of
-- each element into the binary tree must satisfy the binary search
-- property:  The value of the key of an element is greater than the
-- value of the key of any element in its left subtree, and less than
-- the value of the key of any element in its right subtree.

generic

   type Element_Type is private;       -- The type of element in the list
   type Key_Type is limited private;   -- The type of key in the element
   with function Key_Of (Element : Element_Type) return Key_Type;
   with function "=" (Left, Right : Key_Type) return Boolean;
   with function "<" (Left, Right : Key_Type) return Boolean;

package Binary_Search_Tree is

   type Tree_Type is limited private;

   type Traversal_Order is (Inorder, Preorder, Postorder);

   Duplicate_Key, Key_Error : exception;

   function Empty (Tree : Tree_Type) return Boolean;
   function Size (Tree : Tree_Type) return Natural;

   procedure Clear (Tree : in out Tree_Type);

   procedure Insert (Tree : in out Tree_Type; Item : in Element_Type);

   procedure Delete (Tree : in out Tree_Type; Key : in Key_Type);

   procedure Modify (Tree : in out Tree_Type; Element : in Element_Type);

   function Retrieve (Tree : Tree_Type; Key : Key_Type) return Element_Type;

   generic
      -- A procedure that will be called once for every elment
      -- in the Tree.  Process may not change the key of Element.
      with procedure Process (Element : in out Element_Type);

   procedure Traverse (Tree : in out Tree_Type; Order : in Traversal_Order);

private

   type Node_Type;                       -- Incomplete type declaration

   type Tree_Type is access Node_Type;   -- Access to a node
   subtype Node_Ptr is Tree_Type;        -- A synonym for our access type

   type Node_Type is                     -- Complete type declaration
   record
      Info  : Element_Type;    -- One element
      Left  : Node_Ptr;        -- Link to left child
      Right : Node_Ptr;        -- Link to right child
   end record;

end Binary_Search_Tree;
