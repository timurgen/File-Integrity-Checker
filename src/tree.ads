with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada;
package Tree is

   type Node;
   type Tree is access Node;
   type Node is record
      Item        : Integer;
      Left, Right : Tree;
   end record;

   procedure Preorder (T : Tree);

   procedure Inorder (T : Tree);

   procedure Insert (I : Integer; T : in out Tree);

end Tree;
