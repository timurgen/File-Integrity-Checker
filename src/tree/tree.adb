with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada;

package body Tree is

   procedure Preorder (T : Tree) is
   begin
      if (T /= null) then
         Integer_Text_IO.Put (T.Item);
         Text_IO.New_Line;
         Preorder (T.Left);
         Preorder (T.Right);
      end if;
   end Preorder;

   procedure Inorder (T : Tree) is
   begin
      if (T /= null) then
         Inorder (T.Left);
         Integer_Text_IO.Put (T.Item);
         Text_IO.New_Line;
         Inorder (T.Right);
      end if;
   end Inorder;

   procedure Insert (I : Integer; T : in out Tree) is
   begin
      if T = null then
         T := new Node'(I, null, null);
      elsif I < T.Item then
         Insert (I, T.Left);
      else
         Insert (I, T.Right);
      end if;
   end Insert;
end Tree;
