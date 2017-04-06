with Ada.Strings.Unbounded.Less_Case_Insensitive;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
package body Tree_Functions is

   ------------
   -- Key_Of --
   ------------

   function Key_Of (File_Record : File.File_Record_Acess) return Tree_Key is
   begin
      return Tree_Key(File_Record.Path) & Dir_Separator & Tree_Key(File_Record.File_Name);
   end Key_Of;

   ------------------------
   -- Compare_Tree_Nodes --
   ------------------------

   function Compare_Tree_Nodes (Left, Right : Tree_Key) return Boolean is
   begin
      return Left = Right;
   end Compare_Tree_Nodes;

   ------------------------
   -- Compare_Nodes_Less --
   ------------------------
   function Compare_Nodes_Less (Left, Right : Tree_Key) return Boolean is
   begin
      return Ada.Strings.Unbounded.Less_Case_Insensitive
        (Left  => Unbounded_String(Left),
         Right => Unbounded_String(Right));
   end Compare_Nodes_Less;

end Tree_Functions;
