with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with File;
package Tree_Functions is

   type Tree_Key is new Unbounded_String;
   function Key_Of (File_Record : File.File_Record_Acess) return Tree_Key;
   function Compare_Tree_Nodes (Left, Right : Tree_Key) return Boolean;
   function Compare_Nodes_Less (Left, Right : Tree_Key) return Boolean;

end Tree_Functions;
