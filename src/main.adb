pragma Assertion_Policy (Check);
with CLI;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions;        use Ada.Assertions;
with Ada.Directories;       use Ada.Directories;
with Ada.IO_Exceptions;

with Binary_Search_Tree;
with File;
with Ada.Real_Time;

with Ada.Strings.Fixed;
with MD5.Driver;
with Ada.Strings.Fixed.Less_Case_Insensitive;
with Ada.Calendar;
with Ada.Strings.Unbounded.Less_Case_Insensitive;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

procedure Main is
   -- text output
   package TIO renames Ada.Text_IO;
   -- time counting
   package ART renames Ada.Real_Time;

   -- elapsed time measure
   Start_Time   : ART.Time := ART.Clock;
   Finish_Time  : ART.Time;
   Elapsed_Time : ART.Time_Span;

   -- input arguments
   Arg_1 : Unbounded_String;
   -- path to search
   Path_To_Search_U : Unbounded_String;


   --temp
   subtype Tree_Key is Unbounded_String;


   function Key_Of (File_Record : File.File_Record_Acess) return Tree_Key is
   begin
      return File_Record.Path & Dir_Separator & File_Record.File_Name;
   end Key_Of;

   function Compare_Tree_Nodes (Left, Right : Tree_Key) return Boolean is
   begin
      return Left = Right;
   end Compare_Tree_Nodes;

   function Compare_Nodes_Less (Left, Right : Tree_Key) return Boolean is
   begin
      return Ada.Strings.Unbounded.Less_Case_Insensitive
          (Left  => Left,
           Right => Right);
   end Compare_Nodes_Less;

   package Search_Tree is new Binary_Search_Tree
     (Element_Type => File.File_Record_Acess,
      Key_Type     => Tree_Key,
      Key_Of       => Key_Of,
      "="          => Compare_Tree_Nodes,
      "<"          => Compare_Nodes_Less);
   Tree_Root : Search_Tree.Tree_Type;
   procedure Process_Tree_Element (Element : in out File.File_Record_Acess) is
   begin
      TIO.Put_Line (To_String (Element.Path) & Dir_Separator &  To_String (Element.File_Name));
      TIO.New_Line;
   end Process_Tree_Element;

   procedure Traverse_Tree is new Search_Tree.Traverse (Process_Tree_Element);

   procedure Walk (Name : String; Pattern : String) is

      --
      -- function that process every single file
      --
      procedure Process_Item (Item : Directory_Entry_Type) with
         Pre => Kind (Item) = Ordinary_File is
         Digest    : String (1 .. 32);
         New_Entry : File.File_Record_Acess;
      begin
         MD5.Driver.Digest_File (Full_Name (Item), Digest);
         TIO.Put_Line (Digest);
         New_Entry :=
           new File.File_Record'
             (Path =>
                To_Unbounded_String (Containing_Directory (Full_Name (Item))),
              File_Name =>
                To_Unbounded_String (Ada.Directories.Simple_Name (Item)),
              Md_5_Hash => Digest,
              Timestamp => Ada.Calendar.Clock);
         Search_Tree.Insert (Tree_Root, New_Entry);
         TIO.Put_Line (Search_Tree.Size (Tree => Tree_Root)'Img);

      end Process_Item;
      procedure Walk (Item : Directory_Entry_Type) with
         Pre => Kind (Item) = Directory is
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Walk (Full_Name (Item), Pattern);
         end if;
      exception
         when Name_Error =>
            null;
         when Ada.IO_Exceptions.Use_Error =>
            null;

      end Walk;
   begin
      Search (Name, Pattern, (others => True), Process_Item'Access);
      Search (Name, "", (Directory => True, others => False), Walk'Access);
   end Walk;

begin
   CLI.Init_Cli;
   CLI.Capture_Arguments;

   Walk (".", "*.adb");
   Traverse_Tree (Tree_Root, Search_Tree.Preorder);
   Finish_Time  := ART.Clock;
   Elapsed_Time := ART."-" (Finish_Time, Start_Time);
   TIO.New_Line;
   TIO.Put_Line (ART.To_Duration (Elapsed_Time)'Img);

--  exception
--     when Constraint_Error =>
--        TIO.Put_Line ("You must provide at least one valid argument");
--        TIO.Put_Line ("Valid arguments are: help, path <path to search>");
--        return;
end Main;
