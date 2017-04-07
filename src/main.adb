pragma Assertion_Policy (Check);

with CLI; use CLI;
with Db;

with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Less_Case_Insensitive;
with Ada.Strings.Fixed.Less_Case_Insensitive;

with Ada.Assertions;        use Ada.Assertions;

with Ada.Directories;       use Ada.Directories;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with File; use File;

with Tree_Functions; use Tree_Functions;
with Binary_Search_Tree;

with MD5.Driver;
with Ada.Calendar;
with SQLite; use SQLite;

with Ada.IO_Exceptions;

procedure Main is




   -- text output
   package TIO renames Ada.Text_IO;



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
         Digest_File_Content    : String (1 .. 32);
         New_Entry : File.File_Record_Acess;
      begin
         MD5.Driver.Digest_File (Full_Name (Item), Digest_File_Content);
         TIO.Put_Line (Digest_File_Content);
         New_Entry :=
           new File.File_Record'
             (Path =>
                To_Unbounded_String (Containing_Directory (Full_Name (Item))),
              File_Name =>
                To_Unbounded_String (Ada.Directories.Simple_Name (Item)),
              Md_5_Hash => Digest_File_Content,
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
   Db : Data_Base;
begin
   Init_Cli;
   Capture_Arguments;


   --     Walk (".", "*.adb");
   --     Traverse_Tree (Tree_Root, Search_Tree.Preorder);
   --     declare
   --        Tmp : File.File_Record_Acess := Search_Tree.Retrieve(Tree => Tree_Root,
   --                                                             Key  => To_Unbounded_String("C:\Users\80473\Dropbox\books\IT_books_and_papers\Ada\Ada-univercity\Ada 001\lectures-01_Overview-04_Ten_Bouncing_Balls\main.adb"));
   --     begin
   --        TIO.Put_Line(To_String(Tmp.File_Name));
   --     end;


end Main;
