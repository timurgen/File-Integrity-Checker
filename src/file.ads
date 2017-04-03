with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;
with GNAT.MD5;
with Ada.Calendar;

package File is
   Name_Max: constant Integer := 255;
   Path_Max: constant Integer := 4096;

   subtype MD5_String is String(1..32);

   type File_Record is record
      Path: Unbounded_String;
      File_Name: Unbounded_String;
      Md_5_Hash: MD5_String;
      Timestamp: Ada.Calendar.Time;
   end record;

   type File_Record_Acess is access all File_Record;
   --with Dynamic_Predicate => File_Record.Path'Length > 0
     --and then File_Record.Path'Length <= Path_Max
     --and then File_Record.File_Name'Length > 0
     --and then File_Record.File_Name'Length <= Name_Max;

--   procedure Parse_Dir(Path: String) with Pre => Path'Length > 0 and then Path'Length <= Path_Max;

  -- procedure Store_Dir_Structure;

   --procedure Restore_Dir_Structure;


end File;
