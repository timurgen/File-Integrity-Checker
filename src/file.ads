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
   subtype Tree_Key is Unbounded_String;


end File;
