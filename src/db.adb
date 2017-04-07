with SQLite; use SQLite;
with Object; use Object;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
package body Db is

   -------------
   -- Init_Db --
   -------------

   procedure Init_Db (Path_And_Name : String := "db.sqlite") is
   begin
      Db := Open(Path_And_Name);
      Initialized := True;
   exception
      when Data_Error | Use_Error => Initialized := False;

   end Init_Db;

   procedure Close_Db is
   begin
      --find out how to close db
      Initialized := False;
   end Close_Db;

   -----------------------
   -- Transaction_Start --
   -----------------------

   procedure Transaction_Start is
   begin
      Exec(Db, "BEGIN");
   end Transaction_Start;

   ------------------------
   -- Transaction_Commit --
   ------------------------

   procedure Transaction_Commit is
   begin
      Exec(Db, "COMMIT");
   end Transaction_Commit;

end Db;
