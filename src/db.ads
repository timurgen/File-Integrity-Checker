with SQLite;

package Db is

   Db : SQLite.Data_Base;
   Initialized : Boolean := False;

   procedure Init_Db(Path_And_Name : String := "db.sqlite")  with Pre => Initialized = False, Post => Initialized = True;

   procedure Close_Db with Pre => Initialized = True, Post => Initialized = False;

   procedure Transaction_Start with Pre => Initialized = True;

   procedure Transaction_Commit with Pre => Initialized = True;

end Db;
