with GNAT.Command_Line;
package body CLI is

   package cli renames GNAT.Command_Line;
   Config : GNAT.Command_Line.Command_Line_Configuration;
   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help is
   begin
      cli.Display_Help(Config);
   end Display_Help;

   --------------
   -- Init_Cli --
   --------------

   procedure Init_Cli is
   begin
      GNAT.Command_Line.Define_Switch(Config => Config, Switch => "-v", Help => "Prints out version");
      GNAT.Command_Line.Define_Switch(Config => Config, Long_Switch => "--verbose", Help => "Prints verbose output");
   end Init_Cli;

   -----------------------
   -- Capture_Arguments --
   -----------------------

   procedure Capture_Arguments is
   begin
      GNAT.Command_Line.Getopt(Config);
   end Capture_Arguments;

end CLI;
