with WL.Command_Line;

package body Hera.Options is

   pragma Style_Checks (Off);

   function Scenario return String is
   begin
      return WL.Command_Line.Find_Option
               ("scenario", ' ');
   end Scenario;

   function Language return String is
   begin
      return WL.Command_Line.Find_Option
               ("language", ' ');
   end Language;

   function Randomise return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("randomise", ' ');
   end Randomise;

   function Random_Seed return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("random-seed", ' ', 0);
   end Random_Seed;

   function Batch_Mode return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("batch-mode", ' ');
   end Batch_Mode;

   function Update_Count return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("update-count", ' ', 0);
   end Update_Count;

   function Create return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("create", ' ');
   end Create;

   function Log_Folder return String is
   begin
      return WL.Command_Line.Find_Option
               ("log-folder", ' ');
   end Log_Folder;

   function Import_Properties return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("import-properties", ' ');
   end Import_Properties;

   function Work_Task_Count return Positive is
   begin
      return WL.Command_Line.Find_Option
               ("work-task-count", ' ');
   end Work_Task_Count;

   function Player_Corporation return String is
   begin
      return WL.Command_Line.Find_Option
               ("player-corporation", ' ');
   end Player_Corporation;

end Hera.Options;
