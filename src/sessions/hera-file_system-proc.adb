with Ada.Calendar.Formatting;

with Marlowe.Version;
with Kit.Version;

with Hera.Version;

with Hera.Calendar;
with Hera.Updates.Control;

with Hera.UI;

with Hera.File_System.Directories;
with Hera.File_System.Files;

package body Hera.File_System.Proc is

   function Status_Text return String;

   function Version return String
   is (Hera.Version.Version_String);

   -----------------------------
   -- Create_Proc_File_System --
   -----------------------------

   function Create_Proc_File_System return Node_Interface'Class is
   begin
      return P : Node_Interface'Class := Directories.Directory_Node do
         P.Bind_Child
           (Name  => "status",
            Child => Root_Filesystem.Create
              (Files.Dynamic_File_Node (Status_Text'Access)));
         P.Bind_Child
           (Name  => "version",
            Child =>
              Root_Filesystem.Create
                (Files.Dynamic_File_Node (Version'Access)));
      end return;
   end Create_Proc_File_System;

   -----------------
   -- Status_Text --
   -----------------

   function Status_Text return String is
      NL                 : constant Character := Character'Val (10);
      Paused             : Boolean;
      Advance_Per_Second : Duration;
      Start_Time         : Ada.Calendar.Time;
   begin
      Hera.Updates.Control.Get_Status
        (Start_Time, Paused, Advance_Per_Second);
      return
        Hera.Version.Name
        & " version "
        & Hera.Version.Version_String
        & NL
        & "kit     "
        & Kit.Version.Version_String
        & NL
        & "marlowe "
        & Marlowe.Version.Version_String
        & NL
        & "Server started "
        & Ada.Calendar.Formatting.Image (Start_Time)
        & NL
        & "status: " & (if Paused then "paused" else "running")
        & NL
        & "current server date: "
        & Hera.Calendar.Image (Hera.Calendar.Clock)
        & "time acceleration:"
        & Natural'Image (Natural (Advance_Per_Second));
   end Status_Text;

end Hera.File_System.Proc;
