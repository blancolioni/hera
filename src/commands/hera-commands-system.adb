with Ada.Calendar.Formatting;
with Ada.Exceptions;
with Ada.Text_IO;

with Hera.Version;

with Hera.Calendar;
with Hera.Contexts;
with Hera.Updates.Control;

with Hera.UI;

with Hera.Commands.System.Cat;
with Hera.Commands.System.Change_Scope;
with Hera.Commands.System.List;
with Hera.Commands.System.Show;

package body Hera.Commands.System is

   type Pwd_Command is
     new Root_Hera_Command with null record;

   overriding procedure Perform
     (Command   : Pwd_Command;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List);

   type Echo_Command is
     new Root_Hera_Command with null record;

   overriding procedure Perform
     (Command   : Echo_Command;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List);

   type History_Command is
     new Root_Hera_Command with null record;

   overriding procedure Perform
     (Command   : History_Command;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List);

   type Status_Command_Type is
     (Pause_Server, Resume_Server, Stop_Server,
      Update_Speed,
      Show_Status);

   type Status_Command (Command : Status_Command_Type) is
     new Root_Hera_Command with null record;

   overriding function Administrator_Only
     (Command : Status_Command)
      return Boolean
   is (True);

   overriding procedure Perform
     (Command   : Status_Command;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List);

   --------------------------
   -- Load_System_Commands --
   --------------------------

   procedure Load_System_Commands is
      Echo                  : Echo_Command;
      History               : History_Command;
      Pwd                   : Pwd_Command;
      Pause_Command         : Status_Command (Pause_Server);
      Resume_Command        : Status_Command (Resume_Server);
      Stop_Command          : Status_Command (Stop_Server);
      Get_Status_Command    : Status_Command (Show_Status);
      Update_Speed_Command  : Status_Command (Update_Speed);
   begin
      Register ("cat", Cat.Cat_Command);
      Register ("cd", Change_Scope.Change_Scope_Command);
      Register ("change-scope", Change_Scope.Change_Scope_Command);
      Register ("echo", Echo);
      Register ("history", History);
      Register ("ls", List.List_Command);
      Register ("pause", Pause_Command);
      Register ("pwd", Pwd);
      Register ("resume", Resume_Command);
      Register ("show", Show.Show_Command);
      Register ("update-speed", Update_Speed_Command);
      Register ("stop-server", Stop_Command);
      Register ("status", Get_Status_Command);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "caught exception while loading system commands: "
            & Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Message (E));
         raise Program_Error;
   end Load_System_Commands;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Echo_Command;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command, Context);
   begin
      for I in 1 .. Argument_Count (Arguments) loop
         if I > 1 then
            Writer.Put (" ");
         end if;
         Writer.Put (Argument (Arguments, I));
      end loop;

      if not Contains (Arguments, "n") then
         Writer.New_Line;
      end if;

   end Perform;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : History_Command;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command, Arguments);
   begin
      for I in 1 .. Context.History_Length loop
         declare
            Index_Image : String (1 .. 5);
            It          : Natural := I;
         begin
            for Ch of reverse Index_Image loop
               if It = 0 then
                  Ch := ' ';
               else
                  Ch := Character'Val (It mod 10 + 48);
                  It := It / 10;
               end if;
            end loop;
            Writer.Put_Line (Index_Image & "  " & Context.Get_History (I));
         end;
      end loop;
   end Perform;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Pwd_Command;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command, Arguments);
   begin
      Writer.Put_Line
        (Context.Current_Scope);
   end Perform;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Status_Command;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List)
   is
   begin
      case Command.Command is
         when Pause_Server =>
            Hera.Updates.Control.Pause_Updates;
         when Resume_Server =>
            Hera.Updates.Control.Resume_Updates;
         when Stop_Server =>
            Hera.UI.Current_UI.Stop
              (Argument (Arguments, "message", "stop server command"));
            Writer.Put_Line
              (Context.User_Name
               & ": server stopped via stop-server command");
         when Update_Speed =>

            if Argument_Count (Arguments) > 1 then
               Writer.Put_Error ("Usage: update-speed [time factor]");
               return;
            end if;

            if Argument_Count (Arguments) = 1 then
               declare
                  Value : Duration;
               begin
                  Value := Duration'Value (Argument (Arguments, 1));
                  Hera.Updates.Control.Set_Advance_Speed (Value);
               exception
                  when Constraint_Error =>
                     Writer.Put_Error ("Usage: update-speed [time factor]");
                     return;
               end;
            end if;

            declare
               Paused             : Boolean;
               Advance_Per_Second : Duration;
               Start_Time         : Ada.Calendar.Time;
            begin
               Hera.Updates.Control.Get_Status
                 (Start_Time, Paused, Advance_Per_Second);
               Writer.Put_Line
                 ("time acceleration:"
                  & Natural'Image (Natural (Advance_Per_Second)));
            end;

         when Show_Status =>
            declare
               Paused             : Boolean;
               Advance_Per_Second : Duration;
               Start_Time         : Ada.Calendar.Time;
            begin
               Writer.Put_Line ("logged in as " & Context.User_Name);
               Hera.Updates.Control.Get_Status
                 (Start_Time, Paused, Advance_Per_Second);
               Writer.Put_Line
                 (Hera.Version.Name
                  & " version "
                  & Hera.Version.Version_String);
               Writer.Put_Line
                 ("Server started "
                  & Ada.Calendar.Formatting.Image
                    (Start_Time));
               Writer.Put_Line
                 ("status: " & (if Paused then "paused" else "running"));
               Writer.Put_Line
                 ("current server date: "
                  & Hera.Calendar.Image
                    (Hera.Calendar.Clock));
               Writer.Put_Line
                 ("time acceleration:"
                  & Natural'Image (Natural (Advance_Per_Second)));
            end;

      end case;
   end Perform;

end Hera.Commands.System;
