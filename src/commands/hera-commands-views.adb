with Hera.Star_Systems;

with Hera.UI.Models.Galaxy;
with Hera.UI.Models.Market;
with Hera.UI.Models.Orbits;
with Hera.UI.Models.Star_System;
with Hera.UI.Models.World;

with Hera.UI.Views.Console;
with Hera.UI.Views.Galaxy;
with Hera.UI.Views.Star_System;
with Hera.UI.Views.World;

with Hera.UI.Views.Tables;

with Hera.Db;
with Hera.Db.Market;
with Hera.Db.World;

package body Hera.Commands.Views is

   type Load_Galaxy_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_Galaxy_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type;

   type Load_Star_System_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_Star_System_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type;

   type Load_World_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_World_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type;

   type Load_Market_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_Market_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type;

   type Show_Orbiting_Ships_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Show_Orbiting_Ships_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type;

   type Console_View_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Console_View_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_Galaxy_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if Contains (Arguments, "table") then
         return Hera.UI.Views.Tables.Create_Table_View
           (Hera.UI.Models.Galaxy.Create_Galaxy_Model);
      else
         return Hera.UI.Views.Galaxy.Galaxy_View
           (Hera.UI.Models.Galaxy.Create_Galaxy_Model);
      end if;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_Star_System_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if not Contains (Arguments, "name") then
         return null;
      end if;

      declare
         use Hera.Db;
         Star_System : constant Star_System_Reference :=
                         Hera.Star_Systems.Find_Exact
                           (Argument (Arguments, "name"));
      begin
         if Star_System = Null_Star_System_Reference then
            return null;
         end if;

         if Contains (Arguments, "table") then
            return Hera.UI.Views.Tables.Create_Table_View
              (Hera.UI.Models.Star_System.Create (Star_System));
         else
            return Hera.UI.Views.Star_System.Star_System_View
              (Hera.UI.Models.Star_System.Create (Star_System));
         end if;
      end;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_World_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if (not Contains (Arguments, "star-system-name")
          or else not Contains (Arguments, "world-number"))
        and then not Contains (Arguments, "world-name")
      then
         return null;
      end if;

      declare
         use Hera.Db;
         Star_System  : constant Star_System_Reference :=
                          (if Contains (Arguments, "star-system-name")
                           then Hera.Star_Systems.Find_Exact
                             (Argument (Arguments, "star-system-name"))
                           else Null_Star_System_Reference);
         World_Number : constant Natural :=
                          Positive'Value
                            (Argument (Arguments, "world-number", "0"));
         World_Name   : constant String :=
                          Argument (Arguments, "world-name", "");
         Reference    : World_Reference := Null_World_Reference;
         Index        : Natural := 0;
      begin

         if World_Name /= "" then
            Reference :=
              Hera.Db.World.First_Reference_By_Name (World_Name);
         else
            if Star_System = Null_Star_System_Reference then
               return null;
            end if;

            for World of
              Hera.Db.World.Select_By_Star_System (Star_System)
            loop
               Index := Index + 1;
               if Index = World_Number then
                  Reference := World.Get_World_Reference;
                  exit;
               end if;
            end loop;
         end if;

         if Reference = Null_World_Reference then
            return null;
         end if;

         if Contains (Arguments, "table") then
            return Hera.UI.Views.Tables.Create_Table_View
              (Hera.UI.Models.World.Create (Reference),
               Headings_Down => True);
         else
            return Hera.UI.Views.World.World_View
              (Hera.UI.Models.World.Create (Reference));
         end if;
      end;

   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_Market_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if Argument_Count (Arguments) = 1 then
         declare
            use Hera.Db;
            World : constant Hera.Db.World_Reference :=
                      Hera.Db.World.First_Reference_By_Name
                        (Argument (Arguments, 1));
            Market : constant Hera.Db.Market_Reference :=
                       (if World = Null_World_Reference
                        then Null_Market_Reference
                        else Hera.Db.Market.Get_Reference_By_World
                          (World));

         begin
            if World = Null_World_Reference
              or else Market = Null_Market_Reference
            then
               return null;
            end if;

            declare
               Model : constant Hera.UI.Models.Market.Market_Model :=
                         Hera.UI.Models.Market.Create (Market);
            begin
               return Hera.UI.Views.Tables.Create_Table_View (Model);
            end;
         end;
      else
         return null;
      end if;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Show_Orbiting_Ships_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if Argument_Count (Arguments) = 1 then
         declare
            use Hera.Db;
            World  : constant Hera.Db.World_Reference :=
                       Hera.Db.World.First_Reference_By_Name
                         (Argument (Arguments, 1));
         begin
            if World = Null_World_Reference then
               return null;
            end if;

            declare
               Model : constant Hera.UI.Models.Orbits.Orbiting_Ship_Model
                 := Hera.UI.Models.Orbits.Create (World);
            begin
               return Hera.UI.Views.Tables.Create_Table_View (Model);
            end;
         end;
      else
         return null;
      end if;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Console_View_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Arguments, Session);
   begin
      return Hera.UI.Views.Console.Console_View;
   end Create_View;

   ------------------------
   -- Load_View_Commands --
   ------------------------

   procedure Load_View_Commands is
      Load_Galaxy      : Load_Galaxy_Command;
      Load_Star_System : Load_Star_System_Command;
      Load_World       : Load_World_Command;
      Load_Market      : Load_Market_Command;
      Show_Orbit       : Show_Orbiting_Ships_Command;
      Console          : Console_View_Command;
   begin
      Register ("load-galaxy-view", Load_Galaxy);
      Register ("load-star-system-view", Load_Star_System);
      Register ("load-world-view", Load_World);
      Register ("show-market", Load_Market);
      Register ("show-orbit", Show_Orbit);
      Register ("console", Console);
   end Load_View_Commands;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Load_View_Command;
      Session   : Hera.Sessions.Hera_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      use Hera.UI.Views;
      View : constant View_Type :=
               Load_View_Command'Class (Command)
               .Create_View (Session, Arguments);
   begin
      if View = null then
         Writer.Put_Error
           ("unable to create view");
      end if;

      Session.Main_View.Add_Child (View);
   end Perform;

end Hera.Commands.Views;
