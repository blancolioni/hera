with Hera.Calendar;
with Hera.Orbits;

with Hera.Factions;
with Hera.Worlds;

with Hera.UI.Models.Data_Source;
with Hera.UI.Models.Values;

with Hera.Db.Ship;
with Hera.Db.World;

package body Hera.UI.Models.World_Ships is

   type World_Ship_Model_Type is
     new Hera.UI.Models.Data_Source.Simple_Data_Source_Model with
      record
         Last_Fetch : Hera.Calendar.Time;
      end record;

   overriding procedure Start
     (Model     : in out World_Ship_Model_Type;
      User      : Hera.Db.User_Reference;
      Arguments : String);

   overriding function Changed
     (Model : World_Ship_Model_Type)
      return Boolean;

   overriding function Name
     (Model : World_Ship_Model_Type)
      return String
   is ("world_ship");

   overriding function Default_View_Name
     (Model : World_Ship_Model_Type)
      return String
   is ("Table");

   -------------
   -- Changed --
   -------------

   overriding function Changed
     (Model : World_Ship_Model_Type)
      return Boolean
   is
      use type Hera.Calendar.Time;
   begin
      return Model.Last_Fetch /= Hera.Calendar.Clock;
   end Changed;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out World_Ship_Model_Type;
      User      : Hera.Db.User_Reference;
      Arguments : String)
   is
      Faction  : constant Hera.Factions.Faction_Type'Class :=
        Hera.Factions.Get_User_Faction (User);

      Capital  : constant Hera.Db.World_Reference :=
        (if Faction.Has_Element
         then Faction.Capital_World
         else Hera.Db.Null_World_Reference);

      Name     : constant String :=
        (if Arguments = ""
         then Hera.Worlds.Name (Capital)
         else Arguments);

      World : constant Hera.Db.World_Reference :=
        Hera.Db.World.First_Reference_By_Name (Name);

   begin
      Model.Add_Column
        (Id       => "name",
         Label    => "Name",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "owner",
         Label    => "Owner",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "orbit",
         Label    => "Orbit (km)",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "period",
         Label    => "Period (m)",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "latitude",
         Label    => "Latitude",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "longitude",
         Label    => "Longitude",
         Col_Type => Values.Real_Type);

      declare

         procedure Add_Row
           (Ship : Hera.Db.Ship.Ship_Type);

         -------------
         -- Add_Row --
         -------------

         procedure Add_Row
           (Ship : Hera.Db.Ship.Ship_Type)
         is
            function T (S : String) return Values.Model_Value_Type
                        renames Values.Text_Value;
            function R (X : Real) return Values.Model_Value_Type
                        renames Values.Real_Value;

         begin
            Model.Add_Row
              (
                 (T (Ship.Name),
               T (Hera.Factions.Get (Ship.Faction).Name),
               R ((Ship.Semimajor_Axis - Hera.Worlds.Radius (World))
                 / 1000.0),
               R (Hera.Orbits.Period
                 (Hera.Worlds.Mass (World), Ship.Semimajor_Axis) / 60.0),
               R (0.0),
               R (0.0))
              );
         end Add_Row;

      begin
         for Ship of
           Hera.Db.Ship.Select_By_World (World)
         loop
            Add_Row (Ship);
         end loop;
      end;

   end Start;

   ----------------------
   -- World_Ship_Model --
   ----------------------

   function World_Ship_Model return Root_Hera_Model'Class is
   begin
      return Model : constant World_Ship_Model_Type :=
        World_Ship_Model_Type'
          (Hera.UI.Models.Data_Source.Simple_Data_Source_Model with
             Last_Fetch => Hera.Calendar.Clock);
   end World_Ship_Model;

end Hera.UI.Models.World_Ships;
